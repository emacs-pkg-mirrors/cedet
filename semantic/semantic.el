;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.142.2.4 2003/04/22 12:58:16 ponced Exp $

(defvar semantic-version "1.4.4"
  "Current version of Semantic.")

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; API for determining semantic content of a buffer.  The mode using
;; semantic must be a deterministic programming language.
;;
;; The output of a semantic bovine parse is parse tree.  While it is
;; possible to assign actions in the bovine-table in a similar fashion
;; to bison, this is not it's end goal.

(require 'working)
(require 'assoc)

(defun semantic-require-version (major minor &optional beta)
  "Non-nil if this version of semantic does not satisfy a specific version.
Arguments can be:

  (MAJOR MINOR &optional BETA)

  Values MAJOR and MINOR must be integers.  BETA can be an integer, or
excluded if a released version is required.

It is assumed that if the current version is newer than that specified,
everything passes.  Exceptions occur when known incompatibilities are
introduced."
  (when (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\( ?beta ?\\([0-9]+\\)\\)?"
		      semantic-version)
    (let ((vmajor (string-to-int (match-string 1 semantic-version)))
	  (vminor (string-to-int (match-string 2 semantic-version)))
	  (vbeta (match-string 4 semantic-version)))
      (when vbeta (setq vbeta (string-to-int vbeta)))
      (or (> major vmajor)
	  (and (= major vmajor) (> minor vminor))
	  (and (= major vmajor) (= minor vminor)
	       (or (and (not beta) vbeta)
		   (and beta vbeta (> beta vbeta)))))
      )))

(defgroup semantic nil
  "Parser Generator/Parser."
  )

;;; Compatibility
;;
(if (featurep 'xemacs)
    (progn
      (defalias 'semantic-overlay-live-p 'extent-live-p)
      (defalias 'semantic-make-overlay 'make-extent)
      (defalias 'semantic-overlay-put 'set-extent-property)
      (defalias 'semantic-overlay-get 'extent-property)
      (defalias 'semantic-overlay-delete 'delete-extent)
      (defalias 'semantic-overlays-at
        (lambda (pos) (extent-list nil pos pos)))
      (defalias 'semantic-overlays-in
	(lambda (beg end) (extent-list nil beg end)))
      (defalias 'semantic-overlay-buffer 'extent-buffer)
      (defalias 'semantic-overlay-start 'extent-start-position)
      (defalias 'semantic-overlay-end 'extent-end-position)
      (defalias 'semantic-overlay-next-change 'next-extent-change)
      (defalias 'semantic-overlay-previous-change 'previous-extent-change)
      (defalias 'semantic-overlay-lists
	(lambda () (list (extent-list))))
      (defalias 'semantic-overlay-p 'extentp)
      (defun semantic-read-event ()
        (let ((event (next-command-event)))
          (if (key-press-event-p event)
              (let ((c (event-to-character event)))
                (if (char-equal c (quit-char))
                    (keyboard-quit)
                  c)))
          event))
      )
  (defalias 'semantic-overlay-live-p 'overlay-buffer)
  (defalias 'semantic-make-overlay 'make-overlay)
  (defalias 'semantic-overlay-put 'overlay-put)
  (defalias 'semantic-overlay-get 'overlay-get)
  (defalias 'semantic-overlay-delete 'delete-overlay)
  (defalias 'semantic-overlays-at 'overlays-at)
  (defalias 'semantic-overlays-in 'overlays-in)
  (defalias 'semantic-overlay-buffer 'overlay-buffer)
  (defalias 'semantic-overlay-start 'overlay-start)
  (defalias 'semantic-overlay-end 'overlay-end)
  (defalias 'semantic-overlay-next-change 'next-overlay-change)
  (defalias 'semantic-overlay-previous-change 'previous-overlay-change)
  (defalias 'semantic-overlay-lists 'overlay-lists)
  (defalias 'semantic-overlay-p 'overlayp)
  (defalias 'semantic-read-event 'read-event)
  )

(if (and (not (featurep 'xemacs))
	 (>= emacs-major-version 21))
    (defalias 'semantic-make-local-hook 'identity)
  (defalias 'semantic-make-local-hook 'make-local-hook)
  )

;;; Code:

(defvar semantic-edebug nil
  "When non-nil, activate the interactive parsing debugger.
Do not set this yourself.  Call `semantic-bovinate-buffer-debug'.")


(defcustom semantic-dump-parse nil
  "When non-nil, dump parsing information."
  :group 'semantic
  :type 'boolean)

(defvar semantic-toplevel-bovine-table nil
  "Variable that defines how to bovinate top level items in a buffer.
Set this in your major mode to return function and variable semantic
types.

The format of a BOVINE-TABLE is:

 ( ( NONTERMINAL-SYMBOL1 MATCH-LIST1 )
   ( NONTERMINAL-SYMBOL2 MATCH-LIST2 )
   ...
   ( NONTERMINAL-SYMBOLn MATCH-LISTn )
 
Where each NONTERMINAL-SYMBOL is an artificial symbol which can appear
in any child state.  As a starting place, one of the NONTERMINAL-SYMBOLS
must be `bovine-toplevel'.

A MATCH-LIST is a list of possible matches of the form:

 ( STATE-LIST1
   STATE-LIST2
   ...
   STATE-LISTN )

where STATE-LIST is of the form:
  ( TYPE1 [ \"VALUE1\" ] TYPE2 [ \"VALUE2\" ] ... LAMBDA )

where TYPE is one of the returned types of the token stream.
VALUE is a value, or range of values to match against.  For
example, a SYMBOL might need to match \"foo\".  Some TYPES will not
have matching criteria.

LAMBDA is a lambda expression which is evaled with the text of the
type when it is found.  It is passed the list of all buffer text
elements found since the last lambda expression.  It should return a
semantic element (see below.)

You can easily avoid learning the format of this variable by
writing a BNF file.  See notes in `semantic-bnf.el' or the
\"BNF conversion\" section in the semantic texinfo manual.

For consistency between languages, try to use common return values
from your parser.  Please reference the \"Semantic Token Style Guide\"
section in the semantic texinfo manual.")
(make-variable-buffer-local 'semantic-toplevel-bovine-table)

(defvar semantic-toplevel-bovine-table-source nil
  "The .bnf source file the current table was created from.")
(make-variable-buffer-local 'semantic-toplevel-bovine-table-source)

(defvar semantic-symbol->name-assoc-list
  '((type     . "Types")
    (variable . "Variables")
    (function . "Functions")
    (include  . "Dependencies")
    (package  . "Provides"))
  "Association between symbols returned, and a string.
The string is used to represent a group of objects of the given type.
It is sometimes useful for a language to use a different string
in place of the default, even though that language will still
return a symbol.  For example, Java return's includes, but the
string can be replaced with `Imports'.")
(make-variable-buffer-local 'semantic-symbol->name-assoc-list)

(defvar semantic-symbol->name-assoc-list-for-type-parts nil
  "Like `semantic-symbol->name-assoc-list' for type parts.
Some tokens that have children (see `semantic-nonterminal-children')
will want to define the names of classes of tokens differently than
at the top level.  For example, in C++, a Function may be called
a Method.  In addition, there may be new types of tokens that exist
only in classes, such as protection labels.")
(make-variable-buffer-local 'semantic-symbol->name-assoc-list-for-type-parts)

(defvar semantic-case-fold nil
  "Value for `case-fold-search' when parsing.")
(make-variable-buffer-local 'semantic-case-fold)

(defvar semantic-flex-depth 0
  "Default flexing depth.
This specifies how many lists to create tokens in.")
(make-variable-buffer-local 'semantic-flex-depth)

(defvar semantic-ignore-comments t
  "Default comment handling.
t means to strip comments when flexing.  Nil means to keep comments
as part of the token stream.")
(make-variable-buffer-local 'semantic-ignore-comments)

(defvar semantic-expand-nonterminal nil
  "Function to call for each nonterminal production.
Return a list of non-terminals derived from the first argument, or nil
if it does not need to be expanded.
Languages with compound definitions should use this function to expand
from one compound symbol into several.  For example, in C the definition
  int a, b;
is easily parsed into one token.  This function should take this
compound token and turn it into two tokens, one for A, and the other for B.")
(make-variable-buffer-local 'semantic-expand-nonterminal)

(defvar semantic-toplevel-bovine-cache nil
  "A cached copy of a recent bovination, plus state.
If no significant changes have been made (based on the state) then
this is returned instead of re-parsing the buffer.
 
  DO NOT USE THIS VARIABLE IN PROGRAMS.

If you need a token list, use `semantic-bovinate-toplevel'.  If you
need the cached values for some reason, chances are you can, add a
hook to `semantic-after-toplevel-cache-change-hook'.")
(make-variable-buffer-local 'semantic-toplevel-bovine-cache)

(defvar semantic-unmatched-syntax-cache nil
  "A cached copy of unmatched syntax tokens.")
(make-variable-buffer-local 'semantic-unmatched-syntax-cache)

(defvar semantic-unmatched-syntax-cache-check nil
  "Non nil if the unmatched syntax cache is out of date.
This is tracked with `semantic-change-function'.")
(make-variable-buffer-local 'semantic-unmatched-syntax-cache-check)

(defvar semantic-edits-are-safe nil
  "When non-nil, modifications do not require a reparse.
This prevents tokens from being marked dirty, and it
prevents top level edits from causing a cache check.
Use this when writing programs that could cause a full
reparse, but will not change the tag structure, such
as adding or updating top-level comments.")

(defvar semantic-toplevel-bovine-cache-check nil
  "Non nil if the bovine cache is out of date.
This is tracked with `semantic-change-function'.")
(make-variable-buffer-local 'semantic-toplevel-bovine-cache-check)

(defvar semantic-toplevel-bovine-force-reparse nil
  "Non nil if the next token request forces a reparse.")
(make-variable-buffer-local 'semantic-toplevel-bovine-force-reparse)

(defvar semantic-dirty-tokens nil
  "List of tokens in the current buffer which are dirty.
Dirty functions can then be reparsed, and spliced back into the main list.")
(make-variable-buffer-local 'semantic-dirty-tokens)

(defvar semantic-bovinate-nonterminal-check-obarray nil
  "Obarray of streams already parsed for nonterminal symbols.")
(make-variable-buffer-local 'semantic-bovinate-nonterminal-check-obarray)

(defvar semantic-dirty-token-hooks nil
  "Hooks run after when a token is marked as dirty (edited by the user).
The functions must take TOKEN, START, and END as a parameters.
This hook will only be called once when a token is first made dirty,
subsequent edits will not cause this to run a second time unless that
token is first cleaned.  Any token marked as dirty will
also be called with `semantic-clean-token-hooks', unless a full
reparse is done instead.")

(defvar semantic-pre-clean-token-hooks nil
  "Hooks run before a token is reparsed.
The functions must take a TOKEN as a parameter.
Any token sent to this hook is about to be cleaned, or reparsed.
The overlay may change, but many features and properties will
persist unless a full reparse is later required.
See `semantic-dirty-token-hooks' and `semantic-clean-token-hooks'.")

(defvar semantic-clean-token-hooks nil
  "Hooks run after a token is marked as clean (reparsed after user edits.)
The functions must take a TOKEN as a parameter.
Any token sent to this hook will have first been called with
`semantic-dirty-token-hooks'.  This hook is not called for tokens
marked dirty if the buffer is completely reparsed.  In that case, use
`semantic-after-toplevel-cache-change-hook'.")

(defvar semantic-change-hooks nil
  "Hooks run when semantic detects a change in a buffer.
Each hook function must take three arguments, identical to the
common hook `after-change-functions'.")

(defvar semantic-unmatched-syntax-hook nil
  "Hooks run when semantic detects syntax not matched in a grammar.
Each individual piece of syntax (such as a symbol or punctuation
character) is called with this hook when it doesn't match in the
grammar, and multiple unmatched syntax elements are not grouped
together.  Each hook is called with one argument, which is a list of
syntax tokens created by the semantic lexer.  Use the functions
`semantic-flex-start', `semantic-flex-end' and `semantic-flex-text' to
get information about these tokens.  The current buffer is the buffer
these tokens are derived from.")


(defvar semantic-bovinate-toplevel-override nil
  "Local variable set by major modes which provide their own bovination.
This function should behave as the function `semantic-bovinate-toplevel'.")
(make-variable-buffer-local 'semantic-bovinate-toplevel-override)

(defvar semantic-before-toplevel-bovination-hook nil
  "Hooks run before a toplevel token parse.
It is called before any request for tokens is made via the function
`semantic-bovinate-toplevel' by an application.
If any hook returns a nil value, the cached value is returned
immediately, even if it is empty.")

(defvar semantic-after-toplevel-bovinate-hook nil
  "Hooks run after a toplevel token parse.
It is not run if the toplevel parse command is called, and buffer does
not need to be fully reparsed.
For language specific hooks, make sure you define this as a local hook.

This hook should not be used any more.
Use `semantic-after-toplevel-cache-change-hook' instead.")

(defvar semantic-after-toplevel-cache-change-hook nil
  "Hooks run after the buffer token list has changed.
This list will change when a buffer is reparsed, or when the token
list in a buffer is cleared.  It is *NOT* called if the current token
list partially reparsed.

Hook functions must take one argument, which is the new list of
tokens associated with this buffer.

For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-after-partial-cache-change-hook nil
  "Hooks run after the buffer token list has been updated.
This list will change when the current token list has been partially
reparsed.

Hook functions must take one argument, which is the list of tokens
updated among the ones associated with this buffer.

For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-before-toplevel-cache-flush-hook nil
  "Hooks run before the toplevel nonterminal cache is flushed.
For language specific hooks, make sure you define this as a local
hook.  This hook is called before a corresponding
`semantic-after-toplevel-cache-change-hook' which is also called
during a flush when the cache is given a new value of nil.")

(defvar semantic-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as needing a reparse.
For language specific hooks, make sure you define this as a local
hook.
Not used yet; part of the next generation reparse mechanism")

(defvar semantic-no-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as not needing a reparse.
If the hook returns non-nil, then declare that a reparse is needed.
For language specific hooks, make sure you define this as a local
hook.
Not used yet; part of the next generation reparse mechanism.")


;;; Primitive lexeme access system:

(defsubst semantic-flex-start (semobj)
  "Fetch the start position of the semantic object SEMOBJ."
  (nth 1 semobj))

(defsubst semantic-flex-end (semobj)
  "Fetch the end position of the semantic object SEMOBJ."
  (cdr (cdr semobj)))

(defsubst semantic-flex-text (semobj)
  "Fetch the text associated with the semantic object SEMOBJ."
  (buffer-substring-no-properties (semantic-flex-start semobj)
                                  (semantic-flex-end   semobj)))


;;; Primitive Token access system:
;;
;; These are token level APIs (similar to some APIs in semantic-util)
;; which are required for parsing operations.  Semantic.el should have
;; no dependencies on other semantic files.
;;
;; TFE = Token From End

(defconst semantic-tfe-overlay 1
  "Amount to subtract from the length of the token to get the overlay.")
(defconst semantic-tfe-properties 2
  "Amount to subtract from the length of the token to get the property list.")
(defconst semantic-tfe-docstring 3
  "Amount to subtract from the length of the token to get the doc string.")
(defconst semantic-tfe-number 2
  "The number of required end elements.")

(defmacro semantic-token-token (token)
  "Retrieve from TOKEN the token identifier.
ie, the symbol 'variable, 'function, 'type, or other."
  `(nth 1 ,token))

(defsubst semantic-token-name (token)
  "Retrieve the name of TOKEN."
  (car token))

(defun semantic-token-docstring (token &optional buffer)
  "Retrieve the documentation of TOKEN.
Optional argument BUFFER indicates where to get the text from.
If not provided, then only the POSITION can be provided."
  (let ((p (nth (- (length token) semantic-tfe-docstring) token)))
    (if (and p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (semantic-flex-text (car (semantic-flex p (1+ p)))))
      p)))

(defmacro semantic-token-properties (token)
  "Retrieve the PROPERTIES part of TOKEN.
The returned item is an ALIST of (KEY . VALUE) pairs."
  `(nth (- (length ,token) semantic-tfe-properties) ,token))

(defmacro semantic-token-properties-cdr (token)
  "Retrieve the cons cell for the PROPERTIES part of TOKEN."
  `(nthcdr (- (length ,token) semantic-tfe-properties) ,token))

(defun semantic-token-put (token key value)
  "For TOKEN, put the property KEY on it with VALUE.
If VALUE is nil, then remove the property from TOKEN."
  (let* ((c (semantic-token-properties-cdr token))
	 (al (car c))
	 (a (assoc key (car c))))
    (if a
	(if value
	    (setcdr a value)
	  (adelete 'al key)
	  (setcar c al))
      (if value
	  (setcar c (cons (cons key value) (car c)))))
    ))

(defun semantic-token-put-no-side-effect (token key value)
  "For TOKEN, put the property KEY on it with VALUE without side effects.
If VALUE is nil, then remove the property from TOKEN.
All cons cells in the property list are replicated so that there
are no side effects if TOKEN is in shared lists."
  (let* ((c (semantic-token-properties-cdr token))
	 (al (copy-sequence (car c)))
	 (a (assoc key (car c))))
    ;; This removes side effects
    (setcar c a)
    (if a
	(if value
	    (setcdr a value)
	  (adelete 'al key)
	  (setcar c al))
      (if value
	  (setcar c (cons (cons key value) (car c)))))
    ))

(defsubst semantic-token-get (token key)
  "For TOKEN, get the value for property KEY."
  (cdr (assoc key (semantic-token-properties token))))

(defmacro semantic-token-overlay (token)
  "Retrieve the OVERLAY part of TOKEN.
The returned item may be an overlay or an unloaded buffer representation."
  `(nth (- (length ,token) semantic-tfe-overlay) ,token))

(defmacro semantic-token-overlay-cdr (token)
  "Retrieve the cons cell containing the OVERLAY part of TOKEN."
  `(nthcdr (- (length ,token) semantic-tfe-overlay) ,token))

(defmacro semantic-token-extent (token)
  "Retrieve the extent (START END) of TOKEN."
  `(let ((o (semantic-token-overlay ,token)))
     (if (semantic-overlay-p o)
	 (list (semantic-overlay-start o) (semantic-overlay-end o))
       (list (aref o 0) (aref o 1)))))

(defsubst semantic-token-start (token)
  "Retrieve the start location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-start o)
      (aref o 0))))

(defsubst semantic-token-end (token)
  "Retrieve the end location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-end o)
      (aref o 1))))

(defsubst semantic-token-buffer (token)
  "Retrieve the buffer TOKEN resides in."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-buffer o)
      ;; We have no buffer for this token (It's not in Emacs right now.)
      nil)))

(defsubst semantic-token-p (token)
  "Return non-nil if TOKEN is most likely a semantic token."
  (and (listp token)
       (stringp (car token))
       (car (cdr token))
       (symbolp (car (cdr token)))))

(defun semantic-token-with-position-p (token)
  "Return non-nil if TOKEN is a semantic token with positional information."
  (and (semantic-token-p token)
       (let ((o (semantic-token-overlay token)))
	 (or (semantic-overlay-p o)
	     (and (arrayp o)
		  (not (stringp o)))))))


;;; Overlay.
;;
(defun semantic-delete-overlay-maybe (overlay)
  "Delete OVERLAY if it is a semantic token overlay."
  (if (semantic-overlay-get overlay 'semantic)
      (semantic-overlay-delete overlay)))

;;; Interfacing with the system
;;
(defvar semantic-init-hooks nil
  "*Hooks run when a buffer is initialized with a parsing table.")

(defvar semantic-init-db-hooks nil
  "Hooks run when a buffer is initialized with a parsing table for DBs.
This hook is for database functions which intend to swap in a token table.
This guarantees that the DB will go before other modes that require
a parse of the buffer.")

(defun semantic-active-p ()
  "Return non-nil if the current buffer was set up for parsing."
  (or semantic-toplevel-bovine-table
      semantic-bovinate-toplevel-override))

(defsubst semantic-bovine-toplevel-full-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a full reparse.
Optional argument CHECKCACHE indicates if the cache check should be made."
  (or semantic-toplevel-bovine-force-reparse
      (and
       checkcache
       semantic-toplevel-bovine-cache-check)))

(defsubst semantic-bovine-toplevel-partial-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a partial reparse.
This only returns non-nil if `semantic-bovine-toplevel-full-reparse-needed-p'
returns nil.
Optional argument CHECKCACHE indicates if the cache check should be made
when checking `semantic-bovine-toplevel-full-reparse-needed-p'."
  (and semantic-toplevel-bovine-cache
       semantic-dirty-tokens
       (not (semantic-bovine-toplevel-full-reparse-needed-p checkcache))))

(defsubst semantic-bovine-umatched-syntax-refresh-needed-p  (&optional checkcache)
  "Return non-nil if the unmatched syntax cache needs a refresh.
That is if the cache is dirty or if the current buffer needs a full or
partial reparse.  Optional argument CHECKCACHE indicates if the
toplevel cache check should be made."
  (or semantic-unmatched-syntax-cache-check
      (semantic-bovine-toplevel-full-reparse-needed-p checkcache)
      (semantic-bovine-toplevel-partial-reparse-needed-p checkcache)))

(defun semantic-new-buffer-fcn ()
  "Setup Semantic in the current buffer.
Runs `semantic-init-hook' if the major mode is setup to use Semantic."
  (when (semantic-active-p)
    (semantic-clear-toplevel-cache)
    (setq semantic-toplevel-bovine-force-reparse t)
    ;; Call DB hooks before regular init hooks
    (run-hooks 'semantic-init-db-hooks)
    (run-hooks 'semantic-init-hooks)))

(defvar semantic-changed-mode-buffers nil
  "List of buffers whose `major-mode' has changed recently.")

(defun semantic-post-change-major-mode-function ()
  "`post-command-hook' run when there is a `major-mode' change.
This makes sure semantic-init type stuff can occur."
  (remove-hook 'post-command-hook
               'semantic-post-change-major-mode-function)
  (let (buf)
    (while semantic-changed-mode-buffers
      (setq buf (car semantic-changed-mode-buffers)
            semantic-changed-mode-buffers
            (cdr semantic-changed-mode-buffers))
      (and (buffer-live-p buf)
           (buffer-file-name buf)
           (with-current-buffer buf
             (semantic-new-buffer-fcn))))))

(defun semantic-change-major-mode-hook-function ()
  "Function called in `change-major-mode-hook'."
  (add-to-list 'semantic-changed-mode-buffers (current-buffer))
  (add-hook 'post-command-hook 'semantic-post-change-major-mode-function))

(add-hook 'find-file-hooks
          'semantic-post-change-major-mode-function)
(add-hook 'change-major-mode-hook
          'semantic-change-major-mode-hook-function)

;; Test the above hook.
;;(add-hook 'semantic-init-hooks (lambda () (message "init for semantic")))

(defun semantic-rebovinate-quickly-hook ()
  "For use in a hook.  When only a partial reparse is needed, reparse."
  (condition-case nil
      (if (semantic-bovine-toplevel-partial-reparse-needed-p nil)
	  (semantic-bovinate-toplevel))
    (error nil)))

(if (boundp 'eval-defun-hooks)
    (add-hook 'eval-defun-hooks 'semantic-rebovinate-quickly-hook))

;;; Parsing Commands
;;
(eval-when-compile
  (condition-case nil (require 'pp) (error nil)))

(defun bovinate (&optional clear)
  "Bovinate the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before bovinating."
  (interactive "P")
  (if clear (semantic-clear-toplevel-cache))
  (let ((out (semantic-bovinate-toplevel t)))
    (pop-to-buffer "*BOVINATE*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string out))
    (goto-char (point-min))))

(defun bovinate-debug ()
  "Bovinate the current buffer and run in debug mode."
  (interactive)
  (let ((semantic-edebug t)
	(out (semantic-bovinate-debug-buffer)))
    (pop-to-buffer "*BOVINATE*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string out))))

;;; Parsing functions
;;
(defun semantic-set-unmatched-syntax-cache (unmatched-syntax)
  "Set the unmatched syntax cache.
Argument UNMATCHED-SYNTAX is the syntax to set into the cache."
  ;; This function is not actually called by the main parse loop.
  ;; This is intended for use by semanticdb.
  (setq semantic-unmatched-syntax-cache unmatched-syntax
	semantic-unmatched-syntax-cache-check nil)
    ;; Refresh the display of unmatched syntax tokens if enabled
  (run-hook-with-args 'semantic-unmatched-syntax-hook
                      semantic-unmatched-syntax-cache))

(defun semantic-clear-unmatched-syntax-cache ()
  "Clear the cache of unmatched syntax tokens."
  (setq semantic-unmatched-syntax-cache nil
        semantic-unmatched-syntax-cache-check t))

(defun semantic-bovinate-unmatched-syntax (&optional checkcache)
  "Return the list of unmatched syntax tokens.
If the optional argument CHECKCACHE is non-nil, then make sure the
cached token list is up to date."
  ;; If the cache need refresh then do a full re-parse.
  (if (semantic-bovine-umatched-syntax-refresh-needed-p checkcache)
      ;; To avoid a recursive call, temporarily disable
      ;; `semantic-unmatched-syntax-hook'.
      (let (semantic-unmatched-syntax-hook)
        (condition-case nil
            (progn
              (semantic-clear-toplevel-cache)
              (semantic-bovinate-toplevel))
          (quit
           (message "semantic-bovinate-unmatched-syntax: parsing of buffer canceled."))
          )))
    semantic-unmatched-syntax-cache)

(defun semantic-clear-toplevel-cache ()
  "Clear the toplevel bovine cache for the current buffer.
Clearing the cache will force a complete reparse next time a token
stream is requested."
  (interactive)
  (run-hooks 'semantic-before-toplevel-cache-flush-hook)
  (setq semantic-toplevel-bovine-cache nil)
  (semantic-clear-unmatched-syntax-cache)
  ;; Nuke all semantic overlays.  This is faster than deleting based
  ;; on our data structure.
  (let ((l (semantic-overlay-lists)))
    (mapcar 'semantic-delete-overlay-maybe (car l))
    (mapcar 'semantic-delete-overlay-maybe (cdr l))
    )
  ;; Clear the dirty tokens... no longer relevant
  (setq semantic-dirty-tokens nil)
  (setq semantic-toplevel-bovine-force-reparse t)
  ;; Remove this hook which tracks if a buffer is up to date or not.
  (remove-hook 'after-change-functions 'semantic-change-function t)
  ;; Old model.  Delete someday.
  ;;(run-hooks 'semantic-after-toplevel-bovinate-hook)

  (run-hook-with-args 'semantic-after-toplevel-cache-change-hook
		      semantic-toplevel-bovine-cache)
  )

(defvar semantic-bovination-working-type 'percent
  "*The type of working message to use when bovinating.
'percent means we are doing a linear parse through the buffer.
'dynamic means we are rebovinating specific tokens.")

(defun semantic-remove-dirty-children-internal (token dirties)
  "Remove TOKEN children from DIRTIES.
Return the new value of DIRTIES."
  (if dirties
      (let ((children (semantic-nonterminal-children token t))
            child)
        (while (and children dirties)
          (setq child (car children)
                children (cdr children)
                dirties  (semantic-remove-dirty-children-internal
                          child (delq child dirties))))))
  dirties)

(defun semantic-remove-dirty-children ()
  "Remove children of dirty tokens from the list of dirty tokens.
It is not necessary and even dangerous to reparse these tokens as they
will be recreated when reparsing their parents.  Return the new value
of the variable `semantic-dirty-tokens' changed by side effect."
  (let ((dirties semantic-dirty-tokens)
        token)
    (while dirties
      (setq token   (car dirties)
            dirties (cdr dirties)
            semantic-dirty-tokens
            (semantic-remove-dirty-children-internal
             token semantic-dirty-tokens))))
  semantic-dirty-tokens)

;;;###autoload
(defun semantic-bovinate-toplevel (&optional checkcache)
  "Bovinate the entire current buffer.
If the optional argument CHECKCACHE is non-nil, then make sure the
cached token list is up to date.  If a partial reparse is possible, do
that, otherwise, do a full reparse."
  (cond
   ((not (run-hook-with-args-until-failure
	  'semantic-before-toplevel-bovination-hook))
    ;; If any hook returns nil, we must return the cache as the buffer
    ;; is supposedly unsafe for parsing.
    semantic-toplevel-bovine-cache
    )
   ((and semantic-bovinate-toplevel-override
	 ;; We cannot predict partial reparsing for these parsers.  Let them
	 ;; fend for themselves.  We can, however, handle the main cache for them.
	 (or (semantic-bovine-toplevel-partial-reparse-needed-p checkcache)
	     (semantic-bovine-toplevel-full-reparse-needed-p checkcache)))
    (semantic-clear-toplevel-cache)
    ;; Call a custom function
    (let ((res (funcall semantic-bovinate-toplevel-override checkcache)))
      (semantic-set-toplevel-bovine-cache res))
    ;; Check: The below is not needed because of the -set- command above?
    ;;(run-hooks 'semantic-after-toplevel-bovinate-hook)
    semantic-toplevel-bovine-cache
    )
   ((semantic-bovine-toplevel-partial-reparse-needed-p checkcache)
    (garbage-collect)
    (let* ((gc-cons-threshold 10000000)
	   (changes (semantic-remove-dirty-children)))
      ;; We have a cache, and some dirty tokens
      (let ((semantic-bovination-working-type 'dynamic))
        (working-status-forms (buffer-name) "done"
          (while (and semantic-dirty-tokens
                      (not (semantic-bovine-toplevel-full-reparse-needed-p
                            checkcache)))
            (semantic-rebovinate-token (car semantic-dirty-tokens))
            (setq semantic-dirty-tokens (cdr semantic-dirty-tokens))
            (working-dynamic-status))
          (working-dynamic-status t))
        (setq semantic-dirty-tokens nil))
      
      (if (semantic-bovine-toplevel-full-reparse-needed-p checkcache)
          ;; If the partial reparse fails, jump to a full reparse.
          (semantic-bovinate-toplevel checkcache)
        ;; Clear the cache of unmatched syntax tokens
        (semantic-clear-unmatched-syntax-cache)
        ;; After partial reparse completed, let hooks know the updated
        ;; tokens
        (run-hook-with-args 'semantic-after-partial-cache-change-hook
                            changes)
        semantic-toplevel-bovine-cache))
    )
   ((semantic-bovine-toplevel-full-reparse-needed-p checkcache)
    (garbage-collect)
    ;; Reparse the whole system
    (let ((gc-cons-threshold 10000000)
	  ;; Capture the lexical tokens here so that if an error is
	  ;; thrown, the cache is still safe.
	  (lex (semantic-flex (point-min) (point-max)))
          res)
      ;; Init a dump
      (if semantic-dump-parse
          (semantic-dump-buffer-init))
      ;; Clear the caches
      (semantic-clear-toplevel-cache)
      ;; Parse!
      (working-status-forms (buffer-name) "done"
	(setq res (semantic-bovinate-nonterminals
                   lex 'bovine-toplevel semantic-flex-depth))
	(working-status t))
      (setq res (nreverse res))
      ;; Set up the new overlays, and then reset the cache.
      (semantic-overlay-list res)
      (semantic-set-toplevel-bovine-cache res)
      semantic-toplevel-bovine-cache)
    )
   (t
    ;; We have a cache with stuff in it, so return it
    semantic-toplevel-bovine-cache
    )))

(defun semantic-set-toplevel-bovine-cache (tokenlist)
  "Set the toplevel bovine cache to TOKENLIST."
  (setq semantic-toplevel-bovine-cache tokenlist
	semantic-toplevel-bovine-cache-check nil
	semantic-toplevel-bovine-force-reparse nil
        semantic-unmatched-syntax-cache-check nil
        semantic-bovinate-nonterminal-check-obarray nil)
  (semantic-make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'semantic-change-function nil t)
  (run-hook-with-args 'semantic-after-toplevel-cache-change-hook
		      semantic-toplevel-bovine-cache)
  ;; Refresh the display of unmatched syntax tokens if enabled
  (run-hook-with-args 'semantic-unmatched-syntax-hook
                      semantic-unmatched-syntax-cache)
  ;; Old Semantic 1.3 hook API.  Maybe useful forever?
  (run-hooks 'semantic-after-toplevel-bovinate-hook)
  )

(defun semantic-change-function (start end length)
  "Provide a mechanism for semantic token management.
Argument START, END, and LENGTH specify the bounds of the change."
  (setq semantic-unmatched-syntax-cache-check t)
  (run-hook-with-args 'semantic-change-hooks start end length))

;;; Force token lists in and out of overlay mode.
;;
(defun semantic-deoverlay-token (token)
  "Convert TOKEN from using an overlay to using an overlay proxy."
  (when (semantic-token-p token)
    (let ((c (semantic-token-overlay-cdr token))
	  a)
      (when (and c (semantic-overlay-p (car c)))
	(setq a (vector (semantic-overlay-start (car c))
			(semantic-overlay-end (car c))))
	(semantic-overlay-delete (car c))
	(setcar c a)
	;; Fix the children of this token.
	;; Semantic-util is required and the end of semantic, so this will
	;; throw a warning
	(semantic-deoverlay-list (semantic-nonterminal-children token)))
      )))

(defun semantic-overlay-token (token)
  "Convert TOKEN from using an overlay proxy to using an overlay."
  (when (semantic-token-p token)
    (let ((c (semantic-token-overlay-cdr token))
	  o)
      (when (and c (vectorp (car c)) (= (length (car c)) 2))
	(setq o (semantic-make-overlay (aref (car c) 0)
				       (aref (car c) 1)
				       (current-buffer)))
	(setcar c o)
	(semantic-overlay-put o 'semantic token)
	;; Fix overlays in children of this token
	;; Semantic-util is required and the end of semantic, so this will
	;; throw a warning
	(semantic-overlay-list (semantic-nonterminal-children token))
	))))

(defun semantic-deoverlay-list (l)
  "Remove overlays from the list L."
  (mapcar 'semantic-deoverlay-token l))

(defun semantic-overlay-list (l)
  "Convert numbers to  overlays from the list L."
  (mapcar 'semantic-overlay-token l))

(defun semantic-deoverlay-cache ()
  "Convert all tokens in the current cache to use overlay proxies."
  (semantic-deoverlay-list (semantic-bovinate-toplevel)))

(defun semantic-overlay-cache ()
  "Convert all tokens in the current cache to use overlays."
  (condition-case nil
      ;; In this unique case, we cannot call the usual toplevel fn.
      ;; because we don't want a reparse, we want the old overlays.
      (semantic-overlay-list semantic-toplevel-bovine-cache)
    ;; Recover when there is an error restoring the cache.
    (error (message "Error recovering token list.")
	   (semantic-clear-toplevel-cache)
	   nil)))

;;; Token parsing utilities
;;
(defun semantic-raw-to-cooked-token (token)
  "Convert TOKEN from a raw state to a cooked state.
The parser returns raw tokens with positional data START/END.  We
convert it from that to a cooked state with a property list and a
vector [START END].  Change the token with side effects and returns
TOKEN."
  (let* ((ncdr    (- (length token) 2))
	 (propcdr (if (natnump ncdr) (nthcdr ncdr token)))
	 (rngecdr (cdr propcdr))
	 ;; propcdr is the CDR containing the START from the token.
	 ;; rngecdr is the CDR containing the END from the token.
	 ;; PROPCDR will contain the property list after cooking.
	 ;; RNGECDR will contain the [START END] vector after cooking.
	 (range   (condition-case nil
                      (vector (car propcdr) (car rngecdr))
                    (error (debug token)
                           nil)))
         result expandedtokens)
    ;; Convert START/END into PROPERTIES/[START END].
    (setcar rngecdr range)
    (setcar propcdr nil)
    ;; Expand based on local configuration
    (if (not semantic-expand-nonterminal)
	;; No expanders
	(setq result (cons token result))
      ;; Glom generated tokens.  THESE TOKENS MUST BE VALID ONES!
      (setq expandedtokens (funcall semantic-expand-nonterminal token)
            result (if expandedtokens
                       (append expandedtokens result)
                     (cons token result))))
    result))

(defun semantic-bovinate-nonterminals (stream nonterm &optional
					      depth returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
DEPTH is optional, and defaults to 0.
Optional argument RETURNONERROR indicates that the parser should exit with
the current results on a parse error."
  (if (not depth) (setq depth semantic-flex-depth))
  (let ((result nil)
	(case-fold-search semantic-case-fold))
    (while stream
      (let* ((nontermsym
	      (semantic-bovinate-nonterminal
	       stream semantic-toplevel-bovine-table nonterm))
	     (token (car (cdr nontermsym))))
	(if (not nontermsym)
	    (error "Parse error @ %d" (car (cdr (car stream)))))
	(if token
	    (if (car token)
		(progn
		  (setq result (append (semantic-raw-to-cooked-token token)
				       result))
		  ;; Place the nonterm into the token.
		  (if (not (eq nonterm 'bovine-toplevel))
		      (semantic-token-put token 'reparse-symbol nonterm)))
	      ;; No error in this case, a purposeful nil means don't store
	      ;; anything.
	      )
	  (if returnonerror
	      (setq stream nil)
	    ;; The current item in the stream didn't match, so add it to
	    ;; the list of syntax items which didn't match.
	    (setq semantic-unmatched-syntax-cache
		  (cons (car stream) semantic-unmatched-syntax-cache))
	    ))
	;; Designated to ignore.
	(setq stream (car nontermsym)))
      (if stream
	  (if (eq semantic-bovination-working-type 'percent)
	      (working-status (floor
			       (* 100.0 (/ (float (car (cdr (car stream))))
					   (float (point-max))))))
	    (working-dynamic-status))))
    result))

(defun semantic-rebovinate-token (token)
  "Use TOKEN for extents, and reparse it, splicing it back into the cache."
  ;; Pre Hooks
  (run-hook-with-args 'semantic-pre-clean-token-hooks token)

  (let* ((flexbits (semantic-flex (semantic-token-start token)
				  (semantic-token-end token)))
	 ;; For embedded tokens (type parts, for example) we need a
	 ;; different symbol.  Come up with a plan to solve this.
	 (nonterminal (or (semantic-token-get token 'reparse-symbol)
			  'bovine-toplevel))
	 (new (semantic-bovinate-nonterminal
               flexbits
               semantic-toplevel-bovine-table
               nonterminal))
	 (cooked nil)
	 )
    (setq new (car (cdr new)))
    (if (not new)
        ;; Clever reparse failed, queuing full reparse.
        (setq semantic-toplevel-bovine-cache-check t)
      (setq cooked (semantic-raw-to-cooked-token new))
      (if (not (eq new (car cooked)))
          (if (= (length cooked) 1)
              ;; Cooking did a 1 to 1 replacement.  Use it.
              (setq new (car cooked))
          ;; If cooking results in multiple things, do a full reparse.
            (setq semantic-toplevel-bovine-cache-check t))))
    ;; Don't do much if we have to do a full recheck.
    (if semantic-toplevel-bovine-cache-check
        nil
      (semantic-overlay-token new)
      (let ((oo (semantic-token-overlay token))
            (o (semantic-token-overlay new)))
        ;; Copy all properties of the old overlay here.
        ;; I think I can use plists in emacs, but not in XEmacs.  Ack!
        (semantic-overlay-put o 'face (semantic-overlay-get oo 'face))
        (semantic-overlay-put o 'old-face (semantic-overlay-get oo 'old-face))
        (semantic-overlay-put o 'intangible (semantic-overlay-get oo 'intangible))
        (semantic-overlay-put o 'invisible (semantic-overlay-get oo 'invisible))
        ;; Free the old overlay(s)
        (semantic-deoverlay-token token)
        ;; Recover properties
        (let ((p (semantic-token-properties token)))
          (while p
            (semantic-token-put new (car (car p)) (cdr (car p)))
            (setq p (cdr p))))
        (if (not (eq nonterminal 'bovine-toplevel))
            (semantic-token-put new 'reparse-symbol nonterminal))
        (semantic-token-put new 'dirty nil)
        ;; Splice into the main list.
        (setcdr token (cdr new))
        (setcar token (car new))
        ;; This important bit is because the CONS cell representing
        ;; TOKEN is what we need here, even though the whole thing is
        ;; the same.
        (semantic-overlay-put o 'semantic token)
        ;; Hooks
        (run-hook-with-args 'semantic-clean-token-hooks token)
        )
      )))


;;; Semantic Bovination
;;
;; Take a semantic token stream, and convert it using the bovinator.
;; The bovinator takes a state table, and converts the token stream
;; into a new semantic stream defined by the bovination table.
;;
(defsubst semantic-bovinate-symbol-nonterminal-p (sym table)
  "Return non-nil if SYM is in TABLE, indicating it is NONTERMINAL."
  ;; sym is always a sym, so assq should be ok.
  (if (assq sym table) t nil))

(defmacro semantic-bovinate-nonterminal-db-nt ()
  "Return the current nonterminal symbol.
Part of the BNF source debugger.  Depends on the existing environment
of `semantic-bovinate-nonterminal'."
  `(if nt-stack
       (car (aref (car nt-stack) 2))
     nonterminal))

(defun semantic-bovinate-nonterminal-check (stream nonterminal)
  "Check if STREAM not already parsed for NONTERMINAL.
If so abort because an infinite recursive parse is suspected."
  (or (vectorp semantic-bovinate-nonterminal-check-obarray)
      (setq semantic-bovinate-nonterminal-check-obarray
            (make-vector 13 nil)))
  (let* ((nt (symbol-name nonterminal))
         (vs (symbol-value
              (intern-soft
               nt semantic-bovinate-nonterminal-check-obarray))))
    (if (memq stream vs)
        ;; Always enter debugger to see the backtrace
        (let ((debug-on-signal t)
              (debug-on-error  t))
          (setq semantic-bovinate-nonterminal-check-obarray nil)
          (error "Infinite recursive parse suspected on %s" nt))
      (set (intern nt semantic-bovinate-nonterminal-check-obarray)
           (cons stream vs)))))

(defun semantic-bovinate-nonterminal (stream table &optional nonterminal)
  "Bovinate STREAM based on the TABLE of nonterminal symbols.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Use `bovine-toplevel' if it is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (if (not nonterminal)
      (setq nonterminal 'bovine-toplevel))

  ;; Try to detect infinite recursive parse when doing a full reparse.
  (or semantic-toplevel-bovine-cache
      (semantic-bovinate-nonterminal-check stream nonterminal))

  (let ((matchlist (cdr (assq nonterminal table)))
	(starting-stream stream)
        (nt-loop  t)             ;non-terminal loop condition
        nt-popup                 ;non-nil if return from nt recursion
        nt-stack                 ;non-terminal recursion stack
        s                        ;Temp Stream Tracker
        lse                      ;Local Semantic Element
        lte                      ;Local matchlist element
        tev                      ;Matchlist entry values from buffer
        val                      ;Value found in buffer.
        cvl                      ;collected values list.
        out                      ;Output
        end                      ;End of match
        result
        )
    (condition-case nil
        (while nt-loop
          (catch 'push-non-terminal
            (setq nt-popup nil
                  end (cdr (cdr (car stream))))
            (while (or nt-loop nt-popup)
              (setq nt-loop nil
                    out     nil)
              (while (or nt-popup matchlist)
                (if nt-popup
                    ;; End of a non-terminal recursion
                    (setq nt-popup nil)
                  ;; New matching process
                  (setq s   stream      ;init s from stream.
                        cvl nil     ;re-init the collected value list.
                        lte (car matchlist) ;Get the local matchlist entry.
                        )
                  (if (or (byte-code-function-p (car lte))
                          (listp (car lte)))
                      ;; In this case, we have an EMPTY match!  Make
                      ;; stuff up.
                      (setq cvl (list nil))))
            
                (while (and lte
                            (not (byte-code-function-p (car lte)))
                            (not (listp (car lte))))

                  ;; BNF SOURCE DEBUGGING!
                  (if semantic-edebug
                      (let* ((db-nt   (semantic-bovinate-nonterminal-db-nt))
                             (db-ml   (cdr (assq db-nt table)))
                             (db-mlen (length db-ml))
                             (db-midx (- db-mlen (length matchlist)))
                             (db-tlen (length (nth db-midx db-ml)))
                             (db-tidx (- db-tlen (length lte))))
                        (if (eq 'fail
                                (semantic-bovinate-show
                                 (car s) db-nt db-midx db-tidx cvl))
                            (setq lte '(trash 0 . 0)))))
                  ;; END BNF SOURCE DEBUGGING!
              
                  (cond
                   ;; We have a nonterminal symbol.  Recurse inline.
                   ((setq nt-loop (assq (car lte) table))
          
                    (setq
                     ;; push state into the nt-stack
                     nt-stack (cons (vector matchlist cvl lte stream end
                                            )
                                    nt-stack)
                     ;; new non-terminal matchlist
                     matchlist   (cdr nt-loop)
                     ;; new non-terminal stream
                     stream      s)
               
                    (throw 'push-non-terminal t)

                    )
                   ;; Default case
                   (t
                    (setq lse (car s)   ;Get the local stream element
                          s   (cdr s))  ;update stream.
                    ;; Do the compare
                    (if (eq (car lte) (car lse)) ;syntactic match
                        (let ((valdot (cdr lse)))
                          (setq val (semantic-flex-text lse))
                          ;; DEBUG SECTION
                          (if semantic-dump-parse
                              (semantic-dump-detail
                               (if (stringp (car (cdr lte)))
                                   (list (car (cdr lte)) (car lte))
                                 (list (car lte)))
                               (semantic-bovinate-nonterminal-db-nt)
                               val
                               (if (stringp (car (cdr lte)))
                                   (if (string-match (car (cdr lte)) val)
                                       "Term Match" "Term Fail")
                                 "Term Type=")))
                          ;; END DEBUG SECTION
                          (setq lte (cdr lte))
                          (if (stringp (car lte))
                              (progn
                                (setq tev (car lte)
                                      lte (cdr lte))
                                (if (string-match tev val)
                                    (setq cvl (cons
                                               (if (memq (car lse)
                                                         '(comment semantic-list))
                                                   valdot val)
                                               cvl)) ;append this value
                                  (setq lte nil cvl nil))) ;clear the entry (exit)
                            (setq cvl (cons
                                       (if (memq (car lse)
                                                 '(comment semantic-list))
                                           valdot val) cvl))) ;append unchecked value.
                          (setq end (cdr (cdr lse)))
                          )
                      (if (and semantic-dump-parse nil)
                          (semantic-dump-detail (car lte)
                                                (semantic-bovinate-nonterminal-db-nt)
                                                (semantic-flex-text lse)
                                                "Term Type Fail"))
                      (setq lte nil cvl nil)) ;No more matches, exit
                    )))
                (if (not cvl)           ;lte=nil;  there was no match.
                    (setq matchlist (cdr matchlist)) ;Move to next matchlist entry
                  (let ((start (car (cdr (car stream)))))
                    (setq out (cond
                               ((car lte)
                        
              ;; REMOVE THIS TO USE THE REFERENCE/COMPARE CODE
              ;;(let ((o (apply (car lte) ;call matchlist fn on values
              ;;                (nreverse cvl) start (list end))))
              ;;  (if semantic-bovinate-create-reference
              ;;      (semantic-bovinate-add-reference o))
              ;;  (if semantic-bovinate-compare-reference
              ;;      (semantic-bovinate-compare-against-reference o))
              ;;  o
              ;;  )
                            
                                (funcall (car lte) ;call matchlist fn on values
                                         (nreverse cvl) start end))
                               ((and (= (length cvl) 1)
                                     (listp (car cvl))
                                     (not (numberp (car (car cvl)))))
                                (append (car cvl) (list start end)))
                               (t
                                ;;(append (nreverse cvl) (list start end))))
                                ;; MAYBE THE FOLLOWING NEEDS LESS CONS
                                ;; CELLS THAN THE ABOVE?
                                (nreverse (cons end (cons start cvl)))))
                          matchlist nil) ;;generate exit condition
                    (if (not end)
                        (setq out nil)))
                  ;; Nothin?
                  ))
              (setq result
                    (if (eq s starting-stream)
                        (list (cdr s) nil)
                      (list s out)))
              (if nt-stack
                  ;; pop previous state from the nt-stack
                  (let ((state (car nt-stack)))

                    (setq nt-popup    t
                          ;; pop actual parser state
                          matchlist   (aref state 0)
                          cvl         (aref state 1)
                          lte         (aref state 2)
                          stream      (aref state 3)
                          end         (aref state 4)
                          ;; update the stack
                          nt-stack    (cdr nt-stack))
                
                    (if out
                        (let ((len (length out))
                              (strip (nreverse (cdr (cdr (reverse out))))))
                          (if semantic-dump-parse
                              (semantic-dump-detail (cdr result)
                                                    (car lte)
                                                    ""
                                                    "NonTerm Match"))
                          (setq end (nth (1- len) out) ;reset end to the end of exp
                                cvl (cons strip cvl) ;prepend value of exp
                                lte (cdr lte)) ;update the local table entry
                          )
                      ;; No value means that we need to terminate this
                      ;; match.
                      (setq lte nil cvl nil)) ;No match, exit
                    )))))
      (error
       ;; On error just move forward the stream of lexical tokens
       (setq result (list (cdr starting-stream) nil))))
      result))


;;; Bovine table functions
;;
;; These are functions that can be called from within a bovine table.
;; Most of these have code auto-generated from other construct in the BNF.
(defmacro semantic-lambda (&rest return-val)
  "Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table."
  `(lambda (vals start end)
     (append ,@return-val (list start end))))

(defun semantic-bovinate-from-nonterminal (start end nonterm
						 &optional depth length)
  "Bovinate from within a nonterminal lambda from START to END.
Depends on the existing environment created by `semantic-bovinate-stream'.
Argument NONTERM is the nonterminal symbol to start with.
Optional argument DEPTH is the depth of lists to dive into.
When used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part.
Optional argument LENGTH specifies we are only interested in LENGTH tokens."
  (car-safe (cdr (semantic-bovinate-nonterminal
		  (semantic-flex start end (or depth 1) length)
		  ;; the byte compiler will complain about TABLE
		  table
		  nonterm))))

(defun semantic-bovinate-from-nonterminal-full (start end nonterm
						      &optional depth)
  "Bovinate from within a nonterminal lambda from START to END.
Iterates until all the space between START and END is exhausted.
Depends on the existing environment created by `semantic-bovinate-stream'.
Argument NONTERM is the nonterminal symbol to start with.
If NONTERM is nil, use `bovine-block-toplevel'.
Optional argument DEPTH is the depth of lists to dive into.
When used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (nreverse
   (semantic-bovinate-nonterminals (semantic-flex start end (or depth 1))
				   nonterm
				   depth)))

(defun semantic-bovinate-region-until-error (start end nonterm &optional depth)
  "Bovinate between START and END starting with NONTERM.
Optional DEPTH specifies how many levels of parenthesis to enter.
This command will parse until an error is encountered, and return
the list of everything found until that moment.
This is meant for finding variable definitions at the beginning of
code blocks in methods.  If `bovine-inner-scope' can also support
commands, use `semantic-bovinate-from-nonterminal-full'."
  (nreverse
   (semantic-bovinate-nonterminals (semantic-flex start end depth)
				   nonterm
				   depth
				   ;; This says stop on an error.
				   t)))

(defun semantic-bovinate-make-assoc-list (&rest args)
  "Create an association list with ARGS.
Args are of the form (KEY1 VALUE1 ... KEYN VALUEN).
The return value will be of the form: ((KEY1 .  VALUE1) ... (KEYN . VALUEN))
Where KEY is a symbol, and VALUE is the value for that symbol.
If VALUE is nil, then KEY is excluded from the return association list."
  (let ((ret nil))
    (while args
      (let ((value (car-safe (cdr args))))
	(if (and value
		 (or (not (stringp value))
		     (not (string= value "")))
		 (or (not (numberp value))
		     (not (= value 0))))
	    (setq ret (cons (cons (car args) (car (cdr args))) ret)))
	(setq args (cdr (cdr args)))))
    (nreverse ret)))

;;; Debugging in bovine tables
;;
(defun semantic-dump-buffer-init ()
  "Initialize the semantic dump buffer."
  (save-excursion
    (let ((obn (buffer-name)))
      (set-buffer (get-buffer-create "*Semantic Dump*"))
      (erase-buffer)
      (insert "Parse dump of " obn "\n\n")
      (insert (format "%-15s %-15s %10s %s\n\n"
		      "Nonterm" "Comment" "Text" "Context"))
      )))

(defun semantic-dump-detail (lse nonterminal text comment)
  "Dump info about this match.
Argument LSE is the current syntactic element.
Argument NONTERMINAL is the nonterminal matched.
Argument TEXT is the text to match.
Argument COMMENT is additional description."
  (save-excursion
    (set-buffer "*Semantic Dump*")
    (goto-char (point-max))
    (insert (format "%-15S %-15s %10s %S\n" nonterminal comment text lse)))
  )

(defvar semantic-bovinate-debug-table nil
  "A marker where the current table we are debugging is.")

(defun semantic-bovinate-debug-set-table (&optional clear)
  "Set the table for the next debug to be here.
Optional argument CLEAR to unset the debug table."
  (interactive "P")
  (if clear (setq semantic-bovinate-debug-table nil)
    (if (not (eq major-mode 'emacs-lisp-mode))
	(error "Not an Emacs Lisp file"))
    (beginning-of-defun)
    (setq semantic-bovinate-debug-table (point-marker))))
  
;; We will get warnings in here about semantic-bnf-* fns.
;; We cannot require semantic-bnf due to compile errors.
(defun semantic-bovinate-debug-buffer ()
  "Bovinate the current buffer in debug mode."
  (interactive)
  (if (and (not semantic-toplevel-bovine-table-source)
	   (not semantic-bovinate-debug-table))
      (error
       "Call `semantic-bovinate-debug-set-table' from your semantic table"))
  (delete-other-windows)
  (split-window-vertically)
  (if semantic-bovinate-debug-table
      (switch-to-buffer (marker-buffer semantic-bovinate-debug-table))
    (if (not semantic-toplevel-bovine-table-source)
        (error "No debuggable BNF source found"))
    (require 'semantic-bnf)
    (switch-to-buffer (semantic-bnf-find-source-on-load-path
                       semantic-toplevel-bovine-table-source)))
  (other-window 1)
  (semantic-clear-toplevel-cache)
  (let ((semantic-edebug t))
    (semantic-bovinate-toplevel)))

(defun semantic-bovinate-show (lse nonterminal matchlen tokenlen collection)
  "Display some info about the current parse.
Returns 'fail if the user quits, nil otherwise.
LSE is the current listed syntax element.
NONTERMINAL is the current nonterminal being parsed.
MATCHLEN is the number of match lists tried.
TOKENLEN is the number of match tokens tried.
COLLECTION is the list of things collected so far."
  (let* ((semantic-edebug nil)
         (ol1 nil) (ol2 nil) (ret nil)
         (bnf-buffer (semantic-bnf-find-source-on-load-path
                      semantic-toplevel-bovine-table-source)))
    (unwind-protect
	(progn
	  (goto-char (car (cdr lse)))
	  (setq ol1 (semantic-make-overlay (car (cdr lse)) (cdr (cdr lse))))
	  (semantic-overlay-put ol1 'face 'highlight)
	  (goto-char (car (cdr lse)))
	  (if window-system nil (sit-for 1))
	  (other-window 1)
	  (let (s e)
	    (if semantic-bovinate-debug-table
		(progn
		  (set-buffer (marker-buffer semantic-bovinate-debug-table))
		  (goto-char semantic-bovinate-debug-table)
		  (re-search-forward
		   (concat "^\\s-*\\((\\|['`]((\\)\\(" (symbol-name nonterminal)
			   "\\)[ \t\n]+(")
		   nil t)
		  (setq s (match-beginning 2)
			e (match-end 2))
		  (forward-char -2)
		  (forward-list matchlen)
		  (skip-chars-forward " \t\n(")
		  (forward-sexp tokenlen)
		  )
	      ;; The user didn't specify a lisp level table.
	      ;; go to the source...
	      (set-buffer bnf-buffer)
	      (semantic-bnf-find-state-position
	       nonterminal matchlen tokenlen)
	      (save-excursion
		(goto-char (semantic-token-start (semantic-current-nonterminal)))
		(setq s (point)
		      e (progn (forward-sexp 1) (point))))
	      )
	    (setq ol2 (semantic-make-overlay s e))
	    (semantic-overlay-put ol2 'face 'highlight)
	    )
	  (message "%s: %S" lse collection)
	  (let ((e (semantic-read-event)))
	    (cond ((eq e ?f)		;force a failure on this symbol.
		   (setq ret 'fail))
		  ((eq e ?a)		;Abort this syntax element
		   (error "Abort"))
		  ((eq e ?q)		;Quit this debug session
		   (signal 'quit "Abort"))
		  (t nil)))
	  (other-window 1)
	  )
      (semantic-overlay-delete ol1)
      (semantic-overlay-delete ol2))
    ret))

;;; Reference Debugging
;;
(defvar semantic-bovinate-create-reference nil
  "Non nil to create a reference.")

(defvar semantic-bovinate-reference-token-list nil
  "A list generated as a reference (assumed valid).
A second pass compares return values against this list.")

(defun semantic-bovinate-add-reference (ref)
  "Add REF to the reference list."
  (setq semantic-bovinate-reference-token-list
	(cons ref semantic-bovinate-reference-token-list)))

(defvar semantic-bovinate-compare-reference nil
  "Non nil to compare against a reference list.")

(defvar semantic-bovinate-reference-temp-list nil
  "List used when doing a compare.")

(defun semantic-bovinate-compare-against-reference (ref)
  "Compare REF against what was returned last time."
  (if (not (equal ref (car semantic-bovinate-reference-temp-list)))
      (let ((debug-on-error t))
	(error "Stop: %d %S != %S"
	       (- (length semantic-bovinate-reference-token-list)
		  (length semantic-bovinate-reference-temp-list))
	       (car semantic-bovinate-reference-temp-list)
	       ref))
    (setq semantic-bovinate-reference-temp-list
	  (cdr semantic-bovinate-reference-temp-list))))
	   
(defun bovinate-create-reference ()
  "Create a reference list."
  (interactive)
  (condition-case nil
      (progn
	(semantic-clear-toplevel-cache)
	(setq semantic-bovinate-create-reference t
	      semantic-bovinate-reference-token-list nil)
	(bovinate)
	(setq semantic-bovinate-reference-token-list
	      (nreverse semantic-bovinate-reference-token-list)))
    (error nil))
  (setq semantic-bovinate-create-reference nil))

(defun bovinate-reference-compare ()
  "Compare the current parsed output to the reference list.
Create a reference with `bovinate-create-reference'."
  (interactive)
  (let ((semantic-bovinate-compare-reference t))
    (semantic-clear-toplevel-cache)
    (setq semantic-bovinate-reference-temp-list
	  semantic-bovinate-reference-token-list)
    (bovinate)))


;;; Semantic Flexing
;;
;; This is a simple scanner which uses the syntax table to generate
;; a stream of simple tokens.
;;
;; A flex element is of the form:
;;  (SYMBOL START . END)
;; Where symbol is the type of thing it is.  START and END mark that
;; objects boundary.

(eval-and-compile (if (not (fboundp 'with-syntax-table))

;; Copied from Emacs 21 for compatibility with released Emacses.
(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to a copy of TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table (copy-syntax-table ,table))
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

))

;;; Keyword Table Handling.
;;
(defvar semantic-flex-keywords-obarray nil
  "Buffer local keyword obarray for the lexical analyzer.
These keywords are matched explicitly, and converted into special symbols.")
(make-variable-buffer-local 'semantic-flex-keywords-obarray)

(defun semantic-flex-make-keyword-table (keywords &optional propertyalist)
  "Convert a list of KEYWORDS into an obarray.
Save the obarray into `semantic-flex-keywords-obarray'.
If optional argument PROPERTYALIST is non nil, then interpret it, and
apply those properties"
  ;; Create the symbol hash table
  (let ((obarray (make-vector 13 nil)))
    ;; fill it with stuff
    (while keywords
      (set (intern (car (car keywords)) obarray)
	   (cdr (car keywords)))
      (setq keywords (cdr keywords)))
    ;; Apply all properties
    (let ((semantic-flex-keywords-obarray obarray))
      (while propertyalist
	(semantic-flex-keyword-put (car (car propertyalist))
				   (nth 1 (car propertyalist))
				   (nth 2 (car propertyalist)))
	(setq propertyalist (cdr propertyalist))))
    obarray))

(defsubst semantic-flex-keyword-p (text)
  "Return non-nil if TEXT is a keyword in the keyword table.
Return nil if TEXT is not in the symbol table."
  (symbol-value (intern-soft text semantic-flex-keywords-obarray)))

(defun semantic-flex-keyword-put (text property value)
  "For keyword TEXT, set PROPERTY to VALUE."
  (let ((sym (intern-soft text semantic-flex-keywords-obarray)))
    (if (not sym) (signal 'wrong-type-argument (list text 'keyword)))
    (put sym property value)))

(defun semantic-flex-keyword-get (text property)
  "For keyword TEXT, get the value of PROPERTY."
  (let ((sym (intern-soft text semantic-flex-keywords-obarray)))
    (if (not sym) (signal 'wrong-type-argument (list text 'keyword)))
    (get sym property)))

;; David Ponce
(defun semantic-flex-map-keywords (fun &optional property)
  "Call function FUN on every semantic keyword.
If optional PROPERTY is non-nil, call FUN only on every keyword which
as a PROPERTY value.  FUN receives a semantic keyword as argument."
  (if (arrayp semantic-flex-keywords-obarray)
      (mapatoms
       (function
        (lambda (keyword)
          (and keyword
               (or (null property) (get keyword property))
               (funcall fun keyword))))
       semantic-flex-keywords-obarray)))

;; David Ponce
(defun semantic-flex-keywords (&optional property)
  "Return a list of semantic keywords.
If optional PROPERTY is non-nil, return only keywords which have a
PROPERTY set."
  (let (keywords)
    (semantic-flex-map-keywords
     (function
      (lambda (keyword)
        (setq keywords (cons keyword keywords))))
     property)
    keywords))

;;; Lexical Analysis
;;
(defvar semantic-flex-tokens
  '(
    (bol)
    (charquote)
    (close-paren)
    (comment)
    (newline)
    (open-paren)
    (punctuation)
    (semantic-list)
    (string)
    (symbol)
    (whitespace)
    )
  "An alist of of semantic token types.
As of December 2001 (semantic 1.4beta13), this variable is not used in
any code.  The only use is to refer to the doc-string from elsewhere.

The key to this alist is the symbol representing token type that
\\[semantic-flex] returns. These are

  - bol:           Empty string matching a beginning of line.
                   This token is produced only if the user set
                   `semantic-flex-enable-bol' to non-nil.

  - charquote:     String sequences that match `\\s\\+' regexp.

  - close-paren:   Characters that match `\\s)' regexp.
                   These are typically `)', `}', `]', etc.

  - comment:       A comment chunk.  These token types are not
                   produced by default.  They are produced only if the
                   user set `semantic-ignore-comments' to `nil'.

  - newline        Characters matching `\\s-*\\(\n\\|\\s>\\)' regexp.
                   This token is produced only if the user set
                   `semantic-flex-enable-newlines' to non-nil.

  - open-paren:    Characters that match `\\s(' regexp.
                   These are typically `(', `{', `[', etc.
                   Note that these are not usually generated unless
                   the `depth' argument to \\[semantic-flex] is
                   greater than 0.

  - punctuation:   Characters matching `{\\(\\s.\\|\\s$\\|\\s'\\)'
                   regexp.

  - semantic-list: String delimited by matching parenthesis, braces,
                   etc. that the lexer skipped over, because the
                   `depth' parameter to \\[semantic-flex] was not high
                   enough.

  - string:        Quoted strings, i.e., string sequences that start
                   and end with characters matching `\\s\"'
                   regexp. The lexer relies on @code{forward-sexp} to
                   find the matching end.

  - symbol:        String sequences that match `\\(\\sw\\|\\s_\\)+'
                   regexp.

  - whitespace:    Characters that match `\\s-+' regexp.
                   This token is produced only if the user set
                   `semantic-flex-enable-whitespace' to non-nil.
                   If `semantic-ignore-comments' is non-nil too
                   comments are considered as whitespaces.
")

(defvar semantic-flex-unterminated-syntax-end-function
  (lambda (syntax syntax-start flex-end) flex-end)
  "Function called when unterminated syntax is encountered.
This should be set to one function.  That function should take three
parameters.  The SYNTAX, or type of syntax which is unterminated.
SYNTAX-START where the broken syntax begins.
FLEX-END is where the lexical analysis was asked to end.
This function can be used for languages that can intelligently fix up
broken syntax, or the exit lexical analysis via `throw' or `signal'
when finding unterminated syntax.")

(defvar semantic-flex-extensions nil
  "Buffer local extensions to the lexical analyzer.
This should contain an alist with a key of a regex and a data element of
a function.  The function should both move point, and return a lexical
token of the form:
  ( TYPE START .  END)
nil is also a valid return value.
TYPE can be any type of symbol, as long as it doesn't occur as a
nonterminal in the language definition.")
(make-variable-buffer-local 'semantic-flex-extensions)

(defvar semantic-flex-syntax-modifications nil
  "Changes to the syntax table for this buffer.
These changes are active only while the buffer is being flexed.
This is a list where each element has the form:
  (CHAR CLASS)
CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what syntax class CHAR has.")
(make-variable-buffer-local 'semantic-flex-syntax-modifications)

(defvar semantic-flex-enable-newlines nil
  "When flexing, report 'newlines as syntactic elements.
Useful for languages where the newline is a special case terminator.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-newlines)

(defvar semantic-flex-enable-whitespace nil
  "When flexing, report 'whitespace as syntactic elements.
Useful for languages where the syntax is whitespace dependent.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-whitespace)

(defvar semantic-flex-enable-bol nil
  "When flexing, report beginning of lines as syntactic elements.
Useful for languages like python which are indentation sensitive.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-bol)

(defvar semantic-number-expression
  ;; This expression was written by David Ponce for Java, and copied
  ;; here for C and any other similar language.
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Regular expression for matching a number.
If this value is nil, no number extraction is done during lex.
This expression tries to match C and Java like numbers.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

(make-variable-buffer-local 'semantic-number-expression)

(defun semantic-flex (start end &optional depth length)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element is a list, such of
the form (symbol start-expression . end-expression) where SYMBOL
denotes the token type.
See `semantic-flex-tokens' variable for details on token types.
END does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.
The last argument, LENGTH specifies that `semantic-flex' should only
return LENGTH tokens."
  ;;(message "Flexing muscles...")
  (if (not semantic-flex-keywords-obarray)
      (setq semantic-flex-keywords-obarray [ nil ]))
  (let ((ts nil)
        (pos (point))
        (ep nil)
        (curdepth 0)
        (cs (if comment-start-skip
                (concat "\\(\\s<\\|" comment-start-skip "\\)")
              (concat "\\(\\s<\\)")))
        (newsyntax (copy-syntax-table (syntax-table)))
        (mods semantic-flex-syntax-modifications)
        ;; Use the default depth if it is not specified.
        (depth (or depth semantic-flex-depth)))
    ;; Update the syntax table
    (while mods
      (modify-syntax-entry (car (car mods)) (car (cdr (car mods))) newsyntax)
      (setq mods (cdr mods)))
    (with-syntax-table newsyntax
      (goto-char start)
      (while (and (< (point) end) (or (not length) (<= (length ts) length)))
        (cond
         ;; catch beginning of lines when needed.
         ;; Must be done before catching any other tokens!
         ((and semantic-flex-enable-bol
               (bolp)
               ;; Just insert a (bol N . N) token in the token stream,
               ;; without moving the point.  N is the point at the
               ;; beginning of line.
               (setq ts (cons (cons 'bol (cons (point) (point))) ts))
               nil)) ;; CONTINUE
         ;; special extensions, includes whitespace, nl, etc.
         ((and semantic-flex-extensions
               (let ((fe semantic-flex-extensions)
                     (r nil))
                 (while fe
                   (if (looking-at (car (car fe)))
                       (setq ts (cons (funcall (cdr (car fe))) ts)
                             r t
                             fe nil
                             ep (point)))
                   (setq fe (cdr fe)))
                 (if (and r (not (car ts))) (setq ts (cdr ts)))
                 r)))
         ;; catch newlines when needed
         ((looking-at "\\s-*\\(\n\\|\\s>\\)")
          (if semantic-flex-enable-newlines
              (setq ep (match-end 1)
                    ts (cons (cons 'newline
                                   (cons (match-beginning 1) ep))
                             ts))))
         ;; catch whitespace when needed
         ((looking-at "\\s-+")
          (if semantic-flex-enable-whitespace
              ;; Language wants whitespaces, link them together.
              (if (eq (car (car ts)) 'whitespace)
                  (setcdr (cdr (car ts)) (match-end 0))
                (setq ts (cons (cons 'whitespace
                                     (cons (match-beginning 0)
                                           (match-end 0)))
                               ts)))))
         ;; numbers
         ((and semantic-number-expression
               (looking-at semantic-number-expression))
          (setq ts (cons (cons 'number
                               (cons (match-beginning 0)
                                     (match-end 0)))
                         ts)))
         ;; symbols
         ((looking-at "\\(\\sw\\|\\s_\\)+")
          (setq ts (cons (cons
                          ;; Get info on if this is a keyword or not
                          (or (semantic-flex-keyword-p (match-string 0))
                              'symbol)
                          (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Character quoting characters (ie, \n as newline)
         ((looking-at "\\s\\+")
          (setq ts (cons (cons 'charquote
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Open parens, or semantic-lists.
         ((looking-at "\\s(")
          (if (or (not depth) (< curdepth depth))
              (progn
                (setq curdepth (1+ curdepth))
                (setq ts (cons (cons 'open-paren
                                     (cons (match-beginning 0) (match-end 0)))
                               ts)))
            (setq ts (cons
                      (cons 'semantic-list
                            (cons (match-beginning 0)
                                  (save-excursion
                                    (condition-case nil
                                        (forward-list 1)
                                      ;; This case makes flex robust
                                      ;; to broken lists.
                                      (error
                                       (goto-char
                                        (funcall
                                         semantic-flex-unterminated-syntax-end-function
                                         'semantic-list
                                         start end))))
                                    (setq ep (point)))))
                      ts))))
         ;; Close parens
         ((looking-at "\\s)")
          (setq ts (cons (cons 'close-paren
                               (cons (match-beginning 0) (match-end 0)))
                         ts))
          (setq curdepth (1- curdepth)))
         ;; String initiators
         ((looking-at "\\s\"")
          ;; Zing to the end of this string.
          (setq ts (cons (cons 'string
                               (cons (match-beginning 0)
                                     (save-excursion
                                       (condition-case nil
                                           (forward-sexp 1)
                                         ;; This case makes flex
                                         ;; robust to broken strings.
                                         (error
                                          (goto-char
                                           (funcall
                                            semantic-flex-unterminated-syntax-end-function
                                            'string
                                            start end))))
                                       (setq ep (point)))))
                         ts)))
         ;; comments
         ((looking-at cs)
          (if (and semantic-ignore-comments
                   (not semantic-flex-enable-whitespace))
              ;; If the language doesn't deal with comments nor
              ;; whitespaces, ignore them here.
              (let ((comment-start-point (point)))
                (forward-comment 1)
                (if (eq (point) comment-start-point)
                    ;; In this case our start-skip string failed
                    ;; to work properly.  Lets try and move over
                    ;; whatever white space we matched to begin
                    ;; with.
                    (skip-syntax-forward "-.'"
                                         (save-excursion
                                           (end-of-line)
                                           (point)))
                  ;;(forward-comment 1)
                  ;; Generate newline token if enabled
                  (if (and semantic-flex-enable-newlines
                           (bolp))
                      (backward-char 1)))
                (if (eq (point) comment-start-point)
                    (error "Strange comment syntax prevents lexical analysis"))
                (setq ep (point)))
            (let ((tk (if semantic-ignore-comments 'whitespace 'comment)))
              (save-excursion
                (forward-comment 1)
                ;; Generate newline token if enabled
                (if (and semantic-flex-enable-newlines
                         (bolp))
                    (backward-char 1))
                (setq ep (point)))
              ;; Language wants comments or want them as whitespaces,
              ;; link them together.
              (if (eq (car (car ts)) tk)
                  (setcdr (cdr (car ts)) ep)
                (setq ts (cons (cons tk (cons (match-beginning 0) ep))
                               ts))))))
         ;; punctuation
         ((looking-at "\\(\\s.\\|\\s$\\|\\s'\\)")
          (setq ts (cons (cons 'punctuation
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; unknown token
         (t
          (error "What is that?")))
        (goto-char (or ep (match-end 0)))
        (setq ep nil)))
    ;; maybe catch the last beginning of line when needed
    (and semantic-flex-enable-bol
         (= (point) end)
         (bolp)
         (setq ts (cons (cons 'bol (cons (point) (point))) ts)))
    (goto-char pos)
    ;;(message "Flexing muscles...done")
    (nreverse ts)))

(defsubst semantic-flex-buffer (&optional depth)
  "Semantically flex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-flex (point-min) (point-max) depth))

(defsubst semantic-flex-list (semlist depth)
  "Flex the body of SEMLIST to DEPTH."
  (semantic-flex (semantic-flex-start semlist)
                 (semantic-flex-end   semlist)
                 depth))

;;; Settings and autoloads
;;
(autoload 'semantic-create-imenu-index "semantic-imenu"
  "Create an imenu index for any buffer which supports Semantic.")
(autoload 'senator-minor-mode "senator"
  "Minor mode for the SEmantic NAvigaTOR." t)
(autoload 'global-semanticdb-minor-mode "semanticdb"
  "Mode saving token lists between sessions." t)

(provide 'semantic)

;;; semantic.el ends here

;; Semantic-util is a part of the semantic API.  Include it last
;; because it depends on semantic.
(require 'semantic-util)
