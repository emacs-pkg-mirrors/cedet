;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.147 2002/07/15 10:27:20 ponced Exp $

(defvar semantic-version "2.0alpha2"
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
(require 'semantic-lex)

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

(defvar semantic-bovinate-parser nil
  "Function used to parse input stream.
See the default parser `semantic-bovinate-nonterminal-default' for
details.")
(make-variable-buffer-local 'semantic-bovinate-parser)

(defvar semantic-bovinate-parser-name nil
  "Optional name of the parser used to parse input stream.")
(make-variable-buffer-local 'semantic-bovinate-parser-name)

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

(defvar semantic-bovinate-nonterminal-check-obarray nil
  "Obarray of streams already parsed for nonterminal symbols.")
(make-variable-buffer-local 'semantic-bovinate-nonterminal-check-obarray)

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

(defvar semantic-before-toplevel-cache-flush-hook nil
  "Hooks run before the toplevel nonterminal cache is flushed.
For language specific hooks, make sure you define this as a local
hook.  This hook is called before a corresponding
`semantic-after-toplevel-cache-change-hook' which is also called
during a flush when the cache is given a new value of nil.")

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
	  (semantic-flex-text (car (semantic-lex p (1+ p)))))
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
    ;; Here are some buffer local variables we can initialize ourselves
    ;; of a mode does not choose to do so.
    (semantic-lex-init)
    ;; Setup for a needed reparse.
    (setq semantic-toplevel-bovine-force-reparse t)
    ;; Call DB hooks before regular init hooks
    (run-hooks 'semantic-init-db-hooks)
    ;; Lastly, set up semantic modes
    (run-hooks 'semantic-init-hooks)
    ))

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

(defsubst semantic-bovination-working-message (&optional arg)
  "Return the message string displayed while parsing.
If optional argument ARG is non-nil it is appended to the message
string.  See also the function `working-status-forms'."
  (if semantic-bovinate-parser-name
      (format "%s/%s" semantic-bovinate-parser-name (or arg ""))
    (format "%s" (or arg ""))))

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
        (working-status-forms
            (semantic-bovination-working-message (buffer-name))
            "done"
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
	  (lex (semantic-lex (point-min) (point-max)))
          res)
      ;; Init a dump
      (if semantic-dump-parse
          (semantic-dump-buffer-init))
      ;; Clear the caches
      (semantic-clear-toplevel-cache)
      ;; Parse!
      (working-status-forms
          (semantic-bovination-working-message (buffer-name))
          "done"
	(setq res (semantic-bovinate-nonterminals
                   lex nil semantic-flex-depth))
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
(defsubst semantic-cooked-token-p (token)
  "Return non-nil if TOKEN is a cooked one.
See also the function `semantic-raw-to-cooked-token'."
  ;; In fact a cooked token is actually a list of cooked tokens
  ;; because a raw token can be expanded in several cooked ones!
  (when (consp token)
    (while (and (semantic-token-p (car token))
                (vectorp (semantic-token-overlay (car token))))
      (setq token (cdr token)))
    (null token)))

(defun semantic-raw-to-cooked-token (token)
  "Convert TOKEN from a raw state to a cooked state.
The parser returns raw tokens with positional data START/END.  We
convert it from that to a cooked state with a property list and a
vector [START END].  The raw token is changed with side effects and
maybe expanded in several cooked tokens when the variable
`semantic-expand-nonterminal' is set.  So this function always returns
a list of cooked tokens."
  ;; Because some parsers can return tokens already cooked (wisent is
  ;; an example), check if TOKEN was already cooked to just return it.
  (if (semantic-cooked-token-p token)
      token
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
      result)))


;;; Bovine table runtime functions
;;
(defun semantic-bovinate-nonterminals (stream nonterm &optional
					      depth returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
DEPTH is optional, and defaults to 0.
Optional argument RETURNONERROR indicates that the parser should exit
with the current results on a parse error."
  (if (not depth) (setq depth semantic-flex-depth))
  (let ((result nil)
	(case-fold-search semantic-case-fold)
        nontermsym token)
    (while stream
      (setq nontermsym (semantic-bovinate-nonterminal
                        stream semantic-toplevel-bovine-table nonterm)
            token (car (cdr nontermsym)))
      (if (not nontermsym)
          (error "Parse error @ %d" (car (cdr (car stream)))))
      (if token
          (if (car token)
              (setq token (mapcar
                           #'(lambda (token)
                               ;; Set the 'reparse-symbol property to
                               ;; NONTERM unless it was already setup
                               ;; by a token expander
                               (or (semantic-token-get
                                    token 'reparse-symbol)
                                   (semantic-token-put
                                    token 'reparse-symbol nonterm))
                               token)
                           (semantic-raw-to-cooked-token token))
                    result (append token result))
            ;; No error in this case, a purposeful nil means don't
            ;; store anything.
            )
        (if returnonerror
            (setq stream nil)
          ;; The current item in the stream didn't match, so add it to
          ;; the list of syntax items which didn't match.
          (setq semantic-unmatched-syntax-cache
                (cons (car stream) semantic-unmatched-syntax-cache))
          ))
      ;; Designated to ignore.
      (setq stream (car nontermsym))
      (if stream
	  (if (eq semantic-bovination-working-type 'percent)
	      (working-status (floor
			       (* 100.0 (/ (float (car (cdr (car stream))))
					   (float (point-max))))))
	    (working-dynamic-status))))
    result))

(defun semantic-bovinate-nonterminal (stream table &optional nonterminal)
  "Bovinate STREAM based on the TABLE of nonterminal symbols.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (funcall (or semantic-bovinate-parser
               #'semantic-bovinate-nonterminal-default)
           stream
           table
           nonterminal))

;; These are functions that can be called from within a bovine table.
;; Most of these have code auto-generated from other construct in the
;; BNF.

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
		  (semantic-lex start end (or depth 1) length)
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
   (semantic-bovinate-nonterminals (semantic-lex start end (or depth 1))
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
   (semantic-bovinate-nonterminals (semantic-lex start end depth)
				   nonterm
				   depth
				   ;; This says stop on an error.
				   t)))

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
(require 'semantic-edit)
