;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.166 2003/02/01 02:49:34 zappo Exp $

(defvar semantic-version "2.0beta1"
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

(require 'semantic-fw)

;;; Code:
;;

;;; Variables and Configuration
;;
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

(defcustom semantic-dump-parse nil
  "When non-nil, dump parsing information."
  :group 'semantic
  :type 'boolean)

(defvar semantic-parser-name "LL"
  "Optional name of the parser used to parse input stream.")
(make-variable-buffer-local 'semantic-parser-name)

;;; Parse tree state management API
;;
(defvar semantic-parse-tree-state 'needs-rebuild
  "State of the current parse tree.")
(make-variable-buffer-local 'semantic-parse-tree-state)

(defmacro semantic-parse-tree-set-needs-update ()
  "Indicate that the current parse tree needs to be updated.
The parse tree can be updated by `semantic-parse-changes'."
  `(setq semantic-parse-tree-state 'needs-update))

(defmacro semantic-parse-tree-needs-update-p ()
  "Return non-nil if the current parse tree needs to be updated."
  `(eq semantic-parse-tree-state 'needs-update))

(defmacro semantic-parse-tree-set-needs-rebuild ()
  "Indicate that the current parse tree needs to be rebuilt.
The parse tree must be rebuilt by `semantic-parse-region'."
  `(setq semantic-parse-tree-state 'needs-rebuild))

(defmacro semantic-parse-tree-needs-rebuild-p ()
  "Return non-nil if the current parse tree needs to be rebuilt."
  `(eq semantic-parse-tree-state 'needs-rebuild))

(defmacro semantic-parse-tree-set-up-to-date ()
  "Indicate that the current parse tree is up to date."
  `(setq semantic-parse-tree-state nil))

(defmacro semantic-parse-tree-up-to-date-p ()
  "Return non-nil if the current parse tree is up to date."
  `(null semantic-parse-tree-state))

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

(defsubst semantic-active-p ()
  "Return non-nil if the current buffer was set up for parsing."
  semantic-toplevel-bovine-table)

(defsubst semantic-bovine-umatched-syntax-refresh-needed-p  ()
  "Return non-nil if the unmatched syntax cache needs a refresh.
That is if it is dirty or if the current parse tree isn't up to date."
  (or semantic-unmatched-syntax-cache-check
      (not (semantic-parse-tree-up-to-date-p))))

(defun semantic-new-buffer-fcn ()
  "Setup Semantic in the current buffer.
Runs `semantic-init-hook' if the major mode is setup to use Semantic."
  ;; Make sure variables are set up for this mode.
  (semantic-activate-mode-bindings)
  ;; Do stuff if semantic is active in this buffer.b
  (when (semantic-active-p)
    ;; Force this buffer to have its cache refreshed.
    (semantic-clear-toplevel-cache)
    ;; Here are some buffer local variables we can initialize ourselves
    ;; of a mode does not choose to do so.
    (semantic-lex-init)
    ;; Setup for a needed reparse.
    (semantic-parse-tree-set-needs-rebuild)
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
      (if (semantic-parse-tree-needs-update-p)
	  (semantic-bovinate-toplevel))
    (error nil)))

(if (boundp 'eval-defun-hooks)
    (add-hook 'eval-defun-hooks 'semantic-rebovinate-quickly-hook))

;;; Parsing Commands
;;
(eval-when-compile
  (condition-case nil (require 'pp) (error nil)))

(defvar semantic-edebug nil
  "When non-nil, activate the interactive parsing debugger.
Do not set this yourself.  Call `semantic-bovinate-buffer-debug'.")

(defun semantic-elapsed-time (start end)
  "Copied from elp.el.  Was elp-elapsed-time.
Argument START and END bound the time being calculated."
  (+ (* (- (car end) (car start)) 65536.0)
     (- (car (cdr end)) (car (cdr start)))
     (/ (- (car (cdr (cdr end))) (car (cdr (cdr start)))) 1000000.0)))

(defun bovinate (&optional clear)
  "Bovinate the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before bovinating.
If CLEAR is negative, it will do a full reparse, and also not display
the output buffer."
  (interactive "P")
  (if clear (semantic-clear-toplevel-cache))
  (if (eq clear '-) (setq clear -1))
  (let* ((start (current-time))
	 (out (semantic-bovinate-toplevel t))
	 (end (current-time)))
    (message "Retrieving tokens took %.2f seconds."
	     (semantic-elapsed-time start end))
    (when (not (and (numberp clear) (> 0 clear)))
      (pop-to-buffer "*BOVINATE*")
      (require 'pp)
      (erase-buffer)
      (insert (pp-to-string out))
      (goto-char (point-min)))))

(defun bovinate-debug ()
  "Bovinate the current buffer and run in debug mode."
  (interactive)
  (let ((semantic-edebug t)
	(out (semantic-bovinate-debug-buffer)))
    (pop-to-buffer "*BOVINATE*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string out))))

;;; Functions of the parser plug-in API
;;
;; Overload these functions to create new types of parsers.
;;
(define-overload semantic-parse-stream (stream nonterminal)
  "Parse STREAM, starting at the first NONTERMINAL rule.
For bovine and wisent based parsers, STREAM is from the output of
`semantic-lex', and NONTERMINAL is a rule in the apropriate language
specific rules file.
The default parser table used for bovine or wisent based parsers is
`semantic-toplevel-bovine-table'.

Must return a list: (STREAM NONTERMINALTOKENS)
where STREAM is the unused elements from STREAM, and NONTERMINALTOKENS
is the list of nonterminals found, usually only one token is returned
with the exception of compound statements")

(define-overload semantic-parse-changes ()
  "Reparse changes in the current buffer.
The list of changes are tracked as a series of overlays in the buffer.
When overloading this function, use `semantic-changes-in-region' to
analyze.")

(define-overload semantic-parse-region
  (start end &optional nonterminal depth returnonerror)
  "Parse the area between START and END, and return any tokens found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored.

Optional arguments:
NONTERMINAL is the rule to start parsing at. 
DEPTH specifies the lexical depth to decend for parser that use
lexical analysis as their first step.
RETURNONERROR specifies that parsing should stop on the first
unmatched syntax encountered.  When nil, parsing skips the syntax,
adding it to the unmatched syntax cache.

Must return a list of tokens wich have been cooked (repositioned
properly) but which DO NOT HAVE OVERLAYS associated with them.  When
overloading this function, use `semantic-raw-to-cooked-token' to cook
tokens.")

;;;###autoload
(defun semantic-parse-region-default
  (start end &optional nonterminal depth returnonerror)
  "Parse the area between START and END, and return any tokens found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored.
Optional arguments:
NONTERMINAL is the rule to start parsing at if it is known.
DEPTH specifies the lexical depth to scan.
RETURNONERROR specifies that parsing should end when encountering
unterminated syntax."
  (if (or (< end start) (> end (point-max)))
      (error "Invalid bounds passed to `semantic-parse-region'"))
  (let ((lexbits (semantic-lex start end depth))
	tokens)
    ;; Init a dump
    ;;    (if semantic-dump-parse
    ;;	      (semantic-dump-buffer-init))
    (setq tokens (semantic-repeat-parse-whole-stream
                  lexbits nonterminal returnonerror))
    (nreverse tokens)))

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

(defun semantic-bovinate-unmatched-syntax ()
  "Return the list of unmatched syntax tokens."
  ;; If the cache need refresh then do a full re-parse.
  (if (semantic-bovine-umatched-syntax-refresh-needed-p)
      ;; To avoid a recursive call, temporarily disable
      ;; `semantic-unmatched-syntax-hook'.
      (let (semantic-unmatched-syntax-hook)
        (condition-case nil
            (progn
              (semantic-clear-toplevel-cache)
              (semantic-bovinate-toplevel))
          (quit
           (message "semantic-bovinate-unmatched-syntax:\
 parsing of buffer canceled"))
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
  (semantic-parse-tree-set-needs-rebuild)
  ;; Remove this hook which tracks if a buffer is up to date or not.
  (remove-hook 'after-change-functions 'semantic-change-function t)
  ;; Old model.  Delete someday.
  ;;(run-hooks 'semantic-after-toplevel-bovinate-hook)

  (run-hook-with-args 'semantic-after-toplevel-cache-change-hook
		      semantic-toplevel-bovine-cache)
  )

(defun semantic-set-toplevel-bovine-cache (tokenlist)
  "Set the toplevel bovine cache to TOKENLIST."
  (setq semantic-toplevel-bovine-cache tokenlist
        semantic-unmatched-syntax-cache-check nil
        semantic-bovinate-nonterminal-check-obarray nil)
  (semantic-parse-tree-set-up-to-date)
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

(defvar semantic-bovination-working-type 'percent
  "*The type of working message to use when bovinating.
'percent means we are doing a linear parse through the buffer.
'dynamic means we are rebovinating specific tokens.")

(defsubst semantic-bovination-working-message (&optional arg)
  "Return the message string displayed while parsing.
If optional argument ARG is non-nil it is appended to the message
string.  See also the function `working-status-forms'."
  (if semantic-parser-name
      (format "%s/%s" semantic-parser-name (or arg ""))
    (format "%s" (or arg ""))))

;;; Application Parser Entry Point
;;
;; The best way to call the parser from programs is via
;; `semantic-bovinate-toplevel'.  This, in turn, uses other internal
;; API functions which plug-in parsers can take advantage of.

;;;###autoload
(defun semantic-bovinate-toplevel (&optional checkcache)
  "Bovinate the entire current buffer.
Do an incremental reparse if possible, otherwise do a full reparse.

The optional argument CHECKCACHE is ignored.  It is maintained for
compatibility with previous versions of Semantic."
  (and
   ;; Is this a semantic enabled buffer?
   (semantic-active-p)
   ;; Application hooks say the buffer is safe for parsing
   (run-hook-with-args-until-failure
    'semantic-before-toplevel-bovination-hook)
   ;; The parse tree actually needs to be refreshed
   (not (semantic-parse-tree-up-to-date-p))
   ;; So do it!
   (let* ((gc-cons-threshold (max gc-cons-threshold 10000000))
          (res nil))
     (garbage-collect)
     (cond
   
;;;; Try the incremental parser to do a fast update.
     ((semantic-parse-tree-needs-update-p)
      (setq res (semantic-parse-changes))
      (if (semantic-parse-tree-needs-rebuild-p)
          ;; If the partial reparse fails, jump to a full reparse.
          (semantic-bovinate-toplevel)
        ;; Clear the cache of unmatched syntax tokens
        ;;
        ;; NOTE TO SELF:
        ;;
        ;; Move this into the incremental parser.  This is a bug.
        ;;
        (semantic-clear-unmatched-syntax-cache)
        (run-hook-with-args ;; Let hooks know the updated tokens
         'semantic-after-partial-cache-change-hook res))
      )
   
;;;; Parse the whole system.
     ((semantic-parse-tree-needs-rebuild-p)
      (working-status-forms
          (semantic-bovination-working-message (buffer-name)) "done"
        (setq res (semantic-parse-region (point-min) (point-max)))
        (working-status t))
      ;; Clear the caches when we see there were no errors.
      ;; But preserve the unmatched syntax cache!
      (let (semantic-unmatched-syntax-cache
            semantic-unmatched-syntax-cache-check)
        (semantic-clear-toplevel-cache))
      ;; Set up the new overlays
      (semantic-overlay-list res)
      ;; Set up the cache with the new results
      (semantic-set-toplevel-bovine-cache res)
      ))))
  
  ;; Always return the current parse tree.
  semantic-toplevel-bovine-cache)

;;; Tokens and Overlays
;;
;; Overlays are used so that we can quickly identify tokens from
;; buffer positions and regions using built in Emacs commands.
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

;;; Token Cooking
;;
;; Raw tokens from a parser follow a different positional format than
;; those used in the bovine cache.  Raw tokens need to be cooked into
;; semantic cache friendly tokens for use by the masses.
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

;;; Iterative parser helper function
;;
;; Iterative parsers are better than rule-based iterative functions
;; in that they can handle obscure errors more cleanly.
;;
;; `semantic-repeat-parse-whole-stream' abstracts this action for
;; other parser centric routines.
;;
(defun semantic-repeat-parse-whole-stream
  (stream nonterm &optional returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
Optional argument RETURNONERROR indicates that the parser should exit
with the current results on a parse error.
This function returns tokens without overlays."
  (let ((result nil)
	(case-fold-search semantic-case-fold)
        nontermsym token)
    (while stream
      (setq nontermsym (semantic-parse-stream stream nonterm)
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
	      (working-status
               (/ (* 100 (semantic-lex-token-start (car stream)))
                  (point-max)))
	    (working-dynamic-status))))
    result))

;;; Parser action helper functions
;;
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

;;; Backwards Compatible API functions
;;
;; Semantic 1.x functions used by some parsers.
;;
;; Please move away from these functions, and try using
;; semantic 2.x interfaces instead.
;;
(defsubst semantic-bovinate-region-until-error
  (start end nonterm &optional depth)
  "NOTE: Use `semantic-parse-region' instead.

Bovinate between START and END starting with NONTERM.
Optional DEPTH specifies how many levels of parenthesis to enter.
This command will parse until an error is encountered, and return
the list of everything found until that moment.
This is meant for finding variable definitions at the beginning of
code blocks in methods.  If `bovine-inner-scope' can also support
commands, use `semantic-bovinate-from-nonterminal-full'."
  (semantic-parse-region start end nonterm depth t))

(defsubst semantic-bovinate-from-nonterminal (start end nonterm
						 &optional depth length)
  "Bovinate from within a nonterminal lambda from START to END.
Argument NONTERM is the nonterminal symbol to start with.
Optional argument DEPTH is the depth of lists to dive into.
When used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part.
Optional argument LENGTH specifies we are only interested in LENGTH tokens."
  (car-safe (cdr (semantic-parse-stream
		  (semantic-lex start end (or depth 1) length)
		  nonterm))))

(defsubst semantic-bovinate-from-nonterminal-full (start end nonterm
						      &optional depth)
  "Bovinate from within a nonterminal lambda from START to END.
Iterates until all the space between START and END is exhausted.
Argument NONTERM is the nonterminal symbol to start with.
If NONTERM is nil, use `bovine-block-toplevel'.
Optional argument DEPTH is the depth of lists to dive into.
When used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (semantic-parse-region start end nonterm (or depth 1)))

(provide 'semantic)

;;; semantic.el ends here

;; Semantic-util is a part of the semantic API.  Include it last
;; because it depends on semantic.
(require 'semantic-util)
