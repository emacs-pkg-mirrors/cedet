;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.91 2001/03/14 12:41:41 ponced Exp $

(defvar semantic-version "1.4"
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

;;; History:
;; 

(require 'working)
(require 'semantic-util)

(defgroup semantic nil
  "Parser Generator."
  )

;;; Code:

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

For consistency between languages, always use the following symbol
forms.  It is fine to create new symbols, or to exclude some if they
do not exist, however by using these symbols, you can maximize the
number of language-independent programs available for use with your
language.

GENERIC ENTRIES:

 Bovine table entry return elements are up to the table author.  It is
recommended, however, that the following format be used.

 (\"NAME\" type-symbol [\"TYPE\"] ... \"DOCSTRING\" PROPERTIES OVERLAY)

Where type-symbol is the type of return token found, and NAME is it's
name.  If there is any typing information needed to describe this
entry, make that come third.  Next, any additional information follows
the optional type.  The last data entry can be the position in the buffer
of DOCSTRING.  A docstring does not have to exist in the form used by
Emacs Lisp.  It could be the text of a comment appearing just before a
function call, or in line with a variable.

PROPERTIES is a list of additional properties for this token.
PRORPERTIES is not for details of the token.  It is used for
additional tags needed by tools using the parse stream.  For example,
the `dirty' property is used when a given token needs to be reparsed.

PROPERTIES are automatically added to the token by the system when
using BNF, or `semantic-lambda' in the table.

The last element must be OVERLAY.  The OVERLAY is automatically
created by the parsing system.  When programming with BNF, or using
`semantic-lambda', no extra work needs to be done.  If you are
building the parse table yourself, use START and END.

It may seem odd to place NAME in slot 0, and the type-symbol in slot
1, but this turns the returned elements into a list which can be used
by alist based function.  This makes it ideal for passing into generic
sorters, string completion functions, and list searching functions.

In the below entry formats, \"NAME\" is a string which is the name of
the object in question.  It is possible for this to be nil in some
situations, and code dealing with entries should try to be aware of
these situations.

\"TYPE\" is a string representing the type of some objects.  For a
variable, this could very well be another top level token representing
a type nonterminal.

TOP-LEVEL ENTRIES:

 (\"NAME\" variable \"TYPE\" DEFAULT-VALUE EXTRA-SPEC 
         \"DOCSTRING\" PROPERTIES OVERLAY)
   The definition of a variable, or constant.
   DEFAULT-VALUE can be something apropriate such a a string,
                 or list of parsed elements.
   EXTRA-SPEC are details about a variable that are not covered in the TYPE.
             See detail on EXTRA-SPEC after entries section.
   DOCSTRING is optional.

 (\"NAME\" function \"TYPE\" ( ARG-LIST ) EXTRA-SPEC
          \"DOCSTRING\" PROPERTIES OVERLAY)
   A function/procedure definition.
   ARG-LIST is a list of variable definitions.
   DOCSTRING is optional.

 (\"NAME\" type \"TYPE\" ( PART-LIST ) ( PARENTS ) EXTRA-SPEC
          \"DOCSTRING\" PROPERTIES OVERLAY)
   A type definition.
   TYPE of a type could be anything, such as (in C) struct, union, typedef,
        or class.
   PART-LIST is only useful for structs that have multiple individual parts.
            (It is recommended that these be variables, functions or types).
   PARENTS is strictly for classes where there is inheritance.
   

 (\"FILE\" include SYSTEM \"DOCSTRING\" PROPERTIES OVERLAY)
   In C, an #include statement.  In elisp, a require statement.
   Indicates additional locations of sources or definitions.
   SYSTEM is true if this include is part of a set of system includes.

 (\"NAME\" package DETAIL \"DOCSTRING\" PROPERTIES OVERLAY)
   In Emacs Lisp, a `provide' statement.  DETAIL might be an
   associated file name.  In Java, this is a package statement.

EXTRA-SPEC:

  The EXTRA-SPEC section of variables, functions, and types provide a
location to place language specific details which are not accounted
for by the base token type.  Because there is an arbitrary number of
things that could be associated in the EXTRA-SPEC section, this should
be formatted as an association list.

  Here are some typed extra specifiers that may exist.  Any arbitrary
number of extra specifiers may be created, and specifiers not
documented here are allowed.

  (parent .  \"text\") - Name of a parent type/class.  C++ and CLOS allow
     the creation of a function outside the body of that type/class.

  (dereference .  INT) - Number of levels of dereference.  In C, the number
     of `*' characters indicating pointers.

  (typemodifiers .  \"text\") - Keyword modifiers for a type.  In C, such
     words would include `register', and `volatile'.

  (suffix . \"text\") - Suffix information for a variable.

  (const .  t) - This exists if the variable or return value is constant.

  (throws .  \"text\") - For functions or methods in languages that support
     typed signal throwing.

  (user-visible . t) - For functions in interpreted languages such as Emacs Lisp,
     this signals that a function or variable is user visible.  In Emacs Lisp,
     this means a function is `interactive'.")
(make-variable-buffer-local 'semantic-toplevel-bovine-table)

(defvar semantic-toplevel-bovine-table-source nil
  "The .bnf source file the current table was created from.")
(make-variable-buffer-local 'semantic-toplevel-bovine-table-source)

(defvar semantic-symbol->name-assoc-list
  '((variable . "Variables")
    (function . "Functions")
    (type . "Types")
    (include . "Dependencies")
    (package . "Provides"))
  "Association between symbols returned, and a string.
The string is used to represent a group of objects of the given type.
It is sometimes useful for a language to use a different string
in place of the default, even though that language will still
return a symbol.  For example, Java return's includes, but the
string can be replaced with `Imports'.")
(make-variable-buffer-local 'semantic-symbol->name-assoc-list)

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
this is returned instead of re-parsing the buffer.")
(make-variable-buffer-local 'semantic-toplevel-bovine-cache)

(defvar semantic-edits-are-safe nil
  "When non-nil, modifications to not require a reparse.
It prevents tokens from being marked dirty, and it
prevents top level edits from causing a cache check.")

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

(defvar semantic-dirty-token-hooks nil
  "Hooks run after when a token is marked as dirty.
The functions must take TOKEN, START, and END as a parameters.")

(defvar semantic-clean-token-hooks nil
  "Hooks run after when a token is marked as clean.
The functions must take a TOKEN as a parameter.")

(defvar semantic-bovinate-toplevel-override nil
  "Local variable set by major modes which provide their own bovination.
This function should behave as the function `semantic-bovinate-toplevel'.")
(make-variable-buffer-local 'semantic-bovinate-toplevel-override)

(defvar semantic-after-toplevel-bovinate-hook nil
  "Hooks run after a toplevel token parse.
It is not run if the toplevel parse command is called, and buffer does
not need to be reparsed.
For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-before-toplevel-cache-flush-hook nil
  "Hooks run before the toplevel nonterminal cache is flushed.
For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as needing a reparse.
For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-no-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as not needing a reparse.
If the hook returns non-nil, then declare that a reparse is needed.
For language specific hooks, make sure you define this as a local hook.")


;;; Utility API functions
;;
;; These functions use the flex and bovination engines to perform some
;; simple tasks useful to other programs.  These are just the most
;; critical entries.
;;
;; See semantic-util for a wider range of utility functions and macros.
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

(defun semantic-token-name (token)
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

(defun semantic-token-get (token key)
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

(defun semantic-token-start (token)
  "Retrieve the start location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o) (semantic-overlay-start o) (aref o 0))))

(defun semantic-token-end (token)
  "Retrieve the end location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o) (semantic-overlay-end o) (aref o 1))))

(defun semantic-token-buffer (token)
  "Retrieve the buffer TOKEN resides in."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o) (semantic-overlay-buffer o)
      ;; We have no buffer for this token (It's not in Emacs right now.)
      nil)))

(defun semantic-token-p (token)
  "Return non-nil if TOKEN is most likely a semantic token."
  (and (listp token)
       (stringp (car token))
       (symbolp (car (cdr token)))))

(defun semantic-token-with-position-p (token)
  "Return non-nil if TOKEN is a semantic token with positional information."
  (and (listp token)
       (stringp (car token))
       (symbolp (car (cdr token)))
       (let ((o (semantic-token-overlay token)))
	 (or (semantic-overlay-p o)
	     (arrayp o)))))

;;; Overlay and error stacks.
;;
(defvar semantic-overlay-error-recovery-stack nil
  "List of overlays used during error recovery.")

(defun semantic-overlay-stack-add (o)
  "Add overlay O to the error recovery stack."
  (setq semantic-overlay-error-recovery-stack
	(if (listp o)
	    (append o semantic-overlay-error-recovery-stack)
	  (cons o semantic-overlay-error-recovery-stack))))

(defun semantic-overlay-stack-clear ()
  "Clear the overlay error recovery stack."
  (while semantic-overlay-error-recovery-stack
    (semantic-overlay-delete (car semantic-overlay-error-recovery-stack))
    (setq semantic-overlay-error-recovery-stack
	  (cdr semantic-overlay-error-recovery-stack))))

(defun semantic-delete-overlay-maybe (overlay)
  "Delete OVERLAY if it is a semantic token overlay."
  (if (semantic-overlay-get overlay 'semantic)
      (semantic-overlay-delete overlay)))

;;; Interfacing with the system
;;
(defvar semantic-init-hooks nil
  "*Hooks run when a buffer is initialized with a parsing table.")

(defun semantic-active-p ()
  "Return non-nil if the current buffer was set up for parsing."
  (or semantic-toplevel-bovine-table
      semantic-bovinate-toplevel-override))

(defun semantic-find-file-hook ()
  "Run in `find-file-hooks'.
Runs `semantic-init-hook' if the major mode is setup to use semantic."
  (when (semantic-active-p)
    (setq semantic-toplevel-bovine-force-reparse t)
    (run-hooks 'semantic-init-hooks)))
(add-hook 'find-file-hooks 'semantic-find-file-hook)
;; I think this should work, but it does funny stuff.
;(add-hook 'change-major-mode-hook 'semantic-find-file-hook)

;; Test the above hook.
;;(add-hook 'semantic-init-hooks (lambda () (message "init for semantic")))

(defun semantic-eval-defun-hook ()
  "When `eval-defun-hook' is available, use that to partially reparse."
  (condition-case nil
      (if (semantic-bovine-toplevel-partial-reparse-needed-p nil)
	  (semantic-bovinate-toplevel))
    (error nil)))

(if (boundp 'eval-defun-hooks)
    (add-hook 'eval-defun-hooks 'semantic-eval-defun-hook))

;;; Parsing functions
;;
(defun semantic-clear-toplevel-cache ()
  "Clear the toplevel bovin cache for the current buffer.
Clearing the cache will force a complete reparse next time a token
stream is requested."
  (interactive)
  (run-hooks 'semantic-before-toplevel-cache-flush-hook)
  (setq semantic-toplevel-bovine-cache nil)
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
  (run-hooks 'semantic-after-toplevel-bovinate-hook))
(add-hook 'change-major-mode-hook 'semantic-clear-toplevel-cache)

(defvar semantic-bovination-working-type 'percent
  "*The type of working message to use when bovinating.
'percent means we are doing a linear parse through the buffer.
'dynamic means we are rebovinating specific tokens.")

(defun semantic-bovine-toplevel-full-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a full reparse.
Optional argument CHECKCACHE indicates if the cache check should be made."
  (or semantic-toplevel-bovine-force-reparse
      (and
       checkcache
       semantic-toplevel-bovine-cache-check)))

(defun semantic-bovine-toplevel-partial-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a full reparse.
Optional argument CHECKCACHE indicates if the cache check should be made."
  (and semantic-toplevel-bovine-cache
       semantic-dirty-tokens
       (not (semantic-bovine-toplevel-full-reparse-needed-p checkcache))))

;;;###autoload
(defun semantic-bovinate-toplevel (&optional checkcache)
  "Bovinate the entire current buffer.
If the optional argument CHECKCACHE is non-nil, then make sure the cached
token list is up to date.  If a partial reparse is possible, do that,
otherwise, do a full reparse."
  (cond
   ((and semantic-bovinate-toplevel-override
	 ;; We cannot predict partial reparsing for these parsers.  Let them
	 ;; fend for themselves.  We can, however, handle the main cache for them.
	 (or (semantic-bovine-toplevel-partial-reparse-needed-p checkcache)
	     (semantic-bovine-toplevel-full-reparse-needed-p checkcache)))
    (semantic-clear-toplevel-cache)
    ;; Call a custom function
    (let ((res (funcall semantic-bovinate-toplevel-override checkcache)))
      (semantic-set-toplevel-bovine-cache res))
    (run-hooks 'semantic-after-toplevel-bovinate-hook)
    semantic-toplevel-bovine-cache
    )
   ((semantic-bovine-toplevel-partial-reparse-needed-p checkcache)
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
      (setq semantic-dirty-tokens nil)
      )
    (if (semantic-bovine-toplevel-full-reparse-needed-p checkcache)
	;; If the partial reparse fails, jump to a full reparse.
	(semantic-bovinate-toplevel checkcache)
      semantic-toplevel-bovine-cache)
    )
   ((semantic-bovine-toplevel-full-reparse-needed-p checkcache)
    (semantic-clear-toplevel-cache)
    ;; Reparse the whole system
    (let ((ss (semantic-flex (point-min) (point-max)))
	  (res nil)
	  (semantic-overlay-error-recovery-stack nil))
      ;; Init a dump
      (if semantic-dump-parse (semantic-dump-buffer-init))
      ;; Parse!
      (working-status-forms (buffer-name) "done"
	(setq res
	      (semantic-bovinate-nonterminals
	       ss 'bovine-toplevel semantic-flex-depth))
	(working-status t))
      (semantic-set-toplevel-bovine-cache (nreverse res))
      semantic-toplevel-bovine-cache))
   (t
    ;; We have a cache with stuff in it, so return it
    semantic-toplevel-bovine-cache
    )))

(eval-when-compile (require 'semanticdb))

(defun semantic-file-token-stream (file &optional checkcache)
  "Return a token stream for FILE.
If it is loaded, return the stream after making sure it's ok.
If FILE is not loaded, check to see if `semanticdb' feature exists,
   and use it to get un
If FILE is not loaded, and semanticdb is not available, find the file
   and parse it.
Optional argument CHECKCACHE is the same as that for
`semantic-bovinate-toplevel'."
  (if (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(semantic-bovinate-toplevel checkcache))
    ;; File not loaded
    (if (and (fboundp 'semanticdb-minor-mode-p)
	     (semanticdb-minor-mode-p))
	;; semanticdb is around, use it.
	(semanticdb-file-stream file)
      ;; Get the stream ourselves.
      (save-excursion
	(set-buffer (find-file-noselect file))
	(semantic-bovinate-toplevel checkcache)))))

(defun semantic-set-toplevel-bovine-cache (tokenlist)
  "Set the toplevel bovine cache to TOKENLIST."
  (setq semantic-toplevel-bovine-cache tokenlist
	semantic-toplevel-bovine-cache-check nil
	semantic-toplevel-bovine-force-reparse nil)
  (add-hook 'after-change-functions 'semantic-change-function nil t)
  (run-hooks 'semantic-after-toplevel-bovinate-hook))

(defun semantic-change-function (start end length)
  "Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change."
  (when (and (not semantic-toplevel-bovine-cache-check)
	     (not semantic-edits-are-safe))
    (let ((tl (condition-case nil
		  (nreverse (semantic-find-nonterminal-by-overlay-in-region
		   (1- start) (1+ end)))
		(error nil))))
      (if tl
	  (catch 'alldone
	    ;; Loop over the token list
	    (while tl
	      (cond
	       ;; If we are completely enclosed in this overlay.
	       ((and (> start (semantic-token-start (car tl)))
		     (< end (semantic-token-end (car tl))))
		(if (semantic-token-get (car tl) 'dirty)
		    nil
		  (add-to-list 'semantic-dirty-tokens (car tl))
		  (semantic-token-put (car tl) 'dirty t)
		  (condition-case nil
		      (run-hook-with-args 'semantic-dirty-token-hooks
					  (car tl) start end)
		    (error (if debug-on-error (debug)))))
		  (throw 'alldone t))
	       ;; If we cover the beginning or end of this item, we must
	       ;; reparse this object.  If there are more items coming, then postpone
	       ;; this till later.
	       ((not (cdr tl))
		(setq semantic-toplevel-bovine-cache-check t)
		(run-hooks 'semantic-reparse-needed-change-hook))
	       (t nil))
	      ;; next
	      (setq tl (cdr tl))))
	;; There was no hit, perhaps we need to reparse this intermediate area.
	(setq semantic-toplevel-bovine-cache-check t)
	)
      )))

(defun semantic-raw-to-cooked-token (token)
  "Convert TOKEN from a raw state to a cooked state.
The parser returns raw tokens with positional data START/END.
We convert it from that to a cooked state with a property list and an overlay.
Change the token with side effects and returns TOKEN."
  (let* ((result nil)
	 (expandedtokens nil)
	 (ncdr (- (length token) 2))
	 (propcdr (if (natnump ncdr) (nthcdr ncdr token)))
	 (overcdr (cdr propcdr))
	 ;; propcdr is the CDR containing the START from the token.
	 ;; overcdr is the CDR containing the END from the token.
	 ;; PROPCDR will contain the property list after cooking.
	 ;; OVERCDR will contain the overlay after cooking.
	 (o (condition-case nil
		(semantic-make-overlay (car propcdr)
				       (car overcdr)
				       (current-buffer)
				       ;; Examin start/rear
				       ;; advance flags.
				       )
	      (error (debug token)
		     nil))))
    ;; Convert START/END into PROPERTIES/OVERLAY.
    (setcar overcdr o)
    (setcar propcdr nil)
    (semantic-overlay-put o 'semantic token)
    ;; Expand based on local configuration
    (if (not semantic-expand-nonterminal)
	;; no expanders
	(setq result (cons token result))
      ;; Glom generated tokens
      (setq expandedtokens (funcall semantic-expand-nonterminal token))
      (if (not expandedtokens)
	  (progn (setq result (cons token result))
		 (semantic-overlay-stack-add o))
	;; Fixup all overlays, start by deleting the old one
	(let ((tokenloop expandedtokens) o start end)
	  (while tokenloop
	    (setq propcdr (nthcdr (- (length (car tokenloop)) 2)
				   (car tokenloop))
		  overcdr (nthcdr (- (length (car tokenloop)) 1)
				   (car tokenloop))
		  ;; this will support new overlays created by
		  ;; the special function, or recycles
		  start (if (semantic-overlay-live-p (car overcdr))
			    (semantic-overlay-start (car overcdr))
			  start)
		  end (if (semantic-overlay-live-p (car overcdr))
			  (semantic-overlay-end (car overcdr))
			end)
		  o (semantic-make-overlay start end
					   (current-buffer)))
	    (if (semantic-overlay-live-p (car overcdr))
		(semantic-overlay-delete (semantic-token-overlay
					  (car tokenloop))))
	    (semantic-overlay-stack-add o)
	    (setcar propcdr nil)
	    (setcar overcdr o)
	    (semantic-overlay-put o 'semantic (car tokenloop))
	    (setq tokenloop (cdr tokenloop))))
	(setq result (append expandedtokens result))))
    result))

(defun semantic-bovinate-nonterminals (stream nonterm &optional
					      depth returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
DEPTH is optional, and defaults to 0.
Optional argument RETURNONERROR indicates that the parser should exit with
the current results on a parse error."
  (if (not depth) (setq depth semantic-flex-depth))
  (let ((result nil) (case-fold-search semantic-case-fold))
    (while stream
      (let* ((nontermsym
	      (semantic-bovinate-nonterminal
	       stream semantic-toplevel-bovine-table nonterm))
	     (stream-overlays (car (cdr (cdr nontermsym))))
	     (token (car (cdr nontermsym))))
	(if (not nontermsym)
	    (error "Parse error @ %d" (car (cdr (car stream)))))
	(semantic-overlay-stack-add stream-overlays)
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
	  (if returnonerror (setq stream nil))
	  ;;(error "Parse error")
	  )
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
  (let* ((flexbits (semantic-flex (semantic-token-start token)
				  (semantic-token-end token)))
	 ;; For embeded tokens (type parts, for example) we need a
	 ;; different symbol.  Come up with a plan to solve this.
	 (nonterminal (or (semantic-token-get token 'reparse-symbol)
			  'bovine-toplevel))
	 (new (condition-case nil
                  (semantic-bovinate-nonterminal
                   flexbits
                   semantic-toplevel-bovine-table
                   nonterminal)
                ;; Trap `read' errors which may temporarily occurs
                ;; when re-parsing a dirty Elisp token.
                (end-of-file 'delay)
                (invalid-read-syntax 'delay)))
	 (cooked nil)
	 )
    (if (eq new 'delay)
        ;; If a `read' error occured during re-parsing of an Elisp
        ;; token just delay rebovination.  Thus features like Semantic
        ;; completion could continue to work until a clean syntax
        ;; state is reached and re-parse succeeds.
        nil
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
        (let ((oo (semantic-token-overlay token))
              (o (semantic-token-overlay new)))
          ;; Copy all properties of the old overlay here.
          ;; I think I can use plists in emacs, but not in XEmacs.
          ;; Ack!
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
     ;; This important bit is because the CONS cell representing TOKEN
     ;; is what we need here, even though the whole thing is the same.
          (semantic-overlay-put o 'semantic token)
          ;; Hooks
          (run-hook-with-args 'semantic-clean-token-hooks token)
          )
        ))))


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

(defun semantic-bovinate-nonterminal (stream table &optional nonterminal)
  "Bovinate STREAM based on the TABLE of nonterminal symbols.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Use `bovine-toplevel' if it is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM OVERLAYS) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found.  OVERLAYS is the list of overlays found
so far, to be used in the error recovery stack."
  (if (not nonterminal)
      (setq nonterminal 'bovine-toplevel))
  (let ((matchlist (cdr (assq nonterminal table)))
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
        semantic-overlay-error-recovery-stack ;part of error recovery
        )
    (while nt-loop
      (catch 'push-non-terminal
        (setq semantic-overlay-error-recovery-stack nil
              nt-popup nil
              end (cdr (cdr (car stream))))
        (while (or nt-loop nt-popup)
          (setq nt-loop nil
                out     nil)
          (while (or nt-popup matchlist)
            (if nt-popup
                ;; End of a non-terminal recursion
                (setq nt-popup nil)
              ;; New matching process
              (setq s   stream           ;init s from stream.
                    cvl nil              ;re-init the collected value list.
                    lte (car matchlist)  ;Get the local matchlist entry.
                    )
              (if (or (byte-code-function-p (car lte))
                      (listp (car lte)))
                  ;; In this case, we have an EMPTY match!  Make stuff
                  ;; up.
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
                                        ;; error recovery
                                        semantic-overlay-error-recovery-stack
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
                (setq lse (car s)       ;Get the local stream element
                      s   (cdr s))      ;update stream.
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
                              (semantic-overlay-stack-clear)
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
                  (semantic-overlay-stack-clear)
                  (setq lte nil cvl nil)) ;No more matches, exit
                )))
            (if (not cvl)               ;lte=nil;  there was no match.
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
          (setq result (list s out semantic-overlay-error-recovery-stack))
          (if nt-stack
              ;; pop previous state from the nt-stack
              (let ((state (car nt-stack))
                    (ov semantic-overlay-error-recovery-stack))

                (setq nt-popup    t
                      ;; pop actual parser state
                      matchlist   (aref state 0)
                      cvl         (aref state 1)
                      lte         (aref state 2)
                      stream      (aref state 3)
                      end         (aref state 4)
                      ;; pop error recovery state
                      semantic-overlay-error-recovery-stack (aref state 5)
                      ;; update the stack
                      nt-stack    (cdr nt-stack))
                
                (if ov
                    (semantic-overlay-stack-add ov))
                
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
                  (semantic-overlay-stack-clear)
                  (setq lte nil cvl nil)) ;No match, exit
                )))))
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
Whan used in a `lambda' of a MATCH-LIST, there is no need to include
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
Whan used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (nreverse
   (semantic-bovinate-nonterminals (semantic-flex start end (or depth 1))
				   nonterm
				   depth)))

(defun semantic-bovinate-region-until-error (start end nonterm &optional depth)
  "Bovinate between START and END starting with NONTERM.
Optinal DEPTH specifies how many levels of parenthesis to enter.
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

(defun semantic-bovinate-debug-set-table ()
  "Set the table for the next debug to be here."
  (interactive)
  (if (not (eq major-mode 'emacs-lisp-mode))
      (error "Not an Emacs Lisp file"))
  (beginning-of-defun)
  (setq semantic-bovinate-debug-table (point-marker)))

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
  "A list generated as a referece (assumed valid).
A second pass comares return values against this list.")

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
Save the obarry into `semantic-flex-keywords-obarray'.
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

(defun semantic-flex-keyword-p (text)
  "Return a symbol if TEXT is a keyword in the keyword table.
Return nil if TEXT is not in the symbol table."
  (let ((sym (intern-soft text semantic-flex-keywords-obarray)))
    (if sym (symbol-value sym))))

(defun semantic-flex-keyword-put (text property value)
  "For keyword TEXT, set PROPERTY to have VALUE."
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
If optional PROPERTY is non-nil call FUN only on every keyword which
have a PROPERTY value.  FUN receives a semantic keyword as argument."
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
If optional PROPERTY is non-nil return only keyword which have a
PROPERTY value."
  (let (keywords)
    (semantic-flex-map-keywords
     (function
      (lambda (keyword)
        (setq keywords (cons keyword keywords))))
     property)
    keywords))

;;; Lexical Analysis
;;
(defvar semantic-flex-extensions nil
  "Buffer local extensions to the lexical analyzer.
This should contain an alist with a key of a regex and a data element of
a function.  The function should both move point, and return a lexical
token of the form:
  ( TYPE START .  END)
nil is also a valid return.
TYPE can be any type of symbol, as long as it doesn't occur as a
nonterminal in the language definition.")
(make-variable-buffer-local 'semantic-flex-extensions)

(defvar semantic-flex-syntax-modifications nil
  "Updates to the syntax table for this buffer.
These changes are active only while this file is being flexed.
This is a list where each element is of the form:
  (CHAR CLASS)
Where CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what class of syntax CHAR is.")
(make-variable-buffer-local 'semantic-flex-syntax-modifications)

(defvar semantic-flex-enable-newlines nil
  "When flexing, report 'newlines as syntactic elements.
Useful for languages where the newline is a special case terminator.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-newlines)

(defun semantic-flex-buffer (&optional depth)
  "Sematically flex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-flex (point-min) (point-max) depth))

(defun semantic-flex (start end &optional depth length)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element being a list, such
as (symbol start-expression .  end-expresssion).
END does not mark the end of text scanned, only the end of the beginning
of text scanned.  Thus, if a string extended past END, the end of the
return token will be larger than END.  To truly restrict scanning, using
narrow-to-region'.
The last argument, LENGTH specifies that `semantic-flex' should only return
LENGTH tokens."
  ;(message "Flexing muscles...")
  (if (not semantic-flex-keywords-obarray)
      (setq semantic-flex-keywords-obarray [ nil ]))
  (let ((ts nil)
	(sym nil)
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
	(cond (;; catch newlines when needed
	       (and semantic-flex-enable-newlines
		    (looking-at "\\s-*\\(\n\\)"))
	       (setq ep (match-end 1)
		     ts (cons (cons 'newline
				    (cons (match-beginning 1) ep))
			      ts)))
	      ;; special extentions, sometimes includes some whitespace.
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
	      ;; comment end is also EOL for some languages.
	      ((looking-at "\\(\\s-\\|\\s>\\)+"))
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
					   (error (goto-char end)))
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
					    (forward-sexp 1)
					    (setq ep (point)))))
			      ts)))
	      ((looking-at cs)
	       (if semantic-ignore-comments
		   ;; If the language doesn't deal with comments,
		   ;; ignore them here.
		   (progn (forward-comment 1)
			  (setq ep (point)))
		 ;; Language wants comments, link them together.
		 (if (eq (car (car ts)) 'comment)
		     (setcdr (cdr (car ts)) (save-excursion
					      (forward-comment 1)
					      (setq ep (point))))
		   (setq ts (cons (cons 'comment
					(cons (match-beginning 0)
					      (save-excursion
						(forward-comment 1)
						(setq ep (point)))))
				  ts)))))
	      ((looking-at "\\(\\s.\\|\\s$\\|\\s'\\)")
	       (setq ts (cons (cons 'punctuation
				    (cons (match-beginning 0) (match-end 0)))
			      ts)))
	      (t (error "What is that?")))
	(goto-char (or ep (match-end 0)))
	(setq ep nil)))
    (goto-char pos)
    ;(message "Flexing muscles...done")
    (nreverse ts)))

(defun semantic-flex-text (semobj)
  "Fetch the text associated with the semantic object SEMOBJ."
  (buffer-substring-no-properties (car (cdr semobj)) (cdr (cdr semobj))))

(defun semantic-flex-list (semlist depth)
  "Flex the body of SEMLIST to DEPTH."
  (semantic-flex (car (cdr semlist)) (cdr (cdr semlist)) depth))

(defun semantic-flex-start (semobj)
  "Fetch the start position of the semantic object SEMOBJ."
  (nth 1 semobj))

(defun semantic-flex-end (semobj)
  "Fetch the end position of the semantic object SEMOBJ."
  (cdr (cdr semobj)))

;;; Settings and autoloads
;;
(autoload 'semantic-create-imenu-index "semantic-imenu"
  "Create an imenu index for any buffer which supports Semantic.")
(autoload 'bovinate "semantic-util"
  "Bovinate the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before bovinating." t)
(autoload 'bovinate-debug "semantic-util"
  "Bovinate the current buffer and run in debug mode." t)
(autoload 'senator-minor-mode "senator"
  "Minor mode for the SEmantic NAvigaTOR." t)
(autoload 'global-semanticdb-minor-mode "semanticdb"
  "Mode saving token lists between sessions." t)

(provide 'semantic)

;;; semantic.el ends here

