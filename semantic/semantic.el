;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.1
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.47 2000/09/20 17:59:36 zappo Exp $

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
;;
;; Bovine Table Tips & Tricks:
;; ---------------------------
;;
;; Many of the tricks needed to create rules in bison or yacc can be
;; used here.  The exceptions to this rule are that there is no need to
;; declare bison tokens, and you cannot put "code" in the middle of a
;; match rule.  In addition, you should avoid empty matching rules as
;; I haven't quite gotten those to be reliable yet.
;;
;; The top-level bovine table is an association list of all the rules
;; needed to parse your language, or language segment.  It is easiest
;; to create one master rule file, and call the semantic bovinator on
;; subsections passing down the nonterminal rule you want to match.
;;
;; Thus, every entry in the bovine table is of the form:
;; ( NONTERMINAL-SYMBOL MATCH-LIST )
;; 
;; The nonterminal symbol is equivalent to the bison RESULT, and the
;; MATCH-LIST is equivalent to the bison COMPONENTS.  Thus, the bison
;; rule:
;;        expseq: expseq1
;;              | expseq2
;;              ;
;; becomes:
;;        ( expseq ( expseq1 ) ( expseq2 ) )
;; which defines RESULT expseq which can be either COMPONENT expseq1
;; or expseq2.  These two table entries also use nonterminal results,
;; and also use the DEFAULT RESULT LAMBDA (see below for details on
;; the RESULT LAMBDA).
;;
;; You can also have recursive rules, as in bison.  For example the
;; bison rule:
;;        expseq1: exp
;;               | expseq1 ',' exp
;;               ;
;; becomes:
;;        (expseq1 (exp)
;;                 (expseq1 punctuation "," exp
;;                          (lambda (val start end)
;;                                  ( -generator code- ))))
;;
;; This time, the second rule uses it's own RESULT LAMBDA.
;;
;; Lastly, you can also have STRING LITERALS in your rules, though
;; these are different from Bison.  As can be seen above, a literal is
;; a constant lexed symbol, such as `punctuation', followed by a string
;; which is a *regular expression* which must match, or this rule will
;; fail.
;;
;; In BISON, a given rule can have inline ACTIONS.  In the semantic
;; bovinator, there can be only one ACTION which I will refer to here
;; as the RESULT LAMBDA.  There are two default RESULT LAMBDAs which
;; can be used which cover the default case.  The RESULT LAMBDA must
;; return a valid nonterminal token.  A nonterminal token is always of the
;; form ( NAME TOKEN VALUE1 VALUE2 ... START END).  NAME is the name
;; to use for this token.  It is first so that a list of tokens is
;; also an alist, or completion table.  Token should be the same
;; symbol as the nonterminal token generated, though it does not have to
;; be.  The values can be anything you want, including other tokens.
;; START and END indicate where in the buffer this token is, and is
;; easily derived from the START and END parameter passed down.
;;
;; A RESULT LAMBDA must take three parameters, VALS, START and END.
;; VALS is the list of literals derived during the bovination of the
;; match list, including punctuation, parens, and explicit
;; matches.  other elements
;;
;; Here are some example match lists and their code:
;;
;; (expression (lambda (vals start end)
;;                     (append (car vals) (list start end))))
;;
;; In this RESULT LAMBDA, VALS will be of length one, and it's first
;; element will contain the nonterminal expression result.  It is
;; likely to use a rule like this when there is a top level nonterminal
;; symbol whose contents are several other single nonterminal rules.
;; Because of this, we want to result that value with our START and END
;; appended.
;;
;; NOTE: nonterminal values passed in as VALS always have their
;;       START/END parts stripped!
;;
;; This example lambda is also one of the DEFAULT lambdas for the case
;; of a single nonterminal result.  Thus, the above rule could also be
;; written as (expression).
;;
;; A more complex example uses more flex elements.  Lets match this:
;;
;;    (defun myfunction (arguments) "docstring" ...)
;;
;; If we assume a flex depth of 1, we can write it this way:
;;
;; (open-paren "(" symbol "defun" symbol semantic-list string
;;             (lambda (vals start end)
;;                     (list (nth 2 vals) 'function nil (nth 3 vals)
;;                           (nth 4 vals) start end)))
;;
;; The above will create a function token, whose format is
;; predefined.  (See the symbol `semantic-toplevel-bovine-table' for
;; details on some default symbols that should be provided.)
;;
;; From this we can see that VALS will have the value:
;; ( "(" "defun" "myfunction" "(arguments)" "docstring")
;;
;; If we also want to return a list of arguments in our function
;; token, we can replace `semantic-list' with the following recursive
;; nonterminal rule.
;;
;; ( arg-list (semantic-list
;;             (lambda (vals start end)
;;                (semantic-bovinate-from-nonterminal start end 'argsyms))))
;; ( argsyms
;;   (open-paren argsyms (lambda (vals start end)
;;			   (append (car (cdr vals)) (list start end))))
;;   (symbol argsyms (lambda (vals start end)
;;		       (append (cons (car vals) (car (cdr vals)))
;;			       (list start end))))
;;   (symbol close-paren (lambda (vals start end)
;;			   (list (car vals) start end))))
;;
;; This recursive rule can find a parenthetic list with any number of
;; symbols in it.
;;
;; Here we also see a new function, `semantic-bovinate-from-nonterminal'.
;; This function takes START END and a nonterminal result symbol to
;; match.  This will return a complete token, including START and
;; END.  This function should ONLY BE USED IN A RESULT LAMBDA.  It
;; uses knowledge of that scope to reduce the number of parameters
;; that need to be passed in.  This is useful for decomposing complex
;; syntactic elements, such as semantic-list.
;;
;; Token's and the VALS argument
;; -----------------------------
;;
;; Not all syntactic tokens are represented by strings in the VALS argument
;; to the match-list lambda expression.  Some are a dotted pair (START . END).
;; The following are represented as strings:
;;  1) symbols
;;  2) punctuation
;;  3) open/close-paren
;;  4) charquote
;;  5) strings
;; The following are represented as a dotted-pair.
;;  1) semantic-list
;;  2) comments
;; Nonterminals are always lists which are generated in the lambda
;; expression.
;;
;; Semantic Bovine Table Debugger
;; ------------------------------
;;
;; The bovinator also includes a primitive debugger.  This debugger
;; walks through the parsing process and see how it's being
;; interpretted.  There are two steps in debuggin a bovine table.
;;
;; First, place the cursor in the source code where the table is
;; defined.  Execute the command `semantic-bovinate-debug-set-table'.
;; This tells the debugger where you table is.
;;
;; Next, place the cursor in a buffer you which to run the bovinator
;; on, and execute the command `semantic-bovinate-buffer-debug'.  This
;; will parse the table, and highlight the relevant areas and walk
;; through the match list with the cursor, displaying the current list
;; of values (which is always backwards.)
;;
;; DESIGN ISSUES:
;; -------------
;;
;;  At the moment, the only thing I really dislike is the RESULT
;;  LAMBDA format.  While having some good defaults is nice, the use
;;  of append and list in the lambda seems unnecessarily complex.
;;
;;  Also of issue, I am still not sure I like the idea of stripping
;;  BEGIN/END off of nonterminal tokens passed down in VALS.  While they
;;  are often unnecessary, I can imagine that they could prove useful.
;;  Only time will tell.

;;; History:
;; 

(require 'working)
(require 'semantic-util)

(defgroup semantic nil
  "File and tag browser frame."
  )

;;; Code:

;;; Compatibility
;;
(if (fboundp 'make-overlay)
    (progn
      (defalias 'semantic-make-overlay 'make-overlay)
      (defalias 'semantic-overlay-put 'overlay-put)
      (defalias 'semantic-overlay-get 'overlay-get)
      (defalias 'semantic-overlay-delete 'delete-overlay)
      (defalias 'semantic-overlays-at 'overlays-at)
      (defalias 'semantic-overlay-buffer 'overlay-buffer)
      (defalias 'semantic-overlay-start 'overlay-start)
      (defalias 'semantic-overlay-end 'overlay-end)
      )
  (defalias 'semantic-make-overlay 'make-extent)
  (defalias 'semantic-overlay-put 'set-extent-property)
  (defalias 'semantic-overlay-get 'get-extent-property)
  (defalias 'semantic-overlay-delete 'delete-extent)
  (defalias 'semantic-overlays-at 'extents-at)
  (defalias 'semantic-overlay-buffer 'extent-buffer)
  (defalias 'semantic-overlay-start 'extent-start)
  (defalias 'semantic-overlay-end 'extent-end)
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

 (\"NAME\" type-symbol [\"TYPE\"] ... \"DOCSTRING\" OVERLAY)

Where type-symbol is the type of return token found, and NAME is it's
name.  If there is any typing information needed to describe this
entry, make that come third.  Next, any additional information follows
the optional type.  The last data entry can be the position in the buffer
of DOCSTRING.  A docstring does not have to exist in the form used by
Emacs Lisp.  It could be the text of a comment appearing just before a
function call, or in line with a variable.

The last element must be OVERLAY.
The OVERLAY is automatically created by semantic from an input that
consists of START and END.  When building a parser table, use START
and END as positions in the buffer.

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

 (\"NAME\" variable \"TYPE\" CONST DEFAULT-VALUE MODIFIERS [OPTSUFFIX] 
           \"DOCSTRING\" OVERLAY)
   The definition of a variable, or constant.
   CONST is a boolean representing if this variable is considered a constant.
   DEFAULT-VALUE can be something apropriate such a a string,
                 or list of parsed elements.
   MODIFIERS are details about a variable that are not covered in the TYPE.
   OPTSUFFIX is an optional field specifying trailing modifiers such as
             array dimentions or bit fields.
   DOCSTRING is optional.

 (\"NAME\" function \"TYPE\" ( ARG-LIST ) MODIFIERS [THROWS]
          \"DOCSTRING\" OVERLAY)
   A function/procedure definition.
   ARG-LIST is a list of variable definitions.
   THROWS is an optional argument for functions or methods in languages
   that support typed signal throwing.
   DOCSTRING is optional.

 (\"NAME\" type \"TYPE\" ( PART-LIST ) ( PARENTS ) MODIFIERS
          \"DOCSTRING\" OVERLAY)
   A type definition.
   TYPE of a type could be anything, such as (in C) struct, union, typedef,
   or class.
   PART-LIST is only useful for structs that have multiple individual parts.
            (It is recommended that these be variables, functions or types).
   PARENTS is strictly for classes where there is inheritance.
   

 (\"FILE\" include SYSTEM \"DOCSTRING\" OVERLAY)
   In C, an #include statement.  In elisp, a require statement.
   Indicates additional locations of sources or definitions.
   SYSTEM is true if this include is part of a set of system includes.

 (\"NAME\" package DETAIL \"DOCSTRING\" OVERLAY)
   In Emacs Lisp, a `provide' statement.  DETAIL might be an
   associated file name.

OTHER ENTRIES:")
(make-variable-buffer-local 'semantic-toplevel-bovine-table)

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

(defvar semantic-expand-nonterminal nil
  "Function to call for each returned Non-terminal.
Return a list of non-terminals derived from the first argument, or nil
if it does not need to be expanded.")
(make-variable-buffer-local 'semantic-expand-nonterminal)

(defvar semantic-toplevel-bovine-cache nil
  "A cached copy of a recent bovination, plus state.
If no significant changes have been made (based on the state) then
this is returned instead of re-parsing the buffer.")
(make-variable-buffer-local 'semantic-toplevel-bovine-cache)

(defvar semantic-toplevel-bovine-cache-check nil
  "The size of the buffer when `semantic-toplevel-bovine-cache' was found.")
(make-variable-buffer-local 'semantic-toplevel-bovine-cache-check)

(defvar semantic-toplevel-bovinate-override nil
  "Local variable set by major modes which provide their own bovination.
This function should behave as the function `semantic-bovinate-toplevel'.")
(make-variable-buffer-local 'semantic-toplevel-bovinate-override)


;;; Utility API functions
;;
;; These functions use the flex and bovination engines to perform some
;; simple tasks useful to other programs.  These are just the most
;; critical entries.
;;
;; See semantic-util for a wider range of utility functions and macros.
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

(defun semantic-clear-toplevel-cache ()
  "Clear the toplevel bovin cache for the current buffer."
  (interactive)
  (setq semantic-toplevel-bovine-cache nil)
  ;; Nuke all semantic overlays.  This is faster than deleting based
  ;; on our data structure.
  (let ((l (overlay-lists)))
    (mapcar 'semantic-delete-overlay-maybe (car l))
    (mapcar 'semantic-delete-overlay-maybe (cdr l))
    )
  )

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
  (let ((p (nth (- (length token) 3) token)))
    (if (and p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (semantic-flex-text (car (semantic-flex p (1+ p)))))
      p)))

(defmacro semantic-token-overlay (token)
  "Retrieve the OVERLAY part of TOKEN."
  `(nth (- (length ,token) 1) ,token))

(defmacro semantic-token-extent (token)
  "Retrieve the extent (START END) of TOKEN."
  `(let ((over (semantic-token-overlay ,token)))
     (list (semantic-overlay-start over) (semantic-overlay-end over))))

(defmacro semantic-token-start (token)
  "Retrieve the start location of TOKEN."
  `(semantic-overlay-start (semantic-token-overlay ,token)))

(defmacro semantic-token-end (token)
  "Retrieve the end location of TOKEN."
  `(semantic-overlay-end (semantic-token-overlay ,token)))

(defun semantic-token-p (token)
  "Return non-nil if TOKEN is most likely a semantic token."
  (and (listp token)
       (stringp (car token))
       (symbolp (car (cdr token)))))

;;;###autoload
(defun semantic-bovinate-toplevel (&optional depth trashcomments checkcache)
  "Bovinate the entire current buffer to a list depth of DEPTH.
DEPTH is optional, and defaults to 0.
Optional argument TRASHCOMMENTS indicates that comments should be
stripped from the main list of synthesized tokens.
If third argument CHECKCACHE is non-nil, then flush the cache iff
there has been a size change."
  (if (and semantic-toplevel-bovine-cache
	   checkcache
	   (not (eq (point-max) semantic-toplevel-bovine-cache-check)))
      (semantic-clear-toplevel-cache))
  (cond
   (semantic-toplevel-bovinate-override
    (funcall semantic-toplevel-bovinate-override depth trashcomments))
   ((and semantic-toplevel-bovine-cache
	 (car semantic-toplevel-bovine-cache)
	 ;; Add a rule that knows how to see if there have
	 ;; been "big chagnes"
	 )
    (car semantic-toplevel-bovine-cache))
   (t
    (let ((ss (semantic-flex (point-min) (point-max)
			     (or depth 0)))
	  (res nil)
	  (semantic-overlay-error-recovery-stack nil))
      ;; Init a dump
      (if semantic-dump-parse (semantic-dump-buffer-init))
      ;; Parse!
      (working-status-forms (buffer-name) "done"
	(setq res
	      (semantic-bovinate-nonterminals
	       ss 'bovine-toplevel depth trashcomments))
	(working-status t))
      (setq semantic-toplevel-bovine-cache
	    (list (nreverse res) (point-max))
	    semantic-toplevel-bovine-cache-check
	    (point-max))
      (car semantic-toplevel-bovine-cache)))))

(defun semantic-bovinate-nonterminals (stream nonterm &optional
					      depth trashcomments
					      returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
DEPTH is optional, and defaults to 0.
Optional argument TRASHCOMMENTS indicates that comments should be
stripped from the main list of synthesized tokens.
Optional argument RETURNONERROR indicates that the parser should exit with
the current results on a parse error."
  (if (not depth) (setq depth 0))
  (let ((result nil) (case-fold-search semantic-case-fold))
    (while stream
      (if (not (and trashcomments (eq (car (car stream)) 'comment)))
	  (let* ((nontermsym
		  (semantic-bovinate-nonterminal
		   stream semantic-toplevel-bovine-table nonterm))
		 (stream-overlays (car (cdr (cdr nontermsym))))
		 (tmpet nil)
		 (token (car (cdr nontermsym)))
		 (startcdr (nthcdr (- (length token) 2) token)))
	    (if (not nontermsym)
		(error "Parse error @ %d" (car (cdr (car stream)))))
	    (semantic-overlay-stack-add stream-overlays)
	    (if token
		(let ((o
		       (condition-case nil
			   (semantic-make-overlay (car startcdr)
						  (car (cdr startcdr))
						  (current-buffer)
						  ;; Examin start/rear
						  ;; advance flags.
						  )
			 (error (debug token)
				nil))))
		  ;; Convert START/END into an overlay.
		  (setcdr startcdr nil)
		  (setcar startcdr o)
		  (semantic-overlay-put o 'semantic token)
		  ;; Expand based on local configuration
		  (if (not semantic-expand-nonterminal)
		      ;; no expanders
		      (setq result (cons token result))
		    ;; Glom generated tokens
		    (setq tmpet (funcall semantic-expand-nonterminal token))
		    (if (not tmpet)
			(progn (setq result (cons token result))
			       (semantic-overlay-stack-add o))
		      ;; Fixup all overlays, start by deleting the old one
		      (let ((motok tmpet) o start end)
			(while motok
			  (setq startcdr (nthcdr (- (length (car motok)) 1)
						 (car motok))
				;; this will support new overlays created by
				;; the special function, or recycles
				start (or (semantic-overlay-start
					   (car startcdr)) start)
				end (or (semantic-overlay-end
					 (car startcdr)) end)
				o (semantic-make-overlay start end
							 (current-buffer)))
			  (if (semantic-overlay-buffer (car startcdr))
			      (semantic-overlay-delete (semantic-token-overlay
							(car motok))))
			  (semantic-overlay-stack-add o)
			  (setcdr startcdr nil)
			  (setcar startcdr o)
			  (semantic-overlay-put o 'semantic (car motok))
			  (setq motok (cdr motok))))
		      (setq result (append tmpet result)))))
	      (if returnonerror (setq stream nil))
	      ;;(error "Parse error")
	      )
	    ;; Designated to ignore.
	    (setq stream (car nontermsym)))
	(setq stream (cdr stream)))
      (if stream
	  (working-status (floor
			   (* 100.0 (/ (float (car (cdr (car stream))))
				       (float (point-max))))))))
    result))

;;; Semantic Table debugging
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
  (if (not semantic-bovinate-debug-table)
      (error
       "Call `semantic-bovinate-debug-set-table' from your semantic table"))
  (let ((semantic-edebug t))
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer (marker-buffer semantic-bovinate-debug-table))
    (other-window 1)
    (semantic-clear-toplevel-cache)
    (semantic-bovinate-toplevel nil t)))

(defun semantic-bovinate-show (lse nonterminal matchlen tokenlen collection)
  "Display some info about the current parse.
Returns 'fail if the user quits, nil otherwise.
LSE is the current listed syntax element.
NONTERMINAL is the current nonterminal being parsed.
MATCHLEN is the number of match lists tried.
TOKENLEN is the number of match tokens tried.
COLLECTION is the list of things collected so far."
  (let ((ol1 nil) (ol2 nil) (ret nil))
    (unwind-protect
	(progn
	  (goto-char (car (cdr lse)))
	  (setq ol1 (semantic-make-overlay (car (cdr lse)) (cdr (cdr lse))))
	  (semantic-overlay-put ol1 'face 'highlight)
	  (goto-char (car (cdr lse)))
	  (if window-system nil (sit-for 1))
	  (other-window 1)
	  (set-buffer (marker-buffer semantic-bovinate-debug-table))
	  (goto-char semantic-bovinate-debug-table)
	  (re-search-forward
	   (concat "^\\s-*\\((\\|['`]((\\)\\(" (symbol-name nonterminal)
		   "\\)[ \t\n]+(")
	   nil t)
	  (setq ol2 (semantic-make-overlay (match-beginning 2) (match-end 2)))
	  (semantic-overlay-put ol2 'face 'highlight)
	  (forward-char -2)
	  (forward-list matchlen)
	  (skip-chars-forward " \t\n(")
	  (forward-sexp tokenlen)
	  (message "%s: %S" lse collection)
	  (let ((e (read-event)))
	    (cond ((eq e ?f)		;force a failure on this symbol.
		   (setq ret 'fail))
		  (t nil)))
	  (other-window 1)
	  )
      (semantic-delete-overlay ol1)
      (semantic-delete-overlay ol2))
    ret))

(eval-when-compile (require 'pp))

(defun bovinate (&optional clear)
  "Bovinate the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before bovinating."
  (interactive "P")
  (if clear (semantic-clear-toplevel-cache))
  (let ((out (semantic-bovinate-toplevel nil t)))
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


;;; Semantic Bovination
;;
;; Take a semantic token stream, and convert it using the bovinator.
;; The bovinator takes a state table, and converts the token stream
;; into a new semantic stream defined by the bovination table.
;;
(defun semantic-bovinate-nonterminal (stream table &optional nonterminal)
  "Bovinate STREAM based on the TABLE of nonterminal symbols.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Use `bovine-toplevel' if it is not provided."
  (if (not nonterminal) (setq nonterminal 'bovine-toplevel))
  (let ((ml (assq nonterminal table)))
    (semantic-bovinate-stream stream (cdr ml) table)))

(defsubst semantic-bovinate-symbol-nonterminal-p (sym table)
  "Return non-nil if SYM is in TABLE, indicating it is NONTERMINAL."
  ;; sym is always a sym, so assq should be ok.
  (if (assq sym table) t nil))

(defun semantic-bovinate-stream (stream matchlist table)
  "Bovinate STREAM using MATCHLIST resolving nonterminals with TABLE.
This is the core routine for converting a stream into a table.
See the variable `semantic-toplevel-bovine-table' for details on the
format of MATCHLIST.
Return the list (STREAM SEMANTIC-STREAM OVERLAYS) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found.  OVERLAYS is the list of overlays found
so far, to be used in the error recovery stack."
  (let ((s   nil)			;Temp Stream Tracker
	(lse nil)			;Local Semantic Element
	(lte nil)			;Local matchlist element
	(tev nil)			;Matchlist entry values from buffer
	(val nil)			;Value found in buffer.
	(cvl nil)			;collected values list.
	(out nil)			;Output
	(s-stack nil)			;rollback stream stack
	(start nil)			;the beginning and end.
	(end nil)
	(db-mlen (length matchlist))
	(db-tlen 0)
	(semantic-overlay-error-recovery-stack nil) ;part of error recovery
	)
    ;; prime the rollback stack
    (setq s-stack (cons stream s-stack)
	  start (car (cdr (car stream)))
	  end (cdr (cdr (car stream))))
    (while matchlist
      (setq s (car s-stack)		;init s from the stack.
	    cvl nil			;re-init the collected value list.
	    lte (car matchlist)		;Get the local matchlist entry.
	    db-tlen (length lte))	;length of the local match.
      (if (or (byte-code-function-p (car lte))
	      (listp (car lte)))
	  ;; In this case, we have an EMPTY match!  Make stuff up.
	  (setq cvl (list nil)))
      (while (and lte (not (or (byte-code-function-p (car lte))
			       (listp (car lte)))))
	;; debugging!
	(if (and lte semantic-edebug)
	    ;; The below reference to nonterminal is a hack and the byte
	    ;; compiler will complain about it.
	    (let ((r (semantic-bovinate-show (car s) nonterminal
					     (- db-mlen (length matchlist))
					     (- db-tlen (length lte))
					     cvl)))
	      (cond ((eq r 'fail)
		     (setq lte '(trash 0 . 0)))
		    (t nil))))
	(cond
	 ;; We have a nonterminal symbol.  Recurse inline.
	 ((semantic-bovinate-symbol-nonterminal-p (car lte) table)
	  (let ((nontermout (semantic-bovinate-nonterminal s table (car lte))))
	    (setq s (car nontermout)
		  val (car (cdr nontermout))
		  ov (car (cdr (cdr nontermout))))
	    (if ov (semantic-overlay-stack-add ov))
	    (if val
		(let ((len (length val))
		      (strip (nreverse (cdr (cdr (reverse val))))))
		  (if semantic-dump-parse
		      (semantic-dump-detail (cdr nontermout)
					    (car lte)
					    ""
					    "NonTerm Match"))
		  (setq end (nth (1- len) val) ;reset end to the end of exp
			cvl (cons strip cvl) ;prepend value of exp
			lte (cdr lte)) ;update the local table entry
		  )
	      ;; No value means that we need to terminate this match.
	      (semantic-overlay-stack-clear)
	      (setq lte nil cvl nil)) ;No match, exit
	    ))
	 ;; Default case
	 (t
	  (setq lse (car s)		;Get the local stream element
		s (cdr s))		;update stream.
	  ;; trash comments if it's turned on
	  (while (eq (car (car s)) 'comment)
	    (setq s (cdr s)))
	  ;; Do the compare
	  (if (eq (car lte) (car lse))	;syntactic match
	      (let ((valdot (cdr lse)))
		(setq val (semantic-flex-text lse))
		;; DEBUG SECTION
		(if semantic-dump-parse
		    (semantic-dump-detail
		     (if (stringp (car (cdr lte)))
			 (list (car (cdr lte)) (car lte))
		       (list (car lte)))
		     nonterminal val
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
			  (setq cvl (cons val cvl)) ;append this value
			(semantic-overlay-stack-clear)
			(setq lte nil cvl nil))) ;clear the entry (exit)
		  (setq cvl (cons
			     (if (member (car lse)
					 '(comment semantic-list))
				 valdot val) cvl))) ;append unchecked value.
		(setq end (cdr (cdr lse)))
		)
	    (if (and semantic-dump-parse nil)
		(semantic-dump-detail (car lte)
				      nonterminal (semantic-flex-text lse)
				      "Term Type Fail"))
	    (semantic-overlay-stack-clear)
	    (setq lte nil cvl nil)) 	;No more matches, exit
	  )))
      (if (not cvl)			;lte=nil;  there was no match.
	  (setq matchlist (cdr matchlist)) ;Move to next matchlist entry
	(setq out (if (car lte)
; REMOVE THIS TO USE THE REFERENCE/COMPARE CODE
; 		      (let ((o (apply (car lte)	;call matchlist fn on values
; 				      (nreverse cvl) start (list end))))
; 			(if semantic-bovinate-create-reference (semantic-bovinate-add-reference o))
; 			(if semantic-bovinate-compare-reference (semantic-bovinate-compare-against-reference o))
; 			o
; 			)
		      (funcall (car lte)	;call matchlist fn on values
			       (nreverse cvl) start end)
		    (cond ((and (= (length cvl) 1)
				(listp (car cvl))
				(not (numberp (car (car cvl)))) )
			   (append (car cvl) (list start end)))
			  (t
			   (append (nreverse cvl) (list start end))))
		    )
	      matchlist nil)		;generate exit condition
	(if (not end) (setq out nil))
	;; Nothin?
	))
    (list s out semantic-overlay-error-recovery-stack)))


;;; Bovine table functions
;;
;; These are functions that can be called from within a bovine table.
;; Most of these have code auto-generated from other construct in the BNF.
(defmacro semantic-lambda (&rest return-val)
  "Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table."
  `(lambda (vals start end)
     (append ,@return-val (list start end))))

(defun semantic-bovinate-from-nonterminal (start end nonterm &optional depth)
  "Bovinate from within a nonterminal lambda from START to END.
Depends on the existing environment created by `semantic-bovinate-stream'.
Argument NONTERM is the nonterminal symbol to start with.
Optional argument DEPTH is the depth of lists to dive into.
Whan used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (car-safe (cdr (semantic-bovinate-nonterminal
		  (semantic-flex start end (or depth 1))
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
				   depth
				   ;; Compiler will complain.
				   trashcomments)))

(defun semantic-bovinate-block-until-header (start end nonterm &optional depth)
  "Bovinate between START and END starting with NONTERM.
If NONTERM is nil, start with `bovine-block-toplevel'.
This command will parse until an error is encountered, and return
the list of everything found until that moment.
This is meant for finding variable definitions at the beginning of
code blocks in methods.  If `bovine-block-toplevel' can also support
commands, use `semantic-bovinate-from-nonterminal-full'."
  (nreverse
   (semantic-bovinate-nonterminals (semantic-flex start end (or depth 1))
				   nonterm
				   depth
				   (or (symbol-value 'trashcomments)
				       t)
				   ;; This says stop on an error.
				   t)))


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

(defvar semantic-flex-extensions nil
  "Buffer local extensions to the lexical analyzer.
This should contain an alist with a key of a regex and a data element of
a function.  The function should both move point, and return a lexical
token of the form ( TYPE START .  END).  nil is also a valid return.")
(make-variable-buffer-local 'semantic-flex-extensions)

(defvar semantic-flex-keywords-obarray nil
  "Buffer local keyword obarray for the lexical analyzer.
These keywords are matched explicitly, and converted into special symbols.")
(make-variable-buffer-local 'semantic-flex-keywords-obarray)

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

(defun semantic-flex-make-keyword-table (keywords)
  "Convert a list of KEYWORDS into an obarray.
Save the obarry into `semantic-flex-keywords-obarray'."
  ;; Create the symbol hash table
  (let ((obarray (make-vector 13 nil)))
    ;; fill it with stuff
    (while keywords
      (set (intern (car (car keywords)) obarray)
	   (cdr (car keywords)))
      (setq keywords (cdr keywords)))
    obarray))

(defun semantic-flex-is-keyword (text)
  "Return a symbol if TEXT is a keyword."
  (let ((sym (intern-soft text semantic-flex-keywords-obarray)))
    (if sym (symbol-value sym))))

(defun semantic-flex-buffer (&optional depth)
  "Sematically flex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-flex (point-min) (point-max) depth))

(defun semantic-flex (start end &optional depth)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element being a list, such
as (symbol start-expression .  end-expresssion).
END does not mark the end of text scanned, only the end of the beginning
of text scanned.  Thus, if a string extended past END, the end of the
return token will be larger than END.  To truly restrict scanning, using
`narrow-to-region'."
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
	(mods semantic-flex-syntax-modifications))
    ;; Update the syntax table
    (while mods
      (modify-syntax-entry (car (car mods)) (car (cdr (car mods))) newsyntax)
      (setq mods (cdr mods)))
    (with-syntax-table newsyntax
      (goto-char start)
      (while (< (point) end)
	(cond (;; catch newlines when needed
	       (and semantic-flex-enable-newlines
		    (looking-at "\n"))
	       (setq ts (cons (cons 'newline
				    (cons (match-beginning 0) (match-end 0)))
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
			       (or (semantic-flex-is-keyword (match-string 0))
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
					   (error (goto-char (point-max))))
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
	       ;; Zing to the end of this comment.
	       (if (eq (car (car ts)) 'comment)
		   (setcdr (cdr (car ts)) (save-excursion
					    (forward-comment 1)
					    (setq ep (point))))
		 (setq ts (cons (cons 'comment
				      (cons (match-beginning 0)
					    (save-excursion
					      (forward-comment 1)
					      (setq ep (point)))))
				ts))))
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

(provide 'semantic)

;;; semantic.el ends here

