;;; semantic-analyze.el --- Analyze semantic tokens against local context

;;; Copyright (C) 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-analyze.el,v 1.3 2001/10/08 21:08:59 zappo Exp $

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
;; Semantic, as a tool, provides a nice list of searchable tokens.
;; That information can provide some very accurate answers if the current
;; context of a position is known.
;;
;; Semantic-ctxt provides ways of analyzing, and manipulating the
;; semantic context of a language in code.
;;
;; This library provides routines for finding intelligent answers to
;; tough problems, such as if an argument to a function has the correct
;; return type, or all possible tokens that fit in a given local context.
;;

(require 'eieio)
(require 'semantic-ctxt)
(eval-when-compile (require 'semanticdb))

;;; Code:

;;; Context Analysis
;;
(defun semantic-analyze-find-nonterminals-by-prefix (prefix)
  "Attempt to find a nonterminal with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (let ((expr (concat "^" (regexp-quote prefix))))
    (if (and (fboundp 'semanticdb-minor-mode-p)
	     (semanticdb-minor-mode-p))
	;; Search the database
	(let ((dbans (semanticdb-find-nonterminal-by-name-regexp
		      expr  nil nil t nil t)))
	  ;; Concatenate all matches together.
	  (apply #'append (mapcar #'cdr dbans))
	  )
      ;; Search just this file because there is no DB available.
      (semantic-find-nonterminal-by-name-regexp
       expr (current-buffer) nil t))))
  
(defun semantic-analyze-find-nonterminal (name &optional tokentype)
  "Attempt to find a nonterminal with NAME.
Optional argument TOKENTYPE specifies tye type of token to
return, such as 'function or 'variable.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
	   (semanticdb-minor-mode-p))
      ;; Search the database
      (let ((dbans (semanticdb-find-nonterminal-by-name
		    name nil nil t nil t)))
	;; Lame, grabbing the first file match
	(cdr (car dbans)))
    ;; Search just this file
    (semantic-find-nonterminal-by-name
     name (current-buffer) nil t)))

(defun semantic-analyze-token-type-to-name (token)
  "Get the name of TOKEN's type.
The TYPE field in a token can be nil (return nil)
or a string, or a non-positional token."
  (let ((tt (semantic-token-type token)))
    (cond ((semantic-token-p tt)
	   (semantic-token-name tt))
	  ((stringp tt)
	   tt)
	  ((listp tt)
	   (car tt))
	  (t nil))))

(defun semantic-analyze-find-nonterminal-sequence (sequence &optional localvar scope typereturn)
  "Attempt to find all nonterminals in SEQUENCE.
Optional argument SCOPE are additional terminals to search which are currently
scoped.  These are no local variables, but symbols available in a structure
which doesn't need to be dereferneced.
Optional argument TYPERETURN is a symbol ini which the types of all found
will be stored.  If nil, that data is thrown away."
  (let ((s sequence)			;copy of the sequence
	(tmp nil)			;tmp find variable
	(nexttype nil)			;a token for the type next in sequence
	(tok nil)			;token return list
	(toktype nil)			;token types return list
	)
    ;; For the first entry, it better be a variable, but it might
    ;; be in the local context too.
    (setq tmp (or (semantic-analyze-find-nonterminal (car s) 'variable)
		  (semantic-find-nonterminal-by-name
		   (car s) scope)
		  (semantic-find-nonterminal-by-name
		   (car s) (semantic-get-local-arguments))
		  (semantic-find-nonterminal-by-name
		   (car s) localvar)))

    (if (and (listp tmp) (semantic-token-p (car tmp)))
	;; We should be smarter... :(
	(setq tmp (car tmp)))
    (if (not (semantic-token-p tmp))
	(error "Cannot find definition for \"%s\"" (car s)))
    (setq s (cdr s))
    (setq tok (cons tmp tok))

    ;; For the middle entries
    (while s
      ;; Using the token found in TMP, lets find the token
      ;; representing the full typeographic information of it's
      ;; type, and use that to determine the search context for
      ;; (car s)
      (let ((tmptype (semantic-analyze-token-type-to-name tmp))
	    (tmptypetok nil)
	    (slots nil))
	
	(if tmptype
	    (setq tmptypetok
		  (or (semantic-analyze-find-nonterminal tmptype)
		      ;; In some languages, types can be
		      (semantic-find-nonterminal-by-name
		       (car s) localvar)))
	  (error "Semantic token %s has no type info"
		 (semantic-token-name tmp)))

	(if (and (listp tmptypetok) (semantic-token-p (car tmptypetok)))
	    ;; We should be smarter... :(
	    (setq tmptypetok (car tmptypetok)))

	;; Now that we have the type associated with TMP, we can find
	;; fields in that structure.  Make sure we really have a type
	;; first though.
	(if (not (and tmptypetok
		      (eq (semantic-token-token tmptypetok) 'type)))
	    (error "Symbol %s type %s is not a known aggregate type"
		   (semantic-token-name tmp)
		   tmptype))

	;; get the slots
	(setq slots (semantic-token-type-parts tmptypetok))

	;; find (car s) in the list o slots
	(setq tmp (semantic-find-nonterminal-by-name (car s)
						     slots nil nil))
	
	(if (and (listp tmp) (semantic-token-p (car tmp)))
	    ;; We should be smarter... :(
	    (setq tmp (car tmp)))

	(if (not (semantic-token-p tmp))
	    (if (cdr s)
		;; In the middle, we need to keep seeking our types out.
		(error "Cannot find definition for \"%s\"" (car s))
	      ;; Else, it's ok to end with a non-token
	      (setq tmp (car s))))

	(setq tok (cons tmp tok))
	(setq toktype (cons tmptypetok toktype))
	)
      (setq s (cdr s)))

    (if typereturn (set typereturn (nreverse toktype)))
    ;; Return the mess
    (nreverse tok)))

(defun semantic-analyze-type-parts (type)
  "Return all parts of TYPE, a nonterminal representing a TYPE declaration.
This includes both the TYPE parts, and all functions found in all
databases which have this type as a property."
  (let ((slots (semantic-token-type-parts type))
	(extmeth
	 (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
	     (apply #'append
		    (mapcar #'cdr
			    (semanticdb-find-nonterminal-by-extra-spec-value
			     'parent (semantic-token-name type)
			     nil nil nil t)))
	   (semantic-find-nonterminal-by-extra-spec-value
	    'parent (semantic-token-name type) nil nil))))
    ;; Flatten the database output.
    (append slots extmeth)
    ))

(defun semantic-analyze-scoped-nonterminals (typelist)
  "Return a list of nonterminals accessable when TYPELIST is in scope.
Tokens returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\"."
  (apply #'append (mapcar #'semantic-analyze-type-parts typelist)))

(defun semantic-analyze-scoped-types (&optional position)
  "Return a list of types current in scope at POSITION.
This is based on what tokens exist at POSITION, and any associated
types available."
  (save-excursion
    (if position (goto-char position))
    (let ((tok (semantic-current-nonterminal))
	  (parent nil))
      (setq parent
	    (when tok
	      (if (eq (semantic-token-token tok) 'function)
		  ;; If TOK is a function, it may have a parent class.
		  ;; Find it.
		  (let ((p (semantic-token-function-parent tok)))
		    (if p
			;; We have a parent, search for it.
			(let ((ptok (semantic-analyze-find-nonterminal
				     (cond ((stringp p) p)
					   ((semantic-token-p p)
					    (semantic-token-name p))
					   ((and (listp p) (stringp (car p)))
					    (car p))) 'type)))
			  ptok)
		  ;; No specified parent.  See if there is a parent by
		      ;; position
		      (setq p (semantic-current-nonterminal-parent))
		      p)))))
      ;; We should search the hierarchy for all parent classes
      ;; as well.
      (if parent
	  (list parent))
      )))

;;; Top Level context analysis function
;;
(defclass semantic-analyze-context ()
  ((bounds :initarg :bounds
	   :type list
	   :documentation "The bounds of this context.
Usually bound to the dimension of a single symbol or command.")
   (prefix :initarg :prefix
	   :type list
	   :documentation "List of tokens defining local text.
This can be nil, or a list where the last element can be a string
representing text that may be incomplete.  Preceeding elements
must be semantic tokens representing variables or functions
called in a dereference sequence.")
   (prefixtypes :initarg :prefixtypes
	   :type list
	   :documentation "List of tokens defining types for :prefix.
This list is one shorter than :prefix.  Each element is a semantic
token representing a type matching the semantic token in the same
position in PREFIX.")
   (scopetypes :initarg :scopetypes
	       :type list
	       :documentation "List of type tokens in scope.
When in a function is called, it may have certain other types
in scope, such as classes in it's lineage.  This is a list
of all those classes.")
   (scope :initarg :scope
	  :type list
	  :documentation "List of tokens available in scopetype.
See `semantic-analyze-scoped-nonterminals' for details.")
   (localvariables :initarg :localvariables
		   :initform nil
		   :type list
		   :documentation "List of local variables.
Local variables are defined withing the code scope.")
   )
  "Base analysis data for a any context.")

(defclass semantic-analyze-context-assignment (semantic-analyze-context)
  ((assignee :initarg :assignee
	     :type list
	     :documentation "A sequence of tokens for an assignee.
This is a variable into which some value is being placed.  The last
item in the list is the variable accepting the value.  Earlier
tokens represent the variables being derefernece to get to the
assignee."))
  "Analysis class for a value in an assignment.")

(defclass semantic-analyze-context-functionarg (semantic-analyze-context)
  ((function :initarg :function
	     :type list
	     :documentation "A sequence of tokens for a function.
This is a function being called.  The cursor will be in the position
of an argument.
The last token in :function is the function being called.  Earlier
tokens represent the variables being dereferenced to get to the
function.")
   (index :initarg :index
	  :type integer
	  :documentation "The index of the argument for this context.
If a function takes 4 arguments, this value should be bound to
the values 1 through 4.")
   (argument :initarg :argument
	     :type list
	     :documentation "A sequence of tokens for the :index argument.
The argument can accept a value of some type, and this contains the 
nonterminal for that definition.  It should be a nonterminal, but might
be just a string in some circumstances.")
   )
  "Analysis class for a value as a function argument.")

(defclass semantic-analyze-context-return (semantic-analyze-context)
  () ; No extra data.
  "Analysis class for return data.
Return data methods identify the requred type by the return value
of the parent function.") 

(defun semantic-analyze-current-context (position)
  "Analyze the current context at POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'."
  (interactive "d")
  (save-excursion
    (goto-char position)
    (let* ((context-return nil)
	   (prefix (semantic-ctxt-current-symbol))
	   (prefixtypes nil)
	   (scopetypes (semantic-analyze-scoped-types position))
	   (scope (if scopetypes
		      (semantic-analyze-scoped-nonterminals scopetypes)))
	   (function (semantic-ctxt-current-function))
	   (localvar (semantic-get-local-variables)))

      (condition-case nil
	  ;; If we are on lame stuff, it won't be found!
	  (setq prefix (semantic-analyze-find-nonterminal-sequence
			prefix localvar scope 'prefixtypes))
	(error nil))

      (if function
	  ;; If we have a function, then we can get the argument
	  (let* ((arg (semantic-ctxt-current-argument))
		 (fntok (semantic-analyze-find-nonterminal-sequence function localvar scope))
		 (fntokend (car (reverse fntok)))
		 (argtok (nth (1- arg)
			      (semantic-token-function-args fntokend))))

	    (setq context-return
		  (semantic-analyze-context-functionarg
		   "functionargument"
		   :function fntok
		   :index arg
		   :argument (list argtok)
		   :scope scope
		   :scopetypes scopetypes
		   :localvariables localvar
		   :prefix prefix
		   :prefixtypes prefixtypes))
	    )
	;; No function, try assignment
	(let ((assign (semantic-ctxt-current-assignment)))
	  (if assign
	      ;; We have an assignment
	      (let ((asstok (semantic-analyze-find-nonterminal-sequence
			     assign localvar scope)))
		(setq context-return
		      (semantic-analyze-context-assignment
		       "assignment"
		       :assignee asstok
		       :scope scope
		       :scopetypes scopetypes
		       :localvariables localvar
		       :prefix prefix
		       :prefixtypes prefixtypes)))

	    ;; TODO: Identify return value condition.

	    ;; Nothing in particular
	    (setq context-return
		  (semantic-analyze-context
		   "context"
		   :scope scope
		   :scopetypes scopetypes
		   :localvariables localvar
		   :prefix prefix
		   :prefixtypes prefixtypes)))))

      ;; Check for interactivity
      (if (interactive-p)
	  (semantic-analyze-pop-to-context context-return))
      ;; Return our context.
      context-return)))


;;; Context Analysis Results
;;
;; Use the current context to provide useful token streams
(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context) &optional desired-type)
  "Return a type constraint for completing :prefix in CONTEXT.
Optional argument DESIRED-TYPE may be a non-type token to analyze."
  (when desired-type
    ;; Convert the desired type if needed.
    (if (not (eq (semantic-token-token desired-type) 'type))
	(setq desired-type (semantic-token-type desired-type)))
    ;; Protect against plain strings
    (cond ((stringp desired-type)
	   (setq desired-type (list desired-type 'type)))
	  ((and (stringp (car desired-type))
		(not (semantic-token-p desired-type)))
	   (setq desired-type (list (car desired-type) 'type)))
	  (t (setq desired-type nil))
	  ))
  desired-type)

(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context-functionarg))
  "Return a type constraint for completing :prefix in CONTEXT."
  (call-next-method context (car (oref context argument))))

(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context-assignment))
  "Return a type constraint for completing :prefix in CONTEXT."
  (call-next-method context (car (reverse (oref context assignee)))))


(defun semantic-analyze-possible-completions (point)
  "Return a list of semantic tokens which are possible completions.
Analysis is done at POINT."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (fnargs (semantic-get-local-arguments point))
	 (desired-type (semantic-analyze-type-constraint a))
	 (prefix (oref a prefix))
	 (prefixtypes (oref a prefixtypes))
	 (completetext nil)
	 (completetexttype nil)
	 (c nil))

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-token-p completetext)
	(setq completetext (semantic-token-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (if (or (not completetexttype)
	    (not (and (semantic-token-p completetexttype)
		      (eq (semantic-token-token completetexttype) 'type))))
	;; What should I do here?  I think this is an error condition.
	(setq completetexttype nil))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if completetexttype

	(setq c (semantic-find-nonterminal-by-name-regexp
		 (concat "^" completetext)
		 (oref a scope)
		 nil nil)
	      
      (let ((expr (concat "^" completetext)))
	;; No type based on the completetext.  This is a free-range
	;; var or function.  We need to expand our search beyond this
	;; scope into semanticdb, etc.
	(setq c (append
		 ;; Argument list
		 (semantic-find-nonterminal-by-name-regexp
		  expr fnargs
		  nil nil)
		 ;; Local variables
		 (semantic-find-nonterminal-by-name-regexp
		  expr (oref a localvariables)
		  nil nil)
		 ;; The world
		 (semantic-analyze-find-nonterminals-by-prefix
		  completetext))
	      )
	))

    ;; Ok, we now have a completion list based on the text we found
    ;; we want to complete on.  Now filter that stream against the
    ;; type we want to search for.
    (if desired-type
	(setq c (semantic-find-nonterminal-by-type
		 (semantic-token-name desired-type)
		 c nil nil)))

    ;; All done!

    ;; If interactive, display them.
    (when (interactive-p)
      (with-output-to-temp-buffer "*Possible Completions*"
	(semantic-analyze-princ-sequence c ""))
      (shrink-window-if-larger-than-buffer
       (get-buffer-window "*Possible Completions*"))
      )
    c))


;;; Friendly output of a context analysis.
;;
(defcustom semantic-analyze-summary-function 'semantic-prototype-nonterminal
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-token->text-functions'."
  :group 'semantic
  :type semantic-token->text-custom-list)

(defun semantic-analyze-princ-sequence (sequence &optional prefix)
  "Send the token SEQUENCE to standard out.
Use PREFIX as a label."
  (while sequence
    (princ prefix)
    (if (semantic-token-p (car sequence))
	(princ (funcall semantic-analyze-summary-function
			(car sequence)))
      (if (stringp (car sequence))
	  (princ (semantic-colorize-text (car sequence) 'variable))
	(format "%S" (car sequence))))
    (princ "\n")
    (setq sequence (cdr sequence))
    (setq prefix (make-string (length prefix) ? ))
    ))

(defmethod semantic-analyze-show ((context semantic-analyze-context))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context prefix) "Prefix: ")
  (semantic-analyze-princ-sequence (oref context prefixtypes) "Prefix Types: ")
  (princ "--------\n")
  (semantic-analyze-princ-sequence (oref context scopetypes) "Scope Types: ")
  (semantic-analyze-princ-sequence (oref context scope) "Scope: ")
  (semantic-analyze-princ-sequence (oref context localvariables) "LocalVars: ")
  )

(defmethod semantic-analyze-show ((context semantic-analyze-context-assignment))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context assignee) "Assignee: ")
  (call-next-method))

(defmethod semantic-analyze-show ((context semantic-analyze-context-functionarg))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context function) "Function: ")
  (princ "Argument Index: ")
  (princ (oref context index))
  (princ "\n")
  (semantic-analyze-princ-sequence (oref context argument) "Argument: ")
  (call-next-method))

(defun semantic-analyze-pop-to-context (context)
  "Display CONTEXT in a temporary buffer.
CONTEXT's content is described in `semantic-analyze-current-context'."
  (with-output-to-temp-buffer "*Semantic Context Analysis*"
    (princ "Context Type: ")
    (princ (object-name context))
    (princ "\n")
    (semantic-analyze-show context)
    )
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Semantic Context Analysis*"))
  )

(provide 'semantic-analyze)

;;; semantic-analyze.el ends here
