;;; semantic-analyze.el --- Analyze semantic tokens against local context

;;; Copyright (C) 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-analyze.el,v 1.6 2002/05/07 01:31:15 zappo Exp $

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

(defun semantic-analyze-dereference-metatype (type)
  "Return a concrete type token based on input TYPE token.
A concrete type is an actual declaration of a memory description,
such as a structure, or class.  A meta type is an alias,
or a typedef in C or C++.  If TYPE is concrete, it
is returned.  If it is a meta type, it will return the concrete
type defined by TYPE.
The behavior can be overriden using `analyze-derefernce-metatype'.
The default behavior always returns TYPE.
Override functions need not return a real semantic token.
Just a name, or short token will be ok.  It will be expanded here."
  (let* ((s (semantic-fetch-overload 'analyze-dereference-metatype)))
    (if s
	(let ((ans (funcall s type)))
	  ;; If ANS is a string, or if ANS is a short token, we
	  ;; need to do some more work to look it up.
	  (cond ((stringp ans)
		 (semantic-analyze-find-nonterminal ans))
		((and (semantic-token-p ans)
		      (eq (semantic-token-token ans) 'type)
		      (semantic-token-type-parts ans))
		 ans)
		((and (semantic-token-p ans)
		      (eq (semantic-token-token ans) 'type)
		      (not (semantic-token-type-parts ans)))
		 (semantic-analyze-find-nonterminal
		  (semantic-token-name ans)))
		(t nil)))
      ;; Nothing fancy, just return type be default.
      type)))

(defun semantic-analyze-token-type (token)
  "Return the semantic token for a type within the type of TOKEN.
TOKEN can be a variable, function or other type of token.
The type of token (such as a class or struct) is a name.
Lookup this name in database, and return all slots/fields
within that types field.  Also handles anonymous types."
  (let ((ttype (semantic-token-type token))
	(name nil)
	(typetoken nil)
	)

    ;; Is it an anonymous type?
    (if (and ttype
	     (semantic-token-p ttype)
	     (eq (semantic-token-token ttype) 'type)
	     (semantic-nonterminal-children ttype))
	;; We have an anonymous type for TOKEN with children.
	;; Use this type directly.
	(semantic-analyze-dereference-metatype ttype)

      ;; Not an anonymous type.  Look up the name of this type
      ;; elsewhere, and report back.
      (setq name (semantic-analyze-token-type-to-name token))
      (if (and name (not (string= name "")))
	  (setq typetoken (semantic-analyze-find-nonterminal name))
	;; No name to look stuff up with.
	(error "Semantic token %S has no type information"
	       (semantic-token-name ttype)))

      ;; Handle lists of tokens.
      (if (and (listp typetoken) (semantic-token-p (car typetoken)))

	  (let ((toklist typetoken))
	    (setq typetoken nil)
	    ;; Loop over all returned elements until we find a type
	    ;; that is a perfect match.
	    (while (and toklist (not typetoken))
	      ;; FIXME: Do better matching.
	      (if (and (car toklist)
		       (eq (semantic-token-token (car toklist)) 'type))
		  (setq typetoken (car toklist)))
	      (setq toklist (cdr toklist)))))

      ;; We now have a token associated with the type.
      (semantic-analyze-dereference-metatype typetoken))))

(defun semantic-analyze-find-nonterminal-sequence (sequence &optional localvar scope typereturn)
  "Attempt to find all nonterminals in SEQUENCE.
Optional argument LOCALVAR is the list of local variables to use when
finding the details on the first element of SEQUENCE in case
it is not found in the global set of tables.
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
    ;; NOTE: Don't forget c++ namespace foo::bar.
    (setq tmp (or (semantic-analyze-find-nonterminal (car s) 'variable)
		  (semantic-find-nonterminal-by-name
		   (car s) scope)
		  (semantic-find-nonterminal-by-name
		   (car s) (semantic-get-local-arguments))
		  ;; This should be first, but bugs in the
		  ;; C parser will turn function calls into
		  ;; assumed int return function prototypes.  Yuck!
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
      (let ((tmptype (semantic-analyze-token-type tmp))
	    (slots nil))
	
	;; Get the children
	(setq slots (semantic-nonterminal-children tmptype))

	;; find (car s) in the list o slots
	(setq tmp (semantic-find-nonterminal-by-name (car s)
						     slots nil nil))
	
	(if (and (listp tmp) (semantic-token-p (car tmp)))
	    ;; We should be smarter...  For example
	    ;; search for an item only of 'variable if we know
	    ;; the syntax is variable, or only 'function if we
	    ;; can see a function.  Most languages don't allow that
	    ;; type of duality, so we will probably be safe with this
	    ;; forever.
	    (setq tmp (car tmp)))

	;; Make sure we have a token.
	(if (not (semantic-token-p tmp))
	    (if (cdr s)
		;; In the middle, we need to keep seeking our types out.
		(error "Cannot find definition for \"%s\"" (car s))
	      ;; Else, it's ok to end with a non-token
	      (setq tmp (car s))))

	(setq tok (cons tmp tok))
	(setq toktype (cons tmptype toktype))
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
	(extmeth (semantic-nonterminal-external-member-children type)))
    ;; Flatten the database output.
    (append slots extmeth)
    ))

(defun semantic-analyze-scoped-nonterminals (typelist)
  "Return a list of nonterminals accessable when TYPELIST is in scope.
Tokens returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\"."
  (apply #'append (mapcar #'semantic-analyze-type-parts typelist))
  ;; We also need to get all parents of typelist, and include public or
  ;; protected methods of those!
  )

(defun semantic-analyze-scoped-types (&optional position)
  "Return a list of types current in scope at POSITION.
This is based on what tokens exist at POSITION, and any associated
types available."
  (save-excursion
    (if position (goto-char position))
    (let ((tok (semantic-current-nonterminal))
	  (code-scoped-parents nil)
	  (parent nil))
      (setq parent
	    ;; This only makes sense in a function
	    (when (and tok (eq (semantic-token-token tok) 'function))
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
		  ;; position?
		  (setq p (semantic-current-nonterminal-parent))
		  p))
	      ;; Lets ask if any types are currently scoped.  Scoped
	      ;; classes and types provide their public methods and types
	      ;; in source code, but are unrelated hierarchically.
	      (let ((sp (semantic-ctxt-scoped-types)))
		(while sp
		  ;; Get this thing as a non terminal
		  (setq code-scoped-parents
			(cons
			 (semantic-analyze-find-nonterminal (car sp))
			 code-scoped-parents))
		  (setq  sp (cdr sp))))
	      (setq code-scoped-parents (nreverse code-scoped-parents))
	      ))
      ;; We return a list in case a function can have multiple explicit
      ;; parents.
      (if parent
	  (cons parent code-scoped-parents)
	code-scoped-parents))))

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

;;;###autoload
(defun semantic-analyze-current-context (position)
  "Analyze the current context at POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'."
  (interactive "d")
  (save-excursion
    (goto-char position)
    (let* ((context-return nil)
	   (startpoint (point))
	   (prefix (semantic-ctxt-current-symbol))
	   (endsym (car (reverse prefix)))
	   (bounds (save-excursion
		     (cond ((and prefix (looking-at endsym))
			    (cons (point) (progn
					    (forward-sexp 1)
					    (point))))
			   (prefix
			    (condition-case nil
				(cons (progn (forward-sexp -1) (point))
				      (progn (forward-sexp 1) (point)))
			      (error nil)))
			   (t nil))))
	   (prefixtypes nil)
	   (scopetypes (semantic-analyze-scoped-types position))
	   (scope (if scopetypes
		      (semantic-analyze-scoped-nonterminals scopetypes)))
	   (localvar (semantic-get-local-variables))
	   (function (semantic-ctxt-current-function))
	   (fntok nil)
	   arg fntokend argtok
	   )

      (condition-case nil
	  ;; If we are on lame stuff, it won't be found!
	  (setq prefix (semantic-analyze-find-nonterminal-sequence
			prefix localvar scope 'prefixtypes))
	(error nil))

      (when function
	;; If we have a function, then we can get the argument
	(setq arg (semantic-ctxt-current-argument))

	(condition-case nil
	    (setq fntok
		  (semantic-analyze-find-nonterminal-sequence
		   function localvar scope))
	  (error nil))

	(when fntok
	  (setq fntokend (car (reverse fntok))
		argtok (nth (1- arg) (semantic-token-function-args fntokend)))
	  ))

      (if fntok
	  ;; If we found a token for our function, we can go into
	  ;; functional context analysis mode, meaning we have a type
	  ;; for the argument.
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
		   :bounds bounds
		   :prefixtypes prefixtypes))

	;; No function, try assignment
	(let ((assign (semantic-ctxt-current-assignment))
	      (asstok nil))
	  (if assign
	      ;; We have an assignment
	      (setq asstok (semantic-analyze-find-nonterminal-sequence
			    assign localvar scope)))
	  (if asstok
	      (setq context-return
		    (semantic-analyze-context-assignment
		     "assignment"
		     :assignee asstok
		     :scope scope
		     :scopetypes scopetypes
		     :localvariables localvar
		     :bounds bounds
		     :prefix prefix
		     :prefixtypes prefixtypes))
	  
	    ;; TODO: Identify return value condition.

	    ;; Nothing in particular
	    (setq context-return
		  (semantic-analyze-context
		   "context"
		   :scope scope
		   :scopetypes scopetypes
		   :localvariables localvar
		   :bounds bounds
		   :prefix prefix
		   :prefixtypes prefixtypes)))))

      ;; Check for interactivity
      (if (interactive-p)
	  (semantic-analyze-pop-to-context context-return))
      ;; Return our context.
      context-return)))


;;; Context Analysis Completion
;;
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
	  ((semantic-token-p desired-type)
	   ;; We have a token of some sort.  Yay!
	   nil)
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

(defun semantic-analyze-type-constants (type)
  "For the token TYPE, return any constant symbols of TYPE.
Used as options when completing."
  (let* ((s (semantic-fetch-overload 'analyze-type-constants)))
    (if s
	;; We need the real type so that language files don't
	;; need to know much about analysis.
	(let* ((realtype (semantic-analyze-find-nonterminal
			  (semantic-token-name type)))
	       (ans (funcall s realtype))
	       (out nil))
	  (while ans
	    (cond ((stringp (car ans))
		   (setq out (cons (list (car ans)
					 'variable
					 (semantic-token-name type))
				   out)))
		  ((semantic-token-p (car ans))
		   (setq out (cons (car ans) out)))
		  (t nil))
	    (setq ans (cdr ans)))
	  (nreverse out))
      ;; Be default, we don't know.
      nil)))

(defun semantic-analyze-possible-completions (context)
  "Return a list of semantic tokens which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
  (interactive "d")
  (let* ((a (if (semantic-analyze-context-child-p context)
		context
	      (semantic-analyze-current-context context)))
	 (fnargs (semantic-get-local-arguments
		  (car (oref a bounds))))
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
		 (semantic-nonterminal-children completetexttype)
		 nil nil))
	      
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
		 ;; The current scope
		 (semantic-find-nonterminal-by-name-regexp
		  expr (oref a scope)
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

    ;; Some types, like the enum in C, have special constant values that
    ;; we could complete with.  Thus, if the target is an enum, we can
    ;; find possible symbol values to fill in that value.
    (if desired-type
	(let ((constants
	       (semantic-analyze-type-constants desired-type)))
	  (if constants
	      (progn
		;; Filter
		(setq constants
		      (semantic-find-nonterminal-by-name-regexp
		       (concat "^" completetext)
		       constants nil nil))
		;; Add to the list
		(setq c (append c constants)))
	    )))

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
    (princ "Bounds: ")
    (princ (oref context bounds))
    (princ "\n")
    (semantic-analyze-show context)
    )
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Semantic Context Analysis*"))
  )

(provide 'semantic-analyze)

;;; semantic-analyze.el ends here
