;;; semantic-token.el --- token creation and access

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; X-CVS: $Id: semantic-token.el,v 1.2 2003/02/28 14:22:35 zappo Exp $

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
;; I. The core production of semantic is the list of tokens produced by the 
;;    different parsers.  This files provides 3 APIs related to token access:
;;
;;    1) Primitive Token Access
;;       There is a set of common features to all tokens.  These access
;;       functions can get these values.
;;    2) Standard Token Access
;;       A Standard Token should be produced by most traditional languages
;;       with standard styles common to typed object oriented langauges.
;;       These functions can access these data elements from a token.
;;    3) Generic Token Access
;;       Not implemented yet
;;       Access to token structure in a more direct way.  May no be forward
;;       compatible.
;;
;; II. There is also an API for token creation.  Use `semantic-token' to create
;;     a new token.
;;
;; III. Token Comparison.  Allows explicit or comparitive tests to see
;;      if two tokens are the same.


;;; Primitive Token access system:
;;
;; Raw tokens in semantic are lists.  Each token/list has a basic structure
;; for all tokens.  This includes the first two elements, and the last 3.
;; See `semantic-tfe-*' for details.
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

;;; Property Lists on tokens
;;
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


;;; Standard Token Access
;;
;; A standard token describes a token that conforms to one of the documented
;; structures that can describe a typed/oo language.  See the semantic manual
;; for details on what goes into structures of this type.
;;
;; Extensions to standard tokens are lumped into the "extra specifiers" catagory.

(defun semantic-token-type (token)
  "Retrieve the type of TOKEN."
  (if (member (semantic-token-token token)
	      '(function variable type))
      (nth 2 token)))

(defmacro semantic-token-type-parts (token)
  "Retrieve the parts of the type TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-type-parent (token)
  "Retrieve the parent of the type TOKEN.
The return value is a list.  A value of nil means no parents.
The `car' of the list is either the parent class, or a list
of parent classes.  The `cdr' of the list is the list of
interfaces, or abstract classes which are parents of TOKEN."
  `(nth 4 ,token))

(defun semantic-token-type-parent-superclass (token)
  "Retrieve a list of parent superclasses for the type token TOKEN."
  (let ((p (semantic-token-type-parent token)))
    (cond ((stringp (car p))
	   (list (car p)))
	  ((listp (car p))
	   (car p)))))

(defun semantic-token-type-parent-implement (token)
  "Retrieve a list of parent interfaces for the type token TOKEN."
  (cdr (semantic-token-type-parent token)))

(defmacro semantic-token-type-extra-specs (token)
  "Retrieve extra specifications for the type TOKEN."
  `(nth 5 ,token))

(defmacro semantic-token-type-extra-spec (token spec)
  "Retrieve an extra specification for the type TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-type-extra-specs ,token))))

(defmacro semantic-token-type-modifiers (token)
  "Retrieve modifiers for the type TOKEN."
  `(semantic-token-type-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-function-args (token)
  "Retrieve the arguments of the function TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-function-extra-specs (token)
  "Retrieve extra specifications for the function TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-function-extra-spec (token spec)
  "Retrieve an extra specification for the function TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-function-extra-specs ,token))))

(defmacro semantic-token-function-modifiers (token)
  "Retrieve modifiers for the function TOKEN."
  `(semantic-token-function-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-function-throws (token)
  "The symbol string that a function can throws.
Determines if it is available based on the length of TOKEN."
  `(semantic-token-function-extra-spec ,token 'throws))

(defmacro semantic-token-function-parent (token)
  "The parent of the function TOKEN.
A function has a parent if it is a method of a class, and if the
function does not appear in body of it's parent class."
  `(semantic-token-function-extra-spec ,token 'parent))

(defmacro semantic-token-function-destructor (token)
  "Non-nil if TOKEN is a destructor function."
  `(semantic-token-function-extra-spec ,token 'destructor))

(defmacro semantic-token-variable-default (token)
  "Retrieve the default value of the variable TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-variable-extra-specs (token)
  "Retrieve extra specifications for the variable TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-variable-extra-spec (token spec)
  "Retrieve an extra specification for the variable TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-variable-extra-specs ,token))))

(defmacro semantic-token-variable-modifiers (token)
  "Retrieve modifiers for the variable TOKEN."
  `(semantic-token-variable-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-variable-const (token)
  "Retrieve the status of constantness from the variable TOKEN."
  `(semantic-token-variable-extra-spec ,token 'const))

(defmacro semantic-token-variable-optsuffix (token)
  "Optional details if this variable has bit fields, or array dimentions.
Determines if it is available based on the length of TOKEN."
  `(semantic-token-variable-extra-spec ,token 'suffix))

(defmacro semantic-token-include-system (token)
 "Retrieve the flag indicating if the include TOKEN is a system include."
  `(nth 2 ,token))

(defun semantic-token-extra-spec (token spec)
  "Retrieve an extra specification for TOKEN.
SPEC is a symbol whose specification value to get.
This function can get extra specifications from any type of token.
Do not use the function if you know what type of token you are dereferencing.
Instead, use `semantic-token-variable-extra-spec',
`semantic-token-function-extra-spec', or  `semantic-token-type-extra-spec'."
  (let ((tt (semantic-token-token token)))
    (cond ((eq tt 'variable)
	   (semantic-token-variable-extra-spec token spec))
	  ((eq tt 'function)
	   (semantic-token-function-extra-spec token spec))
	  ((eq tt 'type)
	   (semantic-token-type-extra-spec token spec))
	  (t nil))))

(defun semantic-token-extra-specs (token)
  "Retrieve the extra specifications list for TOKEN.
This function can get extra specifications from any type of token.
Do not use the function if you know what type of token you are dereferencing.
Instead, use `semantic-token-variable-extra-specs',
`semantic-token-function-extra-specs', or  `semantic-token-type-extra-specs'."
  (let ((tt (semantic-token-token token)))
    (cond ((eq tt 'variable)
	   (semantic-token-variable-extra-specs token))
	  ((eq tt 'function)
	   (semantic-token-function-extra-specs token))
	  ((eq tt 'type)
	   (semantic-token-type-extra-specs token))
	  (t nil))))

(defun semantic-token-add-extra-spec (token spec value)
  "Add to TOKEN, and extra specifier SPEC with VALUE.
Use this function in a parser when not all specifiers are known
at the same time."
  (let ((tt (semantic-token-token token)))
    (cond ((eq tt 'variable)
	   (setcar (nthcdr 4 token)
		   (cons (cons spec value)
			 (semantic-token-variable-extra-specs token))))
	  ((eq tt 'function)
	   (setcar (nthcdr 4 token)
		   (cons (cons spec value)
			 (semantic-token-function-extra-specs token))))
	  ((eq tt 'type)
	   (setcar (nthcdr 5 token)
		   (cons (cons spec value)
			 (semantic-token-type-extra-specs token))))
	  (t nil))
    token))


(defmacro semantic-token-modifiers (token)
  "Retrieve modifiers for TOKEN.
If TOKEN is of an unknown type, then nil is returned."
  `(semantic-token-extra-spec ,token 'typemodifiers))


;;; Token Tests
;;
;; For tests and comparisons.
;;; 

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

(defun semantic-equivalent-tokens-p (token1 token2)
  "Compare TOKEN1 and TOKEN2 and return non-nil if they are equivalent.
Use `eq' to test of two tokens are the same.  Use this function if tokens
are being copied and regrouped to test for if two tokens represent the same
thing, but may be constructed of different cons cells."
  (and (string= (semantic-token-name token1) (semantic-token-name token2))
       (eq (semantic-token-token token1) (semantic-token-token token2))
       (eq (semantic-token-start token1) (semantic-token-start token2))
       (eq (semantic-token-end token1) (semantic-token-end token2))))


;;; Token Creation
;;
(defun semantic-token-make-assoc-list (args)
  "Create an association list with ARGS.
Args is a list of the form (KEY1 VALUE1 ... KEYN VALUEN).
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

(defun semantic-token (name type-symbol &rest plist)
  "Create generic semantic token.
NAME is a string representing the name of this token.
TYPE-SYMBOL is the symbol that represents the type of token this is,
such as 'variable, or 'function.
PLIST is a property list of additional values belonging to this token."
  (list name type-symbol
	(semantic-token-make-assoc-list plist)
	nil)
  )

(defun semantic-token-new-variable (name type default-value &rest extra-specifiers)
  "Create semantic token of type variable.
NAME is a string representing the name of this token.
TYPE is a string or semantic token representing the type of this token.
DEFAULT-VALUE is a string representing the default value of this variable.
EXTRA-SPECIFIERS is a property list of additional features of this token.
Any property with a value of nil is not stored in the list."
  (list name 'variable type default-value
	(semantic-token-make-assoc-list extra-specifiers)
	nil)
  )

(defun semantic-token-new-function (name type arg-list &rest extra-specifiers)
  "Create semantic token of type function.
NAME is a string representing the name of this token.
TYPE is a string or semantic token representing the type of this token.
ARG-LIST is a list of strings or a list of semantic tokens representing the
argument list of this function.
EXTRA-SPECIFIERS is a property list of additional features of this token.
Any property with a value of nil is not stored in the list."
  (list name 'function type arg-list
	(semantic-token-make-assoc-list extra-specifiers)
	nil)
  )

(defun semantic-token-new-type (name type part-list parents &rest extra-specifiers)
  "Create semantic token of type function.
NAME is a string representing the name of this token.
TYPE is a string or semantic token representing the type of this token.
PART-LIST is a list of strings, or a list of semantic tokens representing the
elements that make up this type if it is a composite type.

PARENTS is a cons cell.  (EXPLICIT-PARENTS . INTERFACE-PARENTS)
EXPLICIT-PARENTS can be a single string (Just one parent) or a
list of parents (in a multiple inheritance situation.  It can also
be nil.
INTERFACE-PARENTS is a list of strings representing the names of
all INTERFACES, or abstract classes inherited from.  It can also be
nil.
This slot can be interesting because the form:
     ( nil \"string\")
is a valid parent where there is no explicit parent, and only an
interface.

EXTRA-SPECIFIERS is a property list of additional features of this token.
Any property with a value of nil is not stored in the list."
  (list name 'type type part-list parents
	(semantic-token-make-assoc-list extra-specifiers)
	nil)
  )

(defun semantic-token-new-include (name system-flag &rest extra-specifiers)
  "Create semantic token of type function.
NAME is a string representing the name of this token.
SYSTEM-FLAG represents that we were able to identify this include as belonging
to the system, as opposed to belonging to the local project.
EXTRA-SPECIFIERS is a property list of additional features of this token.
Any property with a value of nil is not stored in the list."
  ;; Ignore extra specifiers for now.
  (list name 'include system-flag nil)
  )

(defun semantic-token-new-package (name detail &rest extra-specifiers)
  "Create semantic token of type function.
NAME is a string representing the name of this token.
DETAIL is extra information about this package, such as a location where
it can be found.
EXTRA-SPECIFIERS is a property list of additional features of this token.
Any property with a value of nil is not stored in the list."
  (list name 'package detail nil)
  )


(provide 'semantic-token)

;;; semantic-token.el ends here
