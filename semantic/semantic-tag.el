;;; semantic-tag.el --- tag creation and access

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; X-CVS: $Id: semantic-tag.el,v 1.1 2003/03/19 16:10:23 ponced Exp $

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
;; I.  The core production of semantic is the list of tags produced by the
;;    different parsers.  This file provides 3 APIs related to tag access:
;;
;;    1) Primitive Tag Access
;;       There is a set of common features to all tags.  These access
;;       functions can get these values.
;;    2) Standard Tag Access
;;       A Standard Tag should be produced by most traditional languages
;;       with standard styles common to typed object oriented languages.
;;       These functions can access these data elements from a tag.
;;    3) Generic Tag Access
;;       Not implemented yet.
;;       Access to tag structure in a more direct way.  May not be forward
;;       compatible.
;;
;; II.  There is also an API for tag creation.  Use `semantic-tag' to create
;;     a new tag.
;;
;; III.  Tag Comparison.  Allows explicit or comparitive tests to see
;;      if two tags are the same.

;;; History:
;; 

;;; Code:
;;

;; Keep this only so long as we have obsolete fcns.
(require 'semantic-fw)

(defconst semantic-tag-version semantic-version
  "Version string of semantic tags made with this code.")

(defconst semantic-tag-incompatible-version "1.0"
  "Version string of semantic tags which are not currently compatible.
These old style tags may be loaded from a file with semantic db.
In this case, we must flush the old tags and start over.")

;;; Primitive Tag access system:
;;
;; Raw tags in semantic are lists of 5 elements:
;;
;;   (NAME CLASS ATTRIBUTES PROPERTIES OVERLAY)
;;
;; Where:
;;
;;   - NAME is a string that represents the tag name.
;;
;;   - CLASS is a symbol that represent the class of the tag (for
;;     example, usual classes are `type', `function', `variable',
;;     `include', `package', `code').
;;
;;   - ATTRIBUTES is a public list of attributes that describes
;;     language data represented by the tag (for example, a variable
;;     can have a `const' attribute, a function an `arguments'
;;     attribute, etc.).
;;
;;   - PROPERTIES is a private list of properties used internally.
;;
;;   - OVERLAY represent the location of data described by the tag.
;;

(defsubst semantic-tag-name (tag)
  "Return the name of TAG."
  (car tag))

(defsubst semantic-tag-class (tag)
  "Return the class of TAG.
That is, the symbol 'variable, 'function, 'type, or other."
  (nth 1 tag))

(defsubst semantic-tag-attributes (tag)
  "Return the list of public attributes of TAG.
That is a property list: (ATTRIBUTE-1 VALUE-1 ATTRIBUTE-2 VALUE-2...)."
  (nth 2 tag))

(defsubst semantic-tag-properties (tag)
  "Return the list of private properties of TAG.
That is a property list: (PROPERTY-1 VALUE-1 PROPERTY-2 VALUE-2...)."
  (nth 3 tag))

(defsubst semantic-tag-overlay (tag)
  "Return the OVERLAY part of TAG.
That is, an overlay or an unloaded buffer representation."
  (nth 4 tag))

(defsubst semantic--tag-overlay-cdr (tag)
  "Return the cons cell whose car is the OVERLAY part of TAG.
That function is for internal use only."
  (nthcdr 4 tag))

(defsubst semantic-tag-set-overlay (tag overlay)
  "Set the overlay part of TAG with OVERLAY."
  (setcar (semantic--tag-overlay-cdr tag) overlay))

(defsubst semantic-tag-start (tag)
  "Return the start location of TAG."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-start o)
      (aref o 0))))

(defsubst semantic-tag-end (tag)
  "Return the end location of TAG."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-end o)
      (aref o 1))))

(defsubst semantic-tag-extent (tag)
  "Return the extent (START END) of TAG."
  (list (semantic-tag-start tag)
        (semantic-tag-end tag)))

(defsubst semantic-tag-buffer (tag)
  "Return the buffer TAG resides in."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-buffer o)
      ;; We have no buffer for this tag (It's not in Emacs right now.)
      nil)))

(defsubst semantic--tag-attributes-cdr (tag)
  "Return the cons cell whose car is the ATTRIBUTES part of TAG.
That function is for internal use only."
  (nthcdr 2 tag))

(defsubst semantic-tag-put-attribute (tag attribute value)
  "Change value in TAG of ATTRIBUTE to VALUE.
If ATTRIBUTE already exists, its value is set to VALUE, otherwise the
new ATTRIBUTE VALUE pair is added.
Return TAG.
Use this function in a parser when not all attributes are known at the
same time."
  (let* ((plist-cdr (semantic--tag-attributes-cdr tag))
	 (plist (plist-put (car plist-cdr) attribute value)))
    (setcar plist-cdr plist)
    tag))

(defun semantic-tag-put-attribute-no-side-effect (tag attribute value)
  "Change value in TAG of ATTRIBUTE to VALUE without side effects.
All cons cells in the attribute list are replicated so that there
are no side effects if TAG is in shared lists.
If ATTRIBUTE already exists, its value is set to VALUE, otherwise the
new ATTRIBUTE VALUE pair is added.
Return TAG."
  (let* ((plist-cdr (semantic--tag-attributes-cdr tag))
	 (plist (copy-sequence (car plist-cdr))))
    (setcar plist-cdr (plist-put plist attribute value))
    tag))

(defsubst semantic-tag-get-attribute (tag attribute)
  "From TAG, return the value of ATTRIBUTE.
ATTRIBUTE is a symbol whose specification value to get.
Return the value found, or nil if ATTRIBUTE is not one of the
attributes of TAG."
  (plist-get key (semantic-tag-attributes tag)))

;; These functions are for internal use only!
(defsubst semantic--tag-properties-cdr (tag)
  "Return the cons cell whose car is the PROPERTIES part of TAG.
That function is for internal use only."
  (nthcdr 3 tag))

(defun semantic--tag-put-property (tag property value)
  "Change value in TAG of PROPERTY to VALUE.
If PROPERTY already exists, its value is set to VALUE, otherwise the
new PROPERTY VALUE pair is added.
Return TAG.
That function is for internal use only."
  (let* ((plist-cdr (semantic--tag-properties-cdr tag))
	 (plist (car plist-cdr)))
    (setcar plist-cdr (plist-put plist property value))
    tag))

(defun semantic--tag-put-property-no-side-effect (tag property value)
  "Change value in TAG of PROPERTY to VALUE without side effects.
All cons cells in the property list are replicated so that there
are no side effects if TAG is in shared lists.
If PROPERTY already exists, its value is set to VALUE, otherwise the
new PROPERTY VALUE pair is added.
Return TAG.
That function is for internal use only."
  (let* ((plist-cdr (semantic--tag-properties-cdr tag))
	 (plist (copy-sequence (car plist-cdr))))
    (setcar plist-cdr (plist-put plist property value))
    tag))

(defsubst semantic--tag-get-property (tag property)
  "From TAG, extract the value of PROPERTY.
Return the value found, or nil if PROPERTY is not one of the
properties of TAG.
That function is for internal use only."
  (plist-get property (semantic-tag-properties tag)))

;;; Tag tests and comparisons.
;;
(defsubst semantic-tag-p (tag)
  "Return non-nil if TAG is most likely a semantic tag."
  (and (consp tag)
       (stringp (car tag))                ; NAME
       (symbolp (nth 1 tag)) (nth 1 tag)  ; TAG-CLASS
       (listp (nth 2 tag))                ; ATTRIBUTES
       (listp (nth 3 tag))                ; PROPERTIES
       ))

(defun semantic-tag-with-position-p (tag)
  "Return non-nil if TAG has positional information."
  (and (semantic-tag-p tag)
       (let ((o (semantic-tag-overlay tag)))
	 (or (semantic-overlay-p o)
             (arrayp o)))))

(defun semantic-equivalent-tag-p (tag1 tag2)
  "Compare TAG1 and TAG2 and return non-nil if they are equivalent.
Use `eq' to test of two tags are the same.  Use this function if tags
are being copied and regrouped to test for if two tags represent the
same thing, but may be constructed of different cons cells."
  (and (string= (semantic-tag-name tag1) (semantic-tag-name tag2))
       (eq (semantic-tag-class tag1) (semantic-tag-class tag2))
       (eq (semantic-tag-start tag1) (semantic-tag-start tag2))
       (eq (semantic-tag-end tag1) (semantic-tag-end tag2))))

;;; Tag creation
;;

;; Is this function still necessary?
(defun semantic-tag-make-plist (args)
  "Create a property list with ARGS.
Args is a property list of the form (KEY1 VALUE1 ... KEYN VALUEN).
Where KEY is a symbol, and VALUE is the value for that symbol.
The return value will be a new property list, with these KEY/VALUE
pairs eliminated:

  - KEY associated to nil VALUE.
  - KEY associated to an empty string VALUE.
  - KEY associated to a zero VALUE."
  (let (ret key val)
    (while args
      (setq key  (car args)
            val  (nth 1 args)
            args (nthcdr 2 args))
      (and val (not (member val '("" 0)))
           (setq ret (cons key (cons val ret)))))
    ;; It is not useful to reverse the new plist.
    ret))

(defsubst semantic-tag (name class &rest attributes)
  "Create a generic semantic tag.
NAME is a string representing the name of this tag.
CLASS is the symbol that represents the class of tag this is,
such as 'variable, or 'function.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (list name class (semantic-tag-make-plist attributes)))

(defmacro semantic-tag-new-variable (name type default-value &rest attributes)
  "Create a semantic tag of class variable.
NAME is the name of this variable.
TYPE is a string or semantic tag representing the type of this variable.
DEFAULT-VALUE is a string representing the default value of this variable.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'variable
                 :type ,type
                 :default-value ,default-value
                 ,@attributes))

(defmacro semantic-tag-new-function (name type arg-list &rest attributes)
  "Create a semantic tag of class function.
NAME is the name of this function.
TYPE is a string or semantic tag representing the type of this function.
ARG-LIST is a list of strings or semantic tags representing the
arguments of this function.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'function
                 :type ,type
                 :arguments ,arg-list
                 ,@attributes))

(defmacro semantic-tag-new-type (name type part-list parents &rest attributes)
  "Create a semantic tag of class type.
NAME is the name of this type.
TYPE is a string or semantic tag representing the type of this type.
PART-LIST is a list of strings or semantic tags representing the
elements that make up this type if it is a composite type.
PARENTS is a cons cell.  (EXPLICIT-PARENTS . INTERFACE-PARENTS)
EXPLICIT-PARENTS can be a single string (Just one parent) or a
list of parents (in a multiple inheritance situation).  It can also
be nil.
INTERFACE-PARENTS is a list of strings representing the names of
all INTERFACES, or abstract classes inherited from.  It can also be
nil.
This slot can be interesting because the form:
     ( nil \"string\")
is a valid parent where there is no explicit parent, and only an
interface.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'type
                 :type ,type
                 :children ,part-list
                 :superclasses ,(car parents)
                 :interfaces ,(cdr parents)
                 ,@attributes))

(defmacro semantic-tag-new-include (name system-flag &rest attributes)
  "Create a semantic tag of class include.
NAME is the name of this include.
SYSTEM-FLAG represents that we were able to identify this include as belonging
to the system, as opposed to belonging to the local project.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'include
                 :system-flag ,system-flag
                 ,@attributes))

(defmacro semantic-tag-new-package (name detail &rest attributes)
  "Create a semantic tag of class package.
NAME is the name of this package.
DETAIL is extra information about this package, such as a location where
it can be found.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'package
                 :detail ,detail
                 ,@attributes))

(defmacro semantic-tag-new-code (name detail &rest attributes)
  "Create a semantic tag of class code.
NAME is a name for this code.
DETAIL is extra information about the code.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  `(semantic-tag ,name 'code
                 :detail ,detail
                 ,@attributes))

;;; Tag Cloning.
;;
(defun semantic-clone-tag (tag)
  "Clone TAG, creating a new TAG.
The actual values in TAG such as string names are not copied,
but shared with the original.
Use `semantic-tag-put-no-side-effect' to add properties to the clone.
The clone has a 'clone property whose value is t."
  ;; Right now, TAG is a list.
  (let ((new (copy-sequence tag)))
    (semantic--tag-put-property-no-side-effect new 'clone t)
    new))

;;; Standard Tag Access
;;

;;; Common
;;
(defsubst semantic-tag-type (tag)
  "Return the value of the `:type' attribute of TAG."
  (semantic-tag-get-attribute tag :type))

(defsubst semantic-tag-modifiers (tag)
  "Return the value of the 'modifiers attribute of TAG."
  (semantic-tag-get-attribute tag 'typemodifiers))

(defun semantic-tag-docstring (tag &optional buffer)
  "Return the documentation of TAG.
That is the value defined by the `:documentation' attribute.
Optional argument BUFFER indicates where to get the text from.
If not provided, then only the POSITION can be provided."
  (let ((p (semantic-tag-attribute tag :documentation)))
    (if (and p buffer)
        (with-current-buffer buffer
          (semantic-flex-text (car (semantic-lex p (1+ p)))))
      p)))

;;; Tags of class `type'
;;
(defsubst semantic-tag-type-parts (tag)
  "Return the parts of the type that TAG describes.
That is the value of the `:children' attribute."
  (semantic-tag-attribute tag :children))

(defun semantic-tag-type-superclasses (tag)
  "Return the list of superclasses of the type that TAG describes."
  (let ((supers (semantic-tag-attribute tag :superclasses)))
    (cond ((stringp supers)
	   (list supers))
	  ((listp supers)
	   supers))))

(defsubst semantic-tag-type-interfaces (tag)
  "Return the list of interfaces of the type that TAG describes."
  (semantic-tag-attribute tag :interfaces))

(defsubst semantic-token-type-parent (tag)
  "Return the parent of the type that TAG describes.
The return value is a list.  A value of nil means no parents.
The `car' of the list is either the parent class, or a list
of parent classes.  The `cdr' of the list is the list of
interfaces, or abstract classes which are parents of TAG."
  (cons (semantic-tag-attribute tag :superclasses)
        (semantic-tag-type-interfaces tag)))
(make-obsolete 'semantic-token-type-parent)

;;; Tags of class `function'
;;
(defsubst semantic-tag-function-arguments (tag)
  "Return the arguments of the function that TAG describes.
That is the value of the `:arguments' attribute."
  (semantic-tag-attribute tag :arguments))

(defsubst semantic-tag-function-throws (tag)
  "Return the exceptions the function that TAG describes can throw.
That is the value of the `throws' attribute."
  (semantic-tag-attribute tag 'throws))

(defsubst semantic-tag-function-parent (tag)
  "Return the parent of the function that TAG describes.
That is the value of the `parent' attribute.
A function has a parent if it is a method of a class, and if the
function does not appear in body of it's parent class."
  (semantic-tag-attribute tag 'parent))

(defmacro semantic-tag-function-destructor-p (tag)
  "Return non-nil if TAG describes a destructor function.
That is the value of the `destructor' attribute."
  (semantic-tag-attribute tag 'destructor))

;;; Tags of class `variable'
;;
(defsubst semantic-tag-variable-default (tag)
  "Return the default value of the variable that TAG describes.
That is the value of the attribute `:default-value'."
  (semantic-tag-attribute tag :default-value))

(defsubst semantic-tag-variable-constant-p (tag)
  "Return non-nil if the variable that TAG describes is a constant.
That is the value of the attribute `const'."
  (semantic-tag-attribute tag 'const))

(defsubst semantic-tag-variable-optsuffix (tag)
  "Return the optional details of the variable that TAG describes.
That is the value of the attribute `suffix'.
Optional details tell if this variable has bit fields, or array
dimentions."
  (semantic-tag-attribute tag 'suffix))

;;; Tags of class `include'
;;
(defsubst semantic-tag-include-system-p (tag)
  "Return non-nil if the include that TAG describes is a system include.
That is the value of the attribute `:system-flag'."
  (semantic-tag-attribute tag :system-flag))

;;; Tags of class `code'
;;
(defsubst semantic-tag-code-detail (tag)
  "Return detail information from code that TAG describes.
That is the value of the attribute `:detail'."
  (semantic-tag-attribute tag :detail))

;;; Compatibility
;;
(defconst semantic-token-version
  semantic-tag-version)
(defconst semantic-token-incompatible-version
  semantic-tag-incompatible-version)

(semantic-alias-obsolete 'semantic-token-name
                         'semantic-tag-name)

(semantic-alias-obsolete 'semantic-token-token
                         'semantic-tag-class)

(semantic-alias-obsolete 'semantic-token-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-properties
                         'semantic-tag-properties)

(semantic-alias-obsolete 'semantic-token-properties-cdr
                         'semantic--tag-properties-cdr)

(semantic-alias-obsolete 'semantic-token-overlay
                         'semantic-tag-overlay)

(semantic-alias-obsolete 'semantic-token-overlay-cdr
                         'semantic--tag-overlay-cdr)

(semantic-alias-obsolete 'semantic-token-start
                         'semantic-tag-start)

(semantic-alias-obsolete 'semantic-token-end
                         'semantic-tag-end)

(semantic-alias-obsolete 'semantic-token-extent
                         'semantic-tag-extent)

(semantic-alias-obsolete 'semantic-token-buffer
                         'semantic-tag-buffer)

(semantic-alias-obsolete 'semantic-token-put
                         'semantic--tag-put-property)

(semantic-alias-obsolete 'semantic-token-put-no-side-effect
                         'semantic--tag-put-property-no-side-effect)

(semantic-alias-obsolete 'semantic-token-get
                         'semantic--tag-get-property)

(semantic-alias-obsolete 'semantic-token-add-extra-spec
                         'semantic-tag-put-attribute)

(semantic-alias-obsolete 'semantic-token-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-type
                         'semantic-tag-type)

(semantic-alias-obsolete 'semantic-token-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-docstring
                         'semantic-tag-docstring)

(semantic-alias-obsolete 'semantic-token-type-parts
                         'semantic-tag-type-parts)

(semantic-alias-obsolete 'semantic-token-type-parent-superclass
                         'semantic-tag-type-superclasses)

(semantic-alias-obsolete 'semantic-token-type-parent-implement
                         'semantic-tag-type-interfaces)

(semantic-alias-obsolete 'semantic-token-type-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-type-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-type-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-function-args
                         'semantic-tag-function-arguments)

(semantic-alias-obsolete 'semantic-token-function-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-function-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-function-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-function-throws
                         'semantic-tag-function-throws)

(semantic-alias-obsolete 'semantic-token-function-parent
                         'semantic-tag-function-parent)

(semantic-alias-obsolete 'semantic-token-function-destructor
                         'semantic-tag-function-destructor-p)

(semantic-alias-obsolete 'semantic-token-variable-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-variable-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-variable-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-variable-const
                         'semantic-tag-variable-constant-p)

(semantic-alias-obsolete 'semantic-token-variable-optsuffix
                         'semantic-tag-variable-optsuffix)

(semantic-alias-obsolete 'semantic-token-include-system
                         'semantic-tag-include-system-p)

(semantic-alias-obsolete 'semantic-token-p
                         'semantic-tag-p)

(semantic-alias-obsolete 'semantic-token-with-position-p
                         'semantic-tag-with-position-p)

(semantic-alias-obsolete 'semantic-tag-make-assoc-list
                         'semantic-tag-make-plist)

;; Lets test this out during this short transition.
(semantic-alias-obsolete 'semantic-token
                         'semantic-tag)

(semantic-alias-obsolete 'semantic-token-new-variable
                         'semantic-tag-new-variable)

(semantic-alias-obsolete 'semantic-token-new-function
                         'semantic-tag-new-function)

(semantic-alias-obsolete 'semantic-token-new-type
                         'semantic-tag-new-type)

(semantic-alias-obsolete 'semantic-token-new-include
                         'semantic-tag-new-include)

(semantic-alias-obsolete 'semantic-token-new-package
                         'semantic-tag-new-package)

(provide 'semantic-tag)

;;; semantic-tag.el ends here
