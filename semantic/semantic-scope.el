;;; semantic-scope.el --- Analyzer Scope Calculations

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-scope.el,v 1.1 2007/09/08 03:36:51 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Calculate information about the current scope.
;;
;; Manages the current scope as a structure that can be cached on a
;; per-file basis and recycled between different occurances of
;; analysis on different parts of a file.
;;
;; Pattern for Scope Calculation
;;
;; Step 1: Calculate DataTypes in Scope:
;;
;; a) What is in scope via using statements or local namespaces
;; b) Lineage of current context.  Some names drawn from step 1.
;;
;; Step 2: Convert type names into lists of concrete tags
;;
;; a) Convert each datatype into the real datatype tag
;; b) Convert namespaces into the list of contents of the namespace.
;; c) Merge all existing scopes together into one search list.
;; 
;; Step 3: Local variables
;;
;; a) Local variables are in the master search list.
;;

(require 'semanticdb)
(require 'semantic-analyze-fcn)
(require 'semantic-ctxt)


;;; Code:

(defclass semantic-scope-cache (semanticdb-abstract-cache)
  ((tag :initform nil
	:documentation
	"The tag this scope was calculated for.")
   (scopetypes :initform nil
	       :documentation
	       "The list of types currently in scope.
For C++, this would contain anonymous namespaces known, and
anything labled by a `using' statement.")
   (parents :initform nil
	    :documentation
	    "List of parents in scope w/in the body of this function.
Presumably, the members of these parent classes are available for access
based on private:, or public: style statements.")
   (scope :initform nil
	  :documentation
	  "Items in scope due to the scopetypes or parents.")
   (fullscope :initform nil
	      :documentation
	      "All the other stuff on one master list you can search.")
   (localvar :initform nil
	     :documentation
	     "The local variables, function arguments, etc.")
   )
  "Cache used for storage of the current scope by the Semantic Analyzer.
Saves scoping information between runs of the analyzer.")

;;; METHODS
;;
;; Methods for basic management of the structure in semanticdb.
;;
(defmethod semantic-reset ((obj semantic-scope-cache))
  "Reset OBJ back to it's empty settings."
  (oset obj tag nil)
  (oset obj scopetypes nil)
  (oset obj parents nil)
  (oset obj scope nil)
  (oset obj fullscope nil)
  (oset obj localvar nil)
  )

(defmethod semanticdb-synchronize ((cache semantic-scope-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  (semantic-reset cache))


(defmethod semanticdb-partial-synchronize ((cache semantic-scope-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; If there are any includes or datatypes changed, then clear.
  (if (or (semantic-find-tags-by-class 'include new-tags)
	  (semantic-find-tags-by-class 'type new-tags)
	  (semantic-find-tags-by-class 'using new-tags))
      (semantic-reset cache))
  )

(defun semantic-scope-cache-get (slot)
  "Get the current cached value of SLOT."
  (when semanticdb-current-table
    (let ((co (semanticdb-cache-get semanticdb-current-table
				    semantic-scope-cache)))
      (slot-value co slot))))

;; @todo - Delete the below fcn?
(defun semantic-scope-cache-set (slot value)
  "Set the value of SLOT to VALUE in the current analyzer cache."
  (when semanticdb-current-table
    (let ((co (semanticdb-cache-get semanticdb-current-table
				    semantic-scope-cache)))
      (eieio-oset co slot value))))

;;; SCOPE UTILITIES
;;
;; Functions that do the main scope calculations


(define-overload semantic-analyze-scoped-types (position)
  "Return a list of types currently in scope at POSITION.
This is based on what tags exist at POSITION, and any associated
types available.")

(defun semantic-analyze-scoped-types-default (position)
  "Return a list of types currently in scope at POSITION.
Use `semantic-ctxt-scoped-types' to find types."
  (save-excursion
    (goto-char position)
    (let ((tag (semantic-current-tag))
	  (code-scoped-types nil))
      ;; Lets ask if any types are currently scoped.  Scoped
      ;; classes and types provide their public methods and types
      ;; in source code, but are unrelated hierarchically.
      (let ((sp (semantic-ctxt-scoped-types)))
	(while sp
	  ;; Get this thing as a tag
	  (let ((tmp (cond
		      ((stringp (car sp))
		       (semanticdb-typecache-find (car sp)))
		       ;(semantic-analyze-find-tag (car sp) 'type))
		      ((semantic-tag-p (car sp))
		       (if (semantic-analyze-tag-prototype-p (car sp))
			   (semanticdb-typecache-find (semantic-tag-name (car sp)))
			   ;;(semantic-analyze-find-tag (semantic-tag-name (car sp)) 'type)
			 (car sp)))
		      (t nil))))
	    (when tmp
	      (setq code-scoped-types
		    (cons tmp code-scoped-types))))
	  (setq  sp (cdr sp))))
      (setq code-scoped-types (nreverse code-scoped-types))
      (semanticdb-typecache-merge-streams code-scoped-types nil)
      ;;(semantic-unique-tag-table code-scoped-types)
      )))

;;------------------------------------------------------------

(define-overload semantic-analyze-scope-nested-tags (position scopedtypes)
  "Return a list of types in order of nesting for the context of POSITION.
If POSITION is in a method with a named parent, find that parent, and
identify it's scope via overlay instead.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.")

(defun semantic-analyze-scope-nested-tags-default (position scopetypes)
  "Return a list of types in order of nesting for the context of POSITION.
If POSITION is in a method with a named parent, find that parent, and
identify it's scope via overlay instead.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.
This only finds ONE immediate parent by name.  All other parents returned
are from nesting data types."
  (save-excursion
    (if position (goto-char position))
    (let* ((stack (reverse (semantic-find-tag-by-overlay (point))))
	   (tag (car stack))
	   (pparent (car (cdr stack)))
	   )
      ;; Only do this level of analysis for functions.
      (when (eq (semantic-tag-class tag) 'function)
	(if (and pparent (eq (semantic-tag-class pparent) 'type))
	    ;; We have a parent in our stack, so analyze this stack
	    ;; We are done.
	    nil
	  ;; No parent, we need to seek one out.
	  (let ((p (semantic-tag-function-parent tag)))
	    (when p
	      ;; We have a parent, search for it.
	      (let* ((searchname (cond ((stringp p) p)
				       ((semantic-tag-p p)
					(semantic-tag-name p))
				       ((and (listp p) (stringp (car p)))
					(car p))))
		     (scope (apply 'append
				   (mapcar 'semantic-tag-type-members scopetypes)))
		     (ptag (or (semanticdb-typecache-find searchname)
			       ;; @todo - v Use something else here for scope
			       (semantic-analyze-find-tag searchname
							  'type))))
		(setq pparent ptag)))
	    ))
	;; If we have a pparent tag, lets go there
	;; an analyze that stack of tags.
	(when (and pparent (semantic-tag-with-position-p pparent))
	  (semantic-go-to-tag pparent)
	  (setq stack (reverse (semantic-find-tag-by-overlay (point))))
	  (let ((returnlist nil))
	    ;; Add things to STACK until we cease finding tags of class type.
	    (while (and stack (eq (semantic-tag-class (car stack)) 'type))
	      (setq returnlist (cons (car stack) returnlist)
		    stack (cdr stack)))
	    (reverse returnlist))
	  )))))

;;------------------------------------------------------------

(define-overload semantic-analyze-scoped-tags (typelist parentlist)
  "Return accessable tags when TYPELIST and PARENTLIST is in scope.
Tags returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\".")

(defun semantic-analyze-scoped-tags-default (typelist parentlist)
  "Return accessable tags when TYPELIST and PARENTLIST is in scope.
Tags returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\"."
  (let ((typelist2 nil)
	(currentscope nil))
    ;; Loop over typelist, and find and merge all namespaces matching
    ;; the names in typelist.
    (while typelist
      (if (string= (semantic-tag-type (car typelist)) "namespace")
	  ;; By using the typecache, our namespaces are pre-merged.
	  (setq typelist2 (cons (car typelist) typelist2))
	;; Not a namespace.  Leave it off...
	;; (setq typelist2 (cons (car typelist) typelist2))
	)
      (setq typelist (cdr typelist)))

    ;; Loop over the types (which should be sorted by postion
    ;; adding to the scopelist as we go, and using the scopelist
    ;; for additional searching!
    (while typelist2
      (setq currentscope (append
			  (semantic-analyze-type-parts (car typelist2)
						       currentscope)
			  currentscope))
      (setq typelist2 (cdr typelist2)))

    ;; Collect all the types (class, etc) that are in our heratage.
    ;; These are types that we can extract members from, not those
    ;; delclared in using statements, or the like.
    ;; Get the PARENTS including nesting scope for this location.
    (while parentlist
      (setq currentscope (append
			  (semantic-analyze-type-parts (car parentlist)
						       currentscope)
			  currentscope))
      (setq parentlist (cdr parentlist)))

    currentscope))

;;; ANALYZER
;;
;; Create the scope structure for use in the Analyzer.
;;
;;;###autoload
(defun semantic-calculate-scope (&optional point)
  "Calculate the scope at POINT.
If POINT is not provided, then use the current location of `point'.
The class returned from the scope calculation is `semantic-scope-cache'."
  (interactive)
  (if (not point) (setq point (point)))
  (save-excursion
    (goto-char point)
    (let* ((TAG  (semantic-current-tag))
	   (scopecache (semanticdb-cache-get semanticdb-current-table
					     semantic-scope-cache))
	   )
      (when (not (semantic-equivalent-tag-p TAG (oref scopecache tag)))
	(semantic-reset scopecache))
      (if (oref scopecache tag)
	  nil
	(let*
	    (
	     ;; Step 1:
	     (scopetypes (semantic-analyze-scoped-types point))
	     (parents (semantic-analyze-scope-nested-tags point scopetypes))
	     ;; Step 2:
	     (scope (if (or scopetypes parents)
			(semantic-analyze-scoped-tags scopetypes parents)))
	     (fullscope (append scopetypes scope parents))
	     ;; Step 3:
	     (localvar (semantic-get-all-local-variables))
	     )
	  (oset scopecache tag TAG)
	  (oset scopecache scopetypes scopetypes)
	  (oset scopecache parents parents)
	  (oset scopecache scope scope)
	  (oset scopecache fullscope fullscope)
	  (oset scopecache localvar localvar)
	  ))
      (when (interactive-p)
	(semantic-adebug-show scopecache)
	)
      scopecache)))

(defun semantic-scope-find (name &optional class scope-in)
  "Find the tag with NAME, and optinal CLASS in the current SCOPE-IN.
Searches various elements of the scope for NAME.  Return ALL the
hits in order, with the first tag being in the closest scope."
  (let* ((scope (or scope-in (semantic-calculate-scope)))
	 (lv
	  ;; This should be first, but bugs in the
	  ;; C parser will turn function calls into
	  ;; assumed int return function prototypes.  Yuck!
	  (semantic-find-tags-by-name name  (oref scope localvar)))
	 (sc
	  (semantic-find-tags-by-name name (oref scope fullscope)))
	 )
    (if class
	;; Scan out things not of the right class.
	(semantic-find-tags-by-class class (append lv sc))
      (append lv sc))
    ))

;;; DUMP
;;
(defmethod semantic-analyze-show ((context semantic-scope-cache))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context scopetypes) "-> ScopeTypes: " )
  (semantic-analyze-princ-sequence (oref context parents) "-> Parents: " )
  (semantic-analyze-princ-sequence (oref context scope) "-> Scope: " )
  ;;(semantic-analyze-princ-sequence (oref context fullscope) "Fullscope:  " )
  (semantic-analyze-princ-sequence (oref context localvar) "-> Local Vars: " )
  )

(provide 'semantic-scope)
;;; semantic-scope.el ends here
