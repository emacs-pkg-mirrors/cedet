;;; semantic-cb.el --- Manage and maintain a Class Browser database

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-cb.el,v 1.4 2002/03/23 03:07:37 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-sb is free software; you can redistribute it and/or modify
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
;; Files in an OO language don't always properly represent the structure
;; of classes available.  This Class Browser analyzer can create
;; and maintain a database where types and tokens are linked together
;; so that the program structure can be programatically navigated
;; or displayed.
;;
;; By having the classes created inherit from `eieio-speedbar-*',
;; speedbar support for full class browsing is garnered for
;; nearly no effort.

(require 'semantic)
(require 'eieio-speedbar)
(require 'eieio-base)

;;; Code:
(defclass semantic-cb-project ()
  ((types :initarg :types
	  :type list
	  :documentation
	  "List of top level types in this project.")
   )
  "The root of a project's tag system.
The project will consist of top-level types, classes, namespaces,
or whatever is used in that language as a representaton.")

(defclass semantic-cb-token (eieio-named eieio-speedbar)
  ((buttontype :initform statictag)
   (buttonface :initform speedbar-tag-face)
   (token :initarg :token
	  :type semantic-token
	  :documentation
	  "Semantic token which represents a type.")
   (table :initarg :table
	  :type semanticdb-table
	  :documentation
	  "This is the database table that `token' was found in.
Be sure to use this field when loading a token's file into memory.")
   (container :initarg :container
	      :type (or null semantic-cb-token)
	      :documentation
	      "This is the CB object containing this token.
CB Containers are usually types with attributes of methods.")
   )
  "A single semantic token.
Tokens represented in the Class Browser database may not be loaded
in memory, so this forms the structure needed to access them.")

(defclass semantic-cb-type (semantic-cb-token)
  ((buttontype :initform expandtag)
   (buttonface :initform speedbar-tag-face)
   (parents :type list
	     :initform nil
	     :documentation
	     "The full parents this type was derived from.
These are also `semantic-cb-type' objects.
This excludes Java interfaces.")
   (subclasses :type list
	       :initform nil
	       :documentation
	       "A list of types which inherit from this object.
These are also `semantic-cb-type' objects.")
   (children :type list
	     :initform nil
	     :documentation
	     "List of CB token children, both embedded and external.
Embedded children are defined within the scope of this types declaration.
External children are defined within some other scope, and are labeled
as children of this type.
Children are of type `semantic-cb-token'.")
   )
  "One type object for a given project.
Because some connections are derived, or take a while to find,
details which are derivable will be cached in the fields
of a type object.
In addition, type objects will contain the actual tokens created by
semantic, external methods and such will be cached in this object, not
in the semantic token itself.")

(defvar semantic-cb-incomplete-types nil
  "During construction, the list of types that need work.
Types are created without CB objects for parent or interfaces.
We need to go back later and fill them in from this list.")

(defvar semantic-cb-incomplete-scoped-types nil
  "During construction, the list of contained types that need work.
Types are created without CB objects for parent or interfaces.
We need to go back later and fill them in from this list.")

(defun semantic-cb-add-incomplete-type (cbtoken)
  "Add CBTOKEN to the list of incomplete types."
  (add-to-list (if (oref cbtoken :container)
		   'semantic-cb-incomplete-scoped-types
		 'semantic-cb-incomplete-types)
	       cbtoken))

(defvar semantic-cb-current-project nil
  "The current project's class structure.")

(defun semantic-cb-new-class-browser ()
  "Create an object representing this project's organization.
The object returned is of type `semantic-cb-project', which contains
the slot `:types', a list of all top-level types.  Each element is a
class of type `semantic-cb-token', or `semantic-cb-type'."
  (let ((alldbtype (semanticdb-find-nonterminal-by-token
		    'type
		    nil			;current project
		    nil			;only top level types
		    nil			;no include files outside this project
		    nil			;same mode
		    nil))		;do not load unloaded files.
	;; Scope these variables during construction.
	(semantic-cb-incomplete-types nil)
	(semantic-cb-incomplete-scoped-types nil)
	)
    ;; Loop over all discovered types, and construct them.
    (while alldbtype
      (let ((alltype (cdr (car alldbtype)))
	    (db (car (car alldbtype))))
	(while alltype
	  ;; This adds all new types into a special list.
	  (semantic-cb-convert-type (car alltype) db nil)
	  (setq alltype (cdr alltype))))
      (setq alldbtype (cdr alldbtype)))
    ;; Cycle over all incomplete subtypes, finding parents
    ;; The incomplete type lists are in two batches.  The first we
    ;; will deal with are for unscoped classes at the top level.
    ;; The second is for types defined in other types, so that
    ;; they are scoped locally.  This excludes them from our search
    ;; lists.
    (let ((typesearchlist semantic-cb-incomplete-types)
	  (list1 semantic-cb-incomplete-types)
	  (list2 semantic-cb-incomplete-scoped-types)
	  (top-level nil))
      (while list1
	(semantic-cb-complete-type (car list1) typesearchlist)
	;; If there is no parent from our project, then
	;; it must be a top-level object.
	(when (not (oref (car list1) parents))
	  (setq top-level (cons (car list1) top-level)))
	(setq list1 (cdr list1)))
      (while list2
	(semantic-cb-complete-type (car list2) typesearchlist)
	(setq list2 (cdr list2)))
      ;; Create the new cache.
      (setq semantic-cb-current-project
	    (semantic-cb-project
	     "Project"
	     :types (nreverse top-level)
	     )))
    ;; Return it
    semantic-cb-current-project))

(defun semantic-cb-complete-type (cbtoken possibleparents)
  "Complete CBTOKEN, an object which needs to be completed.
POSSIBLEPARENTS is the list of types which are eligible
to be parents of CBTOKEN."
  (let* ((pl (semantic-token-type-parent (oref cbtoken token)))
	 (parents (car pl))
	 (interface (cdr pl))
	 )
    (if (semantic-token-p pl)
	(setq parents (list pl)
	      interface nil))
    (if (or (not (listp parents))
	    (semantic-token-p parents))
	(setq parents (list parents)))
    (while parents
      (semantic-cb-find-parent cbtoken (car parents) possibleparents)
      (setq parents (cdr parents)))
    (if (or (not (listp interface))
	    (semantic-token-p interface))
	(setq interface (list interface)))
    (while interface
      (semantic-cb-find-parent cbtoken (car interface) possibleparents)
      (setq interface (cdr interface)))
    ))

(defun semantic-cb-find-parent (cbt parentobj possibleparents)
  "For CBT, find the CB object represented by PARENTOBJECT.
PARENTOBJ will be in POSSIBLEPARENTS, or determined to be nil.
If a valid CB object is found, link CBT to the found object."
  (let* ((pstr (cond ((stringp parentobj)
		      parentobj)
		     ((semantic-token-p parentobj)
		      (semantic-token-name parentobj))
		     (t (error "Unknown parent object type"))))
	 (po (object-assoc pstr :object-name possibleparents))
	 )
    (when po
      ;; List parent as the
      (object-add-to-list cbt 'parents po t)
      ;; List cbt as inheriting from parent
      (object-add-to-list po 'subclasses cbt t)
      )))

(defun semantic-cb-convert-type (token db parentobj)
  "Convert the semantic TOKEN to a type object.
DB is the semantic database that TOKEN is derived from.
PARENTOBJ is the CB object which is the parent of TOKEN"
  (let* ((chil (cons
		;; This makes a mock DB list
		(cons db (semantic-token-type-parts token))
		;; External children in DB form.
		(semantic-nonterminal-external-member-children-db
		 token t)))
	 ;; This is a created CB object which will represent
	 ;; this type.
	 (tobj (semantic-cb-type
		(semantic-token-name token) ; name
		 :token token		; The token
		 :table db		; database table we came from
		 :container parentobj	; parent container
		 )))
    ;; We now have a token in TOBJ.  Conver the child list
    ;; we just got into a form suitable for a tobj child list.
    (setq chil (semantic-cb-convert-children chil tobj))
    (oset tobj children chil)
    ;; Add ourselves to the list of incomplete types.
    (semantic-cb-add-incomplete-type tobj)
    ;; Return it.
    tobj))

(defun semantic-cb-convert-children (childlist parentobj)
  "Convert CHILDLIST from semantic format to cb objects.
CHILDLIST is in semanticdb search format, such that each
element of the list starts with a database table object.
PARENTOBJ is the CB token which hosts CHILDLIST."
  (let ((newlist nil))
    (while childlist
      (let ((sublist (cdr (car childlist)))
	    (db (car (car childlist))))
	(while sublist
	  (if (semantic-token-with-position-p (car sublist))
	      (let ((newtok
		     (cond
		      ((eq (semantic-token-token (car sublist))
			   'type)
		       (semantic-cb-convert-type (car sublist) db parentobj))
		      (t
		       (semantic-cb-token
			(semantic-token-name (car sublist))
			:token (car sublist)
			:table db
			:container parentobj)))))
		;; We have a new object representing the token.
		;; add it to the new list.
		(setq newlist (cons newtok newlist))))
	  (setq sublist (cdr sublist))))
      (setq childlist (cdr childlist)))
    (nreverse newlist)))

;;; Methods
;;
(defmethod initialize-instance :AFTER ((this semantic-cb-token) &rest fields)
  "After initializing THIS, keep overlay properties up to date."
  (let* ((tok (oref this token))
	 (ol (semantic-token-overlay tok)))
    ;; Ignore tokens that are in the database.
    (when (semantic-overlay-p ol)
      ;; Apply our object onto this overlay for fast
      ;; reference.
      (semantic-overlay-put ol 'semantic-cb this))))


;;; Speedbar specific methods
;;
(defmethod eieio-speedbar-object-children ((object semantic-cb-type))
  "Return children for OBJECT."
  (oref object subclasses))

;(defmethod eieio-speedbar-object-children ((object semantic-cb-token))
;  "Return children of this type."
;  (oref object children))

(defmethod eieio-speedbar-description ((object semantic-cb-token))
  "Descriptive text for OBJECT."
  (semantic-summarize-nonterminal (oref object token)))

(defmethod eieio-speedbar-derive-line-path ((object semantic-cb-token))
  "Get the path to OBJECT's instantiation."
  (oref (oref object table) file))

(defmethod eieio-speedbar-handle-click ((object semantic-cb-token))
  "When clicking on a token OBJECT, jump to its definition."
  (let ((token (oref object token)))
    (speedbar-find-file-in-frame
     (semanticdb-full-filename (oref object table)))
    (save-excursion (speedbar-stealthy-updates))
    (semantic-find-nonterminal token) ;; only positioned objects here.
    (speedbar-maybee-jump-to-attached-frame)
    (run-hooks 'speedbar-visiting-tag-hook)))
	


;;; Speedbar initialization
;;
(defvar semantic-cb-speedbar-key-map
  eieio-speedbar-key-map
  "Extra keybindings used when speedbar displays our class tree.")

(defun semantic-cb-make-map ()
  "Create a keymap and return it."
  nil)

(defvar semantic-cb-speedbar-menu
  eieio-speedbar-menu
  "Menu additions used in speedbar when displaying a callback hierarchy.")

(defun semantic-cb-speedbar-buttons (dir)
  "Return the list of object children to display at the toplevel in speedbar.
Argument DIR is the directory speedbar is asking about."
  (speedbar-with-attached-buffer (semantic-cb-new-class-browser))
  (oref semantic-cb-current-project types))

;;;###autoload
(defun semantic-cb-speedbar-mode ()
  "Bring speedbar up, and put it into Class Browser mode.
This will use the Class Browser logic applied to the current Semantic
project database to build the available relations.  The structure of
the class hierarchy can then be navigated using traditional speedbar
interactions."
  (interactive)
  (speedbar-frame-mode 1)
  (speedbar-change-initial-expansion-list "Class Browser")
  (speedbar-get-focus))

(eieio-speedbar-create 'semantic-cb-make-map
		       'semantic-cb-speedbar-key-map
		       'semantic-cb-speedbar-menu
		       "Class Browser"
		       'semantic-cb-speedbar-buttons)

(provide 'semantic-cb)

;;; semantic-cb.el ends here
