;;; eieio-custom.el -- eieio object customization

;;; Copyright (C) 1999, 2000 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-custom.el,v 1.12 2000/10/03 03:56:10 zappo Exp $
;; Keywords: OO, lisp
;;                                                                          
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;           
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;
;;   This contains support customization of eieio objects.  Enabling your
;; object to be customizable requires use of the added class slot
;; attirbute :custom
;;

(require 'eieio)
(require 'widget)
(require 'wid-edit)
(require 'custom)

;;; Code:
(defclass eieio-widget-test-class nil
  ((a-string :initarg :a-string
	     :initform "The moose is loose"
	     :custom string
	     :label "Amorphous String"
	     :group (default foo)
	     :documentation "A string for testing custom.
This is the next line of documentation.")
   (listostuff :initarg :listostuff
	       :initform ("1" "2" "3")
	       :type list
	       :custom (repeat (string :tag "Stuff"))
	       :label "List of Strings"
	       :group foo
	       :documentation "A list of stuff.")
   (uninitialized :initarg :uninitialized
		  :type string
		  :custom string
		  :documentation "This slot is not initialized.
Used to make sure that custom doesn't barf when it encounters one
of these.")
   (a-number :initarg :a-number
	     :initform 2
	     :custom integer
	     :documentation "A number of thingies."))
  "A class for testing the widget on.")

(defcustom eieio-widget-test (eieio-widget-test-class "Foo")
  "Test variable for editing an object."
  :type 'object)

(defface eieio-custom-slot-tag-face '((((class color)
					(background dark))
				       (:foreground "light blue"))
				      (((class color)
					(background light))
				       (:foreground "blue"))
				      (t (:italic t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)

(defvar eieio-wo nil
  "Buffer local variable in object customize buffers for the current widget.")
(defvar eieio-co nil
  "Buffer local variable in object customize buffers for the current obj.")

(define-widget 'object-edit 'group
  "Abstractly modify a CLOS object."
  :tag "Object"
  :format "%v"
  :convert-widget 'widget-types-convert-widget
  :value-create 'eieio-object-value-create
  :value-get 'eieio-object-value-get
  :value-delete 'widget-children-value-delete
  :validate 'widget-children-validate
  :match 'eieio-object-match
  :clone-object-children nil
  )

(defun eieio-object-match (widget value)
  "Match info for WIDGET against VALUE."
  ;; Write me
  t)

(defun eieio-filter-slot-type (widget slottype)
  "Filter WIDGETs SLOTTYPE."
  (if (widget-get widget :clone-object-children)
      slottype
    (cond ((eq slottype 'object)
	   'object-edit)
	  ((and (listp slottype)
		(eq (car slottype) 'object))
	   (cons 'object-edit (cdr slottype)))
	  ((equal slottype '(repeat object))
	   '(repeat object-edit))
	  ((and (listp slottype)
		(equal (car slottype) 'repeat)
		(listp (car (cdr slottype)))
		(equal (car (car (cdr slottype))) 'object))
	   (list 'repeat
		 (cons 'object-edit
		       (cdr (car (cdr slottype))))))
	  (t slottype))))

(defun eieio-object-value-create (widget)
  "Create the value of WIDGET."
  (if (not (widget-get widget :value))
      (widget-put widget
		  :value (funcall (class-constructor
				   (widget-get widget :objecttype))
				  "Custom-new")))
  (let* ((chil nil)
	 (obj (widget-get widget :value))
	 (master-group (or (widget-get widget :eieio-group) 'default))
	 (cv (class-v (object-class-fast obj)))
	 (fields (aref cv class-public-a))
	 (flabel (aref cv class-public-custom-label))
	 (fgroup (aref cv class-public-custom-group))
	 (fdoc (aref cv class-public-doc))
	 (fcust (aref cv class-public-custom)))
    ;; First line describes the object, but may not editable.
    (if (widget-get widget :eieio-show-name)
	(setq chil (cons (widget-create-child-and-convert
			  widget 'string :tag "Object "
			  :sample-face 'bold
			  (object-name-string obj))
			 chil)))
    ;; Display information about the group being shown
    (if master-group
	(widget-insert "Configuring Group " (symbol-name master-group) "\n\n"))
    ;; Loop over all the fields, creating child widgets.
    (while fields
      ;; Output this slot if it has a customize flag associated with it.
      (when (and (car fcust) (member master-group (car fgroup))
		 (slot-boundp obj (car fields)))
	;; In this case, this field has a custom type.  Create it's
	;; children widgets.
	(setq chil (cons (widget-create-child-and-convert
			  widget
			  (eieio-filter-slot-type widget (car fcust))
			  :tag
			  (concat
			   (make-string
			    (or (widget-get widget :indent) 0)
			    ? )
			   ;;"Slot "
			   (if (car flabel)
			       (car flabel)
			     (let ((s (symbol-name
				       (or
					(class-slot-initarg
					 (object-class-fast obj)
					 (car fields))
					(car fields)))))
			       (capitalize
				(if (string-match "^:" s)
				    (substring s (match-end 0))
				  s)))))
			  :value (slot-value obj (car fields))
			  :sample-face 'eieio-custom-slot-tag-face
			  )
			 chil))
	(setq chil (cons (widget-create-child-and-convert
			  widget 'documentation-string
			  :format "%t   %v"
			  :tag (make-string
				(or (widget-get widget :indent) 0)
				? )
			  :value (if (car fdoc) (car fdoc)
				   "Slot not Documented."))
			 chil))
	)
      (setq fields (cdr fields)
	    fdoc (cdr fdoc)
	    fcust (cdr fcust)
	    flabel (cdr flabel)
	    fgroup (cdr fgroup)))
    (widget-put widget :children (nreverse chil))
    ))

(defun eieio-object-value-get (widget)
  "Get the value of WIDGET."
  (let* ((obj (widget-get widget :value))
	 (wids (widget-get widget :children))
	 (name (car (widget-apply (car wids) :value-inline)))
	 (chil (nthcdr 1 wids))
	 (cv (class-v (object-class-fast obj)))
	 (fields (aref cv class-public-a))
	 (fcust (aref cv class-public-custom)))
    ;; If there are any prefix widgets, clear them.
    ;; -- None yet
    ;; Create a batch of initargs for each slot.
    (while (and fields chil)
      (if (car fcust)
	  (progn
	    ;; Only customized fields have widgets
	    (eieio-oset obj (car fields)
			 (car (widget-apply (car chil) :value-inline)))
	    ;; Two widets per field.  The slot value, and the doc.
	    (setq chil (cdr (cdr chil)))))
      (setq fields (cdr fields)
	    fcust (cdr fcust)))
    ;; Set any name updates on it.
    (aset obj object-name name)
    ;; This is the same object we had before.
    obj))

(defmethod eieio-done-customizing ((obj eieio-default-superclass))
  "When a applying change to a widget, call this method.
This method is called by the default widget-edit commands.  User made
commands should also call this method when applying changes.
Argument OBJ is the object that has been customized."
  nil)

(defun customize-object (obj &optional group)
  "Customize OBJ in a custom buffer.
Optional argument GROUP is the sub-group of slots to display."
  (eieio-customize-object obj group))

(defmethod eieio-customize-object ((obj eieio-default-superclass)
				   &optional group)
  "Customize OBJ in a specialized custom buffer.
To override call the `eieio-custom-widget-insert' to just insert the
object widget.
Optional argument GROUP specifies a subgroup of slots to edit as a symbol.
These groups are specified with the `:group' slot flag."
  ;; Insert check for multiple edits here.
  (let ((b (switch-to-buffer (get-buffer-create
			      (concat "*CUSTOMIZE " (object-name obj) "*")))))
    (toggle-read-only -1)
    (kill-all-local-variables)
    (erase-buffer)
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
    ;; Add an apply reset option at the top of the buffer.
    (eieio-custom-object-apply-reset obj)
    (widget-insert "\n\n")
    (widget-insert "Edit object " (object-name obj) "\n\n")
    ;; Create the widget editing the object.
    (make-local-variable 'eieio-wo)
    (setq eieio-wo (eieio-custom-widget-insert obj :eieio-group group))
    ;;Now generate the apply buttons
    (widget-insert "\n")
    (eieio-custom-object-apply-reset obj)
    ;; Now initialize the buffer
    (use-local-map widget-keymap)
    (widget-setup)
    ;(widget-minor-mode)
    (goto-char (point-min))
    (widget-forward 3)
    (make-local-variable 'eieio-co)
    (setq eieio-co obj)))

(defmethod eieio-custom-object-apply-reset ((obj eieio-default-superclass))
  "Insert an Apply and Reset button into the object editor.
Argument OBJ os the object being customized."
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   ;; I think the act of getting it sets
			   ;; it's value through the get function.
			   (message "Applying Changes...")
			   (widget-apply eieio-wo :value-get)
			   (eieio-done-customizing eieio-co)
			   (message "Applying Changes...Done."))
		 "Apply")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (message "Resetting.")
			   (eieio-customize-object eieio-co))
		 "Reset"))

(defmethod eieio-custom-widget-insert ((obj eieio-default-superclass)
				       &rest flags)
  "Insert the widget used for editing object OBJ in the current buffer.
Arguments FLAGS are widget compatible flags.
Must return the created widget."
  (apply 'widget-create 'object-edit :value obj flags))

(define-widget 'object 'object-edit
  "Instance of a CLOS class."
  :format "%{%t%}:\n%v"
  :value-to-internal 'eieio-object-value-to-abstract
  :value-to-external 'eieio-object-abstract-to-value
  :clone-object-children t
  )

(defun eieio-object-value-to-abstract (widget value)
  "For WIDGET, convert VALUE to an abstract /safe/ representation."
  (clone value))

(defun eieio-object-abstract-to-value (widget value)
  "For WIDGET, convert VALUE to an abstract /safe/ representation."
  value)


;;; Easymenu filter subsections
;;
;; These functions provide the ability to create dynamic menus to
;; customize specific sections of an object.  The do not hook directly
;; into a filter, but can be used to create easymenu vectors.
(defmethod eieio-customize-object-group ((obj eieio-default-superclass))
  "Create a list of vectors for customizing sections of OBJ."
  (mapcar (lambda (group)
	    (vector (concat "Group " (symbol-name group))
		    (list 'customize-object obj (list 'quote group))
		    t))
	  (class-option (object-class-fast obj) :custom-groups)))

(provide 'eieio-custom)

;;; eieio-custom.el ends here
;; 