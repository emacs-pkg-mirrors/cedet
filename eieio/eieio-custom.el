;;; eieio-custom.el -- eieio object customization

;;; Copyright (C) 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-custom.el,v 1.4 1999/02/03 18:10:50 zappo Exp $
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
	     :documentation "A string for testing custom.
This is the next line of documentation.")
   (a-number :initarg :a-number
	     :initform 2
	     :custom integer
	     :documentation "A number of thingies."))
  "A class for testing the widget on.")

(defcustom eieio-widget-test (eieio-widget-test-class "Foo")
  "Test variable for editing an object."
  :type 'object)

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
	  ((equal slottype '(repeat object))
	   '(repeat object-edit))
	  (t slottype))))

(defun eieio-object-value-create (widget)
  "Create the value of WIDGET."
  (let* ((chil nil)
	 (obj (widget-get widget :value))
	 (cv (class-v (object-class-fast obj)))
	 (fields (aref cv class-public-a))
	 (fdoc (aref cv class-public-doc))
	 (fcust (aref cv class-public-custom)))
    ;; Loop over all the fields, creating child widgets.
    (while fields
      ;; Output this slot if it has a customize flag associated with it.
      (if (car fcust)
	  (progn
	    ;; In this case, this field has a custom type.  Create it's
	    ;; children widgets.
	    (setq chil (cons (widget-create-child-and-convert
			      widget
			      (eieio-filter-slot-type widget (car fcust))
			      :tag
			      (concat "   Slot `"
				      (symbol-name
				       (or (class-slot-initarg
					    (object-class-fast obj)
					    (car fields))
					   (car fields)))
				      "'")
			      :value (slot-value obj (car fields)))
			     chil))
	    (setq chil (cons (widget-create-child-and-convert
			      widget 'documentation-string
			      :format "    %v"
			      :tag ""
			      :value (if (car fdoc) (car fdoc)
				       "Slot not Documented."))
			     chil))
	    ))
      (setq fields (cdr fields)
	    fdoc (cdr fdoc)
	    fcust (cdr fcust)))
    (widget-put widget :children (nreverse chil))
    ))

(defun eieio-object-value-get (widget)
  "Get the value of WIDGET."
  (let* ((obj (widget-get widget :value))
	 (chil (widget-get widget :children))
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
	    (oset-engine obj (car fields)
			 (car (widget-apply (car chil) :value-inline)))
	    ;; Two widets per field.  The slot value, and the doc.
	    (setq chil (cdr (cdr chil)))))
      (setq fields (cdr fields)
	    fcust (cdr fcust)))
    ;; This is the same object we had before.
    obj))

(defmethod eieio-customize-object ((obj eieio-default-superclass))
  "Customize OBJ in a specialized custom buffer.
To override call the `eieio-custom-widget-insert' to just insert the
object widget."
  ;; Insert check for multiple edits here.
  (let ((b (switch-to-buffer (get-buffer-create
			      (concat "*CUSTOMIZE " (object-name obj) "*")))))
    (toggle-read-only -1)
    (erase-buffer)
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
    (widget-insert "Edit object " (object-name obj) "\n")
    ;; Create the widget editing the object.
    (make-local-variable 'eieio-wo)
    (setq eieio-wo (eieio-custom-widget-insert obj))
    ;;Now generate the apply buttons
    (widget-insert "\n")
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
		   "Reset")
    ;; Now initialize the buffer
    (use-local-map widget-keymap)
    (widget-setup)
    ;(widget-minor-mode)
    (make-local-variable 'eieio-co)
    (setq eieio-co obj)))

(defmethod eieio-custom-widget-insert ((obj eieio-default-superclass)
				       &rest flags)
  "Insert the widget used for editing object OBJ in the current buffer.
Arguments FLAGS are widget compatible flags.
Must return the created widget."
  (widget-create 'object-edit :value obj))

(defmethod eieio-done-customizing ((obj eieio-default-superclass))
  "When a applying change to a widget, call this method.
This method is called by the default widget-edit commands.  User made
commands should also call this method when applying changes.
Argument OBJ is the object that has been customized."
  nil)

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

(provide 'eieio-custom)

;;; eieio-custom.el ends here
