;;; eieio-custom.el -- eieio object customization

;;; Copyright (C) 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-custom.el,v 1.1 1999/01/21 14:40:52 zappo Exp $
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
	     :docstring "A string for testing custom.
This is the next line of documentation.")
   (a-number :initarg :a-number
	     :initform 2
	     :custom integer
	     :docstring "A number of thingies."))
  "A class for testing the widget on.")

(defcustom eieio-widget-test (eieio-widget-test-class "Foo")
  "Test variable for editing an object."
  :type 'object)

(define-widget 'object-edit 'group
  "Abstractly modify a CLOS object."
  :tag "Object"
  :format "%v"
  :value-create 'eieio-object-value-create
  :value-get 'eieio-object-value-get
  :value-delete 'widget-children-value-delete
  :validate 'widget-children-validate
  :match 'eieio-object-match
  )

(defun eieio-object-match (widget value)
  "Match info for WIDGET against VALUE."
  ;; Write me
  t)

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
			      widget (car fcust)
			      :tag
			      (concat "   Slot "
				      (symbol-name
				       (or (class-slot-initarg
					    (object-class-fast obj)
					    (car fields))
					   (car fields)))
				      "  ")
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
  (let* ((obj (clone (widget-get widget :value)))
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
    ;; Lastly, `clone' changes the name.  We need to set it back
    (aset obj object-name (aref (widget-get widget :value) object-name))
    ;; Return our slightly modified clone
    obj))

(define-widget 'object 'object-edit
  "Instance of a CLOS class."
  :format "%{%t%}:\n%v"
  :value-to-internal 'eieio-object-value-to-abstract
  :value-to-external 'eieio-object-abstract-to-value)

(defun eieio-object-value-to-abstract (widget value)
  "For WIDGET, convert VALUE to an abstract /safe/ representation."
  value
  )

(defun eieio-object-abstract-to-value (widget value)

(provide 'eieio-custom)

;;; eieio-custom.el ends here
