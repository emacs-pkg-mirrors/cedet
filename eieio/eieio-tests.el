;;; eieio-tests.el -- eieio tests routines

;;;
;; Copyright (C) 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-tests.el,v 1.2 1999/12/01 01:39:08 zappo Exp $
;; Keywords: oop, lisp, tools
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
;; Test the various features of EIEIO.  To run the tests, evaluate the
;; entire buffer.

;;; Code:

;;; Multiple Inheritance, and method signal testing
;;
(defclass class-a ()
  ((water :initarg :water
	  :initform h20
	  :type symbol
	  :documentation "Detail about water.")
   (classslot :initform penguin
	      :type symbol
	      :documentation "A class allocated slot."
	      :allocation class)
   (test-tag :initform nil
	     :documentation "Used to make sure methods are called.")
   )
  "Class A")

(defclass class-b ()
  ((land :initform "Sc"
	 :type string
	 :documentation "Detail about land."))
  "Class b")

(defclass class-ab (class-a class-b)
  ((amphibian :initform "frog"
	      :documentation "Detail about amphibian on land and water."))
  "Class A and B combined.")


;;; Perform method testing
;;

;; allocate an object to use
(setq ab (class-ab "abby")
      a  (class-a "aye")
      b (class-b "fooby"))

;; Play with call-next-method
(defmethod class-cn ((a class-a))
  "Try calling `call-next-method' when there isn't one.
Argument A is object of type symbol `class-a'."
  (call-next-method)
  )

(defmethod no-next-method ((a class-a))
  "Override signal throwing for variable `class-a'.
Argument A is the object of class variable `class-a'."
  'moose)

(if (eq (class-cn ab) 'moose)
    nil
  (error "no-next-method return value failure."))

;; Non-existing methods.
(defmethod no-applicable-method ((b class-b) method)
  "No need.
Argument B is for booger.
METHOD is the method that was attempting to be called."
  'moose)

(if (eq (class-cn b) 'moose)
    nil
  (error "no-applicable-method return value failure."))

;;; play with methods and mi
(defmethod class-fun ((a class-a))
  "Fun with class A."
  'moose)

(defmethod class-fun ((b class-b))
  "Fun with class B."
  (error "Class B fun should not be called")
  )

(if (eq (class-fun ab) 'moose)
    nil
  (error "Inheritance method check."))

(defmethod class-fun-foo ((b class-b))
  "Foo Fun with class B."
  'moose)

(if (eq (class-fun-foo ab) 'moose)
    nil
  (error "Multiple inheritance method check."))

;; Play with next-method and mi
(defmethod class-fun2 ((a class-a))
  "More fun with class A."
  'moose)

(defmethod class-fun2 ((b class-b))
  "More fun with class B."
  (error "Class B fun2 should not be called"))

(defmethod class-fun2 ((ab class-ab))
  "More fun with class AB."
  (call-next-method))

(if (eq (class-fun2 ab) 'moose)
    nil
  (error "Call next method inheritance check failed."))

;; How about if B is the only slot?
(defmethod class-fun3 ((b class-b))
  "Even More fun with class B."
  'moose)

(defmethod class-fun3 ((ab class-ab))
  "Even More fun with class AB."
  (call-next-method))

(if (eq (class-fun3 ab) 'moose)
    nil
  (error "Call next method MI check failed."))


;;; Test initialization methods
;;
(defmethod initialize-instance ((a class-a) &rest slots)
  "Initialize the slots of class-a."
  (call-next-method)
  (if (/= (oref a test-tag) 1)
      (error "shared-initialize test failed."))
  (oset a test-tag 2))

(defmethod shared-initialize ((a class-a) &rest slots)
  "Shared initialize method for class-a."
  (call-next-method)
  (oset a test-tag 1))

(let ((ca (class-a "class act")))
  (if (/=  (oref ca test-tag) 2)
      (error "initialize-instance test failed."))
  )


;;; Perform slot testing
;;
(if (and (oref ab water)
	 (oref ab land)
	 (oref ab amphibian))
    nil
  (error "Slot checks failed"))

(defmethod slot-missing ((ab class-ab) &rest foo)
  "If a slot in AB is unbound, return something cool.  FOO."
  'moose)

(if (eq (oref ab ooga-booga) 'moose)
    nil
  (error "Missing slot override failed."))

(condition-case nil
    (progn
      (oref a ooga-booga)
      (error "No invalid slot error thrown."))
  (invalid-slot-name nil))

(slot-makeunbound a 'water)

(if (slot-boundp a 'water)
    (error "Slot makeunbound failed slot-bound-p test"))

(if (and (slot-exists-p a 'water)
	 (not (slot-exists-p a 'moose)))
    nil
  (error "Slot exists-p failed"))

(defmethod slot-unbound ((a class-a) &rest foo)
  "If a slot in A is unbound, ignore FOO."
  'moose)

(if (eq (oref a water) 'moose)
    nil
  (error "Unbound slot reference failed."))

(oset a water 'moose)
(if (eq (oref a water) 'moose)
    nil
  (error "Oset of unbound failed."))

(if (not (eq (oref a water) (oref-default a water)))
    nil
  (error "oref/oref-default comparison failed."))

(oset-default (object-class a) water 'moose)
(if (eq (oref a water) (oref-default a water))
    nil
  (error "oset-default -> oref/oref-default comparison failed."))

;; Slot type checking
(condition-case nil
    (progn
      (oset ab water "a string, not a symbol")
      (error "Slot set to invalid type successfully."))
  (invalid-slot-type nil))

(condition-case nil
    (progn
      (oset ab classslot "a string, not a symbol")
      (error "Slot set to invalid type successfully."))
  (invalid-slot-type nil))

(condition-case nil
    (progn
      (class-a "broken-type-a" :water "a string not a symbol")
      (error "Slot set to invalid type at creation successfully."))
  (invalid-slot-type nil))

;; Test out class allocated slots
(setq aa (class-a "another"))
(oset aa classslot 'moose)
(if (eq (oref a classslot) (oref aa classslot))
    nil
  (error "Class slots are tracking between objects"))


;;; Inheritance status
;;
(if (and
     (child-of-class-p class-ab class-a)
     (child-of-class-p class-ab class-b)
     (obj-of-class-p a class-a)
     (obj-of-class-p ab class-a)
     (obj-of-class-p ab class-b)
     (obj-of-class-p ab class-ab)
     (eq (class-parents class-a) nil)
     (equal (class-parents class-ab) '(class-a class-b))
     (same-class-p a class-a)
     (class-a-p a)
     (not (class-a-p ab))
     (class-a-child-p a)
     (class-a-child-p ab)
     (not (class-a-p "foo"))
     (not (class-a-child-p "foo"))
     )
    nil
  (error "Inheritance tests: failed"))


;;; Slot parameter testing
;;
(defclass class-c ()
  ((slot-1 :initarg :moose
	   :initform moose
	   :type symbol
	   :allocation :instance
	   :documentation "Fisrt slot testing slot arguments."
	   :custom symbol
	   :protection public)
   (slot-2 :initarg :penguin
	   :initform "penguin"
	   :type string
	   :allocation :instance
	   :documentation "Second slot testing slot arguments."
	   :custom string
	   :accessor get-slot-2
	   :protection private)
   )
  '(:documentation "A class for testing slot arguments.")
  )

(setq t1 (class-c "C1"))
(if (not (and (eq (oref t1 slot-1) 'moose)
	      (eq (oref t1 :moose) 'moose)))
    (error "Initialization of slot failed."))

(condition-case nil
    (progn
      (oref t1 slot-2)
      (error "Reference of private slot passed."))
  (invalid-slot-name nil))

(if (not (string= (get-slot-2 t1) "penguin"))
    (error "Accessor to private slot returned bad value."))

(condition-case nil
    (progn
      (class-c "C2" :moose "not a symbol")
      (error "A string was set on a symbol slot during init."))
  (invalid-slot-type nil))



(message "All tests passed.")

(provide 'eieio-tests)

;;; eieio-tests.el ends here
