;;; eieio-tests.el -- eieio tests routines

;;;
;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-tests.el,v 1.24 2002/06/27 02:59:19 zappo Exp $
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

;;; Commentary:
;;  
;; Test the various features of EIEIO.  To run the tests, evaluate the
;; entire buffer.

(require 'eieio-base)

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
	      :allocation :class)
   (test-tag :initform nil
	     :documentation "Used to make sure methods are called.")
   (self :initform nil
	 :type (or null class-a)
	 :documentation "Test self referencing types.")
   )
  "Class A")

(condition-case msg

    (progn
      (defclass class-alloc-initarg ()
	((throwwarning :initarg :throwwarning
		       :allocation :class))
	"Throw a warning mixing allocation class and an initarg.")

      (if (not (string-match "Class allocated slots do not need :initarg"
			     (current-message)))
	  (error ":initarg and :allocation warning not thrown!"))
      )
  (error (error msg)))
  

(defclass class-b ()
  ((land :initform "Sc"
	 :type string
	 :documentation "Detail about land."))
  "Class b")

(defclass class-ab (class-a class-b)
  ((amphibian :initform "frog"
	      :documentation "Detail about amphibian on land and water."))
  "Class A and B combined.")


;;; Defining a class with a slot tag error
;;
(let ((eieio-error-unsupported-class-tags t))
  (condition-case nil
      (progn
	(defclass class-error ()
	  ((error-slot :initarg :error-slot
		       :badslottag 1))
	  "A class with a bad slot tag.")
	(error "No error was thrown for badslottag"))
    (invalid-slot-type nil)))

(let ((eieio-error-unsupported-class-tags nil))
  (condition-case nil
      (progn
	(defclass class-error ()
	  ((error-slot :initarg :error-slot
		       :badslottag 1))
	  "A class with a bad slot tag."))
    (invalid-slot-type
     (error "invalid-slot-type thrown when eieio-error-unsupported-class-tags is nil")
     )))

;;; Abstract base classes
;;
(defclass abstract-class ()
  ((some-slot :initarg :some-slot
	      :initform nil
	      :documentation "A slot."))
  :documentation "An abstract claptionass."
  :abstract t)

(if (condition-case nil
	(progn
	  (abstract-class "Test")
	  t)
      (error nil))
    (error "Instantiation of an abstract class allowed."))

;;; Class with a static method
;;
(defclass static-method-class ()
  ((some-slot :initform nil
	      :allocation :class
	      :documentation "A slot."))
  :documentation "A class used for testing static methods."
  :abstract t)

(defmethod static-method-class-method :STATIC ((c static-method-class))
  "Test static methods.
Argument C is the class bound to this static method."
  (oset-default c some-slot t))

(condition-case nil
    (static-method-class-method static-method-class)
  (error (error "Failed to call static method.")))
(if (not (eq (oref static-method-class some-slot) t))
    (error "Call to static method did not run."))


;;; Perform method testing
;;

;; allocate an object to use
(defvar ab nil)
(setq ab (class-ab "abby"))
(defvar a nil)
(setq a (class-a "aye"))
(defvar b nil)
(setq b (class-b "fooby"))

(condition-case nil
    (progn
      ;; Try make-instance on these guys...
      (make-instance 'class-ab)
      (make-instance 'class-a :water 'cho)
      (make-instance 'class-b "a name")
      )
  (error "make-instance error."))

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

;; Try the self referencing test
(oset a self a)
(oset ab self ab)


;;; Test the BEFORE, PRIMARY, and AFTER method tags.
;;
(defvar class-fun-tag-state nil)

(defmethod class-fun-tag :PRIMARY ((a class-a))
  "Tagging fun primary A."
  (message ":PRIMARY method")
  (unless (eq class-fun-tag-state 'before-generic)
    (error "BEFORE generic not called before PRIMARY method (%s)"
	   class-fun-tag-state))
  (setq class-fun-tag-state 'primary-method)
  (call-next-method))

(defmethod class-fun-tag :BEFORE ((a class-a))
  "Tagging fun before A."
  (message ":BEFORE method")
  (unless (eq class-fun-tag-state nil)
    (error "BEFORE method not called first (%s)" 
	   class-fun-tag-state))
  (setq class-fun-tag-state 'before-method)
  (call-next-method))

(defmethod class-fun-tag :AFTER ((a class-a))
  "Tagging fun after A."
  (message ":AFTER method")
  (unless (eq class-fun-tag-state 'primary-generic)
    (error "PRIMARY generic not called before AFTER method (%s)"
	   class-fun-tag-state))
  (setq class-fun-tag-state 'after-method)
  (call-next-method))

(defmethod class-fun-tag :PRIMARY (a)
  "Generic untyped primary for A."
  (message ":PRIMARY generic")
  (unless (eq class-fun-tag-state 'primary-method)
    (error "PRIMARY generic not called after PRIMARY method (%s)" 
	   class-fun-tag-state))
  (setq class-fun-tag-state 'primary-generic))

(defmethod class-fun-tag :BEFORE (a)
  "Generic untyped before for A."
  (message ":BEFORE generic")
  (unless (eq class-fun-tag-state 'before-method)
    (error "BEFORE generic not called after BEFORE method (%s)" 
	   class-fun-tag-state))
  (setq class-fun-tag-state 'before-generic))

(defmethod class-fun-tag :AFTER (a)
  "Generic untyped after for A."
  (message ":AFTER generic")
  (unless (eq class-fun-tag-state 'after-method)
    (error "AFTER generic not called after AFTER method (%s)" 
	   class-fun-tag-state))
  (setq class-fun-tag-state 'after-generic))

(let ((class-fun-tag-state nil))
  (condition-case er
      (progn
	(class-fun-tag a)
	(unless (eq class-fun-tag-state 'after-generic)
	  (error "AFTER generic not called last. (%s)" 
		 class-fun-tag-state))
	)
    (error 
     (if (eq (car er) 'error)
	 (error (car (cdr er)))
       (error "%S" er)))))


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

(condition-case nil
    (progn
      (oref a water)
      (error ""))
  (unbound-slot nil)
  (error (error "Oref of unbound slot succeeded.")))
  

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

;; After setting 'water to 'moose, make sure a new object has
;; the right stuff.
(oset-default (object-class a) water 'penguin)
(if (eq (oref (class-a "foo") water) 'penguin)
    nil
  (error "oset-default, new instance value failed."))

(defmethod slot-unbound ((a class-a) &rest foo)
  "If a slot in A is unbound, ignore FOO."
  ;; Disable the old slot-unbound so we can run this test
  ;; more than once
  (call-next-method))

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
(defvar aa nil)
(setq aa (class-a "another"))

(let ((newval 'moose))
  (oset aa classslot newval)
  (if (and (eq (oref a classslot) newval)
	   (eq (oref aa classslot) newval))
      nil
    (error "Class slots are tracking between objects")))

(if (not (slot-boundp a 'classslot))
    (error "Class allocatd slot thought unbound when it is bound."))

(if (not (slot-boundp class-a 'classslot))
    (error "Class allocatd slot thought unbound when it is bound."))

(slot-makeunbound a 'classslot)

(if (slot-boundp a 'classslot)
    (error "Class allocatd slot thought bound when it is unbound."))

(if (slot-boundp class-a 'classslot)
    (error "Class allocatd slot thought bound when it is unbound."))


;;; Test function type in a class
;;
(defvar class-typep-var 0
  "A variable used in an initform.")

(setq class-typep-var 1)

(defclass class-typep ()
  ((slot1 :type function
	  :initform <
	  )
   (slot2 :type integer
	  :initform (lambda () class-typep-var)
	  )
   (slot4 :type function
	  :initform (lambda-default () 2)
	  )
   )
  "Test different types in a class.")

(setq class-typep-var 2)

(defvar ct nil)
(setq ct (class-typep "foo"))

(if (/= (oref ct slot2) 2)
    (error "Default value for slot2 incorrect.")) 


;;; Inheritance status
;;
(if (and
     (child-of-class-p class-ab class-a)
     (child-of-class-p class-ab class-b)
     (object-of-class-p a class-a)
     (object-of-class-p ab class-a)
     (object-of-class-p ab class-b)
     (object-of-class-p ab class-ab)
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
	   :label "Wild Animal"
	   :group borg
	   :protection :public)
   (slot-2 :initarg :penguin
	   :initform "penguin"
	   :type string
	   :allocation :instance
	   :documentation "Second slot testing slot arguments."
	   :custom string
	   :label "Wild bird"
	   :group vorlon
	   :accessor get-slot-2
	   :protection :private)
   )
  (:custom-groups (foo))
  "A class for testing slot arguments."
  )

(defvar t1 nil)
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

;; Slot protection
(defclass prot-1 ()
  ((slot-1 :initarg :slot-1
	   :initform nil
	   :protection :public)
   (slot-2 :initarg :slot-2
	   :initform nil
	   :protection :protected)
   (slot-3 :initarg :slot-3
	   :initform nil
	   :protection :private))
  "A class for testing the :protection option.")

(defclass prot-2 (prot-1)
  nil
  "A class for testing the :protection option.")

(defmethod prot1-slot-2 ((s2 prot-1))
  "Try to access slot-2 in S2."
  (oref s2 slot-2))

(defmethod prot1-slot-2 ((s2 prot-2))
  "Try to access slot-2 in S2."
  (oref s2 slot-2))

(defmethod prot1-slot-3-only ((s2 prot-1))
  "Try to access slot-3 in S2.
Do not override for `prot-2'."
  (oref s2 slot-3))

(defmethod prot1-slot-3 ((s2 prot-1))
  "Try to access slot-3 in S2."
  (oref s2 slot-3))

(defmethod prot1-slot-3 ((s2 prot-2))
  "Try to access slot-3 in S2."
  (oref s2 slot-3))

(defvar p1 nil)
(setq p1 (prot-1 ""))
(defvar p2 nil)
(setq p2 (prot-2 ""))

(condition-case nil
    (oref p1 slot-1)
  (error (error "Error accessing public slot.")))
(condition-case nil
    (oref p2 slot-1)
  (error (error "Error accessing public slot.")))

(condition-case nil
    (progn
      (oref p1 slot-2)
      (error "Accessing protected slot out of context succeeded."))
  (error nil))
(condition-case nil
    (prot1-slot-2 p1)
  (error (error "Error accessing protected slot in a method.")))
(condition-case nil
    (prot1-slot-2 p2)
  (error (error "Error accessing protected slot in a subclass method.")))

(condition-case nil
    (progn
      (oref p1 slot-3)
      (error "Accessing private slot out of context succeeded."))
  (error nil))
(condition-case nil
    (prot1-slot-3 p1)
  (error (error "Error accessing private slot in a method.")))
(condition-case nil
    (progn
      (prot1-slot-3 p2)
      (error "Accessing private slot in a subclass method succeeded."))
  (error nil))

(condition-case nil
    (prot1-slot-3-only p1)
  (error (error "Accessing private slot by same class failed.")))

(condition-case nil
    (prot1-slot-3-only p2)
  (error (error "Accessing private slot by subclass in sameclass method failed.")))

;;; eieio-instance-inheritor
;; Test to make sure this works.
(defclass II (eieio-instance-inheritor)
  ((slot1 :initform 1)
   (slot2)
   (slot3))
  "Instance Inheritor test class.")

(defvar II1 nil)
(setq II1 (II "II Test."))
(oset II1 slot2 'cat)
(defvar II2 nil)
(setq II2 (clone II1 "II2 Test."))
(oset II2 slot1 'moose)
(defvar II3 nil)
(setq II3 (clone II2 "II3 Test."))
(oset II3 slot3 'penguin)

(cond ((not (eq (oref II3 slot1) 'moose))
       (error "Instance inheritor: Level one inheritance failed."))
      ((not (eq (oref II3 slot2) 'cat))
       (error "Instance inheritor: Level two inheritance failed."))
      ((not (eq (oref II3 slot3) 'penguin))
       (error "Instance inheritor: Level zero inheritance failed."))
      (t t))


;;; Test the persistent object, and object-write by side-effect.
;;
(defclass PO (eieio-persistent)
  ((slot1 :initarg :slot1
	  :initform 2)
   (slot2 :initarg :slot2
	  :initform "foo"))
  "A Persistent object with two initializable slots.")

(defvar PO1 nil)
(setq PO1 (PO "persist" :slot1 4 :slot2 "testing"
	      :file (concat default-directory "test-p.el")))

(eieio-persistent-save PO1)

(eieio-persistent-read "test-p.el")


;;; Test the instance tracker
;;
(defclass IT (eieio-instance-tracker)
  ((tracking-symbol :initform IT-list)
   (slot1 :initform 'die))
  "Instance Tracker test object.")

(defvar IT-list nil)
(defvar IT1 nil)
(setq IT1 (IT "trackme"))

(if (not (eieio-instance-tracker-find 'die 'slot1 'IT-list))
    (error "Instance tracker lost an instance."))

(delete-instance IT1)

(if (eieio-instance-tracker-find 'die 'slot1 'IT-list)
    (error "Instance tracker delete failed."))


;;; Test singletons
;;
(defclass SINGLE (eieio-singleton)
  ((a-slot :initarg :a-slot :initform t))
  "A Singleton test object.")

(let ((obj1 (SINGLE "Moose"))
      (obj2 (SINGLE "Cow")))
  (if (not (eq obj1 obj2))
      (error "Two instances of a singleton")))


;;; Test the named object.
;;
(defclass NAMED (eieio-named)
  ((some-slot :initform nil)
   )
  "A class inheriting from eieio-named.")

(defvar N nil)
(setq N (NAMED "Foo"))

(if (not (string= "Foo" (oref N object-name)))
    (error "Named object `object-name' slot ref failed."))

(condition-case err
    (progn
      (oref N missing-slot)
      (error "Access to missing slot passed."))
  (invalid-slot-name nil))

(oset N object-name "NewName")

(if (not (string= "NewName" (oref N object-name)))
    (error "Named object `object-name' slot set/ref failed."))


;;; Test some utilities in EIEIO-OPT
;;
(defclass opt-test1 ()
  ()
  "Abstract base class"
  :abstract t)

(defclass opt-test2 (opt-test1)
  ()
  "Instantiable child")

(require 'eieio-opt)

(if (/= (length (eieio-build-class-alist opt-test1 nil)) 2)
    (error "eieio-build-class-alist did not return all possible classes"))

(if (/= (length (eieio-build-class-alist opt-test1 t)) 1)
    (error "eieio-build-class-alist did not filter on INSTANTIABLE-ONLY"))


;;;
(message "All tests passed.")

(provide 'eieio-tests)

;;; eieio-tests.el ends here
