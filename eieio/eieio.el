;;; eieio.el --- Enhanced Implementation of Emacs Interpreted Objects
;;              or maybe Eric's Implementation of Emacs Intrepreted Objects

;;;
;; Copyright (C) 1995,1996, 1998, 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; Version: 0.10
;; RCS: $Id: eieio.el,v 1.33 1999/01/21 14:25:59 zappo Exp $
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
;; EIEIO is a series of Lisp routines which provide a class structure
;; methodology which implements a small subset of CLOS, the Common
;; Lisp Object System.  In addition, eieio also adds a few new
;; features which help it integrate more strongly with the Emacs
;; running environment.
;;
;; Classes can inherit (singly) from other classes, and attributes
;; can be multiply defined (but only one actual storage spot will be
;; allocated.) Attributes may be given initial values in the class
;; definition.  A method can be defined in CLOS style where the
;; parameters determine which implementation to use.
;;
;; Documentation for a class is generated from the class' doc string,
;; and also the doc strings of all its slots.  Documentation for a
;; method uses a default generic doc string, and the collection of
;; all specific method class strings.
;;

;;; Structural description of object vectors
;;
;; Class definitions shall be a stored vector.  Please see the constants
;; class-* for the vector index and documentation about that slot.
;;
;; The vector can be accessed by referencing the named property list
;; `eieio-class-definition', or by using the function `class-v' on the
;; class's symbol.  The symbol will reference itself for simplicity,
;; thus a class will always evaluate to itself.

;;; Upon defining a class, the following functions are created (Assume
;; 'moose is the class being created):
;; moose     - Create an object of type moose
;; moose-p   - t if object is type moose
;;
;; The instantiated object will have the following form:
;; [ 'object class-type name field1 field2 ... fieldn ]
;; Where 'object marks it as an eieio object.
;; Where class-type is the class definition vector
;; Where name is some string or symbol assigned to said object to uniquely
;;            identify it.
;; Where the field# are the public then private attributes.
;;
;; An object slot can be dereferenced with `oref' and set with
;; `oset'.  The CLOS function `slot-value' will also work, but since
;; `cl' may not always be defined, `setf' is only conditionally set
;; to work with `slot-value'
;;
;; Fields in a slot will default to values specified in a class.  Use
;; `oref-default' and `oset-default' to access these values.

;;; Generic functions and methods get a single defined symbol
;; representing the name of the method.  This method always calls the
;; same thing: eieio-generic-call  In order to fathom which method to
;; call, properties are attached to the method name of the form:
;; :KEY-classname where :KEY is :BEFORE :PRIMARY or :AFTER.
;; (:PRIMARY represents the middle, but is not needed when declaring
;; you method) `classname' represents the name of the class for which
;; this method is defined, or `generic' if it isn't defined.  In this
;; way, all implementations can be quickly found and run.

;;; History:
;;
;; 0.1 - 0.5  Comments removed to shorten file.  They referred to
;;             things that have been long since removed.
;;
;; 0.6  - Fixed up the defgeneric default call to handle arguments better.
;;        Added `call-next-method' (calls parent's method)
;;        Fixed `make-instance' so it's no longer a macro
;;        Fixed edebug hooks so they work better
;;        Fixed storage duplication for inherited classes, which also
;;           fixed default-value inheritance bug
;;        Added some error messages to help in debugging programs using eieio.
;;        Fixed class scoping troubles
;;        Added `eieio-thing-to-string' which behaves like (format "%S" ..)
;;           so objects and classes don't appear as symbols and vectors in
;;           your output.
;;        Added `eieio-describe-class' command which creates a buffer
;;           and displays the entire contents of a class or object.
;;        Turned field names into properties on the class to reduce
;;           the lookup times.  Old list is still there because it is
;;           needed for generating sub-classes, and for doing
;;           browsing things.
;; 0.7    Added :accessor as new tag creating a function which can
;;           access a given field.
;;        Added :docstring modifiers for generic function calls to
;;           allow browsing of all specific style methods.
;;        Changed what was once plist associations into a single obarray
;;           in the hopes of allowing faster searches.
;;        Changed plist storage of method definitions first into a single
;;           plist element, `eieio-method-tree', and
;;           `eieio-method-obarrays' a vector of 6 elements.  This
;;           vector contains 6 types of functions, specific :BEFORE,
;;           :PRIMARY and :AFTER elements, and then the :BEFORE,
;;           :PRIMARY and :AFTER generic calls.  Lastly turned lists
;;           of associations into OBARRAYs and symbols.
;; 0.8    Added ability to byte compile methods.  This is implemented
;;           for both XEmacs and GNU emacs.  This will only work with
;;           the modern byte-compiler for these systems.  Routines to
;;           do this are in eieio-comp.el.
;;        Removed all reference to classmethods as no one liked them,
;;           and were wasting space in here.
;;        Added `oset-default' to modify existing classes default values.
;;        `oref-default' can now take a class or object to retrieve
;;           the default value.
;;        Optimized several convenience functions as macros, and made some
;;           signals arise from more logical locations.
;;        Created and used signal symbols `no-method-definition' for
;;           method calls that do not resolve, and `invalid-slot-name'
;;           when the user tries to access an invalid slot name.
;;        Added `eieio-attribute-to-initarg' for reverse translation
;;           init arguments during document generation.
;;        Added new `replacement-args' to `call-next-method'.  It is
;;           still CLOS compatible, but now is more powerful.
;;        Added new default method `object-print' which is used by
;;           the functions in eieio-opt.  You can now specify
;;           additional summary information for object names if
;;           `object-print' is used instead of `object-name'.
;;        Better edebug integration with new spec's for all macros,
;;           plus new override `eieio-edebug-prin1-to-string' to
;;           print the summary with `class-name' and `object-print'.
;;        Added default-object-cache to class definition.  This may
;;           mess up object default functions.  At the moment
;;           however, (copy-sequence VECTOR) is much faster than the
;;           old (make-vector...) (eieio-set-defaults ...) ever was.
;;        Fixed problems with `lambda-default' used to create functions
;;           to be stored as default values, instead of evaluated at
;;           creation time.
;;        New `eieio-doc' file will create texinfo documentation
;;           describing a class hierarchy
;;        Modifies existing `lisp-imenu-generic-expression' to include
;;           defmethod.
;;        Fixed up comments and doc strings.
;;        `oref' and `oset' can now take the :initarg values if desired.

;;;
;; Variable declarations.  These variables are used to hold the call
;; state when using methods.
;;

;;; Code:
(eval-when-compile (require 'cl))

(defvar this nil
  "Inside a method, this variable is the object in question.
DO NOT SET THIS YOURSELF unless you are trying to simulate friendly fields.

Note: Embedded methods are no longer supported.  The variable THIS is
still set for CLOS methods for the sake of routines like
`call-next-method'")

(defvar scoped-class nil
  "This is set to a class when a method is running.
This is so we know we are allowed to check private parts or how to
execute a `call-next-method'.  DO NOT SET THIS YOURSELF!")

(defvar eieio-hook nil
  "*This hook is executed, then cleared each time `defclass' is called.
The immediate effect is that I can safely keep track of common-lisp
`setf' definitions regardless of the order.  Users can add hooks to
this variable without worrying about weather this package has been
loaded or not.")

;; This is a bootstrap for eieio-default-superclass so it has a value
;; while it is being built itself.
(defvar eieio-default-superclass nil)

(defconst class-symbol 1 "Class's symbol (self-referencing.).")
(defconst class-doc 2 "Class's documentation string.")
(defconst class-parent 3 "Class parent field.")
(defconst class-children 4 "Class children class field.")
(defconst class-symbol-obarray 5 "Obarray permitting fast access to variable position indexes.")
(defconst class-public-a 6 "Class public attribute index.")
(defconst class-public-d 7 "Class public attribute defaults index.")
(defconst class-public-doc 8 "Class public documentation strings for attributes.")
(defconst class-public-allocation 9 "Class public allocation type for a slot.")
(defconst class-public-type 10 "Class public type for a slot.")
(defconst class-public-custom 11 "Class public type for a slot.")
(defconst class-private-a 12 "Class private attribute index.")
(defconst class-private-d 13 "Class private attribute defaults index.")
(defconst class-private-doc 14 "Class private documentation strings for attributes.")
(defconst class-private-allocation 15 "Class public allocation type for a slot.")
(defconst class-private-type 16 "Class public type for a slot.")
(defconst class-initarg-tuples 17 "Class initarg tuples list.")
(defconst class-default-object-cache 18
  "Cache index of what a newly created object would look like.
This will speed up instantiation time as only a `copy-sequence' will
be needed, instead of looping over all the values and setting them
from the default.")

(defconst class-num-fields 19
  "Number of fields in the class definition object.")

(defconst object-class 1 "Index in an object vector where the class is stored.")
(defconst object-name 2 "Index in an object where the name is stored.")

(defconst method-before 0 "Index into :BEFORE tag on a method.")
(defconst method-primary 1 "Index into :PRIMARY tag on a method.")
(defconst method-after 2 "Index into :AFTER tag on a method.")
(defconst method-num-lists 3 "Number of indexes into methods vector in which groups of functions are kept.")
(defconst method-generic-before 3 "Index into generic :BEFORE tag on a method.")
(defconst method-generic-primary 4 "Index into generic :PRIMARY tag on a method.")
(defconst method-generic-after 5 "Index into generic :AFTER tag on a method.")
(defconst method-num-fields 6 "Number of indexes into a method's vector.")

;; How to specialty compile stuff.
(autoload 'byte-compile-file-form-defmethod "eieio-comp"
  "This function is used to byte compile methods in a nice way.")
(put 'defmethod 'byte-hunk-handler 'byte-compile-file-form-defmethod)

(eval-when-compile (require 'eieio-comp))


;;; Important macros used in eieio.
;;
(defmacro class-v (class) "Internal: Return the class vector from the CLASS symbol."
  ;; No check: If eieio gets this far, it's probably been checked already.
  (list 'get class ''eieio-class-definition))

(defmacro class-p (class) "Return t if CLASS is a valid class vector."
  ;; this new method is faster since it doesn't waste time checking lots of
  ;; things.
  (list 'condition-case nil
      (list 'eq (list 'aref (list 'class-v class) 0) ''defclass)
    '(error nil)))

(defmacro object-p (obj) "Return t if OBJ is an object vector."
  (list 'condition-case nil
	(list 'let (list (list 'tobj obj))
	      '(and (eq (aref tobj 0) 'object)
		    (class-p (aref tobj object-class))))
	'(error nil)))

(defmacro class-constructor (class)
  "Return the symbol representing the constructor of CLASS."
  (list 'aref (list 'class-v class) class-symbol))

(defmacro generic-p (method)
  "Return t if symbol METHOD is a generic function.
Only methods have the symbol `eieio-method-tree' as a property (which
contains a list of all bindings to that method type.)"
  (list 'and (list 'fboundp method) (list 'get method ''eieio-method-obarray)))


;;; Defining a new class
;;
(defmacro defclass (name superclass fields doc-string)
  "Define NAME as a new class derived from SUPERCLASS with FIELDS.
DOC-STRING is used as the class' base documentation.
SUPERCLASS is a list of superclasses to inherit from, with FIELDS
being the fields residing in that class definition.  NOTE: Currently
only one field may exist in SUPERCLASS as multiple inheritance is not
yet supported.  Supported tags are:

  :initform   - initializing form
  :initarg    - tag used during initialization
  :accessor   - tag used to create a function to access this field
  :writer     - a function symbol which will `write' an object's slot
  :reader     - a function symbol which will `read' an object
  :type       - the type of data allowed in this slot (eg. bufferp)

The following are extensions on CLOS:
  :protection - non-nil means a private slot (accessible when THIS is set)
  :custom     - When customizing an object, the custom :type.  Public only.

The following are accepted, and stored, but have no implementation:

  :allocation - defaults to :instance, but could also be :class"
  (list 'defclass-engine (list 'quote name) (list 'quote superclass)
	(list 'quote fields) doc-string))

(defun defclass-engine (cname superclass fields doc-string)
  "See `defclass' for more information.
Define CNAME as a new subclass of SUPERCLASS, with FIELDS being the
fields residing in that class definition, and with DOC-STRING as the
toplevel documentation for this class."
  ;; Run our eieio-hook each time, and clear it when we are done.
  ;; This way people can add hooks safely if they want to modify eieio
  ;; or add definitions when eieio is loaded or something like that.
  (run-hooks 'eieio-hook)
  (setq eieio-hook nil)
  ;; If no cl, put that sucker back into the hook-list.
  (if (not (featurep 'cl)) (add-hook 'eieio-hook 'eieio-cl-run-defsetf))

  (if (not (symbolp cname)) (signal 'wrong-type-argument '(symbolp cname)))
  (if (not (listp superclass)) (signal 'wrong-type-argument '(listp superclass)))
  (let* ((pname (if superclass (car superclass) nil))
	 (newc (make-vector class-num-fields nil))
	 (clearparent nil))
    (aset newc 0 'defclass)
    (aset newc class-symbol cname)
    (aset newc class-doc doc-string)
    (if (and pname (symbolp pname))
	(if (not (class-p pname))
	    ;; bad class
	    (error "Given parent class %s is not a class" pname)
	  ;; good parent class...
	  ;; save new child in parent
	  (if (not (member cname (aref (class-v pname) class-children)))
	      (aset (class-v pname) class-children (cons cname (aref (class-v pname) class-children))))
	  ;; save parent in child
	  (aset newc class-parent pname))
      (if pname
	  ;; pname has no value
	  (error "Invalid parent class %s" pname)
	(if (eq cname 'eieio-default-superclass)
	    ;; In this case, we have absolutly no parent...
	    (message "Bootstrapping objects...")
	  ;; adopt the default parent here, but clear it later...
	  (setq clearparent t)
	  ;; save new child in parent
	  (if (not (member cname (aref (class-v 'eieio-default-superclass) class-children)))
	      (aset (class-v 'eieio-default-superclass) class-children
		    (cons cname (aref (class-v 'eieio-default-superclass) class-children))))
	  ;; save parent in child
	  (aset newc class-parent eieio-default-superclass))))
    
    ;; before adding new fields, lets add all the methods and classes
    ;; in from the parent class
    (if (aref newc class-parent)
	(progn
	  (aset newc class-private-a (copy-sequence (aref (class-v (aref newc class-parent)) class-private-a)))
	  (aset newc class-private-d (copy-sequence (aref (class-v (aref newc class-parent)) class-private-d)))
	  (aset newc class-private-doc (copy-sequence (aref (class-v (aref newc class-parent)) class-private-doc)))
	  (aset newc class-private-allocation (copy-sequence (aref (class-v (aref newc class-parent)) class-private-allocation)))
	  (aset newc class-private-type (copy-sequence (aref (class-v (aref newc class-parent)) class-private-type)))
	  (aset newc class-public-a (copy-sequence (aref (class-v (aref newc class-parent)) class-public-a)))
	  (aset newc class-public-d (copy-sequence (aref (class-v (aref newc class-parent)) class-public-d)))
	  (aset newc class-public-doc (copy-sequence (aref (class-v (aref newc class-parent)) class-public-doc)))
	  (aset newc class-public-allocation (copy-sequence (aref (class-v (aref newc class-parent)) class-public-allocation)))
	  (aset newc class-public-type (copy-sequence (aref (class-v (aref newc class-parent)) class-public-type)))
	  (aset newc class-public-custom (copy-sequence (aref (class-v (aref newc class-parent)) class-public-custom)))
	  (aset newc class-initarg-tuples (copy-sequence (aref (class-v (aref newc class-parent)) class-initarg-tuples)))))

    ;; Store the new class vector definition into the symbol.  We need to
    ;; do this first so that we can call defmethod for the accessor.
    ;; The vector will be updated by the following while loop and will not
    ;; need to be stored a second time.
    (put cname 'eieio-class-definition newc)

    ;; Query each field in the declaration list and mangle into the
    ;; class structure I have defined.
    (while fields
      (let* ((field1 (car fields))
	     (name (car field1))
	     (field (cdr field1))
	     (acces (car (cdr (member ':accessor field))))
	     (init (car (cdr (member ':initform field))))
	     (initarg (car (cdr (member ':initarg field))))
	     (docstr (car (cdr (member ':docstring field))))
	     (prot (car (cdr (member ':protection field))))
	     (reader (car (cdr (member ':reader field))))
	     (writer (car (cdr (member ':writer field))))
	     (alloc (car (cdr (member ':allocation field))))
	     (type (member ':type field))
	     (custom (car (cdr (member ':custom field))))
	     )

	;; The default type specifier is supposed to be t, meaning anything.
	(if (not type) (setq type t)
	  (setq type (car (cdr type)))
	  (if (not (functionp type))
	      (error "Unlike CLOS, :type must be t, or be a function")))

	(let* ((-a (if (eq prot 'private) class-private-a class-public-a))
	       (-d (if (eq prot 'private) class-private-d class-public-d))
	       (-doc (if (eq prot 'private) class-private-doc class-public-doc))
	       (-alloc (if (eq prot 'private) class-private-allocation class-public-allocation))
	       (-type (if (eq prot 'private) class-private-type class-public-type))
	       (-custom (if (eq prot 'private) nil class-public-custom))
	       (-al (aref newc -a))
	       (-dl (aref newc -d))
	       (-docl (aref newc -doc))
	       (-allocl (aref newc -alloc))
	       (-typel (aref newc -type))
	       (-customl (if -custom (aref newc -custom) nil))
	       (np (member name -al))
	       (dp (if np (nthcdr (- (length -al) (length np)) -dl) nil)))
	  (if np
	      (progn
		;; If we have a repeat, only update the initarg...
		(setcar dp init)
		)
	    (aset newc -a (append -al (list name)))
	    (aset newc -d (append -dl (list init)))
	    (aset newc -doc (append -docl (list docstr)))
	    ;; Should the following slot types be updated always?  Hmm.
	    (aset newc -alloc (append -allocl (list alloc)))
	    (aset newc -type (append -typel (list type)))
	    (if -custom (aset newc -custom (append -customl (list custom))))
	    )
	  ;; public and privates both can install new initargs
	  (if initarg
	      (progn
		;; intern the symbol so we can use it blankly
		(set initarg initarg)
		;; find old occurance
		(let ((a (assoc initarg (aref newc class-initarg-tuples))))
		  ;; set the new arg only if not already set...
		  (if (not a)
		      (aset newc class-initarg-tuples
			    (append (aref newc class-initarg-tuples)
				    (list (cons initarg name))))))))
	  ;; anyone can have an accessor function.  This creates a function
	  ;; of the specified name, and also performs a `defsetf' if applicable
	  ;; so that users can `setf' the space returned by this function
	  (if acces
	      (progn
		(defmethod-engine acces
		  (list (list (list 'this cname))
			(format
			 "Retrieves the slot `%s' from an object of class `%s'"
			 name cname)
			(list 'oref-engine 'this (list 'quote name))))
		;; It turns out that using the setf macro with a
		;; generic method form is impossible because almost
		;; any type of form could be created for disparaging
		;; objects.  Yuck!  Therefore, we shouldn't try to make
		;; setf calls to accessors.
		;; Create a setf definition for this accessor.
		;(eieio-cl-defsetf acces '(widget)
		;		  '(store)
		;		  (list 'oset-engine 'widget
		;			(list 'quote cname)
		;			'store))
		)
	    )
	  ;; If a writer is defined, then create a generic method of that
	  ;; name whose purpose is to write out this slot value.
	  (if writer
	      (progn
		(defmethod-engine writer
		  (list (list (list 'this cname))
			(format
			 "Write the slot `%s' from object of class `%s'"
			 name cname)
			(list 'eieio-override-prin1
			      (list 'oref-engine 'this (list 'quote name)))))
		))
	  ;; If a reader is defined, then create a generic method
	  ;; of that name whose purpose is to read this slot value.
	  (if reader
	      (progn
		(defmethod-engine reader
		  (list (list (list 'this cname))
			(format
			 "Read the slot `%s' from object of class `%s'"
			 name cname)
			'(error "Not implemented")))))
	  )
	)
      (setq fields (cdr fields)))

    ;; turn this into a useable self-pointing symbol
    (set cname cname)

    ;; Set up a specialized doc string
    (eieio-rebuild-doc-string cname)

    ;; Attach field symbols into an obarray, and store the index of
    ;; this field as the variable slot in this new symbol.  We need to
    ;; know about primes, because obarrays are best set in vectors of
    ;; prime number length, and we also need to make our vector small
    ;; to save space, and also optimal for the number of items we have.
    (let* ((cnt 0)
	   (pubsyms (aref newc class-public-a))
	   (privsyms (aref newc class-private-a))
	   (l (+ (length pubsyms) (length privsyms)))
	   (vl (let ((primes '( 3 5 7 11 13 17 19 23 29 31 37 41 43 47
				  53 59 61 67 71 73 79 83 89 97 101 )))
		 (while (and primes (< (car primes) l))
		   (setq primes (cdr primes)))
		 (car primes)))
	   (oa (make-vector vl 0))
	   (newsym))
      (while pubsyms
	(set (intern (symbol-name (car pubsyms)) oa) cnt)
	(setq cnt (1+ cnt))
	(setq pubsyms (cdr pubsyms)))
      (while privsyms
	(setq newsym (intern (symbol-name (car privsyms)) oa))
	(set newsym cnt)
	(put newsym 'private t)
	(setq cnt (1+ cnt))
	(setq privsyms (cdr privsyms)))
      (aset newc class-symbol-obarray oa)
      )

    ;; Create the constructor function
    (fset cname
	  (list 'lambda (list 'newname '&rest 'fields)
		(format "Create a new object with name NAME of class type %s" cname)
		(list
		 'let (list (list 'no
				  (list 'copy-sequence
					(list 'aref
					      (list 'class-v cname)
					      'class-default-object-cache))))
		 '(aset no object-name newname)
		 '(initialize-instance no fields)
		 'no)))

    ;; Create the test function
    (let ((csym (intern (concat (symbol-name cname) "-p"))))
      (fset csym
	    (list 'lambda (list 'obj)
		  (format "Test OBJ to see if it an object of type %s" cname)
		  (list 'same-class-p 'obj cname))))

    ;; if this is a superclass, clear out parent (which was set to the
    ;; default superclass eieio-default-superclass)
    (if clearparent (aset newc class-parent nil))

    ;; Indicate bootstrapping is done...
    (if (eq cname 'eieio-default-superclass)
	(message "Bootstrapping objects...done"))

    ;; Create the cached default object.
    (let ((cache (make-vector (+ (length (aref newc class-public-a))
				 (length (aref newc class-private-a))
				 3) nil)))
	  (aset cache 0 'object)
	  (aset cache object-class cname)
	  (aset cache object-name 'default-cache-object)
	  (eieio-set-defaults cache t)
	  (aset newc class-default-object-cache cache))

    ;; Return our new class object
    newc
    ))

;;; CLOS style implementation of object creators.
;;
(defun make-instance (class &rest initargs)
  "Make a new instance of CLASS with initialization based on INITARGS.
INITARGS starts with a name for the class.  This can be any valid Lisp
object, but is generally a string.  The rest of the init args are
label/value pairs.  The label's are the symbols created with the
:initarg tag from the `defclass' call.  The value is the value stored
in that slot."
  (let ((cc (class-constructor class))) (apply cc class initargs)))

;;; CLOS methods and generics
;;
(defmacro defgeneric (method args &optional doc-string)
  "Create a generic function METHOD.  ARGS is ignored.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as it's purpose is to decide which method body
is appropriate to use.  Use `defmethod' to create methods, and it
calls defgeneric for you.  With this implementation the arguments are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method."
  (list 'defgeneric-engine
	(list 'quote method)
	doc-string))

(defun defgeneric-engine (method doc-string)
  "Engine part to `defgeneric' macro defining METHOD with DOC-STRING."
  (let ((lambda-form
	 (list 'lambda '(&rest local-args)
	       doc-string
	       (list 'eieio-generic-call
		     (list 'quote method)
		     'local-args))))
    (if (and (fboundp method) (not (generic-p method)))
	(error "You cannot create a generic/method over an existing symbol"))
    (fset method lambda-form)
    'method))

(defmacro defmethod (method &rest args)
  "Create a new METHOD through `defgeneric' with ARGS.
ARGS lists any keys (such as :BEFORE or :AFTER), the arglst, and
doc string, and eventually the body, such as:

  (defmethod mymethod [:BEFORE | :AFTER] (args) doc-string body)"
  (list 'defmethod-engine
	(list 'quote method)
	(list 'quote args)))

(defun defmethod-engine (method args)
  "Work part of the `defmethod' macro defining METHOD with ARGS."
  (let ((key nil) (body nil) (firstarg nil) (argfix nil) loopa)
    ;; find optional keys
    (setq key
	  (cond ((eq ':BEFORE (car args))
		 (setq args (cdr args))
		 0)
		((eq ':AFTER (car args))
		 (setq args (cdr args))
		 2)
		(t 1)))
    ;; get body, and fix contents of args to be the arguments of the fn.
    (setq body (cdr args)
	  args (car args))
    (setq loopa args)
    ;; Create a fixed version of the arguments
    (while loopa
      (setq argfix (cons (if (listp (car loopa)) (car (car loopa)) (car loopa))
			 argfix))
      (setq loopa (cdr loopa)))
    ;; make sure there is a generic
    (if (not (fboundp method))
	(defgeneric-engine method
	  (if (stringp (car body))
	      (car body) (format "Generically created method %s" method))))
    ;; create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (setq firstarg (car args))
    (if (listp firstarg)
	(if (not (class-p (nth 1 firstarg)))
	    (error "Unknown class type %s in method parameters" (nth 1 firstarg)))
      ;; generics are higher
      (setq key (+ key method-num-fields)))
    ;; Put this lambda into the symbol so we can find it
    (if (byte-code-function-p (car-safe body))
	(eieiomt-add method (car-safe body) key (nth 1 firstarg))
      (eieiomt-add method (append (list 'lambda (reverse argfix)) body)
		   key (nth 1 firstarg)))
    (eieio-rebuild-generic-doc-string method)
    )
  method)

;;; Get/Set slots in an object.
;;
(defmacro oref (obj field)
  "Retrieve the value stored in OBJ in the slot named by FIELD.
Field is the name of the slot when created by `defclass' or the label
created by the :initarg tag."
  (list 'oref-engine obj (list 'quote field)))

(defun oref-engine (obj field)
  "Return the value in OBJ at FIELD in the object vector."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let ((c (eieio-field-name-index (aref obj object-class) field)))
    (if (not c)
	(slot-missing obj field 'oref)
	;;(signal 'invalid-slot-name (list (object-name obj) field))
      )
    (aref obj c)))

(defalias 'slot-value 'oref-engine)

;; This alias is needed so that functions can be written
;; for defaults, but still behave like lambdas.
(defmacro lambda-default (&rest cdr)
  "The same as `lambda' but is used as a default value in `defclass'.
As such, the form (lambda-default ARGS DOCSTRING INTERACTIVE BODY) is
self quoting.  This macro is mean for the sole purpose of quoting
lambda expressions into class defaults.  Any `lambda-default'
expression is automatically transformed into a `lambda' expression
when copied from the defaults into a new object.  The use of
`oref-default', however, will return a `lambda-default' expression.
CDR is function definition and body."
  ;; This definition is copied directly from subr.el for lambda
  (list 'function (cons 'lambda-default cdr)))

(put 'lambda-default 'lisp-indent-function 'defun)
(put 'lambda-default 'byte-compile 'byte-compile-lambda-form)

(defmacro oref-default (obj field)
  "Gets the default value of OBJ (maybe a class) for FIELD.
The default value is the value installed in a class with the :initform
tag.  FIELD can be the slot name, or the tag specified by the :initarg
tag in the `defclass' call."
  (list 'oref-default-engine obj (list 'quote field)))

(defun oref-default-engine (obj field)
  "Does the work for the macro `oref-default' with similar parameters.
Fills in OBJ's FIELD with it's default value."
  (if (not (or (object-p obj) (class-p obj))) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let* ((cl (if (object-p obj) (aref obj object-class) obj))
	 (c (eieio-field-name-index cl field))
	 (nump (length (aref (class-v cl) class-public-a))))
    (if (not c)
	(slot-missing obj field 'oref-default)
	;;(signal 'invalid-slot-name (list (class-name cl) field))
      )
    (let ((val (if (< c (+ 3 nump))
		   (nth (- c 3) (aref (class-v cl) class-public-d))
		 (nth (- c nump 3) (aref (class-v cl) class-private-d)))))
      ;; check for functions to evaluate
      (if (or (and (listp val) (equal (car val) 'lambda))
	      (and (symbolp val) (fboundp val)))
	  (let ((this obj))
	    (funcall val))
	;; check for quoted things
	(if (and (listp val) (equal (car val) 'quote))
	    (car (cdr val))
	  ;; return it verbatim
	  val)))))

(defun eieio-perform-slot-validation (spec value)
  "Signal if SPEC does not match VALUE."
  ;; This is SUPPOSED to follow CLOS or CL type specifiers, but for
  ;; simplicity and speed, I'll just pretend it's a function.
  (or (eq spec t)
      (and (functionp spec) (funcall spec value))))

(defun eieio-validate-slot-value (class field-idx value)
  "Make sure that for CLASS referencing FIELD-IDX, that VALUE is valid.
Checks the :type specifier."
  ;; Trim off object IDX junk.
  (setq field-idx (- field-idx 3))
  (let ((s (aref (class-v class) class-public-a))
	(spec nil))
    (if (not (eieio-perform-slot-validation
	      (if (< field-idx (length s))
		  ;; It's public
		  (nth field-idx (aref (class-v class) class-public-type))
		;; It's private.  Adjust lists, and zero in.
		(setq s (aref (class-v class) class-private-a)
		      field-idx (- field-idx (length s)))
		(nth field-idx (aref (class-v class) class-private-type)))
	      value))
	(signal 'invalid-slot-type '(list 'oset value)))))

(defmacro oset (obj field value)
  "Set the value in OBJ for slot FIELD to VALUE.
FIELD is the slot name as specified in `defclass' or the tag created
with in the :initarg slot.  VALUE can be any Lisp object."
  (list 'oset-engine obj (list 'quote field) value))

(defun oset-engine (obj field value)
  "Does the work for the macro `oset'.
Fills in OBJ's FIELD with VALUE."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let ((c (eieio-field-name-index (object-class-fast obj) field)))
    (if (not c)
	(slot-missing obj field 'oset value)
	;;(signal 'invalid-slot-name (list (object-name obj) field))
      )
    (aset obj c value)))

(defmacro oset-default (class field value)
  "Set the default slot in CLASS for FIELD to VALUE.
The default value is usually set with the :initform tag during class
creation.  This allows users to change the default behavior of classes
after they are created."
  (list 'oset-default-engine class (list 'quote field) value))

(defun oset-default-engine (class field value)
  "Does the work for the macro `oset-default'.
Fills in the default value in CLASS' in FIELD with VALUE."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let* ((scoped-class class)
	 (c (eieio-field-name-index class field))
	 (nump (length (aref (class-v class) class-public-a))))
    (if (not c) (signal 'invalid-slot-name (list (class-name class) field)))
    (setcar
     (if (< c (+ 3 nump))
	 (nthcdr (- c 3) (aref (class-v class) class-public-d))
       (nthcdr (- c nump 3) (aref (class-v class) class-private-d)))
     value)))

(defmacro with-slots (spec-list object &rest body)
  "Create a lexical scope for slots in SPEC-LIST for OBJECT.
Execute BODY within this lexical scope."
  ;; Special thanks to Kevin Rodgers <kevinr@ihs.com> for helping me with this
  (let ((object-var (make-symbol "with-slots-obj")))
    `(let* ((,object-var ,object)
	    ,@(mapcar (lambda (spec)
			  (cond ((symbolp spec)
				 `(,spec (slot-value ,object-var (quote ,spec))))
				((consp spec)
				 `(,(car spec)
				   (slot-value ,object-var (quote ,(cadr spec)))))
				(t (error "Invalid binding spec: %s" spec))))
			spec-list))
       ,@body)))
(put 'with-slots 'lisp-indent-function 2)

;;; Simple generators, and query functions.  None of these would do
;;  well embedded into an object.
;;
(defmacro object-class-fast (obj) "Return the class struct defining OBJ with no check."
  (list 'aref obj object-class))
  
(defun class-name (class) "Return a Lisp like symbol name for CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  ;; I think this is supposed to return a symbol, but to me CLASS is a symbol,
  ;; and I wanted a string.  Arg!
  (format "#<class %s>" (symbol-name class)))

(defun object-name (obj &optional extra)
  "Return a Lisp like symbol string for object OBJ.
If EXTRA, include that in the string returned to represent the symbol."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (format "#<%s %s%s>" (symbol-name (object-class-fast obj))
	  (aref obj object-name) (or extra "")))

(defun object-name-string (obj) "Return a string which is OBJ's name."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (aref obj object-name))

(defun object-class (obj) "Return the class struct defining OBJ."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (object-class-fast obj))
(defalias 'class-of 'object-class)

(defun object-class-name (obj) "Return a Lisp like symbol name for OBJ's class."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (class-name (object-class-fast obj)))

(defmacro class-parent-fast (class) "Return parent class to CLASS with no check."
  (list 'aref (list 'class-v class) class-parent))

(defun class-parent (class) "Return parent class to CLASS.  (overload of variable)."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (class-parent-fast class))

(defmacro same-class-fast-p (obj class) "Return t if OBJ is of class-type CLASS with no error checking."
  (list 'eq (list 'aref obj object-class) class))

(defun same-class-p (obj class) "Return t if OBJ is of class-type CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (same-class-fast-p obj class))

(defun obj-of-class-p (obj class) "Return t if OBJ inherits anything from CLASS."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  ;; class will be checked one layer down
  (child-of-class-p (aref obj object-class) class))

(defun child-of-class-p (child class) "If CHILD inherits anything from CLASS, return CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (class-p child)) (signal 'wrong-type-argument (list 'class-p child)))
  (while (and child (not (eq child class)))
    (setq child (aref (class-v child) 3)))
  child)

(defun obj-fields (obj) "List of fields available in OBJ."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (aref (class-v (object-class-fast obj)) class-public-a))

(defun class-slot-initarg (class slot) "Fetch from CLASS, SLOT's :initarg."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (let ((ia (aref (class-v class) class-initarg-tuples))
	(f nil))
    (while (and ia (not f))
      (if (eq (cdr (car ia)) slot)
	  (setq f (car (car ia))))
      (setq ia (cdr ia)))
    f))

(defmacro slot-boundp (object slot)
  "Non-nil if OBJECT's SLOT is bound.
Strictly speaking in CLOS, a slot can exist in OBJECT, but not be bound.
This is not the case in EIEIO as all slots are bound at instantiation time.
Therefore `slot-boundp' is really a macro calling `slot-exists-p'"
  `(slot-exists-p ,object ,slot))

(defun slot-exists-p (object slot)
  "Non-nil if OBJECT contains SLOT."
  (let ((cv (class-v (object-class object))))
    (or (memq slot (aref cv class-public-a))
	(memq slot (aref cv class-private-a)))))	

;;; Slightly more complex utility functions for objects
;;
(defun object-assoc (key field list)
  "Return non-nil if KEY is `equal' to the FIELD of the car of objects in LIST.
The value is actually the element of LIST whose field equals KEY."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (while (and list (not (condition-case nil
			    ;; This prevents errors for missing slots.
			    (equal key (oref-engine (car list) field))
			  (error nil))))
    (setq list (cdr list)))
  (car list))

(defun object-assoc-list (field list)
  "Return an association list with the contents of FIELD as the key element.
LIST must be a list of objects with FIELD in it.
This is useful when you need to do completing read on an object group."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (let ((assoclist nil))
    (while list
      (setq assoclist (cons (cons (oref-engine (car list) field)
				  (car list))
			    assoclist))
      (setq list (cdr list)))
    (nreverse assoclist)))


;;; EIEIO internal search functions
;;

(defun eieio-field-name-index (class field)
  "In CLASS find the index of the named FIELD.
The field is a symbol which is installed in CLASS by the `defclass'
call.  If FIELD is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; Removed checks to outside this call
  (let* ((fsym (intern-soft (symbol-name field)
			    (aref (class-v class)
				  class-symbol-obarray)))
	 (fsi (if (symbolp fsym) (symbol-value fsym) nil)))
    (if (integerp fsi)
	(if (or (not (get fsym 'private))
		(and scoped-class (child-of-class-p class scoped-class)))
	    (+ 3 fsi)
	  nil)
      (let ((fn (eieio-initarg-to-attribute class field)))
	(if fn (eieio-field-name-index class fn) nil)))))

;;; CLOS generics internal function handling
;;
(defvar eieio-generic-call-methodname nil
  "When using `call-next-method', provides a context on how to do it.")
(defvar eieio-generic-call-arglst nil
  "When using `call-next-method', provides a context for parameters.")

(defun eieio-generic-call (method args)
  "Call METHOD with ARGS.
ARGS provides the context on which implementation to use.
This should only be called from a generic function."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil)
	(eieio-generic-call-methodname method)
	(eieio-generic-call-arglst args))
    ;; get a copy
    (setq newargs args)
    ;; lookup the forms to use
    (if (object-p (car newargs))
	(setq mclass (object-class-fast (car newargs))))
    ;; Now create a list in reverse order of all the calls we have
    ;; make in order to successfully do this right.  Rules:
    ;; 1) Only call generics if scoped-class is not defined
    ;;    This prevents multiple calls in the case of recursion
    ;; 2) Only call specifics if the definition allows for them.
    ;; 3) Call in order based on :BEFORE, :PRIMARY, and :AFTER
    (if (not scoped-class)
	(setq lambdas (cons (eieio-generic-form method method-after nil)
			    lambdas)))
    (if mclass
	(setq lambdas (cons (eieio-generic-form method method-after mclass)
			    lambdas)))
    (if (not scoped-class)
	(setq lambdas (cons (eieio-generic-form method method-primary nil)
			    lambdas)))
    (if mclass
	(setq lambdas (cons (eieio-generic-form method method-primary mclass)
			    lambdas)))
    (if (not scoped-class)
	(setq lambdas (cons (eieio-generic-form method method-before nil)
			    lambdas)))
    (if mclass
	(setq lambdas (cons (eieio-generic-form method method-before mclass)
			    lambdas)))

    ;; Now loop through all occurances forms which we must execute
    ;; (which are happilly sorted now) and execute them all!
    (let ((rval nil) (found nil))
      (while lambdas
	(if (car lambdas)
	    (let ((scoped-class (cdr (car lambdas))))
	      (setq found t)
	      (setq rval (apply (car (car lambdas)) newargs))))
	(setq lambdas (cdr lambdas)))
      (if (not found) (signal
		       'no-method-definition
		       (list method
			     (if (object-p (car args))
				 (object-name (car args))
			       args))))
      rval)))

(defun call-next-method (&rest replacement-args)
  "Call the next logical method from another method.
The next logical method is the method belong to the parent class of
the currently running method.  If REPLACEMENT-ARGS is non-nil, then
use them instead of `eieio-generic-call-arglst'.  The generic arg list
are the arguments passed in at the top level."
  (if (not scoped-class)
      (error "Call-next-method not called within a class specific method"))
  (let ((newargs (or replacement-args eieio-generic-call-arglst))
	(lambdas nil)
	(mclass (eieiomt-next scoped-class)))
    ;; lookup the form to use for the PRIMARY object for the next level
    (setq lambdas (eieio-generic-form eieio-generic-call-methodname
				      method-primary mclass))
    ;; Setup calling environment, and apply arguments...
    (let ((scoped-class (cdr lambdas)))
      (apply (car lambdas) newargs))))


;;;
;; eieio-method-tree : eieiomt-
;;
;; Stored as eieio-method-tree in property list of a generic method
;;
;; (eieio-method-tree . [BEFORE PRIMARY AFTER
;;                       genericBEFORE genericPRIMARY genericAFTER])
;; and
;; (eieio-method-obarray . [BEFORE PRIMARY AFTER
;;                          genericBEFORE genericPRIMARY genericAFTER])
;;    where the association is a vector.
;;    (aref 0  -- all methods classified as :BEFORE
;;    (aref 1  -- all methods classified as :PRIMARY
;;    (aref 2  -- all methods classified as :AFTER
;;    (aref 3  -- a generic classified as :BEFORE
;;    (aref 4  -- a generic classified as :PRIMARY
;;    (aref 5  -- a generic classified as :AFTER
;;
;; Each list of methods is stored as follows:
;;
;; ( ( class . function ) ( class ... ))
;;
;; The elts 3-5 are mearly function bodies
;;
(defvar eieiomt-optimizing-obarray nil
  "While mapping atoms, this contain the obarray being optimized.")

(defun eieiomt-add (method-name method tag class)
  "Add to METHOD-NAME the forms METHOD in a call position TAG for CLASS.
METHOD-NAME is the name created by a call to `defgeneric'.
METHOD are the forms for a given implementation.
TAG is an integer (see comment in eieio.el near this function) which
is associated with the :BEFORE :PRIMARY and :AFTER tags and weather
CLASS is defined or not.  CLASS is the class this method is associated
with."
  (if (or (>= tag method-num-fields) (< tag 0))
      (error "eieiomt-add: method tag error!"))
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-obarray)))
    (if (or (not emtv) (not emto))
	(progn
	  (setq emtv (put method-name 'eieio-method-tree
			  (make-vector method-num-fields nil))
		emto (put method-name 'eieio-method-obarray
			  (make-vector method-num-fields nil)))
	  (aset emto 0 (make-vector 11 0))
	  (aset emto 1 (make-vector 41 0))
	  (aset emto 2 (make-vector 11 0))
	  ))
    ;; only add new cells on if it doesn't already exist!
    (if (assq class (aref emtv tag))
	(setcdr (assq class (aref emtv tag)) method)
      (aset emtv tag (cons (cons class method) (aref emtv tag))))
    ;; Add function definition into newly created symbol, and store
    ;; said symbol in the correct obarray, otherwise use the
    ;; other array to keep this stuff
    (if (< tag method-num-lists)
	(let ((nsym (intern (symbol-name class) (aref emto tag))))
	  (fset nsym method)))
    ;; Now optimize the entire obarray
    (if (< tag method-num-lists)
	(let ((eieiomt-optimizing-obarray (aref emto tag)))
	  (mapatoms 'eieiomt-sym-optimize eieiomt-optimizing-obarray)))
    ))

(defun eieiomt-get (method-name tag class)
  "Get the implementation for METHOD-NAME for int TAG matching CLASS.
See `eieiomt-add' for details on how these are set."
  (if (>= tag method-num-fields) (< tag 0)
    (error "eieiomt-get: method tag error!"))
  (let ((emto (get method-name 'eieio-method-obarray)))
    (if (not emto)
	nil
      (intern-soft (symbol-name class) (aref emto tag)))))

(defun eieiomt-next (class)
  "Return the next parent class for CLASS.
If CLASS is a superclass, return variable `eieio-default-superclass'.  If CLASS
is variable `eieio-default-superclass' then return nil.  This is different from
function `class-parent' as class parent returns nil for superclasses.  This
function performs no type checking!"
  ;; No type-checking because all calls are made from functions which
  ;; are safe and do checking for us.
  (or (class-parent-fast class)
      (if (eq class 'eieio-default-superclass)
	  nil
	'eieio-default-superclass)))

(defun eieiomt-sym-optimize (s)
  "Find the next class above S which has a function body for the optimizer."
  ;; (message "Optimizing %S" s)
  (let ((es (intern-soft (symbol-name s))) ;external symbol of class
	(ov nil)
	(cont t))
    (setq es (eieiomt-next es))
    (while (and es cont)
      (setq ov (intern-soft (symbol-name es) eieiomt-optimizing-obarray))
      (if (fboundp ov)
	  (progn
	    (set s ov)			;store ov as our next symbol
	    (setq cont nil))
	(setq es (eieiomt-next es))))
    ;; If there is no nearest call, then set our value to nil
    (if (not es) (set s nil))
    ))

(defun eieio-generic-form (method tag class)
 "Return the lambda form belonging to METHOD using TAG based upon CLASS.
If CLASS is not a class then use `generic' instead.  If class has no
form, but has a parent class, then trace to that parent class.  The
first time a form is requested from a symbol, an optimized path is
memoized for future faster use."
 (let ((emto (aref (get method 'eieio-method-obarray) (if class tag (+ tag 3)))))
   (if (class-p class)
       ;; 1) find our symbol
       (let ((cs (intern-soft (symbol-name class) emto)))
	 (if (not cs)
	     ;; 2) If there isn't one, then make on.
	     ;;    This can be slow since it only occurs once
	     (progn
	       (setq cs (intern (symbol-name class) emto))
	       ;; 2.1) Cache it's nearest neighbor with a quick optimize
	       ;;      which should only occur once for this call ever
	       (let ((eieiomt-optimizing-obarray emto))
		 (eieiomt-sym-optimize cs))))
	 ;; 3) If it's bound return this one.
	 (if (fboundp  cs)
	     (cons cs (aref (class-v class) class-symbol))
	   ;; 4) If it's not bound then this variable knows something
	   (if (symbol-value cs)
	       (progn
		 ;; 4.1) This symbol holds the next class in it's value
		 (setq class (symbol-value cs)
		       cs (intern-soft (symbol-name class) emto))
		 ;; 4.2) The optimizer should always have chosen a
		 ;;      function-symbol
		 ;;(if (fboundp cs)
		 (cons cs (aref (class-v (intern (symbol-name class)))
				class-symbol))
		   ;;(error "EIEIO optimizer: erratic data loss!"))
		 )
	       ;; There never will be a funcall...
	       nil)))
     ;; for a generic call, what is a list, is the function body we want.
     (let ((emtl (aref (get method 'eieio-method-tree)
		       (if class tag (+ tag 3)))))
       (if emtl
	 (cons emtl nil)
	 nil)))))

;;;
;; Way to assign fields based on a list.  Used for constructors, or
;; even resetting an object at run-time
;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all fields to their defaults.
If SET-ALL is non-nil, then when a default is nil, that value is
reset.  If SET-ALL is nil, the fields are only reset if the default is
not nil."
  (let ((scoped-class (aref obj object-class))
	(pub (aref (class-v (aref obj object-class)) class-public-a))
	(priv (aref (class-v (aref obj object-class)) class-private-a)))
    (while pub
      (let ((df (oref-default-engine obj (car pub))))
	(if (and (listp df) (eq (car df) 'lambda-default))
	    (progn
	      (setq df (copy-sequence df))
	      (setcar df 'lambda)))
	(if (or df set-all)
	    (oset-engine obj (car pub) df)))
      (setq pub (cdr pub)))
    (while priv
      (let ((df (oref-default-engine obj (car priv))))
	(if (and (listp df) (eq (car df) 'lambda-default))
	    (progn
	      (setq df (copy-sequence df))
	      (setcar df 'lambda)))
	(if (or df set-all)
	    (oset-engine obj (car priv) df)))
      (setq priv (cdr priv)))))

(defun eieio-initarg-to-attribute (class initarg)
  "For CLASS, convert INITARG to the actual attribute name.
If there is no translation, pass it in directly (so we can cheat if
need be.. May remove that later...)"
  (let ((tuple (assoc initarg (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(cdr tuple)
      nil)))

(defun eieio-attribute-to-initarg (class attribute)
  "In CLASS, convert the ATTRIBUTE into the corresponding init argument tag.
This is usually a symbol that starts with `:'."
  (let ((tuple (rassoc attribute (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(car tuple)
      nil)))

(defun eieio-rebuild-doc-string (class)
  "Rebuilds the documentation for CLASS.
Look in CLASS for it's stored doc-string, and the doc string of
it's methods.  Use this to set the variable 'CLASSes doc string for
viewing by apropos, and describe-variables, and the like."
  (if (not (class-p class)) (signal 'wrong-type-argument '(class-p class)))
  (let* ((cv (class-v class))
	 (newdoc (aref cv class-doc))
	 (docs (aref cv class-public-doc))
	 (names (aref cv class-public-a))
	 (deflt (aref cv class-public-d))
	 (pdocs (aref cv class-private-doc))
	 (pnames (aref cv class-private-a))
	 (pdeflt (aref cv class-private-d))
	 )
    (while names
      (setq newdoc (concat newdoc "\n\nSlot: " (symbol-name (car names))
			   "    default = " (format "%S" (car deflt))
			   (if (car docs) (concat "\n" (car docs)) "")))
      (setq names (cdr names)
	    docs (cdr docs)
	    deflt (cdr deflt)))
    (if pnames (setq newdoc (concat newdoc "\n\nPrivate Fields:")))
    (while pnames
      (setq newdoc (concat newdoc "\n\nSlot: " (symbol-name (car pnames))
			   "    default = " (format "%S" (car pdeflt))
			   (if (car pdocs) (concat "\n" (car pdocs)) "")))
      (setq pnames (cdr pnames)
	    pdocs (cdr pdocs)
	    pdeflt (cdr pdeflt)))
    ;; only store this on the variable.  The doc-string in the vector
    ;; is ONLY the top level doc for this class.  The value found via
    ;; emacs needs to be more descriptive.
    (put class 'variable-documentation newdoc)))

(defun eieio-rebuild-generic-doc-string (sym)
  "Rebuilds the documentation string for the generic method SYM.
The documentation is set to a generic doc-string and all the
specialized forms for each implementation."
  (if (not (generic-p sym)) (signal 'wrong-type-argument '(generic-p sym)))
  (let ((newdoc "Generic function.  This function accepts a generic number of arguments
and then, based on the arguments calls some number of polymorphic methods
associated with this symbol.  Current method specific code is:")
	(i 3)
	(prefix [ ":BEFORE" ":PRIMARY" ":AFTER" ] ))
    (while (< i 6)
      (let ((gm (aref (get sym 'eieio-method-tree) i)))
	(if gm
	    (setq newdoc (concat newdoc "\n\nGeneric " (aref prefix (- i 3)) "\n"
				 (if (nth 2 gm) (nth 2 gm) "Undocumented")))))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 3)
      (let ((gm (reverse (aref (get sym 'eieio-method-tree) i))))
	(while gm
	  (setq newdoc (concat newdoc "\n\n" (symbol-name (car (car gm)))
			       ;; prefix type
			       " " (aref prefix i) " "
			       ;; argument list
			       (let* ((func (cdr (car gm)))
				      (arglst (if (byte-code-function-p func)
						  (aref func 0)
						(car (cdr func)))))
				 (format "%S" arglst))
			       "\n"
			       ;; 3 because of cdr
			       (if (documentation (cdr (car gm)))
				   (documentation (cdr (car gm)))
				 "Undocumented")))
	  (setq gm (cdr gm))))
      (setq i (1+ i)))
    ;; tuck this bit of information away.
    (defgeneric-engine sym newdoc)
    ))

;;; Here are some special types of errors
;;
(intern "no-method-definition")
(put 'no-method-definition 'error-conditions '(no-method-definition error))
(put 'no-method-definition 'error-message "No method definition")

(intern "invalid-slot-name")
(put 'invalid-slot-name 'error-conditions '(invalid-slot-name error))
(put 'invalid-slot-name 'error-message "Invalid slot name")

(intern "invalid-slot-type")
(put 'invalid-slot-type 'error-conditions '(invalid-slot-type nil))
(put 'invalid-slot-type 'error-message "Invalid slot type")

;;; Here are some CLOS items that need the CL package
;;
(defun eieio-cl-run-defsetf ()
  "Execute many `defsetf's when the 'cl package is loaded."
  (if (featurep 'cl)
      (progn
	(defsetf slot-value (obj field) (store)
	  (list 'oset-engine obj field store))
	(defsetf oref-engine (obj field) (store)
	  (list 'oset-engine obj field store))
	(defsetf oref (obj field) (store)
	  (list 'oset-engine obj field store))))
  )

(add-hook 'eieio-hook 'eieio-cl-run-defsetf)


;;;
;; We want all objects created by EIEIO to have some default set of
;; behaviours so we can create object utilities, and allow various
;; types of error checking.  To do this, create the default EIEIO
;; class, and when no parent class is specified, use this as the
;; default.  (But don't store it in the other classes as the default,
;; allowing for transparent support.)
;;

(defclass eieio-default-superclass nil
  nil
  "Default class used as parent class for superclasses.
Its fields are automatically adopted by such superclasses but not
stored in the `parent' field.  When searching for attributes or
methods, when the last parent is found, the search will recurse to
this class.")

(defalias 'standard-class 'eieio-default-superclass)

(defmethod shared-initialize ((obj eieio-default-superclass) fields)
  "Set fields of OBJ with FIELDS which is a list of name/value pairs.
Called from the constructor routine."
  (let ((scoped-class (aref obj object-class)))
    (while fields
      (let ((rn (eieio-initarg-to-attribute (object-class-fast obj)
					    (car fields))))
	(oset-engine obj rn (car (cdr fields))))
      (setq fields (cdr (cdr fields))))))

(defmethod initialize-instance ((this eieio-default-superclass)
				&optional fields)
    "Constructs the new object THIS based on FIELDS.
FIELDS is a tagged list where odd numbered elements are tags, and
even numbered elements are the values to store in the tagged slot.  If
you overload the initialize-instance, there you will need to call
`shared-initialize' yourself, or you can call `call-next-method' to
have this constructor called automatically.  If these steps are not
taken, then new objects of your class will not have their values
dynamically set from FIELDS."
    (shared-initialize this fields))

(defmethod slot-missing ((object eieio-default-superclass) slot-name
			 operation &optional new-value)
  "Slot missing is invoked when an attempt to access a slot in OBJECT  fails.
SLOT-NAME is the name of the failed slot, OPERATION is the type of access
that was requested, and optional NEW-VALUE is the value that was desired
to be set."
  (signal 'invalid-slot-name (list (class-name (object-class object))
				   slot-name)))

(defmethod clone ((obj eieio-default-superclass) &rest params)
  "Make a deep copy of OBJ, and then apply PARAMS.
PARAMS is a parameter list of the same form as INITIALIZE-INSTANCE
which are applied to change the object.  When overloading `clone', be
sure to call `call-next-method' first and modify the returned object."
  (let ((nobj (copy-sequence obj))
	(nm (aref obj object-name))
	(num 1))
    (if params (shared-initialize obj params))
    (save-match-data
      (if (string-match "-\\([0-9]+\\)" nm)
	  (setq num (1+ (string-to-int (match-string 1 nm)))
		nm (substring nm 0 (match-beginning 0)))))
    (aset nobj object-name (concat nm "-" (int-to-string num)))
    nobj))

(defmethod destructor ((this eieio-default-superclass) &rest params)
  "Destructor for cleaning up any dynamic links to our object."
  ;; No cleanup... yet.
  )

(defmethod object-print ((this eieio-default-superclass) &rest strings)
  "Pretty printer for any object.  Calls `object-name' with STRINGS.
  The default method for printing an object is to use the
`object-name' function.  At times it could be useful to put a summary
of the object into the default #<notation> string.  Overload this
function to allow summaries of your objects to be used by eieio
browsing tools.  The optional parameter STRINGS is for additional
summary parts to put into the name string.  When passing in extra
strings from child classes, always remember to prepend a space."
  (object-name this (apply 'concat strings)))

(defvar eieio-print-depth 0
  "When printing, keep track of the current indentation depth.")

(defmethod object-write ((this eieio-default-superclass) &optional comment)
  "Write an object out to the current stream.
This writes out the vector version of this object.  Complex and recursive
object are discouraged from being written.
  If optional COMMENT is non-nil, include comments when outputting
this object."
  (if (not comment) nil
    (princ ";; Object ")
    (princ (object-name-string this))
    (princ "\n")
    (princ comment)
    (princ "\n"))
  (let* ((cl (object-class this))
	 (cv (class-v cl)))
    ;; Now output readable lisp to recreate this object
    ;; It should look like this:
    ;; (<constructor> <name> <slot> <field> ... )
    ;; Each slot's field is writen using its :writer.
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "(")
    (princ (symbol-name (class-constructor (object-class this))))
    (princ " \"")
    (princ (object-name-string this))
    (princ "\"\n")
    ;; Loop over all the public slots
    (let ((publa (aref cv class-public-a))
	  (publd (aref cv class-public-d))
	  (eieio-print-depth (1+ eieio-print-depth)))
      (while publa
	(let ((i (class-slot-initarg cl (car publa)))
	      (v (oref-engine this (car publa))))
	  (if (or (not i) (equal v (car publd)))
	      nil ;; Don't bother if it = default, or can't be initialized.
	    (princ (make-string (* eieio-print-depth 2) ? ))
	    (princ (symbol-name i))
	    (princ " ")
	    (let ((o (oref-engine this (car publa))))
	      (eieio-override-prin1 o))
	    (princ "\n")))
	(setq publa (cdr publa) publd (cdr publd)))
      (princ (make-string (* eieio-print-depth 2) ? )))
    (princ ")\n")))

(defun eieio-override-prin1 (thing)
  "Perform a prin1 on THING taking advantage of object knowledge."
  (cond ((object-p thing)
	 (object-write thing))
	((listp thing)
	 (eieio-list-prin1 thing))
	((class-p thing)
	 (princ (class-name thing)))
	(t (prin1 thing))))

(defun eieio-list-prin1 (list)
  "Display LIST where list may contain objects."
  (princ "(list ")
  (while list
    (if (object-p (car list))
	(object-write (car list))
      (prin1 (car list)))
    (insert " ")
    (setq list (cdr list)))
  (princ (make-string (* eieio-print-depth 2) ? ))
  (princ ")"))


;;; Unimplemented functions from CLOS
;;
(defun change-class (obj class)
  "Change the class of OBJ to type CLASS.
This may create or delete slots, but does not affect the return value
of `eq'."
  (error "Eieio: `change-class' is unimplemented"))


;;; Interfacing with edebug
;;
(defun eieio-edebug-prin1-to-string (object &optional noescape)
  "Display eieio OBJECT in fancy format.  Overrides the edebug default.
Optional argument NOESCAPE is passed to `prin1-to-string' when appropriate."
  (cond ((class-p object) (class-name object))
	((object-p object) (object-print object))
	(t (prin1-to-string object noescape))))

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec defmethod
	      (&define			; this means we are defining something
	       [&or name ("setf" :name setf name)]
	       ;; ^^ This is the methods symbol
	       [ &optional symbolp ]    ; this is key :BEFORE etc
	       list              ; arguments
	       [ &optional stringp ]    ; documentation string
	       def-body	                ; part to be debugged
	       ))
	    ;; The rest of the macros
	    (def-edebug-spec oref (form quote))
	    (def-edebug-spec oref-default (form quote))
	    (def-edebug-spec oset (form quote form))
	    (def-edebug-spec oset-default (form quote form))
	    (def-edebug-spec class-v form)
	    (def-edebug-spec class-p form)
	    (def-edebug-spec object-p form)
	    (def-edebug-spec class-constructor form)
	    (def-edebug-spec generic-p form)
	    (def-edebug-spec with-slots (form form def-body))
	    ;; I suspect this isn't the best way to do this, but when
	    ;; cust-print was used on my system all my objects
	    ;; appeared as "#1 =" which was not useful.  This allows
	    ;; edebug to print my objects in the nice way they were
	    ;; meant to with `object-print' and `class-name'
	    (defalias 'edebug-prin1-to-string 'eieio-edebug-prin1-to-string)
	    )
	  )

;;; Interfacing with imenu in emacs lisp mode
;;    (Only if the expression is defined)
;;
(if (eval-when-compile (boundp 'list-imenu-generic-expression))
(progn

(defun eieio-update-lisp-imenu-expression ()
  "Examine `lisp-imenu-generic-expression' and modify it to find `defmethod'."
  (let ((exp lisp-imenu-generic-expression))
    (while exp
      ;; it's of the form '( ( title expr indx ) ... )
      (let* ((subcar (cdr (car exp)))
	     (substr (car subcar)))
	(if (and (not (string-match "|method\\\\" substr))
		 (string-match "|advice\\\\" substr))
	    (setcar subcar
		    (replace-match "|advice\\|method\\" t t substr 0))))
      (setq exp (cdr exp)))))

(eieio-update-lisp-imenu-expression)

))
;;; Autoloading some external symbols
;;
(autoload 'eieio-browse "eieio-opt" "Create an object browser window" t)
(autoload 'eieio-describe-class "eieio-opt" "Describe CLASS defined by a string or symbol" t)
(autoload 'describe-class "eieio-opt" "Describe CLASS defined by a string or symbol" t)
(autoload 'eieiodoc-class "eieio-doc" "Create texinfo documentation about a class hierarchy." t)

(autoload 'eieio-customize "eieio-custom" "Create a custom buffer editing OBJ.")

(provide 'eieio)
;;; eieio ends here
