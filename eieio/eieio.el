;;; eieio.el -- Enhanced Implementation of Emacs Interpreted Objects
;;;             or maybe Eric's Implementation of Emacs Intrepreted Objects

;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.6
;;; RCS: $Id: eieio.el,v 1.7 1996/07/28 18:30:49 zappo Exp $
;;; Keywords: OO                                           
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;           
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Updates can be found at:
;;;    ftp://ftp.ultranet.com/pub/zappo

;;;
;;; Commentary:
;;;      
;;; EIEIO is a series of lisp routines which, if used, provide a class
;;; structure methodology vaguely which implements a small subset of
;;; CLOS, the Common Lisp Object System.  In addition, eieio also adds
;;; a few new features whose value has yet to prove themselves.
;;;
;;; Classes can inherit (singly) from other classes, and attributes
;;; can be multiply defined (but only one actual storage spot will be
;;; allocated) Attributes may be given initial values in the class
;;; definition.  Class methods (methods definied _IN_ a class) can be
;;; defined for each sub-class, or only for a parent class.  A method
;;; can also be defined outside a class in CLOS style where the
;;; parameters determine which implementation to use.
;;;
;;; Documentation for a class is updated as new class methods are
;;; added.  Since emacs documents all functions, and the class methods
;;; are not stored as named functions, their doc-strings are
;;; remembered, and stuck into the classes' doc string as these items
;;; change.  This makes loading slower, but does not affect run-time.
;;;

;;; Structural description of object vectors
;;;
;;; Class definitions shall be a stored vector:
;;; [ 'defclass name-of-class doc-string parent children
;;;   public-attributes public-defaults public-methods
;;;   private-attributes private-defaults private-methods 
;;;   initarg-tuples
;;;   method-implementations 
;;; ]
;;;
;;; name-of-class is a symbol used to store this class
;;; doc-string is the document string associated with this class
;;; parent is the parent class definition
;;; children is a list of children classes inheriting from us
;;; public-attributes is a list of public attributes
;;; public-defaults is a list of public default values
;;; public-methods is a list of public method names
;;; private-attributes is a list of private attributes
;;; private-defaults is a list of private default values
;;; private-methods is private list of methods
;;; initarg-tuples is a list of dotted pairs of (tag: . attribname)
;;; method-implementations is a vector of public/private implementations
;;;
;;; The vector can be accessed by referencing the named property list
;;; 'eieio-class-definition, or by using the function `class-v' on the
;;; class's symbol.  The symbol will reference itself for simplicity,
;;; thus a class will always evaluate to itself.

;;; Upon defining a class, the following functions are created (Assume
;;; 'moose is the class being created):
;;; moose     - Create an object of type moose
;;; moose-p   - t if object is type moose
;;;
;;; The allocated object will have the following form:
;;; [ 'object class-type name field1 field2 ... fieldn ]
;;; Where 'object marks it as an eieio object.
;;; Where class-type is the class definition vector
;;; Where name is some string assigned to said object to uniquely
;;;            identify it.
;;; Where the field# are the public then private attributes.  (Methods
;;;            are stored in the class defenitions.)

;;; Generic functions and methods get a single defined symbol
;;; representing the name of the method.  This method always calls the
;;; same thing: eieio-generic-call  In order to fathom which method to
;;; call, properties are attached to the method name of the form:
;;; :KEY-classname where :KEY is :BEFORE :PRIMARY or :AFTER.
;;; (:PRIMARY represents the middle, but is not needed when declaring
;;; you method) `classname' represents the name of the class for which
;;; this method is defined, or `generic' if it isn't defined.  In this
;;; way, all implementations can be quickly found and run.

;;;
;;; History
;;;
;;; 0.1  - first working copy: could run McDonald Farm example
;;; 0.2  - fixed defmethod: couldn't handle functions over 1 form
;;;        fixed documentation generator:
;;;        added default values
;;;        added fn to get parent class from a class def
;;; 0.3  - fixed ocall so that "this" is reset AFTER args are evaluated
;;;        now stores list of child classes in main class
;;;        added fn to call parent's version of running method
;;;        created default superclass for all objects which contains
;;;           the methods all objects should inherit, including
;;;           constructor, which will always be called at creation.
;;;           The constructor can be overriden by new classes.
;;;        added object browser to display the current class
;;;           inheritance tree.
;;;        Moved class vector information out of the variable slot,
;;;           and into a property.  This makes for prettier prints.
;;;        Moved old 'class-constructor and insted fset the 'class
;;;           variable (where 'class is the named class for `defclass'
;;;        Added edebug support
;;; 0.4  - Removed silly ":" stuff from defclass/defmethod
;;;        Made defclass map to CLOS version, with fewer keys, plus
;;;           some eieio specific ones.
;;;        Renamed defmethod to defclassmethod
;;;        Added CLOS functions `make-instance' and `slot-value'
;;; 0.5  - Finally figured out how to fix macros so they byte compile
;;;        Added CLOS style `defmethod' and `defgeneric'
;;; 0.6  - Fixed up the defgeneric default call to handle arguments better.
;;;        Added `call-next-method' (calls parent's method)
;;;        Fixed `make-instance' so it's no longer a macro
;;;        Fixed edebug hooks so they work better
;;;        Fixed storage duplication for inherited classes, which also
;;;           fixed default-value inheritance bug
;;;        Added some error messages to help in debugging programs using eieio.
;;;        Fixed class scoping troubles
;;;        Added `eieio-thing-to-string' which behaves like (format "%S" ..)
;;;           so objects and classes don't appear as symbols and vectors in 
;;;           your output.
;;;        Added `eieio-describe-class' command which creates a buffer
;;;           and displays the entire contents of a class or object.
;;;        Turned field names into properties on the class to reduce
;;;           the lookup times.  Old list is still there because it is
;;;           needed for generating sub-classes, and for doing
;;;           browsing things.
;;;        Changed Constructor and Destructor into methods, instead of
;;;           classmethods

;;;
;;; Variable declarations.  These variables are used to hold the call
;;; state when using methods.
;;;

(defvar this nil
  "Inside a method, this variable is the object in question.  DO NOT
SET THIS YOURSELF unless you are trying to simulate friendly fields.")

(defvar scoped-class nil
  "This is set when a method is defined so we know we are allowed to
check private parts. DO NOT SET THIS YOURSELF!")

(defvar eieio-generic-args nil
  "When calling methods or generics, specifies the formal parameter
list which has to be faked around the existing emacs interpreter")

;; This is a bootstrap for eieio-default-superclass so it has a value
;; while it is being built itself.
(defvar eieio-default-superclass nil)

(defconst class-parent 3 "Class parent field")
(defconst class-children 4 "Class children class field")
(defconst class-public-a 5 "Class public attribute index")
(defconst class-public-d 6 "Class public attribute defaults index")
(defconst class-public-m 7 "Class pubic method index")
(defconst class-private-a 8 "Class private attribute index")
(defconst class-private-d 9 "Class private attribute defaults index")
(defconst class-private-m 10 "Class private method index")
(defconst class-initarg-tuples 11 "Class initarg tuples list")
(defconst class-methods 12 "Class methods index")
(defconst class-num-fields 13 "Number of fields in the class definition object")

(defconst method-before 0 "Index into :BEFORE tag on a method")
(defconst method-primary 1 "Index into :PRIMARY tag on a method")
(defconst method-after 2 "Index into :AFTER tag on a method")
(defconst method-generic-before 3 "Index into generic :BEFORE tag on a method")
(defconst method-generic-primary 4 "Index into generic :PRIMARY tag on a method")
(defconst method-generic-after 5 "Index into generic :AFTER tag on a method")
(defconst method-num-fields 6 "Number of indexes into a method's vector")


;;;
;;; Defining a new class
;;;
(defmacro defclass (name superclass fields doc-string)
  "Define NAME as a new class, defived from SUPERCLASS which is a list
of superclasses to inherit from, with FIELDS being the fields residing
in that class definition.  NOTE: Currently only one field may exist in
SUPERCLASS as multiple inheritance is not yet supported.  Supported
tags are:

  :initform   - initializing form
  :initarg    - tag used during initialization
  :accessor   - tag used to create a function to access this field
  :protection - non-nil means a private slot (only accessable in a classmethod)
  :method     - non-nil means classify this as a classmethod, not a slot

  You can have multiple tags per slot, though some specifiers can't be
combinded (for instance :method and :initarg make no sense together)
 "
  (list 'defclass-engine (list 'quote name) (list 'quote superclass)
	(list 'quote fields) doc-string))

(defun defclass-engine (cname superclass fields doc-string)
  "Define CNAME as a new class, with FIELDS being the fields residing
in that class definition.  See defclass for more information"

  (if (not (symbolp cname)) (signal 'wrong-type-argument '(symbolp cname)))
  (if (not (listp superclass)) (signal 'wrong-type-argument '(listp superclass)))
  (let* ((pname (if superclass (car superclass) nil))
	 (newc (make-vector class-num-fields nil)) 
	 (private nil)
	 (tmp nil)
	 (clearparent nil))
    (aset newc 0 'defclass)
    (aset newc 1 cname)
    (aset newc 2 doc-string)
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
    (if (aref newc 3)
	(progn
	  (aset newc class-private-m (copy-sequence (aref (class-v (aref newc class-parent)) class-private-m)))
	  (aset newc class-private-a (copy-sequence (aref (class-v (aref newc class-parent)) class-private-a)))
	  (aset newc class-private-d (copy-sequence (aref (class-v (aref newc class-parent)) class-private-d)))
	  (aset newc class-public-m (copy-sequence (aref (class-v (aref newc class-parent)) class-public-m)))
	  (aset newc class-public-a (copy-sequence (aref (class-v (aref newc class-parent)) class-public-a)))
	  (aset newc class-public-d (copy-sequence (aref (class-v (aref newc class-parent)) class-public-d)))
	  (aset newc class-initarg-tuples (copy-sequence (aref (class-v (aref newc class-parent)) class-initarg-tuples)))))

    ;; Query each field in the declaration list and mangle into the
    ;; class structure I have defined.
    (while fields
      (let* ((field1 (car fields))
	     (name (car field1))
	     (field (cdr field1))
	     (acces (car (cdr (member ':accessor field))))
	     (init (car (cdr (member ':initform field))))
	     (initarg (car (cdr (member ':initarg field))))
	     (prot (car (cdr (member ':protection field))))
	     (meth (car (cdr (member ':method field))))
	     )
	(if meth
	    ;; Here we are defining a method
	    (progn
	      (if (eq prot 'private)
		  ;; define method as private
		  (progn
		    (aset newc class-private-m
			  (append (aref newc class-private-m)
				  (list name)))
		    )
		;; define method as public
		(aset newc class-public-m
		      (append (aref newc class-public-m)
			      (list name)))
		))
	  (let* ((-a (if (eq prot 'private) class-private-a class-public-a))
		 (-d (if (eq prot 'private) class-private-d class-public-d))
		 (-al (aref newc -a))
		 (-dl (aref newc -d))
		 (np (member name -al))
		 (dp (if np (nthcdr (- (length -al) (length np)) -dl) nil)))
	    (if np
		(progn
		  ;; If we have a repeat, only update the initarg...
		  (setcar dp init)
		  )
	      (aset newc -a (append -al (list name)))
	      (aset newc -d (append -dl (list init))))
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
	      (let ((newfn acces))
		(fset newfn (list
			     'lambda '(obj) 
			     (format "Retrieves the field `%s' from object of class `%s'"
				     name cname)
			     (list 'oref-engine 'obj (list
						      'quote
						      name))))
		;; If defsetf is loaded, then create the setf definition we want
		(if (fboundp 'defsetf)
		    (eval
		     (list
		      'defsetf newfn '(node) '(store)
		      '(oset-engine node 'name store))))
		)
	    )
	  )
	)
      (setq fields (cdr fields)))

    ;; Create the vector of implementations
    (aset newc class-methods 
	  (make-vector (+ (length (aref newc class-public-m))
			  (length (aref newc class-private-m))) nil))

    ;; Store this forever.  Give it a variable type (The class
    ;; definition symbol), A property (the vector),
    ;; a function type (default creator type)
    ;; and a doc-string
    
    (set cname cname)
    (put cname 'variable-documentation doc-string)
    (put cname 'eieio-class-definition newc)

    ;; Now attach all field symbols onto `cname' such that they are
    ;; properties with the value being the index into the vector which
    ;; contains the actual data belonging to that field
    (let ((cnt 0)
	  (pubsyms (aref newc class-public-a))
	  (privsyms (aref newc class-private-a)))
      (while pubsyms
	(put cname (intern (format "eieio-public-%s" (car pubsyms))) cnt)
	(setq cnt (1+ cnt))
	(setq pubsyms (cdr pubsyms)))
      (while privsyms
	(put cname (intern (format "eieio-private-%s" (car privsyms))) cnt)
	(setq cnt (1+ cnt))
	(setq privsyms (cdr privsyms)))
      )
	  
    ;; Create the constructor function
    (fset cname
	  (list 'lambda (list 'newname '&rest 'fields)
		(format "Create a new object with name NAME of class type %s" cname)
		(list 
		 'let (list (list 'no (list 'make-vector 
					    (+ (length (aref newc class-public-a))
					       (length (aref newc class-private-a))
					       3)
					    nil)))
		 '(aset no 0 'object)
		 (list 'aset 'no 1 cname)
		 '(aset no 2 newname)
		 '(constructor no fields)
		 'no)))

    ;; Create the test function
    (let ((csym (intern (concat (symbol-name cname) "-p"))))
      (fset csym
	    (list 'lambda (list 'obj)
		  (format "Test OBJ to see if it an object of type %s" cname)
		  (list 'same-class-p 'obj newc))))
    ;; if this is a superclass, clear out parent (which was set to the
    ;; default superclass eieio-default-superclass)
    (if clearparent (aset newc class-parent nil))
    ;; Return our new class object
    newc
    ))

;;; CLOS style implementation of object creators.
;;;
(defun make-instance (class &rest initargs)
  "Make a new instance of CLASS with initilaization of some parts with
INITARGS"
  (let ((cc (class-constructor class)))
    (apply cc class initargs)))

;;;
;;; Class Methods (methods stored in a class with no external symbol
;;;                of thier own)
;;;
(defmacro defclassmethod (method class args &rest body)
  "Define a function method for METHODEF with ARGS and BODY.  It
returns a tuple `(class method)'"
  (list 'defclassmethod-engine 
	(list 'quote method) 
	(list 'quote class) 
	(list 'quote args)
	(list 'quote body)))

(defun defclassmethod-engine (method class args body)
  "Define a function method for METHOD in CLASS with ARGS and BODY. It
returns a tuple `(class method)'"
  (let* ((cl class)
	 (mt method)
	 ;; Make sure the scoped class is set while looking up the
	 ;; method becase we can DEFINE a method without being
	 ;; within it's method.  (Else could prove problematic.)
	 (scoped-class cl)
	 (mi (eieio-method-name-index cl mt))
	 (save nil))
    (if (not mi) (error "Method %s does not exist in class %s" 
			mt (class-name cl)))
    (setq save 
	  (aset (aref (class-v cl) class-methods) mi
		(append (list 'lambda args) body)))
    ;; rebuild the doc string...
    (eieio-rebuild-doc-string (symbol-value cl))
    (list cl mt)))

;;;
;;; CLOS methods and generics
;;;
(defmacro defgeneric (method args &optional doc-string)
  "Creates a generic function, which is called whenever a more
specific method is requested.  A generic function has no body, as
it's purpose is to decide which method body is apropriate to use.  Use
`defmethod' to create methods, and it calls defgeneric for you."
  (list 'defgeneric-engine
	(list 'quote method)
	(list 'quote args)
	doc-string))

(defun defgeneric-engine (method args doc-string)
  "Engine part to defgeneric macro"
  (let ((lambda-form
	 (list 'lambda '(&rest local-args)
	       doc-string
	       (list 'let (list (list 'eieio-generic-args 
				      (list 'quote args)))
		     ;; Call generic master with method as first
		     ;; argument, and args as continuing arguments
		     (list 'eieio-generic-call 
			   (list 'quote method) 
			   'local-args)))))
    (if (and (fboundp method) (not (generic-p method)))
	(error "You cannot create a generic/method over an existing symbol"))
    (fset method lambda-form)
    'method))

(defmacro defmethod (method &rest args)
  "Creates a new METHOD through `defgeneric' and adds the apropriate
qualifiers to the symbol METHOD.  ARGS lists any keys (such as :BEFORE
or :AFTER, it's checked for the arglst, and docstring, and eventually
the body, such as: 

  (defmethod mymethod [:BEFORE | :AFTER] (args)
    doc-string
    body)"
  (list 'defmethod-engine
	(list 'quote method)
	(list 'quote args)))

(defun defmethod-engine (method args)
  "Workpart of the defmethod macro"
  (let ((key nil) (body nil) (bindsym nil) (firstarg nil) (typesym nil)
	(argfix nil) loopa)
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
	(defgeneric-engine method args 
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
    (eieiomt-add method (append (list 'lambda (reverse argfix)) body)
		 key (nth 1 firstarg)))
  method)

;;;
;;; Get/Set slots in an object.  `setf' should be used, but that
;;;                              requires that `cl' be loaded.
;;;
(defmacro oref (obj field)
  "Macro calling oref-engine with the quote inserted before field."
  (list 'oref-engine obj (list 'quote field)))

(defun oref-engine (obj field)
  "Return the value in OBJ at FIELD in the object vector."
  (let ((c (eieio-field-name-index (aref obj 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj)))
    (aref obj c)))

(defalias 'slot-value 'oref-engine)

(if (featurep 'cl)
    (progn
      (defsetf slot-value (obj field) (store)
	(list 'oset-engine obj field store))
      (defsetf oref-engine (obj field) (store)
	(list 'oset-engine obj field store))
      (defsetf oref (obj field) (store) 
	(list 'oset-engine obj 'field store))
      ))

;; This alias is needed so that functions can be written
;; for defaults, but still behave like lambdas.
(defalias 'lambda-default 'lambda)

(defmacro oref-default (obj field)
  "Macro calling oref-default-engine with the quote inserted before field."
  (list 'oref-default-engine obj (list 'quote field)))

(defun oref-default-engine (obj field)
  "Return the default value in OBJ at FIELD in the object vector.
This value is found in the objects class structure and does not
represent the actual stored value."
  (let ((c (eieio-field-name-index (aref obj 1) field))
	(nump (length (aref (class-v (aref obj 1)) class-public-a))))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj)))
    (let ((val (if (< c (+ 3 nump))
		   (nth (- c 3) (aref (class-v (aref obj 1)) class-public-d))
		 (nth (- c nump 3) (aref (class-v (aref obj 1)) class-private-d)))))
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

(defmacro oset (obj field value)
  "Macro calling oset-engine with the quote inserted before field."
  (list 'oset-engine obj (list 'quote field) value))

(defun oset-engine (obj field value)
  "Set the value in OBJ at FIELD to be VALUE, and return VALUE."
  (let ((c (eieio-field-name-index (aref obj 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj)))
    (aset obj c value)))

(defmacro ocall (obj method &rest args)
  "For the given OBJ, call the METHOD associated with it using ARGS as
the arguments used to call it."
  (let ((tempvar (make-symbol "objval")))
    (list 'let (list (list tempvar obj))
	  (list 'ocall-engine tempvar (list 'object-class tempvar)
		(list 'quote method) (list 'quote args)))))

(defmacro ocall-parent (&rest args)
  "For the currently active object (stored in THIS) call the currently
running method (stored conveniently in METHOD) on the parent class of
the current methods class (currently stored in scoped-class)"
  (list 'ocall-engine this (list 'class-parent 'scoped-class) 'method
	(list 'quote args)))

(defun ocall-engine (obj class method args)
  "Recursive method finding function caller.  For OBJ, which better be
inheriting something from CLASS (but isn't tested for) call METHOD
with the list of arguments ARGS."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (let* ((newargs nil)
	 (mi (let ((scoped-class class)) ;need class scoped here
	       (eieio-method-name-index class method)))
	 (meth (if mi (aref (aref (class-v class) class-methods) mi) nil)))
    (if meth
	(progn
	  ;; evaluate the arguments BEFORE changing THIS and SCOPED CLASS
	  (while args
	    (setq newargs (cons (eval (car args)) newargs)
		  args (cdr args)))
	  ;; fix the order
	  (setq newargs (reverse newargs))
	  ;; set context of call
	  (let ((this obj)
		(scoped-class class))
	    (apply meth newargs)))
      ;;(eval (append (list 'funcall meth) args))
      ;; Check mi too.  This is a short-cut in the search we don't
      ;; have the function part way down the parental tree
      (if (and (class-parent class) mi)
	  (ocall-engine obj (class-parent class) method args)
	(if (and (not (class-parent class)) 
		 (not (equal class eieio-default-superclass))
		 mi)
	    (ocall-engine obj eieio-default-superclass method args)
	  (error "Method %s in object %s is virtual"
		 method (object-name obj)))))))


;;;
;;; Simple generators, and query functions.  None of these would do
;;; well embedded into an object.
;;;
(defmacro class-v (class) "Internal: Returns the class vector from the CLASS symbol"
  ;(if (not (symbolp class)) (signal 'wrong-type-argument (list 'symbolp class)))
  (list 'get class ''eieio-class-definition))

(defun class-p (class) "Return t if CLASS is a valid class vector."
  (and (symbolp class) 
       (let ((cv (get class 'eieio-class-definition)))
	 (and (vectorp cv) (equal (aref cv 0) 'defclass)))))

(defun object-p (obj) "Return t if OBJ is an OBJECT vector."
  (and (vectorp obj) (equal (aref obj 0) 'object) (class-p (aref obj 1))))

(defun class-name (class) "Return a lisp like symbol name for object OBJ"
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (format "#<class %s>" (symbol-name class)))

(defun class-constructor (class) 
  "Return the symbol representing the constructor of that class"
  (aref (class-v class) 1))

(defun object-name (obj) "Return a lisp like symbol string for object OBJ"
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (format "#<%s %s>" (symbol-name (object-class obj)) (aref obj 2)))

(defun object-class (obj) "Return the class struct defining OBJ"
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (aref obj 1))
  
(defun object-class-name (obj) "Return a lisp like symbol name for OBJ's class"
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (class-name (aref obj 1)))

(defun class-parent (class) "Return parent class to CLASS. (overload of variable)"
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (aref (class-v class) class-parent))

(defun same-class-p (obj class) "Return t if OBJ is of class-type CLASS"
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (and (object-p obj) (equal (aref obj 1) class)))

(defun obj-of-class-p (obj class) "Return t if OBJ inherits anything from CLASS"
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (child-of-class-p (aref obj 1) class))

(defun child-of-class-p (child class) "Return t if CHILD inherits anything from CLASS"
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (class-p child)) (signal 'wrong-type-argument (list 'class-p child)))
  (or (equal child class) 
      (and (aref (class-v child) 3) (child-of-class-p (aref (class-v child) 3) class))))

(defun generic-p (method)
  "Return `t' if symbol METHOD is a generic function.  Only methods
have the symbol `eieio-method-tree' as a property (which contains a
list of all bindings to that method type.)"
  (and (fboundp method) (get method 'eieio-method-tree)))


;;;
;;; EIEIO internal search functions
;;;

(defun eieio-field-name-index (class field)
  "In OBJ find the index of the named FIELD."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let ((pubs (get class (intern (format "eieio-public-%s" field)))))
    (if (integerp pubs)
	(+ 3 pubs)
      (if (child-of-class-p class scoped-class)
	  (let ((pris (get class (intern (format "eieio-private-%s" field)))))
	    (if (integerp pris)
		(+ 3 pris)
	      nil))
	nil))))

(defun eieio-method-name-index (class method)
  "Return the index for a CLASS where a METHOD resides"
  (if (not (class-p class)) (signal 'wrong-type-argument '(class-p class)))
  (if (not (symbolp method)) (signal 'wrong-type-argument '(symbolp method)))
  (let ((c 0) (l (aref (class-v class) class-public-m)))
    ;; Check out the public symbols
    (while (and l (not (equal method (car l))))
      (setq c (1+ c))
      (setq l (cdr l)))
    (if (not l)
	(if (child-of-class-p class scoped-class)
	    (progn
	      (setq l (aref (class-v class) class-private-m))
	      (while (and l (not (equal method (car l))))
		(setq c (1+ c))
		(setq l (cdr l)))
	      (if (not l)
		  l
		c))
	  nil)
      c)))

;;;
;;; CLOS generics internal function handling
;;;
(defvar eieio-generic-call-methodname nil
  "When using `call-next-method' this provides a context on how to do it.")
(defvar eieio-generic-call-arglst nil
  "When using `call-next-method' this provides a context on what to use
for parameters")

(defun eieio-generic-call (method args)
  "Do the hard work of looking up which method to call out of all
available methods which may be programmed in."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (forms nil) (mclass nil)  (lambdas nil)
	(eieio-generic-call-methodname method)
	(eieio-generic-call-arglst args))
    ;; get a copy 
    (setq newargs args)
    ;; lookup the forms to use
    (if (object-p (car newargs))
	(setq mclass (object-class (car newargs))))
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
    (let ((rval nil))
      (while lambdas
	(if (car lambdas)
	    (let ((scoped-class (cdr (car lambdas))))
	      (setq rval (apply (car (car lambdas)) newargs))))
	(setq lambdas (cdr lambdas)))
      rval)))

(defun call-next-method ()
  "When inside a call to a method belonging to some object, call the
method belong to the parent class"
  (if (not scoped-class)
      (error "call-next-method not called within a class specific method"))
  (let ((newargs eieio-generic-call-arglst) (lambdas nil)
	(mclass (class-parent scoped-class)))
    ;; lookup the form to use for the PRIMARY object for the next level
    (setq lambdas (eieio-generic-form eieio-generic-call-methodname
				      method-primary mclass))
    ;; Setup calling environment, and apply arguments...
    (let ((scoped-class (cdr lambdas)))
      (apply (car lambdas) newargs))))

;;;
;;; eieio-method-tree : eieiomt-
;;;
;;; Stored as eieio-method-tree in property list of a generic method
;;;
;;; (eieio-method-tree . [BEFORE PRIMARY AFTER 
;;;                       genericBEFORE genericPRIMARY genericAFTER])
;;;    where the association is a vector.
;;;    (aref 0  -- all methods classified as :BEFORE
;;;    (aref 1  -- all methods classified as :PRIMARY
;;;    (aref 2  -- all methods classified as :AFTER
;;;    (aref 3  -- a generic classified as :BEFORE
;;;    (aref 4  -- a generic classified as :PRIMARY
;;;    (aref 5  -- a generic classified as :AFTER
;;;
;;; Each list of methods is stored as follows:
;;;
;;; ( ( class . function ) ( class ... ))
;;;
;;; The elts 3-5 are mearly function bodies
;;;

(defun eieiomt-add (method-name method tag class)
  "Add to METHOD-NAME the METHOD with associated TAG a function
associated with CLASS"
  (if (or (>= tag method-num-fields) (< tag 0))
      (error "eieiomt-add: method tag error!"))
  (let ((emtv (get method-name 'eieio-method-tree)))
    (if (not emtv)
	(setq emtv (put method-name 'eieio-method-tree 
			(make-vector method-num-fields nil))))
    ;; only cons new cells on if it doesn't already exist!
    (if (assq class (aref emtv tag))
	(setcdr (assq class (aref emtv tag)) method)
      (aset emtv tag (cons (cons class method) (aref emtv tag))))
    ))

(defun eieiomt-get (method-name tag class)
  "Get the method implementation from METHOD-NAME of the correct TAG
matching CLASS"
  (if (>= tag method-num-fields) (< tag 0)
    (error "eieiomt-add: method tag error!"))
  (let ((emtv (get method-name 'eieio-method-tree)))
    (if (not emtv) 
	nil
      (cdr (assq class (aref emtv tag))))))

(defun eieio-generic-form (method tag class)
 "Return the lambda form belonging to METHOD using TAG based upon
CLASS.  If CLASS is not a class then use `generic' instead.  If class
has no form, but has a parent class, then trace to that parent class"

 (let ((emtl (aref (get method 'eieio-method-tree) (if class tag (+ tag 3)))))
   (if (class-p class)
     (let ((ov nil))
       (while (and class (not ov) emtl)
	 (setq ov (assq class emtl))
	 (if ov
	     (setq ov (cons (cdr ov) (aref (class-v class) 1)))
	   (if (class-parent class)
	       (setq class (class-parent class))
	     (if (eq class 'eieio-default-superclass)
		 (setq class nil)
	       (setq class 'eieio-default-superclass)))))
       ;; return the created dotted pair
       ov)
     ;; for a generic call, what is a list, is the function body we want.
     (if emtl
	 (cons emtl nil)
       nil))))

;;;
;;; Way to assign fields based on a list.  Used for constructors, or
;;; even resetting an object at run-time
;;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all fields to their defaults.  If
SET-ALL is non-nil, then when a default is nil, that value is reset.
If SET-ALL is nil, the fields are only reset if the default is not
nil."
  (let ((scoped-class (aref obj 1))
	(pub (aref (class-v (aref obj 1)) class-public-a))
	(priv (aref (class-v (aref obj 1)) class-private-a)))
    (while pub
      (let ((df (oref-default-engine obj (car pub))))
	(if (or df set-all)
	    (oset-engine obj (car pub) df)))
      (setq pub (cdr pub)))
    (while priv
      (let ((df (oref-default-engine obj (car priv))))
	(if (or df set-all)
	    (oset-engine obj (car priv) df)))
      (setq priv (cdr priv)))))

(defun eieio-initarg-to-attribute (class initarg)
  "Converts INITARG to the actual attribute name so we can set it during
instantiation.  If there is no translation, pass it in directly (so 
we can cheat if need be.. May remove that later..."
  (let ((tuple (assoc initarg (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(cdr tuple)
      initarg)))

(defun eieio-set-fields (obj fields)
  "Set the fields of OBJ with the list FIELDS which is a list of
name/value pairs.  Called from the constructor routine."
  (let ((scoped-class (aref obj 1)))
    (while fields
      (let ((rn (eieio-initarg-to-attribute (object-class obj) (car fields))))
	(oset-engine obj rn (car (cdr fields))))
      (setq fields (cdr (cdr fields))))))

(defun eieio-rebuild-doc-string (class)
  "Look in CLASS for it's stored doc-string, and the doc string of
it's methods.  Use this to set the variable 'CLASSes doc string for
viewing by apropos, and describe-variables, and the like."
  (if (not (class-p class)) (signal 'wrong-type-argument '(class-p class)))  
  (let ((newdoc (aref (class-v class) 2))
	(methods (aref (class-v class) class-public-m))
	(meth nil)
	(mdoc nil)
	(index 0))
    (while methods
      (setq meth (aref (aref (class-v class) class-methods) index))
      (setq mdoc nil)
      (if meth
	  (progn
	    (setq mdoc (nth 2 meth))
	    (if (stringp mdoc)
		(setq newdoc (concat newdoc
				     (format "\n\nMethod: %s\n" 
					     (car methods))
				     mdoc))
	      (setq newdoc (concat newdoc
				   (format "\n\nMethod: %s\nUndocumented"
					   (car methods)))))))
      (setq methods (cdr methods))
      (setq index (1+ index)))
    ;; only store this on the variable.  The doc-string in the vector
    ;; is ONLY the top level doc for this class.  The value found via
    ;; emacs needs to be more descriptive.
    (put class 'variable-documentation newdoc)))

;;;
;;; We want all object created by EIEIO to have some default set of
;;; behavious so we can create object utilities, and allow various
;;; types of error checking.  To do this, create the default EIEIO
;;; class, and when no parent class is specified, use this as the
;;; default.  (But don't store it in the other classes as the default,
;;; allowing for transparent support.)
;;;

(if (class-p 'eieio-default-superclass)
    nil ; don't rebuild these objects.

  (defclass eieio-default-superclass nil
    nil
    "Default class used as parent class for superclasses.  It's
fields are automatically adopted by such superclasses but not stored
in the `parent' field.  When searching for attributes or methods, when
the last parent is found, the search will recurse to this class.")
)

;;; We want our superclass to define it's own methods.
(defmethod constructor ((this eieio-default-superclass) &optional fields)
    "Constructor for filling in attributes when constructing a new
class."
    ;; Load in the defaults
    (eieio-set-defaults this t)
    ;; Set fields for ourselves from the list of fields
    (eieio-set-fields this fields)
    )

(defmethod destructor ((this eieio-default-superclass) &rest params)
  "Destructor for cleaning up any dynamic links to our object."
  ;; No cleanup... yet.
  )


;;;
;;; Now, for convenience, we should have a browser, to aid people in
;;; debugging their object oriented emacs lisp programs...
;;;

(defun eieio-browse (&optional root-class)
  "Create an object browser window which shows all objects starting
with root-class, or eieio-default-superclass if none is given."
  (interactive (if current-prefix-arg
		   (list (read (read-string "Class to build tree from:")))
		 nil))
  (if (not root-class) (setq root-class 'eieio-default-superclass))
  (if (not (class-p root-class)) (signal 'wrong-type-argument (list 'class-p root-class)))
  (display-buffer (get-buffer-create "*EIEIO OBJECT BROWSE*") t)
  (save-excursion
    (set-buffer (get-buffer "*EIEIO OBJECT BROWSE*"))
    (erase-buffer)
    (goto-char 0)
    (eieio-browse-tree root-class "" "")
    ))

(defun eieio-browse-tree (this-root prefix ch-prefix)
  "Recursive part of browser, draws the children of the given class on
the screen."
  (if (not (class-p (eval this-root))) (signal 'wrong-type-argument (list 'class-p this-root)))
  (let ((myname (symbol-name this-root))
	(chl (aref (class-v this-root) class-children))
	(fprefix (concat ch-prefix "  +--"))
	(mprefix (concat ch-prefix "  |  "))
	(lprefix (concat ch-prefix "     ")))
    (insert prefix)
    (if (not (and window-system (fboundp 'make-overlay)))
	(insert myname)
      (let ((no (make-overlay (point) (progn (insert myname) (point)))))
	(overlay-put no 'face 'bold)))
    (insert "\n")
;   This didn't really do anything except clutter the screen
;   (if chl
;	(if (= (length chl) 1)
;	    (insert (format " -- [1 child]\n"))
;	  (insert (format " -- [%d children]\n" (length chl))))
;     (insert (format " -- [No children]\n"))))
    (while (cdr chl)
      (eieio-browse-tree (car chl) fprefix mprefix)
      (setq chl (cdr chl)))
    (if chl
	(eieio-browse-tree (car chl) fprefix lprefix))
    ))

(defun eieio-thing-to-string (thing)
  "Convert THING into a string.  If THING is an object, use
`object-name' instead, if THING is a class, then use `class-name'
instead, if THING is a list of stuff, try those."
  (if (object-p thing) (object-name thing)
    (if (class-p thing) (class-name thing)
      (if (and thing (listp thing))
	  (let ((op "("))
	    (while thing
	      (setq op (concat op " " (eieio-thing-to-string (car thing))))
	      (setq thing (cdr thing)))
	    (concat op ")"))
	(format "%S" thing))))
  )

(defun eieio-describe-class (class)
  "Describe a CLASS defined by a string or symbol.  If CLASS is actually
an object, then also display current values of that obect."
  (interactive "sClass: ")
  (switch-to-buffer (get-buffer-create "*EIEIO OBJECT DESCRIBE*"))
  (erase-buffer)
  (let* ((cv (cond ((stringp class) (class-v (read class)))
		   ((symbolp class) (class-v class))
		   ((object-p class) (class-v (object-class class)))
		   (t (error "Can't find class info from parameter"))))
	 (this (if (object-p class) class this))
	 (scoped-class (if (object-p class) (object-class class) scoped-class))
	 (priva (aref cv class-private-a))
	 (publa (aref cv class-public-a))
	 (privd (aref cv class-private-d))
	 (publd (aref cv class-public-d)))
    (insert "Description of")
    (if (object-p class)
	(insert " object `" (aref class 2) "'"))
    (insert " class `" (symbol-name (aref cv 1)) "'\n")
    (insert "\nPRIVATE\n")
    (put-text-property (point)
		       (progn (insert "Field:\t\t\tdefault value"
				      (if (object-p class)
					  "\t\tCurrent Value" ""))
			      (point))
		       'face 'underline)
    (insert "\n")
    (while priva
      (let ((dvs (eieio-thing-to-string (car privd))))
	(insert (symbol-name (car priva)) "\t" 
		(if (< (length (symbol-name (car priva))) 8) "\t" "")
		(if (< (length (symbol-name (car priva))) 16) "\t" "")
		dvs
		(if (object-p class)
		    (concat
		     "\t"
		     (if (< (length dvs) 8) "\t" "")
		     (if (< (length dvs) 16) "\t" "")
		     (eieio-thing-to-string (oref-engine class (car priva))))
		  "")
		"\n"))
      (setq priva (cdr priva)
	    privd (cdr privd)))
    (insert "\nPUBLIC\n")
    (put-text-property (point)
		       (progn (insert "Field:\t\t\tdefault value"
				      (if (object-p class)
					  "\t\tCurrent Value" ""))
			      (point))
		       'face 'underline)
    (insert "\n")
    (while publa
      (let ((dvs (eieio-thing-to-string (car publd))))
	(insert (symbol-name (car publa)) "\t"
		(if (< (length (symbol-name (car publa))) 8) "\t" "")
		(if (< (length (symbol-name (car publa))) 16) "\t" "")
		dvs
		(if (object-p class)
		    (concat
		     "\t"
		     (if (< (length dvs) 8) "\t" "")
		     (if (< (length dvs) 16) "\t" "")
		     (eieio-thing-to-string (oref-engine class (car publa))))
		  "")
		"\n"))
      (setq publa (cdr publa)
	    publd (cdr publd)))))

;; Now lets support edebug in reguard to defmethod forms.
;; reguardless of if edebug is running, this hook is re-eveluated so
;; this is a clever way for edebug to allow us to add hooks
;; dynamically
(add-hook 'edebug-setup-hook
	  (lambda () 
	    (def-edebug-spec defclassmethod
	      (symbolp	   ; This is a symbol
	       symbolp     ; this is the class's symbol
	       lambda-list ; arguments
	       [ &optional stringp ] ; documentation string
	       def-body	   ; part to be debugged
	       )))
	  )

(add-hook 'edebug-setup-hook
	  (lambda () 
	    (def-edebug-spec defmethod
	      (symbolp	                ; This is the methods symbol
	       [ &optional symbolp ]    ; this is key :BEFORE etc
	       list              ; arguments
	       [ &optional stringp ]    ; documentation string
	       def-body	                ; part to be debugged
	       )))
	  )

;;; end of lisp
(provide 'eieio)
