;;; eieio.el -- Enhanced Implementation of Emacs Interpreted Objects
;;;             or maybe Eric's Implementation of Emacs Intrepreted Objects

;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.4
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

;;;
;;; Commentary:
;;;
;;; EIEIO is a series of lisp routines which, if used, provide a class
;;; structure methodology vaguely reminicent of c++ style class
;;; definitions in the emacs lisp setting.
;;;
;;; Classes can inherit (singly) from other classes, and attributes
;;; can be multiply defined (but only one actual storage spot will be
;;; allocated) Attributes may be given initial values in the class
;;; definition.  Methods can be defined for each sub-class, or only
;;; for a parent class.
;;;
;;; Documentation for a class is updated as new methods are added.
;;; Since emacs documents all functions, and the methods are not
;;; stored as named functions, their doc-strings are remembered, and
;;; stuck into the classes' doc string as these items change.  This
;;; makes loading slower, but does not affect run-time.
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
;;; public-methods is a list of public method names
;;; private-attributes is a list of private attributes
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

;;;
;;; Variable declarations.  These variables are used to hold the call
;;; state when using methods.
;;;

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

(defvar this nil
  "Inside a method, this variable is the object in question.  DO NOT
SET THIS YOURSELF unless you are trying to simulate friend: fields.")

(defvar scoped-class nil
  "This is set when a method is defined so we know we are allowed to
check private parts. DO NOT SET THIS YOURSELF!")

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
	  (if (eq prot 'private)
	      ;; define attribute as private
	      (progn
		(aset newc class-private-a
		      (append (aref newc class-private-a)
			      (list name)))
		(aset newc class-private-d
		      (append (aref newc class-private-d)
			      (list init)))
		)
	    ;; define attributes as public
	    (aset newc class-public-a
		  (append (aref newc class-public-a)
			  (list name)))
	    (aset newc class-public-d
		  (append (aref newc class-public-d)
			  (list init)))
	    )
	  ;; public and privates both can install new initargs
	  (if initarg
	      (progn
		;; intern the symbol so we can use it blankly
		(set initarg initarg)
		;; set the new arg
		(aset newc class-initarg-tuples
		      (append (aref newc class-initarg-tuples)
			      (list (cons initarg name))))))
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
		 '(ocall no constructor fields)
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

(defmacro make-instance (class &rest initargs)
  "Make a new instance of CLASS with initilaization of some parts with
INITARGS"
  (list 'make-instance-engine class (list 'quote initargs)))

(defun make-instance-engine (class initargs)
  "Make a new instance of CLASS with initilaization of some parts with
INITARGS"
  (let ((cc (class-constructor class)))
    (apply cc class initargs)))

;;;
;;; Modification and querying of objects
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

(defmacro oref (obj field)
  "Macro which translates an OREF directly into an AREF.  You can't change
a structure without re-evaluating all functions which reference that object!"
  (let* ((obj-val (eval obj))
	 (c (eieio-field-name-index (aref obj-val 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj-val)))
    (list 'aref obj c)))

;(defmacro oref-old (obj field)
;  "Macro calling oref-engine with the quote inserted before field."
;  (list 'oref-engine obj (list 'quote field)))

;; This is the old style which happens to match slot-value from CLOS
(defun oref-engine (obj field)
  "Return the value in OBJ at FIELD in the object vector."
  (let ((c (eieio-field-name-index (aref obj 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj)))
    (aref obj c)))
(defalias 'slot-value 'oref-engine)

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
      (if (or (and (listp val) (equal (car val) 'lambda))
	      (and (symbolp val) (fboundp val)))
	  (let ((this obj))
	    (funcall val))
	val))))

(defmacro oset (obj field value)
  "Set the value in OBJ at FIELD to be VALUE, and return VALUE."
  (let* ((obj-val (eval obj))
	 (c (eieio-field-name-index (aref obj-val 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj-val)))
    (list 'aset obj c value)))

; old macros for oset.. now calculations occur in macro speeding up runtime
;(defmacro oset-old (obj field value)
;  "Macro calling oset-engine with the quote inserted before field."
;  (list 'oset-engine obj (list 'quote field) value))
(defun oset-engine (obj field value)
  "Set the value in OBJ at FIELD to be VALUE, and return VALUE."
  (let ((c (eieio-field-name-index (aref obj 1) field)))
    (if (not c) (error "Named field %s does not occur in %s" 
		       field (object-name obj)))
    (aset obj c value)))

(defmacro ocall (obj method &rest args)
  "For the given OBJ, call the METHOD associated with it using ARGS as
the arguments used to call it."
  (list 'ocall-engine obj (object-class (symbol-value obj))
	(list 'quote method) (list 'quote args)))

(defmacro ocall-parent (&rest args)
  "For the currently active object (stored in THIS) call the currently
running method (stored conveniently in METHOD) on the parent class of
the current methods class (currently stored in scoped-class)"
  (list 'ocall-engine this (class-parent scoped-class) 'method
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
(defun class-v (class) "Internal: Returns the class vector from the CLASS symbol"
  (if (not (symbolp class)) (signal 'wrong-type-argument (list 'symbolp class)))
  (get class 'eieio-class-definition))

(defun class-p (class) "Return t if CLASS is a valid class vector."
  (and (symbolp class) (vectorp (get class 'eieio-class-definition))
       (equal (aref (class-v class) 0) 'defclass)))

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


;;;
;;; EIEIO internal search functions
;;;

(defun eieio-field-name-index (class field)
  "In OBJ find the index of the named FIELD."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let ((c 0) (l (aref (class-v class) class-public-a)))
    ;; Check out the public symbols
    (while (and l (not (equal field (car l))))
      (setq c (1+ c))
      (setq l (cdr l)))
    (if (not l)
	(if (child-of-class-p class scoped-class)
	    (progn
	      (setq l (aref (class-v class) class-private-a))
	      (while (and l (not (equal field (car l))))
		(setq c (1+ c))
		(setq l (cdr l)))
	      (if (not l)
		  l
		(+ c 3)))
	  nil)
      (+ c 3))))

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
    ((constructor :method t)		; Called when created
     (destructor :method t)		; called when removed
     )
    "Default class used as parent class for superclasses.  It's
fields are automatically adopted by such superclasses but not stored
in the `parent' field.  When searching for attributes or methods, when
the last parent is found, the search will recurse to this class.")

;;; We want our superclass to define it's own methods.
  (defclassmethod constructor eieio-default-superclass (&optional fields)
    "Constructor for filling in attributes when constructing a new
class."
    ;; Load in the defaults
    (eieio-set-defaults this t)
    ;; Set fields for ourselves from the list of fields
    (eieio-set-fields this fields)
    )

  (defclassmethod destructor eieio-default-superclass (&rest params)
    "Destructor for cleaning up any dynamic links to our object."
    ;; No cleanup... yet.
    )

  )  ; End bootstrap check


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

;;; end of lisp
(provide 'eieio)

