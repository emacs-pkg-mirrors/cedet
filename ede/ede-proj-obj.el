;;; ede-proj-obj.el --- EDE Generic Project Object code generation support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-obj.el,v 1.9 2000/09/24 15:42:20 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
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
;; Handles a supperclass of target types which create object code in
;; and EDE Project file.

(require 'ede-proj)
;; (require 'ede-pmake)
;; The above require is needed for ede-pmake-varname, but introduces
;; a circular dependency.  Leave it be.

(defvar ede-proj-objectcode-dodependencies nil
  "Flag specifies to do automatic dependencies.")

;;; Code:
(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  (;; Give this a new default
   (configuration-variables :initform ("debug" . (("CFLAGS" . "-g")
						  ("LDFLAGS" . "-g"))))
   (availablecompilers :initform (ede-gcc-compiler
				  ede-g++-compiler
				  ;; More C and C++ compilers, plus
				  ;; fortran or pascal can be added here
				  ))
   (sourcetype :initform (ede-source-c 
			  ede-source-c++
			  ;; ede-source-other
			  ;; This object should take everything that
			  ;; gets compiled into objects like fortran
			  ;; and pascal.
			  ))
   (headers :initarg :headers
	    :initform nil
	    :type list
	    :custom (repeat (string :tag "Header"))
	    :documentation "Header files included in the distribution.
EDE generated Makefiles make all sources dependent on all header files.
In automake mode, these headers are ignored, but automake generates
dependencies automatically.")
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defclass ede-object-compiler (ede-compiler)
  ((dependencyvar :initarg :dependencyvar
		 :type list
		 :custom (cons (string :tag "Variable")
			       (string :tag "Value"))
		 :documentation
		 "A variable dedicated to dependency generation.")
   (linkvariables :initarg :linkvariables
		 :type list
		 :custom (repeat (cons (string :tag "Variable")
				       (string :tag "Value")))
		 :documentation
		 "Like the `variables' slot, but for linker info."))
  "Ede compiler class for source which must compiler, and link.")

(defvar ede-source-c
  (ede-sourcecode "ede-source-c"
		  :name "C"
		  :sourcepattern "\\.c$"
		  :auxsourcepattern "\\.h$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C source code definition.")

(defvar ede-gcc-compiler
  (ede-object-compiler
   "ede-c-compiler-gcc"
   :name "gcc"
   :dependencyvar '("C_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CC" . "gcc")
		("C_COMPILE" .
		 "$(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)"))
   :linkvariables '(("C_LINK" .
		     "$(CC) $(CFLAGS) $(LDFLAGS) -L. -o $@ $^")
		    )
   :rules (list (ede-makefile-rule
		 "c-inference-rule"
		 :target "%.o"
		 :dependencies "%.c"
		 :rules '("@echo '$(C_COMPILE) -c $<'; \\"
			  "$(C_COMPILE) $(C_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :commands '("$(C_LINK) %s")
   :autoconf '("AC_PROG_CC" "AC_PROG_GCC_TRADITIONAL")
   :sourcetype '(ede-source-c)
   :objectextention ".o"
   :makedepends t)
  "Compiler for C sourcecode.")

(defvar ede-source-c++
  (ede-sourcecode "ede-source-c++"
		  :name "C++"
		  :sourcepattern "\\.cpp$"
		  :auxsourcepattern "\\.hpp$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C++ source code definition.")

(defvar ede-g++-compiler
  (ede-object-compiler
   "ede-c-compiler-g++"
   :name "g++"
   :dependencyvar '("CXX_DEPENDENCIES" "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CXX" "g++")
		("CXX_COMPILE" .
		 "$(CXX) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)")
		)
   :linkvariables '(("CXX_LINK" .
		     "$(CXX) $(CFLAGS) $(LDFLAGS) -L. -o $@ $^")
		    )
   :rules (list (ede-makefile-rule
		 "c++-inference-rule"
		 :target "%.o"
		 :dependencies "%.cpp"
		 :rules '("@echo '$(CXX_COMPILE) -c $<'; \\"
			  "$(CXX_COMPILE) $(CXX_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :commands '("$(CXX_LINK) %s"
	       )
   :autoconf '("AC_PROG_CXX")
   :sourcetype '(ede-source-c++)
   :objectextention ".o"
   :makedepends t)
  "Compiler for C sourcecode.")

;;; EDE Object Compiler methods
;;
(defvar ede-proj-compiler-object-linkflags nil
  "Libraries to be linked into this program.")

(defmethod ede-proj-makefile-insert-commands ((this ede-object-compiler))
  "Insert the commands needed to use compiler THIS.
The object creating makefile rules must call this method for the
compiler it decides to use after inserting in the rule.
Optional argument LIBS are libraries to be linked into this thing."
  (if ede-proj-compiler-object-linkflags
      (with-slots (commands) this
	(while commands
	  (insert "\t"
		  (format (car commands) ede-proj-compiler-object-linkflags)
		  "\n")
	  (setq commands (cdr commands)))
	(insert "\n"))
    (call-next-method)))

(defmethod ede-proj-makefile-insert-variables :BEFORE ((this ede-object-compiler))
  "Insert variables needed by the compiler THIS."
  (with-slots (dependencyvar linkvariables) this
    (progn
      ;; Insert the dependency part.
      (insert (car dependencyvar) "="
	      (if ede-proj-objectcode-dodependencies
		  (cdr dependencyvar) "")
	      "\n")
      ;; How about linker variables
      (while linkvariables
	(insert (car (car linkvariables)) "=")
	(let ((cd (cdr (car linkvariables))))
	  (if (listp cd)
	      (mapcar (lambda (c) (insert " " c)) cd)
	    (insert cd)))
	(insert "\n")
	(setq linkvariables (cdr linkvariables))))))

;;; EDE Object target type methods
;;
(defmethod ede-buffer-mine ((this ede-proj-target-makefile-objectcode) buffer)
  "Return non-nil if object THIS lays claim to the file in BUFFER."
  (or (call-next-method)
      (member (ede-convert-path this (buffer-file-name buffer))
	      (oref this headers))
      ))

(defmethod project-add-file ((this ede-proj-target-makefile-objectcode) file)
  "Add to target THIS the current buffer represented as FILE."
  (setq file (file-name-nondirectory file))
  ;; Header files go into auxiliary sources.  Add more for more languages.
  (let ((src (ede-target-sourcecode this))
	(aux nil))
    (while (and src (not (ede-want-file-auxiliary-p (car src) file)))
      (setq src (cdr src)))
    (if (not src)
	;; No match, add to regular sources.
	(call-next-method)
      (if (not (member file (oref this headers)))
	  (oset this headers (append (oref this headers) (list file))))
      (ede-proj-save (ede-current-project)))))

(defmethod project-remove-file ((target ede-proj-target-makefile-objectcode)
				file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target headers (delete (file-name-nondirectory file)
			       (oref target headers)))
  ;; This will do sources, and save the project for us.
  (call-next-method))

(defmethod ede-proj-makefile-sourcevar
  ((this ede-proj-target-makefile-objectcode))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_SOURCES"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-objectcode))
  "Return a list of source files to convert to dependencies.
Argument THIS is the target to get sources from."
  (append (oref this source) (oref this auxsource)))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-makefile-objectcode)
					       &optional moresource)
  "Insert variables needed by target THIS.
Optional argument MORESOURCE is not used."
  (let ((ede-proj-objectcode-dodependencies
	 (oref (ede-target-parent this) automatic-dependencies)))
    (call-next-method)))

(when nil
  (let ((have-libtool (and (slot-exists-p this 'libtool) (oref this libtool))))
    (insert (ede-name this) ": $(" (ede-pmake-varname this) "_OBJ)\n"
	    ;; Compile line
	    (if have-libtool
		"\t$(LTLINK) "
	      "\t$(LINK) ")
	    ;; Shared flag if needed
	    (if (and
		 (featurep 'ede-proj-shared)
		 (obj-of-class-p this 'ede-proj-target-makefile-shared-object)
		 (not have-libtool))
		"-shared "
	      "")
	    ;; Additional linker flags
	    (if (and (obj-of-class-p this 'ede-proj-target-makefile-program)
		     (oref this ldflags))
		(concat (mapconcat (lambda (c) c) (oref this ldflags) " ") " ")
	      "")
	    ;; The objects to link
	    "$(" (ede-pmake-varname this) "_OBJ)"
	    ;; Separate this out later.
	    (if (obj-of-class-p this 'ede-proj-target-makefile-program)
		;; Some libaries
		(concat " "
			(mapconcat (lambda (c)
				     (if (= (aref c 0) ?$)
					 c
				       (concat "-l" c)))
				   (oref this ldlibs) " "))
	      "")
	    "\n\n"))
)

(defmethod ede-buffer-header-file((this ede-proj-target-makefile-objectcode)
				  buffer)
  "There are no default header files."
  (or (call-next-method)
      ;; Ok, nothing obvious. Try looking in ourselves.
      (let ((h (oref this headers)))
	;; Add more logic here when the problem is better understood.
	(car-safe h))))
  
;;; Speedbar options:
;;
(defmethod eieio-speedbar-child-make-tag-lines
  ((this ede-proj-target-makefile-objectcode) depth)
  "Expand an object code node in speedbar.
THIS is special for additional headers.
Argument DEPTH is the tree depth."
  (call-next-method)
  (with-slots (headers) this
    (mapcar (lambda (car)
	      (speedbar-make-tag-line 'bracket ?+
				      'ede-tag-file
				      (concat (oref this :path) car)
				      car
				      'ede-file-find
				      (concat (oref this :path) car)
				      'speedbar-file-face depth))
	    headers)))

(provide 'ede-proj-obj)

;;; ede-proj-obj.el ends here
