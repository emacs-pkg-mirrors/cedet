;;; ede-proj.el --- EDE Generic Project file driver

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj.el,v 1.28 2000/09/28 02:03:31 zappo Exp $

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
;; EDE defines a method for managing a project.  EDE-PROJ aims to be a
;; generic project file format based on the EIEIO object stream
;; methods.  Changes in the project structure will require Makefile
;; rebuild.  The targets provided in ede-proj can be augmented with
;; additional target types inherited directly from `ede-proj-target'.

(require 'ede)
(require 'ede-proj-comp)

;;; Class Definitions:
(defclass ede-proj-target (ede-target)
  ((auxsource :initarg :auxsource
	      :initform nil
	      :type list
	      :custom (repeat (string :tag "File"))
	      :documentation "Auxilliary source files included in this target.
Each of these is considered equivalent to a source file, but it is not
distributed, and each should have a corresponding rule to build it.")
   (dirty :initform nil
	  :type boolean
	  :documentation "Non-nil when generated files needs updating.")
   ;; Class allocated slots
   (availablecompilers :allocation :class
		       :initform nil
		       :type (or null list)
		       :documentation
		       "A list of `ede-compiler' objects.
These are the compilers the user can choose from when setting the
`compiler' slot.")
   )
  "Abstract class for ede-proj targets.")

(defclass ede-proj-target-makefile (ede-proj-target)
  ((makefile :initarg :makefile
	     :initform "Makefile"
	     :type string
	     :custom string
	     :documentation "File name of generated Makefile.")
   (partofall :initarg :partofall
	      :initform t
	      :type boolean
	      :custom boolean
	      :documentation
	      "Non nil means the rule created is part of the all target.
Setting this to nil creates the rule to build this item, but does not
include it in the ALL`all:' rule.")
   (compiler :initarg :compiler
	     :initform nil
	     :type (or null symbol)
	     ;;:custom (choice list o compilers)
	     :documentation
	     "The compiler to be used to compile this object.
This should be a symbol, which contains the object defining the compiler.
This enables save/restore to do so by name, permitting the sharing
of these compiler resources, and global customization thereof.")
   (configuration-variables
    :initarg :configuration-variables
    :initform nil
    :type list
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :documentation "Makefile variables appended to use in different configurations.
These variables are used in the makefile when a configuration becomes active.
Target variables are always renamed such as foo_CFLAGS, then included into
commands where the variable would usually appear.")
   (rules :initarg :rules
	  :initform nil
	  :type list
	  :custom (repeat (object :objecttype ede-makefile-rule))
	  :documentation
	  "Arbitrary rules and dependencies needed to make this target.
It is safe to leave this blank.")
   )
  "Abstract class for Makefile based targets.")

(autoload 'ede-proj-target-aux "ede-proj-aux" 
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-elisp "ede-proj-elisp" 
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-scheme "ede-proj-scheme" 
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-makefile-miscelaneous "ede-proj-misc" 
  "Target class for a group of miscelaneous w/ a special makefile." nil nil)
(autoload 'ede-proj-target-makefile-program "ede-proj-prog" 
  "Target class for building a program." nil nil)
(autoload 'ede-proj-target-makefile-archive "ede-proj-archive" 
  "Target class for building an archive of object code." nil nil)
(autoload 'ede-proj-target-makefile-shared-object "ede-proj-shared" 
  "Target class for building a shared object." nil nil)
(autoload 'ede-proj-target-makefile-info "ede-proj-info" 
  "Target class for info files." nil nil)

(defvar ede-proj-target-alist
  '(("program" . ede-proj-target-makefile-program)
    ("archive" . ede-proj-target-makefile-archive)
    ("sharedobject" . ede-proj-target-makefile-shared-object)
    ("emacs lisp" . ede-proj-target-elisp)
    ("info" . ede-proj-target-makefile-info)
    ("auxiliary" . ede-proj-target-aux)
    ("scheme" . ede-proj-target-scheme)
    ("miscelaneous" . ede-proj-target-makefile-miscelaneous)
    )
  "Alist of names to class types for available project target classes.")

(defclass ede-proj-project (ede-project)
  ((makefile-type :initarg :makefile-type
		  :initform Makefile
		  :type symbol
		  :custom (choice (const Makefile)
				  ;(const Makefile.in)
				  (const Makefile.am)
				  ;(const cook)
				  )
		  :documentation "The type of Makefile to generate.
Can be one of 'Makefile, 'Makefile.in, or 'Makefile.am.
If this value is NOT 'Makefile, then that overrides the :makefile slot
in targets.")
   (variables :initarg :variables
	      :initform nil
	      :type list
	      :custom (repeat (cons (string :tag "Name")
				    (string :tag "Value")))
	      :documentation "Variables to set in this Makefile.")
   (configuration-variables
    :initarg :configuration-variables
    :initform ("debug" (("DEBUG" . "1")))
    :type list
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :documentation "Makefile variables to use in different configurations.
These variables are used in the makefile when a configuration becomes active.")
   (inference-rules :initarg :inference-rules
		    :initform nil
		    :custom (repeat
			     (object :objecttype ede-makefile-rule))
		    :documentation "Inference rules to add to the makefile.")
   (automatic-dependencies
    :initarg :automatic-dependencies
    :initform t
    :type boolean
    :custom boolean
    :documentation
    "Non-nil to do implement automatic dependencies in the Makefile.")
   )
  "The EDE-PROJ project definition class.")

;;; Code:
(defun ede-proj-load (project)
  "Load a project file PROJECT."
  (save-excursion
    (let ((ret nil)
	  (subdirs (directory-files project nil "[^.].*" nil)))
      (set-buffer (get-buffer-create " *tmp proj read*"))
      (unwind-protect
	  (progn
	    (erase-buffer)
	    (insert-file (concat project "Project.ede"))
	    (goto-char (point-min))
	    (setq ret (read (current-buffer)))
	    (if (not (eq (car ret) 'ede-proj-project))
		(error "Corrupt project file"))
	    (setq ret (eval ret))
	    (oset ret file (concat project "Project.ede")))
	(kill-buffer " *tmp proj read*"))
      (while subdirs
	(let ((sd (concat project (car subdirs))))
	  (if (and (file-directory-p sd)
		   (ede-directory-project-p (concat sd "/")))
	      (oset ret subproj (cons (ede-proj-load (concat sd "/"))
				      (oref ret subproj))))
	  (setq subdirs (cdr subdirs))))
      ret)))

(defun ede-proj-save (&optional project)
  "Write out object PROJECT into its file."
  (save-excursion
    (if (not project) (setq project (ede-current-project)))
    (let ((b (set-buffer (get-buffer-create " *tmp proj write*")))
	  (cfn (oref project file)))
      (unwind-protect
	  (save-excursion
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (oset project file (file-name-nondirectory cfn))
	      (object-write project ";; EDE project file."))
	    (write-file (oref project file) nil)
	    )
	;; Restore the :file on exit.
	(oset project file cfn)
	(kill-buffer b)))))

(defmethod ede-commit-local-variables ((proj ede-proj-project))
  "Commit change to local variables in PROJ."
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((proj ede-proj-project))
  "Call this when a user finishes customizing this object.
Argument PROJ is the project to save."
  (call-next-method)
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((target ede-proj-target))
  "Call this when a user finishes customizing this object.
Argument TARGET is the project we are completing customization on."
  (call-next-method)
  (ede-proj-save (ede-current-project)))

(defmethod ede-commit-project ((proj ede-proj-project))
  "Commit any change to PROJ to its file."
  (ede-proj-save proj))

(defmethod ede-buffer-mine ((this ede-proj-project) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((f (ede-convert-path this (buffer-file-name buffer))))
    (or (string= (oref this file) f)
	(string= (ede-proj-dist-makefile this) f))))

(defmethod ede-buffer-mine ((this ede-proj-target) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (or (call-next-method)
      (member (ede-convert-path this (buffer-file-name buffer))
	      (oref this auxsource))))


;;; EDE command functions
;;
(defvar ede-proj-target-history nil
  "History when querying for a target type.")

(defmethod project-new-target ((this ede-proj-project))
  "Create a new target in THIS based on the current buffer."
  (let* ((name (read-string "Name: " ""))
	 (type (completing-read "Type: " ede-proj-target-alist
				nil t nil '(ede-proj-target-history . 1)))
	 (ot nil)
	 (src (if (and (buffer-file-name)
		       (y-or-n-p (format "Add %s to %s? " (buffer-name) name)))
		  (buffer-file-name))))
    (setq ot (funcall (cdr (assoc type ede-proj-target-alist)) name :name name
		      :path (ede-convert-path this default-directory)
		      :source (if src
				  (list (file-name-nondirectory src))
				nil)))
    ;; If we added it, set the local buffer's object.
    (if src (progn 
	      (setq ede-object ot)
	      (ede-apply-object-keymap)))
    ;; Add it to the project object
    (oset this targets (cons ot (oref this targets)))
    ;; And save
    (ede-proj-save this)))

(defmethod project-delete-target ((this ede-proj-target))
  "Delete the current target THIS from it's parent project."
  (let ((p (ede-current-project))
	(ts (oref this source)))
    ;; Loop across all sources.  If it exists in a buffer,
    ;; clear it's object.
    (while ts
      (let* ((default-directory (oref this path))
	     (b (get-file-buffer (car ts))))
	(if b
	    (save-excursion
	      (set-buffer b)
	      (if (eq ede-object this)
		  (progn
		    (setq ede-object nil)
		    (ede-apply-object-keymap))))))
      (setq ts (cdr ts)))
    ;; Remove THIS from it's parent.
    ;; The two vectors should be pointer equivalent.
    (oset p targets (delq this (oref p targets)))
    (ede-proj-save (ede-current-project))))

(defmethod project-add-file ((this ede-proj-target) file)
  "Add to target THIS the current buffer represented as FILE."
  (setq file (file-name-nondirectory file))
  (if (not (member file (oref this source)))
      (oset this source (append (oref this source) (list file))))
  (ede-proj-save (ede-current-project)))

(defmethod project-remove-file ((target ede-proj-target) file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target source (delete (file-name-nondirectory file)
			       (oref target source)))
  (ede-proj-save))

(defmethod project-update-version ((this ede-proj-project))
  "The :version of project THIS has changed."
  (ede-proj-save))

(defmethod project-make-dist ((this ede-proj-project))
  "Build a distribution for the project based on THIS target."
  ;; I'm a lazy bum, so I'll make a makefile for doing this sort
  ;; of thing, and rely only on that small section of code.
  (let ((pm (ede-proj-dist-makefile this)))
    (ede-proj-setup-buildenvironment this)
    (if (string= pm "Makefile.am") (setq pm "Makefile"))
    (compile (concat "make -f " pm " dist"))))

(defmethod project-compile-project ((proj ede-proj-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  (let ((pm (ede-proj-dist-makefile proj))
	(default-directory (file-name-directory (oref proj file))))
    (ede-proj-setup-buildenvironment proj)
    (if (string= pm "Makefile.am") (setq pm "Makefile"))
    (compile (concat "make -f " pm " all"))))

;;; Target type specific compilations/debug
;;
(defmethod project-compile-target ((obj ede-proj-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-project (ede-current-project) command))

(defmethod project-compile-target ((obj ede-proj-target-makefile)
				   &optional command)
  "Compile the current target program OBJ.
Optional argument COMMAND is the s the alternate command to use."
  (ede-proj-setup-buildenvironment (ede-current-project))
  (compile (concat "make -f " (oref obj makefile) " "
		   (ede-proj-makefile-target-name obj))))

(defmethod project-debug-target ((obj ede-proj-target))
  "Run the current project target OBJ in a debugger."
  (error "Debug-target not supported by %s" (object-name obj)))

;;; Compiler and source code generators
;;
(defmethod ede-proj-compilers ((obj ede-proj-target))
  "List of compilers being used by OBJ.
If the `compiler' slot is empty, concoct one on a first match found
basis for any given type from the `availablecompilers' slot.
Otherwise, return the `compiler' slot.
Converts all symbols into the objects to be used."
  (when (slot-exists-p obj 'compiler)
    (let ((comp (oref obj compiler)))
      (if comp
	  ;; Now that we have a pre-set compilers to use, convert tye symbols
	  ;; into objects for ease of use
	  (setq comp (mapcar 'symbol-value comp))
	(let ((avail (mapcar 'symbol-value (oref obj availablecompilers)))
	      (st (oref obj sourcetype))
	      (sources (oref obj source)))
	  ;; COMP is not specified, so generate a list from the available
	  ;; compilers list.
	  (while st
	    (if (ede-want-any-source-files-p (symbol-value (car st)) sources)
		(let ((c (ede-proj-find-compiler avail (car st))))
		  (if c (setq comp (cons c comp)))))
	    (setq st (cdr st)))))
      ;; Return the disovered compilers
      comp)))
    

;;; Target type specific autogenerating gobbldegook.
;;
(eval-when-compile 
  ;; This provides prevents recursive loading during a compile
  (provide 'ede-proj)
  (require 'ede-pmake "ede-pmake.el")
  (require 'ede-pconf "ede-pconf.el"))

(defun ede-proj-makefile-type (&optional proj)
  "Makefile type of the current project PROJ."
  (oref (or proj (ede-current-project)) makefile-type))

(defun ede-proj-automake-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile.am))

(defun ede-proj-autoconf-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile.in))

(defun ede-proj-make-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile))

(defmethod ede-proj-dist-makefile ((this ede-proj-project))
  "Return the name of the Makefile with the DIST target in it for THIS."
  (cond ((eq (oref this makefile-type) 'Makefile.am)
	 "Makefile.am")
	((eq (oref this makefile-type) 'Makefile.in)
	 "Makefile.in")
	((object-assoc "Makefile" 'makefile (oref this targets))
	 "Makefile")
	(t
	 (with-slots (targets) this
	   (while (and targets
		       (not (obj-of-class-p
			     (car targets)
			     'ede-proj-target-makefile)))
	     (setq targets (cdr targets)))
	   (if targets (oref (car targets) makefile)
	     "Makefile")))))
  
(defun ede-proj-regenerate ()
  "Regenerate Makefiles for and edeproject project."
  (interactive)
  (ede-proj-setup-buildenvironment (ede-current-project) t))

(defmethod ede-proj-makefile-create-maybe ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS if needed.
MFILENAME is the makefile to generate."
  ;; For now, pass through until dirty is implemented.
  (require 'ede-pmake)
  (if (file-newer-than-file-p (oref this file) mfilename)
      (ede-proj-makefile-create this mfilename)))

(defmethod ede-proj-setup-buildenvironment ((this ede-proj-project)
					    &optional force)
  "Setup the build environment for project THIS.
Handles the Makefile, or a Makefile.am configure.in combination.
Optional argument FORCE will force items to be regenerated."
  (if (not force)
      (ede-proj-makefile-create-maybe this (ede-proj-dist-makefile this))
    (require 'ede-pmake)
    (ede-proj-makefile-create this (ede-proj-dist-makefile this)))
  (if (ede-proj-automake-p this)
      (progn
	(require 'ede-pconf)
	;; If the user wants to force this, do it some other way?
	(ede-proj-configure-synchronize this)
	;; Now run automake to fill in the blanks, autoconf, and other
	;; auto thingies so that we can just say "make" when done.
	
	)))


;;; Lower level overloads
;;  
(defmethod project-rescan ((this ede-proj-project))
  "Rescan the EDE proj project THIS."
  (ede-with-projectfile this
    (goto-char (point-min))
    (let ((l (read (current-buffer)))
	  (fields (obj-fields this))
	  (targets (oref this targets)))
      (setq l (cdr (cdr l))) ;; objtype and name skip
      (while fields ;  reset to defaults those that dont appear.
	(if (and (not (assoc (car fields) l))
		 (not (eq (car fields) 'file)))
	    (let ((eieio-skip-typecheck t))
	      ;; This is a hazardous thing, for some elements
	      ;; might not be bound.  Skip typechecking and duplicate
	      ;; unbound slots along the way.
	      (eieio-oset this (car fields)
			  (eieio-oref-default this (car fields)))))
	(setq fields (cdr fields)))
      (while l
	(let ((field (car l)) (val (car (cdr l))))
	  (cond ((eq field targets)
		 (let ((targets (oref this targets))
		       (newtarg nil))
		   (setq val (cdr val)) ;; skip the `list'
		   (while val
		     (let ((o (object-assoc (car (cdr (car val))) ; name
					    'name targets)))
		       (if o
			   (project-rescan o (car val))
			 (setq o (eval (car val))))
		       (setq newtarg (cons o newtarg)))
		     (setq val (cdr val)))
		   (oset this targets newtarg)))
		(t
		 (eieio-oset this field val))))
	(setq l (cdr (cdr l))))))) ;; field/value
	
(defmethod project-rescan ((this ede-proj-target) readstream)
  "Rescan target THIS from the read list READSTREAM."
  (setq readstream (cdr (cdr readstream))) ;; constructor/name
  (while readstream
    (let ((tag (car readstream))
	  (val (car (cdr readstream))))
      (eieio-oset this tag val))
    (setq readstream (cdr (cdr readstream)))))

(add-to-list 'auto-mode-alist '("Project\\.ede" . emacs-lisp-mode))

(provide 'ede-proj)

;;; ede-proj.el ends here
