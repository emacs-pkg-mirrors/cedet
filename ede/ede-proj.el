;;; ede-proj.el --- EDE Generic Project file driver

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.1
;; Keywords: project, make
;; RCS: $Id: ede-proj.el,v 1.2 1999/01/21 14:12:09 zappo Exp $

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
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

;;; Class Definitions:
(defclass ede-proj-target (ede-target)
  ((source :initarg :source
	   :custom (repeat object)
	   :documentation "Source files in this target.")
   (dirty :initform nil
	  :documentation "Non-nil when generated files needs updating.")
   )
  "Abstract class for ede-proj targets.")

(defclass ede-proj-target-makefile (ede-proj-target)
  ((makefile :initarg :mfile
	     :initform "Makefile"
	     :custom string
	     :documentation "File name of generated Makefile.")
   )
  "Abstract class for Makefile based targets.")

(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  ((ldflags :initarg :ldflags
	    :initform nil
	    :custom (repeat (string :tag "Flag: "))
	    :documentation "Additional flags to pass to the linker.")
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defclass ede-proj-target-makefile-program (ede-proj-target-makefile-objectcode)
  ()
  "This target is an executable program.")

(defclass ede-proj-target-makefile-archive (ede-proj-target-makefile-objectcode)
  ()
  "This target generates an object code archive.")

(defclass ede-proj-target-makefile-info (ede-proj-target-makefile)
  ((mainmenu :initarg :mainmenu
	     :initform nil
	     :custom string
	     :docstring "The main menu resides in this file."))
  "Target for a single info file.")
   
(defclass ede-proj-target-lisp (ede-proj-target)
  ()
  "This target consists of a group of lisp files.
A lisp target may be one general program with many separate lisp files in it.")

(defclass ede-proj-target-aux (ede-proj-target)
  ()
  "This target consists of aux files such as READMEs and COPYING.")

(defvar ede-proj-target-alist
  '(("program" . ede-proj-target-makefile-program)
    ("archive" . ede-proj-target-makefile-archive)
    ("emacs lisp" . ede-proj-target-lisp)
    ("info" . ede-proj-target-makefile-info)
    ("auxiliary" . ede-proj-target-aux)
    )
  "Alist of names to class types for available project target classes.")

(defclass ede-proj-project (ede-project)
  ()
  "The EDE-PROJ project definition class.")

;;; Code:
(defun ede-proj-load (project)
  "Load a project file PROJECT."
  (save-excursion
    (let ((ret nil))
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
	    (oset ret :file (concat project "Project.ede")))
	(kill-buffer " *tmp proj read*"))
      ret)))

(defun ede-proj-save (&optional project)
  "Write out object PROJECT into its file."
  (save-excursion
    (if (not project) (setq project (ede-current-project)))
    (let ((b (set-buffer (get-buffer-create " *tmp proj write*")))
	  (cfn (oref project :file)))
      (unwind-protect
	  (save-excursion
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (oset project :file (file-name-nondirectory cfn))
	      (object-write project ";; EDE project file."))
	    (write-file (oref project :file) nil)
	    )
	;; Restore the :file on exit.
	(oset project :file cfn)
	(kill-buffer b)))))

(defun ede-proj-write-targets (val)
  "Write the list of targets VAL.
Used in the :writer slot of the eieio class."
  (while val
    (object-write (car val))
    (setq val (cdr val))))

(defmethod ede-commit-project ((proj ede-proj-project))
  "Commit any changes to the project PROJ to its file."
  (ede-proj-save proj))

(defmethod ede-find-target ((proj ede-proj-project) buffer)
  "Fetch the target belonging to BUFFER or nil."
  (or ede-object
      (if (ede-buffer-mine proj buffer)
	  proj
	(let ((targets (oref proj :targets))
	      (f nil))
	  (while (and targets (not f))
	    (if (member (ede-convert-path proj (buffer-file-name buffer))
			(oref (car targets) :source))
		(setq f (car targets)))
	    (setq targets (cdr targets)))
	  f))))

(defmethod ede-buffer-mine ((this ede-proj-project) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :file)
	   (ede-convert-path this (buffer-file-name buffer))))

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
	 (src (if (y-or-n-p (format "Add %s to %s?" (buffer-name) name))
		  (list (buffer-file-name)) nil)))
    (setq ot (funcall (cdr (assoc type ede-proj-target-alist)) name :name name
		      :path (ede-convert-path this default-directory)
		      :source (file-name-nondirectory src)))
    ;; If we added it, set the local buffer's object.
    (if src (setq ede-obj ot))
    ;; Add it to the project object
    (oset this :targets (cons ot (oref this :targets)))
    ;; And save
    (ede-proj-save this)))

(defmethod project-delete-target ((this ede-proj-target))
  "Delete the current target from it's parent project."
  (let ((p (ede-current-project))
	(ts (oref this :source)))
    ;; Loop across all sources.  If it exists in a buffer,
    ;; clear it's object.
    (while ts
      (let* ((default-directory (oref this :path))
	     (b (get-file-buffer (car ts))))
	(if b
	    (save-excursion
	      (set-buffer b)
	      (if (eq ede-object this)
		  (setq ede-object nil)))))
      (setq ts (cdr ts)))
    ;; Remove THIS from it's parent.
    ;; The two vectors should be pointer equivalent.
    (oset p :targets (delq this (oref p :targets)))))

(defmethod project-add-file ((this ede-proj-target) file)
  "Add the current buffer into this target."
  (setq file (file-name-nondirectory file))
  (if (not (member file (oref this source)))
      (oset this :source (append (oref this source) (list file))))
  (ede-proj-save (ede-current-project)))

(defmethod project-remove-file ((target ede-proj-target) file)
  "Remove FILE from TARGET.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target :source (delete (file-name-nondirectory file)
			       (oref target :source)))
  (ede-proj-save))

(defmethod project-make-dist ((this ede-proj-project))
  "Build a distribution for the project based on THIS target."
  ;; I'm a lazy bum, so I'll make a makefile for doing this sort
  ;; of thing, and rely only on that small section of code.
  (let ((pm (ede-proj-dist-makefile this)))
    (ede-proj-makefile-create-maybe this pm)
    (compile (concat "make -f " pm " dist"))))

(defmethod project-compile-project ((obj ede-proj-project) &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  ;; First, find the list of all unique makefiles, then call them all.
  
  )

;;; Target type specific compilations/debug
;;
(defmethod project-compile-target ((obj ede-proj-target) &optional command)
  "Compile the current target.
Argument COMMAND is the command to use for compiling the target."
  (error "compile-target not supported by %s" (object-name obj)))

(defmethod project-compile-target ((obj ede-proj-target-lisp))
  "Compile all sources in a lisp target."
  (mapcar (lambda (src)
	    (let ((elc (concat (file-name-sans-extension src) ".elc")))
	      (if (or (not (file-exists-p elc))
		      (file-newer-than-file-p src elc))
		  (byte-compile-file src))))
	  (oref obj :source)))

(defmethod project-debug-target ((obj ede-proj-target))
  "Run the current project target in a debugger."
  (error "debug-target not supported by %s" (object-name obj)))


;;; Target type specific autogenerating gobbldegook.
;;
(defmethod ede-proj-dist-makefile ((this ede-proj-project))
  "Return the name of the Makefile with the DIST target in it for THIS."
  (cond ((object-assoc "Makefile" :makefile (oref this :targets))
	 (setq mfilename "Makefile"))
	(t
	 (with-slots (targets) this
	   (while (and targets
		       (not (obj-of-class-p (car targets)
					    'ede-proj-target-makefile)))
	     (setq targets (cdr targets)))
	   (setq mfilename 
		 (if targets (oref (car targets) :makefile)
		   "Makefile"))))))

(defmethod ede-proj-makefile-create-maybe ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS if needed.
MFILENAME is the makefile to generate."
  ;; For now, pass through until dirty is implemented.
  (ede-proj-makefile-create this mfilename))

(defmethod ede-proj-makefile-create ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS.
MFILENAME is the makefile to generate."
  (let ((mt nil) tmp
	(isdist (string= mfilename (ede-proj-dist-makefile this))))
    ;; Collect the targets that belong in a makefile.
    (with-slots (targets) this
      (while targets
	(if (and (obj-of-class-p (car targets) 'ede-proj-target-makefile)
		 (string= (oref (car targets) :makefile) mfilename))
	    (setq mt (cons (car targets) mt)))
	(setq targets (cdr targets))))
    (save-excursion
      (set-buffer (find-file-noselect mfilename))
      (erase-buffer)
      (insert
       "# Automatically Generated Makefile by EDE.\n"
       "#\n"
       "# DO NOT MODIFY THIS FILE UNLESS YOU DO NOT PLAN ON USING EDE FOR\n"
       "# FUTURE DEVELOPMENT.  EDE is the Emacs Development Environment.\n"
       "# \n")
      (insert "DISTDIR=" (oref this name) "-" (oref this version) "\n")
      (if isdist
	  (setq tmp (oref this targets))
	(setq tmp mt))
      (while tmp
	(ede-proj-makefile-insert-variables (car tmp))
	(setq tmp (cdr tmp)))
      (setq tmp mt)
      (while tmp
	(ede-proj-makefile-insert-inference (car tmp))
	(setq tmp (cdr tmp)))
      (setq tmp mt)
      (insert "\n\nall:")
      (while tmp (insert " " (oref (car tmp) name))
	     (setq tmp (cdr tmp)))
      (insert "\n")
      (setq tmp mt)
      (while tmp
	(ede-proj-makefile-insert-rules (car tmp))
	(setq tmp (cdr tmp)))
      (if isdist
	  (progn
	    ;; Build DIST, TAG, and other rules here.
	    (insert "\ndist:\n")
	    (insert "\trm -rf $(DISTDIR)\n")
	    (insert "\tmkdir $(DISTDIR)\n")
	    (setq tmp (oref this targets))
	    (insert "\tcp")
	    (while tmp (insert " $(" (ede-proj-makefile-sourcevar (car tmp))
			       ")")
		   (setq tmp (cdr tmp)))
	    (insert " $(DISTDIR)\n")
	    (insert "\ttar -cvzf $(DISTDIR).tar.gz $(DISTDIR)\n\n")
	    (insert "\n\n# End of Makefile\n")))
      (save-buffer))))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target))
  "Return the variable name for THIS's sources."
  (concat (oref this :name) "_SOURCE"))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-info))
  "Return the variable name for THIS's sources."
  (concat (oref this :name) "_INFOS"))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target))
  "Insert variables needed by this target."
  (insert (ede-proj-makefile-sourcevar this) "="
	  (mapconcat (lambda (a) a) (oref this :source) " ")
	  "\n"))
 
(defmethod ede-proj-makefile-insert-inference ((this ede-proj-target-makefile))
  "Insert inference rules needed by this target." nil)

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile))
  "Insert inference rules needed by this target." nil)


;;; Lower level overloads
;;  
(defmethod project-rescan ((this ede-proj-project))
  "Rescan the EDE proj project."
  (ede-with-projectfile this
    (goto-char (point-min))
    (let ((l (read (current-buffer)))
	  (fields (obj-fields this)))
      (setq l (cdr (cdr l))) ;; objtype and name skip
      (while fields ;  reset to defaults those that dont appear.
	(if (and (not (assoc (car fields) l))
		 (not (eq (car fields) 'file)))
	    (oset-engine this (car fields)
			 (oref-default-engine this (car fields))))
	(setq fields (cdr fields)))
      (while l
	(let ((field (car l)) (val (car (cdr l))))
	  (cond ((eq field :targets)
		 (let ((targets (oref this :targets))
		       (newtarg nil))
		   (setq val (cdr val)) ;; skip the `list'
		   (while val
		     (let ((o (object-assoc (car (cdr (car val))) ; name
					    :name targets)))
		       (if o 
			   (project-rescan o (car val))
			 (setq o (eval (car val))))
		       (setq newtarg (cons o newtarg)))
		     (setq val (cdr val)))
		   (oset this :targets newtarg)))
		(t
		 (oset-engine this field val))))
	(setq l (cdr (cdr l))))))) ;; field/value
	
(defmethod project-rescan ((this ede-proj-target) readstream)
  "Rescan target THIS from the read list READSTREAM."
  (setq readstream (cdr (cdr readstream))) ;; constructor/name
  (while readstream
    (let ((tag (car readstream))
	  (val (car (cdr readstream))))
      (oset-engine this tag val))
    (setq readstream (cdr (cdr readstream)))))

(add-to-list 'auto-mode-alist '("Project\\.ede" . emacs-lisp-mode))

(provide 'ede-proj)

;;; ede-proj.el ends here
