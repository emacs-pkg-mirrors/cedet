;;; ede.el --- Emacs Development Environment gloss

;;;  Copyright (C) 1998, 99, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede.el,v 1.29 2000/04/28 22:56:02 zappo Exp $

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
;; EDE is the top level Lisp interface to a project management scheme
;; for Emacs.  Emacs does many things well, including editing,
;; building, and debugging.  Folks migrating from other IDEs don't
;; seem to thing this qualifies, however, because they still have to
;; write the makefiles, and specify parameters to programs.
;;
;; This EDE mode will attempt to link these diverse programs together
;; into a comprehensive single interface, instead of a bunch of
;; different ones.

;;; Install
;;
;;  This command enables project mode on all files.
;;
;;  (global-ede-mode t)

;;; Code:
(defvar ede-version "0.7"
  "Current version of the Emacs EDE.")

(defun ede-version ()
  "Display the current running version of EDE."
  (interactive) (message "EDE %s" ede-version))

;; From custom web page for compatibility between versions of custom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	   ;; Some XEmacsen w/ custom don't have :set keyword.
	   ;; This protects them against custom.
	   (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
	nil
      (defmacro defgroup (&rest args)
	nil))
    (if (boundp 'defcustom)
	nil
      (defmacro defcustom (var value doc &rest args)
	(` (defvar (, var) (, value) (, doc)))))))

(defgroup ede nil
  "Emacs Development Environment gloss."
  :group 'tools
  :group 'convenience
  )

(defcustom ede-auto-add-method 'ask
  "Determins if a new source file shoud be automatically added to a target.
Whenever a new file is encountered in a directory controlled by a
project file, all targets are queried to see if it should be added.
If the value is 'always, then the new file is added to the first
target encountered.  If the value is 'multi-ask, then if more than one
target wants the file, the user is asked.  If only one target wants
the file, then then it is automatically added to that target.  If the
value is 'ask, then the user is always asked, unless there is no
target willing to take the file.  'never means never perform the check."
  :group 'ede
  :type '(choice (const always)
		 (const multi-ask)
		 (const ask)
		 (const never)))

(defcustom ede-debug-program-function 'gdb
  "*Default Emacs command used to debug a target."
  :group 'ede
  :type 'sexp) ; make this be a list of options some day

(require 'eieio)
(require 'eieio-speedbar)

;;; Top level classes for projects and targets
;;
(defclass ede-project-autoload ()
  ((name :initarg :name :documentation "Name of this project type")
   (file :initarg :file :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use."))
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload "edeproject"
			 :name "edeproject" :file 'ede-proj
			 :proj-file "Project.ede"
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project)
   (ede-project-autoload "automake"
			 :name "automake" :file 'project-am
			 :proj-file "Makefile.am"
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile)
   )
  "List of vectos defining how to determine what type of projects exist.")

;;; Generic project information manager objects
;;
(defclass ede-target (eieio-speedbar-directory-button)
  ((buttonface :initform speedbar-file-face) ;override for superclass
   (name :initarg :name
	 :type string
	 :custom string
	 :documentation "Name of this target.")
   (path :initarg :path
	 :type string
	 :custom string
	 :documentation "The path to this target.")
   (takes-compile-command
    :allocation :class
    :initarg :takes-compile-command
    :type boolean
    :initform nil
    :documentation "Non-nil if this target requires a user approved command.")
   (source :initarg :source
	   ;; I'd prefer a list of strings.
	   :type list
	   :custom (repeat (string :tag "File"))
	   :documentation "Source files in this target.")
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target))
		:documentation "Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform ( [ "Debug target" ede-debug-target
		       (and ede-object
			    (obj-of-class-p ede-object ede-target)) ]
		     )
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "A top level target to build.")

(defclass ede-project (eieio-speedbar-directory-button)
  ((name :initarg :name
	 :initform "Untitled"
	 :type string
	 :custom string
	 :documentation "The name used when generating distribution files.")
   (version :initarg :version
	    :initform "1.0"
	    :type string
	    :custom string
	    :documentation "The version number used when distributing files.")
   (file :initarg :file
	 :type string
	 :documentation "File name where this project is stored.")
   ;; No initarg.  We don't want this saved.
   (root :initform nil
	 :documentation "The root project file if this is a subproject.")
   ;; No initarg.  We don't want this saved in a file.
   (subproj :initform nil
	    :type list
	    :documentation "Sub projects controlled by this project.
For Automake based projects, each directory is treated as a project.")
   (targets :initarg :targets
	    :type list
	    :custom (repeat object)
	    :documentation "List of top level targets in this project.")
   (configurations :initarg :configurations
		   :initform ("debug" "release")
		   :type list
		   :custom (repeat string)
		   :documentation "List of available configuration types.
Individual target/project types can form associations between a configuration,
and target specific elements such as build variables.")
   (configuration-default :initarg :configuration-default
			  :initform "debug"
			  :custom string
			  :documentation "The default configuration.")
   (local-variables :initarg :local-variables
		    :initform nil
		    :custom (repeat (cons (sexp :tag "Variable")
					  (sexp :tag "Value")))
		    :documentation "Project local variables")
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target))
		:documentation "Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform nil
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "Top level EDE project specification.
All specific project types must derive from this project.")

;;; Management variables
;;
(defvar ede-projects nil
  "A list of all active projects currently loaded in Emacs.")

(defvar ede-object nil
  "The current buffer's target object.
This object's class determines how to compile and debug from a buffer.")
(make-variable-buffer-local 'ede-object)

(defvar ede-selected-object nil
  "The currently user-selected project or target.
If `ede-object' is nil, then commands will operate on this object.")

(defvar ede-constructing nil
  "Non nil when constructing a project hierarchy.")

(defvar ede-deep-rescan nil
  "Non nil means scan down a tree, otherwise rescans are top level only.
Do not set this to non-nil globally.  It is used internally.")

;;; Important macros for doing commands.
;;
(defmacro ede-with-projectfile (obj &rest forms)
  "For the project in which OBJ resides, execute FORMS."
  (list 'save-window-excursion
	(list 'let* (list
		     (list 'pf
			   (list 'if (list 'obj-of-class-p
					   obj 'ede-target)
				 (list 'ede-load-project-file
				       (list 'oref obj 'path))
				 obj))
		     '(dbka (get-file-buffer (oref pf file))))
	      '(if (not dbka) (find-file (oref pf file))
		 (switch-to-buffer dbka))
	      (cons 'progn forms)
	      '(if (not dbka) (kill-buffer (current-buffer))))))
(put 'ede-with-projectfile 'lisp-indent-function 1)

(defun ede-singular-object (prompt)
  "Using PROMPT, choose a single object from the current buffer."
  (if (listp ede-object)
      (ede-choose-object prompt ede-object)
    ede-object))

(defun ede-choose-object (prompt list-o-o)
  "Using PROMPT, ask the user which OBJECT to use based on the name field.
Argument LIST-O-O is the list of objects to choose from."
  (let* ((al (object-assoc-list 'name list-o-o))
	 (ans (completing-read prompt al nil t)))
    (setq ans (assoc ans al))
    (cdr ans)))

;;; Minor mode specification
;;
(defvar ede-minor-mode nil
  "Non-nil in `emacs-lisp-mode' for automatic documentation checking.")
(make-variable-buffer-local 'ede-minor-mode)

;; We don't want to waste space.  There is a menu after all.
(add-to-list 'minor-mode-alist '(ede-minor-mode ""))

(defvar ede-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "e" 'ede-edit-file-target)
    (define-key pmap "a" 'ede-add-file)
    (define-key pmap "d" 'ede-remove-file)
    (define-key pmap "t" 'ede-new-target)
    (define-key pmap "g" 'ede-rescan-toplevel)
    (define-key pmap "s" 'ede-speedbar)
    (define-key pmap "l" 'ede-load-project-file)
    (define-key pmap "C" 'ede-compile-project)
    (define-key pmap "c" 'ede-compile-target)
    (define-key pmap "\C-c" 'ede-compile-selected)
    (define-key pmap "D" 'ede-debug-target)
    ;; bind our submap into map
    (define-key map "\C-c." pmap)
    map)
  "Keymap used in project minor mode.")

(if ede-minor-keymap
    (progn
      (easy-menu-define
       ede-minor-menu ede-minor-keymap "Project Minor Mode Menu"
       '("Project"
	 [ "Build all" ede-compile-project nil ]
	 [ "Build Active Project" ede-compile-project t ]
	 [ "Build Active Target" ede-compile-target t ]
	 [ "Build Selected..." ede-compile-selected t ]
	 "---"
	 [ "Add File" ede-add-file (ede-current-project) ]
	 [ "Remove File" ede-remove-file
	   (and ede-object
		(or (listp ede-object)
		    (not (obj-of-class-p ede-object ede-project)))) ]
	 "---"
	 [ "Select Active Target" nil nil ]
	 [ "Add Target" ede-new-target (ede-current-project) ]
	 [ "Remove Target" ede-delete-target ede-object ]
	 ( "Target Options" :filter ede-target-forms-menu )
	 "---"
	 [ "Select Active Project" 'undefined nil ]
	 [ "Create Project" ede-new (not ede-object) ]
	 [ "Remove Project" 'undefined nil ]
	 [ "Load a project" ede t ]
	 [ "Rescan Project Files" ede-rescan-toplevel t ]
	 [ "Customize Project" ede-customize-project (ede-current-project) ]
	 [ "Edit Projectfile" ede-edit-file-target
	   (and ede-object
		(not (obj-of-class-p ede-object ede-project))) ]
	 [ "Make distribution" ede-make-dist t ]
	 "---"
	 [ "View Project Tree" ede-speedbar t ]
	 ))
      ))

;; Allow re-insertion of a new keymap
(let ((a (assoc 'ede-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a ede-minor-keymap)
    (add-to-list 'minor-mode-map-alist
		 (cons 'ede-minor-mode
		       ede-minor-keymap))
    ))

(defun ede-target-forms-menu (menu-def)
  "Create a target MENU-DEF based on the object belonging to this buffer."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Target Forms"
    (let* ((obj (or ede-selected-object ede-object))
	   (defaultitems
	     '( [ "Target Preferences..." ede-customize-target
		  (and ede-object
		       (not (obj-of-class-p ede-object ede-project))) ]
		)))
      (if (and obj (oref obj menu))
	  (append defaultitems (oref obj menu))
	defaultitems)))))

(defun ede-apply-object-keymap (&optional default)
  "Add target specific keybindings into the local map.
Optional argument DEFAULT indicates if this should be set to the default
version of the keymap."
  (let ((object (or ede-object ede-selected-object)))
    (condition-case nil
	(let ((keys (ede-object-keybindings object)))
	  (while keys
	    (local-set-key (concat "\C-c." (car (car keys)))
			   (cdr (car keys)))
	    (setq keys (cdr keys))))
      (error nil))))

(autoload 'ede-dired-minor-mode "ede-dired" "EDE commands for dired" t)

(defun ede-minor-mode (&optional arg)
  "Project Automake minor mode.
If this file is contained, or could be contained in an automake
controlled project, then this mode should be active.

Project Automake will manage your automake files, and make it very
easy to edit your automake files.

With argument ARG positive, turn on the mode.  Negative, turn off the
mode.  nil means to toggle the mode."
  (if (eq major-mode 'dired-mode)
      (ede-dired-minor-mode arg)
    (progn
      (setq ede-minor-mode
	    (not (or (and (null arg) ede-minor-mode)
		     (<= (prefix-numeric-value arg) 0))))
      (if (and ede-minor-mode (not ede-constructing))
	  (progn
	    (ede-load-project-file default-directory)
	    (setq ede-object (ede-buffer-object))
	    (if (and (not ede-object) (ede-current-project))
		(ede-auto-add-to-target))
	    (if (ede-current-project)
		(ede-set-project-variables (ede-current-project)))
	    (ede-apply-object-keymap))))))
  
(defun global-ede-mode (arg)
  "Turn on variable `ede-minor-mode' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise."
  (interactive "P")
  (if (not arg)
      (if (member (lambda () (ede-minor-mode 1)) find-file-hooks)
	  (global-ede-mode -1)
	(global-ede-mode 1))
    (if (or (eq arg t) (> arg 0))
	(progn
	  (add-hook 'find-file-hooks (lambda () (ede-minor-mode 1)))
	  (add-hook 'dired-mode-hook (lambda () (ede-minor-mode 1))))
      (remove-hook 'find-file-hooks (lambda () (ede-minor-mode 1)))
      (remove-hook 'dired-mode-hook (lambda () (ede-minor-mode 1))))
    (let ((b (buffer-list)))
      (while b
	(if (buffer-file-name (car b))
	    (save-excursion
	      (set-buffer (car b))
	      (ede-minor-mode arg))
	  (save-excursion
	    (set-buffer (car b))
	    (if (eq major-mode 'dired-mode)
		(ede-minor-mode arg)
	      )))
	(setq b (cdr b))))))

(defun ede-auto-add-to-target ()
  "Look for a target that wants to own the current file.
Follow the preference set with `ede-auto-add-method' and get the list
of objects with the `ede-want-file-p' method."
  (if ede-object (error "Ede-object already defined for %s" (buffer-name)))
  (if (eq ede-auto-add-method 'never)
      nil
    (let (wants desires)
      ;; Find all the objects.
      (setq wants (oref (ede-current-project) targets))
      (while wants
	(if (ede-want-file-p (car wants) (buffer-file-name))
	    (setq desires (cons (car wants) desires)))
	(setq wants (cdr wants)))
      (if desires
	  (cond ((or (eq ede-auto-add-method 'ask)
		     (and (eq ede-auto-add-method 'multi-ask)
			  (< 1 (length desires))))
		 (let* ((al (cons '("none" . nil)
				  (object-assoc-list 'name desires)))
			(ans (completing-read (format "Add %s to target: " (buffer-file-name))
					      al nil t)))
		   (setq ans (assoc ans al))
		   (if (cdr ans)
		       (ede-add-file (cdr ans)))))
		((or (eq ede-auto-add-method 'always)
		     (and (eq ede-auto-add-method 'multi-ask)
			  (= 1 (length desires))))
		 (ede-add-file (car desires)))
		(t nil))))))


;;; Interactive method invocations
;;
(defun ede (file)
  "Start up EDE on something.
Argument FILE is the file or directory to load a project from."
  (interactive "fProject File: ")
  (if (not (file-exists-p file))
      (ede-new file)
    (ede-load-project-file (file-name-directory file))))

(defun ede-new (type)
  "Create a new project starting of project type TYPE."
  (interactive
   (list (completing-read "Project Type: "
			  (object-assoc-list 'name ede-project-class-files)
			  nil t)))
  (let* ((obj (object-assoc type 'name ede-project-class-files))
	 (nobj (progn
		 ;; Make sure this class gets loaded!
		 (require (oref obj file))
		 (make-instance (oref obj class-sym)
				:name (read-string "Name: ")
				:file
				(expand-file-name (oref obj proj-file))))))
    (if (ede-parent-project)
	(ede-add-subproject (ede-parent-project) nobj))
    (ede-commit-project nobj))
  (message "Project created and saved.  You may now create targets."))

(defmethod ede-add-subproject ((proj-a ede-project) proj-b)
  "Add into PROJ-A, the subproject PROJ-B."
  (oset proj-a subproj (cons proj-b (oref proj-a subproj))))

(defun ede-invoke-method (sym &rest args)
  "Invoke method SYM on the current buffer's project object.
ARGS are additional arguments to pass to method sym."
  (if (not ede-object)
      (error "Cannot invoke %s for %s" (symbol-name sym)
	     (buffer-name)))
  ;; Always query a target.  There should never be multiple
  ;; projects in a single buffer.
  (apply sym (ede-singular-object "Target: ") args))

(defun ede-rescan-toplevel ()
  "Rescan all project files."
  (interactive)
  (let ((toppath (ede-toplevel-project default-directory))
	(ede-deep-rescan t))
    (project-rescan (ede-load-project-file toppath))))

(defun ede-new-target ()
  "Create a new target specific to this type of project file."
  (interactive)
  (project-new-target (ede-current-project))
  (setq ede-object nil)
  (setq ede-object (ede-buffer-object (current-buffer)))
  (ede-apply-object-keymap))

(defun ede-delete-target (target)
  "Delete TARGET from the current project."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Target: "))))
  ;; Find all sources in buffers associated with the condemned buffer.
  (let ((condemned (ede-target-buffers target)))
    (project-delete-target target)
    ;; Loop over all project controlled buffers
    (save-excursion
      (while condemned
	(set-buffer (car condemned))
	(setq ede-object nil)
	(setq ede-object (ede-buffer-object (current-buffer)))
	(setq condemned (cdr condemned))))
    (ede-apply-object-keymap)))

(defun ede-add-file (target)
  "Add the current buffer to a TARGET in the current project."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Target: "))))
  (project-add-file target (buffer-file-name))
  (setq ede-object nil)
  (setq ede-object (ede-buffer-object (current-buffer)))
  (ede-apply-object-keymap))

(defun ede-remove-file (&optional force)
  "Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking."
  (interactive "P")
  (if (not ede-object)
      (error "Cannot invoke remove-file for %s" (buffer-name)))
  (let ((eo (if (listp ede-object)
		(prog1
		    ede-object
		  (setq force nil))
	      (list ede-object))))
    (while eo
      (if (or force (y-or-n-p (format "Remove from %s? " (ede-name (car eo)))))
	  (progn
	    (project-remove-file (car eo)
				 (ede-convert-path (ede-current-project)
						   (buffer-file-name)))))
      (setq eo (cdr eo)))
    (setq ede-object nil)
    (setq ede-object (ede-buffer-object (current-buffer)))
    (ede-apply-object-keymap)))

(defun ede-edit-file-target ()
  "Enter the project file to hand edit the current buffer's target."
  (interactive)
  (ede-invoke-method 'project-edit-file-target))

(defun ede-compile-project ()
  "Compile the current project."
  (interactive)
  (let ((cp (ede-current-project)))
    (while (ede-parent-project cp)
      (setq cp (ede-parent-project cp)))
    (let ((ede-object cp))
      (ede-invoke-method 'project-compile-project))))

(defun ede-compile-selected (target)
  "Compile some TARGET from the current project."
  (interactive (list (project-interactive-select-target (ede-current-project)
							"Target to Build: ")))
  (project-compile-target target))

(defun ede-compile-target ()
  "Compile the current buffer's associated target."
  (interactive)
  (ede-invoke-method 'project-compile-target))

(defun ede-debug-target ()
  "Debug the current buffer's assocated target."
  (interactive)
  (ede-invoke-method 'project-debug-target))

(defun ede-make-dist ()
  "Create a distribution from the current project."
  (interactive)
  (let ((ede-object (ede-current-project)))
    (ede-invoke-method 'project-make-dist)))

(eval-when-compile (require 'eieio-custom))

(defvar eieio-ede-old-variables nil
  "The old variables for a project.")

(defalias 'customize-project 'ede-customize-project)
(defun ede-customize-project ()
  "Edit fields of the current project through EIEIO & Custom."
  (interactive)
  (require 'eieio-custom)
  (let ((ov (oref (ede-current-project) local-variables))
	(cp (ede-current-project)))
    (eieio-customize-object cp)
    (make-local-variable 'eieio-ede-old-variables)
    (setq eieio-ede-old-variables ov)))

(defalias 'customize-target 'ede-customize-target)
(defun ede-customize-target ()
  "Edit fields of the current target through EIEIO & Custom."
  (interactive)
  (require 'eieio-custom)
  (if (and ede-object
	   (obj-of-class-p ede-object ede-project))
      (error "No logical target to customize"))
  (eieio-customize-object ede-object))

(defmethod eieio-done-customizing ((proj ede-project))
  "Call this when a user finishes customizing PROJ."
  (let ((ov eieio-ede-old-variables)
	(nv (oref proj local-variables)))
    (setq eieio-ede-old-variables nil)
    (while ov
      (if (not (assoc (car (car ov)) nv))
	  (save-excursion
	    (mapcar (lambda (b)
		      (set-buffer b)
		      (kill-local-variable (car (car ov))))
		    (ede-project-buffers proj))))
      (setq ov (cdr ov)))
    (mapcar (lambda (b) (ede-set-project-variables proj b))
	    (ede-project-buffers proj))))

(defmethod eieio-done-customizing ((target ede-target))
  "Call this when a user finishes customizing TARGET."
  nil)


;;; EDE project target baseline methods.
;;
;;  If you are developing a new project type, you need to implement
;;  all of these methods, unless, of course, they do not make sense
;;  for your particular project.
;;
;;  Your targets should inherit from `ede-target', and your project
;;  files should inherit from `ede-project'.  Create the appropriate
;;  methods based on those below.

(defmethod project-interactive-select-target ((this ede-project) prompt)
  "Interactivly query for a target that exists in project THIS.
Argument PROMPT is the prompt to use when querying the user for a target."
  (let ((ob (object-assoc-list 'name (oref this targets))))
    (cdr (assoc (completing-read prompt ob nil t) ob))))

(defmethod project-add-file ((ot ede-target) file)
  "Add the current buffer into project project target OT.
Argument FILE is the file to add."
  (error "add-file not supported by %s" (object-name ot)))

(defmethod project-remove-file ((ot ede-target) fnnd)
  "Remove the current buffer from project target OT.
Argument FNND is an argument."
  (error "remove-file not supported by %s" (object-name ot)))

(defmethod project-edit-file-target ((ot ede-target))
  "Edit the target OT associated w/ this file."
  (find-file (oref (ede-current-project) file)))

(defmethod project-new-target ((proj ede-project))
  "Create a new target.  It is up to the project PROJ to get the name."
  (error "new-target not supported by %s" (object-name proj)))

(defmethod project-delete-target ((ot ede-target))
  "Delete the current target OT from it's parent project."
  (error "add-file not supported by %s" (object-name ot)))

(defmethod project-compile-project ((obj ede-project) &optional command)
  "Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling."
  (error "compile-project not supported by %s" (object-name obj)))

(defmethod project-compile-target ((obj ede-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (error "compile-target not supported by %s" (object-name obj)))

(defmethod project-debug-target ((obj ede-target))
  "Run the current project target OBJ in a debugger."
  (error "debug-target not supported by %s" (object-name obj)))

(defmethod project-make-dist ((this ede-project))
  "Build a distribution for the project based on THIS target."
  (error "Make-dist not supported by %s" (object-name this)))

;;; Default methods for EDE classes
;;
;; These are methods which you might want to override, but there is
;; no need to in most situations because they are either a) simple, or
;; b) cosmetic.

(defmethod ede-name ((this ede-target))
  "Return the name of THIS targt."
  (oref this name))

(defmethod ede-target-name ((this ede-target))
  "Return the name of THIS target, suitable for make or debug style commands."
  (oref this name))

(defmethod ede-name ((this ede-project))
  "Return a short-name for THIS project file.
Do this by extracting the lowest directory name."
  (oref this name))

(defmethod ede-description ((this ede-project))
  "Return a description suitible for the minibuffer about THIS."
  (format "Project %s: %d subprojects, %d targets."
	  (ede-name this) (length (oref this subproj))
	  (length (oref this targets))))

(defmethod ede-description ((this ede-target))
  "Return a description suitible for the minibuffer about THIS."
  (format "Target %s: with %d source files."
	  (ede-name this) (length (oref this source))))

(defmethod ede-convert-path ((this ede-project) path)
  "Convert path in a standard way for a given project.
Default to making it project relative.
Argument THIS is the project to convert PATH to."
  (let ((pp (file-name-directory (expand-file-name (oref this file))))
	(fp (expand-file-name path)))
    (if (string-match (regexp-quote pp) fp)
	(substring fp (match-end 0))
      (error "Cannot convert relativize path %s" fp))))

(defmethod ede-want-file-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  nil)

(defmethod ede-expand-filename ((this ede-project) filename)
  "Return a fully qualified file name based on project THIS.
FILENAME should be just a filename which occurs in a directory controlled
by this project."
  (let ((path (file-name-directory (oref this file)))
	(proj (oref this subproj))
	(found nil))
    (if (file-exists-p (concat path filename))
	(concat path filename)
      (while (and (not found) proj)
	(setq found (ede-expand-filename (car proj) filename)
	      proj (cdr proj)))
      found)))


;;; EDE project-autoload methods
;;
(defmethod ede-dir-to-projectfile ((this ede-project-autoload) dir)
  "Return a full file name of project THIS found in DIR.
Return nil if the project file does not exist."
  (let ((f (concat dir (oref this proj-file))))
    (and (file-exists-p f) f)))

;;; EDE basic functions
;;
(defun ede-directory-project-p (dir)
  "Return a project description object if DIR has a project.
This depends on an up to day `ede-project-class-files' variable."
  (let ((types ede-project-class-files)
	(ret nil))
    ;; Loop over all types, loading in the first type that we find.
    (while (and types (not ret))
      (if (ede-dir-to-projectfile (car types) dir)
	  (progn
	    ;; We found one!  Require it now since we will need it.
	    (require (oref (car types) file))
	    (setq ret (car types))))
      (setq types (cdr types)))
    ret))

(defun ede-up-directory (dir)
  "Return a path that is up one directory.
Argument DIR is the directory to trim upwards."
  (file-name-directory (substring dir 0 (1- (length dir)))))

(defun ede-toplevel-project (path)
  "Starting with PATH, find the toplevel project directory."
  (let ((toppath nil) (newpath nil))
    ;; Loop up to the topmost project, and then load that single
    ;; project, and it's sub projects.  When we are done, identify the
    ;; sub-project object belonging to file.
    (setq toppath path newpath path)
    (while (ede-directory-project-p newpath)
      (setq toppath newpath newpath (ede-up-directory toppath)))
    toppath))

(defun ede-load-project-file (file)
  "Project file independent way to read in FILE."
  (let* ((path (expand-file-name (file-name-directory file)))
	 (pfc (ede-directory-project-p path))
	 (projtoload nil)
	 (toppath nil)
	 (o nil))
    (if (or (not pfc) ede-constructing)
	nil
      (setq toppath (ede-toplevel-project path))
      ;; We found the top-most directory.  Check to see if we already
      ;; have an object defining it's project.
      (setq pfc (ede-directory-project-p toppath))
      ;; See if it's been loaded before
      (setq o (object-assoc (ede-dir-to-projectfile pfc toppath) 'file
			    ede-projects))
      (if (not o)
	  ;; If not, get it now.
	  (let ((ede-constructing t) (afo nil))
	    (setq o (funcall (oref pfc load-type) toppath))
	    (setq ede-projects (cons o ede-projects))))
      (let (tocheck found)
	;; Now find the project file belonging to FILE!
	(setq tocheck (list o))
	(setq file (ede-dir-to-projectfile pfc (expand-file-name path)))
	(while (and tocheck (not found))
	  (if (string= file (oref (car tocheck) file))
	      (setq found (car tocheck)))
	  (setq tocheck
		(append (cdr tocheck) (oref (car tocheck) subproj))))
	(if (not found)
	    (error "No project for %s, but passes project-p test" file))
	found))))

(defun ede-toplevel ()
  "Return the ede project which is the root of the current project."
  (let ((cp (ede-current-project)))
    (while (ede-parent-project cp)
      (setq cp (ede-parent-project cp)))
    cp))

(defun ede-parent-project (&optional obj)
  "Return the project belonging to the parent directory.
nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of."
  (if obj
      (ede-load-project-file (ede-up-directory (file-name-directory
						(oref obj file))))
    (ede-load-project-file (ede-up-directory default-directory))))

(defun ede-current-project ()
  "Return the current project file."
  (ede-load-project-file default-directory))

(defun ede-buffer-object (&optional buffer)
  "Return the target object for BUFFER."
  (if (not buffer) (setq buffer (current-buffer)))
  (let ((po (ede-current-project)))
    (if po (setq ede-object (ede-find-target po buffer))))
  (if (= (length ede-object) 1)
      (setq ede-object (car ede-object)))
  ede-object)

(defmethod ede-target-in-project-p ((proj ede-project) target)
  "Is PROJ the parent of TARGET?
If TARGET belongs to a subproject, return that project file."
  (if (member target (oref proj targets))
      proj
    (let ((s (oref proj subproj))
	  (ans nil))
      (while (and s (not ans))
	(setq ans (ede-target-in-project-p (car s) target))
	(setq s (cdr s)))
      ans)))

(defun ede-target-parent (target)
  "Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time."
  (let ((ans nil) (projs ede-projects))
    (while (and (not ans) projs)
      (setq ans (ede-target-in-project-p (car projs) target)
	    projs (cdr projs)))
    ans))

(defun ede-maybe-checkout (&optional buffer)
  "Check BUFFER out of VC if necessary."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (and buffer-read-only vc-mode
	     (y-or-n-p "Checkout Makefile.am from VC? "))
	(vc-toggle-read-only))))


;;; Project mapping
;;
(defun ede-project-buffers (project)
  "Return a list of all active buffers controlled by PROJECT.
This includes buffers controlled by a specific target of PROJECT."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if (and ede-object (eq (ede-current-project) project))
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-target-buffers (target)
  "Return a list of buffers that are controlled by TARGET."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if (if (listp ede-object)
		(member target ede-object)
	      (eq ede-object target))
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-buffers ()
  "Return a list of all buffers controled by an EDE object."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if ede-object
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-map-buffers (proc)
  "Execute PROC on all buffers controled by EDE."
  (mapcar proc (ede-buffers)))

(defmethod ede-map-project-buffers ((this ede-project) proc)
  "For THIS, execute PROC on all buffers belonging to THIS."
  (mapcar proc (ede-project-buffers this)))

(defmethod ede-map-target-buffers ((this ede-target) proc)
  "For THIS, execute PROC on all buffers belonging to THIS."
  (mapcar proc (ede-target-buffers this)))

;; other types of mapping
(defmethod ede-map-subprojects ((this ede-project) proc)
  "For object THIS, execute PROC on all subprojects."
  (mapcar proc (oref this subproj)))

(defmethod ede-map-targets ((this ede-project) proc)
  "For object THIS, execute PROC on all targets."
  (mapcar proc (oref this targets)))


;;; Project-local variables
;;
(defun ede-make-project-local-variable (variable &optional project)
  "Make VARIABLE project-local to PROJECT."
  (if (not project) (setq project (ede-current-project)))
  (if (assoc variable (oref project local-variables))
      nil
    (oset project local-variables (cons (list variable)
					(oref project local-variables)))
    (mapcar (lambda (b) (save-excursion
			  (set-buffer  b)
			  (make-local-variable variable)))
	    (ede-project-buffers project))))

(defun ede-set-project-variables (project &optional buffer)
  "Set variables local to PROJECT in BUFFER."
  (if (not buffer) (setq buffer (current-buffer)))
  (save-excursion
   (set-buffer buffer)
   (mapcar (lambda (v)
	     (make-local-variable (car v))
	     ;; set it's value here?
	     (set (car v) (cdr v))
	     )
	   (oref project local-variables))))

(defun ede-set (variable value)
  "Set the project local VARIABLE to VALUE.
If VARIABLE is not project local, just use set."
  (let ((p (ede-current-project)) a)
    (if (and p (setq a (assoc variable (oref p local-variables))))
	(progn
	  (setcdr a value)
	  (mapcar (lambda (b) (save-excursion
				(set-buffer b)
				(set variable value)))
		  (ede-project-buffers p)))
      (set variable value))
    (ede-commit-local-variables p))
  value)

(defmethod ede-commit-local-variables ((proj ede-project))
  "Commit change to local variables in PROJ."
  nil)


;;; Hooks & Autoloads
;;
;;  These let us watch various activities, and respond apropriatly.

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec ede-with-projectfile
	      (form def-body))))

(autoload 'ede-speedbar "ede-speedbar" "Run speedbar in EDE project mode." t)

(provide 'ede)

;;; ede.el ends here
