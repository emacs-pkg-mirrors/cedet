;;; project-am.el --- A project management scheme based on automake files.

;;;  Copyright (C) 1998  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.1
;; Keywords: project, make

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
;; The GNU Automake tool is the first step towards having a really
;; good project management system.  It provides a simple and concise
;; look at what is actually in a project, and records it in a simple
;; fashion.
;;
;; project-am uses the structure defined in all good GNU projects with
;; the Automake file as it's base template, and then maintains that
;; information during edits, automatically updating the automake file
;; where appropriate.

;;; Install
;;
;;  project-am depends on:
;;   eieio and make-mode.  It will use compile, gud, and speedbar.
;;
;;  This command enables project mode on all files.
;;
;;  (global-project-am-mode t)

;;; Adding new target objects
;;
;; To add a new target object, create an object that inherits from
;; either `project-am-target', or some other more similar object.
;; Next, add your type to `project-am-type-alist' which is used to
;; make associations.  Lastly, make sure that the following methods
;; are defined for it:
;;
;; `project-am-rescan'  - Rescan the makefile for contents.  Also used to
;;                        first initialize this object.
;; `project-am-macro'   - Return the name for a macro used in the Makefile
;;                        for holding the source files, or whatever.
;; `project-am-buffer-mine' - Return t if a passed in buffer belongs to your
;;                        target object.
;; `project-am-name'    - The name used.  (Not necessary unless you don't
;;                        want to use the name originally given.)
;; `project-am-compile-target-command' - A string which is the target passed
;;                        to make when building your target.
;;
;; For speedbar support of your object, define these:
;; `project-am-sb-button' - Create a button representing your object.
;; `project-am-sb-expand' - Create the list of sub-buttons under your button
;;                          when it is expanded.

;;; History:
;; 

(require 'makefile "make-mode")
(require 'eieio)
(eval-when-compile (require 'compile) (require 'speedbar))

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

;; customization stuff
(defgroup project-am nil
  "File and tag browser frame."
  :group 'tools
;  :group 'tools
;  :group 'convenience
;  :version "20.3"
  )

(defcustom project-am-compile-project-command "make -k"
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-compile-target-command "make -k %s"
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-debug-target-function 'gdb
  "*Default Emacs command used to debug a target."
  :group 'project-am
  :type 'sexp) ; make this be a list some day

(defconst project-am-type-alist
  '(("bin" project-am-program "bin_PROGRAMS")
    ("sbin" project-am-program "sbin_PROGRAMS")
    ("lib" project-am-lib "noinst_LIBS")
    ("texinfo" project-am-texinfo "texinfo_TEXINFOS")
    ("man" project-am-man "man_MANS")
    ("lisp" project-am-lisp "lisp_LISP"))
  "Alist of type names and the type of object to create for them.")


(defvar project-am-projects nil
  "A list of all active projects currently loaded in Emacs.")

(defvar project-am-object nil
  "Local variable defining a makefile object for a given Makefile.am.")

(defvar project-am-constructing nil
  "Non nil when constructing a project hierarchy.")

(defvar project-am-deep-rescan nil
  "Non nil means scan down a tree, otherwise rescans are top level only.
Do not set this to non-nil globally.  It is used internally.")

(defclass project-am-target ()
  ((name :initarg :name :docstring "Name of this target.")
   (path :initarg :path :docstring "The path to this target.")
   )
  "A top level target to build.")

(defclass project-am-objectcode (project-am-target)
  ((source :initarg :source :docstring "List of source files."))
  "A target which creates object code, like a C program or library.")

(defclass project-am-program (project-am-objectcode)
  ((ldadd :initarg :ldadd :docstring "Additional LD args."
	  :initform nil))
  "A top level program to build")

(defclass project-am-lib (project-am-objectcode)
  nil
  "A top level library to build")

(defclass project-am-lisp (project-am-target)
  ((lisp :initarg :lisp :docstring "List of lisp files to build."))
  "A group of Emacs Lisp programs to byte compile.")

(defclass project-am-texinfo (project-am-target)
  ((include :initarg :include
	    ;; We cheat here.  I happen to know that THIS
	    ;; is defined in eieio.
	    :initform nil
	    :docstring "Additional texinfo included in this one."))
  "A top level texinfo file to build.")

(defclass project-am-man (project-am-target)
  nil
  "A top level man file to build.")

(defclass project-am-makefile ()
  ((file :initarg :file
	 :initform nil
	 :docstring "File name where this object is derived.")
   (root :initarg :root
	 :initform nil
	 :docstring "The root project file.")
   (subproj :initarg :subproj
	    :initform nil
	    :docstrings "More project files build by this project.")
   (targets :initarg :targets
	     :initform nil
	     :docstring "Top level targets in this makefile.")
   )
   "Encode one makefile.")

;;; Code:
(defun project-am (project)
  "Load in information from a directory PROJECT."
  (interactive "DProject Dir: ")
  (project-am-load project)
  ;; Fork of speedbar??  Some other gui?
  )

(defun project-am-maybe-install ()
  "Check the current buffer to see if we should install `project-am' stuff."
  (if (not project-am-constructing)
      (if (file-exists-p "Makefile.am")
	  (let ((amf (project-am-minor-mode 1)))
	    (make-local-variable 'project-am-object)
	    (setq project-am-object
		  (project-am-buffer-object amf (current-buffer)))))))

(defun global-project-am-mode (arg)
  "Turn on `project-am' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise."
  (interactive "p")
  (if (not arg)
      (if (member 'project-am-maybe-install find-file-hooks)
	  (global-project-am-mode -1)
	(global-project-am-mode 1))
    (if (or (eq arg t) (> arg 0))
	(add-hook 'find-file-hooks 'project-am-maybe-install)
      (remove-hook 'find-file-hooks 'project-am-maybe-install))))

;;; Important macros for doing commands.
;;
(defmacro project-am-with-projectfile (obj &rest forms)
  "For the project in which OBJ resides, execute FORMS."
  (list 'save-window-excursion
	(list 'let* (list (list 'pf (list 'project-am-load
					  (list 'oref obj :path)))
			  '(dbka (get-file-buffer (oref pf :file))))
	      '(if (not dbka) (find-file (oref pf :file))
		 (switch-to-buffer dbka))
	      (cons 'progn forms)
	      '(if (not dbka) (kill-buffer (current-buffer))))))
(put 'project-am-with-projectfile 'lisp-indent-function 1)

(defvar project-am-minor-mode nil
  "Non-nil in `emacs-lisp-mode' for automatic documentation checking.")
(make-variable-buffer-local 'project-am-minor-mode)

;; We don't want to waste space.  There is a menu after all.
(add-to-list 'minor-mode-alist '(project-am-minor-mode ""))

(defvar project-am-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "e" 'project-am-edit-file-target)
    (define-key pmap "a" 'project-am-add-file)
    (define-key pmap "d" 'project-am-remove-file)
    (define-key pmap "t" 'project-am-new-target)
    (define-key pmap "g" 'project-am-rescan-toplevel)
    (define-key pmap "s" 'project-speedbar)
    (define-key pmap "l" 'project-am-load-project-file)
    (define-key pmap "C" 'project-am-compile-project)
    (define-key pmap "c" 'project-am-compile-target)
    (define-key pmap "d" 'project-am-debug-target)
    ;; bind our submap into map
    (define-key map "\C-c." pmap)
    map)
  "Keymap used in project minor mode.")

(if project-am-minor-keymap
    (easy-menu-define
     project-am-minor-menu project-am-minor-keymap "Project Minor Mode Menu"
     '("Project"
       [ "Load a project" project-am-load-project-file t ]
       [ "Speedbar Project" project-speedbar t ]
       [ "Rescan Project Files" project-am-rescan-toplevel t ]
       "---"
       [ "Create New Target" project-am-new-target t ]
       [ "Add to Target" project-am-add-file (not project-am-object) ]
       [ "Remove from Targets" project-am-remove-file
	 (and project-am-object
	      (not (project-am-makefile-p project-am-object))) ]
       [ "Modify Target" project-am-edit-file-target
	 (and project-am-object
	      (not (project-am-makefile-p project-am-object))) ]
       "---"
       [ "Compile project" project-am-compile-project t ]
       [ "Compile target" project-am-compile-target t ]
       [ "Debug target" project-am-debug-target
	 (and project-am-object
	      (obj-of-class-p project-am-object project-am-program)) ]
       "---"
       [ "Make distribution" project-am-make-dist t ]
       )))

;; Allow re-insertion of a new keymap
(let ((a (assoc 'project-am-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a project-am-minor-keymap)
    (add-to-list 'minor-mode-map-alist
		 (cons 'project-am-minor-mode
		       project-am-minor-keymap))))

(defun project-am-minor-mode (&optional arg)
  "Project Automake minor mode.
If this file is contained, or could be contained in an automake
controlled project, then this mode should be active.

Project Automake will manage your automake files, and make it very
easy to edit your automake files.

With argument ARG positive, turn on the mode.  Negative, turn off the
mode.  nil means to toggle the mode."
  (setq project-am-minor-mode
	(not (or (and (null arg) project-am-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (if (and project-am-minor-mode (not project-am-constructing))
      (project-am-load default-directory)))

;;; Minor Mode commands
;;
(defun project-am-load-project-file (file)
  "Find and load a project FILE.
A project file is Makefile.am"
  (interactive "FAutomake File: ")
  (if (string-match "Makefile\\.am")
      (find-file file)
    (error "Not an automake file")))

(defun project-am-add-file (target)
  "Add the current buffer into a project.
Argument TARGET is the name of the target to add this file to.
If TARGET does not exists then create a new target."
  (interactive (if project-am-object (error "Already assocated w/ a target")
		 (let ((amf (project-am-load default-directory)))
		   (if (not amf) (error "No project file"))
		   (list
		    (completing-read "Target: "
				     (object-assoc-list :name
							(oref amf :targets))
				     nil t)))))
  ;; The input target might be new.  See if we can find it.
  (let* ((amf (project-am-load default-directory))
	 (ot (object-assoc target :name (oref amf :targets)))
	 (ofn (file-name-nondirectory (buffer-file-name))))
    (if (not ot)
	(setq ot
	      (project-am-new-target
	       target (project-am-preferred-target-type (buffer-file-name)))))
    (project-am-with-projectfile ot
      (makefile-move-to-macro (project-am-macro ot))
      (if (and buffer-read-only vc-mode
	       (y-or-n-p "Checkout Makefile.am from VC? "))
	  (vc-toggle-read-only))
      (makefile-end-of-command)
      (insert " " ofn)
      (makefile-fill-paragraph nil)
      (project-am-rescan ot)
      (save-buffer))
    (setq project-am-object ot)))

(defun project-am-remove-file ()
  "Remove the current buffer from any project targets."
  (interactive)
  (let ((ot project-am-object)
	(fnnd (file-name-nondirectory (buffer-file-name))))
    (if (not ot) (error "File %s is not in a target" fnnd))
    (project-am-with-projectfile ot
      (makefile-move-to-macro (project-am-macro ot))
      (if (and buffer-read-only vc-mode
	       (y-or-n-p "Checkout Makefile.am from VC? "))
	  (vc-toggle-read-only t))
      (makefile-navigate-macro (concat " *" (regexp-quote fnnd)))
      (replace-match "" t t nil 0)
      (makefile-fill-paragraph nil)
      (project-am-rescan ot)
      (save-buffer))
    (setq project-am-object nil)))

(defun project-am-edit-file-target ()
  "Edit the target associated w/ this file."
  (interactive)
  (let ((obj project-am-object))
    (find-file (concat (oref obj :path) "Makefile.am"))
    (goto-char (point-min))
    (makefile-move-to-macro (project-am-macro obj))
    (if (= (point-min) (point))
	(re-search-forward (project-am-name obj)))))

(defun project-am-new-target (name type)
  "Create a new target named NAME.
Argument TYPE is the type of target to insert.  This is a string
matching something in `project-am-type-alist' or type class symbol."
  (interactive (list
		(read-string "Name: "
			     (if project-am-object
				 (project-am-name project-am-object)
			       ""))
		(completing-read "Type: "
				 project-am-type-alist
				 nil t
				 (cond ((eq major-mode 'texinfo-mode)
					"texinfo")
				       ((eq major-mode 'nroff-mode)
					"man")
				       ((eq major-mode 'emacs-lisp-mode)
					"lisp")
				       (t "bin")))))
  
  (let ((ntype (assoc type project-am-type-alist))
	(ot nil))
    (setq ot (apply (car (cdr ntype)) name :name name
		    :path (expand-file-name default-directory) nil))
    (if (not ot) (error "Error creating target object %S" ntype))
    (project-am-with-projectfile ot
      (goto-char (point-min))
      (makefile-next-dependency)
      (if (= (point) (point-min))
	  (goto-char (point-max))
	(beginning-of-line)
	(insert "\n")
	(forward-char -1))
      ;; Add the new target sources macro (if needed)
      (if (project-am-macro ot)
	  (makefile-insert-macro (project-am-macro ot)))
      ;; Add to the list of objects.
      (goto-char (point-min))
      (makefile-move-to-macro (car (cdr (cdr ntype))))
      (if (= (point) (point-min))
	  (progn
	    (if (re-search-forward makefile-macroassign-regex nil t)
		(progn (forward-line -1)
		       (end-of-line)
		       (insert "\n"))
	      ;; If the above search fails, thats ok.  We'd just want to be at
	      ;; point-min anyway.
	      )
	    (makefile-insert-macro (car (cdr (cdr ntype))))))
      (makefile-end-of-command)
      (insert " " (project-am-name ot))
      (save-buffer)
      ;; Rescan the object in this makefile.
      (project-am-rescan project-am-object))))

(defun project-am-rescan-toplevel ()
  "Rescan all projects in which the current buffer resides."
  (interactive)
  (let* ((tlof (project-am-find-topmost-level default-directory))
	 (tlo (project-am-load tlof))
	 (project-am-deep-rescan t))  ; scan deep in this case.
    ;; tlo is the top level object for whatever file we are in
    ;; or nil.  If we have an object, call the rescan method.
    (if tlo (project-am-rescan tlo))))

(defun project-am-compile-project (command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (interactive
   ;; This interactive statement was taken from compile, and I'll
   ;; use the same command history too.
   (progn
     (require 'compile)
     (if (or compilation-read-command current-prefix-arg)
	 (list (read-from-minibuffer "Project compile command: "
				     ;; hardcode make -k
				     ;; This is compile project after all.
				     project-am-compile-project-command nil nil
				     '(compile-history . 1)))
       (list project-am-compile-project-command))))
  ;; When compile a project, we might be in a subdirectory,
  ;; so we have to make sure we move all the way to the top.
  (let* ((tlof (project-am-find-topmost-level default-directory))
	 (default-directory (file-name-nondirectory tlof)))
    (compile command)))

(defun project-am-compile-target (command)
  "Compile the current target.
Argument COMMAND is the command to use for compiling the target."
  (interactive
   ;; This interactive statement was taken from compile, and I'll
   ;; use the same command history too.
   (progn
     (require 'compile)
     (if (or compilation-read-command current-prefix-arg)
	 (list (read-from-minibuffer "Project compile command: "
				     ;; hardcode make -k
				     ;; This is compile project after all.
				     (if project-am-object
					 project-am-compile-project-command
				       (format
					project-am-compile-target-command
					(project-am-compile-target
					 project-am-object)))
				     nil nil
				     '(compile-history . 1)))
       (list (if project-am-object
		 project-am-compile-project-command
	       (format
		project-am-compile-target-command
		(project-am-compile-target-command project-am-object)))))))
  ;; We better be in the right place when compiling a specific target.
  (compile command))

(defun project-am-debug-target ()
  "Run the current project in the debugger."
  (interactive)
  (let ((tb (get-buffer-create " *padt*"))
	(dd default-directory)
	(obj project-am-object)
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name project-am-debug-target-function)
			     " " (project-am-name obj))))
	  (funcall project-am-debug-target-function cmd))
      (kill-buffer tb))))

(defun project-am-make-dist ()
  "Run the current project in the debugger."
  (interactive)
  (project-am-compile-project (concat project-am-compile-project-command
				      " dist")))

;;; Project loading and saving
;;
(defun project-am-load (project)
  "Read an automakefile PROJECT into our data structure.
Make sure that the tree down to our makefile is complete so that there
is cohesion in the project.  Return the project file (or sub-project).
If a given set of projects has already been loaded, then do nothing
but return the project for the directory given."
  (setq project (expand-file-name project))
  (let* ((project-am-constructing t)
	 (fn (project-am-find-topmost-level
	      (if (string-match "/$" project)
		  project
		(concat project "/"))))
	 (amo nil)
	 (trimmed (if (string-match (regexp-quote fn)
				    project)
		      (replace-match "" t t project)
		    ""))
	 (subdir nil))
    (setq amo (object-assoc (concat fn "Makefile.am")
			    :file project-am-projects))
    (if amo
	nil
      (let ((project-am-constructiong t))
	(setq amo (project-am-load-makefile fn)))
      (if (not amo)
	  nil
	(setq project-am-projects (cons amo project-am-projects))))
    (if (not amo)
	nil
      ;; Now scan down from amo, and find the current directory
      ;; from the PROJECT file.
      (while (< 0 (length trimmed))
	(if (string-match "\\([a-zA-Z0-9.-]+\\)/" trimmed)
	    (setq subdir (match-string 0 trimmed)
		  trimmed (replace-match "" t t trimmed))
	  (error "Error scanning down path for project"))
	(setq amo (project-am-subtree amo (concat fn subdir "Makefile.am"))
	      fn (concat fn subdir)))
      amo)))

(defun project-am-find-topmost-level (path)
  "Find the topmost automakefile starting with PATH."
  (let ((newpath path))
    (while (file-exists-p (concat newpath "Makefile.am"))
      (setq path newpath newpath
	    (file-name-directory (substring path 0 (1- (length path))))))
    (expand-file-name path)))

(defun project-am-load-makefile (path)
  "Converts PATH into a project Makefile, and return it's object object.
It does not check for existing project objects.  Use `project-am-load'."
  (let* ((fn (expand-file-name (concat path "Makefile.am")))
	 (kb (get-file-buffer fn)))
    (if (not (file-exists-p fn))
	nil
      (save-excursion
	(set-buffer (find-file-noselect fn))
	(prog1
	    (if (and project-am-object (project-am-makefile-p project-am-object))
		project-am-object
	      (let ((ampf (project-am-makefile (project-am-last-dir fn)
					       :file fn)))
		(project-am-rescan ampf)
		(make-local-variable 'project-am-object)
		(setq project-am-object ampf)
		ampf))
	  ;; If the buffer was not already loaded, kill it.
	  (if (not kb) (kill-buffer (current-buffer))))))))

;;; Methods:
(defmethod project-am-rescan ((this project-am-makefile))
  "Rescan the makefile for all targets and sub targets."
  (let ((osubproj (oref this :subproj))
	(otargets (oref this :targets))
	(csubproj (or
		   ;; If DIST_SUBDIRS doesn't exist, then go for the
		   ;; static list of SUBDIRS.  The DIST version should
		   ;; contain SUBDIRS plus extra stuff.
		   (makefile-macro-file-list "DIST_SUBDIRS")
		   (makefile-macro-file-list "SUBDIRS")))
	(nsubproj nil)
	;; Targets are excluded here because they require
	;; special attention.
	(ntargets nil)
	(tmp nil)
	;; Here are target prefixes as strings
	(tp '(("bin_PROGRAMS" . project-am-program)
	      ("sbin_PROGRAMS" . project-am-program)
	      ("noinst_LIBRARIES" . project-am-lib)
	      ("info_TEXINFOS" . project-am-texinfo)
	      ("man_MANS" . project-am-man)))
	(path (expand-file-name default-directory))
	)
    (mapcar
     ;; Map all tye different types
     (lambda (typecar)
       ;; Map all the found objects
       (mapcar (lambda (lstcar)
		 (setq tmp (object-assoc lstcar :name otargets))
		 (if (not tmp)
		     (setq tmp (apply (cdr typecar) lstcar
				      :name lstcar
				      :path path nil)))
		 (project-am-rescan tmp)
		 (setq ntargets (cons tmp ntargets)))
	       (makefile-macro-file-list (car typecar))))
     tp)
    ;; LISP is different.  Here there is only one kind of lisp (that I know of
    ;; anyway) so it doesn't get mapped when it is found.
    (if (makefile-move-to-macro "lisp_LISP")
	(let ((tmp (project-am-lisp "lisp"
				    :name "lisp"
				    :path path)))
	  (project-am-rescan tmp)
	  (setq ntargets (cons tmp ntargets))))
    ;; Now that we have this new list, chuck the old targets
    ;; and replace it with the new list of targets I just created.
    (oset this :targets (nreverse ntargets))
    ;; We still have a list of targets.  For all buffers, make sure
    ;; their object still exists!

    ;; FIGURE THIS OUT

    ;; Ok, now lets look at all our sub-projects.
    (mapcar (lambda (sp)
	      ;; For each project id found, see if we need to recycle,
	      ;; and if we do not, then make a new one.  Check the deep
	      ;; rescan value for behavior patterns.
	      (setq tmp (object-assoc
			 (concat default-directory sp "/Makefile.am")
			 :file osubproj))
	      (if (not tmp)
		  ;; No tmp?  Create a new one.  Don't bother with
		  ;; non-deep business since we need this object.
		  (setq tmp (project-am-load-makefile
			     (concat (file-name-directory (oref this :file))
				     sp "/")))
		;; If we have tmp, then rescan it only if deep mode.
		(if project-am-deep-rescan
		    (project-am-rescan tmp)))
	      ;; Tac tmp onto our list of things to keep
	      (setq nsubproj (cons tmp nsubproj)))
	    csubproj)
    (oset this :subproj nsubproj)
    ;; All elements should be updated now.
    ))

(defmethod project-am-rescan ((this project-am-program))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this)))
  (oset this :ldadd (makefile-macro-file-list
		     (concat (oref this :name) "_LDADD"))))

(defmethod project-am-rescan ((this project-am-lib))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this))))

(defmethod project-am-rescan ((this project-am-texinfo))
  "Rescan object THIS."
  (oset this :include (makefile-macro-file-list (project-am-macro this))))

(defmethod project-am-rescan ((this project-am-man))
  "Rescan object THIS."
  )

(defmethod project-am-rescan ((this project-am-lisp))
  "Rescan the lisp sources."
  (oset this :lisp (makefile-macro-file-list (project-am-macro this))))

(defmethod project-am-macro ((this project-am-objectcode))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_SOURCES"))

(defmethod project-am-macro ((this project-am-texinfo))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_TEXINFOS"))

(defmethod project-am-macro ((this project-am-man))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_MANS"))

(defmethod project-am-macro ((this project-am-lisp))
  "Return the default macro to 'edit' for this object."
  "lisp_LISP")

(defun project-am-buffer-object (amf buffer)
  "Return an object starting with AMF associated with BUFFER.
nil means that this buffer belongs to no-one."
  (if (not amf)
      nil
    (if (project-am-buffer-mine amf buffer)
	amf
      (let ((targ (oref amf :targets))
	    (sobj (oref amf :subproj))
	    (obj nil))
	(while (and targ (not obj))
	  (if (project-am-buffer-mine (car targ) buffer)
	      (setq obj (car targ)))
	  (setq targ (cdr targ)))
	(while (and sobj (not obj))
	  (setq obj (project-am-buffer-object (car sobj) buffer)
		sobj (cdr sobj)))
	obj))))
  
(defmethod project-am-buffer-mine ((this project-am-makefile) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :file) (expand-file-name (buffer-file-name buffer))))

(defmethod project-am-buffer-mine ((this project-am-objectcode) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-name-nondirectory (buffer-file-name buffer))
	  (oref this :source)))

(defmethod project-am-buffer-mine ((this project-am-texinfo) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((bfn (buffer-file-name buffer)))
    (or (string= (oref this :name)  bfn)
	(member (file-name-nondirectory bfn) (oref this :include)))))
	
(defmethod project-am-buffer-mine ((this project-am-man) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :name) (buffer-file-name buffer)))

(defmethod project-am-buffer-mine ((this project-am-lisp) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-name-nondirectory (buffer-file-name buffer))
	  (oref this :lisp)))

; Resurect this if it's needed.
;(defun project-am-buffer-project (buffer)
;  "Return the project associated with BUFFER."
;  (project-am-load (save-excursion
;		     (set-buffer buffer)
;		     default-directory)))

(defmethod project-am-set-buffer ((ampf project-am-makefile))
  "Set the current buffer to this project file."
  (set-buffer (find-file-noselect (oref ampf :file))))

(defmethod project-am-subtree ((ampf project-am-makefile) subpath)
  "Return the sub project in AMPF specified by SUBPATH."
  (object-assoc (expand-file-name subpath) :file (oref ampf subproj)))

(defmethod project-am-name ((this project-am-makefile))
  "Return a short-name for this project file.
Do this by extracting the lowest directory name."
  (project-am-last-dir (oref this :file)))

(defmethod project-am-name ((this project-am-target))
  "Return the name of this targt."
  (oref this :name))

(defmethod project-am-compile-terget-command ((this project-am-target))
  "Default target to use when compiling a given target."
  ;; This is a pretty good default for most.
  "")

(defmethod project-am-compile-target-command ((this project-am-objectcode))
  "Default target to use when compiling an object code target."
  (oref this :name))

(defmethod project-am-compile-target-command ((this project-am-texinfo))
  "Default target t- use when compling a texinfo file."
  (let ((n (oref this :name)))
    (if (string-match "\\.texi?\\(nfo\\)?" n)
	(setq n (replace-match ".info" t t n)))
    n))

;;; Speedbar support mode
;;
(defvar project-am-speedbar-key-map nil
  "Keymap used when working with a project in speedbar.")

(if project-am-speedbar-key-map
    nil
  (setq project-am-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing pleasure...
  (define-key project-am-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key project-am-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key project-am-speedbar-key-map "-" 'speedbar-contract-line)
  )

(defvar project-am-speedbar-menu
  ()
  "Menu part in easymenu format that is used in speedbar while in `project-am' mode.")

(defun project-speedbar ()
  "Red Hat Package Management in Emacs."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Make sure our special speedbar major mode is loaded
  (speedbar-add-expansion-list '("project"
				 project-am-speedbar-menu
				 project-am-speedbar-key-map
				 project-am-speedbar))
  ;; Now, throw us into PROJECT-AM mode on speedbar.
  (speedbar-change-initial-expansion-list "project")
  ;; Now flip over to the speedbar frame
  (speedbar-get-focus)
  )


(defun project-am-speedbar (dir-or-object depth)
  "Create buttons in speedbar that represents the current project.
DIR-OR-OBJECT is the object to expand, or nil, and DEPTH is the current
expansion depth."
  (let ((obj (if (stringp dir-or-object)
		 ;; For any project ALWAYS start at the top
		 ;; when we first start.  (When we have a string)
		 (project-am-load (project-am-find-topmost-level
				   dir-or-object))
	       dir-or-object)))
    (if (not obj)
	(speedbar-make-tag-line nil nil nil nil "No Project" nil nil
				nil (1+ depth))
      ;; Depend on the object method for expansion rules.
      (project-am-sb-expand obj depth))))

;;; Speedbar Project Methods
;;
(defmethod project-am-sb-button ((this project-am-makefile) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this
			  (project-am-name this)
			  'project-am-file-find
			  (oref this :file)
			  'speedbar-directory-face depth))

(defmethod project-am-sb-button ((this project-am-program) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this (project-am-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-lib) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this (project-am-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-texinfo) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?+
			  'project-am-object-expand
			  this
			  (project-am-name this)
			  'project-am-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-man) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?? nil nil
			  (project-am-name this)
			  'project-am-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-lisp) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this (project-am-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defun project-am-object-expand (text token indent)
  "Expand an object.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentation level.
This function will maintain the state of the +,- and call the objects'
method for the actual text."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((speedbar-tag-hierarchy-method '(sort)))
	       (project-am-sb-expand token (1+ indent))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defmethod project-am-sb-expand ((this project-am-makefile) depth)
  "Expand THIS logically at DEPTH."
  (let* ((subproj nil)
	 (targets nil))
    (setq subproj (oref this :subproj))
    (setq targets (oref this :targets))
    (if (and (= 1 (+ (length subproj) (length targets)))
	     (string= (project-am-name this)
		      (project-am-name (or (car subproj)
					   (car targets)))))
	;; If there is only one target, and it has the same name
	;; as the directory, then expand that target instead.
	(project-am-sb-expand (or (car subproj) (car targets)) depth)
      (while subproj
	(project-am-sb-button (car subproj) depth)
	(setq subproj (cdr subproj)))
      (while targets
	(project-am-sb-button (car targets) depth)
	(setq targets (cdr targets))))))

(defmethod project-am-sb-expand ((this project-am-objectcode) depth)
  "Expand node describing something built into objectcode.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :source)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'project-am-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'project-am-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defmethod project-am-sb-expand ((this project-am-texinfo) depth)
  "Expand node describing a texinfo manual.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((includes (oref this :include)))
    (while includes
      (speedbar-make-tag-line 'bracket ?+
			      'project-am-tag-file
			      (concat (oref this :path)
				      (car includes))
			      (car includes)
			      'project-am-file-find
			      (concat
			       (oref this :path)
			       (car includes))
			      'speedbar-file-face depth)
      (setq includes (cdr includes)))
    ;; Not only do we show the included files (for future expansion)
    ;; but we also want to display tags for this file too.
    (project-am-create-tag-buttons (concat (oref this :path)
					   (oref this :name))
				   depth)))

(defmethod project-am-sb-expand ((this project-am-lisp) depth)
  "Expand node describing lisp code.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :lisp)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'project-am-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'project-am-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defun project-am-file-find (text token indent)
  "Find the file TEXT at path TOKEN.
INDENT is the current indentation level."
  (speedbar-find-file-in-frame token))

(defun project-am-create-tag-buttons (filename indent)
  "Create the tag buttons associated with FILENAME at INDENT."
  (let* ((lst (if speedbar-use-imenu-flag
		  (let ((tim (speedbar-fetch-dynamic-imenu filename)))
		    (if (eq tim t)
			(speedbar-fetch-dynamic-etags filename)
		      tim))
		(speedbar-fetch-dynamic-etags filename))))
    ;; if no list, then remove expando button
    (if (not lst)
	(speedbar-change-expand-button-char ??)
      (speedbar-with-writable
	;; We must do 1- because indent was already incremented.
	(speedbar-insert-generic-list (1- indent)
				      lst
				      'project-am-tag-expand
				      'project-am-tag-find)))))

(defun project-am-tag-file (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (project-am-create-tag-buttons token (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun project-am-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (speedbar-insert-generic-list indent token
					   'project-am-tag-expand
					   'project-am-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun project-am-tag-find (text token indent)
  "For the tag TEXT in a file TOKEN, goto that position.
INDENT is the current indentation level."
  (let ((file (save-excursion
		(while (not (re-search-forward "[]] [^ ]"
					       (save-excursion (end-of-line)
							       (point))
					       t))
		  (re-search-backward (format "^%d:" (1- indent)))
		  (setq indent (1- indent)))
		(get-text-property (point) 'speedbar-token))))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)
    (goto-char token)
    (run-hooks 'speedbar-visiting-tag-hook)
    ;;(recenter)
    (speedbar-maybee-jump-to-attached-frame)
    ))

;;; Generic useful functions

(defun project-am-last-dir (file)
  "Return the last part of a directory name.
Argument FILE is the file to extract the end directory name from."
  (let ((s (file-name-directory file)))
    (string-match "/\\([a-zA-Z0-9_]+\\)/$" s)
    (match-string 1 s)))

(defun project-am-preferred-target-type (file)
  "For FILE, return the preferred type for that file."
  (cond ((string-match "\\.texi?\\(nfo\\)$" file)
	 project-am-texinfo)
	((string-match "\\.[0-9]$" file)
	 project-am-man)
	((string-match "\\.el$" file)
	 project-am-lisp)
	(t
	 project-am-program)))


;;; Makefile editing and scanning commands
;;
;; Formatting of a makefile
;;
;; 1) Creating an automakefile, stick in a top level comment about
;;    being created by emacs
;; 2) Leave order of variable contents alone, except for SOURCE
;;    SOURCE always keep in the order of .c, .h, the other stuff.

;; personal reference until I'm done
; makefile-fill-paragraph -- refill a macro w/ backslashes
; makefile-insert-macro -- insert "foo = "

(defun makefile-beginning-of-command ()
  "Move the the beginning of the current command."
  (interactive)
  (if (save-excursion
	(forward-line -1)
	(makefile-line-continued-p))
      (forward-line -1))
  (beginning-of-line)
  (if (not (makefile-line-continued-p))
      nil
    (while (and (makefile-line-continued-p)
		(not (bobp)))
      (forward-line -1))
    (forward-line 1)))

(defun makefile-end-of-command ()
  "Move the the beginning of the current command."
  (interactive)
  (end-of-line)
  (while (and (makefile-line-continued-p)
	      (not (eobp)))
    (forward-line 1)
    (end-of-line)))

(defun makefile-line-continued-p ()
  "Return non-nil if the current line ends in continuation."
  (save-excursion
    (end-of-line)
    (= (preceding-char) ?\\)))

;;; Programatic editing of a Makefile
;;
(defun makefile-move-to-macro (macro)
  "Move to the definition of MACRO.  Return t if found."
  (let ((oldpt (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" macro "\\s-*=") nil t)
	t
      (goto-char oldpt)
      nil)))

(defun makefile-navigate-macro (stop-before)
  "In a list of files, move forward until STOP-BEFORE is reached.
STOP-BEFORE is a regular expression matching a file name."
  (save-excursion
    (makefile-beginning-of-command)
    (let ((e (save-excursion
	       (makefile-end-of-command)
	       (point))))
      (if (re-search-forward stop-before nil t)
	  (goto-char (match-beginning 0))
	(goto-char e)))))

(defun makefile-macro-file-list (macro)
  "Return a list of all files in MACRO."
  (save-excursion
    (if (makefile-move-to-macro macro)
	(let ((e (save-excursion
		   (makefile-end-of-command)
		   (point)))
	      (lst nil))
	  (while (re-search-forward "\\s-**\\([-a-zA-Z0-9./_@$%()]+\\)\\s-*" e t)
	    (setq lst (cons
		       (buffer-substring-no-properties
			(match-beginning 1)
			(match-end 1))
		       lst)))
	  (nreverse lst)))))

;;; Hooks
;;
;;  These let us watch various activities, and respond apropriatly.

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec project-am-with-projectfile
	      (form def-body))))

(provide 'project-am)

;;; project-am.el ends here
