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
;; where apropriate.

;;; History:
;; 

(require 'speedbar)
(require 'makefile "make-mode")
(require 'eieio)

(defvar project-am-projects nil
  "A list of all active projects currently loaded in Emacs.")

(defvar project-am-object nil
  "Local variable defining a makefile object for a given Makefile.am.")

(defvar project-am-constructing nil
  "Non nil when constructing a project hierarchy.")

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

(defclass project-am-texinfo (project-am-target)
  ((include :initarg :include
	    ;; We cheat here.  I happen to know that THIS
	    ;; is defined in eieio.
	    :initform nil
	    :docstring "Additional texinfo included in this one."))
  "A top level texinfo file to build.")

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
  "Check the current buffer to see if we should install project-am stuff."
  (if (file-exists-p "Makefile.am")
      (project-am-minor-mode 1)))

(defvar project-am-minor-mode nil
  "Non-nil in `emacs-lisp-mode' for automatic documentation checking.")
(make-variable-buffer-local 'project-am-minor-mode)

;; We don't want to waste space.  There is a menu after all.
(add-to-list 'minor-mode-alist '(project-am-minor-mode ""))

(defvar project-am-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    
    ;; bind our submap into map
    (define-key map "\C-c?" pmap)
    map)
  "Keymap used in project minor mode.")

(if project-am-minor-keymap
    (easy-menu-define
     project-am-minor-menu project-am-minor-keymap "Project Minor Mode Menu"
     '("Project"
       [ "Load a project" project-am t ]
       [ "Speedbar Project" project-speedbar t ]
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
  (if (not project-am-constructing)
      (project-am-load default-directory)))

;;; Project loading and saving
;;
(defun project-am-load (project)
  "Read an automakefile PROJECT into our data structure.
Make sure that the tree down to our makefile is complete so that there
is cohesion in the project.  Return the project file (or sub-project)."
  (let* ((project-am-constructing t)
	 (fn (project-am-find-topmost-level
	      (if (string-match "/$" project)
		  project
		(concat project "/"))))
	 (amo nil)
	 (trimmed (if (string-match (regexp-quote fn) project)
		      (replace-match "" t t project)
		    ""))
	 (subdir nil))
    (setq amo (object-assoc fn :file project-am-projects))
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
    path))

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
	      (let ((ampf (make-instance project-am-makefile
					 :file fn
					 :targets
					 (append
					  (mapcar 'project-am-scan-program
						  (makefile-macro-file-list
						   "bin_PROGRAMS"))
					  (mapcar 'project-am-scan-program
						  (makefile-macro-file-list
						   "sbin_PROGRAMS"))
					  (mapcar 'project-am-scan-lib
						  (makefile-macro-file-list
						   "noinst_LIBRARIES"))
					  (mapcar 'project-am-scan-texinfo
						  (makefile-macro-file-list
						   "info_TEXINFOS"))
					  ))))
		(project-am-load-subtree ampf)
		(make-local-variable 'project-am-object)
		(setq project-am-object ampf)
		ampf))
	  ;; If the buffer was not already loaded, kill it.
	  (if (not kb) (kill-buffer (current-buffer))))))))

(defun project-am-scan-program (program)
  "Create a object of CLASS based on NAME.
Argument PROGRAM is the name of the program to create."
  (make-instance project-am-program
		 :name program
		 :path (expand-file-name default-directory)
		 :source (makefile-macro-file-list
			  (concat program "_SOURCES"))
		 :ldadd (makefile-macro-file-list
			  (concat program "_LDADD"))))

(defun project-am-scan-lib (program)
  "Create a object of CLASS based on NAME.
Argument PROGRAM is the name of the library to create."
  (make-instance project-am-lib
		 :name program
		 :path (expand-file-name default-directory)
		 :source (makefile-macro-file-list
			  (concat program "_SOURCES"))))

(defun project-am-scan-texinfo (program)
  "Create a object of CLASS based on NAME.
Argument PROGRAM is the name of the texinfo file."
  (make-instance project-am-texinfo
		 :name program
		 :path (expand-file-name default-directory)
		 :include (makefile-macro-file-list
			   (concat program "_TEXINFOS"))))

(defun project-am-buffer-project (buffer)
  "Return the project associated with BUFFER."
  (project-am-load-project (save-excursion
			     (set-buffer buffer)
			     default-directory)))

;;; Methods:
(defmethod project-am-set-buffer ((ampf project-am-makefile))
  "Set the current buffer to this project file."
  (set-buffer (find-file-noselect (oref ampf :file))))

(defmethod project-am-load-subtree ((ampf project-am-makefile))
  "Load the project file AMPF's subdirs in."
  (let ((sd (save-excursion
	      (project-am-set-buffer ampf)
	      (or
	       ;; If DIST_SUBDIRS doesn't exist, then go for the static list of SUBDIRS.
	       ;; The DIST version should contain SUBDIRS plus extra stuff.
	       (makefile-macro-file-list "DIST_SUBDIRS")
	       (makefile-macro-file-list "SUBDIRS"))))
	(sp nil)
	(tampf nil))
    (while sd
      (setq tampf (project-am-load-makefile
		   (concat (file-name-directory (oref ampf :file))
			   (car sd) "/")))
      (if tampf (setq sp (cons tampf sp)))
      (setq sd (cdr sd)))
    (oset ampf :subproj sp)))

(defmethod project-am-subtree ((ampf project-am-makefile) subpath)
  "Return the sub project in AMPF specified by SUBPATH."
  (object-assoc (expand-file-name subpath) :file (oref ampf subproj)))

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
  "Menu part in easymenu format that is used in speedbar while in project-am mode.")

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
			  (let ((s (file-name-directory
				    (oref this :file))))
			    (string-match "/\\([a-zA-Z0-9_]+\\)/$" s)
			    (match-string 1 s))
			  'project-am-file-find
			  (oref this :file)
			  'speedbar-directory-face depth))

(defmethod project-am-sb-button ((this project-am-program) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this (oref this :name)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-lib) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'project-am-object-expand
			  this (oref this :name)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod project-am-sb-button ((this project-am-texinfo) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?+
			  'project-am-object-expand
			  this
			  (oref this :name)
			  'project-am-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defun project-am-object-expand (text token indent)
  "Expand an object.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level.
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
    (while subproj
      (project-am-sb-button (car subproj) depth)
      (setq subproj (cdr subproj)))
    (while targets
      (project-am-sb-button (car targets) depth)
      (setq targets (cdr targets)))))

(defmethod project-am-sb-expand ((this project-am-objectcode) depth)
  "Expand node describing something built into objectcode.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref token :source)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'project-am-tag-file
			      (concat (oref token :path)
				      (car sources))
			      (car sources)
			      'project-am-file-find
			      (concat
			       (oref token :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defmethod project-am-sb-expand ((this project-am-texinfo) depth)
  "Expand node describing a texinfo manual.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((includes (oref token :include)))
    (while includes
      (speedbar-make-tag-line 'bracket ?+
			      'project-am-tag-file
			      (concat (oref token :path)
				      (car includes))
			      (car includes)
			      'project-am-file-find
			      (concat
			       (oref token :path)
			       (car includes))
			      'speedbar-file-face depth)
      (setq includes (cdr includes)))
    ;; Not only do we show the included files (for future expansion)
    ;; but we also want to display tags for this file too.
    (project-am-create-tag-buttons (concat (oref token :path) 
					   (oref token :name))
				   depth)))

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
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (project-am-create-tag-buttons token indent))))
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
; makefile-macro-table -- alist of macros
; makefile-pickup-macros -- rescan buffer for macro lists.
; makefile-macroassign-regex -- regex for finding macros in a makefile

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
  
)

(defun makefile-macro-file-list (macro)
  "Return a list of all files in MACRO."
  (save-excursion
    (if (makefile-move-to-macro macro)
	(let ((e (save-excursion
		   (makefile-end-of-command)
		   (point)))
	      (lst nil))
	  (while (re-search-forward "\\s-**\\([-a-zA-Z0-9./_@$%()]+\\)\\s-*" e t)
	    (setq lst (cons (match-string 1) lst)))
	  (nreverse lst)))))

;;; Hooks
;;
;;  These let us watch various activities, and respond apropriatly.

(add-hook 'find-file-hooks 'project-am-maybe-install)

(provide 'project-am)

;;; project-am.el ends here
