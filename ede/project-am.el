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
(require 'makefile "mmfm")
(require 'eieio)

(defvar project-am-projects nil
  "A list of all active projects currently loaded in Emacs.")

(defvar project-am-object nil
  "Local variable defining a makefile object for a given Makefile.am.")

(defclass project-am-target ()
  ((name :initarg :name :docstring "Name of this target.")
   (source :initarg :source :docstring "List of source files.")
   (ldadd :initarg :ldadd :docstring "Additional LD args.")
   (path :initarg :path :docstring "The path to this target.")
   )
  "A top level target to build.")

(defclass project-am-program (project-am-target)
  nil
  "A top level program to build")

(defclass project-am-lib (project-am-target)
  nil
  "A top level library to build")

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
  (project-am-load default-directory))

;;; Project loading and saving
;;
(defun project-am-load (project)
  "Read an automakefile PROJECT into our data structure.
Make sure that the tree down to our makefile is complete so that there
is cohesion in the project.  Return the project file (or sub-project)."
  (let* ((fn (project-am-find-topmost-level
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
      (setq amo (project-am-load-makefile fn))
      (if (not amo)
	  nil
	(project-am-load-subtree amo)
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
      (setq amo (project-am-subtree amo (concat fn subdir))))
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
  (let* ((fn (concat path "Makefile.am")))
    (if (not (file-exists-p fn))
	nil
      (save-excursion
	(set-buffer (find-file-noselect fn))
	(if (and project-am-object (project-am-makefile-p project-am-object))
	    project-am-object
	  (let ((ampf (make-instance project-am-makefile
				     :file filename
				     :targets
				     (mapcar 'project-am-scan-program
					     (makefile-macro-file-list
					      "bin_PROGRAMS"))
				     )))
	    (oset ampf :subproj (project-am-load-subtree ampf))
	    (make-local-variable 'project-am-object)
	    (setq project-am-object ampf)
	    ampf))))))

(defun project-am-scan-program (program)
  "Create a PROGRAM object and return it."
  (make-instance project-am-program
		 :name program
		 :source (makefile-macro-file-list (concat program "_SOURCES"))
		 :ldadd (makefile-macro-file-list (concat program "_LDADD"))
		 :path default-directory
		 ))

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
	      (makefile-macro-file-list "SUBDIRS")))
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
  (object-assoc subpath :file (oref ampf subproj)))

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
  (let* ((speedbar-tag-hierarchy-method '(sort))
	 (obj (if (stringp dir-or-object)
		  (project-am-load dir-or-object)
		object))
	 (subproj nil)
	 (targets nil))
    (if (not obj)
	(speedbar-make-tag-line nil nil nil nil "No Project" nil nil
				nil (1+ depth))
      (setq subproj (oref obj :subproj))
      (setq targets (oref obj :targets))
      (while subproj
	(speedbar-make-tag-line 'angle ?+
				'project-am-subproj-expand
				(car subproj)
				(file-name-nondirectory
				 (oref (car subproj) :file))
				nil nil  ; nothing to jump to
				'speedbar-directory-face depth)
	(setq subproj (cdr subproj)))
      (while targets
	(speedbar-make-tag-line 'angle ?+
				'project-am-target-expand
				(car targets)
				(oref (car targets) :name)
				nil nil  ; nothing to jump to
				'speedbar-file-face depth)
	(setq targets (cdr targets))))))

(defun project-am-subproj-expand (text token indent)
  "Expand a sub project.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((speedbar-tag-hierarchy-method '(sort)))
	       (project-am-speedbar token (1+ indent))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun project-am-target-expand (text token indent)
  "Expand a sub project.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     ;; Get all the source files, and insert them as a new
	     ;; kind of button.
	     (let ((speedbar-tag-hierarchy-method '(sort))
		   (sources (oref token :source)))
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
					 'speedbar-file-face (1+ indent))
		 (setq sources (cdr sources)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun project-am-file-find (text token indent)
  "Find the file TEXT at path TOKEN.
INDENT is the current indentation level."
  (speedbar-find-file-in-frame token))

(defun project-am-tag-file (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (let* ((fn token)
		(lst (if speedbar-use-imenu-flag
			 (let ((tim (speedbar-fetch-dynamic-imenu fn)))
			   (if (eq tim t)
			       (speedbar-fetch-dynamic-etags fn)
			     tim))
		       (speedbar-fetch-dynamic-etags fn))))
	   ;; if no list, then remove expando button
	   (if (not lst)
	       (speedbar-change-expand-button-char ??)
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (speedbar-insert-generic-list indent
					       lst
					       'project-am-tag-expand
					       'project-am-tag-find))))))
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



;;; Hooks
;;
;;  These let us watch various activities, and respond apropriatly.

(add-hook 'find-file-hooks 'project-am-maybe-install)

(provide 'project-am)

;;; project-am.el ends here
