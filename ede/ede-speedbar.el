;;; ede-speedbar.el --- Speebar viewing of EDE projects

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.2
;; Keywords: project, make, tags
;; RCS: $Id: ede-speedbar.el,v 1.3 1999/01/21 21:24:27 zappo Exp $

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
;; Display a project's hierarchy in speedbar.
;;
;; For speedbar support of your object, define these:
;; `ede-sb-button' - Create a button representing your object.
;; `ede-sb-expand' - Create the list of sub-buttons under your button
;;                          when it is expanded.

(require 'ede)

;;; Code:
(eval-when-compile (require 'speedbar))

(require 'project-am)

;;; Speedbar support mode
;;
(defvar ede-speedbar-key-map nil
  "Keymap used when working with a project in speedbar.")

(defun ede-speedbar-make-map ()
  "Make a keymap for ede under speedbar."
  (setq ede-speedbar-key-map (speedbar-make-specialized-keymap))
    
  ;; General viewing pleasure...
  (define-key ede-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key ede-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key ede-speedbar-key-map "-" 'speedbar-contract-line)
  )

(if ede-speedbar-key-map
    nil
  (if (featurep 'speedbar)
      (ede-speedbar-make-map)
    (add-hook 'speedbar-load-hook 'ede-speedbar-make-map)))

(defvar ede-speedbar-menu
  ()
  "Menu part in easymenu format that is used in speedbar while in `ede' mode.")

(defun ede-speedbar ()
  "EDE development environment project browser for speedbar."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Make sure our special speedbar major mode is loaded
  (speedbar-add-expansion-list '("EDE"
				 ede-speedbar-menu
				 ede-speedbar-key-map
				 ede-speedbar-buttons))
  ;; Overload those functions that we care about.
  (speedbar-add-mode-functions-list
   '("EDE"
     (speedbar-item-info . ede-speedbar-item-info)
     (speedbar-line-path . ede-speedbar-line-path)))
  ;; Now, throw us into EDE mode on speedbar.
  (speedbar-change-initial-expansion-list "EDE")
  ;; Now flip over to the speedbar frame
  (speedbar-get-focus)
  )

(defun ede-speedbar-item-info ()
  "Display info for the current line when in EDE display mode."
  (or (speedbar-item-info-tag-helper)
      (let ((tok (speedbar-line-token)))
	(cond ((object-p tok)
	       (message (ede-description tok)))
	      ((stringp tok)
	       (speedbar-item-info-file-helper tok))
	      (t nil)))))

(defun ede-speedbar-line-path (&optional depth)
  "Return the path to the file the cursor is on.
Optional DEPTH is the depth we start at."
  (file-name-nondirectory (or (speedbar-line-token) "")))

(defun ede-speedbar-buttons (dir-or-object depth)
  "Create buttons in speedbar that represents the current project.
DIR-OR-OBJECT is the object to expand, or nil, and DEPTH is the current
expansion depth."
  (let ((obj (if (and (stringp dir-or-object)
		      (ede-load-project-file dir-or-object))
		 ;; For any project ALWAYS start at the top
		 ;; when we first start.  (When we have a string)
		 (ede-load-project-file (ede-toplevel-project
					 dir-or-object))
	       nil)))
    (if (not obj)
	(speedbar-make-tag-line nil nil nil nil "No Project" nil nil
				nil (1+ depth))
      ;; Depend on the object method for expansion rules.
      (ede-sb-expand obj depth))))

;;; Speedbar Project Methods
;;
(defmethod ede-sb-button ((this ede-project) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this
			  (ede-name this)
			  'ede-file-find
			  (oref this :file)
			  'speedbar-directory-face depth))

(defmethod ede-sb-button ((this ede-target) depth)
  "The default speedbar button for any target."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil this  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-program) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-lib) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-texinfo) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?+
			  'ede-object-expand
			  this
			  (ede-name this)
			  'ede-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-man) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?? nil nil
			  (ede-name this)
			  'ede-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-lisp) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defun ede-object-expand (text token indent)
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
	       (ede-sb-expand token (1+ indent))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defmethod ede-sb-expand ((this ede-project) depth)
  "Expand THIS logically at DEPTH."
  (with-slots (subproj targets) this
    (if (and (= 1 (+ (length subproj) (length targets)))
	     (string= (ede-name this)
		      (ede-name (or (car subproj)
					   (car targets)))))
	;; If there is only one target, and it has the same name
	;; as the directory, then expand that target instead.
	(ede-sb-expand (or (car subproj) (car targets)) depth)
      (mapcar (lambda (car) (ede-sb-button car depth)) subproj)
      (mapcar (lambda (car) (ede-sb-button car depth)) targets))))

(defmethod ede-sb-expand ((this ede-target) depth)
  "The default speedbar button for any target."
  (with-slots (source) this
    (mapcar (lambda (car)
	      (speedbar-make-tag-line 'bracket ?+
				      'ede-tag-file
				      (concat (oref this :path) car)
				      car
				      'ede-file-find
				      (concat (oref this :path) car)
				      'speedbar-file-face depth))
	    source)))

(defmethod ede-sb-expand ((this project-am-objectcode) depth)
  "Expand node describing something built into objectcode.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :source)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defmethod ede-sb-expand ((this project-am-texinfo) depth)
  "Expand node describing a texinfo manual.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((includes (oref this :include)))
    (while includes
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car includes))
			      (car includes)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car includes))
			      'speedbar-file-face depth)
      (setq includes (cdr includes)))
    ;; Not only do we show the included files (for future expansion)
    ;; but we also want to display tags for this file too.
    (ede-create-tag-buttons (concat (oref this :path)
					   (oref this :name))
				   depth)))

(defmethod ede-sb-expand ((this project-am-lisp) depth)
  "Expand node describing lisp code.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :lisp)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defun ede-file-find (text token indent)
  "Find the file TEXT at path TOKEN.
INDENT is the current indentation level."
  (speedbar-find-file-in-frame token))

(defun ede-create-tag-buttons (filename indent)
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
				      'ede-tag-expand
				      'ede-tag-find)))))

(defun ede-tag-file (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (ede-create-tag-buttons token (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun ede-tag-expand (text token indent)
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
					   'ede-tag-expand
					   'ede-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun ede-tag-find (text token indent)
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

(provide 'ede-speedbar)

;;; ede-speedbar.el ends here
