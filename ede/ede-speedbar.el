;;; ede-speedbar.el --- Speebar viewing of EDE projects

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.2
;; Keywords: project, make, tags
;; RCS: $Id: ede-speedbar.el,v 1.9 1999/12/01 01:54:41 zappo Exp $

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

;;; Code:
(require 'ede)
(require 'eieio-speedbar)

;;; Speedbar support mode
;;
(defvar ede-speedbar-key-map nil
  "A Generic object based speedbar display keymap.")

(defun ede-speedbar-make-map ()
  "Make the generic object based speedbar keymap."
  (setq ede-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing things
  (define-key ede-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key ede-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key ede-speedbar-key-map "=" 'speedbar-expand-line)
  (define-key ede-speedbar-key-map "-" 'speedbar-contract-line)

  ;; Some object based things
  (define-key ede-speedbar-key-map "C" 'eieio-speedbar-customize-line)

  ;; Some project based things
  (define-key ede-speedbar-key-map "R" 'ede-speedbar-remove-file-from-target)
  (define-key ede-speedbar-key-map "b" 'ede-speedbar-compile-line)
  (define-key ede-speedbar-key-map "B" 'ede-speedbar-compile-project)
  (define-key ede-speedbar-key-map "D" 'ede-speedbar-make-distribution)
  (define-key ede-speedbar-key-map "E" 'ede-speedbar-edit-projectfile)
  )

(defvar ede-speedbar-menu
  '([ "Compile" ede-speedbar-compile-line t]
    [ "Compile Project" ede-speedbar-compile-project
      (ede-project-child-p (speedbar-line-token)) ]
    "---"
    [ "Edit File/Tag" speedbar-edit-line
      (not (object-p (speedbar-line-token)))]
    [ "Expand" speedbar-expand-line
      (save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *.\\+. "))]
    [ "Contract" speedbar-contract-line
      (save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *.-. "))]
    "---"
    [ "Remove File from Target" ede-speedbar-remove-file-from-target
      (stringp (speedbar-line-token)) ]
    [ "Customize Project/Target" eieio-speedbar-customize-line
      (object-p (speedbar-line-token)) ]
    [ "Edit Project File" ede-speedbar-edit-projectfile t]
    [ "Make Distribution" ede-speedbar-make-distribution
      (ede-project-child-p (speedbar-line-token)) ]
    )
  "Menu part in easymenu format used in speedbar while browsing objects.")

(eieio-speedbar-create 'ede-speedbar-make-map
		       'ede-speedbar-key-map
		       'ede-speedbar-menu
		       "EDE"
		       'ede-speedbar-toplevel-buttons)

(defun ede-speedbar ()
  "EDE development environment project browser for speedbar."
  (interactive)
  (speedbar-frame-mode 1)
  (speedbar-change-initial-expansion-list "EDE")
  (speedbar-get-focus)
  )

(defun ede-speedbar-toplevel-buttons (dir)
  "Return a list of objects to display in speedbar.
Argument DIR is the directory from which to derive the list of objects."
  ;(list (ede-load-project-file dir))
  (ede-load-project-file dir)
  ede-projects
  )

;;; Some special commands useful in EDE
;;
(defun ede-speedbar-remove-file-from-target ()
  "Remove the file at point from it's target."
  (if (stringp (speedbar-line-token))
      (progn
	(speedbar-edit-line)
	(ede-remove-file))))

(defun ede-speedbar-compile-line ()
  "Compile/Build the project or target on this line."
  (let ((obj (eieio-speedbar-find-nearest-object)))
    (if (not (object-p obj))
	nil
      (cond ((child-of-class-p obj ede-project)
	     (project-compile-project obj))
	    ((child-of-class-p obj ede-target)
	     (project-compile-target obj))
	    (t (error "Error in speedbar structure"))))))

(defun ede-speedbar-get-top-project-for-line ()
  "Return a project object for this line."
  (let ((obj (eieio-speedbar-find-nearest-object)))
    (if (not (object-p obj))
	(error "Error in speedbar or ede structure")
      (if (child-of-class-p obj ede-target)
	  (setq obj (ede-target-parent obj)))
      (if (child-of-class-p obj ede-project)
	  obj
	(error "Error in speedbar or ede structure")))))

(defun ede-speedbar-compile-project ()
  "Compile/Build the project which owns this line."
  (project-compile-project (ede-speedbar-get-top-project-for-line)))

(defun ede-speedbar-make-distribution ()
  "Edit the project file based on this line."
  (project-make-dist (ede-speedbar-get-top-project-for-line)))

(defun ede-speedbar-edit-projectfile ()
  "Edit the project file based on this line."
  (project-edit-file-target (ede-speedbar-get-top-project-for-line)))

;;; Speedbar Project Methods
;;
(defun ede-find-nearest-file-line ()
  "Go backwards until we find a file."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\([0-9]+\\):")
    (let ((depth (string-to-int (match-string 1))))
      (while (not (re-search-forward "[]] [^ ]"
				     (save-excursion (end-of-line)
						     (point))
				     t))
	(re-search-backward (format "^%d:" (1- depth)))
	(setq depth (1- depth)))
      (speedbar-line-token))))

(defmethod eieio-speedbar-derive-line-path ((obj ede-project) &optional depth)
  "Return the path to OBJ.
Optional DEPTH is the depth we start at."
  (file-name-directory (oref obj file))
  )

(defmethod eieio-speedbar-derive-line-path ((obj ede-target) &optional depth)
  "Return the path to OBJ.
Optional DEPTH is the depth we start at."
  (let ((proj (ede-target-parent obj)))
    ;; Check the type of line we are currently on.
    ;; If we are on a child, we need a file name too.
    (save-excursion
      (let ((lt (speedbar-line-token)))
	(if (or (object-p lt) (stringp lt))
	    (eieio-speedbar-derive-line-path proj)
	  ;; a child element is a token.  Do some work to get a filename too.
	  (concat (eieio-speedbar-derive-line-path proj)
		  (ede-find-nearest-file-line)))))))

(defmethod eieio-speedbar-description ((obj ede-project))
  "Provide a speedbar description for OBJ."
  (ede-description obj))

(defmethod eieio-speedbar-description ((obj ede-target))
  "Provide a speedbar description for OBJ."
  (ede-description obj))

(defmethod eieio-speedbar-child-description ((obj ede-target))
  "Provide a speedbar description for a plain-child of OBJ.
A plain child is a child element which is not an EIEIO object."
  (or (speedbar-item-info-file-helper)
      (speedbar-item-info-tag-helper)))

(defmethod eieio-speedbar-object-buttonname ((object ede-project))
  "Return a string to use as a speedbar button for OBJECT."
  (concat (ede-name object) " " (oref object version)))

(defmethod eieio-speedbar-object-buttonname ((object ede-target))
  "Return a string to use as a speedbar button for OBJECT."
  (ede-name object))

(defmethod eieio-speedbar-object-children ((this ede-project))
  "Return the list of speedbar display children for THIS."
  (with-slots (subproj targets) this
    (append subproj targets)))

(defmethod eieio-speedbar-object-children ((this ede-target))
  "Return the list of speedbar display children for THIS."
  (oref this source))

(defmethod eieio-speedbar-child-make-tag-lines ((this ede-target))
  "Create a speedbar tag line for a child of THIS.
It has string CHILD-STRING, and depth DEPTH."
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

;;; Generic file management for TARGETS
;;
(defun ede-file-find (text token indent)
  "Find the file TEXT at path TOKEN.
INDENT is the current indentation level."
  (speedbar-find-file-in-frame
   (concat (speedbar-line-path) token)))

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
	 (let ((file (speedbar-line-file)))
	   (speedbar-with-writable
	     (save-excursion
	       (end-of-line) (forward-char 1)
	       (ede-create-tag-buttons file (1+ indent))))))
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
  (let ((file (ede-find-nearest-file-line)))
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
