;;; dlg-config - configureation specific routines using dialog
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: dlg-config.el,v 1.9 1997/01/10 23:08:20 zappo Exp $
;;; Keywords: OO, dialog, configure
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
;;; dlg-config can be found in the eieio or etalk distributions on:
;;;  ftp://ftp.ultranet.com/pub/zappo
;;;
;;; Commentary:
;;;   This will provide the framework to create dialogs using DIALOG
;;; mode with eieio.  These routines will aid in making configuration
;;; windows under emacs that perform some basic tasks.
;;;           
(require 'eieio)
(require 'dialog)
(require 'dlg-class)

(defvar dlg-config-file "~/.emacs"
  "The config file dlg mode will edit if dlg-auto-edit is t.")

(defvar dlg-xdefaults-file "~/.Xdefaults"
  "The Xdefaults file in which we will store font specific information.")

(defvar dlg-auto-edit nil
  "When set, dlg will auto-edit your .emacs file to adjust the value of
each variable which is modified.")

(defvar dlg-show-edits t
  "When auto-editing files, show the file in another window while the
edit occurs.")

(defvar dlg-modify-running-environment t
  "When set, dlg will change the running environment to include
the changes you just set.")

;;;
;;; DLG builders
;;;
(defun dlg-init (&optional edit-style)
  "Configure some basic emacs stuff"
  (switch-to-buffer (get-buffer-create "Emacs Configure"))
  (toggle-read-only -1)
  (erase-buffer)
  (dialog-mode)
  (dialog-build-group (create-widget "Emacs Config Options" widget-frame
				     :position 'top-right)

    (if (or (not edit-style) (eq edit-style 'dot-emacs))
	(create-widget "config-file" widget-labeled-text
		       :label "Config File  :" :text-length 40
		       :help-hook (lambda (obj reason)
				    (message "This is the lisp file to save changes in when AUTO-EDIT is true."))
		       :value (data-object-symbol "config-file" :protect t
						  :symbol 'dlg-config-file)))
    (if (or (not edit-style) (eq edit-style 'xdefaults))
	(create-widget "x-config-file" widget-labeled-text
		       :label "Xdefault File:" :text-length 40
		       :help-hook (lambda (obj reason)
				    (message "This is the X Defaults file to save changes in when AUTO-EDIT is true."))
		       :value (data-object-symbol "x-file" :protect t
						  :symbol 'dlg-xdefaults-file)))
    (create-widget "Modify running environment"
		   widget-toggle-button :y -1
		   :state dlg-modify-running-environment
		   :activate-hook (lambda (obj reason)
				    (setq dlg-modify-running-environment
					  (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to modify your environment whenever a value changes."))
		   )
    (create-widget "Auto Edit files" widget-toggle-button
		   :x -2 :y t :state dlg-auto-edit
		   :activate-hook (lambda (obj reason)
				    (setq dlg-auto-edit
					  (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to modify the appropriate config file for all changes"))
		   )
    (create-widget "Show Edits" widget-toggle-button
		   :x -2 :y t :state dlg-show-edits
		   :activate-hook (lambda (obj reason)
				    (setq dlg-show-edits
					  (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to show all edits in another window."))
		   )
    ))

(defun dlg-end ()
  "Add the [Done] button to the end."
  (create-widget "econfogok" widget-push-button
		 :label-value "Done"
		 :activate-hook (lambda (obj reason) (bury-buffer))
		 :help-hook (lambda (obj reason)
			      (message "Click to finish configuring."))))

(defun dlg-face-box (face &optional bx by boxjust)
  "Create a frame to edit FACE in.  Optionally set position at BX and BY
using BOXJUST as the justification for the label."
  (dialog-build-group (create-widget (format "Edit %S" face) widget-frame
				     :x bx :y by :position boxjust)

    (create-widget (format "%s-sample-text" face) widget-label
		   :x 10 :y 1 :face face :label-value "** Sample Text **")
    (create-widget (format "%s-fglt" face) widget-labeled-text
		   :label "Foreground:" :text-length 20
		   :value (data-face-foreground-object 
			   (format "%s-fg-data" face)
			   :face face
			   :value (if (face-foreground face) 
				      (face-foreground face)
				    (if (face-foreground 'default)
					(face-foreground 'default)
				      ""))))
    (create-widget (format "%s-bglt" face) widget-labeled-text
		   :label "Background:" :text-length 20
		   :value (data-face-background-object 
			   (format "%s-bg-data" face)
			   :face face
			   :value (if (face-background face) 
				      (face-background face)
				    (if (face-background 'default)
					(face-background 'default)
				      ""))))
    (create-widget (format "%s-under" face) widget-toggle-button
    :x 1 :y -1 :label-value "Underline"
    :state (data-face-underline-object 
	    (format "%s-und-data" face)
	    :face face
	    :value (face-underline-p face)))
    (let* ((f1 (face-font face))
	   (f (if f1 f1 ""))
	   (jnk (string-match x-font-regexp-slant f))
	   ;; match-beginning should be to x-font-regexp-slant-subnum
	   ;; but it doesn't seem to work.
	   (it (if (and jnk (match-beginning 1))
		   (string= (substring 
			     f 
			     (match-beginning 1)
			     (match-end 1))
			    "o")
		 nil))
	   (jnk2 (string-match x-font-regexp-weight f))
	   (bld (if (and jnk2 (match-beginning x-font-regexp-weight-subnum))
		    (string= (substring 
			      f
			      (match-beginning x-font-regexp-weight-subnum)
			      (match-end x-font-regexp-weight-subnum))
			     "bold")
		  nil)))
    (create-widget (format "%s-emph" face) widget-option-button-dlg-font-style
		   :x -5 :y t 
		   :option-list '("default" "bold" "italic" "bold-italic")
		   :state (data-face-emphasis-object
			   (format "%s-emph-data" face)
			   :face face
			   :value (+ (if it 2 0) (if bld 1 0)))))
    ))

(defun dlg-faces (&optional list-o-faces)
  "Creates a dialog mode in which emacs faces are edited.  If optional
LIST-O-FACES is provided, then create a box for each.  Otherwise,
default to a list of simple faces."
  (interactive)
  (if (not list-o-faces)
      (setq list-o-faces (list 'default 
			       'highlight
			       'modeline
			       'region 
			       'secondary-selection
			       (if (member 'paren-mismatch (face-list))
				   'paren-mismatch))))
  (dlg-init 'xdefaults)
  (let ((even t))
    (while list-o-faces
      (if (car list-o-faces)
	  (dlg-face-box (car list-o-faces)
			(if even 1 -1)
			(if even -1 t)
			(if even 'top-right 'top-left)))
      (setq even (not even) list-o-faces (cdr list-o-faces))))
  (dlg-end)
  (dialog-refresh)
  )

(defun dlg-widget-faces ()
  "Edit list of widget faces for all dialog boxes."
  (interactive)
  (dlg-faces '(widget-default-face
	       widget-focus-face
	       widget-box-face
	       widget-frame-label-face
	       widget-indicator-face
	       widget-text-face
	       widget-text-focus-face
	       widget-text-button-face
	       )))

(defun dlg-quick-find (find file)
  "Return t if string FIND is in the FILE.  Don't dispose of file since
we will use it soon."
  (save-excursion
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (re-search-forward find nil t)))

(defun dlg-show-an-edit (buffer pnt)
  "Attempt to put buffer in a minimal window somewhere on the display.
Unfortunately, it currently assumes there is but one dialog window."
  (switch-to-buffer-other-window buffer)
  (let ((ob (current-buffer)))
    (if (> (window-height) window-min-height)
	(enlarge-window (- window-min-height (window-height))))
    (set-buffer buffer)
    (goto-char pnt)
    (other-window 1)))

(defun dlg-edit-config-file (object)
  "Reads the currently stored config-file, and starts saving
the variables we are editing."
  (if (and dlg-auto-edit dlg-config-file (not (oref this protect)))
      (let ((ob (current-buffer))
	    nb pnt)
	(setq nb (set-buffer (find-file-noselect dlg-config-file)))
	(goto-char (point-min))
	(setq pnt (dlg-edit-config-file-object object))
	(set-buffer ob)
	(dlg-show-an-edit nb pnt))))

;;;
;;; Some utility functions to use
;;;
(defun dlg-string-to-list (string separator)
  "Take STRING, and turn it into a list of parameters as though taken by a
program.  Splits the string on SEPARATOR."
  (let ((lst nil)
	(last 0))
    (while (string-match separator string last)
      (setq lst (cons (substring string last (match-beginning 0)) lst)
	    last (match-end 0)))
    (if (/= last (length string))
	(setq lst (cons (substring string last) lst)))
    (nreverse lst))
  )

(defun dlg-list-to-string (list separator)
  "Take LIST, and turn it into a SEPARATOR separated string."
  (let ((str ""))
    (while list
      (setq str (concat str (car list) separator)
	    list (cdr list)))
    str)
  )


;;; end of lisp
(provide 'dlg-config)
