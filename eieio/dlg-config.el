;;; dlg-config - configureation specific routines using dialog
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: dlg-config.el,v 1.2 1996/08/17 00:30:57 zappo Exp $
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
;;; Lets create some new widget types just for option buttons
;;; with specialized display attributes
;;;      

(defclass widget-option-button-dlg-font-style (widget-option-button)
  nil
  "Special kind of option-button whose default face changes with
different values of state")

(defmethod reset-option-label :BEFORE ((this widget-option-button-dlg-font-style))
  "Change our face whenever a new button label is presented."
  (oset this face (aref [ 'default 'bold 'italic 'bold-italic ]
			(get-value (oref this state))))
  (oset this focus-face (aref [ 'bold 'default 'bold-italic 'italic ]
			      (get-value (oref this state)))))

(defmethod verify :AFTER ((this widget-option-button-dlg-font-style) fix)
  "Change our face to be dependant upon our init value in state."
  (oset this face (aref [ 'default 'bold 'italic 'bold-italic ]
			(get-value (oref this state))))
  (oset this focus-face (aref [ 'bold 'default 'bold-italic 'italic ]
			      (get-value (oref this state)))))

;;;
;;; Lets create some special data objects linked direcly into the
;;; emacs system to maintain special types of data.
;;;
(defclass data-object-symbol (data-object)
  ((symbol :initarg :symbol
	   :initform nil
	   :documentation "Symbol whose value changes in parallel to :value"
	   :protection private)
   (protect :initarg :protect
	    :initform nil
	    :documentation "Some symbols you never want to write to a file"
	    :protection private))
  "This type of object will also maintain it's value as the
variable associated with the symbol field")

(defmethod set-value :AFTER ((this data-object-symbol) value &optional setter)
  "When this data object's value is set, also set the value of it's
  symbol"
  (if dlg-modify-running-environment (set (oref this symbol) value))
  (dlg-edit-config-file this))

(defclass data-object-symbol-default (data-object-symbol)
  nil
  "This type of object uses setq-defualt for the given symbol")

(defmethod set-value :AFTER ((this data-object-symbol) value &optional setter)
  "When this data object value is set, set this as the new default."
  (set-default (oref this symbol) value)
  (dlg-edit-config-file this))

;; face specific data objects
;;
(defclass data-face-object (data-object)
  ((face :initarg :face
	 :initform 'default
	 :documentation "The face this data object maintains"
	 :protection private))
  "Takes a standard data object, and modifies it to be able to
maintain a face.  Has nothing special about it, and should not be
instantiated.")

(defclass data-face-foreground-object (data-face-object)
  nil
  "Face object which maintains the foreground")

(defmethod set-value :BEFORE ((this data-face-foreground-object) value &optional setter)
  "Set the value of a `data-face-foreground-object' and modifies said face."
  (if (and (stringp value) (stringp (get-value this)) 
	   (not (string= value (get-value this)))
	   (x-color-defined-p value))
      (set-face-foreground (oref this face) value)
    (if (string= value "")
	(set-face-foreground (oref this face) nil)))
  (if (stringp value)
      (dlg-edit-xdefaults this "attributeForeground" value)))


(defclass data-face-background-object (data-face-object)
  nil
  "Face object which maintains the background")

(defmethod set-value :BEFORE ((this data-face-background-object) value &optional setter)
  "Set the value of a `data-face-foreground-object' and modifies said face."
  (if (and (stringp value) (stringp (get-value this)) 
	   (not (string= value (get-value this)))
	   (x-color-defined-p value))
      (set-face-background (oref this face) value)
    (if (string= value "")
	(set-face-background (oref this face) nil)))
  (if (stringp value)
      (dlg-edit-xdefaults this "attributeBackground" value)))

(defclass data-face-underline-object (data-face-object)
  nil
  "Face object which mnaintains current underline state")

(defmethod set-value :BEFORE ((this data-face-underline-object) value &optional setter)
  "Sets the underline attribute of a face"
  (if (and (or (eq value nil) (eq value t))
	   (or (eq (get-value this) nil) (eq (get-value this) t))
	   (not (eq value (get-value this))))
      (progn
	(set-face-underline-p (oref this face) (not (get-value this)))
	(dlg-edit-xdefaults this "attributeUnderline" 
			    (if value "true" "false")))))

(defclass data-face-emphasis-object (data-face-object)
  nil
  "Face object which maintains current emphasis state. (bold & italic combos)")

(defmethod set-value :BEFORE ((this data-face-emphasis-object) value &optional setter)
  "Set the value of `data-face-foreground-object' and modify said face."
  (if (numberp value)
      (let ((f (oref this face)))
	(cond ((= value 0)
	       (make-face-unbold f)
	       (make-face-unitalic f)
	       (dlg-edit-xdefaults this "attributeFont"
				   "-*-*-medium-r-*-*-*-*-*-*-*-*-*-*"))
	      ((= value 1)
	       (make-face-bold f)
	       (make-face-unitalic f)
	       (dlg-edit-xdefaults this "attributeFont"
				   "-*-*-bold-r-*-*-*-*-*-*-*-*-*-*"))
	      ((= value 2)
	       (make-face-unbold f)
	       (make-face-italic f)
	       (dlg-edit-xdefaults this "attributeFont"
				   "-*-*-medium-o-*-*-*-*-*-*-*-*-*-*"))
	      ((= value 3)
	       (make-face-bold f)
	       (make-face-italic f)
	       (dlg-edit-xdefaults this "attributeFont"
				   "-*-*-bold-i-*-*-*-*-*-*-*-*-*-*"))))))

;;;
;;; DLG builders
;;;
(defun dlg-init ()
  "Configure some basic emacs stuff"
  (switch-to-buffer (get-buffer-create "Emacs Configure"))
  (erase-buffer)
  (dialog-mode)
  (let ((ecframe (create-widget "Toggle Frame dlg" widget-frame
				widget-toplevel-shell
				:x 2 :y 2
				:frame-label "Emacs Config Options"))
	)
    (create-widget "config-file" widget-label ecframe
		   :x 1 :y 1 :label-value "Config File  :")
    (create-widget "config-file" widget-text-field ecframe
		   :width 40 :height 1 :x -2 :y t 
		   :value (data-object-symbol "config-file"
					      :protect t
					      :symbol 'dlg-config-file
					      :value dlg-config-file))
    (create-widget "x-file" widget-label ecframe
		   :x 1 :y -1 :label-value "Xdefault File:")
    (create-widget "x-file" widget-text-field ecframe
		   :width 40 :height 1 :x -2 :y t 
		   :value (data-object-symbol "x-file"
					      :protect t
					      :symbol 'dlg-xdefaults-file
					      :value dlg-xdefaults-file))
    (create-widget "dlgedit" widget-toggle-button ecframe
		   :x 1 :y -1 :label-value "Auto Edit files"
		   :state dlg-auto-edit
		   :activate-hook (lambda (obj reason)
				    (setq dlg-auto-edit
				    (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to modify the appropriate config file for all changes"))
		   )
    (create-widget "dlgshow" widget-toggle-button ecframe
		   :x -5 :y t :label-value "Show Edits"
		   :state dlg-show-edits
		   :activate-hook (lambda (obj reason)
				    (setq dlg-show-edits
				    (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to show all edits in another window."))
		   )
    (create-widget "dlgrun" widget-toggle-button ecframe
		   :x 1 :y -1 :label-value "Modify running environment (Does not affect everything.)"
		   :state dlg-modify-running-environment
		   :activate-hook (lambda (obj reason)
				    (setq dlg-modify-running-environment
				    (get-value (oref obj state))))
		   :help-hook (lambda (obj reason)
				(message "ON means to modify your environment whenever a value changes."))
		   )
    ))

(defun dlg-end ()
  "Add the [Done] button to the end."
  (create-widget "econfogok" widget-button widget-toplevel-shell
		 :x 10 :y -3 :label-value "Done"
		 :activate-hook (lambda (obj reason) (bury-buffer))
		 :help-hook (lambda (obj reason)
			      (message "Click to finish configuring."))))

(defun dlg-face-box (face &optional parent bx by)
  "Create a frame to edit FACE in.  Optionally set PARENT, and position it
at BX and BY"
  (let ((cframe (create-widget "FaceEditFrame" widget-frame
			       (if parent parent widget-toplevel-shell)
			       :x (if bx bx 2) :y (if by by -3)
			       ;; :box-face 'region
			       :frame-label (format "Edit %S" face)))
	)
    (create-widget (format "%s-sample-text" face) widget-label cframe
		   :x 10 :y 1 :face face :label-value "** Sample Text **")
    (create-widget (format "%s-fgl" face) widget-label cframe
		   :x 1 :y -2 :label-value "Foreground:")
    (create-widget (format "%s-foreground" face) widget-text-field cframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-face-foreground-object 
			   (format "%s-fg-data" face)
			   :face face
			   :value (if (face-foreground face) 
				      (face-foreground face)
				    (if (face-foreground 'default)
					(face-foreground 'default)
				      ""))))
    (create-widget (format "%s-bgl" face) widget-label cframe
		   :x 1 :y -1 :label-value "Background:")
    (create-widget (format "%s-foreground" face) widget-text-field cframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-face-background-object 
			   (format "%s-bg-data" face)
			   :face face
			   :value (if (face-background face) 
				      (face-background face)
				    (if (face-background 'default)
					(face-background 'default)
				      ""))))
    (create-widget (format "%s-under" face) widget-toggle-button cframe
		   :x 2 :y -2 :label-value "Underline"
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
			    "i")
		 nil))
	   (jnk2 (string-match x-font-regexp-weight f))
	   (bld (if (and jnk2 (match-beginning x-font-regexp-weight-subnum))
		    (string= (substring 
			      f
			      (match-beginning x-font-regexp-weight-subnum)
			      (match-end x-font-regexp-weight-subnum))
			     "bold")
		  nil)))
      (create-widget (format "%s-emph" face) widget-option-button-dlg-font-style cframe
		     :x -3 :y t 
		     :option-list '("default" "bold" "italic" "bold-italic")
		     :state (data-face-emphasis-object
			     (format "%s-emph-data" face)
			     :face face
			     :value (+ (if it 2 0) (if bld 1 0)))))
    ))

(defun dlg-basic ()
  "Creates a configure window with basic emacs items stored in it.
Booleans are represented with toggle buttons.  Other types are
represented as necessary."
  (interactive)
  (dlg-init)
  (let ((oframe (create-widget "Toggle Frame buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Buffer Options"))
	)

    (create-widget "truncatelines" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Truncate Lines"
		   :state (data-object-symbol-default
			   "truncate-lines"
			   :value truncate-lines
			   :symbol 'truncate-lines))
    (create-widget "localvar" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Enable Local Variables"
		   :state (data-object-symbol "enable-local-variables"
					      :value enable-local-variables
					      :symbol 'enable-local-variables))
    (create-widget "newline" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Require Final Newline"
		   :state (data-object-symbol "require-final-newline"
					      :state require-final-newline
					      :symbol 'require-final-newline))
    (create-widget "qrhighlight" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Query Replace Hightlight"
		   :state (data-object-symbol "query-replace-highlight"
					      :value query-replace-highlight
					      :symbol 'query-replace-highlight))
    )

  (dlg-end)
  (dialog-refresh)
  )

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
  (dlg-init)
  (let ((even t))
    (while list-o-faces
      (if (car list-o-faces)
	  (dlg-face-box (car list-o-faces) nil
			(if even 2 -4)
			(if even -3 t)))
      (setq once t even (not even) list-o-faces (cdr list-o-faces))))
  (dlg-end)
  (dialog-refresh)
  )

(defun dlg-font-lock-faces ()
  "Edit list of font lock used faces"
  (interactive)
  (dlg-faces '(font-lock-comment-face
	       font-lock-function-name-face
	       font-lock-string-face
	       font-lock-keyword-face
	       font-lock-reference-face
	       font-lock-variable-name-face
	       font-lock-type-face))
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

(defmethod dlg-edit-config-file ((this data-object-symbol))
  "Reads the currently stored config-file, and starts saving
the variables we are editing."
  (if (and dlg-auto-edit dlg-config-file (not (oref this protect)))
      (let ((ob (current-buffer))
	    nb pnt)
	(setq nb (set-buffer (find-file-noselect dlg-config-file)))
	(goto-char (point-min))
	(if (or (re-search-forward (concat 
				    "(setq?[ \t\n]+"
				    (symbol-name (oref this symbol))
				    "[ \t\n]+\\([A-Za-z0-9_]+\\)") nil t)
		(re-search-forward (concat 
				    "(setq?[ \t\n]+"
				    (symbol-name (oref this symbol))
				    "[ \t\n]+\\(\"[^\"]*\"\\)") nil t))
	    (progn
	      (goto-char (match-beginning 1))
	      (delete-region (point) (match-end 1))
	      (insert (format "%S" (oref this value))))
	  (goto-char (point-max))
	  (insert (format "\n(setq %s %S)"
			  (symbol-name (oref this symbol))
			  (oref this value))))
	(beginning-of-line)
	(setq pnt (point))
	(set-buffer ob)
	(dlg-show-an-edit nb pnt))))

(defmethod dlg-edit-xdefaults ((this data-face-object) token val)
  "Open and edit the chosen Xdefaults file and store this face
information there so that faces aren't automatically created at
startup (thus creating a real slow load)"
  (if (and dlg-auto-edit dlg-xdefaults-file)
      (let ((ob (current-buffer))
	    nb pnt)
	(set-buffer (find-file-noselect dlg-xdefaults-file))
	(setq nb (current-buffer))
	(goto-char (point-min))
	(if (re-search-forward (concat 
				"emacs.*" 
				(symbol-name (oref this face))
				"." token ":"
				"[ \t\n]+\\([^\n]+\\)$") nil t)
	    (progn
	      (goto-char (match-beginning 1))
	      (delete-region (point) (match-end 1))
	      (insert val))
	  (goto-char (point-max))
	  (insert "\nemacs*" (symbol-name (oref this face))
		  "." token ":\t" val))
	(beginning-of-line)
	(setq pnt (point))
	(set-buffer ob)
	(dlg-show-an-edit nb pnt))))

;;; end of lisp
(provide 'dlg-config)
