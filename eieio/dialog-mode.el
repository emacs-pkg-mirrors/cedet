;;; dialog.el - Code for starting, and managing dialogs buffers
;;;
;;; Copyright (C) 1995, 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.4
;;; RCS: $Id: dialog-mode.el,v 1.2 1996/06/17 22:32:28 zappo Exp $
;;; Keywords: OO widget dialog
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

;;;
;;; Commentary:
;;;   Dialog mode requires the use of widget-d and widget-i.  It
;;; supplies mundane functions (basic drawing routines w/ faces) and
;;; also the framework in which the widgets work (The buffer, mode,
;;; keymap, etc).  Using `dialog-mode' lets you create a dialog in
;;; which you can place buttons and text fields within a top-level
;;; shell.  This mode manages the keymap, and the input is distributed
;;; to the correct active widget.
;;;
;;; To create a new dialog, you must follow these basic steps:
;;; 1) Create a new blank buffer
;;; 2) run `dialog-mode' on it
;;; 3) use `create-widget' to make widgets to make your dialog useful
;;;    (create-widget name class parent &rest resources)
;;;    - widget-label          - static text
;;;    - widget-button         - push it to make something happen
;;;    - widget-toggle-button  - push it to change a value
;;;    - widget-text-field     - a place to edit simple text
;;;    - widget-frame          - make a box around other widgets
;;;    Resources are :keys which specify how your widget behaves
;;; 4) call `dialog-refresh'
;;;
;;; Making widgets talk to eachother:
;;; Asside from `widget-core' there is also a `data-object' which
;;; provides a method for widgets to talk to eachother (ala
;;; fresco/interviews style)  A widget will create a data object if
;;; one is not given to it.  A widget always register's itself with
;;; the data object, and these objects alert viewers if they are
;;; changed.  In this way, a toggle button will automatically update
;;; itself if it's data has changed.
;;;
;;; For examples of how to make widgets interact, examine the function
;;; `dialog-test'

(require 'widget-i)

;;;
;;; Widget definitions using eieio
;;; 
         
(defvar widget-toplevel-shell nil
  "Buffer local variable containing the definition of the toplevel
shell active in the current buffer.  There can be only one toplevel
shell definition in a given buffer.")
(make-variable-buffer-local 'widget-toplevel-shell)

;;;
;;; Dialog mode variables
;;;
(defvar dialog-mode-map nil 
  "Keymap used in dialog mode.")

(if dialog-mode-map () 
  ;; create and fill up the keymap with our event handler
  (setq dialog-mode-map (make-keymap))
  ;(suppress-keymap dialog-mode-map)
  (fillarray (nth 1 dialog-mode-map) 'dialog-handle-kbd)
  ;; some keys we don't want to override
  (define-key dialog-mode-map "\C-x" nil)
  (define-key dialog-mode-map "\e" nil)
  (define-key dialog-mode-map "\C-z" nil)
  (define-key dialog-mode-map "\C-h" nil)
  (define-key dialog-mode-map "\C-g" nil)
  (define-key dialog-mode-map [tab] "\C-i")
  (define-key dialog-mode-map [up] "\C-p")
  (define-key dialog-mode-map [down] "\C-n")
  (define-key dialog-mode-map [right] "\C-f")
  (define-key dialog-mode-map [left] "\C-b")
  (define-key dialog-mode-map [next] "\C-v")
  (define-key dialog-mode-map [prev] "\e-v")
  ;; Now some mouse events
  (define-key dialog-mode-map [mouse-1] 'dialog-handle-mouse)
  (define-key dialog-mode-map [mouse-2] 'dialog-handle-mouse)
  (define-key dialog-mode-map [mouse-3] 'dialog-handle-mouse)
  (define-key dialog-mode-map [down-mouse-1] 'dialog-handle-mouse)
  (define-key dialog-mode-map [down-mouse-2] 'dialog-handle-mouse)
  (define-key dialog-mode-map [down-mouse-3] 'dialog-handle-mouse)
  (define-key dialog-mode-map [drag-mouse-1] 'dialog-handle-mouse)
  (define-key dialog-mode-map [drag-mouse-2] 'dialog-handle-mouse)
  (define-key dialog-mode-map [drag-mouse-3] 'dialog-handle-mouse)
  )
  
(defun dialog-mode (&optional frameafy)
  "Define an existing buffer to be in DIALOG mode.  A dialog is a
buffer which contains interactive text groupings in rectangular
regions.  If optinal FRAMEAFY, then put this buffer into it's own
frame.  A call to dialog-fix-frame will resize it to fit around the
widgets."
  (kill-all-local-variables)
  (setq mode-name "Dialog")
  (setq major-mode 'dialog-mode)
  (use-local-map dialog-mode-map)
  (setq widget-toplevel-shell 
	(widget-toplevel "topLevel" :parent t :rx 0 :x 0 :ry 0 :y 0))
  (verify widget-toplevel-shell t)
  (run-hooks 'dialog-mode-hooks))

(defun dialog-refresh () "Refresh all visible widgets in this buffer"
  (draw widget-toplevel-shell))

(defun dialog-quit () "Quits a dialog."
  (bury-buffer))

(defun dialog-handle-kbd () "Read the last kbd event, and handle it."
  (interactive)
  (input widget-toplevel-shell last-input-char))

(defun dialog-handle-mouse (event) "Reads last mouse event, and handle it"
  (interactive "e")
  ;; First, check to see where the click is, and go there.  The cursor
  ;; will act as our in the widget fields.
  (mouse-set-point event)
  ( input widget-toplevel-shell event))

(defun create-widget (name class parent &rest resources)
  "Create a dialog with name NAME of class CLASS.  PARENT will be the
widget this new widget resides in, and RESOURCES is a list to be
passed to the CLASS routine"
  (let* ((con (class-constructor class))
	 (new (apply con name resources)))
    ;; add this child to the parent, which sets news parent field
    (add-child parent new)
    ;; call the verifier on this new widget.  Verify will transfor
    ;; construction values ('below, 'just-right, nil) into valid
    ;; values in pertinent fields by recursivly dropping from high
    ;; level widget restrictions to low-level widget restrictions
    (verify new t)
    new))

(defun transform-dataobject (thing-or-obj w dval fix)
  "Takes THING-OR-OBJ, and if it's a data-object, returns it,
otherwise create a new data object, and set it's initial value to
THING-OR-OBJ, and set its first watcher to W.  If THING-OR-OBJ is not
an object name id DVAL when created, also, if THING-OR-OBJ is nil,
and not some other value, then set it's value to DVAL instead.  If FIX
is nil, then return nil instead."
  (if (or (not (object-p thing-or-obj))
	  (not (same-class-p thing-or-obj data-object)))
      (if fix
	  (let ((newo (data-object dval))
		(nl nil))
	    ;; Add this to widget
	    (add-reference newo w)
	    (if thing-or-obj
		(set-value newo thing-or-obj w)
	      (set-value newo dval w))
	    newo)
	nil)
    thing-or-obj))

(defun widget-lock-over (w)
  "Called by a widget which wishes to grab cursor until the 'drag or
'click event is recieved."
  (let ((event nil))
    (track-mouse
      (while (progn (setq event (read-event))
		    (or (mouse-movement-p event)
			(eq (car-safe event) 'switch-frame)))
	(if (eq (car-safe event) 'switch-frame)
	    nil
	  ;; (mouse-set-point event)
	  (motion-input w event)))
      (if event (mouse-set-point event)))))

(defun goto-xy (x y)
  "Move cursor to position X Y in buffer, and add spaces and CRs if
needed."
  (let ((indent-tabs-mode nil)
	(num (goto-line y)))
    (if (and (= 0 num) (/= 0 (current-column))) (newline 1))
    (if (eobp) (newline num))
    ;; Now, a quicky column moveto/forceto method.
    (or (= (move-to-column x) x) (indent-to x))))
  
(defun insert-overwrite-face (string face &optional focus-face)
  "Insert STRING into buffer at point, and cover it with FACE"
  (if widget-toplevel-shell
      (let* ((pnt (point))
	     (end (+ pnt (length string))))
	(goto-char pnt)
	(insert string)
	(if (eobp) (save-excursion (insert "\n"))) ;always make sure there's a blank line
	(if (> (length string) (- (save-excursion (end-of-line) (point))
				  (point)))
	    (delete-region (point) (save-excursion (end-of-line) (point)))
	  (delete-char (length string)))
	(if (fboundp 'put-text-property)
	    (progn
	      (if face (put-text-property pnt end 'face face))
	      (if focus-face (put-text-property pnt end 'mouse-face focus-face))
	      )))))

(defun widget-bunch-o-chars (n char)
  "Return string of n dashes"
  (let ((ns (char-to-string char)) (nn 1))
    (while (< nn n)
      (setq nn (+ nn nn))
      (setq ns (concat ns ns)))
    (substring ns 0 n)))

(defun dialog-widget-tree-primitive ()
  "Displays the current dialog box's widget tree in another buffer"
  (interactive)
  (if (not widget-toplevel-shell) (error "Can't generate widget tree from this buffer"))
  (let ((mytls widget-toplevel-shell))
    (display-buffer (get-buffer-create "*WIDGET BROWSE*") t)
    (save-excursion
      (set-buffer (get-buffer "*WIDGET BROWSE*"))
      (erase-buffer)
      (goto-char 0)
      (dialog-browse-tree mytls "" "")
      )))

(defun dialog-browse-tree (this-root prefix ch-prefix)
  "Recursive part of browser, draws the children of the given class on
the screen."
  (if (not (object-p this-root)) (signal 'wrong-type-argument (list 'object-p this-root)))
  (let ((myname (object-name this-root))
	(chl (if (obj-of-class-p this-root widget-group)
		 (get-children this-root) 
	       nil))
	(fprefix (concat ch-prefix "  +--"))
	(mprefix (concat ch-prefix "  |  "))
	(lprefix (concat ch-prefix "     ")))
    (insert prefix)
    (if (not (and window-system (fboundp 'make-overlay)))
	(insert myname)
      (let ((no (make-overlay (point) (progn (insert myname) (point)))))
	(overlay-put no 'face 'bold)))
    (if t
	(insert "\n")
      (if chl
	  (if (= (length chl) 1)
	      (insert (format " -- [1 child]\n"))
	    (insert (format " -- [%d children]\n" (length chl))))
	(insert (format " -- [No children]\n"))))
    (while (cdr chl)
      (dialog-browse-tree (car chl) fprefix mprefix)
      (setq chl (cdr chl)))
    (if chl
	(dialog-browse-tree (car chl) fprefix lprefix))
    ))

(defun dialog-test ()
  "Creates a test dialog using as many widget features as currently works."
  (interactive)
  (switch-to-buffer (get-buffer-create "Dialog Test"))
  (dialog-mode)
  (let ((mytog (data-object "MyTog" :value t)))

    (create-widget "Fred" widget-label widget-toplevel-shell
		   :x 5 :y 5 :face 'modeline 
		   :label-value "Die in a pit")
    (create-widget "Click" widget-button widget-toplevel-shell
		   :x 5 :y 10 :label-value "Quit"
		   :box-face 'font-lock-comment-face
		   :activate-hook (lambda (obj reason) "Activate Quit Button"
				     (message "Quit!")
				     (dialog-quit)))
    (create-widget "Clack" widget-button widget-toplevel-shell
		   :x 25 :y 10 :label-value "Widget Tree"
		   :box-face 'font-lock-comment-face
		   :activate-hook (lambda (obj reason) "Draw a widget tree"
				     (dialog-widget-tree-primitive)
				     (dialog-quit)))
    (create-widget "Cluck" widget-button widget-toplevel-shell
		   :x 40 :y 10 :label-value "Class Tree"
		   :box-face 'font-lock-comment-face
		   :activate-hook (lambda (obj reason) "Draw a widget tree"
				     (eieio-browse)
				     (dialog-quit)))
    (create-widget "Clunk" widget-button widget-toplevel-shell
		   :x 60 :y 10 :label-value "About Dialog Mode"
		   :box-face 'font-lock-comment-face
		   :activate-hook (lambda (obj reason) "Draw a widget tree"
				     (describe-function 'dialog-mode)
				     (dialog-quit)))
    (let ((myframe (create-widget "Togg Frame" widget-frame widget-toplevel-shell
				   :x 5 :y 15
				   :frame-label "Toggle Tests..."
				   :box-face 'font-lock-reference-face)))
      (create-widget "Togg" widget-toggle-button myframe
		     :x 1 :y 1 :label-value "Toggle Me"
		     :face 'underline  :ind-face 'highlight
		     :state mytog
		     :activate-hook (lambda (obj reason) "Switcharoo!"
				       (message "Changed value")))
      (create-widget "Forceon" widget-button myframe
		     :x 20 :y 1 :label-value "Turn On"
		     :box-face font-lock-type-face
		     :activate-hook 
		     (list 'lambda '(obj reason) "Flip Tog"
			   (list 'set-value mytog t)))
      (create-widget "Forceoff" widget-button myframe
		     :x 50 :y 1 :label-value "Turn Off"
		     :face 'underline
		     :box-face font-lock-type-face
		     :activate-hook
		     (list 'lambda '(obj reason) "Flip Tog"
			   (list 'set-value mytog nil)))
      );; let
    (create-widget "MyText" widget-text-field widget-toplevel-shell
		   :x 5 :y 25 :width 20 :height 1
		   :value "My First String")
    )
  (dialog-refresh)
  (goto-char (point-min))
  )

;;; end of lisp
(provide 'dialog)
