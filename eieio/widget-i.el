;;; widget-i.el - simulate widgets in emacs text window
;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.4
;;; RCS: $Id: widget-i.el,v 1.1 1996/03/28 03:51:01 zappo Exp $
;;; Keywords: OO widget
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
;;;
;;; This requires the widget definition file (widget-d) and supplies
;;; the functionality behind all the classes by defining their
;;; methods.  All mundane repetitive work is done in dialog.el. (Such
;;; as creating buffers, modes, and the top-level shell.)
;;;

(require 'eieio)			;objects
(require 'widget-d)			;widget definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Data-Object method definitions
;;;      
(defmethod add-reference ((this data-object) widget)
  "Adds WIDGET to the list of widgets currently referencing THIS"
  ;; Add to our list
  (if (not (member widget (oref this reference)))
      (oset this reference (cons widget (oref this reference)))))

(defmethod set-value ((this data-object) value &optional setter)
  "Set the value field of ourselves to VALUE"
  (if (not (equal (oref this value) value))
      (let ((refs (oref this reference)))
	(oset this value value)
	;; Now update everyone observing us, if setter is an object,
	;; make sure we don't call thier update function.
	(while refs
	  (if (not (equal (car refs) setter))
	      (save-excursion
		(update-symbol (car refs) this)))
	  (setq refs (cdr refs))))))

(defmethod get-value ((this data-object))
  "Get the value from the value out of the data object"
  (oref this value))

(defmethod render ((this data-object))
  "Return a string which represents the RENDERED version of our value.
To render anything in emacs, we have to turn it into a string, so this
is ok."
  (let ((v (oref this value)))
    (cond ((stringp v) v)
	  (t (format "%S" v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Widget method definitions
;;;

;;
;; Core
;;
(defmethod get-parent ((this widget-core))
  "Returns the parent widget of selected widget"
  (oref this parent))

(defmethod verify ((this widget-core) fix)
  "Verifies that fields in the CORE part are correct.  If FIX, then
fixable fields are adjusted, otherwise an error occurs."
  (if (and (not (oref this parent)) 
	   (not (equal (oref this parent) t)))
      (error "%s has no parent!" (object-name this)))
  )

(defmethod update-symbol ((this widget-core) sym)
  "Backup for update-symbol so we don't get errors if it's not defined for some 
broken reason."
  (message "No symbols used in this widget"))


;;
;; visual
;;
(defmethod verify ((this widget-visual) fix)
  "Verifies a visual widget to make sure it's values are allowed.  If
FIX then fix fields which are adjustable.  Call core verify when
done."
  ;; Fix up RX and RY with parent coords
  (if (and fix (object-p (get-parent this))
	   (object-p (get-parent this))
	   (oref (get-parent this) rx))
      (progn
	(oset this rx (+ (oref this x) (oref (get-parent this) rx)))
	(oset this ry (+ (oref this y) (oref (get-parent this) ry)))
	)
    (if (and fix (object-p (get-parent this)))
	(progn
	  (message "Parent %s rx is %s %s" 
		   (object-name (get-parent this))
		   (oref (get-parent this) rx)
		   (oref (get-parent this) ry))
	  (error "Could not set real XY positions for object %s"
		 (object-name this)))))
  ;; Now make sure core parts are valid
  (call-next-method))

(defmethod picked ((this widget-visual) x y)
  "Is this widget picked under X Y"
  nil)


;;
;; square
;;
(defmethod verify ((this widget-square) fix)
  "Verifies that a square widget has resonable size constraints"
  (if (not (and (oref this x) (oref this y) 
		(oref this width) (oref this height)))
      ;; unfixable in this instance
      (error "Square widget %s must have dimentions x,y and width,height"))
  (call-next-method))

(defmethod draw ((this widget-square))
  "Draw handles border drawing.  Is just able to draw a box, which
goes OUTSIDE the size specification of the widget itself, and does not
count when being picked."
  ;; Draw a box around ourselves
  (if (oref this boxed)
      (let* ((ch (widget-bunch-o-chars (oref this width) 
				       (aref (oref this box-char) 4)))
	     (br (concat (char-to-string (aref (oref this box-char) 0)) ch
			 (char-to-string (aref (oref this box-char) 1))))
	     (lr (concat (char-to-string (aref (oref this box-char) 2)) ch
			 (char-to-string (aref (oref this box-char) 3))))
	     (yc 0)
	     (x (oref this rx))
	     (y (oref this ry)))
	(goto-xy (1- x) (1- y))
	(insert-overwrite-face br (oref this box-face))
	(while (< yc (oref this height))
	  (goto-xy (1- x) (+ y yc))
	  (insert-overwrite-face (char-to-string (aref (oref this box-char) 5))
				 (oref this box-face))
	  (goto-xy (+ x (oref this width)) (+ y yc))
	  (insert-overwrite-face (char-to-string (aref (oref this box-char) 5))
				 (oref this box-face))
	  (setq yc (1+ yc)))
	(goto-xy (1- x) (+ y (oref this height)))
	(insert-overwrite-face lr (oref this box-face)))))

(defmethod picked ((this widget-square) x y)
  "Return t if X,Y lies within a square defined by our attributes"
  (let ((mod 0))
    (if (oref this boxed)
	(setq mod 1))
    (and (oref this handle-io)
	 (>= x (- (oref this rx) mod))
	 (< x (+ (oref this rx) (oref this width) mod))
	 (>= y (- (oref this ry) mod))
	 (< y (+ (oref this ry) (oref this height) mod)))))

(defmethod move ((this widget-square) x y)
  "Move ourselves to position X Y"
  (oset this x x)
  (oset this y y)
  (verify this t)
  (draw this))

(defmethod resize ((this widget-square) width height)
  "Resize ourselves to dimentions width height"
  (oset this width width)
  (oset this height height)
  (verify this t)
  (draw this))


;;
;; group
;;
(defmethod verify ((this widget-group) fix)
  "Verifies that a group widget has valid values in dynamic fields"
  ;; make sure our size is set...
  (if (not (oref this width)) (oset this width 0))
  (if (not (oref this height)) (oset this height 0))
  ;; verify our other parts
  (call-next-method)
  )

(defmethod add-child ((this widget-group) child)
  "Add widget CHILD to our personal list of child widgets"
  ;; Add to our list
  (oset this child-list (cons child (oref this child-list)))
  ;; make sure we are marked as that widgets parent.  To do this, we
  ;; must cheat so that THIS is set to child, and then we may set that
  ;; widgets field, and the scoped class to allow us access to private
  ;; field.  *THIS IS A CHEAT - Should implement Friends!*
  (let ((me this) (scoped-class (object-class child)) (this child))
    (oset child parent me)))

(defmethod get-children ((this widget-group))
  "Return our list of children widgets"
  (oref this child-list))

(defmethod input ((this widget-group) char-or-event)
  "Handles the input event char-or-event by passing it to it's
children.  If it is passed to a child, return t, else return nil"
  (cond ((and (numberp char-or-event) (= char-or-event ?\C-i))
	 ;; In this case, move forward one widget
	 )
	((and (numberp char-or-event) (= char-or-event ?\M-i))
	 ;; In this case, move backward one widget
	 )
	(t
	 (let ((x (current-column))
	       (y (count-lines (point-min) (point)))
	       (lop (oref this child-list)))
	   ;; find the child who has been clicked
	   (while lop
	     (if (not (picked (car lop) x y))
		 nil
	       (input (car lop) char-or-event)
	       (setq lop nil))
	     (setq lop (cdr lop)))))))

(defmethod draw ((this widget-group))
  "Draw the basic group widget.  Basically all our children, with
thier X,Y offset by our X,Y"
  ;; We must check our size when we are about to draw ourselves
  (let ((maxw (oref this width))
	(maxh (oref this height))
	(l (oref this child-list)))
    (while l
      (let ((tw (+ (oref (car l) x) (oref (car l) width) 2))
	    (th (+ (oref (car l) y) (oref (car l) height) 1)))
	(if (< maxw tw) (setq maxw tw))
	(if (< maxh th) (setq maxh th)))
      (setq l (cdr l)))
    (oset this width maxw)
    (oset this height maxh))
  ;;draw any visuals we have
  (call-next-method)
  ;; draw our children
  (let ((kids (oref this child-list)))
    (while kids
      (if (obj-of-class-p (car kids) widget-visual)
	  (progn
	    ;(message "Refreshing object %s" (object-name (car kids)))
	    ;(sit-for 1)
	    (draw (car kids))))
      (setq kids (cdr kids))
      ))
  ;; (message "Done...")
  )

;;
;; frame
;;
(defmethod verify ((this widget-frame) fix)
  "Verify a frame widget"
  ;; call parent's verify first to set our position, etc
  (call-next-method)  
  ;; now fix up our label...
  (let* ((tol (oref this frame-label))
	 (lw (if (and (object-p tol) 
		      (obj-of-class-p tol widget-label))
		 ;; it's already a label of some sort
		 tol
	       (if (and (not fix) (not (stringp tol)))
		   ;; it's not a string
		   nil
		 ;; Its a string... make a label of some sort
		 (save-match-data
		   (let ((posstr (symbol-name (oref this position))))
		     (create-widget (format "label on %s" (object-name this))
				    widget-label this 
				    :label-value tol
				    :justification
				    (cond 
				     ((string-match "center" posstr)
				      'center)
				     ((string-match "right" posstr)
				      'right)
				     (t
				      'left))
				    :x (cond 
					((string-match "center" posstr)
					 (/ (- (oref this width)
					       (length tol))
					    2))
					((string-match "right" posstr)
					 (- (oref this width)
					    (length tol)
					    2))
					(t
					 2))
				    :y (if (string-match "top" posstr)
					   -1
					 (1+ (oref this height))))))))))
    (if (and lw fix)
	(oset this frame-label lw))))


;;
;; label
;;
(defmethod verify ((this widget-label) fix)
  "Verify the label widget's componants."
  ;; Make sure the label-value is a data object
  (let ((lv (transform-dataobject  (oref this label-value) this
				   (object-name this) fix)))
    (if lv
	(oset this label-value lv)
      (error "Label value for %s is not a data-object!" (object-name this))))
  ;; If no width/height, try to set them
  (if (not (oref this width))
      (if fix
	  (let ((lv (oref this label-value)))
	    (oset this width (+ (length (format "%s" (render lv)))
				(oref this leftmargin) (oref this rightmargin))))
	(error "Label %s width is invalid" (object-name this))))
  (if (not (oref this height))
      (if fix
	  (oset this height (+ 1 (oref this topmargin) 
			       (oref this bottommargin)))
	(error "Label %s height is invalid" (object-name this))))
  ;; Now verify the rest
  (call-next-method))

(defmethod draw ((this widget-label))
  "Refresh a label widget."
  (let* ((x (+ (oref this rx) (oref this leftmargin)))
	 (y (+ (oref this ry) (oref this topmargin)))
	 (w (- (oref this width) (oref this leftmargin) (oref this rightmargin)))
	 (h (- (oref this height) (oref this topmargin) (oref this bottommargin)))
	 (ds (render (oref this label-value)))
	 (s (length ds))
	 (j (oref this justification)))
    (setq y (+ y (/ h 2)))
    (if (> s w)
	(progn
	  (setq ds (substring ds 0 w))
	  (setq s (length ds))))
    (cond ((string-match "\n" ds)
	   (error "Multiline strings not supported in labels yet")
	   )
	  ((eq j 'center)
	   (setq x (+ x (/ (- w s) 2))))
	  ((eq j 'right)
	   (setq x (+ x (- w s))))
	  ((eq j 'left)
	   )
	  (t (error "Internal label error")))
    (goto-xy x y)
    (insert-overwrite-face ds (oref this face)))
  (call-next-method))

(defmethod input ((this widget-label) coe)
  "Handle IO for a label"  (ding t))


;;
;; button
;;
(defmethod verify ((this widget-button) fix)
  "Verify button parameters"
  ;; Now verify the rest
  (call-next-method))

(defmethod show-arm ((this widget-button) onoff)
  "Show the arming of the widget based on ONOFF"
  (save-excursion
    (if onoff
	(let ((oface (oref this face)))
	  (oset this face (oref this arm-face))
	  (draw this)
	  (oset this face oface))
      (draw this))))

(defmethod input ((this widget-button) coe)
  "What to do if clicked upon by the mouse"
  (if (and (listp coe) (eventp coe))
      (cond (;; Someone pressed us!
	     (member 'down (event-modifiers coe))
	     (show-arm this t)	;arm it
	     (widget-lock-over this)	;visually display arming
	     (let ((x (current-column))
		   (y (count-lines (point-min) (point))))
	       (if (picked this x y)
		   (active-actions this 'click))
	       (show-arm this nil)))
	    ;; ignore the other stuff
	    (t (message "Ignore event %S" (event-modifiers coe))))
    (if (or (eq coe 'return)
	    (= coe ? )
	    (= coe ?\n)
	    (= coe ?\f))
	(progn
	  (show-arm this t)
	  (active-actions this coe)
	  (show-arm this nil))
      (message "RET or SPC to activate button!"))))

(defmethod motion-input ((this widget-button) coe)
  "What do do with motion events from widget-lock-over function"
  (if (and (listp coe) (eventp coe))
      (if (not (event-modifiers coe))
	  (let ((x (car (posn-col-row (event-end coe))))
		;; this _should_ handle scrolling in a window
		(y (+ (cdr (posn-col-row (event-end coe))) 1)))
	    (show-arm this (picked this x y))))))

(defmethod active-actions ((this widget-button) reason)
  "Called when activated to handle any special cases for child widgets"
  (if (oref this activate-hook)
      (funcall (oref this activate-hook) this reason)))

(defmethod draw ((this widget-button))
  "Draw the button widget to the display"
  ;; now draw the label part
  (save-excursion
    (call-next-method)))


;;
;; Toggle Button
;;

(defmethod verify ((this widget-toggle-button) fix)
  "Verifies that a toggle button correctly represented."
  ;; Verify my state button
  (let ((lv (transform-dataobject (oref this state) this "Boolean" fix)))
    (if lv
	(progn
	  (oset this state lv)
	  (add-reference lv this))
      (error "State variable for toggle %s is not a data-object!"
	     (object-name this)))
    (if (stringp (get-value lv))
	(if fix (set-value lv nil this))))
  ;; create the special left margin
  (oset this leftmargin (1+ (length (aref (oref this showvec) 0))))
  ;; Verify parent class members
  (call-next-method))

(defmethod update-symbol ((this widget-toggle-button) sym)
  "If sym is STATE field, then update ourselves"
  (if (eq sym (oref this state))
      (draw this)
    (call-next-method)))

(defmethod draw ((this widget-toggle-button))
  "Draws a toggle button to the display"
  (save-excursion
    ;; now draw the indicator
    (let* ((val1 (oref this state))
	   (val2 (get-value val1)))
      (goto-xy (oref this rx) (oref this ry))
      (insert-overwrite-face (aref (oref this showvec) (if val2 1 0))
			     (oref this ind-face)))
    ;; draw the rest
    (call-next-method)))

(defmethod active-actions ((this widget-toggle-button) reason)
  "When the button part is activated, then we must toggle our state"
  ;; Set our state
  (if (get-value (oref this state))
      (set-value (oref this state) nil)
    (set-value (oref this state) t this))
  ;; do our parents version
  (call-next-method))


;;
;; Text
;;

(defmethod verify ((this widget-text-field) fix)
  "Verifies the text widget is ok"
  ;; verify the textual value as a data-object
  (let ((tv (transform-dataobject (oref this value) this "" fix)))
    (if tv
	(progn
	  (oset this value tv)
	  (add-reference tv this))
      (error "Text field value for %s is not a data-object."
	     (object-name this))))
  ;; now set the keymap we will use
  (if (not (oref this keymap))
      (if fix
	  (oset this keymap (make-sparse-keymap))))
  ;; Verify parent class members
  (call-next-method))

(defmethod draw ((this widget-text-field))
  "Render's a text widget onto the display"
  (let* ((myto (oref this value))
	 (myts (get-value myto))
	 (tlen (oref this width))
	 (os (substring myts (oref this disppos)))
	 (nflag nil)
	 (sflag nil)
	 )  
    (goto-xy (1- (oref this x)) (oref this y))
    ;; check for characters off to the left
    (insert-overwrite-face (if (> (oref this disppos) 0) "<" " ")
			   (oref this spface))
    ;; check for newline inside string
    (if (string-match "\\(\n\\)" os)
	(setq nflag t
	      sflag nil
	      os (substring os 0 (match-beginning 1))))
    ;; check for string too long
    (if (> (length os) tlen)
	(setq sflag t
	      os (substring os 0 tlen)))
    ;; see if string is too short
    (if (< (length os) tlen)
	(setq os (concat
		  os
		  (widget-bunch-o-chars (- tlen (length os)) ? ))))
    ;; insert the string
    (insert-overwrite-face os (oref this face))
    ;; show more-characters this way strings
    (if nflag (insert-overwrite-face "v" (oref this spface))
      (if sflag (insert-overwrite-face ">" (oref this spface))
	(insert-overwrite-face " " (oref this spface)))))
  (call-next-method)
  )

(defmethod input ((this widget-text-field) coe)
  "Handle user input events in the text field"
  ;; first find out if we will be doing any edits at all
  (if (and (listp coe) (eventp coe))
      ()				;ignore it for now
    (let ((cc (if (numberp coe)
		  (char-to-string coe)
		(lookup-key function-key-map (make-vector 1 coe))
		)))
      (if (fboundp (global-key-binding cc))
	  ;; In this case, we have a one-keystroke edit
	  (let ((cp (- (current-column) (oref this rx)))
		(mo (oref this value))
		(mv nil)
		(rp nil)
		;; make sure no new lines are added
		(next-line-add-newlines nil))
	    ;; do the simulated edit in a seperate buffer
	    (save-excursion
	      (set-buffer (get-buffer-create "*Text Widget Scratch*"))
	      (erase-buffer)
	      (insert (get-value mo))
	      (goto-char (+ cp (oref this disppos) 1))
	      (command-execute (lookup-key global-map cc))
	      (setq rp (1- (point)))
	      (setq mv (buffer-string)))
	    ;; reposition disppos based on cursor position
	    (if (and (/= (oref this disppos) 0)
		     (>= (oref this disppos) rp))
		(let ((newsize (if (< (- rp 1) 0) 0 (- rp 1))))
		  (oset this disppos newsize)))
	    (if (>= (1+ rp) (+ (oref this disppos) (oref this width)))
		(let ((newsize  (- rp (oref this width) -2)))
		  (oset this disppos newsize)))
	    ;; Now redraw the text
	    (save-excursion
	      (set-value mo mv this)
	      (draw this))
	    ;; place the cursor
	    (message "disppos is %d rp is %d " (oref this disppos) rp)
	    (goto-xy (+ (oref this rx) (- rp (oref this disppos)))
		     (oref this ry))
	    (sit-for 1)
	    )))))

;;; end of lisp
(provide 'widget-i)
