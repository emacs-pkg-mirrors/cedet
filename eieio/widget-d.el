;;; widget-d.el - widget class definitions
;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.4
;;; RCS: $Id: widget-d.el,v 1.3 1996/07/28 18:34:23 zappo Exp $
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
;;; This file defines all the classes needed to create and maintain
;;; widgets in an emacs controlled environment using the eieio
;;; package.  Only definitions exist in this file.
;;;

(require 'eieio)

;;; Data object definition
;;;
;;; A data object, as discussed in the Fresco documentation, is just a
;;; blob where we store stuff.  Widgets store values in these objects,
;;; and follow their interface, so when the data is updated, other
;;; functions (gadgets, widgets, etc) can update themselves to the
;;; changes in the environment.

(defclass data-object ()
  ((value :initarg :value
	  :initform nil
	  :protection private)
   (reference :initarg :reference
	      :initform nil
	      :protection private)
   )
  "This defines a data-object which is uses for all dynamic viewers.
A label displays a string or number, and a scrollbar edits and
displays a number.  These widgets will use data-object to store thier
data")

;;; Widget definitions for the base set of widgets
;;;

(defclass widget-core ()
  ((parent :initarg :parent
	   :initform nil
	   :protection private)
   (managed :initarg :managed
	    :initform t
	    :protection private)
   (watched-symbols :initarg :watched-symbols
		    :initform nil
		    :protection private)
   )
  "Class for core widget.  This is the widget all other widgets are 
based from.")

(defclass widget-gadget (widget-core)
  ((gadget-callback :initarg :gadget-callback
		    :initform nil)
   (gadget-watched-symbol :initarg :gadget-watched-symbol
			  :initform nil)
   )
  "This defines a gadget, or a non-visual widget.  This widget calls
field GADGET-CALLBACK with value of symbol changes")

(defclass widget-visual (widget-core)
  ((x :initarg :x
      :initform 0) 
   (y :initarg :y
      :initform 0)			; positions
   (face :initarg :face
	 :initform widget-default-face
	 :protection private)			; face used to draw widget
   (selected :initarg :selected
	     :initform nil		; t if currently active
	     :protection private)
   (handle-io :initarg :handle-io
	      :initform  nil)		; t if widget handles IO
   (rx :initarg :rx
       :protection private)
   (ry :initarg :ry
       :protection private)			; real X Y coords after modification
   )
  "Class for visual widget.  This is the widget all visible
displayed widgets are derived from")

(defclass widget-square (widget-visual)
  ((width :initarg :width)
   (height :initarg :height)			; dimentions (no defaults)
   (boxed :initarg :boxed
	  :initform nil			; t if we display a box
	  :protection private)
   (box-face :initarg :box-face
	     :initform widget-box-face		; face used to draw the box
	     :protection private)
   (box-char :initarg :box-char
	     :initform [?+ ?+ ?+ ?+ ?- ?|]	; chars used to draw a box
	     :protection private)
   )
  "This is the definition for square widgets.  This is created 
`just-in-case' we decide to have non-square widgets later")

;;
;; Some group types
;;

(defclass widget-group (widget-square)
  ((child-list :initarg :child-list
	       :initform nil)
   )
  "Definition for the group widget.  This is an intermediary type
whose job is to provide basic child management for higher level
widgets which contain children")

(defclass widget-toplevel (widget-group)
  ((buffer :initarg :buffer
	   :initform current-buffer)
   (handle-io :initarg :handle-io
	      :initform t)
   )
  "Definition for a toplevel shell, which contains all children widget
for a given buffer.")

(defclass widget-frame (widget-group)
  ((handle-io :initarg :handle-io
	      :initform t)
    (boxed :initarg :boxed
	   :initform t 
	   :protection private)
    (frame-label :initarg :frame-label
		 :initform nil 
		 :protection private)
    (position :initarg :position
	      :initform top-left 
	      :protection private)
    )
  "Definition for a frame, which can contain several children grouped
in a labeled box.")

(defclass widget-radio-frame (widget-frame)
  ((state :initarg :state
	  :initform nil)		;state used by our radio buttons
   )
  "Special frame class which behaves as a radio box.  Designed to only
contain widgets of type widget-radio-button.")

;;
;; The important label type
;;

(defclass widget-label (widget-square)
  ((label-value :initarg :label-value
		:initform nil)		;data object we display
   (justification :initarg :justification
		  :initform center)		;how to justify the text
					;'left, 'center, 'right
   (focus-face :initarg :focus-face
	       :initform nil)			;face used when under mouse
   (leftmargin :initarg :leftmargin
	       :initform 0)
   (rightmargin :initarg :rightmargin
		:initform 0)
   (topmargin :initarg :topmargin
	      :initform 0)
   (bottommargin :initarg :bottommargin
		 :initform 0)		;margins inside label
   )
  "Class for label widget.  This widget displays some value, which is
formated to text with the format value.  There are no IO events.")

;;
;; Button types
;;
(defclass widget-button (widget-label)
  ((arm-face :initarg :arm-face
	     :initform widget-arm-face)		; face used when armed
   (focus-face :initarg :focus-face
	       :initform widget-focus-face) ;face used when under mouse
   (boxed :initarg :boxed
	  :initform t
	  :protection private)			; we want to show a box
   (activate-hook :initarg :activate-hook
		  :initform nil)
   (help-hook :initarg :help-hook
	      :initform nil)
   (handle-io :initarg :handle-io
	      :initform t)			; inherited from visual, new default
   )
  "Class for a button widget.  This button will be CLICKED on, or will
have RET or SPC pressed while selected, and it will then call
activate-hook.")

(defclass widget-option-button (widget-button)
  ((option-indicator :initarg :option-indicator	;indicator for option button
		     :initform "<=>")
   (option-list :initarg :option-list	;list of items to choose from
		:initform nil)
   (option-obarray :initform nil	;option list converted to obarray
		   :protection private)
   (ind-face :initarg :ind-face		;face used on indicator
	     :initform widget-indicator-face)
   (state :initarg :state		;index into option
	  :initform 0)
   )
  "Class for option button widget.  This button will provide a menu
when clicked on.  The menu will consist of those items in
`option-list', and the chosen item will appear in the button.")

(defclass widget-toggle-button (widget-button)
  ((boxed :initarg :boxed
	  :initform nil
	  :protection private)		;turn button box off now
   (state :initarg :state
	  :initform nil)
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face) ;face used on indicator
   (showvec :initarg :showvec
	    :initform [ "[ ]" "[X]" ])	;how to represent ON/OFF cases
   )
  "Class for toggle button widget:initform  This button will be CLICKED, and
when successful clicks occur, a boolean value will be turned ON or
OFF, and a visible piece will be modified.")

(defclass widget-radio-button (widget-toggle-button)
  ((radio-index :initarg :radioindex	;our index for parent's state
		:initform 0)
   (parent-state :initform nil		;pointer to parent's state
		 :protection private)
   (showvec :initform [ "< >" "<O>" ])	;change type of indicator
   )
  "Class of toggle button which knows how to talk with several other
versions of itself in order to radio between different values.")

;;
;; Text types
;;
(defclass widget-text-field (widget-square)
  ((handle-io :initarg :handle-io
	      :initform t)
   (face :initarg :face
	 :initform widget-text-face)
   (spface :initarg :spface
	   :initform widget-text-button-face)
   (focus-face :initarg :focus-face
	       :initform widget-text-focus-face) ;face used when under mouse
   (keymap :initarg :keymap
	   :initform nil)
   (disppos :initarg :disppos
	    :initform 0)			;first displayed character
   (keycache :initarg :keycache
	     :initform nil)			;cache of keys in multi-key sequence
   (value :initarg :value
	  :initform nil)			;value displayed (hidden Buffer)
   )
  "Class for a text field widget.  This will accept user-typed text,
and stuff like that for only one line of text.")

;;; end of lisp
(provide 'widget-d)
