;;; widget-d.el - widget class definitions
;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.4
;;; RCS: $Id: widget-d.el,v 1.6 1996/10/19 14:41:09 zappo Exp $
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
	  :docstring "Normal lisp value of any type"
	  :protection private)
   (reference :initarg :reference
	      :initform nil
	      :docstring "List of objects looking at me.  The method `update-symbol' is called
for each object whenever the value is modified."
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
	   :docstring "The parent widget conting this widget."
	   :protection private)
   (managed :initarg :managed
	    :initform t
	    :docstring "True if this widget is active (unused)"
	    :protection private)
   (watched-symbols :initarg :watched-symbols
		    :initform nil
		    "List of symbols I watch (unused)"
		    :protection private)
   )
  "Class for core widget.  This is the widget all other widgets are 
based from.")

(defclass widget-visual (widget-core)
  ((x :initarg :x
      :initform 0
      :docstring "The associative X position in a buffer to parent") 
   (y :initarg :y
      :initform 0
      :docstring "The associative Y position in a buffer to parent")
   (nx :initform 0
       :docstring "The normalized X position relative to parent.")
   (ny :initform 0
       :docstring "The normalized Y position relative to parent.")
   (face :initarg :face
	 :initform widget-default-face
	 :protection private
	 :docstring "Face used to draw this widget")
   (selected :initarg :selected
	     :initform nil
	     :docstring "(unused)"
	     :protection private)
   (handle-io :initarg :handle-io
	      :initform  nil
	      :docstring "t if this widget accepts input")
   (handle-motion :initform nil
		  :docstring "t if this widget handles it's own motion events")
   (rx :initarg :rx
       :docstring "Real S position in buffer"
       :protection private)
   (ry :initarg :ry
       :docstring "Real Y position in buffer"
       :protection private)
   )
  "Class for visual widget.  This is the widget all visible
displayed widgets are derived from")

(defclass widget-square (widget-visual)
  ((width :initarg :width
	  :docstring "Width of the square widget")
   (height :initarg :height
	  :docstring "Height of the square widget")
   (boxed :initarg :boxed
	  :initform nil
	  :docstring "t if a box is to be drawn around this widget"
	  :protection private)
   (box-face :initarg :box-face
	     :initform widget-box-face
	     :docstring "Face used on the box (if drawn)"
	     :protection private)
   (box-char :initarg :box-char
	     :initform [?+ ?+ ?+ ?+ ?- ?|]
	     :docstring "Character set used the draw the box.  The vector is 
[ upperright upperleft bottomright bottomleft horizontal vertical ]"
	     :protection private)
   )
  "This is the definition for square widgets.  This is created 
`just-in-case' we decide to have non-square widgets later")

;;
;; Some group types
;;

(defclass widget-group (widget-square)
  ((child-list :initarg :child-list
	       :initform nil
	       :docstring "List of children this group needs to manage")
   )
  "Definition for the group widget.  This is an intermediary type
whose job is to provide basic child management for higher level
widgets which contain children")

(defclass widget-toplevel (widget-group)
  ((buffer :initarg :buffer
	   :initform current-buffer
	   :docstring "Buffer this dialog resides in")
   (logical-child-list :initform nil
		       :docstring 
		       "Maintained by the top-level shell, this slot contains a list of all the children in thier logical order
for the purpose of tab-stepping across them"
		       :protection private)
   (handle-io :initform t)
   )
  "Definition for a toplevel shell, which contains all children widget
for a given buffer.")

(defclass widget-frame (widget-group)
  ((handle-io :initform t)
    (boxed :initform t 
	   :protection private)
    (frame-label :initarg :frame-label
		 :initform nil 
		 :docstring "Label to place on the edge of our frame."
		 :protection private)
    (position :initarg :position
	      :initform top-left
	      :docstring "Where the `frame-label' will reside.  Valid values are symbols
consisting of substrings of left, right, center, top, and bottom.
top-left is assumed if valid values are not found."
	      :protection private)
    )
  "Definition for a frame, which can contain several children grouped
in a labeled box.")

(defclass widget-radio-frame (widget-frame)
  ((state :initarg :state
	  :initform nil
	  :docstring "Current index of the select radio button")
   )
  "Special frame class which behaves as a radio box.  Designed to only
contain widgets of type widget-radio-button.")

;;
;; The important label type
;;

(defclass widget-label (widget-square)
  ((label-value :initarg :label-value
		:initform nil
		:docstring "The object to display on ourselves")
   (label-list :initarg nil
	       :initform nil
	       :docstring "The label value is transformed into this list, which is broken
into substrings around carriage returns."
	       :protection private)
   (justification :initarg :justification
		  :initform center
		  :docstring "how to justify the text.  Valid values are 'left, 'center, 'right")
   (focus-face :initarg :focus-face
	       :initform nil
	       :docstring "Face used when mouse passes over this text"
	       :protection private)
   (leftmargin :initarg :leftmargin
	       :initform 0
	       :docstring "Size of left space to format around")
   (rightmargin :initarg :rightmargin
		:initform 0
		:docstring "Size of right space to format around")
   (topmargin :initarg :topmargin
	      :initform 0
	      :docstring "Size of space above this label to format around")
   (bottommargin :initarg :bottommargin
		 :initform 0
		 :docstring "Size of space below this labe to format around.")
   )
  "Class for label widget.  This widget displays some value, which is
formated to text with the format value.  There are no IO events.")

;;
;; Button types
;;
(defclass widget-button (widget-label)
  ((arm-face :initarg :arm-face
	     :initform widget-arm-face
	     :docstring "Face used when this button has been pushed."
	     :protection private)
   (focus-face :initarg :focus-face
	       :initform widget-focus-face
	       :protection private)
   (boxed :initarg :boxed
	  :initform t
	  :protection private)
   (activate-hook :initarg :activate-hook
		  :initform nil
		  :docstring "Function to call when a user clicks this button")
   (help-hook :initarg :help-hook
	      :initform nil
	      :docstring "Function to call when clicked with the last mouse button which will
display some help in the minibuffer.")
   (handle-io :initarg :handle-io
	      :initform t)
   )
  "Class for a button widget.  This button will be CLICKED on, or will
have RET or SPC pressed while selected, and it will then call
activate-hook.")

(defclass widget-option-button (widget-button)
  ((option-indicator :initarg :option-indicator
		     :initform "<=>"
		     :docstring "String used to show this is an option button")
   (option-list :initarg :option-list
		:initform nil
		:docstring "List of strings which are the options")
   (option-obarray :initform nil
		   :protection private
		   :docstring "Obarray used for command line reading of symbols")
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face
	     :docstring "Face used on the `option-indicator'"
	     :protection private)
   (state :initarg :state
	  :initform 0
	  :docstring "Numerical index into list of strings representing the current value.")
   )
  "Class for option button widget.  This button will provide a menu
when clicked on.  The menu will consist of those items in
`option-list', and the chosen item will appear in the button.")

(defclass widget-toggle-button (widget-button)
  ((boxed :initarg :boxed
	  :initform nil
	  :protection private)
   (state :initarg :state
	  :initform nil
	  :docstring "Current value of the toggle button")
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face
	     :docstring "Face used on toggle indicator"
	     :protection private)
   (showvec :initarg :showvec
	    :initform [ "[ ]" "[X]" ]
	    :docstring "Vector [ false true ] of strings used to show the state")
   )
  "Class for toggle button widget:initform  This button will be CLICKED, and
when successful clicks occur, a boolean value will be turned ON or
OFF, and a visible piece will be modified.")

(defclass widget-radio-button (widget-toggle-button)
  ((radio-index :initarg :radioindex
		:initform 0
		:docstring "Index referencing the parent's state, which then lets us know if we
are toggled on or off")
   (parent-state :initform nil
		 :docstring "Data object pointing the parent's state"
		 :protection private)
   (showvec :initform [ "< >" "<O>" ])	;change type of indicator
   )
  "Class of toggle button which knows how to talk with several other
versions of itself in order to radio between different values.")

;;
;; Scrollbar types
;;
(defclass widget-scale (widget-square)
  ((handle-io :initform t)
   (boxed :initform t)
   (state :initarg :state
	  :initform 0
	  :docstring "Current value of this scale")
   (minimum :initarg :minimum
	    :initform 0
	    :docstring "Smallest allowed value")
   (maximum :initarg :maximum
	    :initform 10
	    :docstring "Largest allowed value")
   (direction :initarg :direction
	      :initform 'horizontal
	      :docstring "Direction to draw the scale")
   (end-buttons :initarg :end-buttons
		:initform nil
		:docstring "Text used to inc/dec this scale")
   (marker :initarg :marker
	   :initform "#"
	   :docstring "Character used to draw the value indicator")
   )
  "Class of scale.  A scale is mearly a thumb marker displaying the current
value of some number graphically across some random number of text 
characters.")

(defclass widget-scrollbar (widget-scale)
  ((end-buttons :initarg :end-buttons
		:initform [ [ "<" ">" ] [ "/\\" "\\/" ] ])
   (range :initarg :range
	  :initform 10
	  :docstring "Range of currently viewable area"))
  "Class for a scrollbar.  A scrollbar also will have a visual range
where the thumbtac changes size based on RANGE.")
   

;;
;; Text types
;;
(defclass widget-text-field (widget-square)
  ((handle-io :initarg :handle-io
	      :initform t)
   (handle-motion :initform t)
   (face :initarg :face
	 :initform widget-text-face
	 :protection private)
   (spface :initarg :spface
	   :initform widget-text-button-face
	   :docstring "Face used on text buttons (scroll left/right/down/up)"
	   :protection private)
   (focus-face :initarg :focus-face
	       :initform widget-text-focus-face
	       :protection private)
   (keymap :initarg :keymap
	   :initform nil
	   :docstring "Keymap used to interpret text")
   (disppos :initarg :disppos
	    :initform 0
	    :docstring "Current position in the screen to display info from")
   (keycache :initarg :keycache
	     :initform nil
	     :docstring "Collecting keypresses for multi keystroke keys.")
   (value :initarg :value
	  :initform nil
	  :docstring "The string we are editing")
   )
  "Class for a text field widget.  This will accept user-typed text,
and stuff like that for only one line of text.")

;;; end of lisp
(provide 'widget-d)
