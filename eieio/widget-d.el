;;; widget-d.el - widget class definitions
;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: widget-d.el,v 1.9 1996/11/18 00:21:46 zappo Exp $
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

(defvar widget-d-load-hooks nil
  "List of hooks run after this file is loaded.  Permits users to
customize the default widget behavior using `oset-default'")
         
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
	  :accessor get-value
	  :docstring "Lisp object which represents the data this object maintains."
	  :protection private)
   (reference :initarg :reference
	      :initform nil
	      :docstring "List of objects looking at me.  The method `update-symbol' is called
for each member of `reference' whenever `value' is modified."
	      :protection private)
   )
  "This defines a `data-object' which is used for all widgets
maintaining some value.  For example, a `widget-label' displays a
string or number, and a `widget-scrollbar' edits a number.  These
widgets will use data-object to store their data.")

;;; Widget definitions for the base set of widgets
;;;

(defclass widget-core ()
  ((parent :initarg :parent
	   :initform nil
	   :accessor get-parent
	   :docstring "A widget of type `widget-group' of which this is a child.")
   (managed :initarg :managed
	    :initform t
	    :docstring "(unused) t if this widget is active."
	    :protection private)
   (watched-symbols :initarg :watched-symbols
		    :initform nil
		    :docstring "List of symbols this widget cares about."
		    :protection private)
   )
  "Class for core widget.  This is the widget all other widgets are 
based from.")

(defclass widget-gadget-translator (widget-core)
  ((watch :initarg :watch
	  :docstring "A `data-object' to watch.  When it changes, run the translator function.")
   (change :initarg :change
	   :docstring "A `data-object' to change whenever `watch' is modified.")
   (translate-function :initarg :translate-function
		       :initform (lambda-default (watch change) nil)
		       :docstring
		       "Function to call when `watch' changes.  It should modify the `data-object'
`change' from it's value.  It takes two parameters WATCH and CHANGE.")
   )
  "Non-visible class for a gadget translator.  The translator's job is
to convert the `data-object' in `watch' to some new value, and store it
in `change'.  This is useful for translating indices into descriptions
or something like that.")

(defclass widget-visual (widget-core)
  ((x :initarg :x
      :initform nil
      :docstring "The X position in a buffer relative to parent.")
   (y :initarg :y
      :initform nil
      :docstring "The Y position in a buffer relative to parent.")
   (resizeable :initarg :resizeable
	       :initform nil
	       :docstring "(unused) t if this widget has a tendency to resize itself.")
   (nx :initform 0
       :docstring "The normalized X position relative to parent. (After geometry management)")
   (ny :initform 0
       :docstring "The normalized Y position relative to parent. (After geometry management)")
   (marker :initarg :marker
	   :initform nil
	   :protection private
	   :docstring "(Unused) Marker in the dialog buffer from which all drawing commands are based.")
   (face :initarg :face
	 :initform widget-default-face
	 :protection private
	 :docstring "Face used to draw this widget.")
   (handle-io :initarg :handle-io
	      :initform  nil
	      :docstring "t if this widget accepts keyboard or mouse input.")
   (handle-motion :initform nil
		  :docstring "t if this widget handles it's own motion events")
   (rx :docstring "Real X position in buffer"
       :protection private)
   (ry :docstring "Real Y position in buffer"
       :protection private)
   )
  "Class for visual widget.  This is the widget all visible widgets
are derived from. Its definition includes an X,Y position which
defines it's offset inside the parent, and can include its offset from
other widgets which are children of `parent'.
@xref{(dialog) Geometry Management}") 

(defclass widget-square (widget-visual)
  ((width :initarg :width
	  :docstring "Width in characters")
   (height :initarg :height
	   :docstring "Height in characters")
   (boxed :initarg :boxed
	  :initform nil
	  :docstring "t if a box is to be drawn around this widget")
   (box-face :initarg :box-face
	     :initform widget-box-face
	     :docstring "Face used on the box (if drawn)"
	     :protection private)
   (box-char :initarg :box-char
	     :initform [?+ ?+ ?+ ?+ ?- ?- ?| ?|]
	     :docstring "Character set used the draw the box.  The vector is 
[ upper-right upper-left bottom-right bottom-left horizontal vertical ]"
	     :protection private)
   (box-sides :initarg :box-sides
	      :initform [ t t t t ]
	      :docstring "Vector which represents those sides of the
box which will be drawn, where a t in a position indicates the side is
to be drawn.  The vector is of the form [ left right top bottom ]")
   )
  "This is the definition for visible widgets which have a square
shape.  This provides basic sizing schemes and box drawing utilities
for widgets that are square.")

;;
;; Some group types
;;

(defclass widget-group (widget-square)
  ((child-list :initarg :child-list
	       :initform nil
	       :accessor get-children
	       :docstring "List of children this group needs to manage")
   )
  "Definition for the group widget.  This is an intermediary type
whose job is to provide basic child management for higher level
widgets which contain children such as `widget-toplevel' and
`widget-frame'.  This widget knows how to add new children, and manage
its size based on the positions and sizes of it's children.  It also
knows how to create navigation lists.")

(defclass widget-toplevel (widget-group)
  ((rx :initarg :rx)			;create initargs for real parts
   (ry :initarg :ry)			;for toplevel only
   (buffer :initarg :buffer
	   :initform current-buffer
	   :docstring "The buffer this dialog resides in.")
   (logical-child-list 
    :initform nil
    :docstring 
    "Contains a list of all the children and grand-children in their
logical order for the purpose of tab-stepping across them"
    :protection private)
   (handle-io :initform t)
   )
  "Definition for a top-level shell. This maintains the interface to
emacs' buffer, and is a parent of all children displayed in the
buffer.  This will be created automatically with a call to
`dialog-mode' when designing a screen.")

(defclass widget-frame (widget-group)
  ((handle-io :initform t)
   (boxed :initform t)
   (frame-label :initarg :frame-label
		:initform t
		:docstring 
		"Label to place on the edge of our frame.  An initial value of t means
to use the object name.  An initial value of nil means no title.  If
this is initialized with a string, then that string is used as the
label's string.  The created widget will be a `widget-label'.  If this
is a widget, then that widget is positioned on the edge of the
screen."
		:protection private)
   (position :initarg :position
	     :initform top-left
	     :docstring "Where the `frame-label' will reside.  Valid values are symbols
consisting of substrings of left, right, center, top, and bottom."
	     :protection private)
   )
  "Definition for a frame, which can contain several children grouped
in a box with a `widget-label' on one edge (covering a part of the box).")

(defclass widget-radio-frame (widget-frame)
  ((state :initarg :state
	  :initform 0
	  :docstring "Current index of the selected radio button")
   )
  "Special frame class which behaves as a radio box.  Designed to only
contain widgets of type `widget-radio-button'.")

(defclass widget-labeled-text (widget-group)
  ((handle-io :initform t)
   (label :initarg :label
	  :initform nil
	  :docstring "Text object displayed with a `widget-label' before a `widget-text-field'.")
   (unit :initarg :unit
	 :initform nil
	 :docstring "Text object displayed with a `widget-label' after the `widget-text-field'
which represents some sort of typing which would be useful to know.")
   (value :initarg :value
	  :initform nil
	  :docstring "The `data-object' we are going to edit with the text widget")
   (text-length :initarg :text-length
		:initform 20
		:docstring "The width passed to the `widget-text-field'")
   )
  "Special group widget which makes creating text fields with labels next to
them more convenient.")

;;
;; The important label type
;;
(defclass widget-label (widget-square)
  ((label-value :initarg :label-value
		:initform nil
		:docstring "The `data-object' to display on ourselves")
   (label-list :initarg nil
	       :initform nil
	       :docstring "The `label-value' is transformed into this list, which is broken
into substrings around carriage returns."
	       :protection private)
   (justification :initarg :justification
		  :initform center
		  :docstring "how to justify the text.  Valid values are 'left, 'center, 'right")
   (focus-face :initarg :focus-face
	       :initform nil
	       :docstring "Face used when mouse passes over `label-value'"
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
		 :docstring "Size of space below this label to format around.")
   )
  "Class for displaying labels.  The value of the label is determined
by the `data-object' stored in `label-value' which can be initialized
with a string, number, or other lisp object.  Supports strings with
carriage returns in them.")

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
   (activate-hook :initarg :activate-hook
		  :initform nil
		  :docstring "Function to call when a user clicks this button")
   (help-hook :initarg :help-hook
	      :initform nil
	      :docstring "Function to call when help is requested
about this button.  Default value is to display instructions about the
operation of this widget in the minibuffer.")
   (handle-io :initarg :handle-io
	      :initform t)
   )
  "Class for a button widget.  This is the starting point for all
interactive buttons.  This button will be CLICKED on, or will have RET
or SPC pressed while selected, and it will then call `activate-hook'.
If a push button is desired, it is better to use a widget of type
`widget-push-button' instead as it has a better visual effects.")

(defclass widget-push-button (widget-button)
  ((boxed :initarg :boxed
	  :initform t)
   (box-char :initarg :box-char
	     :initform [?  ?  ?  ?  ?  ?  ?< ?> ]
	     :protection private)
   (box-sides :initform [ t t nil nil ])
   (box-face :initarg :box-face
	     :initform widget-indicator-face
	     :protection private)
   ;; Add a little bit of margin
   (leftmargin :initarg :leftmargin
	       :initform 1)
   (rightmargin :initarg :rightmargin
		:initform 1)
   )
  "Class for a push button.  This button behaves as a `widget-button'
but with a different visual effect.  This is the preferred widget to
use as the `widget-button' is used as a starting point for all button
types.")

(defclass widget-option-button (widget-button)
  ((option-indicator :initarg :option-indicator
		     :initform "<=>"
		     :docstring "String printed to the left of the label in `left-margin' used to show this is an option button.")
   (option-list :initarg :option-list
		:initform nil
		:docstring "List of strings which are the options")
   (option-obarray :initform nil
		   :protection private
		   :docstring "Obarray used for command line reading of symbols")
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face
	     :docstring "Face used on the `option-indicator' string"
	     :protection private)
   (justification :initarg :justification
		  :initform left)
   (boxed :initform nil)
   (state :initarg :state
	  :initform 0
	  :docstring "`data-object' used as a numerical index into
list of strings representing the current value.")
   )
  "Class for option button widget.  This button will provide a menu
when clicked on.  The menu will consist of those items in
`option-list', and the chosen item will appear in the button's text.")

(defclass widget-toggle-button (widget-button)
  ((boxed :initform nil)
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
  "Class for toggle button widget.  This button will be CLICKED, and
when successful clicks occur, a boolean value will be turned ON or
OFF, and a visible piece will be modified based on `showvec'.")

(defclass widget-radio-button (widget-toggle-button)
  ((radio-index :initarg :radioindex
		:initform 0
		:docstring "Index indexing the parent's state, which then lets us know if we
are toggled on or off.  ie, if the parent's state is 1, and our index
is 0, then the state of this button will become nil.  This value does
not change during use.")
   (parent-state :initform nil
		 :docstring "Data object pointing the parent's state"
		 :protection private)
   (showvec :initform [ "< >" "<O>" ])	;change type of indicator
   )
  "Subclass of `widget-toggle-button' which knows how to talk with
several other instantiations of itself in order to radio between different
values.")

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
  "Not Implemented Completely Yet.

Class of scale.  A scale is merely a thumb marker displaying the current
value of some number graphically across some random number of text 
characters.")

(defclass widget-scrollbar (widget-scale)
  ((end-buttons :initarg :end-buttons
		:initform [ [ "<" ">" ] [ "/\\" "\\/" ] ])
   (range :initarg :range
	  :initform 10
	  :docstring "Range of currently viewable area"))
  "Not Implemented Completely Yet.

Class for a scrollbar.  A scrollbar also will have a visual range
where the thumbtack changes size based on RANGE.")
   

;;
;; Text types
;;
(defclass widget-text-field (widget-square)
  ((handle-io :initarg :handle-io
	      :initform t)
   (handle-motion :initform t)
   (height :initform 1)
   (face :initarg :face
	 :initform widget-text-face
	 :protection private)
   (spface :initarg :spface
	   :initform widget-text-button-face
	   :docstring "Face used on text buttons which appear to the
left and right of the editable text.  They indicate unseen text to the
left or right of the field."
	   :protection private)
   (focus-face :initarg :focus-face
	       :initform widget-text-focus-face
	       :protection private)
   (keymap :initarg :keymap
	   :initform nil
	   :docstring "Keymap used to interpret text.  By default, the
global map is used when this value is nil.  Otherwise, additional
mode-specific keymaps could be substituted to lend additional
behaviors.")
   (disppos :initform 0
	    :docstring "Current position in the text value to display info from")
   (keycache :initform nil
	     :docstring "Collecting keypresses for multi keystroke keys.")
   (value :initarg :value
	  :initform nil
	  :docstring "A `data-object' representing the string we are editing.")
   )
  "Class for a text field widget.  This will accept user-typed text
which is no more than 1 line high.  Extra text will not be printed,
but characters on either side of the field will display `<' or `>' to
indicate that there is more to see outside of the visible part.")

(run-hooks 'widget-d-load-hooks)

;;; end of lisp
(provide 'widget-d)
