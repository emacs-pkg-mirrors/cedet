;;; cogre.el --- COnnected GRaph Editor for Emacs

;;; Copyright (C) 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: cogre.el,v 1.1 2001/04/14 05:39:17 zappo Exp $

;;; Code:
(defvar cogre-version "0.0"
  "Current version of Cogre.")

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Many types of code can be displayed as a series of connected
;; graphs, such as UML class or sequence diagrams.  COGRE attempts to
;; allow Emacs to display such graphs with data generated from
;; source code.
;;
;; COGRE is based on a generic editor that can handle arbitrary
;; graphs, with different specific graph types derived from that.
;; This depends on EIEIO for graph management.  COGRE depends on
;; `picture-mode' for drawing.  It also depends on semantic for
;; source-code parsing.
;;
;; Because COGRE graphs are meant to be edited in some fashion, COGRE
;; graphs depend on the custom widget library to provide text
;; controls, or toggle buttons for editing state in a graph.

(require 'eieio)
(require 'eieio-opt)
(require 'semantic)
(require 'picture)

;;; Display Faces
(defgroup cogre nil
  "COnnected GRaph Editor."
  )

(defface cogre-box-face  '((((class color) (background dark))
			    (:background "gray30" :foreground "white"))
			   (((class color) (background light))
			    (:background "gray" :foreground "black")))
  "Face used for rectangles of boxes displaying data."
  :group 'cogre)

(defface cogre-box-first-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :overline t))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :overline t)))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

(defface cogre-box-last-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :underline t))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :underline t)))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

;;; Classes
(defclass cogre-graph (eieio-persistent)
  ((name :initarg :name
	 :initform "NewGraph"
	 :type string
	 :custom string
	 :documentation
	 "The name of this graph.
The save file name is based on this name.")
   (buffer :initarg :buffer
	   :initform nil
	   :type (or null buffer)
	   :documentation
	   "When this graph is active, this is the buffer the graph is
displayed in.")
   (elements :initarg :elements
	     :initform nil
	     :type list
	     :documentation
	     "The list of elements in this graph.")
   )
  "A Connected Graph.
a connected graph contains a series of nodes and links which are
rendered in a buffer, or serialized to disk.")

(defclass cogre-graph-element ()
  ((dirty :initform t
	  :documentation
	  "Non-nil if this graph element is dirty.
Elements are made dirty when they are erased from the screen.
Elements must be erased before any graphical fields are changed.")
   (name :initarg :name
	 :initform "Name"
	 :type string
	 :custom string
	 :documentation
	 "The name of this node.
Node names must be unique within the current graph so that save
references in links can be restored."))
  "A Graph Element.
Graph elements are anything that is drawn into a `cogre-graph'.
Graph elements have a method for marking themselves dirty.")

(defclass cogre-node (cogre-graph-element)
  ((position :initarg :position
	     :initform [ 0 0 ]
	     :type vector
	     :documentation
	     "The X,Y (COL, ROW) position as a vector for this node.
The Width/Height if this node is determined by RECTANGLE, which is
a list of strings representing the body of the node."
	     )
   (blank-lines-top :allocation class
		    :initform 1
		    :documentation
		    "Number of blank lines above the name.")
   (blank-lines-bottom :allocation class
		       :initform 1
		       :documentation
		       "Number of blank lines below the last line of text.")
   (rectangle :initarg :rectangle
	      :initform nil
	      :type list
	      :documentation
	      "A List of strings representing an Emacs rectangle.
This rectangle is used for inserting and moving the block of
characters that represent this node in a buffer.
The rectangle is NOT SAVED.
Other fields in the node are used to build a new RECTANGLE of strings
at load time.")
   )
  "Connected Graph node.
Nodes are regions with a fill color, and some amount of text representing
a status, or values.")

(defclass cogre-link (cogre-graph-element)
  ((start :initarg :start
	  :initform nil
	  :type (or null string cogre-node)
	  :documentation "The starting node.
As a string, the name of the node we start on.
As an object, the node we start on.")
   (end :initarg :end
	  :initform nil
	  :type (or null string cogre-node)
	  :documentation "The ending node.
As a string, the name of the node we end on.
As an object, the node we end on.")
   (start-glyph :initarg :start-glyph
		:initform nil
		:allocation class
		:type (or null string vector)
		:documentation "The starting glyph.
A Glyph can be NULL, meaning nothing, a list, a string, or a vector.
A list must represent a rectangle of text.
Tbe string means a one-line block of text.
For these single items, the same glyph is used for all four directions.
A Vector must be 4 elements long.  This represents glyphs on
the [ TOP BOTTOM LEFT RIGHT ] of the attached node.
Each element of the vector can be a list or string.")
   (end-glyph :initarg :end-glyph
	      :initform nil
	      :allocation class
	      :type (or null string)
	      :documentation "The ending glyph.
See slot `start-glyph'")
   )
  "Connected Graph link.
Links are lines drawn between two nodes, or possibly loose in space
as an intermediate step.  Some links have text describing what they
do, and most links have special markers on one end or another, such as
arrows or circles.")

;;; Connecte Graph variables
;;
(defcustom cogre-mode-hooks nil
  "Hooks run in `cogre-mode'."
  :group 'cogre
  :type 'hook)

(defvar cogre-graph nil
  "The current connected graph.")
(make-variable-buffer-local 'cogre-graph)

(defun cogre-substitute (oldfun newfun)
  "Substitue a key binding in ghe `cogre-mode-map'."
  (substitute-key-definition oldfun newfun cogre-mode-map global-map))

(defvar cogre-mode-map nil
  "Keymap used for COGRE mode.")

(if cogre-mode-map
    nil
  (setq cogre-mode-map (make-keymap))
  (define-key cogre-mode-map "R" 'cogre-refresh)
  (define-key cogre-mode-map "N" 'cogre-new-node)
  (define-key cogre-mode-map "L" 'cogre-new-link)
  (define-key cogre-mode-map "n" 'cogre-set-element-name)
  (define-key cogre-mode-map "l" 'cogre-edit-label)
  ;; Move nodes around
  (define-key cogre-mode-map [(meta left)] 'cogre-move-node-left)
  (define-key cogre-mode-map [(meta right)] 'cogre-move-node-right)
  (define-key cogre-mode-map [(meta down)] 'cogre-move-node-down)
  (define-key cogre-mode-map [(meta up)] 'cogre-move-node-up)
  (define-key cogre-mode-map "\M-b" 'cogre-move-node-left)
  (define-key cogre-mode-map "\M-f" 'cogre-move-node-right)
  (define-key cogre-mode-map "\M-n" 'cogre-move-node-down)
  (define-key cogre-mode-map "\M-p" 'cogre-move-node-up)
  ;; Cursor Movement
  (define-key cogre-mode-map "\C-i" 'cogre-next-node)
  (define-key cogre-mode-map "\M-\C-i" 'cogre-prev-node)
  (cogre-substitute 'forward-char  'picture-forward-column)
  (cogre-substitute 'backward-char 'picture-backward-column)
  (cogre-substitute 'next-line     'picture-move-down)
  (cogre-substitute 'previous-line 'picture-move-up)
  )

;;; Buffer initialization
;;
(defun cogre (name)
  "Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details."
  (interactive "sGraph Name: ")
  (let ((newgraph (cogre-graph name :name name)))
    (switch-to-buffer (get-buffer-create (concat "*Graph " name "*")))
    (setq cogre-graph newgraph)
    ;;(toggle-read-only 1)
    (cogre-mode)
    ))

(defun cogre-mode ()
  "Connected Graph Editor Mode."
  (interactive)
  (setq major-mode 'cogre-mode
	mode-name "Cogre")
  (use-local-map cogre-mode-map)
  (run-hooks 'cogre-mode-hook)
  (cogre-render-buffer cogre-graph t)
  )

;;; Interactive utility functions
;;
(defun cogre-node-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (or (not e) (not (obj-of-class-p e cogre-node)))
	(error "No graph node under point")
      e)))

;;; Commands for Graph Mode
;;
(defun cogre-refresh ()
  "Refresh the current display completely."
  (interactive)
  (cogre-render-buffer cogre-graph t))

(defvar cogre-node-history nil
  "The history for reading in node class names.")

(defun cogre-new-node (point nodetype)
  "Insert a new node at the current point.
Argument POINT is a position to insert this node to.
NODETYPE is the eieio class name for the node to insert."
  (interactive (list (point)
		     (eieio-read-subclass "Node Type: "
					  cogre-node
					  'cogre-node-history)))
  (save-excursion
    (goto-char point)
    (if (not nodetype) (setq nodetype 'cogre-node))
    (let* ((x (current-column))
	   (y (cogre-current-line))
	   (n (make-instance nodetype "Node" :position (vector x y)))
	   )
      (cogre-render-buffer cogre-graph)
      )))

(defvar cogre-link-history nil
  "The history for reading in link class names.")

(defun cogre-new-link (mark point linktype)
  "Insert a new link from the node at MARK to POINT of LINKTYPE.
MARK is the node within which the current mark is set.
POINT is the node the cursor is in.
LINKTYPE is the eieio class name for the link to insert."
  (interactive (list (cogre-node-at-point-interactive (mark))
		     (cogre-node-at-point-interactive (point))
		     (eieio-read-subclass "Link Type: "
					  cogre-link
					  'cogre-link-history)))
  (if (not linktype) (setq linktype cogre-link))
  (make-instance linktype "Link" :start mark :end point)
  (cogre-render-buffer cogre-graph)
  )


(defun cogre-set-element-name (node name)
  "Set the name of the current NODE to NAME."
  (interactive (let ((e (cogre-node-at-point-interactive)))
		 (list e  (read-string "New Name: " ""
				       nil (oref e name)))))
  (cogre-erase node)
  (oset node name (cogre-unique-name cogre-graph name))
  (if (interactive-p)
      (cogre-render-buffer cogre-graph))
  )

(defun cogre-next-node (&optional arg)
  "Move forward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (let ((n (cogre-current-element (point)))
	(e (oref cogre-graph elements))
	(next nil))
    (if (not n)
	;; Not on the node?  Tab around.
	(setq next (car e))
      (let* ((l (length e))
	     (i (- l (length (member n e))))
	     (ni (+ i arg)))
	(if (< ni 0) (setq ni (+ l ni))
	  (if (>= ni l) (setq ni (- ni l))))
	(setq next (nth ni e))))
    (let ((p (oref next position)))
      (cogre-goto-coordinate (aref p 0) (aref p 1)))))

(defun cogre-prev-node (&optional arg)
  "Move backward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (cogre-next-node (- arg)))

(defun cogre-move-node (x y)
  "Set NODE to postion X, Y."
  (interactive "nX: \nnY: ")
  (let ((e (cogre-current-element (point))))
    (cogre-erase e)
    (if (> 0 x) (setq x 0))
    (if (> 0 y) (setq y 0))
    (oset e position (vector x y))
    (cogre-goto-coordinate x y)
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-left (arg)
  "Move NODE left by ARG columns."
  (interactive "p")
  (let* ((e (cogre-current-element (point)))
	 (p (oref e position)))
    (cogre-move-node (- (aref p 0) arg) (aref p 1))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-right (arg)
  "Move NODE right by ARG columns."
  (interactive "p")
  (let* ((e (cogre-current-element (point)))
	 (p (oref e position)))
    (cogre-move-node (+ (aref p 0) arg) (aref p 1))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-up (arg)
  "Move NODE up by ARG columns."
  (interactive "p")
  (let* ((e (cogre-current-element (point)))
	 (p (oref e position)))
    (cogre-move-node (aref p 0) (- (aref p 1) arg))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-down (arg)
  "Move NODE down by ARG columns."
  (interactive "p")
  (let* ((e (cogre-current-element (point)))
	 (p (oref e position)))
    (cogre-move-node (aref p 0) (+ (aref p 1) arg))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))


;;; State Management
;;
(defmethod cogre-add-element ((graph cogre-graph) elt)
  "Add to GRAPH a new element ELT."
  (object-add-to-list graph 'elements elt t))

(defmethod cogre-unique-name ((graph cogre-graph) name)
  "Within GRAPH, make NAME unique."
  (let ((newname name)
	(obj (object-assoc name :name (oref graph elements)))
	(inc 1))
    (while obj
      (setq newname (concat name (int-to-string inc)))
      (setq inc (1+ inc))
      (setq obj (object-assoc newname :name (oref graph elements))))
    newname))

(defmethod cogre-set-dirty ((element cogre-graph-element) dirty-state)
  "Set the dirty state for ELEMENT to DIRTY-STATE."
  (oset element dirty dirty-state))

(defmethod cogre-set-dirty ((node cogre-node) dirty-state)
  "Set the dirty state for NODE to DIRTY-STATE."
  (if dirty-state (oset node rectangle nil))
  (call-next-method))

(defmethod initialize-instance :AFTER ((elt cogre-graph-element) fields)
  "When creating a new element, add it to the current graph.
Argument ELT is the element being created.
Argument FIELDS are ignored."
  (let ((n (oref elt name)))
    ;; make sure our name is unique.
    (oset elt name (cogre-unique-name cogre-graph n)))
  (cogre-add-element cogre-graph elt))

;;; Buffer Rendering
;;
(defmethod cogre-render-buffer ((graph cogre-graph) &optional erase)
  "Render the current graph GRAPH.
If optional argument ERASE is non-nil, then erase the buffer,
and render everything.  If ERASE is nil, then only redraw items
with dirty flags set."
  (let ((inhibit-read-only t)
	(x (current-column))
	(y (1- (picture-current-line))))
    (save-excursion
      (with-slots (elements) graph
	(if erase
	    (progn
	      (erase-buffer)
	      (mapcar (lambda (e) (cogre-set-dirty e t)) elements)))
	(mapcar (lambda (e) (cogre-render e)) elements)))
    (cogre-goto-coordinate x y)))

(defmethod cogre-render ((element cogre-graph-element))
  "Render ELEMENT.
By default, an ELEMENT has nothing to see, but assume we
are called from `call-next-method', so reset our dirty flag."
  (cogre-set-dirty element nil))

(defmethod cogre-erase ((element cogre-graph-element))
  "Erase ELEMENT.
By default, an ELEMENT has nothing to erase, but assume we
are called from `call-next-method', so set our dirty flag."
  (cogre-set-dirty element t))

;;; Nodes
(defmethod cogre-erase ((node cogre-node))
  "Erase NODE from the screen."
  (let ((position (oref node position))
	(rectangle (cogre-node-rectangle node))
	(links (cogre-node-links node)))
    (cogre-erase-rectangle (aref position 0) (aref position 1)
			   (length (car rectangle))
			   (length rectangle))
    (mapcar 'cogre-erase links))
  (call-next-method))

(defmethod cogre-node-links ((node cogre-node))
  "Return a list of links which reference NODE."
  (with-slots (elements) cogre-graph
    (let ((links nil))
      (mapcar (lambda (n) (if (and (obj-of-class-p n cogre-link)
				   (or (eq (oref n start) node)
				       (eq (oref n end) node)))
			      (setq links (cons n links))))
	      elements)
      links)))

(defmethod cogre-node-rectangle  ((node cogre-node))
  "Fetch the rectangle representation for NODE."
  (or (oref node rectangle)
      (cogre-node-rebuild node)))

(defmethod cogre-render ((node cogre-node))
  "Render NODE in the current graph."
  (cogre-node-rectangle node)
  (with-slots (position rectangle) node
    (cogre-goto-coordinate (aref position 0) (aref position 1))
    (picture-insert-rectangle rectangle nil))
  (call-next-method))

(defmethod cogre-node-rebuild ((node cogre-node))
  "Create a new value for `:rectangle' in NODE.
The `:rectangle' slot is inserted with rectangle commands.
A Rectangle is basically a list of equal length strings.
Those strings must have the proper face values on them.
Always make the width 2 greater than the widest string."
  (let* ((width (+ (cogre-node-widest-string node) 2))
	 (top-lines (oref node blank-lines-top))
	 (bottom-lines (oref node blank-lines-bottom))
	 (title (cogre-node-title node))
	 (slots (cogre-node-slots node))
	 (first t)
	 (rect nil))
    (while (> top-lines 0)
      (setq rect (cons (cogre-string-with-face
			""
			(if first
			    (progn (setq first nil)
				   'cogre-box-first-face)
			  'cogre-box-face)
			node width)
		       rect)
	    top-lines (1- top-lines)))
    (setq title (nreverse title))
    (while title
      (setq rect (cons (cogre-string-with-face
			(car title)
			(if first
			    (progn (setq first nil)
				   'cogre-box-first-face)
			  'cogre-box-face)
			node width)
		       rect)
	    title (cdr title)))
    (while slots
      (let ((sl (car slots)))
	(setq first t)
	(while sl
	  (setq rect (cons (cogre-string-with-face
			    (car sl)
			    (if first
				(progn (setq first nil)
				       'cogre-box-first-face)
			      (if (and (= bottom-lines 0)
				       (= (length slots) 1)
				       (= (length sl) 1))
				  'cogre-box-first-face
				'cogre-box-face))
			    node width)
			   rect)
		sl (cdr sl))))
      (setq slots (cdr slots)))
    (while (> bottom-lines 0)
      (setq rect (cons (cogre-string-with-face
			""
			(if (= bottom-lines 1)
			    'cogre-box-last-face
			  'cogre-box-face)
			node width)
		       rect)
	    bottom-lines (1- bottom-lines)))
    (oset node rectangle (nreverse rect))))

(defmethod cogre-node-title ((node cogre-node))
  "Return a list of strings representing the title of the NODE.
For example: ( \"Title\" ) or ( \"<Type>\" \"Title\" )"
  (list (oref node name)))

(defmethod cogre-node-slots ((node cogre-node))
  "For NODE, return a list of slot lists.
Slots are individual lines of text appearing in the body of a node.
Each list will be prefixed with a line before it."
  nil)

(defmethod cogre-node-widest-string ((node cogre-node))
  "Return the widest string in NODE."
  (length (oref node name)))

;;; Links
;;
(defmethod cogre-erase ((link cogre-link))
  "Erase LINK from the screen."
  (let ((picture-rectangle-ctl ? )
	(picture-rectangle-ctr ? )
	(picture-rectangle-cbl ? )
	(picture-rectangle-cbr ? )
	(picture-rectangle-v ? )
	(picture-rectangle-h ? ))
    ;; Links use picture line drawing teqnique to wander about.
    ;; By setting the picture line characters to spaces, we can
    ;; erase the line with the render command.
    (cogre-render link)
    (call-next-method)))

(defmethod cogre-render ((link cogre-link))
  "Render LINK in the current graph."
  (with-slots (start end start-glyph end-glyph) link
    (let* ((p1 (oref start position))	;position vector
	   (p2 (oref end position))
	   (x1 (aref p1 0))		;X,Y for START
	   (y1 (aref p1 1))
	   (x2 (aref p2 0))		;X,Y for END
	   (y2 (aref p2 1))
	   (r1 (cogre-node-rectangle start)) ;rectangle text
	   (r2 (cogre-node-rectangle end))
	   (w1 (length (car r1)))	;Width
	   (w2 (length (car r2)))
	   (h1 (length r1))		;Height
	   (h2 (length r2))
	   xstart xend ystart yend
	   )
      ;; Calculate starting points in relation to our attached nodes.
      (if (> (abs (- x1 x2)) (abs (- y1 y2)))
	  ;; In this case, the X delta is larger than the Y delta,
	  ;; so the line is going mostly left/right.
	  (progn
	    (if (< x1 x2)
		(setq xstart (+ x1 w1)
		      xend x2)
	      (setq xstart x1
		    xend (+ x2 w2))
	      )
	    (setq ystart (+ y1 (/ h1 2))
		  yend (+ y2 (/ h2 2)))
	    )
	;; In this case, the Y delta is larger than the X delta,
	;; so the line is going mostly up/down.
	(if (< y1 y2)
	    (setq ystart (+ y1 h1)
		  yend y2)
	  (setq ystart y1
		yend (+ y2 h2))
	  )
	(setq xstart (+ x1 (/ w1 2))
	      xend (+ x2 (/ w2 2)))
	)
      ;; Now draw a rectiliniar line
      (cogre-draw-rectilinear-line xstart ystart xend yend)
      ;; Handle start/end glyps.
      ;; foo
      ))
  (call-next-method))

;;; Low Level Rendering and status
;;
(defun cogre-goto-coordinate (x y)
  "Goto coordinate X, Y."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (picture-newline y)
    (move-to-column x t)
    ))

(defun cogre-picture-set-motion (vert horiz)
  "Set VERT and HORIZ increments for movement in Picture mode.
Hack from picture mode."
  (setq picture-vertical-step vert
	picture-horizontal-step horiz))

(defun cogre-draw-rectilinear-line (x1 y1 x2 y2)
  "Draw a line from X1, Y1 to X2, Y2.
The line is drawn in a rectilinear fashion."
  ;; A rectilinear line for us (short term) is a line travelling
  ;; in the direction of greatest distance, with a jog in the middle.
  (let (xdir ydir halfway htwiddle
	)
    ;; Travelling
    (if (> x1 x2)
	(setq xdir -1)
      (setq xdir 1))
    (if (> y1 y2)
	(setq ydir -1)
      (setq ydir 1))
    ;; Get there
    (cogre-goto-coordinate x1 y1)
    (picture-update-desired-column t)
    ;; Determine primary direction
    (if (> (abs (- x1 x2)) (abs (- y1 y2)))
	;; This means that X is primary direction
	(progn
	  (setq halfway (/ (abs (- x1 x2)) 2)
		htwiddle (% (abs (- x1 x2)) 2))
	  (cogre-picture-set-motion 0 xdir)
	  (picture-insert picture-rectangle-h (+ halfway htwiddle))
	  (if (/= y1 y2)
	      (progn
		(cogre-picture-set-motion ydir 0)
		(picture-insert picture-rectangle-ctl 1)
		(picture-insert picture-rectangle-v (1- (abs (- y1 y2))))
		(cogre-picture-set-motion 0 xdir)
		(picture-insert picture-rectangle-ctl 1)
		(setq halfway (1- halfway))
		))
	  (picture-insert picture-rectangle-h halfway)
	  )
      ;; This means that Y is the primary direction
      (setq halfway (/ (abs (- y1 y2)) 2)
	    htwiddle (% (abs (- y1 y2)) 2))
      (cogre-picture-set-motion ydir 0)
      (picture-insert picture-rectangle-v (+ halfway htwiddle))
      (if (/= y1 y2)
	  (progn
	    (cogre-picture-set-motion 0 xdir)
	    (picture-insert picture-rectangle-ctl 1)
	    (picture-insert picture-rectangle-h (1- (abs (- x1 x2))))
	    (cogre-picture-set-motion ydir 0)
	    (picture-insert picture-rectangle-ctl 1)
	    (setq halfway (1- halfway))
	    ))
      (picture-insert picture-rectangle-v halfway)
      )
    ))

(defun cogre-string-with-face (string face element &optional length)
  "Using text STRING, apply FACE to that text.
The string in question belongs to the graph ELEMENT.
If optional argument LENGTH is supplied, pad STRING on the left and
right so that it is centered.  Return the new string."
  (if length
      (let* ((ws (- length (length string)))
	     (sws (make-string (/ ws 2) ? ))
	     (ews (make-string (+ (/ ws 2) (% ws 2)) ? )))
      (setq string (concat sws string ews))))
  (add-text-properties 0 (length string) (list 'face face
					       'rear-nonsticky t
					       'detachable t ;; xemacs
					       'element element
					       )
		       string)
  string)

(defun cogre-erase-rectangle (x y width height)
  "Clear out the rectangle at X Y, with dimensions WIDTH HEIGHT."
  (cogre-goto-coordinate x y)
  (clear-rectangle (point)
		   (save-excursion
		     (cogre-goto-coordinate (+ x width)
					    (+ y height))
		     (point))
		   t))

(defun cogre-current-element (point)
  "Return the element under POINT."
  (get-text-property point 'element))

(defun cogre-current-line ()
  "Get the current line."
  (cond ((eq (point-min) (point))
	 0)
	(t (1- (count-lines (point-min) (point))))))

(provide 'cogre)

;;; cogre.el ends here
