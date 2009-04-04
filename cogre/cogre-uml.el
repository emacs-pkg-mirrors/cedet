;;; cogre-uml.el --- UML support for COGRE

;;; Copyright (C) 2001, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: oop, uml
;; X-RCS: $Id: cogre-uml.el,v 1.22 2009/04/04 15:32:44 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Provides UML support for COGRE.
;;
;; See http://c2.com/cgi/wiki?UmlAsciiArt for more examples of using
;; ASCII to draw UML diagrams.

(require 'cogre)

;;; Code:
;;;###autoload
(defclass cogre-package (cogre-node)
  ((name-default :initform "Package")
   (blank-lines-top :initform 1)
   (blank-lines-bottom :initform 1)
   (alignment :initform left)
   (subgraph :initarg :subgraph
	     :initform nil
	     :type (or null cogre-graph)
	     :documentation
	     "A graph which represents the classes within this package.
The subgraph should be scanned to extract all the elements drawn into
the package node.")
   )
  "A Package node.
Packages represent other class diagrams, and list the major nodes
within them.  They can be linked by dependency links.")

(defmethod cogre-node-rebuild-default ((node cogre-package))
  "Create the text rectangle for the COGRE package.
Calls the base method, and takes the return argument and
tweaks the faces."
  (let* ((rect (call-next-method))
	 (first (car rect))
	 (second (car (cdr rect))))
    ;; Tweak the first and second string iff it is long enough.
    (when (> (length first) 7)
      (remove-text-properties 5 (length first) '(face) first)
      (setcar rect first)
      (cogre-string-merge-faces 5 (length second)
				'cogre-box-first-face
				second)
      (setcar (cdr rect) second)
      )
    ;; Return it.
    rect))

;;;###autoload
(defclass cogre-note (cogre-node)
  ((name-default :initform "Note...")
   (blank-lines-top :initform 1)
   (blank-lines-bottom :initform 1)
   (alignment :initform left)
   )
  "An note node.
Notes are used to add annotations inside a graph.
Notes are generally linked to some node, and are supposed to look
like a little pieces of paper.")

(defmethod cogre-node-rebuild-default ((node cogre-note))
  "Create the text rectangle for the COGRE package.
Calls the base method, and takes the return argument and
tweaks the faces."
  (let* ((rect (call-next-method))
	 (first (car rect)))
    (remove-text-properties 0 1 '(face) first)
    (aset first 0 ?/)
    (setcar rect first)
    ;; Return it.
    rect))

;;;###autoload
(defclass cogre-scoped-node (cogre-node)
  ((package-name :initform ""
		 :initarg :package-name
		 :type string
		 :custom string
		 :documentation
		 "The package name of this node.
Package names are displayed in italic at the top of the node above the name
in UML, usuall like this:
   +---------------+
   | <<mypackage>> |
   | NameOfNode    |          
   | ...           |")
   (package-delimiters :allocation :class
		       :initform ( "<<" . ">>" )
		       :documentation
		       "Decoration delimiters for left/right side of package name.
It is a list of the form ( \"LEFTDELIM\" . \"RIGHTDELIM\").")
   )
  "A UML node that has a package specifier within which it is scoped."
  :abstract t)

(defmethod cogre-node-title ((node cogre-scoped-node))
  "Return the title of a scoped node.
If there is no package name, it is (\"name\").  If there
is a package, it is ( \"<package>\" \"name\")."
  (if (not (string= (oref node package-name) ""))
      (let* ((p (oref node package-name))
	     (delim (oref node package-delimiters))
	     (s (concat (car delim) p (cdr delim))))
	(cogre-string-merge-faces 1 (+ (length p) 1) 'italic s)
	(list s (oref node object-name)))
    (list (oref node object-name))))

;;;###autoload
(defclass cogre-class (cogre-scoped-node)
  ((name-default :initform "Class")
   (blank-lines-top :initform 0)
   (blank-lines-bottom :initform 0)
   (alignment :initform left)
   (class :initarg :class
	  :initform nil
	  :type (or string list)
	  :custom sexp
	  :documentation
	  "The semantic token representing the class this is drawing.")
   (attributes :initarg :attributes
	       :initform nil
	       :type list
	       :custom sexp
	       :documentation
	       "A list of attributes belonging to this Class representation.
Each attribute must in the form of a semantic token. ei.
 (\"object-name\" variable \"type\" ... )
See `semantic-fetch-tags' for details on possible token forms.
These items do not need to be REAL semantic tokens, however.
Only the format is needed to get the name/typing information.")
   (methods :initarg :methods
	    :initform nil
	    :type list
	    :custom sexp
	    :documentation
	    "A list of methods belonging to this Class representation.
See `attribute' slot for details on the form of each token in this list.")
   )
  "A Class node.
Class nodes represent a class, and can list the attributes and methods
within them.  Classes can have attribute links, and class hierarchy links.")

(defmethod cogre-uml-stoken->uml ((class cogre-class) stoken &optional text)
  "For CLASS convert a Semantic style token STOKEN into a uml definition.
It also adds properties that enable editing, and interaction with
this node.  Optional argument TEXT is a preformatted string."
  (if (semantic-tag-p stoken)
      (semantic-format-tag-uml-concise-prototype stoken)
    ;; Else, old style COGRE tag thing.
    (let ((newtext 
	   (or text
	       (concat (car stoken) ":"
		       (cond ((stringp (nth 2 stoken))
			      (nth 2 stoken))
			     ((listp (nth 2 stoken))
			      (car (nth 2 stoken)))
			     (t ""))))))
      ;; Add in some useful properties
      (add-text-properties 0 (length newtext)
			   (list 'semantic stoken
			       
				 )
			   newtext)
      ;; Return the string
      newtext)))

(defmethod cogre-node-slots ((class cogre-class))
  "Return a list of each section, including title, attributes, and methods.
Argument CLASS is the class whose slots are referenced."
  (list
   (mapcar (lambda (s) (cogre-uml-stoken->uml class s)) (oref class attributes))
   (mapcar (lambda (s) (cogre-uml-stoken->uml class s)) (oref class methods))
   ))

;;;###autoload
(defclass cogre-instance (cogre-scoped-node)
  ((name-default :initform "Instance")
   (blank-lines-top :initform 1)
   (blank-lines-bottom :initform 1)
   (alignment :initform left)
   )
  "An instance node.
Instances are used in instance diagrams.
Instances are linked together with plain links.")

(defmethod cogre-node-title ((node cogre-instance))
  "Return a list of strings representing the title of the NODE.
For example: ( \"Title\" ) or ( \"<Type>\" \"Title\" )"
  (let* ((prev (call-next-method))
	 (name (concat ":" (oref node object-name))))
    (cogre-string-merge-faces 0 (length name) 'underline name)
    (if (= (length prev) 1)
	;; It's just us.
	(list name)
      ;; Else, we probably have a package name.
      (setcar (cdr prev) name)
      prev)))

;;; Links
;;
;;;###autoload
(defclass cogre-inherit (cogre-link)
  ((end-glyph :initform [ (" ^ " "/_\\")
			  ("_|_" "\\ /" " V ")
			  (" /|" "< |" " \\|")
			  ("|\\ " "| >" "|/ ") ])
   (horizontal-preference-ratio :initform .0001)
   )
  "This type of link indicates that the two nodes reference infer inheritance.
The `start' node is the child, and the `end' node is the parent.
This is supposed to infer that START inherits from END.")

;;;###autoload
(defclass cogre-aggregate (cogre-link)
  ((start-glyph :initform [ ("/\\ " "\\/" )
			    ("/\\ " "\\/" )
			    ("<>") ("<>") ])
   (horizontal-preference-ratio :initform 1)
   )
  "This type of link indicates aggregation.
The `start' node is the owner of the aggregation, the `end' node is
the item being aggregated.
This is supposed to infer that START contains END.")

;;;###autoload
(defun cogre-uml-enable-unicode ()
  "Enable use of UNICODE symbols to create COGRE graphs.
Inheritance uses math triangle on page 25a0.
Aggregation uses math square on edge 25a0.
Line-drawing uses line-drawing codes on page 2500.
See http://unicode.org/charts/symbols.html.

The unicode symbols can be differing widths.  This will make the
cogre chart a little screwy somteims.  Your mileage may vary."
  (interactive)
  (oset-default cogre-inherit end-glyph
		[ ("\u25b3") ("\u25bd") ("\u25c1") ("\u25b7") ])
  (oset-default cogre-aggregate start-glyph
		[ ("\u2b25") ("\u2b25") ("\u25c6") ("\u25c6") ] )
  ;; Nice idea, but too small.  Oh well.  Maybe someone else
  ;; can design something better.
  (oset-default cogre-arrow end-glyph
  		[ ("\u2191") ("\u2193") ("\u2190") ("\u2192") ] )

  ;; Set the special single-char << and >> thingies.
  (let ((delim '( "\u226a" . "\u226b" )))
    (oset-default cogre-scoped-node package-delimiters delim)
    ;; Note: I should use some sort of eieio looping to do this change.
    (oset-default cogre-class package-delimiters delim)
    (oset-default cogre-instance package-delimiters delim))

  ;; "\u25c7" - open box like "\u25c6"
  (setq picture-rectangle-v ?\u2502)
  (setq picture-rectangle-h ?\u2500)
  (setq picture-rectangle-ctl ?\u250C)
  (setq picture-rectangle-ctr ?\u2510)
  (setq picture-rectangle-cbl ?\u2514)
  (setq picture-rectangle-cbr ?\u2518)
  )

(provide 'cogre-uml)

;;; cogre-uml.el ends here
