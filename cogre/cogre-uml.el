;;; cogre-uml.el --- UML support for COGRE

;;; Copyright (C) 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: oop, uml
;; X-RCS: $Id: cogre-uml.el,v 1.2 2001/04/25 02:46:21 zappo Exp $

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
;; Provides UML support for COGRE.

(require 'cogre)
(require 'semantic)
(require 'semanticdb)

;;; Code

(defclass cogre-package (cogre-node)
  ((name :initform "Package")
   (blank-lines-top :initform 0)
   (blank-lines-bottom :initform 0)
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

(defmethod cogre-node-slots ((package cogre-package))
  "Return a list containing the list of classes in PACKAGE.
The `subgraph' slot must be scanned for this information."
  (list nil)
  )

(defclass cogre-class (cogre-node)
  ((name :initform "Class")
   (blank-lines-top :initform 0)
   (blank-lines-bottom :initform 0)
   (attributes :initarg :attributes
	       :initform nil
	       :type list
	       :documentation
	       "A list of attributes belonging to this Class representation.
Each attribute must in the form of a semantic token. ei.
 (\"name\" variable \"type\" ... )
See `semantic-toplevel-bovine-table' for details on possible token forms.")
   (methods :initarg :methods
	       :initform nil
	       :type list
	       :documentation
	       "A list of methods belonging to this Class representation.
See `attribute' slot for details on the form of each token in this list.")
   )
  "A Class node.
Class nodes represent a class, and can list the attributes and methods
within them.  Classes can have attribute links, and class hierarchy links.")

(defmethod initialize-instance ((this cogre-class) &optional fields)
  "When interactively creating a class node THIS, query for the class name.
Optional argument FIELDS are not used."
  (call-next-method)
  (if (string-match "^Class[0-9]*" (oref this name))
      ;; In this case, we have a default class name, so try and query
      ;; for the real class (from sources) which we want to use.
      (let* ((class (cogre-read-class-name))
	     (tok (cdr (car (semanticdb-find-nonterminal-by-name
			     class nil nil nil t))))
	     )
	(if (and tok (eq (semantic-token-token tok) 'type)
		 (string= (semantic-token-type tok) "class"))
	    (let ((slots (semantic-token-type-parts tok))
		  (extmeth (semanticdb-find-nonterminal-by-extra-spec-value
			    'parent (semantic-token-name tok) nil nil nil t))
		  attrib method)
	      ;; Bin them up
	      (while slots
		(cond 
		 ;; A plain string, a simple language, just do attributes.
		 ((stringp (car slots))
		  (setq attrib (cons (list (car slots) 'variable nil)
				     attrib))
		  )
		 ;; Variable decl is an attribute
		 ((eq (semantic-token-token (car slots)) 'variable)
		  (setq attrib (cons (car slots) attrib)))
		 ;; A function decle is a method.
		 ((eq (semantic-token-token (car slots)) 'function)
		  (setq method (cons (car slots) method)))
		 )
		(setq slots (cdr slots)))
	      ;; Add in all those extra methods
	      (while extmeth
		(let ((sl (cdr (car extmeth))))
		  (while sl
		    (if (eq (semantic-token-token (car sl)) 'function)
			(setq method (cons (car sl) method)))
		    (setq sl (cdr sl))))
		(setq extmeth (cdr extmeth)))
	      ;; Put them into the class.
	      (oset this name class)
	      (oset this attributes attrib)
	      (oset this methods method)
	      ;; Tada!
	      ))))
  this)

(defmethod cogre-node-slots ((class cogre-class))
  "Return a list of each section, including title, attributes, and methods.
Argument CLASS is the class whose slots are referenced."
  (list
   (mapcar 'semantic-token-name (oref class attributes))
   (mapcar 'semantic-token-name (oref class methods))
   ))


(defclass cogre-inherit (cogre-link)
  ((end-glyph :initform [ (" ^ " "/_\\")
			  ("_|_" "\\ /" " V ")
			  ("<|")
			  ("|>") ])
   (horizontal-preference-ratio :initform .1)
   )
  "This type of link indicates that the two nodes reference infer inheritance.
The `start' node is the child, and the `end' node is the parent.
This is supposed to infer that START inherits from END.")

(defclass cogre-aggrigate (cogre-link)
  ((start-glyph :initform [ (" ^ " "< >" " V ")
			    (" ^ " "< >" " V ")
			    ("<>") ("<>") ])
   (horizontal-preference-ratio :initform 1)
   )
  "This type of link indicates aggregation.
The `start' node is the owner of the aggregation, the `end' node is
the item being aggregated.
This is supposed to infer that START contains END.")

;;; Auto-Graph generation
;;
;; Functions for creating a graph from semantic parts.
(defvar cogre-class-history nil
  "History for inputting class names.")

(defun cogre-read-class-name ()
  "Read in a class name to be used by a cogre node."
  (let ((finddefaultlist (semantic-find-nonterminal-by-overlay))
	class prompt stream
	)
    ;; Assume the top most item is the all encompassing class.
    (if finddefaultlist
	(setq class (car finddefaultlist)))
    ;; Make sure our class is really a class
    (if (not (and
	      class
	      (eq (semantic-token-token class) 'type)
	      (string= (semantic-token-type class) "class")))
	(setq class nil))
    ;; Create a prompt
    (setq prompt (if class (concat "Class (default " class "): ") "Class: "))
    ;; Get the stream used for completion.
    (setq stream
	  (apply #'append
		 (mapcar #'cdr
			 (semanticdb-find-nonterminal-by-type
			  "class" nil nil nil t))))
    ;; Do the query
    (completing-read prompt stream
		     nil nil nil 'cogre-class-history
		     class)
    ))

;;;###autoload
(defun cogre-uml-create (class)
  "Create a new UML diagram, with CLASS as the root node.
CLASS must be a type in the current project."
  (interactive (list (cogre-read-class-name)))
  (let ((root (cdr (car (semanticdb-find-nonterminal-by-name class))))
	)
    
    ))


