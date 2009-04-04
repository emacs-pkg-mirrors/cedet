;;; cogre-convert.el --- Conversion for cogre charts into other formats
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cogre-convert.el,v 1.2 2009/04/04 15:35:16 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Conversions for COGRE charts.
;;
;; A cogre chart is a collection of nodes and lines.  Some charts,
;; such as UML class diagrams, actually represent other kinds of structure.
;;
;; The COGRE-CONVERT utilities is a framework for transforming a graph
;; into some other LISP structure.  Some conversions, such as to ASCII
;; override drawing code.  Conversions transform the data instead.
;;
;; @TODO - re-write below as things are implemented
;; Ideas for conversions:
;;   graph -> semantic tags
;;   graph -> dot tags
;;   graph -> xml for dia, or other case tool
;;
;; Ideas for reverse conversion?
;;   semantic tags   -> graph
;;   xml fir dia     -> graph
;;   parsed dot file -> graph
;;
;;
;; Implementation strategy
;;  My thoughts on getting to the above.
;;  1) Write a straight-up converter to dot.
;;  2) Guess what an abstract converter looks like based on dot
;;  3) Promote the generic API, and write semantic-tag converter.

(require 'cogre-srecode)

;;; Code:
;;;###autoload
(defun cogre-export-dot ()
  "Export the current COGRE graph to DOT notation.
DOT is a part of GraphViz."
  (interactive)
  (when (not (eieio-object-p cogre-graph)) (error "No graph to export"))

  (let* ((g cogre-graph)
	 (name (oref g name))
	 (fname (concat name ".dot"))
	 (ede-auto-add-method 'never)
	 )
    ;; Force graphviz mode to be loaded, just in case the user didn't.
    (condition-case nil
	(inversion-require 'graphviz-dot-mode "0.3.2")
      (error
       (error "You need to install graphviz-dot-mode.el to use dot-export feature")))

    ;; Load in the file
    (switch-to-buffer (find-file fname))
    (erase-buffer)
    
    ;; Convert G into this buffer.
    (let* ((graphtag (cogre-export-dot-method g))
	   (members (semantic-tag-get-attribute graphtag :members))
	   (cogre-srecode-current-graph g)
	   )

      ;; Start it out.
      (srecode-insert "file:cogre")

      ;; Insert all the tags.
      (srecode-semantic-insert-tag graphtag)

      (dolist (M members)
	(srecode-semantic-insert-tag M))

      (save-buffer)
      (message "Converted graph into %d dot nodes."
	       (length (semantic-tag-get-attribute graphtag :members)))
      )))

(defmethod cogre-export-dot-method ((g cogre-graph))
  "Convert G into DOT syntax of semantic tags."
  (semantic-tag (oref g :name)
		'digraph
		:members 
		(cogre-map-elements 'cogre-export-dot-method g)
		)
  )

(defun cogre-tag-put-dot-attribute (tag attribute value)
  "Get the attributes in TAG, and set ATTRIBUTE to VALUE.
This works similarly to `semantic-tag-put-attribute'."
  (let* ((plist (semantic-tag-get-attribute tag :attributes)))
    (semantic-tag-put-attribute tag :attributes
				(plist-put plist attribute value))
    tag))

;;; NODES
(defmethod cogre-export-dot-method ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  (semantic-tag (oref node :object-name)
		'node
		:attributes
		(list
		 :shape (cogre-export-dot-shape node)
		 :label (cogre-export-dot-label node)
		 )
		)
  ;; For dot language,
  ;; attr 'pos' is position in points
  )

(defmethod cogre-export-dot-shape ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  "box")

(defmethod cogre-export-dot-shape ((node cogre-class))
  "Convert NODE into DOT syntax of semantic tags."
  "record")

(defmethod cogre-export-dot-shape ((node cogre-package))
  "Convert NODE into DOT syntax of semantic tags."
  "tab")

(defmethod cogre-export-dot-shape ((node cogre-note))
  "Convert NODE into DOT syntax of semantic tags."
  "note")

(defmethod cogre-export-dot-label ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  (mapconcat 'identity (cogre-node-title node) "\\n"))

(defmethod cogre-export-dot-label ((node cogre-scoped-node))
  "Convert NODE into DOT syntax of semantic tags."
  (let ((name (oref node :object-name))
	(pack (oref node :package-name)))
    (if (<= (length pack) 0)
	name
      (setq pack (concat "\\<\\<" pack "\\>\\>"))
      (concat pack "\\n" name))))

(defmethod cogre-export-dot-label ((node cogre-class))
  "Convert NODE into DOT syntax of semantic tags."
  (concat "{" (call-next-method) "||}"))

(defmethod cogre-export-dot-label ((node cogre-instance))
  "Convert NODE into DOT syntax of semantic tags."
  (let ((title (call-next-method)))
    (if (string-match "\\n" title)
	(replace-match "n:" t t title)
      (concat ":" title))))

;;; LINKS		  
(defmethod cogre-export-dot-method ((link cogre-link))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((start (oref link start))
	(end (oref link end)))
    (semantic-tag (oref end :object-name)
		  'link
		  :to (oref start :object-name)
		  :attributes
		  ( list
		    :arrowhead "none"
		    :arrowtail "none"
		    )
		  )))
		
(defmethod cogre-export-dot-method ((link cogre-inherit))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method))
	(end (oref link end)))
    (cogre-tag-put-dot-attribute tag :arrowtail "empty")
    (cogre-tag-put-dot-attribute tag :arrowsize "2")
    ;(cogre-tag-put-dot-attribute tag :sametail (oref end :object-name))
    tag))
		
(defmethod cogre-export-dot-method ((link cogre-aggregate))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method)))
    (cogre-tag-put-dot-attribute tag :arrowhead "diamond")
    tag))
		
(defmethod cogre-export-dot-method ((link cogre-arrow))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method)))
    (cogre-tag-put-dot-attribute tag :arrowhead "open")
    tag))

;;; TESTS
;;
(eval-when-compile (require 'cogre-periodic))

;;;###autoload
(defun cogre-export-utest ()
  "Run all the COGRE structured export/convert test."
  (interactive)
  (cogre-export-dot-utest)
  ;;(cogre-export-typed-lang-utest)
  )

(defun cogre-export-dot-utest ()
  "Run the COGRE structured dot output converter test.
Basic DOT doesn't require much, so we'll use the periodic
table as an example."
  (interactive)

  (if (not (locate-library "graphviz-dot-mode"))
      (message "Skipping COGRE export to DOT test.  Can't find graphviz-dot-mode.")

    ;; Step one, create the graph.
    (if (get-buffer "*Graph Periodic*")
	(switch-to-buffer "*Graph Periodic*")
      (cogre-periodic))
    ;; Step 2, convert.
    (message "Converting graph %s to DOT structure." (oref cogre-graph name))
    (let* ((graphtag (cogre-export-dot-method cogre-graph))
	   (members (semantic-tag-get-attribute graphtag :members))
	   )
    
      (when (not graphtag)
	(error "Conversions failed to make anything"))

      (when (not (string= (semantic-tag-name graphtag) "Periodic"))
	(error "Converted graph has wrong name: %S" (semantic-tag-name graphtag)))
      (when (not (semantic-tag-of-class-p graphtag 'digraph))
	(error "Converted graph is not a digraph"))

      (let ((N cogre-periodic-node-name-list)
	    (L cogre-periodic-link-connectivity-list)
	    )
	(while members

	  (let* ((M (car members))
		 (n (semantic-tag-name M)))

	    (cond ((semantic-tag-of-class-p M 'node)
		   (if (string= n (car (car N)))
		       (setq N (cdr N))
		     (error "Unexpected node %S in conversion" n))
		   )
		  ((semantic-tag-of-class-p M 'link)
		   (if (string= n (car (car L)))
		       (setq L (cdr L))
		     (message "Expected link %S to %S"
			      (car (car L)) (car (cdr (car L))))
		     (error "Unexpected link from %S to %S in conversion"
			    n (semantic-tag-get-attribute M :to)))
		   )
		  (t
		   (error "Unknown dot tag %S" M)))

	    )
	  (setq members (cdr members)))
	)

      (message "Graph Conversion to DOT success."))))

(provide 'cogre-convert)
;;; cogre-convert.el ends here
