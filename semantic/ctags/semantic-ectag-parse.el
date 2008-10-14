;;; semantic-ectag-parse.el --- exuberent CTags into Semantic tags

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-parse.el,v 1.2 2008/10/14 01:02:22 zappo Exp $

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
;; Converting CTags output tables into Semantic Tag Tables.
;;
;; This works by scanning the output of a CTags output buffer,
;; and instead creating semantic tags for each line.
;;
;; Tags that appear as members, or otherwise appear to belong to
;; other tags in the list will be parented appropriately.

(require 'semantic-ectag-util)
;;; Code:

;; These variables need to be bound to values on a per-mode basis.
(defvar semantic-ectag-lang nil
  "The language name used by Exuberent CTags for the current buffer.")
(defvar semantic-ectag-lang-kind nil
  "The kinds of tags fetched by Exuberent CTags for the current language.")

;;;###autoload
(defun semantic-ectag-parse-buffer ()
  "Execute Exuberent CTags on this buffer.
Convert the output tags into Semantic tags."
  (interactive)
  (when (not semantic-ectag-lang)
    (error "Exuberent CTag support for Semantic not configured for %s"
	   major-mode))
  (let* ((start (current-time))
	 (buff (semantic-ectag-run
		"--sort=no" ;; Don't resort the names.
		"--excmd=number" ;; add line numbers
		"--fields=aKStsim" ;; Add extra info
		(format "--%s-kinds=%s"
			semantic-ectag-lang
			semantic-ectag-lang-kind)
		"-f" "-" ;; Send to standard out.
		(buffer-file-name) ;; The file to parse.
		))
	 (mode major-mode)
	 (end (current-time))
	 (parsed-output (save-excursion
			  (set-buffer buff)
			  (funcall mode)
			  (semantic-ectag-parse-tags))
			)
	 )
    (when (interactive-p)
      (message "Parsed %d tags in %d seconds."
	       (length parsed-output)
	       (semantic-elapsed-time start end))
      (let ((ab (data-debug-new-buffer (concat "*"
					       (buffer-name)
					       " ADEBUG*")))
	    )
	(data-debug-insert-tag-list parsed-output "* ")))
    ))

(defun semantic-ectag-parse-tags ()
  "Parse the Exuberent CTags output in the current buffer."
  (goto-char (point-min))
  (let ((tags nil)
	(ptag-stack nil) ; parent tag stack.
	(pname nil)      ; parent names.
	)
    (while (not (eobp))
      (let* ((ptag (semantic-ectag-parse-one-tag
		   (buffer-substring (point) (point-at-eol))))
	     (tag (car ptag))
	     (parents (cdr ptag))
	     )

	;; Set some language specific attributes.
	(semantic-ectag-set-language-attributes tag parents)

	;; At this point, we have to guess if TAG is embedded into one
	;; of the parents in the parent stack.  There are three cases:
	;;
	;; 1) Old Lineage - "parent" matches pname exactly.
	;;    --> embed into the end of the parent tag stack.
	;; 2) Mixed Lineage - "parent" matches only part of the parent tag stack.
	;;    --> Find the partial match, reset to there, and
	;;        then embed the tag into the correct parent.
	;; 3) New Lineage - "parent" does not match pname at all.
	;;    --> Start over.
	;;
	;; Note that old/mixed/new lineage are a mixture of the same basic
	;; algorithm to scan the list of known parents to find the match.
	;;

	(if (not parents)
	    (progn
	      ;; Push the tag into our list.
	      (push tag tags)

	      (if (semantic-tag-of-class-p tag 'type)
		  ;; Merge stacks, and also
		  (setq ptag-stack (list tag)
			pname (list (semantic-tag-name tag)))
		;; Flush embedded parantage
		(setq ptag-stack nil
		      pname nil))
	      )

	  ;; If we have parents, lets look them up.
	  (let ((oldnames pname)
		(newnames parents)
		(oldstack ptag-stack)
		(newstack nil)
		(add-to-this-parent nil)
		(pushed-parent-list nil)
		)
	    (while (and oldstack (string= (car oldnames) (car newnames)))
	      (setq newstack (cons (car oldstack) newstack)
		    oldstack (cdr oldstack)
		    oldnames (cdr oldnames)
		    newnames (cdr newnames)))

	    ;; Push this tag into the last parent we found.
	    (setq add-to-this-parent (car newstack))
	    (setq pushed-parent-list newnames)

	    ;; Do special stuff with type tags.
	    (when (semantic-tag-of-class-p tag 'type)
	      ;; Fill in the intermediate stack with NIL.
	      (while newnames
		(setq newnames (cdr newnames)
		      newstack (cons nil newstack)))
	      ;; Add TAG to the end for matching the next tag in
	      ;; the list.
	      (setq newstack (cons tag newstack)))

	    ;; Set back into ptag-stack.
	    (setq ptag-stack (nreverse newstack))
	    
	    ;; Fix up the name list too
	    (if (semantic-tag-of-class-p tag 'type)
		(setq pname (append parents (list (semantic-tag-name tag))))
	      (setq pname parents))

	    ;; Set the lineage of the new tag.
	    (if (not add-to-this-parent)
		;; No parent to add to.
		(progn
		  (push tag tags)
		  (semantic-ectag-add-parent tag parents)
		  )
	      ;; Add TAG to the correct parent, and save name
	      (semantic-ectag-add-child add-to-this-parent tag)
	      (semantic-ectag-add-parent tag pushed-parent-list)
	      )
	    )
	  )
      
	(end-of-line)
	(condition-case nil (forward-char 1) (error nil))))
    (nreverse tags)))

(defun semantic-ectag-add-child (parent child)
  "Add into the PARENT tag a new CHILD tag."
  (let ((children (semantic-tag-type-members parent))
	)
    (add-to-list 'children child t)
    (semantic--tag-put-attribute parent :members children)
    ))

(defun semantic-ectag-add-parent (tag parentlist)
  "Add to TAG the tag name in PARENTLIST."
  (when parentlist
    (let ((pstring (semantic-analyze-unsplit-name parentlist)))
      (semantic--tag-put-attribute tag :parent pstring)
      )))

(defun semantic-ectag-parse-one-tag (line)
  "Split the Exuberent Ctag LINE into a new tag.
Returns the list ( TAG P1 P2 Pn...)
where TAG is the new tag, P1, P2, and Pn is the list of
parents running forward, such as namespace/namespace/class"
  (let* ((elements (split-string line "\t"))
	 (ect-class (nth 3 elements))
	 (class (intern ect-class))
	 (prototype nil)
	 (type nil)

	 (class-sym (cond
		     ((member class '(function variable))
		      class)
		     ((eq class 'prototype)
		      (setq prototype t)
		      'function)
		     ((member class '(namespace class struct union enum typedef))
		      (setq type (symbol-name class))
		      'type)
		     ((eq class 'member)
		      'variable)
		     (t
		      (error "Unknown ctag output kind %s" class))))

	 (attr (semantic-ectag-split-fields (nthcdr 4 elements) class))
	 (line (string-to-number (nth 2 elements)))

	 (tag (semantic-tag (nth 0 elements)
			    class-sym
			    ;; Leave filename for some other time
			    ;; :filename (nth 1 elements)
			    :line line
			    :prototype-flag prototype
			    :type (if (eq class-sym 'type) type nil) ;; Nil alows override later
			    ))
	 (parents nil)
	 )
    (while attr
      ;; Loop over each attribute, adding it into the tag.
      (cond ((eq (car attr) :parent)
	     (setq parents (semantic-analyze-split-name (car (cdr attr))))
	     (when (stringp parents)
	       (setq parents (list parents))))
	    (t
	     (semantic-tag-put-attribute tag (car attr) (car (cdr attr)))))
      (setq attr (cdr (cdr attr)))
      )
    ;; Now return the new tag.
    (cons tag parents)
    ))

(defun semantic-ectag-split-fields (fields class)
  "Convert FIELDS into a list of Semantic tag attributes.
CLASS is the class of the tag we are parsing these fields for."
  (let ((attr nil))
    (while fields
      (string-match "\\w+:" (car fields))
      (let* ((me (match-end 0))
	     (field (substring (car fields) 0 (1- me)))
	     (str (substring (car fields) me))
	     )
	(cond ((string= field "type")
	       (push str attr)
	       (push :type attr))
	      ((string= field "line")
	       (push (string-to-number str) attr)
	       (push :line attr))
	      ;; Class and Namespace seem to provide a name similar
	      ;; to our :parent tag, so both should do that.
	      ;; There is something extra here though..  It should
	      ;; be possible to use this info to do a reparenting operation.
	      ((string= field "class")
	       (push str attr)
	       (push :parent attr))
	      ((string= field "namespace")
	       (push str attr)
	       (push :parent attr))
	      ;((string= field "inheritance")
	       ;(push str attr)
	       ;(push :parent attr)
	       ;)
	      ((string= field "access")
	       (push str attr)
	       (push :protection attr))
	      ((string= field "signature")
	       (let ((sigattr (semantic-ectag-split-signature-summary str)))
		 (push sigattr attr)
		 (push :arguments attr)))
	      (t
	       (message "Unknown ectag field %s" field))))
      (setq fields (cdr fields)))
    attr))

(define-overloadable-function semantic-ectag-split-signature-summary (summary)
  "Split SUMMARY into Semantic tag compatible attributes.
SUMMARY is part of the output from Exuberent CTags that shows the
text from a file where the tag was found.")

(defun semantic-ectag-split-signature-summary-default (summary)
  "Default behavior for splitting a Exuberent CTags SUMMARY.
Assume comma separated list."
  (split-string summary "[(),]" t))

(define-overloadable-function semantic-ectag-set-language-attributes (tag parents)
  "Augment TAG with additional attributes based on language.
PARENTS is the list of parent names for TAG.")

(defun semantic-ectag-set-language-attributes-default (tag parents)
  "Default behavior does nothing.
TAG and PARENTS are ignored."
  nil)

(provide 'semantic-ectag-parse)
;;; semantic-ectag-parse.el ends here
