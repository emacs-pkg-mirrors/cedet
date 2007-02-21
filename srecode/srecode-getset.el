;;; srecode-getset.el --- 

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; SRecoder application for inserting new get/set methods into a class.

(require 'semantic)
(require 'semantic-analyze)
(require 'srecode-insert)
(require 'srecode-dictionary)

;;; Code:

(defun srecode-insert-getset ()
  "Insert get/set methods for the current class."
  (interactive)

  ;; Step 1: Try to derive the tag we will use
  (let* ((class (srecode-auto-choose-class (point)))
	 (inclass (eq (semantic-current-tag-of-class 'type) class))
	 (field nil)
	 )
    (when (not class)
      (error "Move point to a class and try again"))

    ;; Step 2: Select a name for the field we will use.
    (when inclass
      (setq field (srecode-auto-choose-field (point))))

    (when (not field)
      (setq field (srecode-query-for-field class)))

    ;; Step 3: Insert a new field if needed
    (when (stringp field)
      (srecode-position-new-field class inclass)
      )

    ;; Step 4: Insert the get/set methods

    ))

(defun srecode-position-new-field (class inclass)
  "Select a position for a new field for CLASS.
If INCLASS is non-nil, then the cursor is already in the class
and should not be moved during point selection."
  
  ;; If we aren't in the class, get the cursor there, pronto!
  (when (not inclass)

    (let ((kids (semantic-find-tags-by-class
		 'variable (semantic-tag-type-members class))))
      (cond (kids
	     (semantic-go-to-tag (car kids) class))
	    (t
	     (semantic-go-to-tag class)))
      )

    (switch-to-buffer (current-buffer)))

  
  
  ;; Once the cursor is in our class, ask the user to position
  ;; the cursor to keep going.
  

  )



(defun srecode-auto-choose-field (point)
  "Choose a field for the get/set methods."
  (save-excursion
    (when point
      (goto-char point))

    (let ((field (semantic-current-tag-of-class 'variable)))
      
      ;; If we get a field, make sure the user gets a chance to choose.
      (when field
	(when (not (y-or-n-p "Use field %s?" (semantic-tag-name field)))
	  (setq field nil))

      field))))

(defun srecode-query-for-field (class)
  "Query for a field in CLASS."
  (let* ((kids (semantic-find-tags-by-class
		'variable (semantic-tag-type-members class)))
	 (sel (completing-read "Use Field: " kids))
	 )

    (or (semantic-find-tags-by-name sel kids)
	sel)
    ))

(defun srecode-auto-choose-class (point)
  "Choose a class based on locatin of POINT."
  (save-excursion
    (when point
      (goto-char point))
    
    (let ((tag (semantic-current-tag-of-class 'type)))

      (when (or (not tag)
		(not (string= (semantic-tag-type tag) "class")))
	;; The current tag is not a class.  Are we in a fcn
	;; that is a method?
	(setq tag (semantic-current-tag-of-class 'function))

	(when (and tag
		   (semantic-tag-function-parent tag))
	  (let ((p (semantic-tag-function-parent tag)))
	    ;; @TODO : Copied below out of semantic-analyze
	    ;;         Turn into a routine.

	    (let* ((searchname (cond ((stringp p) p)
				     ((semantic-tag-p p)
				      (semantic-tag-name p))
				     ((and (listp p) (stringp (car p)))
				      (car p))))
		   (ptag (semantic-analyze-find-tag searchname
						    'type nil)))
	      (when ptag (setq tag ptag ))
	      ))))

      (when (or (not tag)
		(not (semantic-tag-of-class-p tag 'type))
		(not (string= (semantic-tag-type tag) "class")))
	;; We are not in a class that needs a get/set method.
	;; Analyze the current context, and derive a class name.
	(let* ((ctxt (semantic-analyze-current-context))
	       (pfix nil)
	       (ans nil))
	  (when ctxt
	    (setq pfix (reverse (oref ctxt prefix)))
	    (while (and (not ans) pfix)
	      ;; Start at the end and back up to the first class.
	      (when (and (semantic-tag-p (car pfix))
			 (semantic-tag-of-class-p (car pfix) 'type)
			 (string= (semantic-tag-type (car pfix))
				  "class"))
		(setq ans (car pfix)))
	      (setq pfix (cdr pfix))))
	  (setq tag ans)))

      tag)))

(provide 'srecode-getset)

;;; srecode-getset.el ends here

