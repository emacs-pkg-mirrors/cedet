;;; semantic-find.el --- Search routines

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-find.el,v 1.1 2003/03/28 02:40:08 zappo Exp $

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
;; Routines for searching through lists of tags.
;; There are several groups of tag search routines:
;;
;; 1) semantic-brute-find-tag-by-*
;;    These routines use brute force hierarchical search to scan
;;    through lists of tags.  They include some parameters
;;    used for compatibility with the semantic 1.x search routines.
;;
;; 1.5) semantic-brute-find-first-tag-by-*
;;    Like 1, except seraching stops on the first match for the given
;;    information.
;;
;; 2) semantic-find-tag-by-*
;;    These prefered search routines attempt to scan through lists
;;    in an intelligent way based on questions asked.
;;
;; 3) semantic-find-*-overlay
;;    These routines use overlays to return tags based on a buffer position.
;;
;; 4) ...

(require 'semantic-tag)

;;; Code:


;;; Overlay Search Routines
;;
(defun semantic-find-tag-by-overlay (&optional positionormarker buffer)
  "Find all nonterminals covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tags covering the specified position
by checking for all overlays covering the current spot.  They are then sorted
from largest to smallest via the start location."
  (save-excursion
    (when positionormarker
      (if (markerp positionormarker)
	  (set-buffer (marker-buffer positionormarker))
	(if (bufferp buffer)
	    (set-buffer buffer))))
    (let ((ol (semantic-overlays-at (or positionormarker (point))))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic)))
	  (when (and tmp
		     ;; We don't need with-position because no tag w/out
		     ;; a position could exist in an overlay.
		     (semantic-tag-p tmp))
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-tag-start a)
				 (semantic-tag-start b)))))))

(defun semantic-find-tag-by-overlay-in-region (start end &optional buffer)
  "Find all nonterminals which exist in whole or in part between START and END.
Uses overlays to determine positin.
Optional BUFFER argument specifies the buffer to use."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((ol (semantic-overlays-in start end))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic)))
	  (when (and tmp
		     ;; See above about position
		     (semantic-tag-p tmp))
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-tag-start a)
				 (semantic-tag-start b)))))))

(defun semantic-find-tag-by-overlay-next (&optional start buffer)
  "Find the next nonterminal after START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (not start) (setq start (point)))
    (let ((os start) (ol nil))
      (while (and os (< os (point-max)) (not ol))
	(setq os (semantic-overlay-next-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (if (and (semantic-overlay-get (car ol) 'semantic)
		     (semantic-tag-p
		      (semantic-overlay-get (car ol) 'semantic))
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a tag
      (when (and ol (semantic-tag-p (semantic-overlay-get ol 'semantic)))
	(semantic-overlay-get ol 'semantic)))))

(defun semantic-find-tag-by-overlay-prev (&optional start buffer)
  "Find the next tag before START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (not start) (setq start (point)))
    (let ((os start) (ol nil))
      (while (and os (> os (point-min)) (not ol))
	(setq os (semantic-overlay-previous-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (if (and (semantic-overlay-get (car ol) 'semantic)
		     (semantic-tag-p
		      (semantic-overlay-get (car ol) 'semantic))
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a tag
      (when (and ol
		 (semantic-tag-p (semantic-overlay-get ol 'semantic)))
	(semantic-overlay-get ol 'semantic)))))

(defun semantic-find-tag-parent-by-overlay (tag)
  "Find the parent of TAG by overlays.
Overlays are a fast way of finding this information for active buffers."
  (let ((tag (nreverse (semantic-find-tag-by-overlay
			(semantic-tag-start tag)))))
    ;; This is a lot like `semantic-current-nonterminal-parent', but
    ;; it uses a position to do it's work.  Assumes two tags don't share
    ;; the same start unless they are siblings.
    (car (cdr tag))))

(defun semantic-current-tag ()
  "Return the current nonterminal in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here."
  (car (nreverse (semantic-find-tag-by-overlay))))

(defun semantic-current-tag-parent ()
  "Return the current nonterminals parent in the current buffer.
A tag's parent would be a containing structure, such as a type
containing a field.  Return nil if there is no parent."
  (car (cdr (nreverse (semantic-find-tag-by-overlay)))))

(defun semantic-current-tag-of-class (class)
  "Return the current (smallest) nonterminal of CLASS in the current buffer.
If the smallest tag is not of type CLASS, keep going upwards until one
is found.
Uses `semantic-tag-class' for classification."
  (let ((tags (nreverse (semantic-find-tag-by-overlay))))
    (while (and tags
		(not (eq (semantic-tag-class (car tags)) class)))
      (setq tags (cdr tags)))
    (car tags)))


;;; Search Routines
;;

; NEW SEARCH FCNS GO HERE.  WHAT ARE THEY?


;;
;; ************************** Compatibility ***************************
;;

;;; Old Style Brute Force Search Routines
;;
;; These functions will search through nonterminal lists explicity for
;; desired information.

;; The -by-name nonterminal search can use the built in fcn
;; `assoc', which is faster than looping ourselves, so we will
;; not use `semantic-brute-find-tag-by-function' to do this,
;; instead erroring on the side of speed.

(defun semantic-brute-find-first-tag-by-name
  (name streamorbuffer &optional search-parts search-include)
  "Find a tag NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tags.
If SEARCH-INCLUDE is non-nil, search include files."
  (let* ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel))
		   streamorbuffer))
         (assoc-fun (if semantic-case-fold
                        #'assoc-ignore-case
                      #'assoc))
	 (m (funcall assoc-fun name stream)))
    (if m
	m
      (let ((toklst stream)
	    (children nil))
	(while (and (not m) toklst)
	  (if search-parts
	      (progn
		(setq children (semantic-tag-components-with-overlays
				(car toklst)))
		(if children
		    (setq m (semantic-brute-find-first-tag-by-name
			     name children search-parts search-include)))))
	  (setq toklst (cdr toklst)))
	(if (not m)
	    ;; Go to dependencies, and search there.
	    nil)
	m))))

(defmacro semantic-brute-find-tag-by-class
  (class streamorbuffer &optional search-parts search-includes)
  "Find all tags with a class CLASS within STREAMORBUFFER.
CLASS is a symbol representing the class of the tokens to find.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  `(semantic-brute-find-tag-by-function
    (lambda (tag) (eq ,class (semantic-tag-class tag)))
    ,streamorbuffer ,search-parts ,search-includes))

(defmacro semantic-brute-find-tag-standard
  (streamorbuffer &optional search-parts search-includes)
  "Find all tags in STREAMORBUFFER which define simple class types.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  `(semantic-brute-find-tag-by-function
    (lambda (tag) (member (semantic-tag-class tag)
			  '(function variable type)))
    ,streamorbuffer ,search-parts ,search-includes))

(defun semantic-brute-find-tag-by-type
  (type streamorbuffer &optional search-parts search-includes)
  "Find all tags with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the token returned.
See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag)
     (let ((ts (semantic-tag-type tag)))
       (if (and (listp ts)
		(or (= (length ts) 1)
		    (eq (semantic-tag-class ts) 'type)))
	   (setq ts (semantic-tag-name ts)))
       (equal type ts)))
   streamorbuffer search-parts search-includes))

(defun semantic-brute-find-tag-by-type-regexp
  (regexp streamorbuffer &optional search-parts search-includes)
  "Find all tags with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tokens returned.  See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag)
     (let ((ts (semantic-tag-type tag)))
       (if (listp ts)
	   (setq ts
		 (if (eq (semantic-tag-class ts) 'type)
		     (semantic-tag-name ts)
		   (car ts))))
       (and ts (string-match regexp ts))))
   streamorbuffer search-parts search-includes))

(defun semantic-brute-find-tag-by-name-regexp
  (regex streamorbuffer &optional search-parts search-includes)
  "Find all tags whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (string-match regex (semantic-tag-name tag)))
    streamorbuffer search-parts search-includes)
  )

(defun semantic-brute-find-tag-by-property
  (property value streamorbuffer &optional search-parts search-includes)
  "Find all tags with PROPERTY equal to VALUE in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (equal (semantic--tag-get-property tag property) value))
   streamorbuffer search-parts search-includes)
  )

(defun semantic-brute-find-tag-by-attribute
  (attr streamorbuffer &optional search-parts search-includes)
  "Find all tags with a given ATTR in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (semantic-tag-get-attribute tag attr))
   streamorbuffer search-parts search-includes)
  )

(defun semantic-brute-find-tag-by-attribute-value
  (attr value streamorbuffer &optional search-parts search-includes)
  "Find all tags with a given ATTR equal to VALUE in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
VALUE is the value that ATTR should match.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag) (equal (semantic-tag-get-attribute tag attr) value))
   streamorbuffer search-parts search-includes)
  )

(defun semantic-brute-find-tag-by-function
  (function streamorbuffer &optional search-parts search-includes)
  "Find all tags for which FUNCTION returns non-nil within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tags
are searched.  The overloadable function `semantic-tag-componenets' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches.  This parameter hasn't be active for a while
and is obsolete."
  (let ((streamlist (list
		     (semantic-something-to-stream streamorbuffer)))
	(includes nil)			;list of includes
	(stream nil)			;current stream
        (tag  nil)                    ;current tag
	(sl nil)			;list of tag children
	(nl nil)			;new list
        (case-fold-search semantic-case-fold))
    (if search-includes
	(setq includes (semantic-brute-find-tag-by-class
			'include (car streamlist))))
    (while streamlist
      (setq stream     (car streamlist)
            streamlist (cdr streamlist))
      (while stream
        (setq tag  (car stream)
              stream (cdr stream))
	(if (not (semantic-tag-p tag))
            ;; `semantic-tag-components-with-overlays' can return invalid
            ;; tags if search-parts is not equal to 'positiononly
            nil ;; Ignore them!
          (if (funcall function tag)
              (setq nl (cons tag nl)))
          (and search-parts
               (setq sl (if (eq search-parts 'positiononly)
			    (semantic-tag-components-with-overlays tag)
			  (semantic-tag-components tag))
		     )
               (setq nl (nconc nl
                               (semantic-brute-find-tag-by-function
                                function sl
                                search-parts search-includes)))))))
    (setq nl (nreverse nl))
;;;    (while includes
;;;      (setq nl (append nl (semantic-brute-find-tag-by-function
;;;			   
;;;			   ))))
    nl))

(defun semantic-brute-find-first-tag-by-function
  (function streamorbuffer &optional search-parts search-includes)
  "Find the first nonterminal which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

The following parameters were never implemented.

If optional argument SEARCH-PARTS, all sub-parts of tags are searched.
The overloadable function `semantic-tag-components' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches."
  (let ((stream (semantic-something-to-stream streamorbuffer))
	(found nil)
        (case-fold-search semantic-case-fold))
    (while (and (not found) stream)
      (if (funcall function (car stream))
	  (setq found (car stream)))
      (setq stream (cdr stream)))
    found))


;;; Old Positional Searches
;;
;; Are these useful anymore?
;;
(defun semantic-brute-find-tag-by-position (position streamorbuffer
						     &optional nomedian)
  "Find a nonterminal covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil."
  (save-excursion
    (if (markerp position) (set-buffer (marker-buffer position)))
    (let* ((stream (if (bufferp streamorbuffer)
		       (save-excursion
			 (set-buffer streamorbuffer)
			 (semantic-bovinate-toplevel))
		     streamorbuffer))
	   (prev nil)
	   (found nil))
      (while (and stream (not found))
	;; perfect fit
	(if (and (>= position (semantic-tag-start (car stream)))
		 (<= position (semantic-tag-end (car stream))))
	    (setq found (car stream))
	  ;; Median between to objects.
	  (if (and prev (not nomedian)
		   (>= position (semantic-tag-end prev))
		   (<= position (semantic-tag-start (car stream))))
	      (let ((median (/ (+ (semantic-tag-end prev)
				  (semantic-tag-start (car stream)))
			       2)))
		(setq found
		      (if (> position median)
			  (car stream)
			prev)))))
	;; Next!!!
	(setq prev (car stream)
	      stream (cdr stream)))
      found)))

(defun semantic-brute-find-innermost-tag-by-position
  (position streamorbuffer &optional nomedian)
  "Find a list of tags covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable."
  (let* ((returnme nil)
	 (current (semantic-brute-find-tag-by-position
		   position streamorbuffer nomedian))
	 (nextstream (and current
			  (if (eq (semantic-tag-class current) 'type)
			      (semantic-tag-type-members current)
			    nil))))
    (while nextstream
      (setq returnme (cons current returnme))
      (setq current (semantic-brute-find-tag-by-position
		     position nextstream nomedian))
      (setq nextstream (and current
			    ;; NOTE TO SELF:
			    ;; Looking at this after several years away,
			    ;; what does this do???
			    (if (eq (semantic-tag-class current) 'token)
				(semantic-tag-type-members current)
			      nil))))
    (nreverse (cons current returnme))))


;;; Compatibility Aliases
(semantic-alias-obsolete 'semantic-find-tag-by-overlay
			 'semantic-find-token-by-overlay)

(semantic-alias-obsolete 'semantic-find-tag-by-overlay-in-region
			 'semantic-find-token-by-overlay-in-region)

(semantic-alias-obsolete 'semantic-find-tag-by-overlay-next
			 'semantic-find-token-by-overlay-next)

(semantic-alias-obsolete 'semantic-find-tag-by-overlay-prev
			 'semantic-find-token-by-overlay-prev)

(semantic-alias-obsolete 'semantic-find-tag-parent-by-overlay
			 'semantic-find-token-parent-by-overlay)

(semantic-alias-obsolete 'semantic-current-tag
			 'semantic-current-token)

(semantic-alias-obsolete 'semantic-current-tag-parent
			 'semantic-current-token-parent)

(semantic-alias-obsolete 'semantic-current-tag-of-class
			 'semantic-current-token-of-class)

(semantic-alias-obsolete 'semantic-brute-find-first-tag-by-name
			 'semantic-find-first-nonterminal-by-name)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-class
			 'semantic-find-nonterminal-by-token)

(semantic-alias-obsolete 'semantic-brute-find-tag-standard
			 'semantic-find-nonterminal-standard)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-type
			 'semantic-find-nonterminal-by-type)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-type-regexp
			 'semantic-find-nonterminal-by-type-regexp)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-name-regexp
			 'semantic-find-nonterminal-by-name-regexp)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-property
			 'semantic-find-nonterminal-by-property)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-attribute
			 'semantic-find-nonterminal-by-extra-spec)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-extra-spec-value
			 'semantic-find-nonterminal-by-attribute-value)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-function
			 'semantic-find-nonterminal-by-function)

(semantic-alias-obsolete 'semantic-brute-find-first-tag-by-function
			 'semantic-find-nonterminal-by-function-first-match)

(semantic-alias-obsolete 'semantic-brute-find-tag-by-position
			 'semantic-find-token-by-position)

(semantic-alias-obsolete 'semantic-brute-find-innermost-tag-by-position
			 'semantic-find-innermost-token-by-position)


(provide 'semantic-find)

;;; semantic-find.el ends here
