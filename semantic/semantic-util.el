;;; semantic-util.el --- Utilities for use with semantic token streams

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util.el,v 1.53 2001/03/10 02:25:39 zappo Exp $

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
;; API for accessing and searching nonterminal streams from the
;; Semantic Bovinator.
;;

;;; Code:

(defvar semantic-type-relation-separator-character '(".")
  "Character strings used to separation a parent/child relationship.
This list of strings are used for displaying or finding separators
in variable field dereferencing.  The first character will be used for
display.  In C, a type field is separated like this: \"type.field\"
thus, the character is a \".\".  In C, and additional value of \"->\"
would be in the list, so that \"type->field\" could be found.")
(make-variable-buffer-local 'semantic-type-relation-separator-character)

;;; Simple APIs
;;
;; These macros extract parts from the default token types as
;; described by `semantic-toplevel-bovine-table'

;; Check semantic.el for the other token information extraction functions.

(defun semantic-token-type (token)
  "Retrieve the type of TOKEN."
  (if (member (semantic-token-token token)
	      '(function variable type))
      (nth 2 token)))

(defmacro semantic-token-type-parts (token)
  "Retrieve the parts of the type TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-type-parent (token)
  "Retrieve the parent of the type TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-type-extra-specs (token)
  "Retrieve extra specifications for the type TOKEN."
  `(nth 5 ,token))

(defmacro semantic-token-type-extra-spec (token spec)
  "Retrieve an extra specification for the type TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-type-extra-specs ,token))))

(defmacro semantic-token-type-modifiers (token)
  "Retrieve modifiers for the type TOKEN."
  `(semantic-token-type-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-function-args (token)
  "Retrieve the arguments of the function TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-function-extra-specs (token)
  "Retrieve extra specifications for the function TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-function-extra-spec (token spec)
  "Retrieve an extra specification for the function TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-function-extra-specs ,token))))

(defmacro semantic-token-function-modifiers (token)
  "Retrieve modifiers for the function TOKEN."
  `(semantic-token-function-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-function-throws (token)
  "The symbol string that a function can throws.
Determines if it is available based on the length of TOKEN."
  `(semantic-token-function-extra-spec ,token 'throws))

(defmacro semantic-token-function-parent (token)
  "The parent of the function TOKEN.
A function has a parent if it is a method of a class, and if the
function does not appear in body of it's parent class."
  `(semantic-token-function-extra-spec ,token 'parent))

(defmacro semantic-token-variable-default (token)
  "Retrieve the default value of the variable TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-variable-extra-specs (token)
  "Retrieve extra specifications for the variable TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-variable-extra-spec (token spec)
  "Retrieve an extra specification for the variable TOKEN.
SPEC is the symbol whose specification value to get."
  `(cdr (assoc ,spec (semantic-token-variable-extra-specs ,token))))

(defmacro semantic-token-variable-modifiers (token)
  "Retrieve modifiers for the variable TOKEN."
  `(semantic-token-variable-extra-spec ,token 'typemodifiers))

(defmacro semantic-token-variable-const (token)
  "Retrieve the status of constantness from the variable TOKEN."
  `(semantic-token-variable-extra-spec ,token 'const))

(defmacro semantic-token-variable-optsuffix (token)
  "Optional details if this variable has bit fields, or array dimentions.
Determines if it is available based on the length of TOKEN."
  `(semantic-token-variable-extra-spec ,token 'suffix))

(defmacro semantic-token-include-system (token)
 "Retrieve the flag indicating if the include TOKEN is a system include."
  `(nth 2 ,token))

(defun semantic-token-extra-spec (token spec)
  "Retrieve an extra specification for TOKEN.
SPEC is a symbol whose specification value to get.
This function can get extra specifications from any type of token.
Do not use the function if you know what type of token you are dereferencing.
Instead, use `semantic-token-variable-extra-spec',
`semantic-token-function-extra-spec', or  `semantic-token-type-extra-spec'."
  (let ((tt (semantic-token-token token)))
    (cond ((eq tt 'variable)
	   (semantic-token-variable-extra-spec token spec))
	  ((eq tt 'function)
	   (semantic-token-function-extra-spec token spec))
	  ((eq tt 'type)
	   (semantic-token-type-extra-spec token spec))
	  (t nil))))

;;; Searching by Position APIs
;;
;; These functions will find nonterminals based on a position.
(defun semantic-find-nonterminal-by-position (position streamorbuffer
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
	(if (and (>= position (semantic-token-start (car stream)))
		 (<= position (semantic-token-end (car stream))))
	    (setq found (car stream))
	  ;; Median between to objects.
	  (if (and prev (not nomedian)
		   (>= position (semantic-token-end prev))
		   (<= position (semantic-token-start (car stream))))
	      (let ((median (/ (+ (semantic-token-end prev)
				  (semantic-token-start (car stream)))
			       2)))
		(setq found
		      (if (> position median)
			  (car stream)
			prev)))))
	;; Next!!!
	(setq prev (car stream)
	      stream (cdr stream)))
      found)))

(defun semantic-find-innermost-nonterminal-by-position
  (position streamorbuffer &optional nomedian)
  "Find a list of nonterminals covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable."
  (let* ((returnme nil)
	 (current (semantic-find-nonterminal-by-position
		   position streamorbuffer nomedian))
	 (nextstream (and current
			  (if (eq (semantic-token-token current) 'type)
			      (semantic-token-type-parts current)
			    nil))))
    (while nextstream
      (setq returnme (cons current returnme))
      (setq current (semantic-find-nonterminal-by-position
		     position nextstream nomedian))
      (setq nextstream (and current
			    (if (eq (semantic-token-token current) 'token)
				(semantic-token-type-parts current)
			      nil))))
    (nreverse (cons current returnme))))

(defun semantic-find-nonterminal-by-overlay (&optional positionormarker buffer)
  "Find all nonterminals covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tokens covering the specified position
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
	  (when tmp
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-token-start a)
				 (semantic-token-start b)))))))

(defun semantic-find-nonterminal-by-overlay-in-region (start end &optional buffer)
  "Find all nonterminals which exist in whole or in part between START and END.
Uses overlays to determine positin.
Optional BUFFER argument specifies the buffer to use."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((ol (semantic-overlays-in start end))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic)))
	  (when tmp
	    (setq ret (cons tmp ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-token-start a)
				 (semantic-token-start b)))))))

(defun semantic-find-nonterminal-by-overlay-next (&optional start buffer)
  "Find the next nonterminal after START in BUFFER.
If START is in an overlay, find the token which starts next,
not the current token."
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
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a token
      (when ol
	(semantic-overlay-get ol 'semantic)))))

(defun semantic-find-nonterminal-by-overlay-prev (&optional start buffer)
  "Find the next nonterminal after START in BUFFER.
If START is in an overlay, find the token which starts next,
not the current token."
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
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ;; convert ol to a token
      (when ol
	(semantic-overlay-get ol 'semantic)))))

(defun semantic-current-nonterminal ()
  "Return the current nonterminal in the current buffer.
If there are more than one in the same location, return the
smallest token."
  (car (nreverse (semantic-find-nonterminal-by-overlay))))

;;; Nonterminal regions and splicing
;;
;; This functionality is needed to take some set of dirty code,
;; and splice in new tokens after a partial reparse.
;;
;; Properties set on the tokens are:
;;  dirty          - This token is dirty
;;  dirty-after    - This token, and the white space after it is dirty
;;  dirty-before   - This token, and the white space before it is dirty
;;  dirty-children - This token has children that are dirty.
;;
;; EXPERIMENTAL
(defsubst semantic-find-nearby-dirty-tokens (beg end)
  "Make a special kind of token for dirty whitespace.
Argument BEG and END is the region to find nearby tokens.
EXPERIMENTAL"
  (let ((prev (semantic-find-nonterminal-by-overlay-prev beg))
	(next (semantic-find-nonterminal-by-overlay-next end)))
    (if prev (semantic-token-put prev 'dirty-after t))
    (if next (semantic-token-put next 'dirty-before t))
    (list prev next)))

(defun semantic-set-tokens-dirty-in-region (beg end)
  "Mark the region between BEG and END as dirty.
This is done by finding tokens overlapping the region, and marking
them dirty.  Regions not covered by a token are then marked as
dirty-after, meaning the space after that area is dirty.
This function will be called in an after change hook, and must
be very fast.
EXPERIMENTAL"
  (let ((tromp (semantic-find-nonterminal-by-overlay-in-region beg end))
	(ttmp nil)
	)
    (if (not tromp)
	;; No tokens hit, setup a dirty region on the screen.
	(setq tromp (semantic-get-dirty-token beg end))
      ;; First, mark all fully dirty tokens.
      (setq ttmp tromp)
      (while ttmp
	(and (> beg (semantic-token-start (car tromp)))
	     (< end (semantic-token-end (car tromp))))

	)
	)))


;;; Generalized nonterminal searching
;;
;; These functions will search through nonterminal lists explicity for
;; desired information.

;; The -by-name nonterminal search can use the built in fcn
;; `assoc', which is faster than looping ourselves, so we will
;; not use `semantic-find-nonterminal-by-function' to do this,
;; instead erroring on the side of speed.
(defun semantic-find-nonterminal-by-name
  (name streamorbuffer &optional search-parts search-include)
  "Find a nonterminal NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tokens.
If SEARCH-INCLUDE is non-nil, search include files."
  (let* ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel))
		   streamorbuffer))
	 (m (assoc name stream)))
    (if m
	m
      (let ((toklst stream)
	    (children nil))
	(while (and (not m) toklst)
	  (if search-parts
	      (progn
		(setq children (semantic-nonterminal-children (car toklst) t))
		(if children
		    (setq m (semantic-find-nonterminal-by-name
			     name children search-parts search-include)))))
	  (setq toklst (cdr toklst)))
	(if (not m)
	    ;; Go to dependencies, and search there.
	    nil)
	m))))

(defmacro semantic-find-nonterminal-by-token
  (token streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals with a token TOKEN within STREAMORBUFFER.
TOKEN is a symbol representing the type of the tokens to find.
Optional argument SEARCH-PARTS and SEARCH-INCLUDE are passed to
`semantic-find-nonterminal-by-function'."
  `(semantic-find-nonterminal-by-function
    (lambda (tok) (eq ,token (semantic-token-token tok)))
    ,streamorbuffer ,search-parts ,search-includes))

(defmacro semantic-find-nonterminal-standard
  (streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals in STREAMORBUFFER which define simple token types.
Optional argument SEARCH-PARTS and SEARCH-INCLUDE are passed to
`semantic-find-nonterminal-by-function'."
  `(semantic-find-nonterminal-by-function
    (lambda (tok) (member tok '(function variable type)))
    ,streamorbuffer ,search-parts ,search-includes))

(defvar semantic-default-built-in-types nil
  "For a given language, a set of built-in types.")
(make-variable-buffer-local 'semantic-default-built-in-types)

(defun semantic-find-nonterminal-by-type
  (type streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the token returned.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-find-nonterminal-by-function'."
  (if (member type semantic-default-built-in-types)
      (list (list type 'type "built in"))
    (semantic-find-nonterminal-by-function
     (lambda (tok)
       (let ((ts (semantic-token-type tok)))
	 (if (and (listp ts) (eq (semantic-token-token ts) 'type))
	     (setq ts (semantic-token-name ts)))
	 (equal type ts)))
     streamorbuffer search-parts search-includes)))

(defun semantic-find-nonterminal-by-type-regexp
  (regexp streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tokens returned.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-find-nonterminal-by-function'."
  (semantic-find-nonterminal-by-function
   (lambda (tok)
     (let ((ts (semantic-token-type tok)))
       (if (listp ts)
	   (setq ts
		 (if (eq (semantic-token-token ts) 'type)
		     (semantic-token-name ts)
		   (car ts))))
       (and ts (string-match regexp ts))))
   streamorbuffer search-parts search-includes))

(defmacro semantic-find-nonterminal-by-name-regexp
  (regex streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-find-nonterminal-by-function'."
  `(semantic-find-nonterminal-by-function
    (lambda (tok) (string-match ,regex (semantic-token-name tok)))
    ,streamorbuffer ,search-parts ,search-includes)
  )

(defmacro semantic-find-nonterminal-by-property
  (property value streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals with PROPERTY equal to VALUE in STREAMORBUFFER.
Properties can be added with `semantic-token-put'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-find-nonterminal-by-function'."
  `(semantic-find-nonterminal-by-function
   (lambda (tok) (equal (semantic-token-get tok ,property) ,value))
   ,streamorbuffer ,search-parts ,search-includes)
  )

(defmacro semantic-find-nonterminal-by-extra-spec
  (spec streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals with a given SPEC in STREAMORBUFFER.
SPEC is a symbol key into the modifiers association list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-find-nonterminal-by-function'."
  `(semantic-find-nonterminal-by-function
    (lambda (tok) (semantic-token-extra-spec tok ,spec))
    ,streamorbuffer ,search-parts ,search-includes)
  )

(defun semantic-find-nonterminal-by-function
  (function streamorbuffer &optional search-parts search-includes)
  "Find all nonterminals in which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tokens
are searched.  The overloadable function `semantic-nonterminal-children' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches."
  (let ((streamlist (list
		     (if (bufferp streamorbuffer)
			 (save-excursion
			   (set-buffer streamorbuffer)
			   (semantic-bovinate-toplevel))
		       streamorbuffer)))
	(includes nil)			;list of includes
	(stream nil)			;current stream
	(sl nil)			;list of token children
	(nl nil))			;new list
    (if search-includes
	(setq includes (semantic-find-nonterminal-by-token
			'include (car streamlist))))
    (while streamlist
      (setq stream (car streamlist))
      (while stream
	(if (funcall function (car stream))
	    (setq nl (cons (car stream) nl)))
	(if search-parts
	    (progn
	      (setq sl (semantic-nonterminal-children
			(car stream)
			t
			))
	      (if sl
		  (setq nl (append nl (semantic-find-nonterminal-by-function
				       function sl
				       search-parts search-includes))))))
	;; next token
	(setq stream (cdr stream)))
      (setq streamlist (cdr streamlist)))
    (setq nl (nreverse nl))
;    (while includes
;      (setq nl (append nl (semantic-find-nonterminal-by-function
;			   
;			   ))))
    nl))

(defun semantic-find-nonterminal-by-function-first-match
  (function streamorbuffer &optional search-parts search-includes)
  "Find the first nonterminal which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.
If optional argument SEARCH-PARTS, all sub-parts of tokens are searched.
The overloadable function `semantic-nonterminal-children' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel))
		   streamorbuffer))
	(found nil))
    (while (and (not found) stream)
      (if (funcall function (car stream))
	  (setq found (car stream)))
      (setq stream (cdr stream)))
    found))

;;; Bucketizing: Take and convert the tokens based on type.
;;
(defun semantic-bucketize (tokens &optional filter)
  "Sort TOKENS into a group of buckets based on token type.
Unknown types are placed in a Misc bucket.
The buckets will be organized into a form usable by `semantic-sb-buttons'.
Optional argument FILTER is a filter function to be applied to each bucket.
The filter function will take one argument, which is a list of tokens, and
may re-organize the list with side-effects."
  (let ((bins (make-vector (1+ (length semantic-symbol->name-assoc-list)) nil))
	ask toktype
	(sn semantic-symbol->name-assoc-list)
	(nsn nil)
	(num 1)
	(out nil))
    ;; Build up the bucket vector
    (while sn
      (setq nsn (cons (cons (car (car sn)) num) nsn)
	    sn (cdr sn)
	    num (1+ num)))
    ;; Place into buckets
    (while tokens
      (setq toktype (semantic-token-token (car tokens))
	    ask (assq toktype nsn)
	    num (or (cdr ask) 0))
      (aset bins num (cons (car tokens) (aref bins num)))
      (setq tokens (cdr tokens)))
    ;; Remove from buckets into a list.
    (setq num 1)
    (while (< num (length bins))
      (when (aref bins num)
	(setq out
	      (cons (cons
		     (cdr (nth (1- num) semantic-symbol->name-assoc-list))
		     ;; Filtering, First hacked by David Ponce david@dponce.com
		     (funcall (or filter 'nreverse) (aref bins num)))
		    out)))
      (setq num (1+ num)))
    (if (aref bins 0)
	(setq out (cons (cons "Misc"
			      (funcall (or filter 'nreverse) (aref bins 0)))
			out)))
    (nreverse out)))

;; Some sorting functions
(defun semantic-string-lessp-ci (s1 s2)
  "Case insensitive version of `string-lessp'."
  ;; Use downcase instead of upcase because an average name
  ;; has more lower case characters.
  (string-lessp (downcase s1) (downcase s2)))

(defun semantic-sort-token-type (token)
  "Return a type string for TOKEN guaranteed to be a string."
  (let ((ty (semantic-token-type token)))
    (cond ((stringp ty)
	   ty)
	  ((listp ty)
	   (or (car ty) ""))
	  (t ""))))

(defun semantic-sort-tokens-by-name-increasing (tokens)
  "Sort TOKENS by name in increasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (string-lessp (semantic-token-name a)
			       (semantic-token-name b)))))

(defun semantic-sort-tokens-by-name-decreasing (tokens)
  "Sort TOKENS by name in decreasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (string-lessp (semantic-token-name b)
			       (semantic-token-name a)))))

(defun semantic-sort-tokens-by-type-increasing (tokens)
  "Sort TOKENS by type in increasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (string-lessp (semantic-sort-token-type a)
			       (semantic-sort-token-type b)))))

(defun semantic-sort-tokens-by-type-decreasing (tokens)
  "Sort TOKENS by type in decreasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (string-lessp (semantic-sort-token-type b)
			       (semantic-sort-token-type a)))))

(defun semantic-sort-tokens-by-name-increasing-ci (tokens)
  "Sort TOKENS by name in increasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (semantic-string-lessp-ci (semantic-token-name a)
					   (semantic-token-name b)))))

(defun semantic-sort-tokens-by-name-decreasing-ci (tokens)
  "Sort TOKENS by name in decreasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (semantic-string-lessp-ci (semantic-token-name b)
					   (semantic-token-name a)))))

(defun semantic-sort-tokens-by-type-increasing-ci (tokens)
  "Sort TOKENS by type in increasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (semantic-string-lessp-ci (semantic-sort-token-type a)
					   (semantic-sort-token-type b)))))

(defun semantic-sort-tokens-by-type-decreasing-ci (tokens)
  "Sort TOKENS by type in decreasing order with side effects.
Return the sorted list."
  (sort tokens (lambda (a b)
		 (semantic-string-lessp-ci (semantic-sort-token-type b)
					   (semantic-sort-token-type a)))))

;;; Recursive searching through dependency trees
;;
;; This will depend on the general searching APIS defined above.
;; but will add full recursion through the dependencies list per
;; stream.
(defun semantic-recursive-find-nonterminal-by-name (name buffer)
  "Recursivly find the first occurance of NAME.
Start search with BUFFER.  Recurse through all dependencies till found.
The return item is of the form (BUFFER TOKEN) where BUFFER is the buffer
in which TOKEN (the token found to match NAME) was found."
  (save-excursion
    (set-buffer buffer)
    (let* ((stream (semantic-bovinate-toplevel))
	   (includelist (or (semantic-find-nonterminal-by-token 'include stream)
			    "empty.silly.thing"))
	   (found (semantic-find-nonterminal-by-name name stream))
	   (unfound nil))
      (while (and (not found) includelist)
	(let ((fn (semantic-find-dependency (car includelist))))
	  (if (and fn (not (member fn unfound)))
	      (save-excursion
		(set-buffer (find-file-noselect fn))
		(message "Scanning %s" (buffer-file-name))
		(setq stream (semantic-bovinate-toplevel))
		(setq found (semantic-find-nonterminal-by-name name stream))
		(if found
		    (setq found (cons (current-buffer) (list found)))
		  (setq includelist
			(append includelist
				(semantic-find-nonterminal-by-token
				 'include stream))))
		(setq unfound (cons fn unfound)))))
	(setq includelist (cdr includelist)))
      found)))
  
;;; Completion APIs
;;
;; These functions provide minibuffer reading/completion for lists of
;; nonterminals.
(defvar semantic-read-symbol-history nil
  "History for a symbol read.")

(defun semantic-read-symbol (prompt &optional default stream filter)
  "Read a symbol name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from.
FILTER is provides a filter on the types of things to complete.
FILTER must be a function to call on each element."
  (if (not default) (setq default (thing-at-point 'symbol)))
  (if (not stream) (setq stream (semantic-bovinate-toplevel)))
  (setq stream
	(if filter
	    (semantic-find-nonterminal-by-function filter stream)
	  (semantic-find-nonterminal-standard stream)))
  (completing-read prompt stream nil t ""
		   'semantic-read-symbol-history))

(defun semantic-read-variable (prompt &optional default stream)
  "Read a variable name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'variable stream)))

(defun semantic-read-function (prompt &optional default stream)
  "Read a function name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'function stream)))

(defun semantic-read-type (prompt &optional default stream)
  "Read a type name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'type stream)))

;;; Behavioral APIs
;;
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.  This section permits the setting of override functions
;; for tasks of that nature, and also provides reasonable defaults.

(defvar semantic-override-table nil
  "Buffer local semantic function overrides obarray.
These overrides provide a hook for a `major-mode' to override specific
behaviors with respect to generated semantic toplevel nonterminals and
things that these non-terminals are useful for.  Use the function
`semantic-install-function-overrides' to install overrides.

Available override symbols:

  SYBMOL                  PARAMETERS         DESCRIPTION
 `find-dependency'        (token)            Find the dependency file
 `find-nonterminal'       (token & parent)   Find token in buffer.
 `find-documentation'     (token & nosnarf)  Find doc comments.
 `abbreviate-nonterminal' (token & parent)   Return summary string.
 `summarize-nonterminal'  (token & parent)   Return summary string.
 `prototype-nonterminal'  (token)            Return a prototype string.
 `concise-prototype-nonterminal' (token) Return a concise prototype string.
 `prototype-file'         (buffer)           Return a file in which
 	                                     prototypes are placed
 `nonterminal-children'   (token)            Return first rate children.
					     These are children which may
					     contain overlays.

  CONTEXT FUNCTIONS:
 `beginning-of-context'   (& point)          Move to the beginning of the
					     current context.
 `end-of-context'         (& point)          Move to the end of the
					     current context.
 `up-context'             (& point)          Move up one context level.
 `get-local-variables'    (& point)          Get local variables.
 `get-all-local-variables'(& point)          Get all local variables.
 `get-local-arguments'    (& point)          Get arguments to this function.

 `end-of-command'                            Move to the end of the current
                                             command
 `beginning-of-command'                      Move to the beginning of the
                                             current command
 `ctxt-current-symbol'    (& point)          List of related symbols.
 `ctxt-current-assignment'(& point)          Variable being assigned to.
 `ctxt-current-function'  (& point)          Function being called at point.
 `ctxt-current-argument'  (& point)          The index to the argument of
                                             the current function the cursor
                                             is in.

Parameters mean:

  &      - Following parameters are optional
  buffer - The buffer in which a token was found.
  token  - The nonterminal token we are doing stuff with
  parent - If a TOKEN is stripped (of positional infomration) then
           this will be the parent token which should have positional
           information in it.")
(make-variable-buffer-local 'semantic-override-table)

(defun semantic-install-function-overrides (overrides &optional transient)
  "Install function OVERRIDES in `semantic-override-table'.
If optional TRANSIENT is non-nil installed overrides can in turn be
overridden by next installation.  OVERRIDES must be an alist.  Each
element must be of the form: (SYM . FUN) where SYM is the symbol to
override, and FUN is the function to override it with."
  (if (not (arrayp semantic-override-table))
      (setq semantic-override-table (make-vector 13 nil)))
  (let (sym sym-name fun override)
    (while overrides
      (setq override  (car overrides)
            overrides (cdr overrides)
            sym-name  (symbol-name (car override))
            fun       (cdr override))
      (if (setq sym (intern-soft sym-name semantic-override-table))
          (if (get sym 'override)
              (set sym fun)
            (or (equal (symbol-value sym) fun)
                (message "Current `%s' function #'%s not overrode by #'%s"
                         sym (symbol-value sym) fun)))
        (setq sym (intern sym-name semantic-override-table))
        (set sym fun))
      (put sym 'override transient))))

(defun semantic-fetch-overload (sym)
  "Find and return the overload function for SYM.
Return nil if not found."
  (symbol-value
   (and (arrayp semantic-override-table)
        (intern-soft (symbol-name sym) semantic-override-table))))

(defvar semantic-dependency-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific to
the file being included.
This variable can also be set to a single function.  If it is a
function, it will be called with one arguments, the file to find as a
string, and  it should return the full path to that file, or nil.")
(make-variable-buffer-local `semantic-dependency-include-path)

(defun semantic-find-dependency (&optional token)
  "Find the filename represented from TOKEN.
TOKEN may be a stripped element, in which case PARENT specifies a
parent token that has positinal information.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths."
  (if (not token)
      (setq token (car (semantic-find-nonterminal-by-overlay nil))))

  (if (not (eq (semantic-token-token token) 'include))
      (signal 'wrong-type-argument (list token 'include)))

  ;; First, see if this file exists in the current EDE projecy
  (if (and (fboundp 'ede-expand-filename) ede-minor-mode
	   (ede-expand-filename (ede-toplevel)
				(semantic-token-name token)))
      (ede-expand-filename (ede-toplevel)
			   (semantic-token-name token))
  
    (let ((s (semantic-fetch-overload 'find-dependency)))
      (if s (funcall s token)
	(save-excursion
	  (set-buffer (semantic-token-buffer token))
	  (let ((name (semantic-token-name token)))
	    (cond ((file-exists-p name)
		   (expand-file-name name))
		  ((and (symbolp semantic-dependency-include-path)
			(fboundp semantic-dependency-include-path))
		   (funcall semantic-dependency-include-path name))
		  (t
		   (let ((p semantic-dependency-include-path)
			 (found nil))
		     (while (and p (not found))
		       (if (file-exists-p (concat (car p) "/" name))
			   (setq found (concat (car p) "/" name)))
		       (setq p (cdr p)))
		     found)))))))))

(defun semantic-find-nonterminal (&optional token parent)
  "Find the location of TOKEN.
TOKEN may be a stripped element, in which case PARENT specifies a
parent token that has position information.
Different behaviors are provided depending on the type of token.
For example, dependencies (includes) will seek out the file that is
depended on, and functions will move to the specified definition."
  (if (not token)
      (setq token (car (semantic-find-nonterminal-by-overlay nil))))
  (if (and (eq (semantic-token-token token) 'include)
	   (let ((f (semantic-find-dependency token)))
	     (if f (find-file f))))
      nil
    (let ((s (semantic-fetch-overload 'find-nonterminal)))
      (if s (funcall s token parent)
	(if (semantic-token-buffer token)
	    ;; If the token has no buffer, it may be deoverlayed.
	    ;; Assume the tool doing the finding knows that we came
	    ;; in from a database, and use the current buffer.
	    (set-buffer (semantic-token-buffer token)))
	(let ((start (semantic-token-start token)))
	  (if (numberp start)
	      ;; If it's a number, go there
	      (goto-char start)
	    ;; Otherwise, it's a trimmed vector, such as a parameter,
	    ;; or a structure part.
	    (if (not parent)
		nil
	      (goto-char (semantic-token-start parent))
	      ;; Here we make an assumtion that the text returned by
	      ;; the bovinator and concocted by us actually exists
	      ;; in the buffer.
	      (re-search-forward (semantic-token-name token) nil t))))))))

(defun semantic-find-documentation (&optional token nosnarf)
  "Find documentation from TOKEN and return it as a clean string.
TOKEN might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceeding TOKEN's definition which we
cal look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the flex token for it.
If nosnarf if 'flex, then only return the flex token."
  (if (not token)
      (setq token (car (semantic-find-nonterminal-by-overlay nil))))
  (let ((s (semantic-fetch-overload 'find-documentation)))
    (if s (funcall s token nosnarf)
      ;; No override.  Try something simple to find documentation nearby
      (save-excursion
	(set-buffer (semantic-token-buffer token))
	(semantic-find-nonterminal token)
	(or
	 ;; Is there doc in the token???
	 (if (semantic-token-docstring token)
	     (if (stringp (semantic-token-docstring token))
		 (semantic-token-docstring token)
	       (goto-char (semantic-token-docstring token))
	       (semantic-find-doc-snarf-comment nosnarf)))
	 ;; Check just before the definition.
	 (save-excursion
	   (re-search-backward comment-start-skip nil t)
	   (if (not (semantic-find-nonterminal-by-position
		     (point) (current-buffer) t))
	       ;; We found a comment that doesn't belong to the body
	       ;; of a function.
	       (semantic-find-doc-snarf-comment nosnarf)))
	 ;;  Lets look for comments either after the definition, but before code:
	 ;; Not sure yet.  Fill in something clever later....
	 nil
	 )))))

(defun semantic-find-doc-snarf-comment (nosnarf)
  "Snarf up the comment at POINT for `semantic-find-documentation'.
Attempt to strip out comment syntactic sugar.
Argument NOSNARF means don't modify the found text.
If NOSNARF is 'flex, then return the flex token."
  (let ((semantic-ignore-comments nil))
    (if (eq nosnarf 'flex)
	(car (semantic-flex (point) (1+ (point))))
      (let ((ct (semantic-flex-text
		 (car (semantic-flex (point) (1+ (point)))))))
	(if nosnarf
	    nil
	  ;; ok, try to clean the text up.
	  ;; Comment start thingy
	  (while (string-match (concat "^\\s-*" comment-start-skip) ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; Arbitrary punctuation at the beginning of each line.
	  (while (string-match "^\\s-*\\s.+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; End of a block comment.
	  (if (and block-comment-end (string-match block-comment-end ct))
	      (setq ct (concat (substring ct 0 (match-beginning 0))
			       (substring ct (match-end 0)))))
	  ;; In case it's a real string, STRIPIT.
	  (while (string-match "\\s-*\\s\"+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0))))))
	;; Now return the text.
	ct))))

(defun semantic-abbreviate-nonterminal (token &optional parent)
  "Return an abbreviated string describing TOKEN.
The abbreviation is to be short, with possible symbols indicating
the type of token, or other information.
Optional argument PARENT is the parent type if TOKEN is a detail."
  (let ((s (semantic-fetch-overload 'abbreviate-nonterminal))
	tt)
    (if s
	(funcall s token parent)
      (semantic-abbreviate-nonterminal-default token parent))))

(defun semantic-abbreviate-nonterminal-default (token &optional parent)
  "Return an abbreviated string describing TOKEN.
Optional argument PARENT is a parent token in the token hierarchy.
This is a simple C like default."
  ;; Do lots of complex stuff here.
  (let ((tok (semantic-token-token token))
	(name (semantic-token-name token))
	str)
    (setq str
	  (concat name
		  (cond ((eq tok 'function) "()")
			((eq tok 'include) "<>")
			((and (eq tok 'variable)
			      (semantic-token-variable-default token))
			 "=")
			)))
    (if parent
	(setq str
	      (concat (semantic-token-name parent)
		      (car semantic-type-relation-separator-character)
		      str)))
    str))

;; Semantic 1.2.x had this misspelling.  Keep it for backwards compatibiity.
(defalias 'semantic-summerize-nonterminal 'semantic-summarize-nonterminal)

(defun semantic-summarize-nonterminal (token &optional parent)
  "Summarize TOKEN in a reasonable way.
Optional argument PARENT is the parent type if TOKEN is a detail."
  (let ((s (semantic-fetch-overload 'summarize-nonterminal)))
    (if s
	(funcall s token parent)
      ;; FLESH THIS OUT MORE
      (concat (capitalize
	       (or (cdr-safe (assoc (semantic-token-token token)
				    semantic-symbol->name-assoc-list))
		   (symbol-name (semantic-token-token token))))
	      ": "
	      (semantic-prototype-nonterminal token)))))

(defun semantic-prototype-nonterminal (token)
  "Return a prototype for TOKEN.
This functin must be overloaded, though it need not be used."
  (let ((tt (semantic-token-token token))
	(s (semantic-fetch-overload 'prototype-nonterminal)))
    (if s
	;; Prototype is non-local
	(funcall s token)
      (semantic-prototype-nonterminal-default token))))

(defun semantic-prototype-nonterminal-default (token)
  "Default method for returning a prototype for TOKEN.
This will work for C like languages."
  (let* ((tok (semantic-token-token token))
	 (type (if (member tok '(function variable type))
		   (semantic-token-type token) ""))
	 (args (cond ((eq tok 'function)
		      (semantic-token-function-args token))
		     ((eq tok 'type)
		      (semantic-token-type-parts token))
		     (t nil)))
	 (const (semantic-token-extra-spec token 'const))
	 (mods (append
		(if const '("const") nil)
		(semantic-token-extra-spec token 'typemodifiers)))
	 (array (if (eq tok 'variable)
		    (let ((deref
			   (semantic-token-variable-extra-spec
			    token 'dereference))
			  (r ""))
		      (while (and deref (/= deref 0))
			(setq r (concat r "[]")
			      deref (1- deref)))
		      r)))
	 (suffix (if (eq tok 'variable)
		     (semantic-token-variable-extra-spec token 'suffix)))
	 )
    (if (and (listp mods) mods)
	(setq mods (concat (mapconcat (lambda (a) a) mods " ") " ")))
    (if args
	(setq args
	      (concat " " (if (eq tok 'type) "{" "(")
		      (if (stringp (car args))
			  (mapconcat (lambda (a) a) args ",")
			(mapconcat 'car args ","))
		      (if (eq tok 'type) "}" ")"))))
    (if (and type (listp type))
	(setq type (car type)))
    (concat (or mods "")
	    (if type (concat type " "))
	    (semantic-token-name token)
	    (or args "")
	    (or array ""))))

(defun semantic-concise-prototype-nonterminal (token)
  "Return a concise prototype for TOKEN.
This function must be overloaded, though it need not be used."
  (let ((s (semantic-fetch-overload 'concise-prototype-nonterminal)))
    (if s
	(funcall s token)
      (semantic-concise-prototype-nonterminal-default token))))

(defun semantic-concise-prototype-nonterminal-default (token)
  "Return a concise prototype for TOKEN.
This default cococt a cheap concise prototype using C like syntax."
  (let ((tok (semantic-token-token token)))
    (cond
     ((eq tok 'type)
      (concat (semantic-token-name token) "{}"))
     ((eq tok 'function)
      (let ((args (semantic-token-function-args token)))
        (concat (semantic-token-name token)
                "("
                (if args
                    (cond ((stringp (car args))
			   (mapconcat 'identity args ","))
			  ((semantic-token-p (car args))
			   (mapconcat
			    (lambda (a)
			      (let ((ty (semantic-token-type a)))
				(cond ((stringp ty)
				       ty)
				      ((semantic-token-p ty)
				       (semantic-prototype-nonterminal ty))
				      ((consp ty)
				       (car ty))
				      (t (error "Concice-prototype")))))
			    args ", "))
			  ((consp (car args))
			   (mapconcat 'car args ","))
			  (t (error "Concice-prototype")))
                  "")
                ")")))
     ((eq tok 'variable)
      (let* ((deref (semantic-token-variable-extra-spec
                     token 'dereference))
             (array "")
             (suffix (semantic-token-variable-extra-spec
                      token 'suffix)))
        (while (and deref (/= deref 0))
          (setq array (concat array "[]")
                deref (1- deref)))
        (concat (semantic-token-name token)
                array)))
     (t
      (semantic-abbreviate-nonterminal token)))))

(defun semantic-prototype-file (buffer)
  "Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overriden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in."
  (let ((s (semantic-fetch-overload 'prototype-file)))
    (if s
	(funcall s buffer)
      ;; Else, perform some default behaviors
      (if (and (fboundp 'ede-header-file) ede-minor-mode)
	  (save-excursion
	    (set-buffer buffer)
	    (ede-header-file))
	;; No EDE options for a quick answer.  Search.
	(save-excursion
	  (set-buffer buffer)
	  (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
	      (match-string 1)))))))

(defun semantic-nonterminal-children (token &optional positionalonly)
  "Return the list of top level children belonging to TOKEN.
Children are any sub-tokens which may contain overlays.
The default behavior (if not overriden with `nonterminal-children'
is to return type parts for a type, and arguments for a function.

If optional argument POSITIONALONLY is non-nil, then only return valid
children if they contain positions.  Some languages may choose to create
lists of children without position/overlay information.

If this function is overriden, use `semantic-nonterminal-children-default'
to also include the default behavior, and merely extend your own.

Note for language authors:
  If a mode defines a language that has tokens in it with overlays that
should not be considered children, you should still return them with
this function."
  (let* ((s (semantic-fetch-overload 'nonterminal-children))
	 (chil (if s (funcall s token)
		 (semantic-nonterminal-children-default token))))
    (if (or (not positionalonly)
	    (semantic-token-with-position-p (car chil)))
	chil
      nil)))

(defun semantic-nonterminal-children-default (token)
  "Return the children of TOKEN.
For types, return the type parts.
For functions return the argument list."
  (cond ((eq (semantic-token-token token) 'type)
	 (semantic-token-type-parts token))
	((eq (semantic-token-token token) 'function)
	 (semantic-token-function-args token))
	(t nil)))

;;; Do some fancy stuff with overlays
;;
(defun semantic-highlight-token (token &optional face)
  "Specify that TOKEN should be highlighted.
Optional FACE specifies the face to use."
  (let ((o (semantic-token-overlay token)))
    (semantic-overlay-put o 'old-face
			  (cons (semantic-overlay-get o 'face)
				(semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'face (or face 'highlight))
    ))

(defun semantic-unhighlight-token (token)
  "Unhighlight TOKEN, restoring it's previous face."
  (let ((o (semantic-token-overlay token)))
    (semantic-overlay-put o 'face (car (semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'old-face (cdr (semantic-overlay-get o 'old-face)))
    ))

(defun semantic-momentary-unhighlight-token (token)
  "Unhighlight TOKEN, restoring it's previous face."
  (semantic-unhighlight-token token)
  (remove-hook 'pre-command-hook
	       `(lambda () (semantic-momentary-unhighlight-token ',token))))

(defun semantic-momentary-highlight-token (token &optional face)
  "Highlight TOKEN, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used."
  (semantic-highlight-token token face)
  (add-hook 'pre-command-hook
	    `(lambda () (semantic-momentary-unhighlight-token ',token))))

(defun semantic-set-token-face (token face)
  "Specify that TOKEN should use FACE for display."
  (semantic-overlay-put (semantic-token-overlay token) 'face face))

(defun semantic-set-token-invisible (token &optional visible)
  "Enable the text in TOKEN to be made invisible.
If VISIBLE is non-nil, make the text visible."
  (semantic-overlay-put (semantic-token-overlay token) 'invisible
			(not visible)))

(defun semantic-token-invisible-p (token)
  "Return non-nil if TOKEN is invisible."
  (semantic-overlay-get (semantic-token-overlay token) 'invisible))

(defun semantic-set-token-intangible (token &optional tangible)
  "Enable the text in TOKEN to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-put (semantic-token-overlay token) 'intangible
			(not tangible)))

(defun semantic-token-intangible-p (token)
  "Return non-nil if TOKEN is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-get (semantic-token-overlay token) 'intangible))

(defun semantic-overlay-signal-read-only
  (overlay after start end &optional len)
  "Hook used in modification hooks to prevent modification.
Allows deletion of the entire text.
Argument OVERLAY, AFTER, START, END, and LEN are passed in by the system."
  ;; Stolen blithly from cpp.el in Emacs 21.1
  (if (and (not after)
	   (or (< (semantic-overlay-start overlay) start)
	       (> (semantic-overlay-end overlay) end)))
      (error "This text is read only")))

(defun semantic-set-token-read-only (token &optional writable)
  "Enable the text in TOKEN to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable.
instead of read-only."
  (let ((o (semantic-token-overlay token))
	(hook (if writable nil '(semantic-overlay-signal-read-only))))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-put o 'read-only (not writable))
      (semantic-overlay-put o 'modification-hooks hook)
      (semantic-overlay-put o 'insert-in-front-hooks hook)
      (semantic-overlay-put o 'insert-behind-hooks hook))))

(defun semantic-token-read-only-p (token)
  "Return non-nil if the current TOKEN is marked read only."
  (let ((o (semantic-token-overlay token)))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-get o 'read-only)
      (member 'semantic-overlay-signal-read-only
              (semantic-overlay-get o 'modification-hooks)))))

(defun semantic-narrow-to-token (token)
  "Narrow to the region specified by TOKEN."
  (narrow-to-region (semantic-token-start token)
		    (semantic-token-end token)))

;;; Force token lists in and out of overlay mode.
;;
(defun semantic-deoverlay-token (token)
  "Convert TOKEN from using an overlay to using an overlay proxy."
  (when (semantic-token-p token)
    (let ((c (semantic-token-overlay-cdr token))
	  a)
      (when (and c (semantic-overlay-p (car c)))
	(setq a (vector (semantic-overlay-start (car c))
			(semantic-overlay-end (car c))))
	(semantic-overlay-delete (car c))
	(setcar c a)
	;; Fix the children of this token
	(semantic-deoverlay-list (semantic-nonterminal-children token)))
      )))

(defun semantic-overlay-token (token)
  "Convert TOKEN from using an overlay proxy to using an overlay."
  (when (semantic-token-p token)
    (let ((c (semantic-token-overlay-cdr token))
	  o)
      (when (and c (vectorp (car c)) (= (length (car c)) 2))
	(setq o (semantic-make-overlay (aref (car c) 0)
				       (aref (car c) 1)
				       (current-buffer)))
	(setcar c o)
	(semantic-overlay-put o 'semantic token)
	;; Fix overlays in children of this token
	(semantic-overlay-list (semantic-nonterminal-children token))
	))))

(defun semantic-deoverlay-list (l)
  "Remove overlays from the list L."
  (mapcar 'semantic-deoverlay-token l))

(defun semantic-overlay-list (l)
  "Convert numbers to  overlays from the list L."
  (mapcar 'semantic-overlay-token l))

(defun semantic-deoverlay-cache ()
  "Convert all tokens in the current cache to use overlay proxies."
  (semantic-deoverlay-list (semantic-bovinate-toplevel)))

(defun semantic-overlay-cache ()
  "Convert all tokens in the current cache to use overlays."
  (condition-case nil
      ;; In this unique case, we cannot call the usual toplevel fn.
      ;; because we don't want a reparse, we want the old overlays.
      (semantic-overlay-list semantic-toplevel-bovine-cache)
    ;; Recover when there is an error restoring the cache.
    (error (message "Error recovering token list.")
	   (semantic-clear-toplevel-cache)
	   nil)))

;;; Interactive Functions for bovination
;;
(eval-when-compile
  (condition-case nil (require 'pp) (error nil)))

(defun bovinate (&optional clear)
  "Bovinate the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before bovinating."
  (interactive "P")
  (if clear (semantic-clear-toplevel-cache))
  (let ((out (semantic-bovinate-toplevel t)))
    (pop-to-buffer "*BOVINATE*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string out))
    (goto-char (point-min))))

(defun bovinate-debug ()
  "Bovinate the current buffer and run in debug mode."
  (interactive)
  (let ((semantic-edebug t)
	(out (semantic-bovinate-debug-buffer)))
    (pop-to-buffer "*BOVINATE*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string out))))

(defun semantic-describe-token (&optional token)
  "Describe TOKEN in the minibuffer.
If TOKEN is nil, describe the token under the cursor."
  (interactive)
  (if (not token) (setq token (semantic-current-nonterminal)))
  (semantic-bovinate-toplevel t)
  (if token (message (semantic-summarize-nonterminal token))))


;;; Putting keys on tokens.
;;
(defun semantic-add-label (label value &optional token)
  "Add a LABEL with VALUE on TOKEN.
If TOKEN is not specified, use the token at point."
  (interactive "sLabel: \nXValue (eval): ")
  (if (not token)
      (progn
	(semantic-bovinate-toplevel t)
	(setq token (semantic-current-nonterminal))))
  (semantic-token-put token (intern label) value)
  (message "Added label %s with value %S" label value))

(defun semantic-show-label (label &optional token)
  "Show the value of LABEL on TOKEN.
If TOKEN is not specified, use the token at point."
  (interactive "sLabel: ")
  (if (not token)
      (progn
	(semantic-bovinate-toplevel t)
	(setq token (semantic-current-nonterminal))))
  (message "%s: %S" label (semantic-token-get token (intern label))))


;;; Show dirty mode
;;
;;;###autoload
(defcustom semantic-show-dirty-mode nil
  "*If non-nil enable the use of `semantic-show-dirty-mode'."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (semantic-show-dirty-mode (if val 1 -1))
         (custom-set-default sym val)))

(defface semantic-dirty-token-face  '((((class color) (background dark))
				       (:background "gray10"))
				      (((class color) (background light))
				       (:background "gray90")))
  "Face used to show dirty tokens in `semantic-show-dirty-token-mode'."
  :group 'semantic)

(defun semantic-show-dirty-token-hook-fcn (token start end)
  "Function set into `semantic-dirty-token-hooks'.
This will highlight TOKEN as dirty.
START and END define the region changed, but are not used."
  (semantic-highlight-token token 'semantic-dirty-token-face))

(defun semantic-show-clean-token-hook-fcn (token)
  "Function set into `semantic-clean-token-hooks'.
This will unhighlight TOKEN from being dirty."
  (semantic-unhighlight-token token))

(defun semantic-show-dirty-mode (&optional arg)
  "Enable the display of dirty tokens.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
    (interactive "P")
  (if (not arg)
      (if (member #'semantic-show-dirty-token-hook-fcn
		  semantic-dirty-token-hooks)
	  (setq arg -1)
	(setq arg 1)))
  (if (< arg 0)
      (progn
	;; Remove hooks
	(remove-hook 'semantic-dirty-token-hooks 'semantic-show-dirty-token-hook-fcn)
	(remove-hook 'semantic-clean-token-hooks 'semantic-show-clean-token-hook-fcn)
	)
    (add-hook 'semantic-dirty-token-hooks 'semantic-show-dirty-token-hook-fcn)
    (add-hook 'semantic-clean-token-hooks 'semantic-show-clean-token-hook-fcn)
    ))

;;; Hacks
;;
;; Some hacks to help me test these functions
(defun semantic-current-token (p)
  "Display the curent token.
Argument P is the point to search from in the current buffer."
  (interactive "d")
  (let ((tok (semantic-find-innermost-nonterminal-by-position
	      p (current-buffer))))
    (message (mapconcat 'semantic-abbreviate-nonterminal tok ","))
    (car tok))
  )

(defun semantic-hack-search ()
  "Disply info about something under the cursor using generic methods."
  (interactive)
  (let (
	;(name (thing-at-point 'symbol))
	(strm (cdr (semantic-bovinate-toplevel)))
	(res nil))
;    (if name
	(setq res
;	      (semantic-find-nonterminal-by-name name strm)
;	      (semantic-find-nonterminal-by-type name strm)
;	      (semantic-recursive-find-nonterminal-by-name name (current-buffer))
	      (semantic-find-nonterminal-by-position (point) strm)
	      
	      )
;	)
    (if res
	(progn
	  (pop-to-buffer "*SEMANTIC HACK RESULTS*")
	  (require 'pp)
	  (erase-buffer)
	  (insert (pp-to-string res) "\n")
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer))
      (message "nil"))))

(provide 'semantic-util)

;;; semantic-util.el ends here
