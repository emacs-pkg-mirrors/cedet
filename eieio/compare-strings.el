;;; compare-strings.el --- Compare Strings (copied from Emacs 21)

;;;
;; Copyright (C) 2001 David Ponce
;; C implementation Copyright (C) Free Software Foundation
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.

;;; Comentary
;;
;; Xemacs does not have this function.  Implemented from Emacs 21
;; built-in compare-strings code in src/fns.c.
;;

(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, converting to multibyte if needed.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.
Unibyte strings are converted to multibyte for comparison.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning."
  (if (null start1)
      (setq start1 0))
  (if (null start2)
      (setq start2 0))
  (setq end1 (if end1
		 (min end1 (length str1))
	       (length str1)))
  (setq end2 (if end2
		 (min end2 (length str2))
	       (length str2)))
  (let ((i1 start1)
	(i2 start2)
	result
	c1 c2 i1 i2)
    (while (and (not result) (< i1 end1) (< i2 end2))
      (setq c1 (aref str1 i1)
	    c2 (aref str2 i2)
	    i1 (1+ i1)
	    i2 (1+ i2))
      (if ignore-case
	  (setq c1 (upcase c1)
		c2 (upcase c2)))
      (cond ((< c1 c2)
	     (setq result (- i1)))
	    ((> c1 c2)
	     (setq result i1))))

    (if (null result)
	(setq result
	      (cond ((< i1 end1)
		     (1+ (- i1 start1)))
		    ((< i2 end2)
		     (1- (- start1 i1)))
		    (t
		     t))))
    result))

