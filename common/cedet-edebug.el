;;; cedet-edebug.el --- Special EDEBUG augmentation code

;;;
;; Copyright (C) 2003 Eric M. Ludlam
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
;;
;; Please send bug reports, etc. to zappo@gnu.org


;;; History:
;; 

;;; Commentary:
;;
;; Some aspects of EDEBUG are not extensible.  It is possible to extend
;; edebug through other means, such as alias or advice, but those don't stack
;; very well when there are multiple tools trying to do the same sort of thing.
;;
;; This package provides a way to extend some aspects of edebug, such as value
;; printing.

;; Default extensions are for EIEIO.
(require 'eieio)
(require 'semantic-tag)

(defvar cedet-edebug-prin1-extensions
  '(
    ;; EIEIO extensions
    ( (class-p object) . (class-name object) )
    ( (object-p object) . (object-print object) )
    ( (and (listp object) (or (class-p (car object)) (object-p (car object)))) .
      (cedet-edebug-prin1-recurse object) )
    ;; Semantic TAG datatype extensions
    ( (semantic-tag-p object) . (concat "#<TAG " (semantic-format-tag-name object) ">") )
    ( (and (listp object) (semantic-tag-p (car object))) .
      (cedet-edebug-prin1-recurse object) )
    ;; Semantic DB find result datatype
    ( (semanticdb-find-results-p object) .
      (semanticdb-find-result-prin1-to-string object) )
    )
  "An alist of of code that can extend PRIN1 for edebug.
Each entry has the value: (CONDITION . PRIN1COMMAND).")

(defun cedet-edebug-prin1-recurse (object)
  "Recurse into OBJECT for prin1 on `cedet-edebug-prin1-to-string'."
  (concat "(" (mapconcat 'cedet-edebug-prin1-to-string object " ") ")"))

(defun cedet-edebug-rebuild-prin1 ()
  "Rebuild the function `cedet-edebug-prin1-to-string'.
Use the values of `cedet-edebug-prin1-extensions' as the means of
constructing the function."
  (interactive)
  (let ((c cedet-edebug-prin1-extensions)
	(code nil))
    (while c
      (setq code (append (list (list (car (car c))
				     (cdr (car c))))
			 code))
      (setq c (cdr c)))
    (fset 'cedet-edebug-prin1-to-string
	  `(lambda (object &optional noescape)
	     "Display eieio OBJECT in fancy format.  Overrides the edebug default.
Optional argument NOESCAPE is passed to `prin1-to-string' when appropriate."
	     (cond
	      ,@(nreverse code)
	      (t (prin1-to-string object noescape)))))
    ))

;;; NOTE TO SELF.  Make this system used as an extension
;;; and then autoload the below.
;;;###NOT autoload
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (require 'cedet-edebug)
	    ;; I suspect this isn't the best way to do this, but when
	    ;; cust-print was used on my system all my objects
	    ;; appeared as "#1 =" which was not useful.  This allows
	    ;; edebug to print my objects in the nice way they were
	    ;; meant to with `object-print' and `class-name'
	    (defalias 'edebug-prin1-to-string 'cedet-edebug-prin1-to-string)
	    ))

(provide 'cedet-edebug)

;;; cedet-edebug.el ends here
