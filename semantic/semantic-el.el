;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-el.el,v 1.38 2001/01/24 21:17:38 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-ex is free software; you can redistribute it and/or modify
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
;; Use the Semantic Bovinator for Emacs Lisp

(require 'semantic)
(require 'backquote)

;;; Code:
(defvar semantic-toplevel-elisp-bovine-table
  `((bovine-toplevel
     (semantic-list
      ,(lambda (vals start end)
	 (append (semantic-elisp-use-read (car vals))
		 (list start end)))))
    )
  "Top level bovination table for elisp.")

(defun semantic-elisp-desymbolify (arglist)
  "Convert symbols to strings for ARGLIST."
  (let ((out nil))
    (while arglist
      (setq out
	    (cons
	     (if (symbolp (car arglist))
		 (symbol-name (car arglist))
	       (if (and (listp (car arglist))
			(symbolp (car (car arglist))))
		   (symbol-name (car (car arglist)))
		 (format "%S" (car arglist))))
	     out)
	    arglist (cdr arglist)))
    (nreverse out)))

(defun semantic-elisp-use-read (sl)
  "Use `read' on the semantic list SL.
Return a bovination list to use."
  (let* ((rt (read (buffer-substring (car sl) (cdr sl)))) ; read text
	 (ts (car rt)) ; type symbol
	 (ss (if (listp (nth 1 rt))
		 (nth 1 (nth 1 rt))
		(nth 1 rt))) ; symbol name
	 (sn (format "%S" ss))
	 )
    (cond
     ((or (eq ts 'defvar)
	  (eq ts 'defconst)
	  (eq ts 'defcustom)
	  (eq ts 'defface)
	  (eq ts 'defimage))
      ;; Variables and constants
      (list sn 'variable nil (nth 2 rt)
	    (semantic-bovinate-make-assoc-list
	     'const (if (eq ts 'defconst) t nil))
	    (nth 3 rt))
      )
     ((or (eq ts 'defun)
	  (eq ts 'defsubst)
	  (eq ts 'defmacro))
      ;; functions and macros
      (list sn 'function nil (semantic-elisp-desymbolify (nth 2 rt))
	    nil (nth 3 rt))
      )
     ((or (eq ts 'defmethod)
	  (eq ts 'defgeneric))
      ;; methods
      (let* ((a2 (nth 2 rt))
	     (a3 (nth 3 rt))
	     (args (if (listp a2) a2 a3))
	     (doc (nth (if (listp a2) 3 4) rt)))
	(list sn 'function nil
	      (if (listp (car args))
		  (cons (symbol-name (car (car args)))
			(semantic-elisp-desymbolify (cdr args)))
		(semantic-elisp-desymbolify (cdr args)))
	      (semantic-bovinate-make-assoc-list
	       'parent (symbol-name
			(if (listp (car args)) (car (cdr (car args))))))
	      doc)
	))
     ((eq ts 'defadvice)
      ;; Advice
      (list sn 'function nil (semantic-elisp-desymbolify (nth 2 rt))
	    nil (nth 3 rt)))
     ((eq ts 'defclass)
      ;; classes
      (let ((docpart (nth 4 rt)))
	(list sn 'type "class" (semantic-elisp-desymbolify (nth 3 rt))
	      (semantic-elisp-desymbolify (nth 2 rt))
	      (semantic-bovinate-make-assoc-list
	       'typemodifiers
	       (semantic-elisp-desymbolify
		(if (not (stringp docpart))
		    docpart))
	       )
	      (if (stringp docpart)
		  docpart
		(car (cdr (member :documentation docpart))))))
      )
     ((eq ts 'defstruct)
      ;; structs
      (list sn 'type "struct" (semantic-elisp-desymbolify (nth 3 rt))
	    (semantic-elisp-desymbolify (nth 2 rt))
	    nil (nth 4 rt))
      )
     ;; Now for other stuff
     ((eq ts 'require)
      (list sn 'include nil nil))
     ((eq ts 'provide)
      (list sn 'package (nth 3 rt) nil))
     (t
      ;; Other stuff
      (list (symbol-name ts) 'code)
      ))))

(defun semantic-elisp-find-dependency (token)
  "Find the file BUFFER depends on described by TOKEN."
  (let ((f (file-name-sans-extension
	    (locate-library (semantic-token-name token)))))
    (concat f ".el")))

(defun semantic-default-elisp-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  (setq semantic-toplevel-bovine-table semantic-toplevel-elisp-bovine-table
	semantic-override-table
	'((find-dependency . semantic-elisp-find-dependency))
	semantic-symbol->name-assoc-list
	'( (variable . "Variables")
	   (type     . "Types")
	   (function . "Defuns")
	   (include  . "Requires")
	   (package  . "Provides"))
	imenu-create-index-function 'semantic-create-imenu-index
	))

(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

(provide 'semantic-el)

;;; semantic-el.el ends here
