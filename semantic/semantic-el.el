;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-el.el,v 1.33 2000/09/27 01:00:00 zappo Exp $

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
	 (let ((i (semantic-bovinate-from-nonterminal
		   start end 'extract-toplevel)))
	   (append (nreverse (cdr (cdr (reverse i))))
		   (list start end)))))
     (extract-toplevel))
    ;; When parsing at depth 0, we need to extract elements from semantic
    ;; lists at bovine-toplevel.  This symbol provides the needed redirection.
    (extract-toplevel
     (function)
     (variable)
     (type)
     (include)
     (package)
     (method)
     (advice)
     (code)
     (comment) )
    ;; A type is defined by extended tools like CL, or EIEIO
    (type
     (open-paren symbol "defclass" symbol arg-list
		 field-list doc-string
		 ,(semantic-lambda
		   (list (nth 2 vals) 'type
			 "class"
			 (nth 4 vals) (nth 3 vals) nil
			 (car-safe (nth 5 vals))))))
    ;; A function is anything that starts with a (defun
    (function
     (open-paren symbol "defun\\|defmacro" symbol arg-list doc-string
		 ,(semantic-lambda
		    (list (nth 2 vals) 'function nil (nth 3 vals) nil
			  (car-safe (nth 4 vals))))))
    (method
     (open-paren symbol "defmethod\\|defgeneric" symbol opt-label arg-list
		 doc-string
		 ,(semantic-lambda
		    (list (nth 2 vals) 'function nil (nth 4 vals) nil
			  (car-safe (nth 5 vals))))))
    (advice
     (open-paren symbol "defadvice" symbol arg-list
		 doc-string
		 ,(semantic-lambda
		   (list (nth 2 vals) 'function nil (nth 3 vals) nil
			 (car-safe (nth 4 vals))))))
    ;; A variable can be a defvar or defconst.
    (variable
     (open-paren symbol "defvar\\|defconst\\|defcustom\\|defface\\|defimage"
		 symbol expression doc-string
		 ,(semantic-lambda
		    (list (nth 2 vals) 'variable nil
			  (if (string= (nth 1 vals) "defconst") t nil)
			  nil nil (car-safe (nth 4 vals))))))
    ;; In elisp, an include is just the require statement.
    (include
     (open-paren symbol "require" quote symbol
		 ,(semantic-lambda
		    (list (nth 3 vals) 'include nil nil))))
    ;; in elisp, a package statement is the same as the provide token.
    (package
     (open-paren symbol "provide" quote symbol opt-filestring close-paren
		 ,(semantic-lambda
		    (list (nth 3 vals) 'package (nth 4 vals) nil))))
    (opt-filestring
     (string)
     ( ,(lambda (vals start end) (list nil))))
    ;; Some random code stuck in there.
    (code
     (open-paren symbol
		 ,(semantic-lambda
		    (let ((sym (if (nth 1 vals) (intern-soft (nth 1 vals)))))
		      (if (and sym (fboundp sym))
			  (list (nth 1 vals) 'code))))))
    ;; Doc strings are sometimes optional, and always just return the
    ;; start position.
    (doc-string
     (string ,(lambda (vals start end) (list start start end)))
     (comment ,(lambda  (vals start end) (list start start end)))
     ())
    ;; Quotes are oft optional in some cases
    (quote (punctuation "'"))
    ;; Backquotes are also optional for macro type thingies
    (backquote (punctuation "`"))
    ;; Something that can be evaluated out to something.
    (expression
     (quote expression ,(semantic-lambda (list (car (cdr vals)))))
     (backquote expression ,(semantic-lambda (list (car (cdr vals)))))
     (semantic-list) (symbol) (string))
    ;; An argument list to a function
    (arg-list
     (symbol "nil" ,(lambda (vals start end) (list nil)))
     (semantic-list ,(lambda (vals start end)
		       (semantic-bovinate-from-nonterminal start end 'argsyms)
		       ))
     ;; If it's already opened, what to do??
     )
    (argsyms
     (open-paren close-paren ,(semantic-lambda
				(list nil)))
     (open-paren argsyms ,(semantic-lambda (car (cdr vals))))
     (symbol argsyms ,(semantic-lambda
			(append (cons (car vals) (car (cdr vals))))))
     (semantic-list argsyms
		    ,(semantic-lambda
		      (let ((e (read (buffer-substring (car (nth 0 vals))
						      (cdr (nth 0 vals))))))
			(cons (symbol-name (car e))
			      (car (cdr vals))))))
     (symbol close-paren ,(semantic-lambda (list (car vals))))
     (semantic-list close-paren
		    ,(semantic-lambda
		      (let ((e (read (buffer-substring (car (nth 0 vals))
						       (cdr (nth 0 vals))))))
			(list (symbol-name (car e)))))))
    ;; This guys is some number of argument symbols...
    (field-list
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal-full start end 'fieldsyms)
	 )))
    (fieldsyms
     (semantic-list ,(semantic-lambda
		      (let ((e (read (buffer-substring (car (nth 0 vals))
						       (cdr (nth 0 vals))))))
			(list (symbol-name (car e))))))
     )
    ;; Labels
    (opt-label
     (symbol "^:" ,(semantic-lambda (car vals)))
     ())
    )
  "Top level bovination table for elisp.")

(defun semantic-elisp-find-dependency (buffer token)
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
