;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-el.el,v 1.28 2000/06/11 18:48:25 zappo Exp $

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
	   (setq i (append (nreverse (cdr (cdr (reverse i))))
			   (list start end))))))
     (extract-toplevel))
    ;; When parsing at depth 0, we need to extract elements from semantic
    ;; lists at bovine-toplevel.  This symbol provides the needed redirection.
    (extract-toplevel
     (function)
     (variable)
     (include)
     (package)
     (code)
     (comment) )
    ;; A function is anything that starts with a (defun
    (function
     (open-paren symbol "defun\\|defmacro" symbol arg-list doc-string
		 ,(lambda (vals start end)
		    (list (nth 2 vals) 'function nil (nth 3 vals) nil
			  (car-safe (nth 4 vals))
			  start end))))
    ;; A variable can be a defvar or defconst.
    (variable
     (open-paren symbol "defvar\\|defconst\\|defcustom\\|defface\\|defimage"
		 symbol expression doc-string
		 ,(lambda (vals start end)
		    (list (nth 2 vals) 'variable nil
			  (if (string= (nth 1 vals) "defconst") t nil)
			  nil nil (car-safe (nth 4 vals))
			  start end))))
    ;; In elisp, an include is just the require statement.
    (include
     (open-paren symbol "require" quote symbol
		 ,(lambda (vals start end)
		    (list (nth 3 vals) 'include nil start end))))
    ;; in elisp, a package statement is the same as the provide token.
    (package
     (open-paren symbol "provide" quote symbol opt-filestring close-paren
		 ,(lambda (vals start end)
		    (list (nth 3 vals) 'package (nth 4 vals)
			  nil start end))))
    (opt-filestring
     (string)
     ( ,(lambda (vals start end) (list nil))))
    ;; Some random code stuck in there.
    (code
     (open-paren symbol
		 ,(lambda (vals start end)
		    (let ((sym (if (nth 1 vals) (intern-soft (nth 1 vals)))))
		      (if (and sym (fboundp sym))
			  (list (nth 1 vals) 'code start end)
			)))))
    ;; Doc strings are sometimes optional, and always just return the
    ;; start position.
    (doc-string
     (string ,(lambda (vals start end) (list start start end)))
     (comment ,(lambda (vals start end) (list start start end)))
     ())
    ;; Quotes are oft optional in some cases
    (quote (punctuation "'"))
    ;; Something that can be evaluated out to something.
    (expression
     (quote expression ,(lambda (vals start end)
			  (list (car (cdr vals)) start end)))
     (semantic-list) (symbol) (string))
    ;; An argument list to a function
    (arg-list
     (symbol "nil" ,(lambda (vals start end) (list nil)))
     (semantic-list ,(lambda (vals start end)
		       (semantic-bovinate-from-nonterminal start end 'argsyms)
		       ))
     ;; If it's already opened, what to do??
     )
    ;; This guys is some number of argument symbols...
    (argsyms
     (open-paren close-paren ,(lambda (vals start end)
				(list nil start end)))
     (open-paren argsyms ,(lambda (vals start end)
			    (append (car (cdr vals)) (list start end))))
     (symbol argsyms ,(lambda (vals start end)
			(append (cons (car vals) (car (cdr vals)))
				(list start end))))
     (symbol close-paren ,(lambda (vals start end)
			    (list (car vals) start end))))
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
	   (function . "Defuns")
	   (include . "Requires")
	   (package . "Provides"))
	))

(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

(provide 'semantic-el)

;;; semantic-el.el ends here
