;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: goofy
;; X-RCS: $Id: semantic-el.el,v 1.1 1999/05/03 18:06:27 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Use the semantic bovinator in a couple languages as examples.
;;
;; <Add more here>

;;; History:
;; 

(require 'semantic)

;;; Code:
(defvar semantic-toplevel-elisp-bovine-table
  '((bovine-toplevel
     (semantic-list (lambda (vals start end)
		      (semantic-bovinate-from-synthetic
		       start end 'extract-toplevel)))
     (extract-toplevel))
    ;; When parsing at depth 0, we need to extract elements from semantic
    ;; lists at bovine-toplevel.  This symbol provides the needed redirection.
    (extract-toplevel
     (function) (variable) (include) (comment))
    ;; A function is anything that starts with a (defun
    (function
     (open-paren symbol "defun" symbol arg-list string
		 (lambda (vals start end)
		   (list 'function (nth 2 vals) nil (nth 3 vals) nil;(nth 4 vals)
			 start end))))
    ;; A variable can be a defvar or defconst.
    (variable
     (open-paren symbol "defvar\\|defconst" symbol expression string
		 (lambda (vals start end)
		   (list 'variable (nth 2 vals) nil
			 (if (string= (nth 1 vals) "defconst") t nil)
			 nil;(nth 4 vals)
			 start end))))
    ;; In elisp, and include is just the require statement.
    (include
     (open-paren symbol "require" quote symbol
		 (lambda (vals start end)
		   (list 'include (nth 3 vals) nil start end))))
    ;; Quotes are oft optional in some cases
    (quote (punctuation "'"))
    ;; Something that can be evaluated out to something.
    (expression
     (quote expression (lambda (vals start end)
			 (list (car (cdr vals)) start end)))
     (semantic-list) (symbol) (string))
    ;; An argument list to a function
    (arg-list
     (semantic-list (lambda (vals start end)
		      (semantic-bovinate-from-synthetic start end 'argsyms)
		      ))
     ;; If it's already opened, what to do??
     )
    ;; This guys is some number of argument symbols...
    (argsyms
     (open-paren argsyms (lambda (vals start end)
			   (append (car (cdr vals)) (list start end))))
     (symbol argsyms (lambda (vals start end)
		       (append (cons (car vals) (car (cdr vals)))
			       (list start end))))
     (symbol close-paren (lambda (vals start end)
			   (list (car vals) start end))))
    )
  "Top level bovination table for elisp.")

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq semantic-toplevel-bovine-table
		  semantic-toplevel-elisp-bovine-table)))

(defun semantic-bovinate-interactive ()
  "Bovinate the toplevel of this buffer, and dis play the results."
  (interactive)
  (message "%S" (semantic-bovinate-toplevel nil t)))

(defun semantic-bovinate-elisp-functions-old ()
  "Demo function which return a list of functions, and thier arguments."
  (interactive)
  (let ((ss (semantic-flex (point-min) (point-max) 0))
	(tlss nil)
	(tls nil)			;top level returned spec
	(ts nil))			;temp spec
    (while ss
      (if (eq (car (car ss)) 'semantic-list)
	  (progn
	    (setq tlss (semantic-flex-list (car ss) 1)
		  ts (semantic-bovinate-synthetic
		      tlss semantic-toplevel-elisp-bovine-table))
	    (if (and ts (car (cdr ts)))
		(setq tls (cons (car (cdr ts)) tls)))))
      (setq ss (cdr ss)))
    (message "%S" (nreverse tls))))
