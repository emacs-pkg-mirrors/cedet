;;; semantic-scm.el --- Semantic details for Scheme (guile)

;;; Copyright (C) 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-scm.el,v 1.6 2003/03/15 19:30:29 zappo Exp $

;; This program is free software; you can redistribute it and/or modify
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
;; Use the Semantic Bovinator for Scheme (guile)

(require 'semantic)
(require 'backquote)

(eval-when-compile
  (require 'document))

;;; Code:
(defvar semantic-toplevel-scheme-bovine-table
  ;;DO NOT EDIT! Generated from scheme.by - 2003-03-13 20:18-0500
  `(
    (bovine-toplevel ;;scheme
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'scheme-list))
      )
     ) ;; end scheme

    (scheme-list
     (open-paren
      "("
      scheme-in-list
      close-paren
      ")"
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end scheme-list

    (scheme-in-list
     (DEFINE
       symbol
       expression
       ,(semantic-lambda
	 (semantic-tag-new-variable
	  (nth 1 vals) nil
	  (nth 2 vals)))
       )
     (DEFINE
       name-args
       opt-doc
       sequence
       ,(semantic-lambda
	 (semantic-tag-new-function
	  (car
	   (nth 1 vals)) nil
	  (cdr
	   (nth 1 vals))))
       )
     (DEFINE-MODULE
       name-args
       ,(semantic-lambda
	 (semantic-tag-new-package
	  (nth
	   (length
	    (nth 1 vals))
	   (nth 1 vals)) nil))
       )
     (LOAD
      string
      ,(semantic-lambda
	(semantic-tag-new-include
	 (file-name-nondirectory
	  (read
	   (nth 1 vals)))
	 (read
	  (nth 1 vals))))
      )
     (symbol
      ,(semantic-lambda
	(semantic-tag
	 (nth 0 vals)
	 'code))
      )
     ) ;; end scheme-in-list

    (name-args
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'name-arg-expand))
      )
     ) ;; end name-args

    (name-arg-expand
     (open-paren
      name-arg-expand
      ,(semantic-lambda
	(nth 1 vals))
      )
     (symbol
      name-arg-expand
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end name-arg-expand

    (opt-doc
     (string)
     ( ;;EMPTY
      )
     ) ;; end opt-doc

    (sequence
     (expression
      sequence)
     (expression)
     ) ;; end sequence

    (expression
     (symbol)
     (semantic-list)
     (string)
     ) ;; end expression
    )
  "Top level bovination table for scheme.")

(defvar semantic-scheme-keyword-table
  ;;DO NOT EDIT! Generated from scheme.by - 2003-03-13 20:18-0500
  (semantic-lex-make-keyword-table
   '(("define" . DEFINE)
     ("define-module" . DEFINE-MODULE)
     ("load" . LOAD))
   '(("load" summary "Function: (load \"filename\")")
     ("define-module" summary "Function: (define-module (name arg1 ...)) ")
     ("define" summary "Function: (define symbol expression)")))
  "Some keywords used in scheme.")

(defcustom semantic-default-scheme-path '("/usr/share/guile/")
  "Default set of include paths for scheme (guile) code.
Used by `semantic-inc' to define an include path.  This should
probably do some sort of search to see what is actually on the local
machine."
  :group 'scheme
  :type '(repeat (string :tag "Path")))

(defun semantic-scheme-prototype-nonterminal (token)
  "Return a prototype for the Emacs Lisp nonterminal TOKEN."
  (let* ((tok (semantic-token-token token))
	 (args (semantic-nonterminal-children token))
	 )
    (if (eq tok 'function)
	(concat (semantic-token-name token) " ("
		(mapconcat (lambda (a) a) args " ")
		")")
      (semantic-prototype-nonterminal-default token))))

(defun semantic-scheme-find-documentation (token &optional nosnarf)
  "Return the documentation string for TOKEN.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-token-docstring token)))
    (if (and d (> (length d) 0) (= (aref d 0) ?*))
	(substring d 1)
      d)))

(defun semantic-scheme-insert-foreign-token (token tokenfile)
  "Insert TOKEN from TOKENFILE at point.
Attempts a simple prototype for calling or using TOKEN."
  (cond ((eq (semantic-token-token token) 'function)
	 (insert "(" (semantic-token-name token) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-token-name token)))))

(define-lex semantic-scheme-lexer
  "A simple lexical analyzer that handles simple buffers.
This lexer ignores comments and whitespace, and will return
syntax as specified by the syntax table."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

;;;###autoload
(defun semantic-default-scheme-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  ;;DO NOT EDIT! Generated from scheme.by - 2003-03-13 20:18-0500
  (progn
    (setq semantic-toplevel-bovine-table semantic-toplevel-scheme-bovine-table
	  semantic-debug-parser-source "scheme.by"
	  semantic-debug-parser-class 'semantic-bovine-debug-parser
	  semantic-flex-keywords-obarray semantic-scheme-keyword-table
	  )
    (setq semantic-symbol->name-assoc-list '( (variable . "Variables")
					      ;;(type     . "Types")
					      (function . "Functions")
					      (include  . "Loads")
					      (package  . "DefineModule"))
	  imenu-create-index-function 'semantic-create-imenu-index
	  semantic-dependency-include-path semantic-default-scheme-path
	  imenu-create-index-function 'semantic-create-imenu-index
	  document-comment-start ";;"
	  document-comment-line-prefix ";;"
	  document-comment-end "\n"
	  ))
  (setq semantic-lex-analyzer #'semantic-scheme-lexer)
  )

;;;###autoload
(add-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

(provide 'semantic-scm)

;;; semantic-scm.el ends here
