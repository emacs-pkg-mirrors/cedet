;;; semantic-scm.el --- Semantic details for Scheme (guile)

;;; Copyright (C) 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-scm.el,v 1.9 2003/08/02 08:15:01 ponced Exp $

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
(require 'semantic-scm-by)
(require 'backquote)

(eval-when-compile
  (require 'document))

;;; Code:

(defcustom semantic-default-scheme-path '("/usr/share/guile/")
  "Default set of include paths for scheme (guile) code.
Used by `semantic-inc' to define an include path.  This should
probably do some sort of search to see what is actually on the local
machine."
  :group 'scheme
  :type '(repeat (string :tag "Path")))

(defun semantic-scheme-prototype-nonterminal (token)
  "Return a prototype for the Emacs Lisp nonterminal TOKEN."
  (let* ((tok (semantic-tag-class token))
	 (args (semantic-tag-components token))
	 )
    (if (eq tok 'function)
	(concat (semantic-tag-name token) " ("
		(mapconcat (lambda (a) a) args " ")
		")")
      (semantic-prototype-nonterminal-default token))))

(defun semantic-scheme-find-documentation (token &optional nosnarf)
  "Return the documentation string for TOKEN.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-tag-docstring token)))
    (if (and d (> (length d) 0) (= (aref d 0) ?*))
	(substring d 1)
      d)))

(defun semantic-scheme-insert-foreign-token (token tokenfile)
  "Insert TOKEN from TOKENFILE at point.
Attempts a simple prototype for calling or using TOKEN."
  (cond ((eq (semantic-tag-class token) 'function)
	 (insert "(" (semantic-tag-name token) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-tag-name token)))))

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
  (semantic-scm-by--install-parser)
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
        )
  (setq semantic-lex-analyzer #'semantic-scheme-lexer)
  )

;;;###autoload
(add-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

(provide 'semantic-scm)

;;; semantic-scm.el ends here
