;;; semantic-make.el --- Makefile parsing rules.

;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-make.el,v 1.12 2004/01/12 21:13:29 zappo Exp $

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
;; Use the Semantic Bovinator to parse Makefiles.
;; Concocted as an experiment for nonstandard languages.

(require 'semantic)
(require 'semantic-make-by)
(require 'backquote)

;;; Code:

(define-lex-simple-regex-analyzer semantic-lex-make-backslash-newline
  "A line ending with a \ continues to the next line and is treated as whitespace."
  "\\(\\\\\n\\s-*\\)" 'whitespace 1)

(define-lex-regex-analyzer semantic-lex-make-command
  "A command in a Makefile consists of a line starting with TAB, and ending at the newline."
  "^\\(\t\\)"
  (let ((start (match-end 0)))
    (while (progn (end-of-line)
		  (save-excursion (forward-char -1) (looking-at "\\\\")))
      (forward-char 1))
    (semantic-lex-push-token
     (semantic-lex-token 'shell-command start (point)))))

(define-lex semantic-make-lexer
  "Lexical analyzer for Makefiles."
  semantic-lex-make-command
  semantic-lex-make-backslash-newline
  semantic-lex-whitespace
  semantic-lex-newline
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(defun semantic-make-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil."
  (let ((name (semantic-tag-name tag))
        xpand)
    (when (consp name)
      (cond
       ((semantic-tag-of-class-p tag 'function)
        (while name
          (setq xpand (cons (semantic-tag-clone tag (car name)) xpand)
                name  (cdr name)))
        )
       ((semantic-tag-of-class-p tag 'include)
        (while name
          (setq xpand (cons (semantic-tag-clone tag (caar name)) xpand)
                name  (cdr name)))
        )))
    xpand))

(define-mode-overload-implementation semantic-get-local-variables
  makefile-mode (&optional point)
  "Override `semantic-get-local-variables' so it does not throw an error.
We never have local variables in Makefiles."
  nil)

;;;###autoload
(defun semantic-default-make-setup ()
  "Set up a Makefile buffer for parsing with semantic."
  (semantic-make-by--install-parser)
  (setq semantic-symbol->name-assoc-list '((variable . "Variables")
                                           (function . "Rules")
                                           (include . "Dependencies"))
        semantic-case-fold t
        semantic-tag-expand-function 'semantic-make-expand-tag
        semantic-lex-syntax-modifications '((?. "_")
                                            (?= ".")
                                            (?/ "_")
                                            (?$ ".")
                                            (?+ ".")
                                            (?\\ ".")
                                            )
        imenu-create-index-function 'semantic-create-imenu-index
        )
  (setq semantic-lex-analyzer #'semantic-make-lexer)
  )

;;;###autoload
(add-hook 'makefile-mode-hook 'semantic-default-make-setup)

(provide 'semantic-make)

;;; semantic-make.el ends here
