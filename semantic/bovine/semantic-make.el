;;; semantic-make.el --- Makefile parsing rules.

;; Copyright (C) 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-make.el,v 1.9 2003/04/07 11:22:54 ponced Exp $

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
(require 'backquote)

;;; Code:
(defvar semantic-toplevel-make-bovine-table
  ;;DO NOT EDIT! Generated from make.by - 2003-04-07 12:33+0200
  `(
    (bovine-toplevel ;;Makefile
     (variable)
     (rule)
     (conditional)
     (include)
     (whitespace
      ,(semantic-lambda
        (list nil))
      )
     (newline
      ,(semantic-lambda
        (list nil))
      )
     ) ;; end Makefile

    (variable
     (symbol
      opt-whitespace
      equals
      opt-whitespace
      element-list
      ,(semantic-lambda
        (semantic-tag-new-variable
         (nth 0 vals) nil
         (nth 4 vals)))
      )
     ) ;; end variable

    (rule
     (targets
      opt-whitespace
      colons
      opt-whitespace
      element-list
      commands
      ,(semantic-lambda
        (semantic-tag-new-function
         (nth 0 vals) nil
         (nth 4 vals)))
      )
     ) ;; end rule

    (targets
     (target
      opt-whitespace
      targets
      ,(semantic-lambda
        (list
         (car
          (nth 0 vals))
         (car
          (nth 2 vals))))
      )
     (target
      ,(semantic-lambda
        (list
         (car
          (nth 0 vals))))
      )
     ) ;; end targets

    (target
     (sub-target
      target
      ,(semantic-lambda
        (list
         (concat
          (car
           (nth 0 vals))
          (car
           (nth 2 vals)))))
      )
     (sub-target
      ,(semantic-lambda
        (list
         (car
          (nth 0 vals))))
      )
     ) ;; end target

    (sub-target
     (symbol)
     (string)
     (varref)
     ) ;; end sub-target

    (conditional
     (IF
      whitespace
      symbol
      newline
      ,(semantic-lambda
        (list nil))
      )
     (IFDEF
      whitespace
      symbol
      newline
      ,(semantic-lambda
        (list nil))
      )
     (IFNDEF
      whitespace
      symbol
      newline
      ,(semantic-lambda
        (list nil))
      )
     (IFEQ
      whitespace
      expression
      newline
      ,(semantic-lambda
        (list nil))
      )
     (IFNEQ
      whitespace
      expression
      newline
      ,(semantic-lambda
        (list nil))
      )
     (ELSE
      newline
      ,(semantic-lambda
        (list nil))
      )
     (ENDIF
      newline
      ,(semantic-lambda
        (list nil))
      )
     ) ;; end conditional

    (expression
     (semantic-list)
     ) ;; end expression

    (include
     (INCLUDE
      whitespace
      element-list
      ,(semantic-lambda
        (semantic-tag-new-include
         (nth 2 vals) nil))
      )
     ) ;; end include

    (equals
     (punctuation
      "\\b[:]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda)
      )
     (punctuation
      "\\b[+]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda)
      )
     (punctuation
      "\\b[=]\\b"
      ,(semantic-lambda)
      )
     ) ;; end equals

    (colons
     (punctuation
      "\\b[:]\\b"
      punctuation
      "\\b[:]\\b"
      ,(semantic-lambda)
      )
     (punctuation
      "\\b[:]\\b"
      ,(semantic-lambda)
      )
     ) ;; end colons

    (element-list
     (elements
      newline
      ,(semantic-lambda
        (nth 0 vals))
      )
     ) ;; end element-list

    (elements
     (element
      whitespace
      elements
      ,(semantic-lambda
        (list
         (nth 0 vals))
        (nth 2 vals))
      )
     (element
      ,(semantic-lambda
        (list
         (nth 0 vals)))
      )
     ( ;;EMPTY
      )
     ) ;; end elements

    (element
     (sub-element
      element
      ,(semantic-lambda
        (list
         (concat
          (car
           (nth 0 vals))
          (car
           (nth 1 vals)))))
      )
     ( ;;EMPTY
      )
     ) ;; end element

    (sub-element
     (symbol)
     (string)
     (punctuation)
     (semantic-list
      ,(semantic-lambda
        (list
         (buffer-substring-no-properties
          (identity start)
          (identity end))))
      )
     ) ;; end sub-element

    (varref
     (punctuation
      "\\b[$]\\b"
      semantic-list
      ,(semantic-lambda
        (list
         (buffer-substring-no-properties
          (identity start)
          (identity end))))
      )
     ) ;; end varref

    (commands
     (shell-command
      newline
      commands
      ,(semantic-lambda
        (list
         (nth 0 vals))
        (nth 1 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end commands

    (opt-whitespace
     (whitespace
      ,(semantic-lambda
        (list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-whitespace
    )
  "Table for parsing Makefiles.")

(defvar semantic-make-keyword-table
  ;;DO NOT EDIT! Generated from make.by - 2003-04-07 12:33+0200
  (semantic-lex-make-keyword-table
   '(("if" . IF)
     ("ifdef" . IFDEF)
     ("ifndef" . IFNDEF)
     ("ifeq" . IFEQ)
     ("ifneq" . IFNEQ)
     ("else" . ELSE)
     ("endif" . ENDIF)
     ("include" . INCLUDE))
   '(("include" summary "Macro: include filename1 filename2 ...")
     ("ifneq" summary "Conditional: ifneq (expression) ... else ... endif")
     ("ifeq" summary "Conditional: ifeq (expression) ... else ... endif")
     ("ifndef" summary "Conditional: ifndef (expression) ... else ... endif")
     ("ifdef" summary "Conditional: ifdef (expression) ... else ... endif")
     ("endif" summary "Conditional: if (expression) ... else ... endif")
     ("else" summary "Conditional: if (expression) ... else ... endif")
     ("if" summary "Conditional: if (expression) ... else ... endif")))
  "Keyword table for Makefiles.")

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
  (let (name xpand)
    (when (and (memq (semantic-tag-class tag) '(function include))
               (listp (setq name (semantic-tag-name tag))))
      (while name
        (setq xpand (cons (semantic-tag-clone tag (car name)) xpand)
              name  (cdr name)))
      xpand)))

;;;###autoload
(defun semantic-default-make-setup ()
  "Set up a Makefile buffer for parsing with semantic."
  ;;DO NOT EDIT! Generated from make.by - 2003-04-07 12:33+0200
  (progn
    (setq semantic-toplevel-bovine-table semantic-toplevel-make-bovine-table
          semantic-debug-parser-source "make.by"
          semantic-debug-parser-class 'semantic-bovine-debug-parser
          semantic-flex-keywords-obarray semantic-make-keyword-table
          )
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
          ))
  (setq semantic-lex-analyzer #'semantic-make-lexer)
  )

;;;###autoload
(add-hook 'makefile-mode-hook 'semantic-default-make-setup)

(provide 'semantic-make)

;;; semantic-make.el ends here
