;;; wisent-java-lex.el --- Wisent Java Lexical Analyzers

;; Copyright (C) 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-java-lex.el,v 1.3 2002/08/04 16:16:44 ponced Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:
(require 'wisent-bovine)

;;; Analyzers
;;
(define-lex-regex-analyzer wisent-java-lex-symbol
  "Detect and create identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-token
   (or (semantic-lex-keyword-p (match-string 0))
       'IDENTIFIER)
   (match-beginning 0)
   (match-end 0)))

(define-lex-regex-analyzer wisent-java-lex-symbol2
  "Detect and create literal, identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (let ((sy (match-string 0)))
    (semantic-lex-token
     (or (semantic-lex-keyword-p sy)
         (cond
          ((string-equal sy "null")
           'NULL_LITERAL)
          ((string-equal sy "true")
           'BOOLEAN_LITERAL)
          ((string-equal sy "false")
           'BOOLEAN_LITERAL)
          (t
           'IDENTIFIER)))
     (match-beginning 0)
     (match-end 0))))

(define-lex-simple-regex-analyzer wisent-java-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUMBER_LITERAL)

(define-lex-regex-analyzer wisent-java-lex-string
  "Detect and create a string token."
  "\\s\""
  ;; Zing to the end of this string.
  (semantic-lex-token
   'STRING_LITERAL (point)
   (save-excursion
     (condition-case nil
         (forward-sexp 1)
       ;; This case makes lex
       ;; robust to broken strings.
       (error
        (goto-char
         (funcall
          semantic-lex-unterminated-syntax-end-function
          'string
          start end))))
     (point))))

(define-lex-block-analyzer wisent-java-lex-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACK_BLOCK ("[" LBRACK) ("]" RBRACK))
  )

;;; Lexers
;;
(define-lex wisent-java-tags-lexer
  "Lexical analyzer that handles Java buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-java-lex-number
  wisent-java-lex-string
  wisent-java-lex-symbol
  wisent-flex-punctuation
  wisent-java-lex-blocks
  semantic-lex-default-action)

(define-lex wisent-java-lexer
  "Lexical analyzer that handles Java buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-java-lex-number
  wisent-java-lex-string
  wisent-java-lex-symbol2
  wisent-flex-punctuation
  wisent-java-lex-blocks
  semantic-lex-default-action)

;;; Test
;;
(defun wisent-java-tags-lex-buffer ()
  "Run `wisent-java-tags-lexer' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'wisent-java-tags-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer
        (get-buffer-create "*wisent-java-tags-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun wisent-java-lex-buffer ()
  "Run `wisent-java-lexer' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'wisent-java-lex)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer
        (get-buffer-create "*wisent-java-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'wisent-java-lex)

;;; wisent-java-lex.el ends here
