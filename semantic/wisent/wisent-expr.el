;;; wisent-expr.el --- Sample expression LALR parser for Emacs

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Version: 1.0
;; Keywords: syntax
;; X-RCS: $Id: wisent-expr.el,v 1.1.1.1 2001/07/20 11:07:24 ponced Exp $

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
;; Just convert expressions from postfix to infix notation.

;;; History:
;; 

;;; Code:

(require 'wisent)

(defconst wisent-expr-grammar
  '(
    ;; non-terminals
    CONSTANT
    LPAREN
    MINUS
    PLUS
    RPAREN
    SEMI
    SLASH
    STAR
    ;; productions
    (grammar (grammar expr)      : (append $1 (list $2))
             (expr)              : (list 'progn $1)
             )
    (expr    (add SEMI)          : $1
             (error SEMI)        : nil  ; error recovery
             (SEMI)              : nil
             )
    (add     (add MINUS mult)    : (list '- $1 $3)
             (add PLUS mult)     : (list '+ $1 $3)
             (mult)              : $1
             )
    (mult    (mult SLASH final)  : (list '/ $1 $3)
             (mult STAR final)   : (list '* $1 $3)
             (final)             : $1
             )
    (final   (LPAREN add RPAREN) : $2
             (CONSTANT)          : $1
             )
    )
  )

(defconst wisent-expr-parser-tables
  (wisent-compile-grammar wisent-expr-grammar))

(defconst wisent-expr-operators
  '((?\; . SEMI)
    (?\( . LPAREN)
    (?\) . RPAREN)
    (?\+ . PLUS)
    (?\- . MINUS)
    (?\* . STAR)
    (?\/ . SLASH)))

(defconst wisent-expr-digits
  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

(defvar wisent-expr-lexer-input-stream nil)

(defun wisent-expr-lexer ()
  (let* ((is  (or wisent-expr-lexer-input-stream ""))
         (isl (1- (length is)))
         (i   (string-match "[^ ]" is))
         (lex (list wisent-eoi-term))
         j c k)
    (if i
        (progn
          (setq c (aref is i)
                j i)
          (cond
           ;; Operator
           ((setq k (assq c wisent-expr-operators))
            (setq j   (1+ j)
                  lex (cons (cdr k) (cons c (cons i j)))))
           ;; Number
           ((setq k (memq c wisent-expr-digits))
            (while (and (< j isl) k (setq j (1+ j)))
              (setq c (aref is j)
                    k (memq c wisent-expr-digits)))
            (if (> j i)
                (setq lex (cons 'CONSTANT
                                (cons (car (read-from-string
                                            (substring is i j)))
                                      (cons i j))))))
           ;; Invalid input
           (t
            (error "Invalid input character %c" c)))
          (setq wisent-expr-lexer-input-stream (substring is j))))
    lex))

(defun wisent-expr-error (msg)
  (message msg)
;;  (debug)
  )

(defun wisent-expr-test ()
  (interactive)
  (let ((string (read-from-minibuffer "Expression: ")))
    (setq wisent-expr-lexer-input-stream string)
    (message "%s -> %s"
             string
             (wisent-parse wisent-expr-parser-tables
                           #'wisent-expr-lexer
                           #'wisent-expr-error))))

(provide 'wisent-expr)

;;; wisent-expr.el ends here
