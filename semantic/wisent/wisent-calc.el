;;; wisent-calc.el --- Infix notation calculator

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 11 Sep 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-calc.el,v 1.1 2001/09/12 08:19:10 ponced Exp $

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
;; This is a port of the Bison 1.28d Infix Calc sample program to the
;; elisp LALR parser Wisent.  It illustrates usage of operator
;; precedence and contextual precedence.  The grammar is generated
;; from the BNF file wisent-calc.bnf.
;;
;; To run the calculator use M-x wisent-calc and at "calc:" prompt
;; enter expressions separated by semicolons.  Here is a sample run of
;; `wisent-calc':
;;
;;   calc: 4 + 4.5 - (34.0/(8*3+-3));
;;   -> 6.880952380952381;
;;   calc: -56 + 2;
;;   -> -54;
;;   calc: 3 ^ 2;
;;   -> 9;
;;   calc: 2*2*2 = 2^3;
;;   -> t;
;;   calc: 2*2*2; 2^3;
;;   -> 8; 8;

;;; History:
;; 

;;; Code:
(require 'wisent-bovine)

(defconst wisent-calc-parser-tables
  (eval-when-compile
  (wisent-compile-grammar
   '((NUM NEG NL EQ PLUS MINUS MULT DIV EXP LPAREN RPAREN)
     ((nonassoc EQ)
      (left PLUS MINUS)
      (left MULT DIV)
      (left NEG)
      (right EXP))
     (input
      (nil
       (identity ""))
      ((input line)
       (format "%s%s" $1 $2)))
     (line
      ((NL)
       (format "%s " $1))
      ((exp NL)
       (format "%s%s " $1 $2)))
     (exp
      ((NUM))
      ((exp EQ exp)
       (progn
         (if
             (not
              (= $1 $3))
             (message "wisent-calc error: %d != %d" $1 $3))
         (= $1 $3)))
      ((exp PLUS exp)
       (+ $1 $3))
      ((exp MINUS exp)
       (- $1 $3))
      ((exp MULT exp)
       (* $1 $3))
      ((exp DIV exp)
       (/ $1 $3))
      ((MINUS exp)
       [NEG]
       (- $2))
      ((exp EXP exp)
       (expt $1 $3))
      ((LPAREN exp RPAREN)
       (identity $2))))
   'nil))
"Calculator parser tables.")

(defconst wisent-calc-keywords
  nil
  "Calculator keywords.
Semantic BNF converter needs that this variable exists.  But it is not
used by the calculator.")

(defconst wisent-calc-tokens
  '((punctuation
     (RPAREN . ")")
     (LPAREN . "(")
     (EXP . "^")
     (DIV . "/")
     (MULT . "*")
     (MINUS . "-")
     (PLUS . "+")
     (EQ . "=")
     (NL . ";"))
    (dummy
     (NEG . ""))
    (literal
     (NUM . "")))
  "Calculator terminals.")

(defun wisent-calc-default-setup ()
  "Calculator setup hook.
Semantic BNF converter needs that this function exists.  But it is not
used by the calculator."
  ;; Code generated from wisent-calc.bnf
  (setq semantic-toplevel-bovine-table wisent-calc-parser-tables
        semantic-toplevel-bovine-table-source "wisent-calc.bnf")
  (setq semantic-flex-keywords-obarray wisent-calc-keywords)
 
 ;; End code generated from wisent-calc.bnf
 )

(defconst wisent-calc-number-regexp
  (eval-when-compile
    (concat "^\\("
            "[0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?"
            "\\)"
            ))
  "Lexer regexp to match calculator numbers.")

(defvar wisent-calc-lexer-input-stream nil
  "Calculator input stream.")

(defun wisent-calc-lexer ()
  "Calculator lexer."
  (let* ((is  (or wisent-calc-lexer-input-stream ""))
         (i   (string-match "[^ ]" is))
         (lex (list wisent-eoi-term))
         k)
    (if (not i)
        nil
      (setq is (substring is i))
      (cond
       ;; Number
       ((string-match wisent-calc-number-regexp is)
        (setq lex (list 'NUM (read (match-string 0 is)))
              is  (substring is (match-end 0))))
       ;; Punctuation
       ((setq k (semantic-flex-token-key
                 wisent-calc-tokens 'punctuation
                 (string (aref is 0))))
        (setq lex (list k (string (aref is 0)))
              is  (substring is 1)))
       ;; Invalid input
       (t
        (error "Invalid input character %s" is)))
      (setq wisent-calc-lexer-input-stream is))
    lex))

(defun wisent-calc (input)
  "Infix desktop calculator.
Parse INPUT string and output the result of computation."
  (interactive "scalc: ")
  (or (string-match ";\\s-*$" input)
      (setq input (concat input ";")))
  (let ((wisent-calc-lexer-input-stream input))
    (message "%s -> %s"
             input
             (wisent-parse wisent-calc-parser-tables
                           #'wisent-calc-lexer
                           #'error))))

(provide 'wisent-calc)

;;; wisent-calc.el ends here
