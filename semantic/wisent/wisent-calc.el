;;; wisent-calc.el --- Infix notation calculator

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 11 Sep 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-calc.el,v 1.2 2002/02/26 18:50:17 ponced Exp $

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
;; from the WY file wisent-calc.wy.
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

(defconst wisent-calc-automaton
  (eval-when-compile
    ;;DO NOT EDIT! Generated from wisent-calc.wy - 2002-02-25 23:43+0100
    (wisent-compile-grammar
     '((NUM NL EQ PLUS MINUS MULT DIV EXP LPAREN RPAREN)
       ((left PLUS MINUS MULT DIV NEG)
        (right EXP)
        (nonassoc EQ))
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
        ((NUM)
         (string-to-number $1))
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
     'nil)
    )
  "Parser automaton.")

(defconst wisent-calc-tokens
  (identity
   ;;DO NOT EDIT! Generated from wisent-calc.wy - 2002-02-25 23:43+0100
   (wisent-flex-make-token-table
    '(("close-paren"
       (RPAREN . ")"))
      ("open-paren"
       (LPAREN . "("))
      ("punctuation"
       (EXP . "^")
       (DIV . "/")
       (MULT . "*")
       (MINUS . "-")
       (PLUS . "+")
       (EQ . "=")
       (NL . ";"))
      ("number"
       (NUM)))
    '(("punctuation" string t)
      ("close-paren" string t)
      ("open-paren" string t)))
   )
  "Tokens.")

(defun wisent-calc-setup-parser ()
  "Setup buffer for parse."
  ;;DO NOT EDIT! Generated from wisent-calc.wy - 2002-02-25 23:43+0100
  (progn
    (setq semantic-bovinate-toplevel-override 'wisent-bovinate-toplevel
          semantic-toplevel-bovine-table wisent-calc-automaton
          semantic-flex-keywords-obarray nil
          wisent-flex-tokens-obarray wisent-calc-tokens)
    (setq semantic-number-expression
          (concat "\\([0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
                  "\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)")
          semantic-flex-depth nil
          semantic-flex-syntax-modifications '((?\; ".") (?\= ".")
                                               (?\+ ".") (?\- ".")
                                               (?\* ".") (?\/ ".")
                                               (?\^ ".")
                                               )
          ))
  )

(defun wisent-calc (input)
  "Infix desktop calculator.
Parse INPUT string and output the result of computation."
  (interactive "scalc: ")
  (or (string-match ";\\s-*$" input)
      (setq input (concat input ";")))
  (with-temp-buffer
    (wisent-calc-setup-parser)
    (insert input)
    (let ((wisent-flex-istream (semantic-flex-buffer)))
      (message "%s -> %s"
               input
               (wisent-parse wisent-calc-automaton
                             #'wisent-flex
                             #'error)))))

(provide 'wisent-calc)

;;; wisent-calc.el ends here
