;;; wisent-grammar-macros.el --- Semantic macros for LALR grammars
;;
;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 02 Aug 2003
;; Keywords: syntax
;; X-RCS: $Id: wisent-grammar-macros.el,v 1.1 2003/08/11 06:37:09 ponced Exp $
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library defines the default set of Semantic grammar macros
;; used in wisent (.wy) grammars.

;;; History:
;;

;;; Code:

(defsubst wisent-grammar-region-placeholder (symb)
  "Given a $N placeholder symbol in SYMB, return a $regionN symbol.
Return nil if $N is not a valid placeholder symbol."
  (let ((n (symbol-name symb)))
    (if (string-match "^[$]\\([1-9][0-9]*\\)$" n)
        (intern (concat "$region" (match-string 1 n))))))

(defun wisent-grammar-EXPAND (symb nonterm)
  "Expand call to EXPAND grammar macro.
Return the expanded form.
SYMB is the $I placeholder symbol to expand.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-bovinate-from-nonterminal
          (car ,$ri) (cdr ,$ri) ',nonterm)
      (error "Invalid form (EXPAND %s %s)" symb nonterm))))

(defun wisent-grammar-EXPANDFULL (symb nonterm)
  "Expand call to EXPANDFULL grammar macro.
Return the expanded form.
SYMB is the $I placeholder symbol to expand.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-parse-region
          (car ,$ri) (cdr ,$ri) ',nonterm 1)
      (error "Invalid form (EXPANDFULL %s %s)" symb nonterm))))

(defun wisent-grammar-TAG (&rest args)
  "Expand call to TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag ,@args)))

(defun wisent-grammar-VARIABLE-TAG (&rest args)
  "Expand call to VARIABLE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-variable ,@args)))

(defun wisent-grammar-FUNCTION-TAG (&rest args)
  "Expand call to FUNCTION-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-function ,@args)))

(defun wisent-grammar-TYPE-TAG (&rest args)
  "Expand call to TYPE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-type ,@args)))

(defun wisent-grammar-INCLUDE-TAG (&rest args)
  "Expand call to INCLUDE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-include ,@args)))

(defun wisent-grammar-PACKAGE-TAG (&rest args)
  "Expand call to PACKAGE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-package ,@args)))

(defun wisent-grammar-EXPANDTAG (&rest args)
  "Expand call to EXPANDTAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-cook-tag ,@args))

(defun wisent-grammar-CODE-TAG (&rest args)
  "Expand call to CODE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(wisent-raw-tag (semantic-tag-new-code ,@args)))

(defun wisent-grammar-AST-ADD (&rest args)
  "Expand call to AST-ADD grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-add ,@args))

(defun wisent-grammar-AST-PUT (&rest args)
  "Expand call to AST-PUT grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-put ,@args))

(defun wisent-grammar-AST-GET (&rest args)
  "Expand call to AST-GET grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-get ,@args))

(defun wisent-grammar-AST-GET1 (&rest args)
  "Expand call to AST-GET1 grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-get1 ,@args))

(defun wisent-grammar-AST-GET-STRING (&rest args)
  "Expand call to AST-GET-STRING grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-get-string ,@args))

(defun wisent-grammar-AST-MERGE (&rest args)
  "Expand call to AST-MERGE grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-ast-merge ,@args))

(defvar-mode-local wisent-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . wisent-grammar-EXPAND)
    (EXPANDFULL     . wisent-grammar-EXPANDFULL)
    (TAG            . wisent-grammar-TAG)
    (VARIABLE-TAG   . wisent-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . wisent-grammar-FUNCTION-TAG)
    (TYPE-TAG       . wisent-grammar-TYPE-TAG)
    (INCLUDE-TAG    . wisent-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . wisent-grammar-PACKAGE-TAG)
    (EXPANDTAG      . wisent-grammar-EXPANDTAG)
    (CODE-TAG       . wisent-grammar-CODE-TAG)
    (AST-ADD        . wisent-grammar-AST-ADD)
    (AST-PUT        . wisent-grammar-AST-PUT)
    (AST-GET        . wisent-grammar-AST-GET)
    (AST-GET1       . wisent-grammar-AST-GET1)
    (AST-GET-STRING . wisent-grammar-AST-GET-STRING)
    (AST-MERGE      . wisent-grammar-AST-MERGE)
    )
  "Semantic grammar macros used in wisent grammars.")

(provide 'wisent-grammar-macros)

;;; wisent-grammar-macros.el ends here
