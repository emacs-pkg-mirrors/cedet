;;; bovine-grammar-macros.el --- Semantic macros for LL grammars
;;
;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 02 Aug 2003
;; Keywords: syntax
;; X-RCS: $Id: bovine-grammar-macros.el,v 1.1 2003/08/11 06:35:41 ponced Exp $
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
;; used in bovine (.by) grammars.

;;; History:
;;

;;; Code:

(defun bovine-grammar-EXPAND (&rest args)
  "Expand call to EXPAND grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-bovinate-from-nonterminal
    (car ,(car args)) (cdr ,(car args)) ',(cadr args)))

(defun bovine-grammar-EXPANDFULL (&rest args)
  "Expand call to EXPANDFULL grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-parse-region
    (car ,(car args)) (cdr ,(car args)) ',(cadr args) 1))

(defun bovine-grammar-TAG (&rest args)
  "Expand call to TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag ,@args))

(defun bovine-grammar-VARIABLE-TAG (&rest args)
  "Expand call to VARIABLE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-variable ,@args))

(defun bovine-grammar-FUNCTION-TAG (&rest args)
  "Expand call to FUNCTION-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-function ,@args))

(defun bovine-grammar-TYPE-TAG (&rest args)
  "Expand call to TYPE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-type ,@args))

(defun bovine-grammar-INCLUDE-TAG (&rest args)
  "Expand call to INCLUDE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-include ,@args))

(defun bovine-grammar-PACKAGE-TAG (&rest args)
  "Expand call to PACKAGE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-package ,@args))

(defun bovine-grammar-CODE-TAG (&rest args)
  "Expand call to CODE-TAG grammar macro.
Return the expanded form.
ARGS are the arguments passed to the macro."
  `(semantic-tag-new-code ,@args))

(defvar-mode-local bovine-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . bovine-grammar-EXPAND)
    (EXPANDFULL     . bovine-grammar-EXPANDFULL)
    (TAG            . bovine-grammar-TAG)
    (VARIABLE-TAG   . bovine-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . bovine-grammar-FUNCTION-TAG)
    (TYPE-TAG       . bovine-grammar-TYPE-TAG)
    (INCLUDE-TAG    . bovine-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . bovine-grammar-PACKAGE-TAG)
    (CODE-TAG       . bovine-grammar-CODE-TAG)
    )
  "Semantic grammar macros used in bovine grammars.")

(provide 'bovine-grammar-macros)

;;; bovine-grammar-macros.el ends here
