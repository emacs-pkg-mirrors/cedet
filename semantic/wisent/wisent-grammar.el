;;; wisent-grammar.el --- Wisent's input grammar mode
;;
;; Copyright (C) 2002, 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 26 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-grammar.el,v 1.13 2003/08/02 08:20:15 ponced Exp $
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
;; Major mode for editing Wisent's input grammar (.wy) files.

;;; History:
;; 

;;; Code:
(require 'semantic-grammar)

(defsubst wisent-grammar-region-placeholder ($n)
  "Return $regionN placeholder symbol corresponding to given $N one.
Return nil if $N is not a valid placeholder symbol."
  (let ((n (symbol-name $n)))
    (if (string-match "^[$]\\([1-9][0-9]*\\)$" n)
        (intern (concat "$region" (match-string 1 n))))))

(defun wisent-grammar-EXPAND ($i nonterm)
  "Return expansion of built-in EXPAND expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (let ((start (semantic-grammar-start))
	($ri (wisent-grammar-region-placeholder $i)))
    (if (not (member nonterm start))
	(error "EXPANDFULL macro called with %s, but not used with %%start"
	       nonterm))
    (if $ri
        `(semantic-bovinate-from-nonterminal
          (car ,$ri) (cdr ,$ri) ',nonterm)
      (error "Invalid form (EXPAND %s %s)" $i nonterm))))

(defun wisent-grammar-EXPANDFULL ($i nonterm)
  "Return expansion of built-in EXPANDFULL expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (let ((start (semantic-grammar-start))
	($ri (wisent-grammar-region-placeholder $i)))
    (if (not (member nonterm start))
	(error "EXPANDFULL macro called with %s, but not used with %%start"
	       nonterm))
    (if $ri
        `(semantic-parse-region
          (car ,$ri) (cdr ,$ri) ',nonterm 1)
      (error "Invalid form (EXPANDFULL %s %s)" $i nonterm))))

(defun wisent-grammar-TAG (&rest args)
  "Return expansion of built-in TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag ,@args)))

(defun wisent-grammar-VARIABLE-TAG (&rest args)
  "Return expansion of built-in VARIABLE-TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag-new-variable ,@args)))

(defun wisent-grammar-FUNCTION-TAG (&rest args)
  "Return expansion of built-in FUNCTION-TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag-new-function ,@args)))

(defun wisent-grammar-TYPE-TAG (&rest args)
  "Return expansion of built-in TYPE-TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag-new-type ,@args)))

(defun wisent-grammar-INCLUDE-TAG (&rest args)
  "Return expansion of built-in INCLUDE-TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag-new-include ,@args)))

(defun wisent-grammar-PACKAGE-TAG (&rest args)
  "Return expansion of built-in PACKAGE-TAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-raw-tag (semantic-tag-new-package ,@args)))

(defun wisent-grammar-EXPANDTAG (&rest args)
  "Return expansion of built-in EXPANDTAG expression.
ARGS are the arguments passed to the expanded form."
  `(wisent-cook-tag ,@args))

(defun wisent-grammar-CODE-TAG (name &rest args)
  "Return expansion of built-in CODE-TAG expression.
NAME is the tag name."
  `(wisent-raw-tag (semantic-tag-new-code ,name ,@args)))

(defun wisent-grammar-AST-ADD (&rest args)
  "Return expansion of built-in AST-ADD expression.
ARGS are arguments passed to the function `semantic-ast-add'."
  `(semantic-ast-add ,@args))

(defun wisent-grammar-AST-PUT (&rest args)
  "Return expansion of built-in AST-PUT expression.
ARGS are arguments passed to the function `semantic-ast-put'."
  `(semantic-ast-put ,@args))

(defun wisent-grammar-AST-GET (&rest args)
  "Return expansion of built-in AST-GET expression.
ARGS are arguments passed to the function `semantic-ast-get'."
  `(semantic-ast-get ,@args))

(defun wisent-grammar-AST-GET1 (&rest args)
  "Return expansion of built-in AST-GET1 expression.
ARGS are arguments passed to the function `semantic-ast-get1'."
  `(semantic-ast-get1 ,@args))

(defun wisent-grammar-AST-GET-STRING (&rest args)
  "Return expansion of built-in AST-GET-STRING expression.
ARGS are arguments passed to the function `semantic-ast-get-string'."
  `(semantic-ast-get-string ,@args))

(defun wisent-grammar-AST-MERGE (&rest args)
  "Return expansion of built-in AST-MERGE expression.
ARGS are arguments passed to the function `semantic-ast-merge'."
  `(semantic-ast-merge ,@args))

(defconst wisent-grammar-builtins
  '(
    ;; Builtin name   . Expander
    ;; -------------- . ---------------------------------
    (  ASSOC          . semantic-grammar-ASSOC)
    (  EXPAND         . wisent-grammar-EXPAND)
    (  EXPANDFULL     . wisent-grammar-EXPANDFULL)
    (  TAG            . wisent-grammar-TAG)
    (  VARIABLE-TAG   . wisent-grammar-VARIABLE-TAG)
    (  FUNCTION-TAG   . wisent-grammar-FUNCTION-TAG)
    (  TYPE-TAG       . wisent-grammar-TYPE-TAG)
    (  INCLUDE-TAG    . wisent-grammar-INCLUDE-TAG)
    (  PACKAGE-TAG    . wisent-grammar-PACKAGE-TAG)
    (  CODE-TAG       . wisent-grammar-CODE-TAG)
    (  EXPANDTAG      . wisent-grammar-EXPANDTAG)
    (  AST-ADD        . wisent-grammar-AST-ADD)
    (  AST-PUT        . wisent-grammar-AST-PUT)
    (  AST-GET        . wisent-grammar-AST-GET)
    (  AST-GET1       . wisent-grammar-AST-GET1)
    (  AST-GET-STRING . wisent-grammar-AST-GET-STRING)
    (  AST-MERGE      . wisent-grammar-AST-MERGE)
    ;; -------------- . ---------------------------------
    )
  "Expanders of Semantic built-in functions in LALR grammar.")

(defun wisent-grammar-builtin-names ()
  "Return the list of built-in function names used in LALR grammars."
  (mapcar #'(lambda (e) (symbol-name (car e)))
          wisent-grammar-builtins))

(defun wisent-grammar-expand-builtins (expr)
  "Expand Semantic built-in function calls in expression EXPR.
Return the expanded expression.
The variable `wisent-grammar-builtins' defines the built-in functions
and their corresponding expanders."
  (if (or (atom expr) (semantic-grammar-quote-p (car expr)))
      expr ;; Just return atom or quoted expression.
    (let* ((expr (mapcar 'wisent-grammar-expand-builtins expr))
           (bltn (assq (car expr) wisent-grammar-builtins)))
      (if bltn ;; Expand Semantic built-in.
          (apply (cdr bltn) (cdr expr))
        expr))))

(defun wisent-grammar-assocs ()
  "Return associativity and precedence level definitions."
  (mapcar
   #'(lambda (tag)
       (cons (intern (semantic-tag-name tag))
             (mapcar #'semantic-grammar-item-value
                     (semantic-tag-get-attribute tag :value))))
   (semantic-find-tags-by-class 'assoc (current-buffer))))

(defun wisent-grammar-terminals ()
  "Return the list of terminal symbols.
Keep order of declaration in the WY file without duplicates."
  (let (terms)
    (mapcar
     #'(lambda (tag)
         (mapcar #'(lambda (name)
                     (add-to-list 'terms (intern name)))
                 (cons (semantic-tag-name tag)
                       (semantic-tag-get-attribute tag :rest))))
     (semantic--find-tags-by-function
      #'(lambda (tag)
          (memq (semantic-tag-class tag) '(token keyword)))
      (current-buffer)))
    (nreverse terms)))

(defun wisent-grammar-nonterminals ()
  "Return the list form of nonterminal definitions."
  (let ((nttags (semantic-find-tags-by-class
                 'nonterminal (current-buffer)))
        rltags nterms rules rule elems elem actn sexp prec)
    (while nttags
      (setq rltags (semantic-tag-components (car nttags))
            rules  nil)
      (while rltags
        (setq elems (semantic-tag-get-attribute (car rltags) :value)
              prec  (semantic-tag-get-attribute (car rltags) :prec)
              actn  (semantic-tag-get-attribute (car rltags) :expr)
              rule  nil)
        (when elems ;; not an EMPTY rule
          (while elems
            (setq elem  (car elems)
                  elems (cdr elems))
            (setq elem (if (consp elem) ;; mid-rule action
                           (wisent-grammar-expand-builtins (read (car elem)))
                         (semantic-grammar-item-value elem)) ;; item
                  rule (cons elem rule)))
          (setq rule (nreverse rule)))
        (if prec
            (setq prec (vector (semantic-grammar-item-value prec))))
        (if actn
            (setq sexp (wisent-grammar-expand-builtins (read actn))))
        (setq rule (if actn
                       (if prec
                           (list rule prec sexp)
                         (list rule sexp))
                     (if prec
                         (list rule prec)
                       (list rule))))
        (setq rules (cons rule rules)
              rltags (cdr rltags)))
      (setq nterms (cons (cons (intern (semantic-tag-name (car nttags)))
                               (nreverse rules))
                         nterms)
            nttags (cdr nttags)))
    (nreverse nterms)))

(defun wisent-grammar-grammar ()
  "Return Elisp form of the grammar."
  (let* ((terminals    (wisent-grammar-terminals))
         (nonterminals (wisent-grammar-nonterminals))
         (assocs       (wisent-grammar-assocs)))
    (cons terminals (cons assocs nonterminals))))

(defun wisent-grammar-tokentable-builder ()
  "Return the default value of the token table."
  (let ((tokens (semantic-grammar-tokens)))
    `(wisent-lex-make-token-table
      ',tokens
      ',(semantic-grammar-token-properties tokens))))

(defun wisent-grammar-parsetable-builder ()
  "Return the value of the parser table."
  `(progn
     ;; Ensure that the grammar [byte-]compiler is available.
     (eval-when-compile (require 'wisent-comp))
     (wisent-compile-grammar
      ',(wisent-grammar-grammar)
      ',(semantic-grammar-start))))

(defun wisent-grammar-setupcode-builder ()
  "Return the parser setup code."
  (format
   "(semantic-install-function-overrides\n\
      '((parse-stream . wisent-parse-stream)))\n\
    (setq semantic-parser-name \"LALR\"\n\
          semantic-toplevel-bovine-table %s\n\
          semantic-debug-parser-source %S\n\
          semantic-flex-keywords-obarray %s\n\
          semantic-lex-types-obarray %s)\n\
    ;; Collect unmatched syntax lexical tokens\n\
    (semantic-make-local-hook 'wisent-discarding-token-functions)\n\
    (add-hook 'wisent-discarding-token-functions\n\
              'wisent-collect-unmatched-syntax nil t)"
   (semantic-grammar-parsetable)
   (buffer-name)
   (semantic-grammar-keywordtable)
   (semantic-grammar-tokentable)))

;;;###autoload
(define-derived-mode wisent-grammar-mode semantic-grammar-mode "WY"
  "Major mode for editing Wisent grammars."
  (set (make-local-variable 'semantic-grammar-builtin-names)
       (wisent-grammar-builtin-names))
  (semantic-install-function-overrides
   '((grammar-tokentable-builder . wisent-grammar-tokentable-builder)
     (grammar-parsetable-builder . wisent-grammar-parsetable-builder)
     (grammar-setupcode-builder  . wisent-grammar-setupcode-builder)
     )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-grammar-mode))

;;;###autoload
(eval-after-load "speedbar"
  '(speedbar-add-supported-extension ".wy"))

(provide 'wisent-grammar)

;;; wisent-grammar.el ends here
