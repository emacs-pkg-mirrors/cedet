;;; wisent-grammar.el --- Wisent's input grammar mode
;;
;; Copyright (C) 2002, 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 26 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-grammar.el,v 1.10 2003/03/31 07:48:09 ponced Exp $
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
(require 'cl) ;; `cl-macroexpand-all'

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
	(error "EXPANDFULL macro called with %s, but not used with %%start."
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
	(error "EXPANDFULL macro called with %s, but not used with %%start."
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
  `(wisent-raw-tag (semantic-tag-new-code name ,@args)))

(defconst wisent-grammar-builtins
  '(
    ;; Builtin name . Expander
    ;; ------------ . ---------------------------------
    (  ASSOC        . semantic-grammar-ASSOC)
    (  EXPAND       . wisent-grammar-EXPAND)
    (  EXPANDFULL   . wisent-grammar-EXPANDFULL)
    (  TAG          . wisent-grammar-TAG)
    (  VARIABLE-TAG . wisent-grammar-VARIABLE-TAG)
    (  FUNCTION-TAG . wisent-grammar-FUNCTION-TAG)
    (  TYPE-TAG     . wisent-grammar-TYPE-TAG)
    (  INCLUDE-TAG  . wisent-grammar-INCLUDE-TAG)
    (  PACKAGE-TAG  . wisent-grammar-PACKAGE-TAG)
    (  CODE-TAG     . wisent-grammar-CODE-TAG)
    (  EXPANDTAG    . wisent-grammar-EXPANDTAG)
    ;; ------------ . ---------------------------------
    )
  "Expanders of Semantic built-in functions in LALR grammar.")

(defun wisent-grammar-expand-builtins (expr)
  "Return expanded form of the expression EXPR.
Semantic built-in function calls are expanded.  The variable
`wisent-grammar-builtins' defines built-in functions and corresponding
expanders."
  (if (or (atom expr) (semantic-grammar-quote-p (car expr)))
      expr ;; Just return atom or quoted expression.
    (let* ((args (mapcar 'wisent-grammar-expand-builtins (cdr expr)))
           (bltn (assq (car expr) wisent-grammar-builtins)))
      (if bltn ;; Expand Semantic built-in.
          (apply (cdr bltn) args)
        (cons (car expr) args)))))

(defun wisent-grammar-expand-sexpr (expr)
  "Return expanded form of the expression EXPR.
Macro and Semantic built-in function calls are expanded."
  (cl-macroexpand-all (wisent-grammar-expand-builtins expr)))

(defun wisent-grammar-assocs ()
  "Return associativity and precedence level definitions."
  (mapcar
   #'(lambda (tag)
       (cons (intern (semantic-tag-name tag))
             (mapcar #'semantic-grammar-item-value
                     (semantic-tag-get-attribute tag :value))))
   (semantic-find-nonterminal-by-token 'assoc (current-buffer))))

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
     (semantic-find-nonterminal-by-function
      #'(lambda (tag)
          (memq (semantic-tag-class tag) '(token keyword)))
      (current-buffer)))
    (nreverse terms)))

(defun wisent-grammar-nonterminals ()
  "Return the list form of nonterminal definitions."
  (let ((nttags (semantic-find-nonterminal-by-token
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
                           (wisent-grammar-expand-sexpr (read (car elem)))
                         (semantic-grammar-item-value elem)) ;; item
                  rule (cons elem rule)))
          (setq rule (nreverse rule)))
        (if prec
            (setq prec (vector (semantic-grammar-item-value prec))))
        (if actn
            (setq sexp (wisent-grammar-expand-sexpr (read actn))))
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
  "Return the text of the setup code."
  (format
   "(progn\n\
      (semantic-install-function-overrides\n\
       '((parse-stream . wisent-parse-stream)))\n\
      (setq semantic-parser-name \"LALR\"\n\
            semantic-toplevel-bovine-table %s\n\
            semantic-debug-parser-source %S\n\
            semantic-flex-keywords-obarray %s\n\
            semantic-lex-types-obarray %s)\n\
      ;; Collect unmatched syntax lexical tokens\n\
      (semantic-make-local-hook 'wisent-discarding-token-functions)\n\
      (add-hook 'wisent-discarding-token-functions\n\
                'wisent-collect-unmatched-syntax nil t)\n\
     %s)"
   (semantic-grammar-parsetable)
   (file-name-nondirectory (buffer-file-name))
   (semantic-grammar-keywordtable)
   (semantic-grammar-tokentable)
   (semantic-grammar-setupcode-text)))


;;;###autoload
(define-derived-mode wisent-grammar-mode semantic-grammar-mode "WY"
  "Major mode for editing Wisent grammars."
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
