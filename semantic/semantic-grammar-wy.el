;;; semantic-grammar-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Created: 2003-10-02 15:27:21+0200
;; Keywords: syntax
;; X-RCS: $Id: semantic-grammar-wy.el,v 1.7 2003/10/02 13:56:59 ponced Exp $

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
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file semantic-grammar.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-grammar-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("%default-prec" . DEFAULT-PREC)
     ("%no-default-prec" . NO-DEFAULT-PREC)
     ("%languagemode" . LANGUAGEMODE)
     ("%left" . LEFT)
     ("%nonassoc" . NONASSOC)
     ("%package" . PACKAGE)
     ("%prec" . PREC)
     ("%put" . PUT)
     ("%quotemode" . QUOTEMODE)
     ("%right" . RIGHT)
     ("%scopestart" . SCOPESTART)
     ("%start" . START)
     ("%token" . TOKEN)
     ("%use-macros" . USE-MACROS)
     ("%keywordtable" . KEYWORDTABLE)
     ("%outputfile" . OUTPUTFILE)
     ("%parsetable" . PARSETABLE)
     ("%setupfunction" . SETUPFUNCTION)
     ("%tokentable" . TOKENTABLE))
   'nil)
  "Table of language keywords.")

(defconst semantic-grammar-wy--token-table
  (wisent-lex-make-token-table
   '(("punctuation"
      (GT . ">")
      (LT . "<")
      (OR . "|")
      (SEMI . ";")
      (COLON . ":"))
     ("close-paren"
      (RBRACE . "}"))
     ("open-paren"
      (LBRACE . "{"))
     ("semantic-list"
      (BRACE_BLOCK . "^{")
      (PAREN_BLOCK . "^("))
     ("code"
      (EPILOGUE . "%%...EOF")
      (PROLOGUE . "%{...%}"))
     ("sexp"
      (PREFIXED_LIST . "\\s'\\s-*(")
      (SEXP))
     ("char"
      (CHARACTER))
     ("symbol"
      (PERCENT_PERCENT . "%%")
      (SYMBOL))
     ("string"
      (STRING)))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-grammar-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((DEFAULT-PREC NO-DEFAULT-PREC LANGUAGEMODE LEFT NONASSOC PACKAGE PREC PUT QUOTEMODE RIGHT SCOPESTART START TOKEN USE-MACROS KEYWORDTABLE OUTPUTFILE PARSETABLE SETUPFUNCTION TOKENTABLE STRING SYMBOL PERCENT_PERCENT CHARACTER SEXP PREFIXED_LIST PROLOGUE EPILOGUE PAREN_BLOCK BRACE_BLOCK LBRACE RBRACE COLON SEMI OR LT GT)
       nil
       (grammar
        ((prologue))
        ((epilogue))
        ((declaration))
        ((nonterminal))
        ((PERCENT_PERCENT)))
       (prologue
        ((PROLOGUE)
         (wisent-raw-tag
          (semantic-tag-new-code "prologue" nil))))
       (epilogue
        ((EPILOGUE)
         (wisent-raw-tag
          (semantic-tag-new-code "epilogue" nil))))
       (declaration
        ((decl)
         (eval $1)))
       (decl
        ((default_prec_decl))
        ((no_default_prec_decl))
        ((languagemode_decl))
        ((package_decl))
        ((precedence_decl))
        ((put_decl))
        ((quotemode_decl))
        ((scopestart_decl))
        ((start_decl))
        ((token_decl))
        ((use_macros_decl))
        ((keywordtable_decl))
        ((outputfile_decl))
        ((parsetable_decl))
        ((setupfunction_decl))
        ((tokentable_decl)))
       (default_prec_decl
         ((DEFAULT-PREC)
          `(wisent-raw-tag
            (semantic-tag "default-prec" 'assoc :value
                          '("t")))))
       (no_default_prec_decl
        ((NO-DEFAULT-PREC)
         `(wisent-raw-tag
           (semantic-tag "default-prec" 'assoc :value
                         '("nil")))))
       (languagemode_decl
        ((LANGUAGEMODE symbols)
         `(wisent-raw-tag
           (semantic-tag ',(car $2)
                         'languagemode :rest ',(cdr $2)))))
       (package_decl
        ((PACKAGE SYMBOL)
         `(wisent-raw-tag
           (semantic-tag-new-package ',$2 nil))))
       (precedence_decl
        ((associativity token_type_opt items)
         `(wisent-raw-tag
           (semantic-tag ',$1 'assoc :type ',$2 :value ',$3))))
       (associativity
        ((LEFT)
         (progn "left"))
        ((RIGHT)
         (progn "right"))
        ((NONASSOC)
         (progn "nonassoc")))
       (put_decl
        ((PUT SYMBOL put_value)
         `(wisent-raw-tag
           (semantic-tag ',$2 'put :value ',(list $3))))
        ((PUT SYMBOL put_value_list)
         (let*
             ((vals
               (mapcar 'semantic-tag-name $3)))
           `(wisent-raw-tag
             (semantic-tag ',$2 'put :value ',vals))))
        ((PUT put_name_list put_value)
         (let*
             ((names
               (mapcar 'semantic-tag-name $2)))
           `(wisent-raw-tag
             (semantic-tag ',(car names)
                           'put :rest ',(cdr names)
                           :value ',(list $3)))))
        ((PUT put_name_list put_value_list)
         (let*
             ((names
               (mapcar 'semantic-tag-name $2))
              (vals
               (mapcar 'semantic-tag-name $3)))
           `(wisent-raw-tag
             (semantic-tag ',(car names)
                           'put :rest ',(cdr names)
                           :value ',vals)))))
       (put_name_list
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'put_names 1)))
       (put_names
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((SYMBOL)
         (wisent-raw-tag
          (semantic-tag $1 'put-name))))
       (put_value_list
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'put_values 1)))
       (put_values
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((put_value)
         (wisent-raw-tag
          (semantic-tag $1 'put-value))))
       (put_value
        ((SYMBOL any_value)
         (cons $1 $2)))
       (scopestart_decl
        ((SCOPESTART SYMBOL)
         `(wisent-raw-tag
           (semantic-tag ',$2 'scopestart))))
       (quotemode_decl
        ((QUOTEMODE SYMBOL)
         `(wisent-raw-tag
           (semantic-tag ',$2 'quotemode))))
       (start_decl
        ((START symbols)
         `(wisent-raw-tag
           (semantic-tag ',(car $2)
                         'start :rest ',(cdr $2)))))
       (token_decl
        ((TOKEN token_type_opt SYMBOL string_value)
         `(wisent-raw-tag
           (semantic-tag ',$3 ',(if $2 'token 'keyword)
                         :type ',$2 :value ',$4)))
        ((TOKEN token_type_opt symbols)
         `(wisent-raw-tag
           (semantic-tag ',(car $3)
                         'token :type ',$2 :rest ',(cdr $3)))))
       (token_type_opt
        (nil)
        ((token_type)))
       (token_type
        ((LT SYMBOL GT)
         (progn $2)))
       (use_macros_decl
        ((USE-MACROS SYMBOL put_name_list)
         (let*
             ((names
               (mapcar 'semantic-tag-name $3)))
           `(wisent-raw-tag
             (semantic-tag "macro" 'macro :type ',$2 :value ',names)))))
       (keywordtable_decl
        ((KEYWORDTABLE SYMBOL)
         (progn
           (message "The %s keyword is obsolete and no more used" $1)
           nil)))
       (outputfile_decl
        ((OUTPUTFILE string_value)
         (progn
           (message "The %s keyword is obsolete and no more used" $1)
           nil)))
       (parsetable_decl
        ((PARSETABLE SYMBOL)
         (progn
           (message "The %s keyword is obsolete and no more used" $1)
           nil)))
       (setupfunction_decl
        ((SETUPFUNCTION SYMBOL)
         (progn
           (message "The %s keyword is obsolete and no more used" $1)
           nil)))
       (tokentable_decl
        ((TOKENTABLE SYMBOL)
         (progn
           (message "The %s keyword is obsolete and no more used" $1)
           nil)))
       (string_value
        ((STRING)
         (read $1)))
       (any_value
        ((SYMBOL))
        ((STRING))
        ((PAREN_BLOCK))
        ((PREFIXED_LIST))
        ((SEXP)))
       (symbols
        ((lifo_symbols)
         (nreverse $1)))
       (lifo_symbols
        ((lifo_symbols SYMBOL)
         (cons $2 $1))
        ((SYMBOL)
         (list $1)))
       (nonterminal
        ((SYMBOL COLON rules SEMI)
         (wisent-raw-tag
          (semantic-tag $1 'nonterminal :children $3))))
       (rules
        ((lifo_rules)
         (apply 'nconc
                (nreverse $1))))
       (lifo_rules
        ((lifo_rules OR rule)
         (cons $3 $1))
        ((rule)
         (list $1)))
       (rule
        ((rhs)
         (let*
             ((rhs $1)
              name type comps prec action elt)
           (while rhs
             (setq elt
                   (car rhs)
                   rhs
                   (cdr rhs))
             (cond
              ((vectorp elt)
               (if prec
                   (message "Duplicate %%prec in a rule, keep latest"))
               (setq prec
                     (aref elt 0)))
              ((consp elt)
               (if
                   (or action comps)
                   (setq comps
                         (cons elt comps))
                 (setq action
                       (car elt))))
              (t
               (setq comps
                     (cons elt comps)))))
           (if comps
               (setq type "group" name
                     (mapconcat
                      #'(lambda
                          (e)
                          (if
                              (consp e)
                              "{}" e))
                      comps " "))
             (setq type "empty" name ";;EMPTY"))
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag name 'rule :type type :value comps :prec prec :expr action))))))
       (rhs
        (nil)
        ((rhs item)
         (cons $2 $1))
        ((rhs action)
         (cons
          (list $2)
          $1))
        ((rhs PREC item)
         (cons
          (vector $3)
          $1)))
       (action
        ((PAREN_BLOCK))
        ((PREFIXED_LIST))
        ((BRACE_BLOCK)
         (format "(progn\n%s)"
                 (let
                     ((s $1))
                   (if
                       (string-match "^{[\n	 ]*" s)
                       (setq s
                             (substring s
                                        (match-end 0))))
                   (if
                       (string-match "[\n	 ]*}$" s)
                       (setq s
                             (substring s 0
                                        (match-beginning 0))))
                   s))))
       (items
        ((lifo_items)
         (nreverse $1)))
       (lifo_items
        ((lifo_items item)
         (cons $2 $1))
        ((item)
         (list $1)))
       (item
        ((SYMBOL))
        ((CHARACTER))))
     '(grammar prologue epilogue declaration nonterminal rule put_names put_values)))
  "Parser table.")

(defun semantic-grammar-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic-toplevel-bovine-table semantic-grammar-wy--parse-table
        semantic-debug-parser-source "semantic-grammar.wy"
        semantic-flex-keywords-obarray semantic-grammar-wy--keyword-table
        semantic-lex-types-obarray semantic-grammar-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Epilogue
;;

(provide 'semantic-grammar-wy)

;;; semantic-grammar-wy.el ends here
