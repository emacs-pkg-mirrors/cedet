;;; wisent-wy.el --- Wisent's input grammar mode
;;
;; Copyright (C) 2002 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 Feb 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-wy.el,v 1.1 2002/02/23 08:57:09 ponced Exp $
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
(require 'wisent-bovine)
(eval-when-compile
  (require 'font-lock))

;;;;
;;;; Set up parser
;;;;

(defconst wisent-wy-automaton
  (eval-when-compile
    ;;DO NOT EDIT! Generated from wisent-wy.wy - 2002-02-22 21:11+0100
    (wisent-compile-grammar
     '((LEFT NONASSOC PREC PUT RIGHT START TOKEN LANGUAGEMODE OUTPUTFILE SETUPFUNCTION KEYWORDTABLE PARSETABLE TOKENTABLE COLON GT LT OR PERCENT SEMI PAREN_BLOCK BRACE_BLOCK LBRACE RBRACE NUMBER STRING SYMBOL)
       nil
       (grammar
        ((PERCENT)
         nil)
        ((code))
        ((declaration))
        ((nonterminal)))
       (code
        ((PAREN_BLOCK)
         (wisent-token "code" 'code nil $1 nil))
        ((BRACE_BLOCK)
         (wisent-token "code" 'code nil $1 nil)))
       (declaration
        ((decl)
         (apply #'wisent-token $1)))
       (decl
        ((languagemode_decl))
        ((outputfile_decl))
        ((setupfunction_decl))
        ((keywordtable_decl))
        ((parsetable_decl))
        ((tokentable_decl))
        ((token_decl))
        ((start_decl))
        ((left_decl))
        ((right_decl))
        ((nonassoc_decl))
        ((put_decl)))
       (languagemode_decl
        ((LANGUAGEMODE symbols)
         (list
          (car $2)
          'languagemode nil
          (cdr $2)
          nil)))
       (outputfile_decl
        ((OUTPUTFILE string_value)
         (list $2 'outputfile nil nil)))
       (string_value
        ((STRING)
         (read $1)))
       (setupfunction_decl
        ((SETUPFUNCTION SYMBOL)
         (list $2 'setupfunction nil nil)))
       (keywordtable_decl
        ((KEYWORDTABLE SYMBOL)
         (list $2 'keywordtable nil nil)))
       (parsetable_decl
        ((PARSETABLE SYMBOL)
         (list $2 'parsetable nil nil)))
       (tokentable_decl
        ((TOKENTABLE SYMBOL)
         (list $2 'tokentable nil nil)))
       (token_decl
        ((TOKEN token_type_opt SYMBOL string_value)
         (list $3
               (if $2 'token 'keyword)
               $2 $4 nil))
        ((TOKEN token_type_opt symbols)
         (list "" 'token $2 $3 nil)))
       (token_type_opt
        (nil)
        ((token_type)))
       (token_type
        ((LT SYMBOL GT)
         (identity $2)))
       (start_decl
        ((START symbols)
         (list
          (car $2)
          'start nil
          (cdr $2)
          nil)))
       (left_decl
        ((LEFT symbols)
         (list
          (car $2)
          'left nil
          (cdr $2)
          nil)))
       (right_decl
        ((RIGHT symbols)
         (list
          (car $2)
          'right nil
          (cdr $2)
          nil)))
       (nonassoc_decl
        ((NONASSOC symbols)
         (list
          (car $2)
          'nonassoc nil
          (cdr $2)
          nil)))
       (put_decl
        ((PUT SYMBOL put_value)
         (list $2 'put nil nil
               (list $3)
               nil))
        ((PUT SYMBOL put_value_list)
         (let*
             ((vals
               (mapcar
                #'(lambda
                    (tok)
                    (nth 3 tok))
                $3)))
           (list $2 'put nil nil vals nil)))
        ((PUT put_name_list put_value)
         (let*
             ((names
               (mapcar #'semantic-token-name $2)))
           (list
            (car names)
            'put nil
            (cdr names)
            (list $3)
            nil)))
        ((PUT put_name_list put_value_list)
         (let*
             ((names
               (mapcar #'semantic-token-name $2))
              (vals
               (mapcar
                #'(lambda
                    (tok)
                    (nth 3 tok))
                $3)))
           (list
            (car names)
            'put nil
            (cdr names)
            vals nil))))
       (put_name_list
        ((BRACE_BLOCK)
         (wisent-bovinate-from-nonterminal-full
          (car $region1)
          (cdr $region1)
          'put_names)))
       (put_names
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((SYMBOL)
         (list
          (list $1 'put-name nil nil))))
       (put_value_list
        ((BRACE_BLOCK)
         (wisent-bovinate-from-nonterminal-full
          (car $region1)
          (cdr $region1)
          'put_values)))
       (put_values
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((put_value)
         (list
          (list "" 'put-value nil $1 nil))))
       (put_value
        ((SYMBOL any_value)
         (cons $1 $2)))
       (any_value
        ((STRING))
        ((SYMBOL))
        ((NUMBER))
        ((PAREN_BLOCK)))
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
         (wisent-token $1 'nonterminal nil $3 nil))
        ((error SEMI)))
       (rules
        ((lifo_rules)
         (apply #'nconc
                (nreverse $1))))
       (lifo_rules
        ((lifo_rules OR rule)
         (cons $3 $1))
        ((rule)
         (list $1)))
       (rule
        (nil
         (wisent-token "empty" 'rule nil nil nil nil nil))
        ((action)
         (wisent-token "empty" 'rule nil nil $1 nil nil))
        ((prec action)
         (wisent-token "empty" 'rule nil nil $2 $1 nil))
        ((elements action_opt)
         (let
             ((elts
               (nreverse $1)))
           (wisent-token
            (mapconcat #'cdr elts " ")
            'rule nil elts $2 nil nil)))
        ((elements prec action_opt)
         (let
             ((elts
               (nreverse $1)))
           (wisent-token
            (mapconcat #'cdr elts " ")
            'rule nil elts $3 $2 nil))))
       (prec
        ((PERCENT PREC SYMBOL)
         (identity $3)))
       (action_opt
        (nil)
        ((action)))
       (action
        ((PAREN_BLOCK))
        ((BRACE_BLOCK)
         (format "(progn\n%s)"
                 (let
                     ((s $1))
                   (if
                       (string-match "^{[
\n	 ]*" s)
                       (setq s
                             (substring s
                                        (match-end 0))))
                   (if
                       (string-match "[
\n	 ]*}$" s)
                       (setq s
                             (substring s 0
                                        (match-beginning 0))))
                   s))))
       (elements
        ((elements element)
         (cons $2 $1))
        ((element)
         (list $1)))
       (element
        ((action_opt SYMBOL)
         (cons $1 $2))))
     '(grammar code declaration nonterminal rule put_names put_values))
    )
  "Parser automaton.")

(defconst wisent-wy-keywords
  (identity
   ;;DO NOT EDIT! Generated from wisent-wy.wy - 2002-02-22 21:11+0100
   (semantic-flex-make-keyword-table
    '(("left" . LEFT)
      ("nonassoc" . NONASSOC)
      ("prec" . PREC)
      ("put" . PUT)
      ("right" . RIGHT)
      ("start" . START)
      ("token" . TOKEN)
      ("languagemode" . LANGUAGEMODE)
      ("outputfile" . OUTPUTFILE)
      ("setupfunction" . SETUPFUNCTION)
      ("keywordtable" . KEYWORDTABLE)
      ("parsetable" . PARSETABLE)
      ("tokentable" . TOKENTABLE))
    'nil)
   )
  "Keywords.")

(defun wisent-wy-setup-semantic ()
  "Setup buffer for parse."
  ;;DO NOT EDIT! Generated from wisent-wy.wy - 2002-02-22 21:11+0100
  (progn
    (setq semantic-toplevel-bovine-table wisent-wy-automaton
          ;; Use the Wisent's parser.
          semantic-bovinate-toplevel-override 'wisent-bovinate-toplevel
          ;; Language keywords
          semantic-flex-keywords-obarray wisent-wy-keywords
          ;; Numbers
          semantic-number-expression
          (concat "[-+]?\\([0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
                  "\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)")
          ;; Define the lexer
          wisent-lexer-function 'wisent-wy-lex
          ;; How `semantic-flex' will setup the lexer input stream.
          wisent-flex-depth 0
          ;; Case sensitive
          semantic-case-fold nil
          ;; Disable reporting of parse errors
          wisent-error-function 'ignore
          ;; Parent/Child separator
          semantic-type-relation-separator-character '(":")
          ;; Names
          semantic-symbol->name-assoc-list
          '(
            (code         . "Setup Code")
            (keyword      . "Keyword")
            (token        . "Token")
            (nonterminal  . "Nonterminal")
            (rule         . "Rule")
            )
          ;; Faces
          semantic-face-alist
          '(
            (code         . default)
            (keyword      . font-lock-keyword-face)
            (token        . font-lock-constant-face)
            (nonterminal  . font-lock-function-name-face)
            (rule         . default)
            )
          )

    (semantic-install-function-overrides
     '(
       ;;(abbreviate-nonterminal    . wisent-wy-abbreviate-nonterminal)
       ;;(summarize-nonterminal     . wisent-wy-summarize-nonterminal)
       ;;(eldoc-current-symbol-info . wisent-wy-ecsi)
       (nonterminal-children      . wisent-wy-nonterminal-children)
       )
     t))
  )

;;;;
;;;; The lexer
;;;;

(defun wisent-wy-lex ()
  "Return the next wisent grammar lexical token in input.
When the end of input is reached return (`wisent-eoi-term').  Each
lexical token has the form (TERMINAL VALUE START . END) where TERMINAL
is the terminal symbol for this token, VALUE is the string value of
the token, START and END are respectively the beginning and end
positions of the token in input."
  (if (null wisent-flex-istream)
      ;; End of input
      (list wisent-eoi-term)
    (let* ((is wisent-flex-istream)
           (tk (car is))
           (ft (car tk))
           lex x y)
      (cond
       
       ;; Keyword
       ;; -------
       ((setq x (semantic-flex-text tk)
              y (semantic-flex-keyword-p x))
        ;; Only a %thing is a true keyword
        (or (equal ?\% (char-before (semantic-flex-start tk)))
            (setq y 'SYMBOL))
        (setq lex (cons y (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Symbol
       ;; ------
       ((eq ft 'symbol)
        (setq x   (semantic-flex-text tk)
              lex (cons 'SYMBOL (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; String
       ;; ------
       ((eq ft 'string)
        (setq x   (semantic-flex-text tk)
              lex (cons 'STRING (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Number
       ;; ------
       ((eq ft 'number)
        (setq x   (semantic-flex-text tk)
              lex (cons 'NUMBER (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Punctuation
       ;; -----------
       ((and (eq ft 'punctuation)
             (setq x (char-after (semantic-flex-start tk))
                   y (cond ((eq x ?\:) 'COLON)
                           ((eq x ?\;) 'SEMI)
                           ((eq x ?\|) 'OR)
                           ((eq x ?\<) 'LT)
                           ((eq x ?\>) 'GT)
                           ((eq x ?\%) 'PERCENT))))
        (setq lex (cons y (cons x (cdr tk)))
              is (cdr is)))
       
       ;; Block
       ;; -----
       ((and (eq ft 'semantic-list)
             (setq x (char-after (semantic-flex-start tk))
                   y (cond ((eq x ?\() 'PAREN_BLOCK)
                           ((eq x ?\{) 'BRACE_BLOCK))))
        (setq x   (semantic-flex-text tk)
              lex (cons y (cons x (cdr tk)))
              is  (cdr is)))
              
       ;; Paren
       ;; -----
       ((and (memq ft '(open-paren close-paren))
             (setq x (char-after (semantic-flex-start tk))
                   y (cond ((eq x ?\{) 'LBRACE)
                           ((eq x ?\}) 'RBRACE))))
        (setq lex (cons y (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Unhandled
       ;; ---------
       (t
        ;; Other flex tokens are not yet used by the parser so for now
        ;; just return a lexical token: (ft nil start . end).  I think
        ;; this is better than raising a lexer error ;)
        (setq lex (cons ft (cons nil (cdr tk)))
              is  (cdr is))))
      
      (setq wisent-flex-istream is)
      lex)))

;;;; 
;;;; Semantic action expansion
;;;;

(defun wisent-wy-ASSOC (&rest args)
  "Return expansion of built-in ASSOC expression.
ARGS are ASSOC's key value list."
  (let ((key t))
    `(semantic-bovinate-make-assoc-list
      ,@(mapcar #'(lambda (i)
                    (prog1
                        (if key
                            (list 'quote i)
                          i)
                      (setq key (not key))))
                args))))

(defun wisent-wy-EXPANDTHING ($i nonterm expander)
  "Return expansion of built-in EXPAND/EXPANDFULL expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with.
EXPANDER is the Semantic function called to expand NONTERM"
  (let* ((n   (symbol-name $i))
         ($ri (and (string-match "^[$]\\([1-9][0-9]*\\)$" n)
                   (intern (concat "$region" (match-string 1 n))))))
    (if $ri
        `(,expander (car ,$ri) (cdr ,$ri) ',nonterm))))

(defun wisent-wy-EXPAND ($i nonterm)
  "Return expansion of built-in EXPAND expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (or (wisent-wy-EXPANDTHING
       $i nonterm 'wisent-bovinate-from-nonterminal)
      (error "Invalid form (EXPAND %s %s)" $i nonterm)))

(defun wisent-wy-EXPANDFULL ($i nonterm)
  "Return expansion of built-in EXPANDFULL expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (or (wisent-wy-EXPANDTHING
       $i nonterm 'wisent-bovinate-from-nonterminal-full)
      (error "Invalid form (EXPANDFULL %s %s)" $i nonterm)))

(defconst wisent-wy-builtins
  '(
    ;; Builtin name . Expander
    ;; ------------ . ---------------------------------
    (  ASSOC        . wisent-wy-ASSOC)
    (  EXPAND       . wisent-wy-EXPAND)
    (  EXPANDFULL   . wisent-wy-EXPANDFULL)
    ;; ------------ . ---------------------------------
    )
  "Expanders of Semantic built-in functions in LALR grammar.")

(defsubst wisent-wy-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst wisent-wy-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

(defun wisent-wy-expand-sexpr (expr)
  "Return expanded form of the expression EXPR.
`backquote' expressions and Semantic built-in function calls are
expanded.  The variable `wisent-wy-builtins' defines
built-in functions and corresponding expanders."
  (if (not (listp expr))
      ;; EXPR is an atom, no expansion needed
      expr
    ;; EXPR is a list, expand inside it
    (let (eexpr sexpr bltn)
      ;; If backquote expand it first
      (if (wisent-wy-backquote-p (car expr))
          (setq expr (macroexpand expr)))
      ;; Expand builtins
      (if (setq bltn (assq (car expr) wisent-wy-builtins))
          (setq expr (apply (cdr bltn) (cdr expr))))
      (while expr
        (setq sexpr (car expr)
              expr  (cdr expr))
        ;; Recursively expand function call but quote expression
        (and (consp sexpr)
             (not (wisent-wy-quote-p (car sexpr)))
             (setq sexpr (wisent-wy-expand-sexpr sexpr)))
        ;; Accumulate expanded forms
        (setq eexpr (nconc eexpr (list sexpr))))
      eexpr)))

;;;;
;;;; API to access `wy-mode' tokens
;;;;

(defun wisent-wy-nonterminal-children (token)
  "Return the children of TOKEN."
  (if (eq (semantic-token-token token) 'nonterminal)
      (nth 3 token)))

(defun wisent-wy-token-name (type)
  "Return the name of the first TYPE token found.
Warn if other TYPE tokens exist."
  (let* ((tokens (semantic-find-nonterminal-by-token
                  type (current-buffer))))
    (if tokens
        (prog1
            (semantic-token-name (car tokens))
          (if (cdr tokens)
              (message "*** Ignore all but first declared %s"
                       type))))))

(defun wisent-wy-token-symbols (type)
  "Return the list of symbols from names of TYPE tokens found."
  (let* ((tokens (semantic-find-nonterminal-by-token
                  type (current-buffer))))
    (apply #'append
           (mapcar #'(lambda (token)
                       (mapcar #'intern
                               (cons (semantic-token-name token)
                                     (nth 3 token))))
                   tokens))))

(defun wisent-wy-setupfunction ()
  "Return the %setupfunction value as a symbol or nil."
  (wisent-wy-token-name 'setupfunction))

(defun wisent-wy-setupcode ()
  "Return setupcode expressions as a string."
  (format "(progn\n%s)"
          (mapconcat
           #'(lambda (code)
               (let ((s (nth 3 code)))
                 (if (string-match "^{[\r\n\t ]*" s)
                     (setq s (substring s (match-end 0))))
                 (if (string-match "[\r\n\t ]*%?}$" s)
                     (setq s (substring s 0 (match-beginning 0))))
                 s))
           (semantic-find-nonterminal-by-token 'code (current-buffer))
           "\n")))

(defun wisent-wy-tokentable ()
  "Return the %tokentable value as a symbol or nil."
  (wisent-wy-token-name 'tokentable))

(defun wisent-wy-parsetable ()
  "Return the %parsetable value as a symbol or nil."
  (wisent-wy-token-name 'parsetable))

(defun wisent-wy-keywordtable ()
  "Return the %keywordtable value as a symbol or nil."
  (wisent-wy-token-name 'keywordtable))

(defun wisent-wy-languagemode ()
  "Return the %languagemode value as a symbol or nil."
  (wisent-wy-token-name 'languagemode))

(defun wisent-wy-start ()
  "Return the %start value as a symbol list or nil."
  (wisent-wy-token-symbols 'start))

(defun wisent-wy-left ()
  "Return the %left value as a symbol list or nil."
  (wisent-wy-token-symbols 'left))

(defun wisent-wy-right ()
  "Return the %right value as a symbol list or nil."
  (wisent-wy-token-symbols 'right))

(defun wisent-wy-nonassoc ()
  "Return the %nonassoc value as a symbol list or nil."
  (wisent-wy-token-symbols 'nonassoc))

(defun wisent-wy-outputfile ()
  "Return the %outputfile value as a string or nil."
  (wisent-wy-token-name 'outputfile))

(defun wisent-wy-keywords ()
  "Return the language keywords.
That is an alist of (VALUE . TOKEN) where VALUE is the string value of
the keyword and TOKEN is the terminal symbol identifying the keyword."
  (mapcar
   #'(lambda (k) (cons (nth 3 k) (intern (semantic-token-name k))))
   (semantic-find-nonterminal-by-token 'keyword (current-buffer))))

(defun wisent-wy-keyword-properties (keywords)
  "Return the list of KEYWORDS properties."
  (let ((puts (semantic-find-nonterminal-by-token
               'put (current-buffer)))
        put keys key plist assoc pkey pval props)
    (while puts
      (setq put   (car puts)
            puts  (cdr puts)
            keys  (mapcar
                   #'intern
                   (cons (semantic-token-name put) (nth 3 put))))
      (while keys
        (setq key   (car keys)
              keys  (cdr keys)
              assoc (rassq key keywords))
        (if (null assoc)
            (message "*** %%put to undefined keyword %s ignored" key)
          (setq key   (car assoc)
                plist (nth 4 put))
          (while plist
            (setq pkey  (intern (caar plist))
                  pval  (read (cdar plist))
                  props (cons (list key pkey pval) props)
                  plist (cdr plist))))))
    props))

(defun wisent-wy-tokens ()
  "Return defined tokens.
That is an alist (TYPE . DEFS) where type is a %token <type> symbol
and DEFS is an alist of (TOKEN . VALUE).  TOKEN is the terminal symbol
identifying the token and VALUE is the string value of the token or
nil."
  (let ((tokens (semantic-find-nonterminal-by-token
                 'token (current-buffer)))
        alist assoc token type name value)
    (while tokens
      (setq token  (car tokens)
            tokens (cdr tokens))
        (setq name  (semantic-token-name token)
              type  (nth 2 token)
              value (nth 3 token)
              type  (if type (intern type))
              assoc (assq type alist))
        (or assoc (setq assoc (list type)
                        alist (cons assoc alist)))
        (if (string-equal name "")
            ;; multiple token in one declaration
            (while value
              (setq name  (intern (car value))
                    value (cdr value))
              (setcdr assoc (cons (list name) (cdr assoc))))
          (setcdr assoc (cons (cons (intern name) value)
                              (cdr assoc)))))
    alist))

(defun wisent-wy-terminals ()
  "Return the list of terminal symbols."
  (let ((tokens (semantic-find-nonterminal-by-token
                 'token (current-buffer)))
        token name terms)
    (while tokens
      (setq token  (car tokens)
            tokens (cdr tokens)
            name   (semantic-token-name token))
      (if (string-equal name "")
          (progn
            (setq name (nth 3 token))
            (while name
              (setq terms (cons (intern (car name)) terms)
                    name  (cdr name))))
        (setq terms (cons (intern name) terms))))
    (nconc
     (mapcar
      #'(lambda (k) (intern (semantic-token-name k)))
      (semantic-find-nonterminal-by-token 'keyword (current-buffer)))
     (nreverse terms))))

(defun wisent-wy-nonterminals ()
  "Return the list form of nonterminal definitions."
  (let ((nttoks (semantic-find-nonterminal-by-token
                 'nonterminal (current-buffer)))
        rltoks nterms rules rule elems actn sexp prec)
    (while nttoks
      (setq rltoks (semantic-nonterminal-children (car nttoks))
            rules  nil)
      (while rltoks
        (setq elems  (nth 3 (car rltoks))
              rule   nil)
        (if (not elems)
            nil ;; EMPTY rule
          (while elems
            (if (caar elems)
                (setq actn (wisent-wy-expand-sexpr (read (caar elems)))
                      rule (cons actn rule)))
            (setq rule (cons (intern (cdar elems)) rule)
                  elems (cdr elems)))
          (setq rule (nreverse rule)))
        (setq actn (nth 4 (car rltoks))
              prec (nth 5 (car rltoks)))
        (if prec
            (setq prec (vector (intern prec))))
        (if actn
            (setq sexp (wisent-wy-expand-sexpr (read actn))))
        (setq rule (if actn
                       (if prec
                           (list rule prec sexp)
                         (list rule sexp))
                     (if prec
                         (list rule prec)
                       (list rule))))
        (setq rules (cons rule rules)
              rltoks (cdr rltoks)))
      (setq nterms (cons (cons (intern (semantic-token-name (car nttoks)))
                               (nreverse rules))
                         nterms)
            nttoks (cdr nttoks)))
    (nreverse nterms)))

(defun wisent-wy-grammar ()
  "Return Elisp form of the grammar."
  (let* ((terminals    (wisent-wy-terminals))
         (nonterminals (wisent-wy-nonterminals))
         (left         (wisent-wy-left))
         (right        (wisent-wy-right))
         (nonassoc     (wisent-wy-nonassoc)))
    (if left
        (setq left (cons 'left left)))
    (if right
        (setq right (cons 'right right)))
    (if nonassoc
        (setq nonassoc (cons 'nonassoc nonassoc)))
    (cons terminals
          (cons (delq nil (list left right nonassoc))
                nonterminals))))

;;;;
;;;; Lisp code generation
;;;;

(defconst wisent-wy-autogen-cookie
  ";;DO NOT EDIT! Generated from")

(defconst wisent-wy-autogen-cookie-re
  (format "^\\s-*%s\\s-*" (regexp-quote wisent-wy-autogen-cookie)))

(defvar wisent-wy-buffer)

(defun wisent-wy-beginning-of-code ()
  "Move the point to the beginning of code in current buffer.
That is after any header comments and `require' statements."
  (let (last)
    (goto-char (point-min))
    (forward-comment (point-max))
    (setq last (point))
    (while (looking-at "^(require\\s-+")
      (forward-sexp)
      (setq last (point))
      (forward-comment (point-max)))
    (goto-char last)
    (and (eolp) (not (bolp)) (newline))))

(defun wisent-wy-beginning-of-body ()
  "Move point to the beginning of the body of the function at point.
Skip docstring and `interactive' form if present.  If there are
comment lines before the first statement move point to the beginning
of the first line of comment."
  (interactive)
  (beginning-of-defun)
  ;; Skip `defun' and function name
  (re-search-forward "(defun\\s-*\\(\\sw\\|\\s_\\)+\\s-*")
  ;; Skip arglist
  (forward-sexp)
  ;; Skip spaces and comments
  (forward-comment (point-max))
  ;; Maybe skip docstring
  (if (looking-at "\\s\"")
      (progn
        (forward-sexp)
        ;; Skip spaces and comments
        (forward-comment (point-max))))
  ;; Maybe skip `interactive' form
  (if (looking-at "\\s([ \r\n\t]*\\binteractive\\b")
      (progn
        (forward-list)
        ;; Skip spaces and comments
        (forward-comment (point-max))))
  ;; Now move back to the first line of comments before this statement
  (forward-comment (- (point-max)))
  ;; Maybe skip line comment
  (if (looking-at "\\s-*\\(\\s<\\)")
      (forward-comment 1))
  ;; Move point to the beginning of comment or statement
  (skip-chars-forward "[ \n\r\t]"))

(defmacro wisent-wy-with-outputfile (&rest body)
  "Execute BODY in outputfile buffer."
  `(save-excursion
     (with-current-buffer
         (find-file-noselect
          (or (wisent-wy-outputfile)
              (error "A %%outputfile declaration is required")))
       (pop-to-buffer (current-buffer))
       (goto-char (point-min))
       ,@ body)))

(defmacro wisent-wy-with-wy-buffer (&rest body)
  "Execute BODY in current WY file buffer."
  `(save-excursion
     (with-current-buffer wisent-wy-buffer
       ,@ body)))

(defsubst wisent-wy-autogen-cookie ()
  "Return a cookie comment identifying generated code."
  (format "%s %s - %s"
          wisent-wy-autogen-cookie
          (buffer-name wisent-wy-buffer)
          (format-time-string "%Y-%m-%d %R%z")))

(defun wisent-wy-parsetable-value ()
  "Return the string value of the parser table."
   (format "(eval-when-compile\n%s\n%s)\n"
           (wisent-wy-autogen-cookie)
           (pp-to-string
            (wisent-wy-with-wy-buffer
             `(wisent-compile-grammar
               ',(wisent-wy-grammar)
               ',(wisent-wy-start))))))

(defun wisent-wy-keywordtable-value ()
  "Return the string value of the table of keywords."
  (format "(identity\n%s\n%s)\n"
          (wisent-wy-autogen-cookie)
          (pp-to-string
           (wisent-wy-with-wy-buffer
            (let ((keywords (wisent-wy-keywords)))
              `(semantic-flex-make-keyword-table
                ',keywords
                ',(wisent-wy-keyword-properties keywords)))))))

(defun wisent-wy-tokentable-value ()
  "Return the string value of the table of tokens."
   (format "(identity\n%s\n%s)\n"
           (wisent-wy-autogen-cookie)
           (pp-to-string
            (wisent-wy-with-wy-buffer
             `(quote ,(wisent-wy-tokens))))))

(defun wisent-wy-update-def (def comment &optional noerror)
  "Create or update the Lisp declaration for %DEF.
Use COMMENT when a new definition is created.
If NOERROR is non-nil then does nothing if there is no %DEF."
  (let ((def-name-fun (intern (format "wisent-wy-%s" def)))
        (def-value-fun (intern (format "wisent-wy-%s-value" def)))
        table)
    (if (not (fboundp def-name-fun))
        (error "Function %s not found" def-name-fun))
    (if (not (fboundp def-value-fun))
        (error "Function %s not found" def-value-fun))
    (if (not (setq table (funcall def-name-fun)))
        (or noerror
            (error "A %%%s declaration is required" def))
      (wisent-wy-with-outputfile
       (if (re-search-forward
            (format "^(def\\(var\\|const\\)[\r\n\t ]+%s\\b[\r\n\t ]+"
                    (regexp-quote table))
            nil t)
           ;; Update definition
           (progn
             (kill-region (point)
                          (progn (forward-sexp)
                                 (skip-chars-forward "\r\n\t ")
                                 (point)))
             (insert (funcall def-value-fun)))
         ;; Insert a new `defconst' at the beginning of code
         (wisent-wy-beginning-of-code)
         (insert
          (format "(defconst %s\n%s%S)\n\n"
                  table (funcall def-value-fun) comment)))
       (re-search-backward "^(def\\(var\\|const\\)\\s-+")
       (indent-sexp)))))
  
(defun wisent-wy-update-parsetable ()
  "Create or update the parsetable Lisp declaration."
  (wisent-wy-update-def 'parsetable "Parser automaton."))

(defun wisent-wy-update-keywordtable ()
  "Create or update the keywordtable Lisp declaration."
  (wisent-wy-update-def 'keywordtable "Keywords." t))

(defun wisent-wy-update-tokentable ()
  "Create or update the tokentable Lisp declaration."
  (wisent-wy-update-def 'tokentable "Tokens." t))

(defun wisent-wy-update-setupfunction ()
  "Create or update the setupfunction Lisp code."
  (let ((fun  (wisent-wy-setupfunction))
        (code (wisent-wy-setupcode)))
    (when (and fun code)
      (wisent-wy-with-outputfile
       (if (re-search-forward
            (format "^(defun[\r\n\t ]+%s\\b[\r\n\t ]+"
                    (regexp-quote fun))
            nil t)
           ;; Update setup code
           (let* ((eod (save-excursion (end-of-defun) (point))))
             (if (re-search-forward wisent-wy-autogen-cookie-re eod t)
                 ;; Replace existing one
                 (progn
                   (beginning-of-line)
                   (kill-region (point)
                                (progn (forward-comment (point-max))
                                       (forward-sexp)
                                       (skip-chars-forward "\r\n\t ")
                                       (point))))
               ;; Insert new one
               (goto-char eod)
               (wisent-wy-beginning-of-body)
               (and (eolp) (not (bolp)) (newline)))
             (insert
              (format "%s\n%s\n" (wisent-wy-autogen-cookie) code)))
         ;; Insert a new `defun' at the beginning of code
         (wisent-wy-beginning-of-code)
         (insert
          (format "(defun %s ()\n%S\n%s\n%s\n)\n\n"
                  "Setup buffer for parse."
                  fun (wisent-wy-autogen-cookie) code)))
       (re-search-backward "^(defun\\s-+")
       (indent-sexp)))))

(defun wisent-wy-update-outputfile ()
  "Create or update grammar Lisp code in outputfile."
  (interactive)
  (let ((wisent-wy-buffer (current-buffer)))
    (semantic-bovinate-toplevel t)
    (wisent-wy-update-setupfunction)
    (wisent-wy-update-tokentable)
    (wisent-wy-update-keywordtable)
    (wisent-wy-update-parsetable)))

;;;;
;;;; Define major mode
;;;;

(defvar wisent-wy-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\: "."     table) ;; COLON
    (modify-syntax-entry ?\> "."     table) ;; GT
    (modify-syntax-entry ?\< "."     table) ;; LT
    (modify-syntax-entry ?\| "."     table) ;; OR
    (modify-syntax-entry ?\% "."     table) ;; PERCENT
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    table)
  "Syntax table used in a WY buffer.")

(defvar wisent-wy-elisp-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    table)
  "Syntax table used to indent Elisp code.
This is a modified copy of `emacs-lisp-mode-syntax-table' with
brackets considered as parenthesis.")

(defvar wy-mode-hook nil
  "Hook run when starting WY mode.")

(defvar wisent-wy-mode-keywords-1
  `(("^\\(\\w+\\)[ \n\r\t]*:" 1 font-lock-function-name-face)
    ("(\\s-*\\(ASSOC\\|EXPAND\\(FULL\\)?\\)\\>"
     1 ,(if (featurep 'xemacs)
            'font-lock-preprocessor-face
          'font-lock-builtin-face))
    ("\\$\\(\\sw\\|\\s_\\)*" 0 font-lock-variable-name-face)
    ("%" 0 font-lock-reference-face)
    ("%\\(\\w+\\)" 1 font-lock-type-face)
    )
  "Font Lock keywords used to highlight WY buffer.")

(defvar wisent-wy-mode-keywords-2
  (append wisent-wy-mode-keywords-1
          lisp-font-lock-keywords-1)
  "Font Lock keywords used to highlight WY buffer.")

(defvar wisent-wy-mode-keywords-3
  (append wisent-wy-mode-keywords-2
          lisp-font-lock-keywords-2)
  "Font Lock keywords used to highlight WY buffer.")

(defvar wisent-wy-mode-keywords
  wisent-wy-mode-keywords-1
  "Font Lock keywords used to highlight WY buffer.")

(defvar wisent-wy-map
  (let ((km (make-sparse-keymap)))
    
    (define-key km "|" 'wisent-wy-electric-punctuation)
    (define-key km ";" 'wisent-wy-electric-punctuation)
    (define-key km "%" 'wisent-wy-electric-punctuation)
    (define-key km "(" 'wisent-wy-electric-punctuation)
    (define-key km ")" 'wisent-wy-electric-punctuation)
    
    (define-key km "\t"       'wisent-wy-indent)
    (define-key km "\C-c\C-c" 'wisent-wy-update-outputfile)
;;  (define-key km "\C-cc"    'wisent-wy-generate-and-load)
;;  (define-key km "\C-cr"    'wisent-wy-generate-one-rule)
;;  (define-key km "\M-\t"    'wisent-wy-complete)
    
    km)
  "Keymap used in `wisent-wy-mode'.")

(defalias 'wy-mode 'wisent-wy-mode)

;;;###autoload
(defun wisent-wy-mode ()
  "Initialize a buffer for editing WY grammar code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wisent-wy-mode
	mode-name "WY")
  (make-local-variable 'comment-start)
  (setq comment-start ";;")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  ;;(setq comment-start-skip ";;+")
  (set-syntax-table wisent-wy-syntax-table)
  (use-local-map wisent-wy-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'wisent-wy-indent)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function #'lisp-fill-paragraph)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((wisent-wy-mode-keywords
           wisent-wy-mode-keywords-1
           wisent-wy-mode-keywords-2
           wisent-wy-mode-keywords-3)
          nil ;; perform string/comment fontification
          nil ;; keywords are case sensitive.
          ;; This puts _ & - as a word constituant,
          ;; simplifying our keywords significantly
          ((?_ . "w") (?- . "w"))))
  ;; Set up Semantic environment
  (wisent-wy-setup-semantic)
  (run-hooks 'wy-mode-hook))

(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-wy-mode))
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".wy"))

;;;;
;;;; Useful commands
;;;;

(defun wisent-wy-skip-comments-backward ()
  "Move point backward, stopping after comments and whitespaces."
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (while (nth 4 (parse-partial-sexp bol (point)))
      (re-search-backward ";;"))
    (forward-comment (- (point-max)))))

(defun wisent-wy-goto-grammar-indent-anchor ()
  "Move the point to current grammar indent anchor.
That is just after the previous percent, colon or semicolon character
found, taking care of comments and Lisp code.  Return the column where
the anchor is or nil if the point has not moved."
    (condition-case nil
        (let ((found nil))
          (save-excursion
            ;; Escape Lisp code
            (wisent-wy-skip-comments-backward)
            (condition-case nil
                (while t (up-list -1))
              (error nil))
            ;; Search for previous [%;:]
            (while (not found)
              (wisent-wy-skip-comments-backward)
              (cond
               ((memq (char-before) '(?\% ?\: ?\;))
                (setq found (point)))
               ((bobp)
                (error "")))
              (forward-sexp -1)))
          (goto-char found)
          (1- (current-column)))
      (error nil)))

(defun wisent-wy-grammar-compute-indentation ()
  "Compute indentation of the current line of grammar."
  (save-excursion
    (beginning-of-line)
    (if (or (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
            (looking-at "\\s-*%"))
        0
      (let* ((p (point))
             (i (wisent-wy-goto-grammar-indent-anchor)))
        (if (not (and i (eq (char-before) ?\:)))
            0
          (if (or (looking-at "\\s-*$")
                  (save-excursion (beginning-of-line)
                                  (looking-at "\\s-*:")))
              (setq i 2))
          (goto-char p)
          (cond ((looking-at "\\s-*;;")
                 (1- i))
                ((looking-at "\\s-*[|;]")
                 i)
                (t
                 (+ i 2))))))))
      
(defun wisent-wy-do-grammar-indent ()
  "Indent a line of grammar.
When called the point is not in Lisp code."
  (let ((indent (wisent-wy-grammar-compute-indentation)))
    (if (/= (current-indentation) indent)
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to indent)))))

(defun wisent-wy-do-lisp-indent ()
  "Maybe run the Emacs Lisp indenter on a line of code.
Return nil if not in a Lisp expression."
    (condition-case nil
        (save-excursion
          (beginning-of-line)
          (skip-chars-forward "\t ")
          (let ((first (point)))
            (up-list -1)
            (condition-case nil
                (while t
                  (up-list -1))
              (error nil))
            (beginning-of-line)
            (save-restriction
              (narrow-to-region (point) first)
              (goto-char (point-max))
              (with-syntax-table wisent-wy-elisp-syntax-table
                (lisp-indent-line))))
          t)
      (error nil)))

(defun wisent-wy-indent ()
  "Indent the current line.
Use the Lisp or grammar indenter depending on point location."
  (interactive)
  (let ((orig (point))
        first)
    (or (wisent-wy-do-lisp-indent)
        (wisent-wy-do-grammar-indent))
    (setq first (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward "\t ")
                  (point)))
    (if (or (< orig first) (/= orig (point)))
        (goto-char first))))

(defun wisent-wy-electric-punctuation ()
  "Insert and reindent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    (wisent-wy-indent)))

(provide 'wisent-wy)

;;; wisent-wy.el ends here
