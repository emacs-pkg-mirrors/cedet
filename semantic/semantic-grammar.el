;;; semantic-grammar.el --- Major mode framework for Semantic grammars
;;
;; Copyright (C) 2002, 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: semantic-grammar.el,v 1.12 2003/02/20 08:28:48 ponced Exp $
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
;; Major mode framework for editing Semantic's input grammar files.

;;; History:
;; 

;;; Code:
(require 'wisent-bovine)

(eval-when-compile
  (require 'font-lock)
  (require 'semantic-edit))

;;;;
;;;; Set up lexer
;;;;

;;; Analyzers
;;
(define-lex-regex-analyzer semantic-grammar-lex-symbol
  "Detect and create an identifier or keyword token."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0))
	'SYMBOL)
    (match-beginning 0)
    (match-end 0))))

(define-lex-regex-analyzer semantic-grammar-lex-string
  "Detect and create a string token."
  "\\s\""
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'STRING (point)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'STRING
	(forward-sexp 1)
	(point))
      )))
  )

(defconst semantic-grammar-lex-c-char-re "'\\s\\?.'"
  "Regexp matching C-like character literals.")

(define-lex-simple-regex-analyzer semantic-grammar-lex-char
  "Detect and create a C-like character token."
  semantic-grammar-lex-c-char-re 'CHARACTER)

(define-lex-block-analyzer semantic-grammar-lex-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  )

(define-lex-analyzer semantic-grammar-lex-sexp
  "Detect and create an s-expression token."
  t
  (semantic-lex-push-token
   (semantic-lex-token
    'SEXP
    (match-beginning 0)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'SEXP
	(forward-sexp 1)
	(point))
      ))))

;;; Lexer
;;
(define-lex semantic-grammar-lexer
  "Lexical analyzer that handles Semantic grammar buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-newline
  semantic-lex-ignore-whitespace
  semantic-grammar-lex-symbol
  semantic-grammar-lex-char
  semantic-grammar-lex-string
  ;; Must detect comments after strings because `comment-start-skip'
  ;; regexp match semicolons inside strings!
  semantic-lex-ignore-comments
  ;; Must detect punctuations after comments because the semicolon can
  ;; be a punctuation or a comment start!
  semantic-lex-punctuation-type
  semantic-grammar-lex-blocks
  semantic-grammar-lex-sexp)

;;; Test the lexer
;;
(defun semantic-grammar-lex-buffer ()
  "Run `semantic-grammar-lex' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'semantic-grammar-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer (get-buffer-create "*semantic-grammar-lex*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;;
;;;; Set up parser
;;;;

(defconst semantic-grammar-automaton
  ;;DO NOT EDIT! Generated from semantic-grammar.wy - 2003-02-18 22:35+0100
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((LEFT NONASSOC PREC PUT RIGHT START SCOPESTART QUOTEMODE TOKEN LANGUAGEMODE OUTPUTFILE SETUPFUNCTION KEYWORDTABLE PARSETABLE TOKENTABLE STRING SYMBOL CHARACTER SEXP PAREN_BLOCK BRACE_BLOCK LBRACE RBRACE COLON SEMI OR LT GT PERCENT)
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
        ((scopestart_decl))
        ((quotemode_decl))
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
        ((SETUPFUNCTION any_symbol)
         (list $2 'setupfunction nil nil)))
       (keywordtable_decl
        ((KEYWORDTABLE any_symbol)
         (list $2 'keywordtable nil nil)))
       (parsetable_decl
        ((PARSETABLE any_symbol)
         (list $2 'parsetable nil nil)))
       (tokentable_decl
        ((TOKENTABLE any_symbol)
         (list $2 'tokentable nil nil)))
       (token_decl
        ((TOKEN token_type_opt any_symbol string_value)
         (list $3
               (if $2 'token 'keyword)
               $2 nil $4 nil))
        ((TOKEN token_type_opt symbols)
         (list
          (car $3)
          'token $2
          (cdr $3)
          nil nil)))
       (token_type_opt
        (nil)
        ((token_type)))
       (token_type
        ((LT any_symbol GT)
         (identity $2)))
       (start_decl
        ((START symbols)
         (list
          (car $2)
          'start nil
          (cdr $2)
          nil)))
       (scopestart_decl
        ((SCOPESTART any_symbol)
         (list $2 'scopestart nil nil)))
       (quotemode_decl
        ((QUOTEMODE any_symbol)
         (list $2 'quotemode nil nil)))
       (left_decl
        ((LEFT token_type_opt items)
         (list $1 'assoc $2 $3 nil)))
       (right_decl
        ((RIGHT token_type_opt items)
         (list $1 'assoc $2 $3 nil)))
       (nonassoc_decl
        ((NONASSOC token_type_opt items)
         (list $1 'assoc $2 $3 nil)))
       (put_decl
        ((PUT any_symbol put_value)
         (list $2 'put nil nil
               (list $3)
               nil))
        ((PUT any_symbol put_value_list)
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
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'put_names 1)))
       (put_names
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((any_symbol)
         (wisent-token $1 'put-name nil nil)))
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
         (wisent-token "" 'put-value nil $1 nil)))
       (put_value
        ((any_symbol any_value)
         (cons $1 $2)))
       (any_value
        ((any_symbol))
        ((STRING))
        ((PAREN_BLOCK))
        ((SEXP)))
       (symbols
        ((lifo_symbols)
         (nreverse $1)))
       (lifo_symbols
        ((lifo_symbols any_symbol)
         (cons $2 $1))
        ((any_symbol)
         (list $1)))
       (nonterminal
        ((any_symbol COLON rules SEMI)
         (wisent-token $1 'nonterminal nil $3 nil)))
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
           (wisent-cooked-token name 'rule type comps prec action nil))))
       (rhs
        (nil)
        ((rhs item)
         (cons $2 $1))
        ((rhs action)
         (cons
          (list $2)
          $1))
        ((rhs level)
         (cons
          (vector $2)
          $1)))
       (level
        ((PERCENT PREC item)
         (identity $3)))
       (action
        ((PAREN_BLOCK))
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
        ((any_symbol))
        ((CHARACTER)))
       (any_symbol
        ((SYMBOL))
        ((LEFT))
        ((NONASSOC))
        ((PREC))
        ((PUT))
        ((RIGHT))
        ((START))
        ((SCOPESTART))
        ((QUOTEMODE))
        ((TOKEN))
        ((LANGUAGEMODE))
        ((OUTPUTFILE))
        ((SETUPFUNCTION))
        ((KEYWORDTABLE))
        ((PARSETABLE))
        ((TOKENTABLE))))
     '(grammar code declaration nonterminal rule put_names put_values)))
  "Parser automaton.")

(defconst semantic-grammar-keywords
  ;;DO NOT EDIT! Generated from semantic-grammar.wy - 2003-02-18 22:35+0100
  (semantic-lex-make-keyword-table
   '(("left" . LEFT)
     ("nonassoc" . NONASSOC)
     ("prec" . PREC)
     ("put" . PUT)
     ("right" . RIGHT)
     ("start" . START)
     ("scopestart" . SCOPESTART)
     ("quotemode" . QUOTEMODE)
     ("token" . TOKEN)
     ("languagemode" . LANGUAGEMODE)
     ("outputfile" . OUTPUTFILE)
     ("setupfunction" . SETUPFUNCTION)
     ("keywordtable" . KEYWORDTABLE)
     ("parsetable" . PARSETABLE)
     ("tokentable" . TOKENTABLE))
   'nil)
  "Keywords.")

(defconst semantic-grammar-tokens
  ;;DO NOT EDIT! Generated from semantic-grammar.wy - 2003-02-18 22:35+0100
  (wisent-lex-make-token-table
   '(("punctuation"
      (PERCENT . "%")
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
     ("sexp"
      (SEXP))
     ("char"
      (CHARACTER))
     ("symbol"
      (SYMBOL))
     ("string"
      (STRING)))
   'nil)
  "Tokens.")

(defun semantic-grammar-setup-semantic ()
  "Setup buffer for parse."
  ;;DO NOT EDIT! Generated from semantic-grammar.wy - 2003-02-18 22:35+0100
  (progn
    (semantic-install-function-overrides
     '((parse-stream . wisent-parse-stream)))
    (setq semantic-parser-name "LALR"
          semantic-toplevel-bovine-table semantic-grammar-automaton
          semantic-debug-parser-source "semantic-grammar.wy"
          semantic-flex-keywords-obarray semantic-grammar-keywords
          semantic-lex-types-obarray semantic-grammar-tokens)
    ;; Collect unmatched syntax lexical tokens
    (semantic-make-local-hook 'wisent-discarding-token-functions)
    (add-hook 'wisent-discarding-token-functions
              'wisent-collect-unmatched-syntax nil t)
    (setq
     ;; Lexical analysis
     semantic-lex-comment-regex ";;"
     semantic-lex-analyzer 'semantic-grammar-lexer
     ;; Environment
     semantic-type-relation-separator-character '(":")
     semantic-symbol->name-assoc-list
     '(
       (code         . "Setup Code")
       (keyword      . "Keyword")
       (token        . "Token")
       (nonterminal  . "Nonterminal")
       (rule         . "Rule")
       )
     semantic-face-alist
     '(
       (code         . default)
       (keyword      . font-lock-keyword-face)
       (token        . font-lock-type-face)
       (nonterminal  . font-lock-function-name-face)
       (rule         . default)
       )
     )))

;; semantic overloaded functions
(semantic-install-function-overrides
 '(
   ;;(abbreviate-nonterminal    . semantic-grammar-abbreviate-nonterminal)
   ;;(summarize-nonterminal     . semantic-grammar-summarize-nonterminal)
   ;;(eldoc-current-symbol-info . semantic-grammar-ecsi)
   (nonterminal-children      . semantic-grammar-nonterminal-children)
   )
 t 'semantic-grammar-mode)

(defun semantic-grammar-edits-new-change-hook-fcn (overlay)
  "Function set into `semantic-edits-new-change-hook'.
Argument OVERLAY is the overlay created to mark the change.
When OVERLAY marks a change in the scope of a nonterminal token extend
the change bounds to encompass the whole nonterminal token."
  (let ((outer (car (semantic-find-nonterminal-by-overlay-in-region
                     (semantic-edits-os overlay)
                     (semantic-edits-oe overlay)))))
    (if (eq 'nonterminal (semantic-token-token outer))
        (semantic-overlay-move overlay
                               (semantic-token-start outer)
                               (semantic-token-end outer)))))

;;;; 
;;;; Semantic action expansion
;;;;

(defun semantic-grammar-ASSOC (&rest args)
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

(defsubst semantic-grammar-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst semantic-grammar-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

;;;;
;;;; API to access grammar tokens
;;;;

(defun semantic-grammar-nonterminal-children (token)
  "Return the children of TOKEN."
  (if (eq (semantic-token-token token) 'nonterminal)
      (nth 3 token)))

(defun semantic-grammar-token-name (type)
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

(defun semantic-grammar-token-symbols (type)
  "Return the list of symbols from names of TYPE tokens found."
  (let* ((tokens (semantic-find-nonterminal-by-token
                  type (current-buffer))))
    (apply #'append
           (mapcar #'(lambda (token)
                       (mapcar #'intern
                               (cons (semantic-token-name token)
                                     (nth 3 token))))
                   tokens))))

(defsubst semantic-grammar-item-text (item)
  "Return the readable string form of ITEM."
  (if (string-match semantic-grammar-lex-c-char-re item)
      (concat "?" (substring item 1 -1))
    item))

(defsubst semantic-grammar-item-value (item)
  "Return symbol or character value of ITEM string."
  (if (string-match semantic-grammar-lex-c-char-re item)
      (read (concat "?" (substring item 1 -1)))
    (intern item)))

(defsubst semantic-grammar-setupfunction ()
  "Return the %setupfunction value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'setupfunction) "nil")))

(defun semantic-grammar-code-value (token)
  "Return value of the code TOKEN as a list of expressions."
  (let* ((s (nth 3 token))
         rdata form start end)
    (if (string-match "^{[\r\n\t ]*" s)
        (setq start (match-end 0)))
    (if (string-match "[\r\n\t ]*%?}$" s start)
        (setq end (match-beginning 0)))
    (condition-case nil
        (while (setq rdata (read-from-string s start end))
          (setq form (cons (car rdata) form)
                start (cdr rdata)))
      (error nil))
    (nreverse form)))

(defsubst semantic-grammar-setupcode-forms ()
  "Return grammar setup code as a list of expressions."
  (apply #'nconc
         (mapcar #'semantic-grammar-code-value
                 (semantic-find-nonterminal-by-token
                  'code (current-buffer)))))

(defsubst semantic-grammar-setupcode-text ()
  "Return grammar setup code as a string value."
  (mapconcat
   #'(lambda (code)
       (let* ((s (nth 3 code))
              start end)
         (if (string-match "^{[\r\n\t ]*" s)
             (setq start (match-end 0)))
         (if (string-match "[\r\n\t ]*%?}$" s)
             (setq start (or start 0)
                   end   (match-beginning 0)))
         (if start
             (substring s start end)
           s)))
   (semantic-find-nonterminal-by-token 'code (current-buffer))
   "\n"))

(defsubst semantic-grammar-tokentable ()
  "Return the %tokentable value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'tokentable) "nil")))

(defsubst semantic-grammar-parsetable ()
  "Return the %parsetable value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'parsetable) "nil")))

(defsubst semantic-grammar-keywordtable ()
  "Return the %keywordtable value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'keywordtable) "nil")))

(defsubst semantic-grammar-languagemode ()
  "Return the %languagemode value as a symbol or nil."
  (semantic-grammar-token-symbols 'languagemode))

(defsubst semantic-grammar-start ()
  "Return the %start value as a symbol list or nil."
  (semantic-grammar-token-symbols 'start))

(defsubst semantic-grammar-scopestart ()
  "Return the %scopestart value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'scopestart) "nil")))

(defsubst semantic-grammar-quotemode ()
  "Return the %quotemode value as a symbol or nil."
  (intern (or (semantic-grammar-token-name 'quotemode) "nil")))

(defsubst semantic-grammar-outputfile ()
  "Return the %outputfile value as a string or nil."
  (semantic-grammar-token-name 'outputfile))

(defsubst semantic-grammar-keywords ()
  "Return the language keywords.
That is an alist of (VALUE . TOKEN) where VALUE is the string value of
the keyword and TOKEN is the terminal symbol identifying the keyword."
  (mapcar
   #'(lambda (key)
       (cons (nth 4 key) (intern (semantic-token-name key))))
   (semantic-find-nonterminal-by-token 'keyword (current-buffer))))

(defun semantic-grammar-keyword-properties (keywords)
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
            nil ;;(message "*** %%put to undefined keyword %s ignored" key)
          (setq key   (car assoc)
                plist (nth 4 put))
          (while plist
            (setq pkey  (intern (caar plist))
                  pval  (read (cdar plist))
                  props (cons (list key pkey pval) props)
                  plist (cdr plist))))))
    props))

(defun semantic-grammar-tokens ()
  "Return defined tokens.
That is an alist (TYPE . DEFS) where type is a %token <type> symbol
and DEFS is an alist of (TOKEN . VALUE).  TOKEN is the terminal symbol
identifying the token and VALUE is the string value of the token or
nil."
  (let (tokens alist assoc token type term names value)
    
    ;; Check for <type> in %left, %right & %nonassoc declarations
    (setq tokens (semantic-find-nonterminal-by-token
                 'assoc (current-buffer)))
    (while tokens
      (setq token  (car tokens)
            tokens (cdr tokens))
      (when (setq type (nth 2 token))
        (setq names (nth 3 token)
              assoc (assoc type alist))
        (or assoc (setq assoc (list type)
                        alist (cons assoc alist)))
        (while names
          (setq term  (car names)
                names (cdr names))
          (or (string-match semantic-grammar-lex-c-char-re term)
              (setcdr assoc (cons (list (intern term))
                                  (cdr assoc)))))))
    
    ;; Then process %token declarations so they can override any
    ;; previous specifications
    (setq tokens (semantic-find-nonterminal-by-token
                  'token (current-buffer)))
    (while tokens
      (setq token  (car tokens)
            tokens (cdr tokens))
        (setq names (cons (semantic-token-name token) (nth 3 token))
              type  (or (nth 2 token) "<no-type>")
              value (nth 4 token)
              assoc (assoc type alist))
        (or assoc (setq assoc (list type)
                        alist (cons assoc alist)))
        (while names
          (setq term  (intern (car names))
                names (cdr names))
          (setcdr assoc (cons (cons term value) (cdr assoc)))))
    alist))

(defun semantic-grammar-token-properties (tokens)
  "Return the list of TOKENS properties."
  (let ((puts (semantic-find-nonterminal-by-token
               'put (current-buffer)))
        put keys key plist assoc pkey pval props)
    (while puts
      (setq put   (car puts)
            puts  (cdr puts)
            keys  (cons (semantic-token-name put) (nth 3 put)))
      (while keys
        (setq key   (car keys)
              keys  (cdr keys)
              assoc (assoc key tokens))
        (if (null assoc)
            nil ;; (message "*** %%put to undefined token %s ignored" key)
          (setq key   (car assoc)
                plist (nth 4 put))
          (while plist
            (setq pkey  (intern (caar plist))
                  pval  (read (cdar plist))
                  props (cons (list key pkey pval) props)
                  plist (cdr plist))))))
    props))

;;;;
;;;; Lisp code generation
;;;;

(defconst semantic-grammar-autogen-cookie
  ";;DO NOT EDIT! Generated from")

(defconst semantic-grammar-autogen-cookie-re
  (format "^\\s-*%s\\s-*" (regexp-quote semantic-grammar-autogen-cookie)))

(defvar semantic-grammar-buffer)

(defun semantic-grammar-beginning-of-code ()
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

(defun semantic-grammar-beginning-of-body ()
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

(defmacro semantic-grammar-with-outputfile (&rest body)
  "Execute BODY in outputfile buffer."
  `(save-excursion
     (with-current-buffer
         (find-file-noselect
          (or (semantic-grammar-outputfile)
              (error "Missing %%outputfile declaration")))
       (pop-to-buffer (current-buffer))
       (goto-char (point-min))
       ,@ body)))

(defmacro semantic-grammar-with-grammar-buffer (&rest body)
  "Execute BODY in current grammar buffer."
  `(save-excursion
     (with-current-buffer semantic-grammar-buffer
       ,@ body)))

(defsubst semantic-grammar-autogen-cookie ()
  "Return a cookie comment identifying generated code."
  (format "%s %s - %s"
          semantic-grammar-autogen-cookie
          (buffer-name semantic-grammar-buffer)
          (format-time-string "%Y-%m-%d %R%z")))

(defmacro semantic-grammar-as-string (object)
  "Return object as a string value."
  `(if (stringp ,object)
       ,object
     (require 'pp)
     (pp-to-string ,object)))

;;; Setup code generation
;;
(defun semantic-grammar-setupcode-builder-default ()
  "Return the default value of the setup code form."
  (error "`semantic-grammar-setupcode-builder' not defined"))

(define-overload semantic-grammar-setupcode-builder ()
  "Return the setup code form.")
  
(defsubst semantic-grammar-setupcode-value ()
  "Return the setup code form as a string value."
  (semantic-grammar-as-string
   (semantic-grammar-with-grammar-buffer
    (semantic-grammar-setupcode-builder))))

;;; Parser table generation
;;
(defun semantic-grammar-parsetable-builder-default ()
  "Return the default value of the parse table."
  (error "`semantic-grammar-parsetable-builder' not defined"))

(define-overload semantic-grammar-parsetable-builder ()
  "Return the parser table value.")
  
(defsubst semantic-grammar-parsetable-value ()
  "Return the parser table as a string value."
  (format "%s\n%s"
          (semantic-grammar-autogen-cookie)
          (semantic-grammar-as-string
           (semantic-grammar-with-grammar-buffer
            (semantic-grammar-parsetable-builder)))))

;;; Keyword table generation
;;
(defun semantic-grammar-keywordtable-builder-default ()
  "Return the default value of the keyword table."
  (let ((keywords (semantic-grammar-keywords)))
    `(semantic-lex-make-keyword-table
      ',keywords
      ',(semantic-grammar-keyword-properties keywords))))
  
(define-overload semantic-grammar-keywordtable-builder ()
  "Return the keyword table table value.")
  
(defsubst semantic-grammar-keywordtable-value ()
  "Return the string value of the table of keywords."
  (format "%s\n%s"
          (semantic-grammar-autogen-cookie)
          (semantic-grammar-as-string
           (semantic-grammar-with-grammar-buffer
            (semantic-grammar-keywordtable-builder)))))

;;; Token table generation
;;
(defun semantic-grammar-tokentable-builder-default ()
  "Return the default value of the token table."
  (let ((tokens (semantic-grammar-tokens)))
    `(semantic-lex-make-type-table
      ',tokens
      ',(semantic-grammar-token-properties tokens))))

(define-overload semantic-grammar-tokentable-builder ()
  "Return the token table value.")
  
(defsubst semantic-grammar-tokentable-value ()
  "Return the string value of the table of tokens."
  (format "%s\n%s"
          (semantic-grammar-autogen-cookie)
          (semantic-grammar-as-string
           (semantic-grammar-with-grammar-buffer
            (semantic-grammar-tokentable-builder)))))

(defun semantic-grammar-update-def (def comment &optional noerror)
  "Create or update the Lisp declaration for %DEF.
Use COMMENT when a new definition is created.
If NOERROR is non-nil then does nothing if there is no %DEF."
  (let ((def-name-fun (intern (format "semantic-grammar-%s" def)))
        (def-value-fun (intern (format "semantic-grammar-%s-value" def)))
        table)
    (or (fboundp def-name-fun)
        (error "Function %s not found" def-name-fun))
    (or (fboundp def-value-fun)
        (error "Function %s not found" def-value-fun))
    (if (not (setq table (funcall def-name-fun)))
        (or noerror
            (error "A %%%s declaration is required" def))
      (semantic-grammar-with-outputfile
       (if (re-search-forward
            (format "^(def\\(var\\|const\\)[\r\n\t ]+%s\\b"
                    (regexp-quote (symbol-name table)))
            nil t)
           ;; Update definition
           (progn
             (kill-region (point)
                          (progn (forward-sexp)
                                 (skip-chars-forward "^\")")
                                 (point)))
             (newline)
             (insert (funcall def-value-fun)))
         ;; Insert a new `defconst' at the beginning of code
         (semantic-grammar-beginning-of-code)
         (insert
          (format "(defconst %s\n%s%S)\n\n"
                  table (funcall def-value-fun) comment)))
       (re-search-backward "^(def\\(var\\|const\\)\\s-+")
       (indent-sexp)
       (eval-defun nil)))))
  
(defsubst semantic-grammar-update-parsetable ()
  "Create or update the parsetable Lisp declaration."
  (semantic-grammar-update-def 'parsetable "Parser automaton."))

(defsubst semantic-grammar-update-keywordtable ()
  "Create or update the keywordtable Lisp declaration."
  (semantic-grammar-update-def 'keywordtable "Keywords." t))

(defsubst semantic-grammar-update-tokentable ()
  "Create or update the tokentable Lisp declaration."
  (semantic-grammar-update-def 'tokentable "Tokens." t))

(defun semantic-grammar-update-setupfunction ()
  "Create or update the setupfunction Lisp code."
  (let ((fun  (semantic-grammar-setupfunction))
        (code (semantic-grammar-setupcode-value)))
    (when (and fun code)
      (semantic-grammar-with-outputfile
       (if (re-search-forward
            (format "^(defun[\r\n\t ]+%s\\b[\r\n\t ]+"
                    (regexp-quote (symbol-name fun)))
            nil t)
           ;; Update setup code
           (let* ((eod (save-excursion (end-of-defun) (point))))
             (if (re-search-forward semantic-grammar-autogen-cookie-re eod t)
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
               (semantic-grammar-beginning-of-body)
               (or (= (point)
                      (save-excursion
                        (beginning-of-line)
                        (skip-chars-forward "\n\r\t ")
                        (point)))
                   (newline)))
             (insert
              (format "%s\n%s" (semantic-grammar-autogen-cookie) code))
             (or (looking-at ")") (newline)))
         ;; Insert a new `defun' at the beginning of code
         (semantic-grammar-beginning-of-code)
         (insert
          (format "(defun %s ()\n%S\n%s\n%s)\n\n"
                  fun "Setup buffer for parse."
                  (semantic-grammar-autogen-cookie) code)))
       (re-search-backward "^(defun\\s-+")
       (indent-sexp)
       (eval-defun nil)))))

(defun semantic-grammar-update-outputfile ()
  "Create or update grammar Lisp code in outputfile."
  (interactive)
  (let ((semantic-grammar-buffer (current-buffer)))
    (semantic-bovinate-toplevel t)
    (semantic-grammar-update-setupfunction)
    (semantic-grammar-update-tokentable)
    (semantic-grammar-update-keywordtable)
    (semantic-grammar-update-parsetable)
    ;; The above functions each evaluate the tables created
    ;; into memory.  Now find all buffers that match the
    ;; major mode we have created this language for, and
    ;; force them to call our setup function again, refreshing
    ;; all semantic data, and enabling them to work with the
    ;; new code just created.
    (semantic-map-mode-buffers
     (semantic-grammar-setupfunction)
     (semantic-grammar-languagemode))
    ;; Make sure the file was required.  This solves the problem
    ;; of compiling a grammar, followed by loading a file and not
    ;; having the rest of the source loaded up.
    (require (intern (file-name-sans-extension
                      (semantic-grammar-outputfile))))
    ))

;;;;
;;;; Define major mode
;;;;

(defvar semantic-grammar-syntax-table
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
    (modify-syntax-entry ?\. "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)
    (modify-syntax-entry ?\# "'"     table) ;; Prefix # (sharp)
    table)
  "Syntax table used in a Semantic grammar buffers.")

(defvar semantic-grammar-mode-hook nil
  "Hook run when starting Semantic grammar mode.")

(defvar semantic-grammar-mode-keywords-1
  `(("\\(%\\)\\(\\(\\sw\\|\\s_\\)+\\)"
     (1 font-lock-reference-face)
     (2 font-lock-keyword-face))
    ("^\\(\\(\\sw\\|\\s_\\)+\\)[ \n\r\t]*:"
     1 font-lock-function-name-face)
    ("(\\s-*\\(ASSOC\\|EXPAND\\(FULL\\)?\\)\\>"
     1 ,(if (boundp 'font-lock-builtin-face)
            'font-lock-builtin-face
          'font-lock-preprocessor-face))
    ("\\$\\(\\sw\\|\\s_\\)*" 0 font-lock-variable-name-face)
    ("%" 0 font-lock-reference-face)
    ("<\\(\\(\\sw\\|\\s_\\)+\\)>" 1 font-lock-type-face)
    (,semantic-grammar-lex-c-char-re
     0 ,(if (boundp 'font-lock-constant-face)
            'font-lock-constant-face
          'font-lock-string-face) t)
    )
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords-2
  (append semantic-grammar-mode-keywords-1
          lisp-font-lock-keywords-1)
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords-3
  (append semantic-grammar-mode-keywords-2
          lisp-font-lock-keywords-2)
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords
  semantic-grammar-mode-keywords-1
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-map
  (let ((km (make-sparse-keymap)))
    
    (define-key km "|" 'semantic-grammar-electric-punctuation)
    (define-key km ";" 'semantic-grammar-electric-punctuation)
    (define-key km "%" 'semantic-grammar-electric-punctuation)
    (define-key km "(" 'semantic-grammar-electric-punctuation)
    (define-key km ")" 'semantic-grammar-electric-punctuation)
    
    (define-key km "\t"       'semantic-grammar-indent)
    (define-key km "\C-c\C-c" 'semantic-grammar-update-outputfile)
;;  (define-key km "\C-cc"    'semantic-grammar-generate-and-load)
;;  (define-key km "\C-cr"    'semantic-grammar-generate-one-rule)
;;  (define-key km "\M-\t"    'semantic-grammar-complete)
    
    km)
  "Keymap used in `semantic-grammar-mode'.")

(defun semantic-grammar-mode ()
  "Initialize a buffer for editing Semantic grammars."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-grammar-mode
	mode-name "Semantic Grammar Framework")
  (make-local-variable 'comment-start)
  (setq comment-start ";;")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table semantic-grammar-syntax-table)
  (use-local-map semantic-grammar-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'semantic-grammar-indent)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function #'lisp-fill-paragraph)
  (make-local-variable 'font-lock-multiline)
  (setq font-lock-multiline 'undecided)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((semantic-grammar-mode-keywords
           semantic-grammar-mode-keywords-1
           semantic-grammar-mode-keywords-2
           semantic-grammar-mode-keywords-3)
          nil ;; perform string/comment fontification
          nil ;; keywords are case sensitive.
          ;; This puts _ & - as a word constituant,
          ;; simplifying our keywords significantly
          ((?_ . "w") (?- . "w"))))
  ;; Set up Semantic environment
  (semantic-grammar-setup-semantic)
  (semantic-make-local-hook 'semantic-edits-new-change-hooks)
  (add-hook 'semantic-edits-new-change-hooks
            'semantic-grammar-edits-new-change-hook-fcn
            nil t)
  (run-hooks 'semantic-grammar-mode-hook))

;;;;
;;;; Useful commands
;;;;

(defun semantic-grammar-skip-comments-backward ()
  "Move point backward, stopping after comments and whitespaces."
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (while (nth 4 (parse-partial-sexp bol (point)))
      (re-search-backward ";;"))
    (forward-comment (- (point-max)))))

(defun semantic-grammar-goto-grammar-indent-anchor ()
  "Move the point to current grammar indent anchor.
That is just after the previous percent, colon or semicolon character
found, taking care of comments and Lisp code.  Return the column where
the anchor is or nil if the point has not moved."
    (condition-case nil
        (let ((found nil))
          (save-excursion
            ;; Escape Lisp code
            (semantic-grammar-skip-comments-backward)
            (condition-case nil
                (while t (up-list -1))
              (error nil))
            ;; Search for previous [%;:]
            (while (not found)
              (semantic-grammar-skip-comments-backward)
              (cond
               ((eq (char-before) ?\')
                ;; `with-syntax-table' copy given syntax table
                (with-syntax-table semantic-grammar-syntax-table
                  ;; Can't be is Lisp code here!  Temporarily consider
                  ;; quote as a "paired delimiter", so `forward-sexp'
                  ;; will skip the whole quoted expression.
                  (modify-syntax-entry ?\' "$" (syntax-table))
                  (forward-sexp -1)))
               ((eq (char-before) ?\%)
                (or (looking-at "\\<prec\\>")
                    (setq found (point))))
               ((memq (char-before) '(?\: ?\;))
                (setq found (point)))
               ((bobp)
                (error ""))
               (t
                (forward-sexp -1)))))
          (goto-char found)
          (1- (current-column)))
      (error nil)))

(defsubst semantic-grammar-between-name-and-colon-p (point)
  "Return non-nil if POINT is between name and colon.
If so move to POINT."
  (let (name-end)
    (if (save-excursion
          (forward-comment (point-max))
          (when (looking-at "\\(\\w\\|\\s_\\)+\\s-*$")
            (forward-sexp 1)
            (setq name-end (point))
            (forward-comment (point-max))
            (when (looking-at ":")
              (beginning-of-line)
              (and (> point name-end) (<= point (point))))))
        (goto-char point))))
      
(defun semantic-grammar-grammar-compute-indentation ()
  "Compute indentation of the current line of grammar."
  (save-excursion
    (beginning-of-line)
    (if (or (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
            (looking-at "\\s-*%"))
        0
      (let* ((p (point))
             (i (semantic-grammar-goto-grammar-indent-anchor)))
        (if (not (and i (eq (char-before) ?\:)))
            (if (semantic-grammar-between-name-and-colon-p p)
                (if (looking-at "\\s-*;;")
                    1
                  2)
              0)
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
      
(defun semantic-grammar-do-grammar-indent ()
  "Indent a line of grammar.
When called the point is not in Lisp code."
  (let ((indent (semantic-grammar-grammar-compute-indentation)))
    (if (/= (current-indentation) indent)
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to indent)))))

(defun semantic-grammar-do-lisp-indent ()
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
              ;; `with-syntax-table' copy given syntax table
              (with-syntax-table emacs-lisp-mode-syntax-table
                ;; Consider brackets as parenthesis
                (modify-syntax-entry ?\{ "(}  " (syntax-table))
                (modify-syntax-entry ?\} "){  " (syntax-table))
                (lisp-indent-line))))
          t)
      (error nil)))

(defun semantic-grammar-indent ()
  "Indent the current line.
Use the Lisp or grammar indenter depending on point location."
  (interactive)
  (let ((orig (point))
        first)
    (or (semantic-grammar-do-lisp-indent)
        (semantic-grammar-do-grammar-indent))
    (setq first (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward "\t ")
                  (point)))
    (if (or (< orig first) (/= orig (point)))
        (goto-char first))))

(defun semantic-grammar-electric-punctuation ()
  "Insert and reindent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    (semantic-grammar-indent)))

(provide 'semantic-grammar)

;;; semantic-grammar.el ends here
