;;; semantic-bnf.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2
;; Keywords: parse
;; X-RCS: $Id: semantic-bnf.el,v 1.55 2002/06/18 21:31:43 ponced Exp $

;; Semantic-bnf is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Convert BNF definitions similar to bison into bovine tables.
;;
;; Major mode for BNF-for-emacs editing.
;;
;; See the semantic info file for details.

;;; History:
;; 

(require 'semantic)
(eval-when-compile
  (require 'speedbar)
  (require 'senator))

;;; Code:
(defvar semantic-setup-code-delimiters '("^\\s-*;; Code generated from" .
					 "^\\s-*;; End code generated from")
  "Delimiter comments in a setup function where code is added from a bnf file.")

(defvar semantic-bovine-bnf-table
  ;; BNF's BNF
  ;;
  ;; decl : punctuation "%" semantic-list punctuation "%"
  ;;      ;
  ;;
  ;; Here, the first symbol is a special token meaning something to
  ;; the generator.
  ;; percenttoken : punctuation "%" symbol symbol
  ;;              ;
  ;;
  ;; rule : result punctuation ":" rule-list
  ;;      ;
  ;;
  ;; result : symbol
  ;;        ;
  ;;
  ;; rule-list : match-list lambda rule-or-list punctuation ";"
  ;;           ;
  ;;
  ;; rule-or-list : punctuation "|" match-list lambda rule-or-list
  ;;              | EMPTY
  ;;              ;
  ;;
  ;; match-list : symbol match-list
  ;;            | string match-list
  ;;            | symbol
  ;;            | string
  ;;            ;
  `((bovine-toplevel
     (symbol punctuation ":" rule-list punctuation ";"
	     ,(semantic-lambda
	       (list (nth 0 vals) 'rule nil (nth 2 vals))))
     (punctuation "%" percent-thing
		  ,(semantic-lambda
		    (nth 1 vals))))
    (percent-thing
     (semantic-list punctuation "%"
		    ;; When loading lisp rules, use READ to convert
		    ;; into a list we can pretty print later.
		    ,(semantic-lambda
		      (let ((r (buffer-substring-no-properties
				(car (car vals))
				(cdr (car vals)))))
			(list (symbol-name (car (read r))) 'setting r))))
				   ;     (symbol "token" symbol symbol
					;	     ,(semantic-lambda
	      ;	       (list (nth 1 vals) 'token (nth 2 vals))))
     (START symbol
	    ,(semantic-lambda
	      (list (nth 1 vals) 'start)))
     (SCOPESTART symbol
		 ,(semantic-lambda
		   (list (nth 1 vals) 'scopestart)))
     (TOKEN symbol string
	    ,(semantic-lambda
	      (list (nth 1 vals) 'keyword "symbol" (nth 2 vals))))
     (TOKEN symbol symbol string
	    ,(semantic-lambda
	      (list (nth 1 vals) 'token
		    (nth 2 vals)  (nth 3 vals))))
     (PUT symbol symbol put-value
	  ,(semantic-lambda
	    (list (nth 1 vals) 'put
		  (list (nth 1 vals))
		  (list (apply 'list (nth 2 vals) 'property (nth 3 vals))))))
     (PUT symbol semantic-list
	  ,(semantic-lambda
	    (list (nth 1 vals) 'put
		  (list (nth 1 vals))
		  (semantic-bovinate-from-nonterminal-full
		   (car (nth 2 vals)) (cdr (nth 2 vals))
		   `put-value-list))))
     (PUT semantic-list symbol put-value
	  ,(semantic-lambda
	    (let ((names (semantic-bovinate-from-nonterminal-full
			  (car (nth 1 vals)) (cdr (nth 1 vals))
			  `put-name-list)))
	      (list (car (car names)) 'put names
		    (list (apply 'list (nth 2 vals) 'property (nth 3 vals)))))))
     (PUT semantic-list semantic-list
	  ,(semantic-lambda
	    (let ((names (semantic-bovinate-from-nonterminal-full
			  (car (nth 1 vals)) (cdr (nth 1 vals))
			  `put-name-list)))
	      (list (car (car names)) 'put names
		    (semantic-bovinate-from-nonterminal-full
		     (car (nth 2 vals)) (cdr (nth 2 vals))
		     `put-value-list)))))
     (OUTPUTFILE symbol punctuation "." symbol "\\bel\\b"
		 ,(semantic-lambda
		   (list (concat (nth 1 vals) ".el") 'outputfile)))
     (PARSETABLE symbol
		 ,(semantic-lambda
		   (list (nth 1 vals) 'parsetable)))
     (KEYWORDTABLE symbol
		   ,(semantic-lambda
		     (list (nth 1 vals) 'keywordtable)))
     (TOKENTABLE symbol
		   ,(semantic-lambda
		     (list (nth 1 vals) 'tokentable)))
     (LANGUAGEMODE symbol
		   ,(semantic-lambda
		     (list (nth 1 vals) 'languagemode)))
     (LANGUAGEMODE semantic-list
		   ,(semantic-lambda
		     (let ((r (buffer-substring-no-properties
			       (car (nth 1 vals))
			       (cdr (nth 1 vals)))))
		       (list r 'languagemode))))
     (SETUPFUNCTION symbol
		    ,(semantic-lambda
		      (list (nth 1 vals) 'setupfunction)))
     (QUOTEMODE symbol
		,(semantic-lambda
		  (list (nth 1 vals) 'quotemode)))
     (PARSERMODE symbol
		,(semantic-lambda
		  (list (nth 1 vals) 'parsermode)))
     (NONASSOC terms punctuation ";"
               ,(semantic-lambda
                 (list (nth 0 vals) 'assoc nil (nth 1 vals))))
     (LEFT terms punctuation ";"
           ,(semantic-lambda
             (list (nth 0 vals) 'assoc nil (nth 1 vals))))
     (RIGHT terms punctuation ";"
            ,(semantic-lambda
              (list (nth 0 vals) 'assoc nil (nth 1 vals))))
     )
    (terms
     (symbol terms
             ,(semantic-lambda
               (cons (nth 0 vals) (nth 1 vals))))
     (symbol
      ,(semantic-lambda
        (list (nth 0 vals))))
     )
    (put-name-list
     (open-paren ,(semantic-lambda (list nil)))
     (close-paren ,(semantic-lambda (list nil)))
     (symbol ,(semantic-lambda (list (nth 0 vals) 'name))))
    (put-value-list
     (open-paren ,(semantic-lambda (list nil)))
     (close-paren ,(semantic-lambda (list nil)))
     (symbol put-value
	     ,(semantic-lambda
	       (apply 'list (nth 0 vals) 'property (nth 1 vals))))
     )
    (put-value
     (symbol ,(semantic-lambda (list (nth 0 vals))))
     (string ,(semantic-lambda (list (nth 0 vals))))
     (semantic-list
      ,(semantic-lambda (list (semantic-flex-text (cons 1 (nth 0 vals)))))))
    (rule-list
     (match-list lambda-fn rule-or-list
		 ,(semantic-lambda
		   (cons (cons (car (nth 1 vals)) (nth 0 vals))
			 (nth 2 vals)))))
    (rule-or-list
     (punctuation "|" match-list lambda-fn rule-or-list
		  ,(semantic-lambda
		    (cons (cons (car (nth 2 vals)) (nth 1 vals))
			  (nth 3 vals))))
     (,(semantic-lambda nil)))
    (match-list
     (match-list-1 prec
                   ,(semantic-lambda
                     (append (nth 0 vals) (nth 1 vals))))
     (match-list-1)
     )
    (match-list-1
     (symbol match-list-1
	     ,(semantic-lambda
	       (cons (nth 0 vals) (nth 1 vals))))
     (string match-list-1
	     ,(semantic-lambda
	       (cons (nth 0 vals) (nth 1 vals))))
     (string)
     (symbol)
     )
    (prec
     (punctuation "%" PREC symbol
                  ,(semantic-lambda
                    (list '%prec (nth 2 vals))))
     )
    (lambda-fn
     (semantic-list
      ,(semantic-lambda
	(list (buffer-substring-no-properties start end))))
     (,(semantic-lambda (list "" ))))
    )
"Bovine table used to convert a BNF language file into a bovine table.")

(defvar semantic-bnf-keyword-table
  (semantic-flex-make-keyword-table
   `( ("start" . START)
      ("scopestart" . SCOPESTART)
      ("token" . TOKEN)
      ("put" . PUT)
      ("outputfile" . OUTPUTFILE)
      ("parsetable" . PARSETABLE)
      ("keywordtable" . KEYWORDTABLE)
      ("tokentable" . TOKENTABLE)
      ("languagemode" . LANGUAGEMODE)
      ("setupfunction" . SETUPFUNCTION)
      ("quotemode" . QUOTEMODE)
      ("parsermode" . PARSERMODE)
      ("nonassoc" . NONASSOC)
      ("left" . LEFT)
      ("right" . RIGHT)
      ("prec" . PREC)
      )
   `(("put" summary "%put <keyword> <lisp expression>")
     ("token" summary "%token <keyword> [syntax] \"matchtext\"")
     ("start" summary "%start <starting rule name>")
     ("scopestart" summary "%scopestart <starting scope (code) rule name>")
     ("languagemode" summary "%languagemode [ lispsymbol | ( lispsym lispsym ...) ]")
     ("parsermode" summary "%parsermode <mode>")
     ("prec" summary "rule precedence")
     ))
  "Keyword table used for Semantic BNF files.")


;;; Basic API
;;
(defsubst semantic-bnf-token-name-symbol (token)
  "Return TOKEN name as an interned symbol."
  (intern (semantic-token-name token)))

(defmacro semantic-bnf-token-token-type (token)
  "Return type of a 'token TOKEN."
  `(nth 2 ,token))

(defmacro semantic-bnf-token-token-value (token)
  "Return the lisp value of a 'token TOKEN."
  `(condition-case nil
       (car (read-from-string (nth 3 ,token)))
     (error
      ;;(message "read error on %S" (nth 3 ,token))
      nil)))

(defmacro semantic-bnf-token-rule-matchlist (token)
  "Return the matching list of a 'rule TOKEN."
  `(nth 3 ,token))

(defmacro semantic-bnf-token-assoc-terms (token)
  "Return the list of terminals from associativity TOKEN.
Token must be of category 'assoc."
  `(nth 3 ,token))


;;; Conversion routines
;;
(defun semantic-bnf-EXPAND (lst)
  "Insert a token expand function based on LST."
  (let ((argv (1- (string-to-int (substring (symbol-name (car (cdr lst)))
					    1)))))
    (insert "\n ")
    (insert "(semantic-bovinate-from-nonterminal "
	    "(car (nth " (int-to-string argv) " vals)) "
	    "(cdr (nth " (int-to-string argv) " vals)) "
	    "'" (symbol-name (car (cdr (cdr lst))))
	    ")\n ")))

(defun semantic-bnf-EXPANDFULL (lst)
  "Insert a token full expand function based on LST."
  (let ((argv (1- (string-to-int (substring (symbol-name (car (cdr lst)))
					    1)))))
    (insert "\n ")
    (insert "(semantic-bovinate-from-nonterminal-full "
	    "(car (nth " (int-to-string argv) " vals)) "
	    "(cdr (nth " (int-to-string argv) " vals)) "
	    "'" (symbol-name (car (cdr (cdr lst))))
	    ")\n ")))

(defun semantic-bnf-ASSOC (lst quotemode)
  "Handle an ASSOC list based on LST.
QUOTEMODE is the current mode of quotation."
  (let ((lst (cdr lst))
	l)
    (while lst
      ;; quote the key
      (setq l   (cons (list 'quote (car lst)) l)
	    lst (cdr lst))
      ;; push the value
      (if lst
	  (setq l   (cons (car lst) l)
		lst (cdr lst))))
    ;; substitute ASSOC by call to semantic-bovinate-make-assoc-list
    ;; and do BNF lambda substitution on the whole expression
    (semantic-bnf-lambda-substitute
     (cons 'semantic-bovinate-make-assoc-list (nreverse l)) quotemode t)))

(defun semantic-bnf-lambda-substitute (lst quotemode &optional inplace)
  "Insert LST substituting based on rules for the BNF converter.
LST is the list in which we are substituting.
Argument QUOTEMODE is non-nil if we are in backquote mode.
Optional INPLACE indicates that the list is being expanded from elsewhere."
  (if (eq (car lst) 'quote)
      (progn
	(setq lst (cdr lst))
	(if (and (= (length lst) 1) (listp (car lst)))
	    (progn
	      (insert " (append")
	      (semantic-bnf-lambda-substitute (car lst) quotemode nil)
	      (insert ")")
	      (setq lst nil inplace nil))
	  (if (and (= (length lst) 1) (symbolp (car lst)))
	      (progn
		(insert " '" (symbol-name (car lst)))
		(setq lst nil inplace nil))
	    (insert "(list")
	    (setq inplace t))
	  )))
  (cond ((eq (car lst) 'EXPAND)
	 (semantic-bnf-EXPAND lst))
	((eq (car lst) 'EXPANDFULL)
	 (semantic-bnf-EXPANDFULL lst))
	((eq (car lst) 'ASSOC)
	 (semantic-bnf-ASSOC lst quotemode))
	(t
	 (if inplace (insert " ("))
	 (let ((inlist nil))
	   (while lst
	     (cond ((eq (car lst) nil)
		    (if (and (not inlist) (not inplace))
			(progn (insert " (list")
			       (setq inlist t)))
		    (insert " nil"))
		   ((listp (car lst))
		    (let ((fn (and (symbolp (car (car lst))) (fboundp (car (car lst))))))
		      (if (and (not inlist) (not inplace))
			  (progn (insert " (list")
				 (setq inlist t)))
;		      (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
;			  (insert " (append"))
		      (semantic-bnf-lambda-substitute (car lst) quotemode t);(and fn (not (eq fn 'quote))))
;		      (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
;			  (insert  ")"))
		      ))
		   ((symbolp (car lst))
		    (let ((n (symbol-name (car lst))) ;the name
			  (q quotemode)	;implied quote flag
			  (x nil))	;expand flag
		      (if (eq (aref n 0) ?,)
			  (if quotemode
			      ;; backquote mode needs the @
			      (if (eq (aref n 1) ?@)
				  (setq n (substring n 2)
					q nil
					x t)
				;; non backquote mode behaves normally.
				(setq n (substring n 1)
				      q nil))
			    (setq n (substring n 1)
				  x t)))
		      (if (string= n "")
			  ;; We expand only the next item in place (a list?)
			  (progn
			    (setq lst (cdr lst))
			    ;; A regular inline-list...
			    (semantic-bnf-lambda-substitute (car lst) quotemode t))
			(if (and (eq (aref n 0) ?$)
				 ;; Don't expand $ tokens in implied quote
				 ;; mode.  This acts like quoting in other
				 ;; symbols.
				 (not q))
			    (let ((val (1- (string-to-int (substring n 1)))))
			      (if (and (not x) (not inlist) (not inplace))
				  (insert " (list")
				(if (and x inlist (not inplace))
				    (progn (insert ")")
					   (setq inlist nil))))
			      (insert " (nth " (int-to-string val) " vals)")
			      (if (and (not x) (not inplace)) (setq inlist t)))
			  (if (and (not inlist) (not inplace) )
			      (progn (insert " (list")
				     (setq inlist t)))
			  (insert " "
				  (if (or inplace (eq (car lst) t)) "" "'")
				  n; " "
				  )))))
		   (t
		    (if (and (not inlist) (not inplace))
			(progn (insert " (list")
			       (setq inlist t)))
		    (insert (format " %S" (car lst)))))
	     (setq lst (cdr lst)))
	   (if inlist (insert ")")))
	   (if inplace (insert ")"))))
  )

(defun semantic-bnf-lambda-convert (semliststr vals quotemode)
  "Convert SEMLISTSTR into Lisp code based on VALS.
VALS are the matches in the BNF notation file.
QUOTEMODE is the mode in which quoted symbols are slurred."
  (if (string= "" semliststr)
      nil
    (let ((slsr (read semliststr)))
      ;; We converted the lambda string into a list.  Now write it
      ;; out as the bovine lambda expression, and do macro-like
      ;; conversion upon it.
      (insert "\n ")
      (cond ((eq (car slsr) 'EXPAND)
	     (insert ",(lambda (vals start end)\n ")
	     (semantic-bnf-EXPAND slsr)
	     )
	    ((and (listp (car slsr))
		  (eq (car (car slsr)) 'EVAL))
	     ;; The user wants to evaluate the following args.
	     ;; Use a simpler expander
	     )
	    (t
	     (insert " ,(semantic-lambda\n ")
	     (semantic-bnf-lambda-substitute slsr quotemode)
	     ))
      (insert ")"))))

(defun semantic-bnf-to-bovine (tokstream &optional start scopestart)
  "Insert the BNF TOKSTREAM into the current buffer as a bovine table.
Optional argument START is the token to start with.
Optional argument SCOPESTART is the token to start subscopes with."
  (interactive "FBNF file: ")
  (let ((tl (float (length tokstream)))
	(tokens (semantic-find-nonterminal-by-token 'token tokstream))
	(quotemode (if (semantic-find-nonterminal-by-token 'quotemode tokstream)
		       t nil)))
    (insert "`(")
    (working-status-forms "Building bovine table" "done"
      (while tokstream
	;; Each element is a top level match, of the form:
	;; ( RESULT MATCH1 MATCH2 ... )
	;; where a match is of the form:
	;; ( LAMBDA-STRING TOKEN1 TOKEN2 ... )
	(let* ((rule (car tokstream))
	       (matches (car (cdr (cdr (cdr rule))))))
	  (when (eq (car (cdr rule)) 'rule)
	    (insert "(")
	    (cond ((and start (string= start (car rule)))
		   (insert "bovine-toplevel"))
		  ((and scopestart (string= scopestart (car rule)))
		   (insert "bovine-inner-scope"))
		  (t
		   (insert (car rule))))
	    (insert "\n ")
	    (while matches
	      (let* ((mla (car matches))
		     (lamb (car mla))
		     (ml (cdr mla)))
		(insert "(")
		(if (and (= (length ml) 1) (string= (car ml) "EMPTY"))
		    nil
		  (while ml
		    (let ((a (assoc (car ml) tokens)))
		      (if a
			  (insert " " (nth 2 a) " "
				  (format
				   "%S"
				   (concat "\\b"
					   (regexp-quote (read (nth 3 a)))
					   "\\b")))
			(insert " " (car ml))))
		    (setq ml (cdr ml))))
		(semantic-bnf-lambda-convert lamb (car (cdr mla)) quotemode)
		(insert ")\n "))
	      (setq matches (cdr matches)))
	    (insert ") ; end " (car rule) "\n ")))
	(setq tokstream (cdr tokstream))
	(working-status (* 100.0 (- 1.0 (/ (float (length tokstream)) tl)))))
      (working-status t))
    (insert ")\n")
    ))


;;; LALR conversion routines
;;
(defun semantic-bnf-to-lalr-ASSOC (&rest args)
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

(defun semantic-bnf-to-lalr-EXPANDTHING ($i nonterm expander)
  "Return expansion of built-in EXPAND/EXPANDFULL expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with.
EXPANDER is the Semantic function called to expand NONTERM"
  (let* ((n   (symbol-name $i))
         ($ri (and (string-match "^[$]\\([1-9][0-9]*\\)$" n)
                   (intern (concat "$region" (match-string 1 n))))))
    (if $ri
        `(,expander (car ,$ri) (cdr ,$ri) ',nonterm))))

(defun semantic-bnf-to-lalr-EXPAND ($i nonterm)
  "Return expansion of built-in EXPAND expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (or (semantic-bnf-to-lalr-EXPANDTHING
       $i nonterm 'wisent-bovinate-from-nonterminal)
      (error "Invalid form (EXPAND %s %s)" $i nonterm)))

(defun semantic-bnf-to-lalr-EXPANDFULL ($i nonterm)
  "Return expansion of built-in EXPANDFULL expression.
$I is the placeholder value to expand.
NONTERM is the nonterminal symbol to start with."
  (or (semantic-bnf-to-lalr-EXPANDTHING
       $i nonterm 'wisent-bovinate-from-nonterminal-full)
      (error "Invalid form (EXPANDFULL %s %s)" $i nonterm)))

(defconst semantic-bnf-to-lalr-builtins
  '(
    ;; Builtin name . Expander
    ;; ------------ . ---------------------------------
    (  ASSOC        . semantic-bnf-to-lalr-ASSOC)
    (  EXPAND       . semantic-bnf-to-lalr-EXPAND)
    (  EXPANDFULL   . semantic-bnf-to-lalr-EXPANDFULL)
    ;; ------------ . ---------------------------------
    )
  "Expanders of Semantic built-in functions in LALR grammar.")

(defsubst semantic-bnf-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst semantic-bnf-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

(defun semantic-bnf-to-lalr-action (expr)
  "Return expanded form of the semantic action expression EXPR.
`backquote' expressions and Semantic built-in function calls are
expanded.  The variable `semantic-bnf-to-lalr-builtins' defines
built-in functions and corresponding expanders."
  (if (not (listp expr))
      ;; EXPR is an atom, no expansion needed
      expr
    ;; EXPR is a list, expand inside it
    (let (eexpr sexpr bltn)
      ;; If backquote expand it first
      (if (semantic-bnf-backquote-p (car expr))
          (setq expr (macroexpand expr)))
      ;; Expand builtins
      (if (setq bltn (assq (car expr) semantic-bnf-to-lalr-builtins))
          (setq expr (apply (cdr bltn) (cdr expr))))
      (while expr
        (setq sexpr (car expr)
              expr  (cdr expr))
        ;; Recursively expand function call but quote expression
        (and (consp sexpr)
             (not (semantic-bnf-quote-p (car sexpr)))
             (setq sexpr (semantic-bnf-to-lalr-action sexpr)))
        ;; Accumulate expanded forms
        (setq eexpr (nconc eexpr (list sexpr))))
      eexpr)))

(defun semantic-bnf-matching-to-lalr (matching)
  "Convert the rule MATCHING from BNF to LALR internal lisp form."
  (let* ((act (condition-case nil
                  (read-from-string (car matching))
                (error
                 ;;(message "read error on %S" (car matching))
                 nil)))
         (l   (cdr matching))
         ml prec)
    (if (string-equal (car l) "EMPTY")
        (setq l (cdr l)) ;; EMPTY rule can have a %prec clause too!
      (while (and l (not (eq (car l) '%prec)))
        (setq ml (cons (intern (car l)) ml)
              l  (cdr l)))
      (setq ml (nreverse ml)))
    (if (eq (car l) '%prec)
        (setq prec (vector (intern (cadr l)))))
    ;; Here `act' is nil if previous `read-from-string' failed of a
    ;; cons (OBJECT-READ . FINAL-STRING-INDEX) otherwise.  This permit
    ;; to distinguish if the semantic action is missing or is actually
    ;; nil!
    (if act
        (progn
          (setq act (semantic-bnf-to-lalr-action (car act)))
          (if prec
              (list ml prec act)
            (list ml act)))
      (if prec
          (list ml prec)
        (list ml)))))

(defsubst semantic-bnf-terminal-token-p (token)
  "Return non-nil if BNF TOKEN is a terminal one.
That is a token of 'keyword or 'token category."
  (memq (semantic-token-token token) '(token keyword)))

(defsubst semantic-bnf-find-terminals (tokstream)
  "Return the list of terminal tokens from TOKSTREAM."
  (semantic-find-nonterminal-by-function
   #'semantic-bnf-terminal-token-p tokstream))

(defsubst semantic-bnf-find-terminal-symbols (tokstream)
  "Return the list of terminal symbols from TOKSTREAM."
  (mapcar #'semantic-bnf-token-name-symbol
          (semantic-bnf-find-terminals tokstream)))

(defun semantic-bnf-token-table (tokstream)
  "Return the table of 'token tokens from TOKSTREAM.
The table is an alist of (TOK-CAT . TOK-DEFS) where TOK-CAT is a token
category symbol.  TOK-DEFS is an alist of (TOK-KEY . TOK-VALUE) where
TOK-KEY is a token symbol and TOK-VALUE its value as a string.

For example the following BNF entries:

  %token EQ     operator \":=\"
  %token LT     operator \"<\"
  %token LPAREN paren    \"(\"
  %token RPAREN paren    \")\"

produce the following table of tokens:

  '((operator (EQ . \":=\")
              (LT . \"<\"))
    (paren (LPAREN . \"(\")
           (RPAREN . \")\")))"
  (let ((tokens (semantic-find-nonterminal-by-token 'token tokstream))
        token tsymb ttype tvalue bin bins)
    (while tokens
      (setq token  (car tokens)
            tokens (cdr tokens)
            tsymb  (semantic-bnf-token-name-symbol token)
            ttype  (intern (semantic-bnf-token-token-type token))
            tvalue (semantic-bnf-token-token-value token)
            bin    (assq ttype bins))
      (if bin
          (setcdr bin (cons (cons tsymb tvalue) (cdr bin)))
        (setq bins (cons (list ttype (cons tsymb tvalue)) bins))))
    bins))

(defun semantic-bnf-find-terminal-assocs (tokstream)
  "Return terminals associativity from TOKSTREAM.
This is a list of elements of the form: (ASSOC-TYPE . TERMS) where
ASSOC-TYPE is one of 'nonassoc, 'left or 'right.  And TERMS is a list
of terminal symbols.  Elements are in the same order as the
corresponding %nonassoc, %left and %right statements in the BNF file."
  (let ((tokens (semantic-find-nonterminal-by-token
                 'assoc tokstream))
        assocs terms type)
    (while tokens
      (setq type (intern (semantic-token-name (car tokens)))
            terms (mapcar #'intern (semantic-bnf-token-assoc-terms
                                    (car tokens)))
            assocs (cons (cons type terms) assocs)
            tokens (cdr tokens)))
    (nreverse assocs)))

(defun semantic-bnf-to-lalr (&optional tokstream start)
  "Convert the BNF rules in TOKSTREAM to LALR internal lisp form.
START is a list of 'start tokens defining alternate entry point in the
grammar.  The result is inserted at point in the current buffer."
  (setq tokstream (or tokstream (semantic-bovinate-toplevel t)))
  (let ((terms  (semantic-bnf-find-terminal-symbols tokstream))
        (starts (mapcar #'semantic-bnf-token-name-symbol start))
        (assocs (semantic-bnf-find-terminal-assocs tokstream))
        vars tok lhs rhs gram)
    (while tokstream
      (setq tok       (car tokstream)
            tokstream (cdr tokstream))
      (if (not (eq (semantic-token-token tok) 'rule))
          nil
        (setq lhs  (semantic-bnf-token-name-symbol tok)
              rhs  (mapcar #'semantic-bnf-matching-to-lalr
                           (semantic-bnf-token-rule-matchlist tok))
              vars (cons (cons lhs rhs) vars))))
    (setq gram (cons terms (cons assocs (nreverse vars))))
    (require 'wisent) ;; `wisent-compile-grammar' must be defined!
    ;; Insert the grammar
    (indent-according-to-mode)
    (pp (list 'eval-when-compile
              (list 'wisent-compile-grammar
                    (list 'quote gram)
                    (list 'quote starts)))
        (current-buffer))
    ))


;;; Output File hacks
;;
(defun semantic-beginning-of-body ()
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

(defun semantic-bnf-find-table-destination-old ()
  "Find the destination file for this BNF file via comments."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 "^#\\s-*TABLE:\\s-*\\([-a-zA-Z0-9_-]+\\.el\\):\\([-a-zA-Z0-9_]+\\)$"
	 nil t)
	(save-excursion
	  (let ((f (match-string 1))
		(v (match-string 2)))
	    (set-buffer (find-file-noselect f))
	    (goto-char (point-min))
	    (if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
					   (regexp-quote v) "\\b"
                                           ) nil t)
		(progn
		  (goto-char (match-beginning 0))
		  (point-marker)))))
      nil)))

(defun semantic-bnf-find-table-destination (tokstream)
  "Find the destination file for this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
parse table variable."
  (save-excursion
    (let ((file (semantic-find-nonterminal-by-token 'outputfile tokstream))
	  (var (semantic-find-nonterminal-by-token 'parsetable tokstream)))
      (if (or (not file) (not var))
	  (semantic-bnf-find-table-destination-old)
	;; Fix file/var to strings
	(setq file (semantic-token-name (car file))
	      var (semantic-token-name (car var)))
	;; Look these items up.
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
				       (regexp-quote var) "\\b"
                                       ) nil t)
	    (progn
	      (goto-char (match-beginning 0))
	      (point-marker))
	  (error "You must add a declaration for %s in %s"
		 var file))))))

(defun semantic-bnf-find-keyword-destination (tokstream)
  "Find the destination file for keywords in this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
keyword table variable."
  (save-excursion
    (let ((file (semantic-find-nonterminal-by-token 'outputfile tokstream))
	  (var (semantic-find-nonterminal-by-token 'keywordtable tokstream)))
      (if (or (not file) (not var))
	  nil
	;; Fix file/var to strings
	(setq file (semantic-token-name (car file))
	      var (semantic-token-name (car var)))
	;; Look these items up.
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
				       (regexp-quote var) "\\b"
                                       ) nil t)
	    (progn
	      (goto-char (match-beginning 0))
	      (point-marker))
	  (error "You must add a declaration for %s in %s"
		 var file))))))

(defun semantic-bnf-find-languagemode-old ()
  "Find the mode this BNF is used in."
  (error "Upgrade")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\-*MODE:\\s-*\\([-a-z]+\\)$" nil t)
	(save-excursion
	  (let ((m (match-string 1)))
	    (read m)))
      nil)))

(defun semantic-bnf-find-languagemode (tokstream)
  "Find the language mode for this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
parse table variable."
  (let ((mode (semantic-find-nonterminal-by-token 'languagemode tokstream)))
    (if mode
	(let ((m (read (semantic-token-name (car mode)))))
	  (if (listp m)
	      m
	    (list m)))
      (list (semantic-bnf-find-languagemode-old)))))

(defun semantic-bnf-find-setup-code (tokstream sourcefile)
  "Find the setup code based on TOKSTREAM.
Return a marker where the code is to be inserted.
SOURCEFILE is the file name from whence tokstream came."
  (let ((setfn (semantic-find-nonterminal-by-token 'setupfunction tokstream)))
    (if (not setfn)
	nil
      ;; The setup function
      (goto-char (point-min))
      (if (not (re-search-forward (concat "(defun\\s-+"
					  (semantic-token-name (car setfn))
					  "\\s-+\\(()\\|nil\\)")
				  nil t))
	  (error "Setup function %s not found in %s"
		 (semantic-token-name (car setfn)) (buffer-file-name))
	;; Scan for setup text, and remove old stuff, insert new.
	(let ((e (save-excursion (end-of-defun) (point))))
	  (if (re-search-forward (car semantic-setup-code-delimiters)
				 nil t)
	      ;; Search and destroy
	      (let ((mb (progn (goto-char (match-end 0))
			       (end-of-line)
			       (point)))
		    (me (progn (re-search-forward
				(cdr semantic-setup-code-delimiters) e t)
			       (beginning-of-line)
			       (point))))
		(delete-region (1+ mb) (1- me))
		(goto-char (1+ mb))
		t)
	    ;; Add a new on in at the beginning
	    (goto-char e)
	    (semantic-beginning-of-body)
	    ;; Insert delimiters, move cursor
	    (let ((m (string-match ";"
				   (car semantic-setup-code-delimiters))))
	      (insert (substring (car semantic-setup-code-delimiters) m))
	      (insert " " sourcefile "\n  ")
	      (save-excursion;; save in the middle
		(insert "\n " (substring (cdr semantic-setup-code-delimiters)
					m))
		(insert " " sourcefile "\n "))
	      t)
	    ))))))

(defsubst semantic-bnf-find-parser-mode (tok)
  "Find the parser mode symbol in this BNF file.
If not found return nil."
  (let ((gm (semantic-find-nonterminal-by-token 'parsermode tok)))
    (if gm
        (semantic-bnf-token-name-symbol (car gm)))))

(defun semantic-bnf-find-token-destination (tokstream)
  "Find the destination for tokens in this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
token table variable."
  (save-excursion
    (let ((file (semantic-find-nonterminal-by-token 'outputfile tokstream))
	  (var (semantic-find-nonterminal-by-token 'tokentable tokstream)))
      (if (or (not file) (not var))
	  nil
	;; Fix file/var to strings
	(setq file (semantic-token-name (car file))
	      var (semantic-token-name (car var)))
	;; Look these items up.
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
				       (regexp-quote var) "\\b"
                                       ) nil t)
	    (progn
	      (goto-char (match-beginning 0))
	      (point-marker))
	  (error "You must add a declaration for %s in %s"
		 var file))))))

(defvar semantic-bnf-indent-table t
  "Non nil means to indent the large table during creation.")

(defun semantic-bnf-generate-and-load-no-indent ()
  "Call `semantic-bnf-generate-and-load' without indenting the table."
  (interactive)
  (let ((semantic-bnf-indent-table nil))
    (semantic-bnf-generate-and-load)))
  
(defun semantic-bnf-generate-and-load ()
  "Take the current BNF, auto-generate it into a table, and load it."
  (interactive)
  (if (not (eq major-mode 'semantic-bnf-mode))
      (error "Not valid outside the scope of a BNF file"))
  ;; Do the work
  (semantic-clear-toplevel-cache)
  (let* ((inhibit-modification-hooks t) ;; Make it go fast.
	 (fname (file-name-nondirectory (buffer-file-name)))
	 (tok (semantic-bovinate-toplevel t))
	 (dest (semantic-bnf-find-table-destination tok))
	 (keydest (semantic-bnf-find-keyword-destination tok))
	 (tokdest (semantic-bnf-find-token-destination tok))
	 (mode (semantic-bnf-find-languagemode tok))
	 (start (semantic-find-nonterminal-by-token 'start tok))
	 (scopestart (semantic-find-nonterminal-by-token 'scopestart tok))
	 (setup-fn (semantic-find-nonterminal-by-token 'setupfunction tok))
         (pmode (semantic-bnf-find-parser-mode tok))
	 )
    (if (not dest)
	(error "You must specify a destination table in your BNF file"))
    (save-excursion
      (set-buffer (marker-buffer dest))
      ;; Keyword table
      (when keydest
	(goto-char keydest)
	(re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
	(if (looking-at "\\s-*\\(nil\\|(semantic-flex-make-keyword-table\\)")
	    (delete-region (point) (save-excursion (forward-sexp 1) (point))))
	(delete-blank-lines)
	(let ((key (semantic-find-nonterminal-by-token 'keyword tok))
	      keys
	      (put (semantic-find-nonterminal-by-token 'put tok))
	      (start (point)))
	  (if (not key)
	      (insert "nil\n ")
	    (insert "(semantic-flex-make-keyword-table \n `(")
	    ;; Get all the keys
	    (while key
	      (insert " (" (nth 3 (car key)) " . " (car (car key)) ")\n ")
	      (setq key (cdr key)))
	    (insert ")\n  '(\n ")
	    ;; Now get all properties
	    (while put
	      (setq keys (nth 2 (car put)))
	      (while keys
		(setq key (semantic-find-nonterminal-by-token 'keyword tok))
		(let ((a (assoc (if (listp (car keys))
				    (car (car keys))
				  (car keys))
				key)))
		  (if (not a) (error "Token %s not found" (car keys)))
		  (let ((pairs (nth 3 (car put))))
		    (while pairs
		      (insert "  ("
			      (nth 3 a) " "
			      (car (car pairs)) " "
			      (car (cdr (cdr (car pairs)))) ")\n ")
		      (setq pairs (cdr pairs)))))
		(setq keys (cdr keys)))
	      (setq put (cdr put)))
	    (insert "))\n "))
	  (save-excursion
            (indent-region start (point) nil)))
	(eval-defun nil))
      ;; Token table
      (when tokdest
	(goto-char tokdest)
	(re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
        (if (looking-at "\\s-*\\('?(\\|nil\\)")
            (delete-region (point) (save-excursion (forward-sexp 1) (point))))
        (delete-blank-lines)
	(let ((tokens (semantic-bnf-token-table tok))
	      (start (point)))
	  (if (not tokens)
	      (insert "nil\n ")
            (insert "'")
            (pp tokens (current-buffer)))
	  (save-excursion
            (indent-region start (point) nil)))
	(eval-defun nil))
      ;; Insert setup code in the startup function or hook
      (when (semantic-bnf-find-setup-code tok fname)
	;; Point should now be in the region to add stuff
	;; Add in the bovine table to be used
	(indent-region
	 (point)
	 (let ((var (semantic-find-nonterminal-by-token 'parsetable tok))
	       (key (semantic-find-nonterminal-by-token 'keywordtable tok)))
	   (when var
	     ;; The bovine table
	     (insert "(setq semantic-toplevel-bovine-table "
		     (semantic-token-name (car var)) "\n ")
	     (insert "semantic-toplevel-bovine-table-source \""
		     fname "\")\n")
	     )
	   ;; Keytable setup
	   (when key
	     (insert "(setq semantic-flex-keywords-obarray "
		     (semantic-token-name (car key)) ")\n "))
	   ;; Is there more than one major mode?
	   (if (and (listp mode) (> (length mode) 1))
	       (insert "(setq semantic-equivalent-major-modes '"
		       (format "%S" mode) ")\n"))
	   ;; Add in user specified settings
	   (let ((settings (semantic-find-nonterminal-by-token 'setting tok)))
	     (while settings
	       (insert (nth 2 (car settings)))
	       (insert "\n ")
	       (setq settings (cdr settings))))
	   (point))
	 nil)
	(eval-defun nil))
      ;; The table
      (goto-char dest)
      (re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
      (if (looking-at "\\s-*\\(`?(\\|nil\\)")
	  (delete-region (point) (save-excursion (forward-sexp 1) (point))))
      (delete-blank-lines)
      (cond
       ((eq pmode 'lalr)
        ;; generate table for the LALR parser
        (semantic-bnf-to-lalr tok start))
       (t
        ;; generate table for the default parser
        (semantic-bnf-to-bovine
         tok (if start (semantic-token-name (car start)))
         (if scopestart (semantic-token-name (car scopestart))))))
      (if semantic-bnf-indent-table
          (save-excursion
            (message "Indenting table....")
            (indent-region (progn (re-search-backward "(def\\(var\\|const\\)\\s-+")
                                  (goto-char (match-beginning 0))
                                  (point))
                           (progn (forward-sexp 1) (point))
                           nil)))
      (eval-defun nil))
    (message "Done.")
    (when mode
      (save-excursion
  	(let ((bufs (buffer-list)))
  	  (while bufs
  	    (set-buffer (car bufs))
  	    (if (member major-mode mode)
  		(progn
  		  (if setup-fn
  		      (funcall (intern (semantic-token-name (car setup-fn))))
  		    (funcall mode)))
  	      )
  	    (setq bufs (cdr bufs))))))))

(defun semantic-bnf-generate-one-rule ()
  "Generate code for one rule in a temporary buffer."
  (interactive)
  (semantic-bovinate-toplevel t)
  (let ((r (semantic-current-nonterminal)))
    (if (or (not r) (not (eq (semantic-token-token r) 'rule)))
	(error "No rule to expand nearby"))
    (pop-to-buffer "*Rule Expansion*" t)
    (save-excursion
      (set-buffer "*Rule Expansion*")
      (erase-buffer)
      (insert "Expanding rule [" (semantic-token-name r) "]\n\n")
      (semantic-bnf-to-bovine (list r)))))

;;; Debugging support
;;
;; Source level debugging if a BNF table requires a few simple functions.
(defun semantic-bnf-skip-string-or-comment ()
  "Return non-nil if point was moved after a string or comment."
  (let ((state (parse-partial-sexp (save-excursion
                                     (beginning-of-line) (point))
                                   (point))))
    (cond ((nth 3 state) ;; string
           (re-search-backward "\\s\"")
           (forward-sexp)
           t)
          ((nth 4 state) ;; comment
           (forward-line)
           t)
          (t
           nil))))

(defun semantic-bnf-find-state-position (rule matchlistindex matchindex)
  "Find the current debugger position in the current buffer.
RULE is a symbol representing the rule name we are currently in.
MATCHLISTINDEX is the index to the current match list being tested.
MATCHINDEX is the index into the matchlist being tested."
  (let* ((start (car (semantic-find-nonterminal-by-token 'start (current-buffer))))
	 (findme (if (and start (eq rule 'bovine-toplevel))
		     (semantic-token-name start)
		   (symbol-name rule)))
	 (r (semantic-find-nonterminal-by-name
	     findme (semantic-find-nonterminal-by-token
		     'rule (current-buffer)))))
    (if (not r)
	(error "Semantic debugger error: Cannot find rule %s" findme))
    ;; Find the rule
    (goto-char (semantic-token-start r))
    ;; find the matchlist
    (re-search-forward ":\\s-*")
    (while (/= matchlistindex 0)
      (re-search-forward "\\s-*|\\s-*")
      ;; If point is in a comment or a string skip it
      (or (semantic-bnf-skip-string-or-comment)
          (setq matchlistindex (1- matchlistindex)))
      )
    ;; find the specific token we are matching
    (while (/= matchindex 0)
      (when (semantic-bnf-looking-at-%token-not-keyword)
	(setq matchindex (1- matchindex)))
      (forward-sexp 1)
      (setq matchindex (1- matchindex))
      )
    (skip-chars-forward " \t\n")
    ;; Leave the cursor here, and let them highlight if for us
    (current-buffer)
    ))

(defun semantic-bnf-looking-at-%token-not-keyword ()
  "Return non-nil if the token following the cursor is a %token.
Some tokens are keywords.  Make sure we know the difference."
  (when (looking-at "\\s-*\\(\\(\\w\\|\\s_\\)+\\)")
    (semantic-find-nonterminal-by-name
     (match-string 1)
     (semantic-find-nonterminal-by-token 'token (current-buffer)))))

(defun semantic-bnf-find-source-on-load-path (sourcefile)
  "Find the BNF file SOURCEFILE on the Emacs `load-path'.
Once found, put it in a buffer, and return it."
  (let ((sf (locate-library sourcefile)))
    (if sf (find-file-noselect sf)))
  )

;;; Semantic BNF mode
;;
;; Major mode for editing BNF files.  More importantly, define a syntax
;; table so that the semantic do-whatsis will work correctly.
(defvar semantic-bnf-syntax-table nil
  "Syntax used in a BNF buffer.")

(if semantic-bnf-syntax-table
    nil
  (setq semantic-bnf-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?: "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?| "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?% "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\; "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\" "\"" semantic-bnf-syntax-table)
  (modify-syntax-entry ?- "_" semantic-bnf-syntax-table)
  (modify-syntax-entry ?# "<" semantic-bnf-syntax-table)
  (modify-syntax-entry ?\n ">" semantic-bnf-syntax-table)
  'foo
  )

(defvar semantic-bnf-mode-hook nil
  "Hook run when starting BNF mode.")

(defvar semantic-bnf-mode-keywords
  `((";\\s-*[^#\n ].*$" 0 font-lock-comment-face)
    ("^\\(\\w+\\)[ \n\r\t]*:" 1 font-lock-function-name-face)
    ("\\<\\(EMPTY\\|symbol\\|number\\|punctuation\\|string\\|semantic-list\
\\|\\(open\\|close\\)-paren\\|comment\\)\\>"
     1 font-lock-keyword-face)
    ("(\\s-*\\(ASSOC\\|EXPAND\\(FULL\\)?\\)\\>"
     1 ,(if (featurep 'xemacs)
            'font-lock-preprocessor-face
          'font-lock-builtin-face))
    ("\\$[0-9]+" 0 font-lock-variable-name-face)
    ("%" 0 font-lock-reference-face)
    ("%\\(\\w+\\)" 1 font-lock-type-face)
    )
  "Font Lock keywords used to highlight BNF buffer.")

(defvar semantic-bnf-map nil
  "Keymap used in `semantic-bnf-mode'.")

(if semantic-bnf-map
    nil
  (setq semantic-bnf-map (make-sparse-keymap))
  (define-key semantic-bnf-map "\t" 'semantic-bnf-indent)
  (define-key semantic-bnf-map "|" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map ";" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "#" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "%" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "(" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map ")" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "\C-c\C-c" 'semantic-bnf-generate-and-load-no-indent)
  (define-key semantic-bnf-map "\C-cc" 'semantic-bnf-generate-and-load)
  (define-key semantic-bnf-map "\C-cr" 'semantic-bnf-generate-one-rule)
  (define-key semantic-bnf-map "\M-\t" 'semantic-bnf-complete)
  )

(eval-after-load "speedbar" '(speedbar-add-supported-extension ".bnf"))

;;;###autoload
(defalias 'bnf-mode 'semantic-bnf-mode)
;;;###autoload
(defun semantic-bnf-mode ()
  "Initialize a buffer for editing BNF code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-bnf-mode
	mode-name "BNF")
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "# *")
  (set-syntax-table semantic-bnf-syntax-table)
  (use-local-map semantic-bnf-map)
  (setq semantic-toplevel-bovine-table semantic-bovine-bnf-table)
  (setq semantic-flex-keywords-obarray semantic-bnf-keyword-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'semantic-bnf-indent)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'semantic-bnf-fill-paragraph)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((semantic-bnf-mode-keywords)
			     nil ; do not do string/comment highlighting
			     nil ; keywords are case insensitive.
			     ;; This puts _ & - as a word constituent,
			     ;; simplifying our keywords significantly
			     ((?_ . "w") (?- . "w"))))
  (setq semantic-symbol->name-assoc-list
	'( (keyword . "Keyword")
	   (token . "Token")
	   (rule  . "Rule")
	   )
	imenu-create-index-function 'semantic-create-imenu-index)
  (semantic-install-function-overrides
   '( (abbreviate-nonterminal . semantic-bnf-abbreviate-nonterminal)
      (summarize-nonterminal . semantic-bnf-summarize-nonterminal)
      (eldoc-current-symbol-info . semantic-bnf-ecsi)
      (nonterminal-children . semantic-bnf-nonterminal-children)
      )
   t)
  (make-local-variable 'semantic-face-alist)
  (setq semantic-face-alist
	(append semantic-face-alist
		'( (rule . font-lock-function-name-face)
		   (keyword . font-lock-keyword-face)
		   (token . font-lock-variable-name-face) )
		))
		    
  (run-hooks 'semantic-bnf-mode-hook))

(defun semantic-bnf-nonterminal-children (token &optional positiononly)
  "Return the children belonging to TOKEN.
These children may or not be full tokens for bnf files, but will have
overlays associated with them.
Optional argument POSITIONONLY is passed to the default function but is not
used locally."
  (if (eq (semantic-token-token token) 'put)
      (let ((a (nth 2 token))
	    (b (nth 3 token)))
	(if (not (semantic-token-with-position-p (car a)))
	    (setq a nil))
	(if (not (semantic-token-with-position-p (car b)))
	    (setq b nil))
	(append a b)
	)
    (semantic-nonterminal-children-default token))
  )

(defun semantic-bnf-abbreviate-nonterminal (token &optional parent color)
  "Return a string abbreviation of TOKEN.
Optional PARENT is not used.
Optional COLOR is used to flag if color is added to the text."
  (let ((tok (semantic-token-token token))
	(name (semantic-name-nonterminal token parent color)))
    (cond
     ((eq tok 'rule) (concat name ":"))
     ((eq tok 'setting) "%settings%")
     ((or (eq tok 'token) (eq tok 'keyword)) name)
     (t (concat "%" (symbol-name tok) " " name)))))

(defun semantic-bnf-summarize-nonterminal (token &optional parent color)
  "Return a string summarizing TOKEN.
Optional PARENT is not used.
Optional argument COLOR determines if color is added to the text."
  (let ((tok (semantic-token-token token))
	(name (semantic-name-nonterminal token parent color))
	(label nil)
	(desc nil))
    (cond
     ((eq tok 'rule)
      (setq label "Rule: "
	    desc (concat " with "
			 (int-to-string (length (nth 3 token)))
			 " match lists.")))
     ((eq tok 'keyword)
      (setq label "Keyword: "
	    desc (concat " " (nth 3 token))))
     ((eq tok 'token)
      (setq label "Token: "
	    desc (concat " " (nth 2 token) " " (nth 3 token))))
     (t (setq desc
	      (semantic-bnf-abbreviate-nonterminal token parent color))))
    (if (and color label)
	(setq label (semantic-colorize-text label 'label)))
    (if (and color label desc)
	(setq desc (semantic-colorize-text desc 'comment)))
    (if label
	(concat label name desc)
      ;; Just a description is the abbreviated version
      desc))
  )

(defvar semantic-bnf-syntax-help
  `( ("symbol" . "Syntax: A symbol of alpha numeric and symbol characters")
     ("number" . "Syntax: Numeric characters.")
     ("punctuation" . "Syntax: Punctuation character.")
     ("semantic-list" . "Syntax: A list delimited by any valid list characters")
     ("open-paren" . "Syntax: Open Parenthesis character")
     ("close-paren" . "Syntax: Close Parenthesis character")
     ("string" . "Syntax: String character delimited text")
     ("comment" . "Syntax: Comment character delimited text")
     ("EMPTY" . "Syntax: Match empty text")
     ("ASSOC" . "Lambda Key: (ASSOC key1 value1 key2 value2 ...)")
     ("EXPAND" . "Lambda Key: (EXPAND <list id> <rule>)")
     ("EXPANDFULL" . "Lambda Key: (EXPANDFULL <list id> <rule>)")
     ("$1" . "Match Value: Value from match list in slot 1")
     ("$2" . "Match Value: Value from match list in slot 2")
     ("$3" . "Match Value: Value from match list in slot 3")
     ("$4" . "Match Value: Value from match list in slot 4")
     ("$5" . "Match Value: Value from match list in slot 5")
     ("$6" . "Match Value: Value from match list in slot 6")
     ("$7" . "Match Value: Value from match list in slot 7")
     ("$8" . "Match Value: Value from match list in slot 8")
     ("$9" . "Match Value: Value from match list in slot 9")
     ("nil" . "Value: Empty List, False, nothing.")
     )
  "Association of syntax elements, and the corresponding help.")

(defun semantic-bnf-ecsi ()
  "Return an info string about the current context."
  (let* ((sym (semantic-ctxt-current-symbol))
	 (summ (assoc (car sym) semantic-bnf-syntax-help))
	 (found (cdr summ)))
    (if found
	found
      (senator-eldoc-print-current-symbol-info-default)
      )))

(defun semantic-bnf-electric-punctuation ()
  "Insert and re-indent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (semantic-bnf-indent))

(defun semantic-bnf-in-settings-p (&optional point)
  "Non-nil if POINT is in a settings block."
  (condition-case nil
      (save-excursion
	(if point (goto-char point))
	(up-list -1)
	(while (not (eq (preceding-char) ?%))
	  (up-list -1))
	t)
    (error nil)))

(defun semantic-bnf-in-lambda-continuation-p (&optional point)
  "Non-nil if POINT is in a settings block."
  (condition-case nil
      (save-excursion
	(if point (goto-char point) (setq point (point)))
	(beginning-of-line)
	(condition-case nil
	    (while t
	      (up-list -1))
	  (error nil))
	(end-of-line)
	(< (point) point)
	)
    (error nil)))

(defun semantic-bnf-previous-colon-indentation ()
  "Calculation the indentation of the last colon operator.
Returns the previous colon's column."
  (save-excursion
    (let ((p (point))
	  (ci (progn
                (end-of-line)
		(if (re-search-backward "^\\s-*\\(\\w\\|\\s_\\)+[ \n\r\t]*:" nil t)
                    (let ((here (match-end 0)))
                      (if (save-excursion
                            (goto-char here)
                            (looking-at "\\s-*$"))
                          ;; line ends with the colon
                          2
                        (if (save-excursion
                              (goto-char here)
                              (beginning-of-line)
                              (looking-at "\\s-*:"))
                            ;; line begin with the colon
                            2
                          (beginning-of-line)
                          (- here 1 (point)))))
		  0)))
	  (cp (point))
	  (sc nil))
      (goto-char p)
      (while (and (re-search-backward "^\\s-*;\\s-*$" nil t)
		  (semantic-bnf-in-lambda-continuation-p)))
      (and (/= (point) p)
           (looking-at "\\s-*;")
           (setq sc t))
      (if sc
	  (if (< (point) cp)
	      ci
	    0)
	ci))))

(defun semantic-bnf-do-lisp-indent (&optional point)
  "Run the stander Emacs Lisp indenter on a line of code.
Optional argument POINT is the position on the line to indent."
  (condition-case nil
      (save-excursion
	(if point (goto-char point) (setq point (point)))
	(up-list -1)
	(condition-case nil
	    (while t
	      (up-list -1))
	  (error nil))
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region (point) point)
	  (goto-char point)
	  (with-syntax-table emacs-lisp-mode-syntax-table
	    (lisp-indent-line))))
    (error nil)))

(defun semantic-bnf-indent ()
  "Indent the current line according to BNF rules."
  (interactive)
  (if (semantic-bnf-in-settings-p)
      (semantic-bnf-do-lisp-indent)
    (if (semantic-bnf-in-lambda-continuation-p)
	(semantic-bnf-do-lisp-indent)
      (save-excursion
	(beginning-of-line)
	(let ((indent (semantic-bnf-previous-colon-indentation)))
	  (cond
	   ((or (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
		(looking-at "\\s-*%"))
	    (delete-horizontal-space))
	   (t
	    (save-excursion
	      (if (and (not (looking-at "\\s-*[:|;#]"))
		       (/= indent 0))
		  (setq indent (+ 2 indent))))
	    (if (= (current-indentation) indent)
		nil
	      (delete-horizontal-space)
	      (indent-to indent))))))))
  (if (bolp) (if (looking-at "\\s-+") (end-of-line))))

(defun semantic-bnf-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle BNF comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial #."
  (interactive "P")
  (let (
	;; Non-nil if the current line contains a comment.
	has-comment
        
	;; Non-nil if the current line contains code and a comment.
	has-code-and-comment
        
        ;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
       
       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((condition-case nil
	    (save-restriction
	      (narrow-to-region (point-min)
				(save-excursion (end-of-line) (point)))
	      (while (not (looking-at "#\\|$"))
		(skip-chars-forward "^#\n\"\\\\?")
		(cond
		 ((eq (char-after (point)) ?\\) (forward-char 2))
		 ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
	      (looking-at "#+[\t ]*"))
	  (error nil))
	(setq has-comment t has-code-and-comment t)
	(setq comment-fill-prefix
	      (concat (make-string (/ (current-column) 8) ?\t)
		      (make-string (% (current-column) 8) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))))))
    
    (if (not has-comment)
        nil
      ;; Narrow to include only the comment, and then fill the region.
      (save-excursion
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region
           ;; Find the first line we should include in the region to fill.
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*#")))
	     ;; We may have gone too far.  Go forward again.
	     (or (looking-at ".*#")
		 (forward-line 1))
	     (point))
           ;; Find the beginning of the first line past the region to fill.
	   (save-excursion
	     (while (progn (forward-line 1)
			   (looking-at "^[ \t]*#")))
	     (point)))
          
          ;; Lines with only semicolons on them can be paragraph boundaries.
	  (let* ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
		 (paragraph-separate (concat paragraph-start "\\|[ \t#]*$"))
		 (paragraph-ignore-fill-prefix nil)
		 (fill-prefix comment-fill-prefix)
		 (after-line (if has-code-and-comment
				 (save-excursion
				   (forward-line 1) (point))))
		 (end (progn
			(forward-paragraph)
			(or (bolp) (newline 1))
			(point)))
		 ;; If this comment starts on a line with code,
		 ;; include that like in the filling.
		 (beg (progn (backward-paragraph)
			     (if (eq (point) after-line)
				 (forward-line -1))
			     (point))))
	    (fill-region-as-paragraph beg end
				      justify nil
				      (save-excursion
					(goto-char beg)
					(if (looking-at fill-prefix)
					    nil
					  (re-search-forward comment-start-skip)
					  (point))))))))
    t))

(defun semantic-bnf-complete ()
  "Complete the symbol under point from various sources."
  (interactive)
  (if (or (semantic-bnf-in-settings-p)
	  (semantic-bnf-in-lambda-continuation-p))
      ;; In a lisp part... do lisp completion
      (lisp-complete-symbol)
    ;; In BNF part, to BNF completion.
    (senator-complete-symbol)))

(add-to-list 'auto-mode-alist '("\\.bnf$" . semantic-bnf-mode))

(eval-after-load "which-func"
  '(unless (eq which-func-modes t)
     '(add-to-list 'which-func-modes 'semantic-bnf-mode)))

;; Add the necessary hooks so that `C-h C-i' of a BNF setting will
;; launch the info browser and bring up the page describing the
;; setting. 
(eval-after-load "info-look"
  '(let ()
     (info-lookup-add-help
      :mode 'semantic-bnf-mode
      :regexp "%[_a-zA-Z][_a-zA-Z0-9]*"
      :ignore-case t
      :doc-spec '(
		  ("(semantic)Index" nil
		   "^[ \t]+- [^:]+:[ \t]*" "\\b")))))

(provide 'semantic-bnf)

;;; semantic-bnf.el ends here
