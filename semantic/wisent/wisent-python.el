;;; wisent-python.el --- LALR grammar for Python
;;
;; Copyright (C) 2002 Richard Kim
;;
;; Author: Richard Kim <ryk@dspwiz.com>
;; Maintainer: Richard Kim <ryk@dspwiz.com>
;; Created: June 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-python.el,v 1.4 2002/06/22 03:33:43 emacsman Exp $
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
;; The goal is to provide full parser for python so that all the higher
;; level semantic tools work for python as they to for other languages.
;;
;; This is still a work in progress.
;; It is able to parse simple Python expressions, but not yet
;; generally usable.
;;
;; The official Grammar file from the Python source code distribution
;; was the starting point of this wisent version.
;;
;; Approximate non-terminal (NT) hierarchy of the python grammer for
;; the `single_input' NT is shown below.
;;
;;   goal
;;     single_input
;;       NEWLINE
;;       simple_stmt
;;         small_stmt_list semicolon_opt NEWLINE
;;           small_stmt
;;             print_stmt
;;             del_stmt
;;             pass_stmt
;;             flow_stmt
;;             import_stmt
;;             global_stmt
;;             exec_stmt
;;             assert_stmt
;;             expr_stmt
;;               augassign
;;               testlist
;;                 test
;;                   test_testlambdef
;;                   test_test
;;                     and_test
;;                       not_test
;;                         comparison
;;                           expr
;;                             xor_expr
;;                               and_expr
;;                                 shift_expr
;;                                   arith_expr
;;                                     term
;;                                       factor
;;                                         power
;;                                           atom
;;                                           trailer
;;                                             ( arglist_opt )
;;                                               test
;;                                             [ subscriptlist ]
;;       compound_stmt NEWLINE
;;         if_stmt
;;         while_stmt
;;         for_stmt
;;         try_stmt
;;         funcdef
;;         classdef

;;; To do:
;;
;; * Debug the grammar so that it can parse at least all standard *.py
;;   files distributed along with python.
;;
;; * Enhance the lexer so that DEDENT, INDENT, and NEWLINE tokens are
;;   properly suppressed when a logical line continues on two or more
;;   physical lines explicitly via '\'.
;;
;; * Figure out why "server_address = ('', port)" cannot be parsed!
;;
;; * Delete most semantic rules when the grammar is debugged.
;;
;; * Updated semantic-python-number-regexp based on Python reference
;;   manual.  Currently version is an exact replica from semantic-java.el.
;;
;; * Optimize `string-indentation'.  This was a quick hack to get us going.
;;
;; * Figure out what ENDMARKER token is for.

(require 'wisent-bovine)

;;;****************************************************************************
;;;@ Support Code
;;;****************************************************************************
;;
;; Some of these need to come before `wisent-python-default-setup' so that
;; symbols are defined before their first use.

;; Indentation stack to keep track of INDENT tokens generated without
;; matching DEDENT tokens. Generation of each INDENT token results in
;; a new integer being added to the beginning of this list where the
;; integer represents the indentation of the current line. Each time a
;; DEDENT token is generated, the latest entry added is popped off
;; this list.
(defvar wisent-python-lexer-indent-stack '(0))

;; When a physical line ends without one or more matching ")", "]", or
;; "}", then the following line is an implicit continuation of the
;; current line. This means that the NEWLINE token plus any
;; immediately following INDENT or DEDENT tokens must be discarded in
;; the lexer stage as required by the python grammar.
;;
;; This stack, if non-nil, indicates that the following line is an
;; implicit continuation of the current line. The idea is to push "(",
;; "[", and "{" tokens whenever they are encountered, then popped when
;; matching token is encountered.
(defvar wisent-python-matching-pair-stack nil)

;; Quick hack to compute indentation.
;; Probably not good enough for production use.
;; Return -1 if a blank line containing white space and/or comments only.
;; Otherwise return the indentation of the line at POS.
(defsubst string-indentation (pos)
  (save-excursion
    (goto-char pos)
    (cond ((eobp)
	   '(setq wisent-flex-istream
		 (cons (cons 'newline (cons (point) (point)))
		       wisent-flex-istream))
	   0)
	  ((looking-at "\\s-*\\(#\\|$\\)") -1)
	  (t (current-indentation)))))

(defun wisent-python-lex-bol ()
  "Handle BOL syntactic tokens.
Produce corresponding INDENT or DEDENT python's lexical tokens."
  (let* ((is          wisent-flex-istream)
	 (stok        (car is))
	 (curr-indent (string-indentation (cadr stok)))
	 (last-indent (or (car wisent-python-lexer-indent-stack) 0)))
    (cond
     ;; No indentation change
     ((= curr-indent -1)
      ;; Eat 'bol token
      (setq wisent-flex-istream (cdr is))
      ;; Eat the next NEWLINE token
      (if (eq (caar wisent-flex-istream) 'newline)
	  (setq wisent-flex-istream (cdr wisent-flex-istream)))
      (wisent-flex))
     ((= curr-indent last-indent)
      ;;  Just eat 'bol token
      (setq wisent-flex-istream (cdr is))
      (wisent-flex))
     ;; Indentation increased
     ((> curr-indent last-indent)
      ;; Eat 'bol token
      (setq wisent-flex-istream (cdr is))
      ;; Return an INDENT lexical token
      (push curr-indent wisent-python-lexer-indent-stack)
      (list 'INDENT curr-indent))
     ;; Indentation decreased
     (t
      ;; Pop one item from indentation stack
      (pop wisent-python-lexer-indent-stack)
      ;; Leave 'bol token in place
      ;; Return a DEDENT lexical token
      (list 'DEDENT last-indent)))))

(defun wisent-python-lex-open-paren ()
  (let* ((stok (car wisent-flex-istream))
	 (tok-string (buffer-substring-no-properties (cadr stok) (cddr stok))))
    (setq wisent-flex-istream (cdr wisent-flex-istream))
    (cond
     ((string= tok-string "(")
      (setq wisent-python-matching-pair-stack
	    (cons ")" wisent-python-matching-pair-stack))
      (cons 'LPAREN (cons "(" (cdr stok))))
     ((string= tok-string "[")
      (setq wisent-python-matching-pair-stack
	    (cons "]" wisent-python-matching-pair-stack))
      (cons 'LBRACK (cons "[" (cdr stok))))
     (t
      (setq wisent-python-matching-pair-stack
	    (cons "}" wisent-python-matching-pair-stack))
      (cons 'LBRACE (cons "{" (cdr stok)))))))

(defun wisent-python-lex-close-paren ()
  (let* ((stok (car wisent-flex-istream))
	 (tok-string (buffer-substring-no-properties (cadr stok) (cddr stok))))
    ;; If matching delimiter, then pop it off the stack, else error.
    (if (string= (car wisent-python-matching-pair-stack) tok-string)
	(setq wisent-python-matching-pair-stack
	      (cdr wisent-python-matching-pair-stack))
      (error "Expected %s token, but got %s"
	     (car wisent-python-matching-pair-stack) tok-string))
    (setq wisent-flex-istream (cdr wisent-flex-istream))
    (cond
     ((string= tok-string ")")
      (cons 'RPAREN (cons ")" (cdr stok))))
     ((string= tok-string "]")
      (cons 'RBRACK (cons "]" (cdr stok))))
     (t
      (cons 'RBRACE (cons "}" (cdr stok)))))))

(defun wisent-python-lex-newline ()
  "Handle NEWLINE syntactic tokens.
If the following line is an implicit continuation of current line,
then throw away any immediately following INDENT and DEDENT tokens."
  (let ((stok (car wisent-flex-istream)))
    ;; Pop the current 'newline token
    (setq wisent-flex-istream (cdr wisent-flex-istream))
    ;; If implicit line continuation,
    (cond
     (wisent-python-matching-pair-stack
      ;; Pop the immediately following `bol' and `newline' tokens.
      (while (memq (caar wisent-flex-istream) '(bol newline))
	(setq wisent-flex-istream (cdr wisent-flex-istream)))
      (wisent-flex))
     (t
      ;; Replace 'newline semantic token with NEWLINE wisnet token, then
      ;; return it.
      (cons 'NEWLINE (cons "\n" (cdr stok)))))))

(defconst semantic-python-number-regexp
  (eval-when-compile
    (concat "\\("
	    "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[.][fFdD]\\>"
	    "\\|"
	    "\\<[0-9]+[.]"
	    "\\|"
	    "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
	    "\\|"
	    "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
	    "\\|"
	    "\\<[0-9]+[lLfFdD]?\\>"
	    "\\)"
	    ))
  "Lexer regexp to match Python number terminals.
To begin with, this is an exact copy of `semantic-java-number-regexp'.
This needs to be updated based on the Python language spec when
we get around ot it.")

(defun semantic-flex-python-triple-quotes ()
  "Create a 'string token from strings quoted with triple double-quotes."
  (let (beg end)
    (setq beg (point))
    (forward-char 3)
    (search-forward "\"\"\"")
    (setq end (point))
    (cons 'string (cons beg end))))

;; This should be called everytime before parsing starts.
;; Is there a better hook than python-mode-hook which gets called
;; at the start of every parse? -ryk6/21/02.
(add-hook 'python-mode-hook #'wisent-python-default-setup)

;;;****************************************************************************
;;;@ Code Filled in by wisent-wy-update-outputfile
;;;****************************************************************************

(defconst wisent-python-parser-tables
  (eval-when-compile
;;DO NOT EDIT! Generated from wisent-python.wy - 2002-06-21 13:12-0700
    (wisent-compile-grammar
     '((NEWLINE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LTLTEQ GTGTEQ EXPEQ DIVDIVEQ DIVDIV LTLT GTGT EXPONENT EQ GE LE PLUSEQ MINUSEQ MULTEQ DIVEQ MODEQ AMPEQ OREQ HATEQ LTGT NE HAT LT GT AMP MULT DIV MOD PLUS MINUS PERIOD TILDE BAR COLON SEMICOLON COMMA ASSIGN BACKQUOTE BACKSLASH STRING_LITERAL NUMBER_LITERAL NAME INDENT DEDENT AND ASSERT BREAK CLASS CONTINUE DEF DEL ELIF ELSE EXCEPT EXEC FINALLY FOR FROM GLOBAL IF IMPORT IN IS LAMBDA NOT OR PASS PRINT RAISE RETURN TRY WHILE YIELD)
       nil
       (goal
	((single_input)))
       (single_input
	((NEWLINE))
	((simple_stmt))
	((compound_stmt NEWLINE)))
       (file_input
	((stmt_list ENDMARKER)))
       (ENDMARKER
	(nil))
       (stmt_list
	(nil)
	((stmt_list newline_or_stmt)))
       (newline_or_stmt
	((NEWLINE))
	((stmt)))
       (eval_input
	((testlist newline_list ENDMARKER)))
       (newline_list
	(nil)
	((newline_list NEWLINE)
	 (format "%s" $2)))
       (varargslist
	((fpdef_opt_test_zom mult_name)
	 (format "%s %s"
		 (or $1 "")
		 $2))
	((fpdef eq_test_opt fpdef_opt_test_trailer_zom comma_opt)
	 (format "%s %s %s %s" $1
		 (or $2 "")
		 (or $3 "")
		 (or $4 ""))))
       (fpdef_opt_test_zom
	(nil)
	((fpdef_opt_test_zom fpdef eq_test_opt COMMA)
	 (format "%s %s %s,"
		 (or $1 "")
		 $2
		 (or $3 ""))))
       (eq_test_opt
	(nil)
	((ASSIGN test)
	 (format " = %s" $2)))
       (mult_name
	((MULT NAME multmult_name_opt))
	((EXPONENT NAME)))
       (multmult_name_opt
	(nil)
	((COMMA EXPONENT NAME)
	 (format ", ** %s" $3)))
       (fpdef_opt_test_trailer_zom
	(nil)
	((fpdef_opt_test_trailer_zom COMMA fpdef eq_test_opt)
	 (format "%s, %s%s"
		 (or $1 "")
		 $2
		 (or $3 ""))))
       (fpdef
	((NAME))
	((LPAREN fplist RPAREN)
	 (format "(%s)"
		 (or $2 ""))))
       (fplist
	((fpdef_list comma_opt)
	 (format "%s %s"
		 (or $1 "")
		 (or $2 ""))))
       (fpdef_list
	((fpdef))
	((fpdef_list COMMA fpdef)
	 (format "%s, %s" $1 $3)))
       (stmt
	((simple_stmt))
	((compound_stmt)))
       (simple_stmt
	((small_stmt_list semicolon_opt NEWLINE)
	 (wisent-token
	  (format "%s %s" $1
		  (or $2 ""))
	  'simple_stmt nil nil)))
       (small_stmt_list
	((small_stmt))
	((small_stmt_list SEMICOLON small_stmt)
	 (format "%s; %s" $1 $3)))
       (semicolon_opt
	(nil)
	((SEMICOLON)))
       (small_stmt
	((expr_stmt))
	((print_stmt))
	((del_stmt))
	((pass_stmt))
	((flow_stmt))
	((import_stmt))
	((global_stmt))
	((exec_stmt))
	((assert_stmt)))
       (expr_stmt
	((testlist expr_stmt_trailer)
	 (if $2
	     (format "%s %s" $1 $2)
	   (format "%s" $1))))
       (expr_stmt_trailer
	((augassign testlist)
	 (format "%s %s" $1 $2))
	((eq_testlist_zom)))
       (eq_testlist_zom
	(nil)
	((eq_testlist_zom ASSIGN testlist)
	 (format "%s = %s"
		 (or $1 "")
		 $3)))
       (augassign
	((PLUSEQ))
	((MINUSEQ))
	((MULTEQ))
	((DIVEQ))
	((MODEQ))
	((AMPEQ))
	((OREQ))
	((HATEQ))
	((LTLTEQ))
	((GTGTEQ))
	((EXPEQ))
	((DIVDIVEQ)))
       (print_stmt
	((PRINT print_stmt_trailer)
	 (if $2
	     (format "%s %s" $1 $2)
	   (format "%s" $1))))
       (print_stmt_trailer
	((test_list_with_opt_comma_opt))
	((GTGT test trailing_test_list_with_opt_comma_opt)
	 (if $3
	     (format "%s %s %s" $1 $2 $3)
	   (format "%s %s" $1 $2))))
       (trailing_test_list_with_opt_comma_opt
	(nil)
	((trailing_test_list comma_opt)))
       (trailing_test_list
	((COMMA test)
	 (format ", %s" $2))
	((trailing_test_list COMMA test)
	 (format "%s, %s" $1 $3)))
       (test_list_with_opt_comma_opt
	(nil)
	((testlist)))
       (del_stmt
	((DEL exprlist)
	 (format "del %s" $2)))
       (pass_stmt
	((PASS)))
       (flow_stmt
	((break_stmt))
	((continue_stmt))
	((return_stmt))
	((raise_stmt))
	((yield_stmt)))
       (break_stmt
	((BREAK)))
       (continue_stmt
	((CONTINUE)))
       (return_stmt
	((RETURN testlist_opt)
	 (if $2
	     (format "return %s" $2)
	   "return")))
       (testlist_opt
	(nil)
	((testlist)))
       (yield_stmt
	((YIELD testlist)
	 (format "%s %s" $1 $2)))
       (raise_stmt
	((RAISE zero_one_two_or_three_tests)))
       (zero_one_two_or_three_tests
	(nil)
	((test zero_one_or_two_tests)
	 (format "%s %s" $1
		 (or $2 ""))))
       (zero_one_or_two_tests
	(nil)
	((COMMA test zero_or_one_comma_test)
	 (format ", %s %s" $2
		 (or $3 ""))))
       (zero_or_one_comma_test
	(nil)
	((COMMA test)
	 (format ", %s" $2)))
       (import_stmt
	((IMPORT dotted_as_name_list)
	 (format "import %s" $2))
	((FROM dotted_name IMPORT star_or_import_as_name_list)
	 (format "from %s import %s" $2
		 (or $4 ""))))
       (dotted_as_name_list
	((dotted_as_name))
	((dotted_as_name_list COMMA dotted_as_name)
	 (format "%s, %s" $1 $3)))
       (star_or_import_as_name_list
	((MULT))
	((import_as_name_list)))
       (import_as_name_list
	((import_as_name))
	((import_as_name_list COMMA import_as_name)
	 (format "%s, %s" $1 $3)))
       (import_as_name
	((NAME name_name_opt)
	 (format "%s %s" $1
		 (or $2 ""))))
       (dotted_as_name
	((dotted_name name_name_opt)
	 (format "%s %s" $1
		 (or $2 ""))))
       (name_name_opt
	(nil)
	((NAME NAME)
	 (format "%s %s" $1 $2)))
       (dotted_name
	((NAME))
	((dotted_name PERIOD NAME)
	 (format "%s %s %s"
		 (or $1 "")
		 $2 $3)))
       (global_stmt
	((GLOBAL comma_sep_name_list)
	 (format "global %s" $2)))
       (comma_sep_name_list
	((NAME))
	((comma_sep_name_list COMMA NAME)
	 (format "%s %s %s" $1 $2 $3)))
       (exec_stmt
	((EXEC expr exec_trailer)
	 (format "exec %s %s" $2
		 (or $3 ""))))
       (exec_trailer
	(nil)
	((IN test comma_test_opt)
	 (format "in %s %s" $2
		 (or $3 ""))))
       (comma_test_opt
	(nil)
	((COMMA test)
	 (format ", %s" $2)))
       (assert_stmt
	((ASSERT test comma_test_opt)
	 (format "assert %s %s" $2
		 (or $3 ""))))
       (compound_stmt
	((if_stmt))
	((while_stmt))
	((for_stmt))
	((try_stmt))
	((funcdef))
	((classdef)))
       (if_stmt
	((IF test COLON suite elif_suite_pair_list else_suite_pair_opt)
	 (wisent-token
	  (format "if %s: %s %s" $2
		  (or $4 "")
		  (or $5 ""))
	  'if_stmt_list nil nil)))
       (elif_suite_pair_list
	(nil)
	((elif_suite_pair_list ELIF test COLON suite)
	 (format "%s elif %s: %s"
		 (or $1 "")
		 $3
		 (or $5 ""))))
       (while_stmt
	((WHILE test COLON suite else_suite_pair_opt)
	 (wisent-token
	  (format "while %s: %s %s" $2
		  (or $4 "")
		  (or $5 ""))
	  'while_stmt_list nil nil)))
       (for_stmt
	((FOR exprlist IN testlist COLON suite else_suite_pair_opt)
	 (wisent-token
	  (format "for %s in %s: %s %s" $2 $4
		  (or $6 "")
		  (or $7 ""))
	  'for_stmt_list nil nil)))
       (try_stmt
	((TRY COLON suite except_clause_suite_pair_list else_suite_pair_opt)
	 (wisent-token
	  (format "try: %s %s %s" $3
		  (or $4 "")
		  (or $5 ""))
	  'try_stmt_list nil nil))
	((TRY COLON suite FINALLY COLON suite)
	 (wisent-token
	  (format "try: %s finally: %s" $3
		  (or $6 ""))
	  'try_stmt_list nil nil)))
       (except_clause_suite_pair_list
	((except_clause COLON suite)
	 (concat "except_clause_suite_pair_list"))
	((except_clause_suite_pair_list except_clause COLON suite)
	 (concat "except_clause_suite_pair_list")))
       (else_suite_pair_opt
	(nil)
	((ELSE COLON suite)
	 (format "else: %s"
		 (or $3 ""))))
       (except_clause
	((EXCEPT zero_one_or_two_test)))
       (zero_one_or_two_test
	(nil)
	((test zero_or_one_comma_test)))
       (suite
	((simple_stmt))
	((NEWLINE INDENT stmt_oom DEDENT)
	 (format "%s" $3)))
       (stmt_oom
	((stmt))
	((stmt_oom stmt)
	 (format "%s; %s" $1 $2)))
       (test
	((test_test))
	((lambdef)))
       (test_test
	((and_test))
	((test_test OR and_test)
	 (format "%s %s %s" $1 $2 $3)))
       (and_test
	((not_test))
	((and_test AND not_test)
	 (format "%s %s %s" $1 $2 $3)))
       (not_test
	((NOT not_test)
	 (format "%s %s" $1 $2))
	((comparison)))
       (comparison
	((expr))
	((comparison comp_op expr)
	 (format "%s %s %s" $1 $2 $3)))
       (comp_op
	((LT))
	((GT))
	((EQ))
	((GE))
	((LE))
	((LTGT))
	((NE))
	((IN))
	((NOT IN))
	((IS))
	((IS NOT)))
       (expr
	((xor_expr))
	((expr BAR xor_expr)
	 (format "%s %s %s" $1 $2 $3)))
       (xor_expr
	((and_expr))
	((xor_expr HAT and_expr)
	 (format "%s %s %s" $1 $2 $3)))
       (and_expr
	((shift_expr))
	((and_expr AMP shift_expr)
	 (format "%s %s %s" $1 $2 $3)))
       (shift_expr
	((arith_expr))
	((shift_expr shift_expr_operators arith_expr)
	 (format "%s %s %s" $1 $2 $3)))
       (shift_expr_operators
	((LTLT))
	((GTGT)))
       (arith_expr
	((term))
	((arith_expr plus_or_minus term)
	 (format "%s %s %s" $1 $2 $3)))
       (plus_or_minus
	((PLUS))
	((MINUS)))
       (term
	((factor))
	((term term_operator factor)
	 (format "%s %s %s" $1 $2 $3)))
       (term_operator
	((MULT))
	((DIV))
	((MOD))
	((DIVDIV)))
       (factor
	((prefix_operators factor)
	 (format "%s %s" $1 $2))
	((power)))
       (prefix_operators
	((PLUS))
	((MINUS))
	((TILDE)))
       (power
	((atom trailer_zom exponent_zom)
	 (format "%s %s %s" $1
		 (or $2 "")
		 (or $3 ""))))
       (trailer_zom
	(nil)
	((trailer_zom trailer)
	 (format "%s %s"
		 (or $1 "")
		 $2)))
       (exponent_zom
	(nil)
	((exponent_zom EXPONENT factor)
	 (format "%s ** %s"
		 (or $1 "")
		 $3)))
       (atom
	((LPAREN testlist_opt RPAREN)
	 (format "(%s)"
		 (or $2 "")))
	((LBRACK listmaker_opt RBRACK)
	 (format "[%s]"
		 (or $2 "")))
	((LBRACE dictmaker_opt RBRACE)
	 (format "{%s}"
		 (or $2 "")))
	((BACKQUOTE testlist BACKQUOTE)
	 (format "`%s`"
		 (or $2 "")))
	((NAME))
	((NUMBER_LITERAL))
	((one_or_more_string)))
       (listmaker_opt
	(nil)
	((listmaker)))
       (dictmaker_opt
	(nil)
	((dictmaker)))
       (one_or_more_string
	((STRING_LITERAL)
	 (read $1))
	((one_or_more_string STRING_LITERAL)
	 (format "%s %s" $1
		 (read $2))))
       (listmaker
	((test listmaker_trailer)))
       (listmaker_trailer
	((list_for))
	((testlist_trailer comma_opt)))
       (testlist_trailer
	(nil)
	((testlist_trailer COMMA test)
	 (format "%s %s %s" $1 $2 $3)))
       (lambdef
	((LAMBDA COLON test)
	 (format "%s %s %s" $1 $2 $3)))
       (trailer
	((LPAREN arglist_opt RPAREN)
	 (format "(%s)"
		 (or $2 "")))
	((LBRACK subscriptlist RBRACK)
	 (format "[%s]"
		 (or $2 "")))
	((PERIOD NAME)
	 (format ".%s" $2)))
       (arglist_opt
	(nil)
	((arglist)))
       (subscriptlist
	((comma_sep_subscript_list comma_opt)
	 (format "%s %s"
		 (or $1 "")
		 (or $2 ""))))
       (comma_sep_subscript_list
	((subscript))
	((comma_sep_subscript_list COMMA subscript)))
       (subscript
	((PERIOD PERIOD PERIOD)
	 (format "..."))
	((test))
	((zero_or_one_test COLON zero_or_one_test zero_or_one_sliceop)
	 (format "%s : %s %s"
		 (or $1 "")
		 (or $3 "")
		 (or $4 ""))))
       (zero_or_one_sliceop
	(nil)
	((sliceop)))
       (sliceop
	((COLON zero_or_one_test)
	 (format ": %s"
		 (or $2 ""))))
       (zero_or_one_test
	(nil)
	((test)))
       (exprlist
	((expr_list comma_opt)
	 (format "%s %s"
		 (or $1 "")
		 (or $2 ""))))
       (expr_list
	((expr))
	((expr_list COMMA expr)
	 (format "%s, %s" $1 $2)))
       (testlist
	((comma_sep_test_list comma_opt)
	 (if $2
	     (format "%s %s" $1 $2)
	   (format "%s" $1))))
       (comma_sep_test_list
	((test))
	((comma_sep_test_list COMMA test)
	 (format "%s %s %s" $1 $2 $3)))
       (comma_opt
	(nil)
	((COMMA)))
       (testlist_safe
	((test testlist_safe_trailer_opt)
	 (if $2
	     (format "%s %s" $1 $2)
	   (format "%s" $1))))
       (testlist_safe_trailer_opt
	(nil)
	((testlist_safe_term comma_opt)
	 (if $2
	     (format "%s %s" $1 $2)
	   (format "%s" $1))))
       (testlist_safe_term
	((COMMA test)
	 (format "%s %s" $1 $2))
	((testlist_safe_term COMMA test)
	 (format "%s %s %s" $1 $2 $3)))
       (dictmaker
	((test COLON test colon_sep_test comma_opt)
	 (format "%s : %s %s %s" $1 $3 $4
		 (or $5 ""))))
       (colon_sep_test
	(nil)
	((colon_sep_test COMMA test COLON test)
	 (format "%s, %s : %s"
		 (or $1 "")
		 $3 $4)))
       (funcdef
	((DEF NAME parameters COLON suite)
	 (wisent-token
	  (format "def %s(%s): %s" $2
		  (or $3 "")
		  (or $5 ""))
	  'funcdef nil nil)))
       (parameters
	((LPAREN varargslist_opt RPAREN)
	 (format "(%s)"
		 (or $2 ""))))
       (varargslist_opt
	(nil)
	((varargslist)))
       (classdef
	((CLASS NAME paren_testlist_opt COLON suite)
	 (wisent-token
	  (format "class %s %s: %s" $2
		  (or $3 "")
		  (or $5 ""))
	  'classdef_stmt_list nil nil)))
       (paren_testlist_opt
	(nil)
	((LPAREN testlist RPAREN)
	 (format "(%s)"
		 (or $2 ""))))
       (arglist
	((argument_comma_zom arglist_trailer)
	 (format "%s %s"
		 (or $1 "")
		 (or $2 ""))))
       (argument_comma_zom
	(nil)
	((argument_comma_zom argument COMMA)
	 (format "%s %s,"
		 (or $1 "")
		 $2)))
       (arglist_trailer
	((argument comma_opt)
	 (format "%s%s" $1
		 (or $2 "")))
	((MULT test comma_mult_mult_test_opt)
	 (format "* %s %s"
		 (or $2 "")
		 (or $3 "")))
	((EXPONENT test)
	 (format "** %s"
		 (or $2 ""))))
       (comma_mult_mult_test_opt
	(nil)
	((COMMA EXPONENT test)
	 (format ", ** %s"
		 (or $3 ""))))
       (argument
	((test eq_test_opt)
	 (format "%s" $2)))
       (test_eq_opt
	(nil)
	((test ASSIGN)
	 (format "%s = " $1)))
       (list_iter
	((list_for))
	((list_if)))
       (list_for
	((FOR exprlist IN testlist_safe list_iter_opt)))
       (list_if
	((IF test list_iter_opt)))
       (list_iter_opt
	(nil)
	((list_iter))))
     '(goal))
    )
  "Parser automaton.")

(defconst wisent-python-keywords
  (identity
;;DO NOT EDIT! Generated from wisent-python.wy - 2002-06-21 13:12-0700
   (semantic-flex-make-keyword-table
    '(("and" . AND)
      ("assert" . ASSERT)
      ("break" . BREAK)
      ("class" . CLASS)
      ("continue" . CONTINUE)
      ("def" . DEF)
      ("del" . DEL)
      ("elif" . ELIF)
      ("else" . ELSE)
      ("except" . EXCEPT)
      ("exec" . EXEC)
      ("finally" . FINALLY)
      ("for" . FOR)
      ("from" . FROM)
      ("global" . GLOBAL)
      ("if" . IF)
      ("import" . IMPORT)
      ("in" . IN)
      ("is" . IS)
      ("lambda" . LAMBDA)
      ("not" . NOT)
      ("or" . OR)
      ("pass" . PASS)
      ("print" . PRINT)
      ("raise" . RAISE)
      ("return" . RETURN)
      ("try" . TRY)
      ("while" . WHILE)
      ("yield" . YIELD))
    '(("yield" summary "...")
      ("while" summary "...")
      ("try" summary "...")
      ("return" summary "...")
      ("raise" summary "...")
      ("print" summary "...")
      ("pass" summary "...")
      ("or" summary "...")
      ("not" summary "...")
      ("is" summary " ... ")
      ("in" summary " ... ")
      ("import" summary " ... ")
      ("if" summary " ... ")
      ("global" summary " ... ")
      ("from" summary " ... ")
      ("for" summary " ... ")
      ("finally" summary " ... ")
      ("exec" summary " ... ")
      ("except" summary " ... ")
      ("else" summary " ... ")
      ("elif" summary " ... ")
      ("del" summary " ... ")
      ("def" summary " ... ")
      ("continue" summary " ... ")
      ("class" summary " ... ")
      ("break" summary " ... ")
      ("assert" summary " ... ")
      ("and" summary " ... ")))
   )
  "Keywords.")

(defconst wisent-python-tokens
  (identity
;;DO NOT EDIT! Generated from wisent-python.wy - 2002-06-21 13:12-0700
   (wisent-flex-make-token-table
    '(("bol"
       (DEDENT)
       (INDENT))
      ("symbol"
       (NAME))
      ("number"
       (NUMBER_LITERAL))
      ("string"
       (STRING_LITERAL))
      ("punctuation"
       (BACKSLASH . "\\")
       (BACKQUOTE . "`")
       (ASSIGN . "=")
       (COMMA . ",")
       (SEMICOLON . ";")
       (COLON . ":")
       (BAR . "|")
       (TILDE . "~")
       (PERIOD . ".")
       (MINUS . "-")
       (PLUS . "+")
       (MOD . "%")
       (DIV . "/")
       (MULT . "*")
       (AMP . "&")
       (GT . ">")
       (LT . "<")
       (HAT . "^")
       (NE . "!=")
       (LTGT . "<>")
       (HATEQ . "^=")
       (OREQ . "|=")
       (AMPEQ . "&=")
       (MODEQ . "%=")
       (DIVEQ . "/=")
       (MULTEQ . "*=")
       (MINUSEQ . "-=")
       (PLUSEQ . "+=")
       (LE . "<=")
       (GE . ">=")
       (EQ . "==")
       (EXPONENT . "**")
       (GTGT . ">>")
       (LTLT . "<<")
       (DIVDIV . "//")
       (DIVDIVEQ . "//=")
       (EXPEQ . "**=")
       (GTGTEQ . ">>=")
       (LTLTEQ . "<<="))
      ("close-paren"
       (RBRACK . "]")
       (RBRACE . "}")
       (RPAREN . ")"))
      ("open-paren"
       (LBRACK . "[")
       (LBRACE . "{")
       (LPAREN . "("))
      ("newline"
       (NEWLINE)))
    '(("bol" handler wisent-python-lex-bol)
      ("close-paren" handler wisent-python-lex-close-paren)
      ("open-paren" handler wisent-python-lex-open-paren)
      ("newline" handler wisent-python-lex-newline)
      ("punctuation" multiple t)
      ("punctuation" string t)
      ("symbol" string t)
      ("close-paren" string t)
      ("open-paren" string t)))
   )
  "Tokens.")

(defun wisent-python-default-setup ()
  "Setup buffer for parse."
;;DO NOT EDIT! Generated from wisent-python.wy - 2002-06-21 13:12-0700
  (progn
    (setq semantic-bovinate-toplevel-override 'wisent-bovinate-toplevel
	  semantic-toplevel-bovine-table wisent-python-parser-tables
	  semantic-flex-keywords-obarray wisent-python-keywords
	  wisent-flex-tokens-obarray wisent-python-tokens)
    (setq
     ;; How `semantic-flex' will setup the lexer input stream.
     semantic-flex-depth nil
     ;; python grammar requires BOL tokens to compute indent/dedent
     semantic-flex-enable-bol t
     ;; python grammar requires NEWLINE tokens!
     semantic-flex-enable-newlines t
     ;; Tell `semantic-flex' to handle Python numbers
     semantic-number-expression semantic-python-number-regexp
     ;; Character used to separation a parent/child relationship
     semantic-type-relation-separator-character '(".")
     semantic-command-separation-character ";"
     ;; Init indentation stack
     wisent-python-lexer-indent-stack '(0)
     ;; Init paired delimiter stack
     wisent-python-matching-pair-stack nil

     semantic-flex-python-extensions
     '(("\"\"\"" . semantic-flex-python-triple-quotes))

     semantic-flex-extensions semantic-flex-python-extensions
     ))
  )

(provide 'wisent-python)

;;; wisent-python.el ends here
