;;; wisent-awk.bnf --- GNU AWK Grammar

;; Copyright (C) 2002 David Ponce
;; Copyright 2001 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Jan 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-awk.el,v 1.1 2002/02/01 23:56:13 ponced Exp $

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
;; Just a good torture test for Wisent ;-)
;; The grammar is generated from the BNF file wisent-awk.bnf.

;;; History:
;; 

;;; Code:
(require 'wisent-bovine)

(defconst wisent-awk-parser-tables
  (eval-when-compile
  (wisent-compile-grammar
   '((FUNC_CALL NAME REGEXP ERROR YNUMBER YSTRING RELOP APPEND_OP ASSIGNOP MATCHOP NEWLINE CONCAT_OP TWOWAYIO UNARY LEX_BEGIN LEX_END LEX_IF LEX_ELSE LEX_RETURN LEX_DELETE LEX_WHILE LEX_DO LEX_FOR LEX_BREAK LEX_CONTINUE LEX_PRINT LEX_PRINTF LEX_NEXT LEX_EXIT LEX_FUNCTION LEX_GETLINE LEX_NEXTFILE LEX_IN LEX_AND LEX_OR INCREMENT DECREMENT LEX_BUILTIN LEX_LENGTH LPAR RPAR LBRAK RBRAK LBRAC RBRAC QUEST COLON COMMA SEMI LT GT PLUS MINUS MULT DIV MOD COMPL NOT PIPE DOLAR)
     ((right ASSIGNOP)
      (right QUEST COLON)
      (left LEX_OR)
      (left LEX_AND)
      (left LEX_GETLINE)
      (nonassoc LEX_IN)
      (left FUNC_CALL LEX_BUILTIN LEX_LENGTH)
      (nonassoc COMMA)
      (nonassoc MATCHOP)
      (nonassoc RELOP LT GT PIPE APPEND_OP TWOWAYIO)
      (left CONCAT_OP)
      (left YSTRING YNUMBER)
      (left PLUS MINUS)
      (left MULT DIV MOD)
      (right NOT UNARY)
      (right COMPL)
      (left INCREMENT DECREMENT)
      (left DOLAR)
      (left LPAR RPAR))
     (start_
      ((opt_nls program opt_nls)))
     (program
      ((rule))
      ((program rule))
      ((error))
      ((program error))
      (nil))
     (rule
      ((LEX_BEGIN action))
      ((LEX_END action))
      ((LEX_BEGIN statement_term))
      ((LEX_END statement_term))
      ((pattern action))
      ((action))
      ((pattern statement_term))
      ((function_prologue function_body)))
     (func_name
      ((NAME))
      ((FUNC_CALL))
      ((lex_builtin)))
     (lex_builtin
      ((LEX_BUILTIN))
      ((LEX_LENGTH)))
     (function_prologue
      ((LEX_FUNCTION func_name LPAR opt_param_list r_paren opt_nls)))
     (function_body
      ((l_brace statements r_brace opt_semi opt_nls))
      ((l_brace r_brace opt_semi opt_nls)))
     (pattern
      ((exp))
      ((exp COMMA exp)))
     (regexp
      ((DIV REGEXP DIV)))
     (action
      ((l_brace statements r_brace opt_semi opt_nls))
      ((l_brace r_brace opt_semi opt_nls)))
     (statements
      ((statement))
      ((statements statement))
      ((error))
      ((statements error)))
     (statement_term
      ((nls))
      ((semi opt_nls)))
     (statement
      ((semi opt_nls))
      ((l_brace r_brace))
      ((l_brace statements r_brace))
      ((if_statement))
      ((LEX_WHILE LPAR exp r_paren opt_nls statement))
      ((LEX_DO opt_nls statement LEX_WHILE LPAR exp r_paren opt_nls))
      ((LEX_FOR LPAR NAME LEX_IN NAME r_paren opt_nls statement))
      ((LEX_FOR LPAR opt_exp semi opt_nls exp semi opt_nls opt_exp r_paren opt_nls statement))
      ((LEX_FOR LPAR opt_exp semi opt_nls semi opt_nls opt_exp r_paren opt_nls statement))
      ((LEX_BREAK statement_term))
      ((LEX_CONTINUE statement_term))
      ((print LPAR expression_list r_paren output_redir statement_term))
      ((print opt_rexpression_list output_redir statement_term))
      ((LEX_NEXT statement_term))
      ((LEX_NEXTFILE statement_term))
      ((LEX_EXIT opt_exp statement_term))
      ((LEX_RETURN opt_exp statement_term))
      ((LEX_DELETE NAME LBRAK expression_list RBRAK statement_term))
      ((LEX_DELETE NAME statement_term))
      ((exp statement_term)))
     (print
      ((LEX_PRINT))
      ((LEX_PRINTF)))
     (if_statement
      ((LEX_IF LPAR exp r_paren opt_nls statement))
      ((LEX_IF LPAR exp r_paren opt_nls statement LEX_ELSE opt_nls statement)))
     (nls
      ((NEWLINE))
      ((nls NEWLINE)))
     (opt_nls
      (nil)
      ((nls)))
     (input_redir
      (nil)
      ((LT simp_exp)))
     (output_redir
      (nil)
      ((GT exp))
      ((APPEND_OP exp))
      ((PIPE exp))
      ((TWOWAYIO exp)))
     (opt_param_list
      (nil)
      ((param_list)))
     (param_list
      ((NAME))
      ((param_list comma NAME))
      ((error))
      ((param_list error))
      ((param_list comma error)))
     (opt_exp
      (nil)
      ((exp)))
     (opt_rexpression_list
      (nil)
      ((rexpression_list)))
     (rexpression_list
      ((rexp))
      ((rexpression_list comma rexp))
      ((error))
      ((rexpression_list error))
      ((rexpression_list error rexp))
      ((rexpression_list comma error)))
     (opt_expression_list
      (nil)
      ((expression_list)))
     (expression_list
      ((exp))
      ((expression_list comma exp))
      ((error))
      ((expression_list error))
      ((expression_list error exp))
      ((expression_list comma error)))
     (exp
      ((variable ASSIGNOP exp))
      ((LPAR expression_list r_paren LEX_IN NAME))
      ((exp PIPE LEX_GETLINE opt_variable))
      ((exp TWOWAYIO LEX_GETLINE opt_variable))
      ((LEX_GETLINE opt_variable input_redir))
      ((exp LEX_AND exp))
      ((exp LEX_OR exp))
      ((exp MATCHOP exp))
      ((regexp))
      ((NOT regexp)
       [UNARY])
      ((exp LEX_IN NAME))
      ((exp RELOP exp))
      ((exp LT exp))
      ((exp GT exp))
      ((exp QUEST exp COLON exp))
      ((simp_exp))
      ((exp simp_exp)
       [CONCAT_OP]))
     (rexp
      ((variable ASSIGNOP rexp))
      ((rexp LEX_AND rexp))
      ((rexp LEX_OR rexp))
      ((LEX_GETLINE opt_variable input_redir))
      ((regexp))
      ((NOT regexp)
       [UNARY])
      ((rexp MATCHOP rexp))
      ((rexp LEX_IN NAME))
      ((rexp RELOP rexp))
      ((rexp QUEST rexp COLON rexp))
      ((simp_exp))
      ((rexp simp_exp)
       [CONCAT_OP]))
     (simp_exp
      ((non_post_simp_exp))
      ((simp_exp COMPL simp_exp))
      ((simp_exp MULT simp_exp))
      ((simp_exp DIV simp_exp))
      ((simp_exp MOD simp_exp))
      ((simp_exp PLUS simp_exp))
      ((simp_exp MINUS simp_exp))
      ((variable INCREMENT))
      ((variable DECREMENT)))
     (non_post_simp_exp
      ((NOT simp_exp)
       [UNARY])
      ((LPAR exp r_paren))
      ((LEX_BUILTIN LPAR opt_expression_list r_paren))
      ((LEX_LENGTH LPAR opt_expression_list r_paren))
      ((LEX_LENGTH))
      ((FUNC_CALL LPAR opt_expression_list r_paren))
      ((variable))
      ((INCREMENT variable))
      ((DECREMENT variable))
      ((YNUMBER))
      ((YSTRING))
      ((MINUS simp_exp)
       [UNARY])
      ((PLUS simp_exp)
       [UNARY]))
     (opt_variable
      (nil)
      ((variable)))
     (variable
      ((NAME))
      ((NAME LBRAK expression_list RBRAK))
      ((DOLAR non_post_simp_exp)))
     (l_brace
      ((LBRAC opt_nls)))
     (r_brace
      ((RBRAC opt_nls)))
     (r_paren
      ((RPAR)))
     (opt_semi
      (nil)
      ((semi)))
     (semi
      ((SEMI)))
     (comma
      ((COMMA opt_nls))))
   'nil))
"Parser tables.")

(defconst wisent-awk-keywords
  (semantic-flex-make-keyword-table 
   `( ("BEGIN" . LEX_BEGIN)
      ("END" . LEX_END)
      ("IF" . LEX_IF)
      ("ELSE" . LEX_ELSE)
      ("RETURN" . LEX_RETURN)
      ("DELETE" . LEX_DELETE)
      ("WHILE" . LEX_WHILE)
      ("DO" . LEX_DO)
      ("FOR" . LEX_FOR)
      ("BREAK" . LEX_BREAK)
      ("CONTINUE" . LEX_CONTINUE)
      ("PRINT" . LEX_PRINT)
      ("PRINTF" . LEX_PRINTF)
      ("NEXT" . LEX_NEXT)
      ("EXIT" . LEX_EXIT)
      ("FUNCTION" . LEX_FUNCTION)
      ("GETLINE" . LEX_GETLINE)
      ("NEXTFILE" . LEX_NEXTFILE)
      ("IN" . LEX_IN)
      ("AND" . LEX_AND)
      ("OR" . LEX_OR)
      ("INCREMENT" . INCREMENT)
      ("DECREMENT" . DECREMENT)
      ("BUILTIN" . LEX_BUILTIN)
      ("LENGTH" . LEX_LENGTH)
      )
   '(
     ))
  "Keywords.")

(defconst wisent-awk-tokens
  '((oper
     (DOLAR . "$")
     (PIPE . "|")
     (NOT . "!")
     (COMPL . "^")
     (MOD . "%")
     (DIV . "/")
     (MULT . "*")
     (MINUS . "-")
     (PLUS . "+")
     (GT . ">")
     (LT . "<")
     (SEMI . ";")
     (COMMA . ",")
     (COLON . ":")
     (QUEST . "?"))
    (parens
     (RBRAC . "}")
     (LBRAC . "{")
     (RBRAK . "]")
     (LBRAK . "[")
     (RPAR . ")")
     (LPAR . "("))
    (dummy
     (UNARY . "")
     (TWOWAYIO . ""))
    (literal
     (CONCAT_OP . "")
     (NEWLINE . "")
     (MATCHOP . "")
     (ASSIGNOP . "")
     (APPEND_OP . "")
     (RELOP . "")
     (YSTRING . "")
     (YNUMBER . "")
     (ERROR . "")
     (REGEXP . "")
     (NAME . "")
     (FUNC_CALL . "")))
  "Tokens.")

(defun wisent-awk-default-setup ()
  "Setup."
  ;; Code generated from awk.bnf
  (setq semantic-toplevel-bovine-table wisent-awk-parser-tables
        semantic-toplevel-bovine-table-source "wisent-awk.bnf")
  (setq semantic-flex-keywords-obarray wisent-awk-keywords)
 
 ;; End code generated from awk.bnf
  )

(provide 'wisent-awk)

;;; wisent-awk.el ends here
