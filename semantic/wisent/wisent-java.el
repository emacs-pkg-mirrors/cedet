;;; wisent-java.el --- Java LALR parser for Emacs

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Version: 1.0
;; Keywords: syntax
;; X-RCS: $Id: wisent-java.el,v 1.2 2001/07/21 11:00:55 ponced Exp $

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

;;; History:
;; 

;;; Code:

(require 'wisent)
(require 'semantic)

(defvar wisent-java-lex-istream nil
  "The java lexer stream of Semantic flex tokens.")

(defun wisent-java-expand-nonterminal (token)
  "Expand TOKEN into a list of equivalent nonterminals, or nil.
Handle multiple variable declarations in the same statement that is
tokens of the form:

\(NAME-LIST variable TYPE DEFAULT EXTRA-SPECS DOCSTRING PROPS OVERLAY)

Where NAME-LIST is a list of elements of the form (NAME START . END).
NAME is the variable name.  START and END are respectively the
beginning and end of the region of declaration related to this
variable NAME."
  (if (eq (semantic-token-token token) 'variable)
      (let ((nl (semantic-token-name token)))
        (if (consp nl)
            ;; There are multiple names in the same variable
            ;; declaration.
            (let* ((ty (semantic-token-type                 token))
                   (dv (semantic-token-variable-default     token))
                   (xs (semantic-token-variable-extra-specs token))
                   (ds (semantic-token-docstring            token))
                   (pr (semantic-token-properties           token))
                   (ov (semantic-token-overlay              token))
                   (ov-start  (aref ov 0))
                   (ov-end    (aref ov 1))
                   nelt start end vl)
              ;; Merge in new 'variable tokens each name and other
              ;; values from the initial token.
              (while nl
                (setq nelt  (car nl)
                      nl    (cdr nl)
                      start (if nl (cadr nelt) ov-start)
                      end   (if vl (cddr nelt) ov-end)
                      vl    (cons (list (car nelt)
                                        'variable
                                        ty ; type
                                        dv ; default value
                                        xs ; extra specs
                                        ds ; docstring
                                        pr ; properties
                                        (vector start end))
                                  vl)))
              vl)))))

(defconst wisent-java-grammar
  '(IDENTIFIER
    NULL_LITERAL BOOLEAN_LITERAL STRING_LITERAL NUMBER_LITERAL
    RPAREN LPAREN RBRACK LBRACK RBRACE LBRACE
    COMMA COLON SEMICOLON DOT
    EQ MULT OREQ XOREQ ANDEQ URSHIFTEQ RSHIFTEQ LSHIFTEQ MINUSEQ
    PLUSEQ MODEQ DIVEQ MULTEQ QUESTION OROR ANDAND OR XOR AND NOTEQ
    EQEQ GTEQ LTEQ GT LT URSHIFT RSHIFT LSHIFT MOD DIV NOT COMP MINUS
    PLUS MINUSMINUS PLUSPLUS
    INSTANCEOF NEW FINALLY CATCH TRY THROW RETURN CONTINUE BREAK FOR
    DO WHILE DEFAULT CASE SWITCH ELSE IF INTERFACE SUPER THIS THROWS
    VOID IMPLEMENTS EXTENDS CLASS STRICTFP VOLATILE TRANSIENT
    SYNCHRONIZED NATIVE FINAL ABSTRACT STATIC PRIVATE PROTECTED PUBLIC
    IMPORT PACKAGE DOUBLE FLOAT CHAR LONG INT SHORT BYTE BOOLEAN
    CONST GOTO
    (goal
     (compilation_unit)
     : $1
     )
    (literal
     (NULL_LITERAL)
     : $1
     (BOOLEAN_LITERAL)
     : $1
     (STRING_LITERAL)
     : $1
     (NUMBER_LITERAL)
     : $1
     )
    (type
     (reference_type)
     : $1
     (primitive_type)
     : $1
     )
    (primitive_type
     (BOOLEAN)
     : $1
     (numeric_type)
     : $1
     )
    (numeric_type
     (floating_point_type)
     : $1
     (integral_type)
     : $1
     )
    (integral_type
     (CHAR)
     : $1
     (LONG)
     : $1
     (INT)
     : $1
     (SHORT)
     : $1
     (BYTE)
     : $1
     )
    (floating_point_type
     (DOUBLE)
     : $1
     (FLOAT)
     : $1
     )
    (reference_type
     (array_type)
     : $1
     (class_or_interface_type)
     : $1
     )
    (class_or_interface_type
     (name)
     : $1
     )
    (class_type
     (class_or_interface_type)
     : $1
     )
    (interface_type
     (class_or_interface_type)
     : $1
     )
    (array_type
     (name dims)
     : (concat $1 $2)
     (primitive_type dims)
     : (concat $1 $2)
     )
    (name
     (qualified_name)
     : $1
     (simple_name)
     : $1
     )
    (simple_name
     (IDENTIFIER)
     : $1
     )
    (qualified_name
     (name DOT IDENTIFIER)
     : (concat $1 "." $3)
     )
    (compilation_unit
     (package_declaration_opt import_declarations_opt type_declarations_opt)
     : (nconc $1 $2 $3)
     )
    (package_declaration_opt
     ()
     : nil
     (package_declaration)
     : $1
     )
    (import_declarations_opt
     ()
     : nil
     (import_declarations)
     : (apply #'nconc (nreverse $1))
     )
    (type_declarations_opt
     ()
     : nil
     (type_declarations)
     : (apply #'nconc (nreverse $1))
     )
    (import_declarations
     (import_declarations import_declaration)
     : (cons $2 $1)
     (import_declaration)
     : (list $1)
     )
    (type_declarations
     (type_declarations type_declaration)
     : (cons $2 $1)
     (type_declaration)
     : (list $1)
     )
    (package_declaration
     (PACKAGE name SEMICOLON)
     : (wisent-token $2 'package nil nil)
     (error)
     ;; On error skip token and continue
     : (wisent-skip-token)
     )
    (import_declaration
     (IMPORT name SEMICOLON)
     : (wisent-token $2 'include nil nil)
     (IMPORT name DOT MULT SEMICOLON)
     : (wisent-token (concat $2 $3 $4) 'include nil nil)
     (error)
     ;; On error skip token and loop to import_declarations
     : (wisent-skip-token)
     )
    (type_declaration
     (SEMICOLON)
     : nil
     (interface_declaration)
     : $1
     (class_declaration)
     : $1
     (error)
     ;; On error skip token and loop to type_declarations
     : (wisent-skip-token)
     )
    (modifiers_opt
     ()
     : nil
     (modifiers)
     : (nreverse $1)
     )
    (modifiers
     (modifiers modifier)
     : (cons $2 $1)
     (modifier)
     : (list $1)
     )
    (modifier
     (STRICTFP)
     : $1
     (VOLATILE)
     : $1
     (TRANSIENT)
     : $1
     (SYNCHRONIZED)
     : $1
     (NATIVE)
     : $1
     (FINAL)
     : $1
     (ABSTRACT)
     : $1
     (STATIC)
     : $1
     (PRIVATE)
     : $1
     (PROTECTED)
     : $1
     (PUBLIC)
     : $1
     )
    (class_declaration
     (modifiers_opt CLASS IDENTIFIER superc_opt interfaces_opt class_body)
     : (wisent-token $3 'type $2 $6 (if (or $4 $5) (cons $4 $5))
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1)
                     nil)
     )
    (superc
     (EXTENDS class_type)
     : $2
     )
    (superc_opt
     ()
     : nil
     (superc)
     : $1
     )
    (interfaces
     (IMPLEMENTS interface_type_list)
     : $2
     )
    (interfaces_opt
     ()
     : nil
     (interfaces)
     : (nreverse $1)
     )
    (interface_type_list
     (interface_type_list COMMA interface_type)
     : (cons $3 $1)
     (interface_type)
     : (list $1)
     )
    (class_body
     (LBRACE class_body_declarations_opt RBRACE)
     : $2
     )
    (class_body_declarations_opt
     ()
     : nil
     (class_body_declarations)
     : (apply #'nconc (nreverse $1))
     )
    (class_body_declarations
     (class_body_declarations class_body_declaration)
     : (cons $2 $1)
     (class_body_declaration)
     : (list $1)
     )
    (class_body_declaration
     (block)
     : nil
     (constructor_declaration)
     : $1
     (static_initializer)
     : nil
     (class_member_declaration)
     : $1
     (error)
     ;; On error skip token and loop to class_body_declarations
     : (wisent-skip-token)
     )
    (class_member_declaration
     (interface_declaration)
     : $1
     (class_declaration)
     : $1
     (method_declaration)
     : $1
     (field_declaration)
     : (wisent-java-expand-nonterminal (car $1))
     )
    (field_declaration
     (modifiers_opt type variable_declarators SEMICOLON)
     : (wisent-token $3 'variable $2 nil
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1)
                     nil)
     )
    (variable_declarators
     (variable_declarators COMMA variable_declarator)
     : (cons $3 $1)
     (variable_declarator)
     : (list $1)
     )
    (variable_declarator
     (variable_declarator_id EQ variable_initializer)
     : (cons $1 $region)
     (variable_declarator_id)
     : (cons $1 $region)
     )
    (variable_declarator_id
     (variable_declarator_id LBRACK RBRACK)
     : (concat $1 "[]")
     (IDENTIFIER)
     : $1
     )
    (variable_initializer
     (array_initializer)
     (expression)
     )
    (method_declaration
     (modifiers_opt VOID method_declarator throwsc_opt method_body)
     : (wisent-token (car $3) 'function $2 (cdr $3)
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1
                      'throws $4)
                     nil)
     (modifiers_opt type method_declarator throwsc_opt method_body)
     : (wisent-token (car $3) 'function $2 (cdr $3)
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1
                      'throws $4)
                     nil)
     )
    (method_declarator
     (method_declarator LBRACK RBRACK)
     : (cons (concat (car $1) "[]") (cdr $1))
     (IDENTIFIER LPAREN formal_parameter_list_opt RPAREN)
     : (cons $1 $3)
     )
    (formal_parameter_list_opt
     ()
     : nil
     (formal_parameter_list)
     : (apply #'nconc (nreverse $1))
     )
    (formal_parameter_list
     (formal_parameter_list COMMA formal_parameter)
     : (cons $3 $1)
     (formal_parameter)
     : (list $1)
     )
    (formal_parameter
     (FINAL type variable_declarator_id)
     : (wisent-token $3 'variable $2 nil
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1)
                     nil)
     (type variable_declarator_id)
     : (wisent-token $2 'variable $1 nil
                     nil
                     nil)
     )
    (throwsc_opt
     ()
     : nil
     (throwsc)
     : $1
     )
    (throwsc
     (THROWS class_type_list)
     : (nreverse $2)
     )
    (class_type_list
     (class_type_list COMMA class_type)
     : (cons $3 $1)
     (class_type)
     : (list $1)
     )
    (method_body
     (SEMICOLON)
     (block)
     )
    (static_initializer
     (STATIC block)
     )
    (constructor_declaration
     (modifiers_opt constructor_declarator throwsc_opt constructor_body)
     : (wisent-token (car $2) 'function nil (cdr $2)
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1
                      'throws $3)
                     nil)
     )
    (constructor_declarator
     (simple_name LPAREN formal_parameter_list_opt RPAREN)
     : (cons $1 $3)
     )
    (constructor_body
     (LBRACE RBRACE)
;;; For now dont parse statements inside constructor or method bodies.
;;; This is not necessary to get Semantic tags and improves
;;; performance!  Any token encountered inside {} raises a parse error
;;; and the current block is skept.
;;;          (LBRACE block_statements RBRACE)
;;;          (LBRACE explicit_constructor_invocation RBRACE)
;;;          (LBRACE explicit_constructor_invocation block_statements RBRACE)
     (LBRACE error)
     ;; Skip the block if it contains error
     : (wisent-skip-block)
     )
    (explicit_constructor_invocation
     (primary DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON)
     (primary DOT THIS LPAREN argument_list_opt RPAREN SEMICOLON)
     (SUPER LPAREN argument_list_opt RPAREN SEMICOLON)
     (THIS LPAREN argument_list_opt RPAREN SEMICOLON)
     )
    (interface_declaration
     (modifiers_opt INTERFACE IDENTIFIER extends_interfaces_opt interface_body)
     : (wisent-token $3 'type $2 $5 (if $4 (cons nil $4))
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1)
                     nil)
     )
    (extends_interfaces_opt
     ()
     : nil
     (extends_interfaces)
     : (nreverse $1)
     )
    (extends_interfaces
     (extends_interfaces COMMA interface_type)
     : (cons $3 $1)
     (EXTENDS interface_type)
     : (list $2)
     )
    (interface_body
     (LBRACE interface_member_declarations_opt RBRACE)
     : $2
     )
    (interface_member_declarations_opt
     ()
     : nil
     (interface_member_declarations)
     : (apply #'nconc (nreverse $1))
     )
    (interface_member_declarations
     (interface_member_declarations interface_member_declaration)
     : (cons $2 $1)
     (interface_member_declaration)
     : (list $1)
     )
    (interface_member_declaration
     (interface_declaration)
     : $1
     (class_declaration)
     : $1
     (abstract_method_declaration)
     : $1
     (constant_declaration)
     : $1
     (error)
     ;; On error skip token and loop to interface_member_declarations
     : (wisent-skip-token)
     )
    (constant_declaration
     (field_declaration)
     : $1
     )
    (abstract_method_declaration
     (modifiers_opt VOID method_declarator throwsc_opt SEMICOLON)
     : (wisent-token (car $3) 'function $2 (cdr $3)
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1
                      'throws $4)
                     nil)
     (modifiers_opt type method_declarator throwsc_opt SEMICOLON)
     : (wisent-token (car $3) 'function $2 (cdr $3)
                     (semantic-bovinate-make-assoc-list
                      'typemodifiers $1
                      'throws $4)
                     nil)
     )
    (array_initializer
     (LBRACE RBRACE)
;;; For now dont parse statements inside array initializers.  This is
;;; not necessary to get Semantic tags and improves performance!  Any
;;; token encountered inside {} raises a parse error and the current
;;; block is skept.
;;;          (LBRACE COMMA RBRACE)
;;;          (LBRACE variable_initializers RBRACE)
;;;          (LBRACE variable_initializers COMMA RBRACE)
     (LBRACE error)
     ;; Skip the block if it contains error
     : (wisent-skip-block)
     )
    (variable_initializers
     (variable_initializers COMMA variable_initializer)
     (variable_initializer)
     )
    (block
;;; For now dont parse statements inside blocks.  This is not
;;; necessary to get Semantic tags and improves performance!  Any
;;; token encountered inside {} raises a parse error and the current
;;; block is skept.
;;;             (LBRACE block_statements_opt RBRACE)
        (LBRACE RBRACE) ;; Added to skip the block!
      (LBRACE error)
      ;; Skip the block if it contains error
      : (wisent-skip-block)
      )
;;;     (block_statements_opt
;;;      ()
;;;      (block_statements)
;;;      )
;;;     (block_statements
;;;      (block_statements block_statement)
;;;      (block_statement)
;;;      )
;;;     (block_statement
;;;      (interface_declaration)
;;;      (class_declaration)
;;;      (statement)
;;;      (local_variable_declaration_statement)
;;;      )
;;;     (local_variable_declaration_statement
;;;      (local_variable_declaration SEMICOLON)
;;;      )
;;;     (local_variable_declaration
;;;      (FINAL type variable_declarators)
;;;      (type variable_declarators)
;;;      )
;;;     (statement
;;;      (for_statement)
;;;      (while_statement)
;;;      (if_then_else_statement)
;;;      (if_then_statement)
;;;      (labeled_statement)
;;;      (statement_without_trailing_substatement)
;;;      )
;;;     (statement_no_short_if
;;;      (for_statement_no_short_if)
;;;      (while_statement_no_short_if)
;;;      (if_then_else_statement_no_short_if)
;;;      (labeled_statement_no_short_if)
;;;      (statement_without_trailing_substatement)
;;;      )
;;;     (statement_without_trailing_substatement
;;;      (try_statement)
;;;      (throw_statement)
;;;      (synchronized_statement)
;;;      (return_statement)
;;;      (continue_statement)
;;;      (break_statement)
;;;      (do_statement)
;;;      (switch_statement)
;;;      (expression_statement)
;;;      (empty_statement)
;;;      (block)
;;;      )
;;;     (empty_statement
;;;      (SEMICOLON)
;;;      )
;;;     (labeled_statement
;;;      (IDENTIFIER COLON statement)
;;;      )
;;;     (labeled_statement_no_short_if
;;;      (IDENTIFIER COLON statement_no_short_if)
;;;      )
;;;     (expression_statement
;;;      (statement_expression SEMICOLON)
;;;      )
;;;     (statement_expression
;;;      (class_instance_creation_expression)
;;;      (method_invocation)
;;;      (postdecrement_expression)
;;;      (postincrement_expression)
;;;      (predecrement_expression)
;;;      (preincrement_expression)
;;;      (assignment)
;;;      )
;;;     (if_then_statement
;;;      (IF LPAREN expression RPAREN statement)
;;;      )
;;;     (if_then_else_statement
;;;      (IF LPAREN expression RPAREN statement_no_short_if ELSE statement)
;;;      )
;;;     (if_then_else_statement_no_short_if
;;;      (IF LPAREN expression RPAREN statement_no_short_if ELSE statement_no_short_if)
;;;      )
;;;     (switch_statement
;;;      (SWITCH LPAREN expression RPAREN switch_block)
;;;      )
;;;     (switch_block
;;;      (LBRACE RBRACE)
;;;      (LBRACE switch_labels RBRACE)
;;;      (LBRACE switch_block_statement_groups RBRACE)
;;;      (LBRACE switch_block_statement_groups switch_labels RBRACE)
;;;      (LBRACE error)
;;;      ;; Skip the block if it contains error
;;;      : (wisent-skip-block)
;;;      )
;;;     (switch_block_statement_groups
;;;      (switch_block_statement_groups switch_block_statement_group)
;;;      (switch_block_statement_group)
;;;      )
;;;     (switch_block_statement_group
;;;      (switch_labels block_statements)
;;;      )
;;;     (switch_labels
;;;      (switch_labels switch_label)
;;;      (switch_label)
;;;      )
;;;     (switch_label
;;;      (DEFAULT COLON)
;;;      (CASE constant_expression COLON)
;;;      )
;;;     (while_statement
;;;      (WHILE LPAREN expression RPAREN statement)
;;;      )
;;;     (while_statement_no_short_if
;;;      (WHILE LPAREN expression RPAREN statement_no_short_if)
;;;      )
;;;     (do_statement
;;;      (DO statement WHILE LPAREN expression RPAREN SEMICOLON)
;;;      )
;;;     (for_statement
;;;      (FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON for_update_opt
;;;           RPAREN statement)
;;;      )
;;;     (for_statement_no_short_if
;;;      (FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON for_update_opt
;;;           RPAREN statement_no_short_if)
;;;      )
;;;     (for_init_opt
;;;      ()
;;;      (for_init)
;;;      )
;;;     (for_init
;;;      (local_variable_declaration)
;;;      (statement_expression_list)
;;;      )
;;;     (for_update_opt
;;;      ()
;;;      (for_update)
;;;      )
;;;     (for_update
;;;      (statement_expression_list)
;;;      )
;;;     (statement_expression_list
;;;      (statement_expression_list COMMA statement_expression)
;;;      (statement_expression)
;;;      )
;;;     (identifier_opt
;;;      ()
;;;      (IDENTIFIER)
;;;      )
;;;     (break_statement
;;;      (BREAK identifier_opt SEMICOLON)
;;;      )
;;;     (continue_statement
;;;      (CONTINUE identifier_opt SEMICOLON)
;;;      )
;;;     (return_statement
;;;      (RETURN expression_opt SEMICOLON)
;;;      )
;;;     (throw_statement
;;;      (THROW expression SEMICOLON)
;;;      )
;;;     (synchronized_statement
;;;      (SYNCHRONIZED LPAREN expression RPAREN block)
;;;      )
;;;     (try_statement
;;;      (TRY block catches_opt finallyc)
;;;      (TRY block catches)
;;;      )
;;;     (catches_opt
;;;      ()
;;;      (catches)
;;;      )
;;;     (catches
;;;      (catches catch_clause)
;;;      (catch_clause)
;;;      )
;;;     (catch_clause
;;;      (CATCH LPAREN formal_parameter RPAREN block)
;;;      )
;;;     (finallyc
;;;      (FINALLY block)
;;;      )
    (primary
     (array_creation_expression)
     (primary_no_new_array)
     )
    (primary_no_new_array
     (name DOT THIS)
     (name DOT CLASS)
     (array_type DOT CLASS)
     (VOID DOT CLASS)
     (primitive_type DOT CLASS)
     (array_access)
     (method_invocation)
     (field_access)
     (class_instance_creation_expression)
     (LPAREN expression RPAREN)
     (THIS)
     (literal)
     )
    (class_instance_creation_expression
     (primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN class_body)
     (primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN)
     (NEW class_type LPAREN argument_list_opt RPAREN class_body)
     (NEW class_type LPAREN argument_list_opt RPAREN)
     )
    (argument_list_opt
     ()
     (argument_list)
     )
    (argument_list
     (argument_list COMMA expression)
     (expression)
     )
    (array_creation_expression
;;;          (NEW class_or_interface_type dims array_initializer)
;;;          (NEW primitive_type dims array_initializer)
;;;          (NEW class_or_interface_type dim_exprs dims_opt)
;;;          (NEW primitive_type dim_exprs dims_opt)
     ;; Use the following rules instead of previous ones to avoid
     ;; reduce conflicts between dims and dim_exprs.  These
     ;; conflicts occurs because expression is ignored in dim_expr
     ;; (that is dims and dim_exprs became equivalents).
     (NEW class_or_interface_type dim_exprs)
     (NEW primitive_type dim_exprs)
     )
    (dim_exprs
     (dim_exprs dim_expr)
     (dim_expr)
     )
    (dim_expr
;;; For now dont parse expressions inside array [].
;;; This is not necessary to get Semantic tags and improves
;;; performance!  Any token encountered inside [] raises a parse error
;;; and the current block is skept.
;;;          (LBRACK expression RBRACK)
     (LBRACK RBRACK) ;; Added to skip the block
     (LBRACK error)
     ;; Skip the block if it contains error
     : (wisent-skip-block)
     )
    (dims_opt
     ()
     : ""
     (dims)
     : $1
     )
    (dims
     (dims LBRACK RBRACK)
     : (concat $1 "[]")
     (LBRACK RBRACK)
     : "[]"
     )
    (field_access
     (name DOT SUPER DOT IDENTIFIER)
     (SUPER DOT IDENTIFIER)
     (primary DOT IDENTIFIER)
     )
    (method_invocation
     (name DOT SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
     (SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
     (primary DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
     (name LPAREN argument_list_opt RPAREN)
     )
    (array_access
     (primary_no_new_array LBRACK expression RBRACK)
     (name LBRACK expression RBRACK)
     )
    (postfix_expression
     (postdecrement_expression)
     (postincrement_expression)
     (name)
     (primary)
     )
    (postincrement_expression
     (postfix_expression PLUSPLUS)
     )
    (postdecrement_expression
     (postfix_expression MINUSMINUS)
     )
    (unary_expression
     (unary_expression_not_plus_minus)
     (MINUS unary_expression)
     (PLUS unary_expression)
     (predecrement_expression)
     (preincrement_expression)
     )
    (preincrement_expression
     (PLUSPLUS unary_expression)
     )
    (predecrement_expression
     (MINUSMINUS unary_expression)
     )
    (unary_expression_not_plus_minus
     (cast_expression)
     (NOT unary_expression)
     (COMP unary_expression)
     (postfix_expression)
     )
    (cast_expression
     (LPAREN name dims RPAREN unary_expression_not_plus_minus)
     (LPAREN expression RPAREN unary_expression_not_plus_minus)
     (LPAREN primitive_type dims_opt RPAREN unary_expression)
     )
    (multiplicative_expression
     (multiplicative_expression MOD unary_expression)
     (multiplicative_expression DIV unary_expression)
     (multiplicative_expression MULT unary_expression)
     (unary_expression)
     )
    (additive_expression
     (additive_expression MINUS multiplicative_expression)
     (additive_expression PLUS multiplicative_expression)
     (multiplicative_expression)
     )
    (shift_expression
     (shift_expression URSHIFT additive_expression)
     (shift_expression RSHIFT additive_expression)
     (shift_expression LSHIFT additive_expression)
     (additive_expression)
     )
    (relational_expression
     (relational_expression INSTANCEOF reference_type)
     (relational_expression GTEQ shift_expression)
     (relational_expression LTEQ shift_expression)
     (relational_expression GT shift_expression)
     (relational_expression LT shift_expression)
     (shift_expression)
     )
    (equality_expression
     (equality_expression NOTEQ relational_expression)
     (equality_expression EQEQ relational_expression)
     (relational_expression)
     )
    (and_expression
     (and_expression AND equality_expression)
     (equality_expression)
     )
    (exclusive_or_expression
     (exclusive_or_expression XOR and_expression)
     (and_expression)
     )
    (inclusive_or_expression
     (inclusive_or_expression OR exclusive_or_expression)
     (exclusive_or_expression)
     )
    (conditional_and_expression
     (conditional_and_expression ANDAND inclusive_or_expression)
     (inclusive_or_expression)
     )
    (conditional_or_expression
     (conditional_or_expression OROR conditional_and_expression)
     (conditional_and_expression)
     )
    (conditional_expression
     (conditional_or_expression QUESTION expression COLON conditional_expression)
     (conditional_or_expression)
     )
    (assignment_expression
     (assignment)
     (conditional_expression)
     )
    (assignment
     (left_hand_side assignment_operator assignment_expression)
     )
    (left_hand_side
     (array_access)
     (field_access)
     (name)
     )
    (assignment_operator
     (OREQ)
     (XOREQ)
     (ANDEQ)
     (URSHIFTEQ)
     (RSHIFTEQ)
     (LSHIFTEQ)
     (MINUSEQ)
     (PLUSEQ)
     (MODEQ)
     (DIVEQ)
     (MULTEQ)
     (EQ)
     )
    (expression_opt
     ()
     (expression)
     )
    (expression
     (assignment_expression)
     )
    (constant_expression
     (expression)
     )
    )
  "Wisent LALR(1) grammar for Semantic.
Tweaked for Semantic needs.  That is to avoid full parsing of
unnecessary stuff to improve performance.")

(defconst wisent-java-parser-tables
  (wisent-compile-grammar wisent-java-grammar)
  "Wisent parser tables.")

(defconst wisent-java-parens
  '(
    ("("    . LPAREN)
    (")"    . RPAREN)
    ("{"    . LBRACE)
    ("}"    . RBRACE)
    ("["    . LBRACK)
    ("]"    . RBRACK)
    )
  "Java parenthesis terminals for the lexer.")

(defconst wisent-java-operators
  '(
    ("!"    . NOT)
    ("!="   . NOTEQ)
    ("%"    . MOD)
    ("%="   . MODEQ)
    ("&"    . AND)
    ("&&"   . ANDAND)
    ("&="   . ANDEQ)
    ("*"    . MULT)
    ("*="   . MULTEQ)
    ("+"    . PLUS)
    ("++"   . PLUSPLUS)
    ("+="   . PLUSEQ)
    (","    . COMMA)
    ("-"    . MINUS)
    ("--"   . MINUSMINUS)
    ("-="   . MINUSEQ)
    ("."    . DOT)
    ("/"    . DIV)
    ("/="   . DIVEQ)
    (":"    . COLON)
    (";"    . SEMICOLON)
    ("<"    . LT)
    ("<<"   . LSHIFT)
    ("<<="  . LSHIFTEQ)
    ("<="   . LTEQ)
    ("="    . EQ)
    ("=="   . EQEQ)
    (">"    . GT)
    (">="   . GTEQ)
    (">>"   . RSHIFT)
    (">>="  . RSHIFTEQ)
    (">>>"  . URSHIFT)
    (">>>=" . URSHIFTEQ)
    ("?"    . QUESTION)
    ("^"    . XOR)
    ("^="   . XOREQ)
    ("|"    . OR)
    ("|="   . OREQ)
    ("||"   . OROR)
    ("~"    . COMP)
    )
  "Java operator terminals for the lexer.")

(defconst wisent-java-number-regexps
  '(
    "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
    "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
    "\\<[0-9]+[.][fFdD]\\>"
    "\\<[0-9]+[.]"
    "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
    "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
    "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
    "\\<[0-9]+[lLfFdD]?\\>"
    )
  "Lexer regexps to match Java number terminals.")

;;;;
;;;; The Java Lexer
;;;;

(defun wisent-java-lex ()
  "Return the next Java lexical token in input.
When the end of input is reached return (`wisent-eoi-term').  Each
lexical token has the form (TERMINAL VALUE START . END) where TERMINAL
is the terminal symbol for this token, VALUE is the string value of
the token, START and END are respectively the beginning and end
positions of the token in input."
  (if (null wisent-java-lex-istream)
      ;; End of input
      (list wisent-eoi-term)
    (let* ((is wisent-java-lex-istream)
           (tk (car is))
           (ft (car tk))
           lex x y is2 rl)
      (cond
       
       ;; Keyword
       ;; -------
       ((setq x (semantic-flex-text tk)
              y (semantic-flex-keyword-p x))
        (setq lex (cons y (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Number
       ;; -------
       ((save-excursion
          (setq rl wisent-java-number-regexps
                x (semantic-flex-start tk))
          (goto-char x)
          (while (and rl (not (looking-at (car rl))))
            (setq rl (cdr rl)))
          (if (null rl)
              nil
            (setq y (match-end 0))
            ;; Adjust input stream.
            (while (<= (semantic-flex-end tk) y)
              (setq is (cdr is)
                    tk (car is)))
            (setq lex (cons 'NUMBER_LITERAL
                            (cons (buffer-substring-no-properties x y)
                                  (cons x y)))))))
       
       ;; Punctuation
       ;; -----------
       ((eq ft 'punctuation)
        (setq x   (semantic-flex-text tk)
              is2 (cdr is))
        ;; Concat all successive punctuations
        (while (and is2 (eq (caar is2) 'punctuation))
          (setq x   (concat x (semantic-flex-text (car is2)))
                is2 (cdr is2)))
        ;; Starting with the longest punctuation string search if it
        ;; matches a Java operator.
        (while (and (> (length x) 0)
                    (not (setq y (assoc x wisent-java-operators))))
          (setq x (substring x 0 -1)))
        (or y (error "Invalid punctuation %s in input" x))
        ;; Here y is the operator (term-value . term-symbol).  Now
        ;; build the lex token.
        (setq lex (list (semantic-flex-start tk)) ;; (start . nil)
              x   (length x)
              ;; Adjust input stream.
              is  (nthcdr x is))
        (setcdr lex (+ (car lex) x)) ;; (start . end)
        ;; Finalize lex token: (term-symbol term-value start . end)
        (setq lex (cons (cdr y) (cons (car y) lex))))
       
       ;; Parens
       ;; ------
       ((memq ft '(open-paren close-paren))
        (or (setq x (semantic-flex-text tk)
                  y (assoc x wisent-java-parens))
            (error "Invalid %s %s in input" ft x))
        (setq lex (cons (cdr y) (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; String
       ;; ------
       ((eq ft 'string)
        (setq lex (cons 'STRING_LITERAL
                        (cons (semantic-flex-text tk) (cdr tk)))
              is  (cdr is)))
       
       ;; Symbol
       ;; ------
       ((eq ft 'symbol)
        (setq x  (semantic-flex-text tk)
              y  (cond ((string-equal x "null")
                        'NULL_LITERAL)
                       ((member x '("true" "false"))
                        'BOOLEAN_LITERAL)
                       (t
                        'IDENTIFIER))
              lex (cons y (cons x (cdr tk)))
              is  (cdr is)))
       
       ;; Unhandled
       ;; ---------
       (t
        (error "Invalid input form %s" ft)))
      
      (setq wisent-java-lex-istream is)
      (if is
          (if (eq semantic-bovination-working-type 'percent)
              (working-status
               (floor
                (* 100.0 (/ (float (semantic-flex-start (car is)))
                            (float (point-max))))))
            (working-dynamic-status)))
      lex)))

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-java-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Semantic integration of the Java LALR parser
;;;;

(defun wisent-java-bovinate-toplevel (&optional checkcache)
  "Semantic alternate Java LALR(1) parser.
The optional argument CHECKCACHE is ignored."
  (let ((gc-cons-threshold 10000000)
        (bname (format "%s [LALR]" (buffer-name)))
        cache semantic-flex-depth)
    (working-status-forms bname "done"
      (setq wisent-java-lex-istream (semantic-flex-buffer)
            cache (wisent-parse wisent-java-parser-tables
                                #'wisent-java-lex
                                #'ignore ;; Don't report syntax errors
                                ))
      (working-status t))
    (semantic-overlay-list cache)
    cache))

(defun wisent-java-mode-hook ()
  "`java-mode' hook to setup the Semantic alternate LALR(1) parser."
  (setq
   ;; Override the default parser
   semantic-bovinate-toplevel-override #'wisent-java-bovinate-toplevel
   ;; Handling of multiple variable declarations/statement
   ;; semantic-expand-nonterminal #'wisent-java-expand-nonterminal
   ))

(add-hook 'java-mode-hook #'wisent-java-mode-hook t)

;;;;
;;;; Useful to debug the parser
;;;;

(defun wisent-java-parse ()
  "Parse the current buffer."
  (interactive)
  (let ((gc-cons-threshold 10000000)
        semantic-flex-depth ast clock)
    (garbage-collect)
    (message "Parsing buffer...")
    (setq clock (float-time)
          wisent-java-lex-istream
          (semantic-flex (point-min) (point-max)))
    (setq ast   (wisent-parse wisent-java-parser-tables
                              #'wisent-java-lex
                              #'wisent-java-parse-error)
          clock (- (float-time) clock))
    (message "Generating AST...")
    (with-current-buffer (get-buffer-create "*wisent-java-parse*")
      (let ((standard-output (current-buffer)))
        (pp ast))
      (pop-to-buffer (current-buffer)))
    (apply #'message
           `("Buffer parsed in %gS (%s error%s)"
             ,clock ,@(cond ((= wisent-nerrs 0) '("No" ""))
                            ((= wisent-nerrs 1) '("One" ""))
                            (t (list wisent-nerrs "s")))))))

;;;;
;;;; Useful to debug the lexer
;;;;

(defun wisent-java-lex-buffer ()
  "Scan the current buffer and show the stream of lexical tokens."
  (interactive)
  (let ((terminals (aref wisent-java-parser-tables 3))
        semantic-flex-depth lex tok)
    (setq wisent-java-lex-istream (semantic-flex-buffer))
    (while wisent-java-lex-istream
      (setq lex (wisent-java-lex)
            tok (wisent-translate (car lex) terminals))
      (with-current-buffer
          (get-buffer-create "*wisent-java-lex-buffer*")
        (insert
         (format "(%s[%s]\t\t\t. %S)\n"
                 (car lex) tok (cdr lex)))))
    (pop-to-buffer "*wisent-java-lex-buffer*")))

(provide 'wisent-java)

;;; wisent-java.el ends here
