;;; wisent-java.el --- Java LALR parser for Emacs

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Version: 1.0
;; Keywords: syntax
;; X-RCS: $Id: wisent-java.el,v 1.7 2001/08/16 15:13:42 ponced Exp $

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
(require 'semantic-java)
(eval-when-compile
  (require 'document))

;;;;
;;;; These two functions should be moved to semantic.el
;;;;

(defsubst semantic-flex-token-value (table category &optional key)
  "Return %token values from the token table TABLE.
CATEGORY is a symbol identifying a token category.  If the symbol KEY
is specified the function returns the particular value of this token.
Otherwise the function returns the alist of (KEY . VALUE) for this
category.  See also the function `semantic-bnf-token-table'."
  (let ((cat-alist (cdr (assq category table))))
    (if key
        (cdr (assq key cat-alist))
      cat-alist)))

(defsubst semantic-flex-token-key (table category value)
  "Search for a %token symbol in the token table TABLE.
CATEGORY is a symbol identifying a token category.  VALUE is the value
of the token to search for.  If not found return nil.  See also the
function `semantic-bnf-token-table'."
  (car (rassoc value (cdr (assq category table)))))

;;;;
;;;; Global stuff
;;;;

(defvar wisent-java-lex-istream nil
  "The java lexer stream of Semantic flex tokens.")

(eval-and-compile ;; Definitions needed at compilation time

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
  
  )

;; Do not change here the value of the following constant!  It is
;; automatically generated from the BNF file.
(defconst wisent-java-parser-tables
  (eval-when-compile
    (wisent-compile-grammar
      `(LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ URSHIFT URSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP NULL_LITERAL BOOLEAN_LITERAL STRING_LITERAL NUMBER_LITERAL IDENTIFIER ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE _AUTHOR _VERSION _PARAM _RETURN _EXCEPTION _THROWS _SEE _SINCE _SERIAL _SERIALDATA _SERIALFIELD _DEPRECATED
        (goal
         (compilation_unit)
         :
         (identity $1))
        (literal
         (NULL_LITERAL)
         :
         (identity $1)
         (BOOLEAN_LITERAL)
         :
         (identity $1)
         (STRING_LITERAL)
         :
         (identity $1)
         (NUMBER_LITERAL)
         :
         (identity $1))
        (type
         (reference_type)
         :
         (identity $1)
         (primitive_type)
         :
         (identity $1))
        (primitive_type
         (BOOLEAN)
         :
         (identity $1)
         (numeric_type)
         :
         (identity $1))
        (numeric_type
         (floating_point_type)
         :
         (identity $1)
         (integral_type)
         :
         (identity $1))
        (integral_type
         (CHAR)
         :
         (identity $1)
         (LONG)
         :
         (identity $1)
         (INT)
         :
         (identity $1)
         (SHORT)
         :
         (identity $1)
         (BYTE)
         :
         (identity $1))
        (floating_point_type
         (DOUBLE)
         :
         (identity $1)
         (FLOAT)
         :
         (identity $1))
        (reference_type
         (array_type)
         :
         (identity $1)
         (class_or_interface_type)
         :
         (identity $1))
        (class_or_interface_type
         (name)
         :
         (identity $1))
        (class_type
         (class_or_interface_type)
         :
         (identity $1))
        (interface_type
         (class_or_interface_type)
         :
         (identity $1))
        (array_type
         (name dims)
         :
         (concat $1 $2)
         (primitive_type dims)
         :
         (concat $1 $2))
        (name
         (qualified_name)
         :
         (identity $1)
         (simple_name)
         :
         (identity $1))
        (simple_name
         (IDENTIFIER)
         :
         (identity $1))
        (qualified_name
         (name DOT IDENTIFIER)
         :
         (concat $1 "." $3))
        (compilation_unit
         (package_declaration_opt import_declarations_opt type_declarations_opt)
         :
         (nconc $1 $2 $3))
        (package_declaration_opt nil : nil
                                 (package_declaration)
                                 :
                                 (identity $1))
        (import_declarations_opt nil : nil
                                 (import_declarations)
                                 :
                                 (apply 'nconc
                                        (nreverse $1)))
        (type_declarations_opt nil : nil
                               (type_declarations)
                               :
                               (apply 'nconc
                                      (nreverse $1)))
        (import_declarations
         (import_declarations import_declaration)
         :
         (cons $2 $1)
         (import_declaration)
         :
         (list $1))
        (type_declarations
         (type_declarations type_declaration)
         :
         (cons $2 $1)
         (type_declaration)
         :
         (list $1))
        (package_declaration
         (PACKAGE name SEMICOLON)
         :
         (wisent-token $2 'package nil nil)
         (error)
         :
         (wisent-skip-token))
        (import_declaration
         (IMPORT name SEMICOLON)
         :
         (wisent-token $2 'include nil nil)
         (IMPORT name DOT MULT SEMICOLON)
         :
         (wisent-token
          (concat $2 $3 $4)
          'include nil nil)
         (error)
         :
         (wisent-skip-token))
        (type_declaration
         (SEMICOLON)
         : nil
         (interface_declaration)
         :
         (identity $1)
         (class_declaration)
         :
         (identity $1)
         (error)
         :
         (wisent-skip-token))
        (modifiers_opt nil : nil
                       (modifiers)
                       :
                       (nreverse $1))
        (modifiers
         (modifiers modifier)
         :
         (cons $2 $1)
         (modifier)
         :
         (list $1))
        (modifier
         (STRICTFP)
         :
         (identity $1)
         (VOLATILE)
         :
         (identity $1)
         (TRANSIENT)
         :
         (identity $1)
         (SYNCHRONIZED)
         :
         (identity $1)
         (NATIVE)
         :
         (identity $1)
         (FINAL)
         :
         (identity $1)
         (ABSTRACT)
         :
         (identity $1)
         (STATIC)
         :
         (identity $1)
         (PRIVATE)
         :
         (identity $1)
         (PROTECTED)
         :
         (identity $1)
         (PUBLIC)
         :
         (identity $1))
        (class_declaration
         (modifiers_opt CLASS IDENTIFIER superc_opt interfaces_opt class_body)
         :
         (wisent-token $3 'type $2 $6
                       (if
                           (or $4 $5)
                           (cons $4 $5))
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil))
        (superc
         (EXTENDS class_type)
         :
         (identity $2))
        (superc_opt nil : nil
                    (superc)
                    :
                    (identity $1))
        (interfaces
         (IMPLEMENTS interface_type_list)
         :
         (identity $2))
        (interfaces_opt nil : nil
                        (interfaces)
                        :
                        (nreverse $1))
        (interface_type_list
         (interface_type_list COMMA interface_type)
         :
         (cons $3 $1)
         (interface_type)
         :
         (list $1))
        (class_body
         (LBRACE class_body_declarations_opt RBRACE)
         :
         (identity $2))
        (class_body_declarations_opt nil : nil
                                     (class_body_declarations)
                                     :
                                     (apply 'nconc
                                            (nreverse $1)))
        (class_body_declarations
         (class_body_declarations class_body_declaration)
         :
         (cons $2 $1)
         (class_body_declaration)
         :
         (list $1))
        (class_body_declaration
         (block)
         : nil
         (constructor_declaration)
         :
         (identity $1)
         (static_initializer)
         : nil
         (class_member_declaration)
         :
         (identity $1)
         (error)
         :
         (wisent-skip-token))
        (class_member_declaration
         (interface_declaration)
         :
         (identity $1)
         (class_declaration)
         :
         (identity $1)
         (method_declaration)
         :
         (identity $1)
         (field_declaration)
         :
         (wisent-java-expand-nonterminal
          (car $1)))
        (field_declaration
         (modifiers_opt type variable_declarators SEMICOLON)
         :
         (wisent-token $3 'variable $2 nil
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil))
        (variable_declarators
         (variable_declarators COMMA variable_declarator)
         :
         (cons $3 $1)
         (variable_declarator)
         :
         (list $1))
        (variable_declarator
         (variable_declarator_id EQ variable_initializer)
         :
         (cons $1 $region)
         (variable_declarator_id)
         :
         (cons $1 $region))
        (variable_declarator_id
         (variable_declarator_id LBRACK RBRACK)
         :
         (concat $1 "[]")
         (IDENTIFIER)
         :
         (identity $1))
        (variable_initializer
         (array_initializer)
         (expression))
        (method_declaration
         (modifiers_opt VOID method_declarator throwsc_opt method_body)
         :
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil)
         (modifiers_opt type method_declarator throwsc_opt method_body)
         :
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil))
        (method_declarator
         (method_declarator LBRACK RBRACK)
         :
         (cons
          (concat
           (car $1)
           "[]")
          (cdr $1))
         (IDENTIFIER LPAREN formal_parameter_list_opt RPAREN)
         :
         (cons $1 $3))
        (formal_parameter_list_opt nil : nil
                                   (formal_parameter_list)
                                   :
                                   (apply 'nconc
                                          (nreverse $1)))
        (formal_parameter_list
         (formal_parameter_list COMMA formal_parameter)
         :
         (cons $3 $1)
         (formal_parameter)
         :
         (list $1))
        (formal_parameter
         (FINAL type variable_declarator_id)
         :
         (wisent-token $3 'variable $2 nil
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil)
         (type variable_declarator_id)
         :
         (wisent-token $2 'variable $1 nil nil nil))
        (throwsc_opt nil : nil
                     (throwsc)
                     :
                     (identity $1))
        (throwsc
         (THROWS class_type_list)
         :
         (nreverse $2))
        (class_type_list
         (class_type_list COMMA class_type)
         :
         (cons $3 $1)
         (class_type)
         :
         (list $1))
        (method_body
         (SEMICOLON)
         (block))
        (static_initializer
         (STATIC block))
        (constructor_declaration
         (modifiers_opt constructor_declarator throwsc_opt constructor_body)
         :
         (wisent-token
          (car $2)
          'function nil
          (cdr $2)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $3)
          nil))
        (constructor_declarator
         (simple_name LPAREN formal_parameter_list_opt RPAREN)
         :
         (cons $1 $3))
        (constructor_body
         (LBRACE RBRACE)
         (LBRACE error)
         :
         (wisent-skip-block))
        (explicit_constructor_invocation
         (primary DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON)
         (primary DOT THIS LPAREN argument_list_opt RPAREN SEMICOLON)
         (SUPER LPAREN argument_list_opt RPAREN SEMICOLON)
         (THIS LPAREN argument_list_opt RPAREN SEMICOLON))
        (interface_declaration
         (modifiers_opt INTERFACE IDENTIFIER extends_interfaces_opt interface_body)
         :
         (wisent-token $3 'type $2 $5
                       (if $4
                           (cons nil $4))
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil))
        (extends_interfaces_opt nil : nil
                                (extends_interfaces)
                                :
                                (nreverse $1))
        (extends_interfaces
         (extends_interfaces COMMA interface_type)
         :
         (cons $3 $1)
         (EXTENDS interface_type)
         :
         (list $2))
        (interface_body
         (LBRACE interface_member_declarations_opt RBRACE)
         :
         (identity $2))
        (interface_member_declarations_opt nil : nil
                                           (interface_member_declarations)
                                           :
                                           (apply 'nconc
                                                  (nreverse $1)))
        (interface_member_declarations
         (interface_member_declarations interface_member_declaration)
         :
         (cons $2 $1)
         (interface_member_declaration)
         :
         (list $1))
        (interface_member_declaration
         (interface_declaration)
         :
         (identity $1)
         (class_declaration)
         :
         (identity $1)
         (abstract_method_declaration)
         :
         (identity $1)
         (constant_declaration)
         :
         (identity $1)
         (error)
         :
         (wisent-skip-token))
        (constant_declaration
         (field_declaration)
         :
         (identity $1))
        (abstract_method_declaration
         (modifiers_opt VOID method_declarator throwsc_opt SEMICOLON)
         :
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil)
         (modifiers_opt type method_declarator throwsc_opt SEMICOLON)
         :
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil))
        (array_initializer
         (LBRACE RBRACE)
         (LBRACE error)
         :
         (wisent-skip-block))
        (variable_initializers
         (variable_initializers COMMA variable_initializer)
         (variable_initializer))
        (block
            (LBRACE RBRACE)
          (LBRACE error)
          :
          (wisent-skip-block))
        (primary
         (array_creation_expression)
         (primary_no_new_array))
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
         (literal))
        (class_instance_creation_expression
         (primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN class_body)
         (primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN)
         (NEW class_type LPAREN argument_list_opt RPAREN class_body)
         (NEW class_type LPAREN argument_list_opt RPAREN))
        (argument_list_opt nil
                           (argument_list))
        (argument_list
         (argument_list COMMA expression)
         (expression))
        (array_creation_expression
         (NEW class_or_interface_type dim_exprs)
         (NEW primitive_type dim_exprs))
        (dim_exprs
         (dim_exprs dim_expr)
         (dim_expr))
        (dim_expr
         (LBRACK RBRACK)
         (LBRACK error)
         :
         (wisent-skip-block))
        (dims_opt nil :
                  (identity "")
                  (dims)
                  :
                  (identity $1))
        (dims
         (dims LBRACK RBRACK)
         :
         (concat $1 "[]")
         (LBRACK RBRACK)
         :
         (identity "[]"))
        (field_access
         (name DOT SUPER DOT IDENTIFIER)
         (SUPER DOT IDENTIFIER)
         (primary DOT IDENTIFIER))
        (method_invocation
         (name DOT SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
         (SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
         (primary DOT IDENTIFIER LPAREN argument_list_opt RPAREN)
         (name LPAREN argument_list_opt RPAREN))
        (array_access
         (primary_no_new_array LBRACK expression RBRACK)
         (name LBRACK expression RBRACK))
        (postfix_expression
         (postdecrement_expression)
         (postincrement_expression)
         (name)
         (primary))
        (postincrement_expression
         (postfix_expression PLUSPLUS))
        (postdecrement_expression
         (postfix_expression MINUSMINUS))
        (unary_expression
         (unary_expression_not_plus_minus)
         (MINUS unary_expression)
         (PLUS unary_expression)
         (predecrement_expression)
         (preincrement_expression))
        (preincrement_expression
         (PLUSPLUS unary_expression))
        (predecrement_expression
         (MINUSMINUS unary_expression))
        (unary_expression_not_plus_minus
         (cast_expression)
         (NOT unary_expression)
         (COMP unary_expression)
         (postfix_expression))
        (cast_expression
         (LPAREN name dims RPAREN unary_expression_not_plus_minus)
         (LPAREN expression RPAREN unary_expression_not_plus_minus)
         (LPAREN primitive_type dims_opt RPAREN unary_expression))
        (multiplicative_expression
         (multiplicative_expression MOD unary_expression)
         (multiplicative_expression DIV unary_expression)
         (multiplicative_expression MULT unary_expression)
         (unary_expression))
        (additive_expression
         (additive_expression MINUS multiplicative_expression)
         (additive_expression PLUS multiplicative_expression)
         (multiplicative_expression))
        (shift_expression
         (shift_expression URSHIFT additive_expression)
         (shift_expression RSHIFT additive_expression)
         (shift_expression LSHIFT additive_expression)
         (additive_expression))
        (relational_expression
         (relational_expression INSTANCEOF reference_type)
         (relational_expression GTEQ shift_expression)
         (relational_expression LTEQ shift_expression)
         (relational_expression GT shift_expression)
         (relational_expression LT shift_expression)
         (shift_expression))
        (equality_expression
         (equality_expression NOTEQ relational_expression)
         (equality_expression EQEQ relational_expression)
         (relational_expression))
        (and_expression
         (and_expression AND equality_expression)
         (equality_expression))
        (exclusive_or_expression
         (exclusive_or_expression XOR and_expression)
         (and_expression))
        (inclusive_or_expression
         (inclusive_or_expression OR exclusive_or_expression)
         (exclusive_or_expression))
        (conditional_and_expression
         (conditional_and_expression ANDAND inclusive_or_expression)
         (inclusive_or_expression))
        (conditional_or_expression
         (conditional_or_expression OROR conditional_and_expression)
         (conditional_and_expression))
        (conditional_expression
         (conditional_or_expression QUESTION expression COLON conditional_expression)
         (conditional_or_expression))
        (assignment_expression
         (assignment)
         (conditional_expression))
        (assignment
         (left_hand_side assignment_operator assignment_expression))
        (left_hand_side
         (array_access)
         (field_access)
         (name))
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
         (EQ))
        (expression_opt nil
                        (expression))
        (expression
         (assignment_expression))
        (constant_expression
         (expression)))
      '(package_declaration import_declaration class_declaration field_declaration method_declaration formal_parameter constructor_declaration interface_declaration abstract_method_declaration)
  ))
  "Wisent LALR(1) grammar for Semantic.
Tweaked for Semantic needs.  That is to avoid full parsing of
unnecessary stuff to improve performance.")

;; Do not change here the value of the following constant!  It is
;; automatically generated from the BNF file.
(defconst wisent-java-keywords
  (semantic-flex-make-keyword-table 
   `( ("abstract" . ABSTRACT)
      ("boolean" . BOOLEAN)
      ("break" . BREAK)
      ("byte" . BYTE)
      ("case" . CASE)
      ("catch" . CATCH)
      ("char" . CHAR)
      ("class" . CLASS)
      ("const" . CONST)
      ("continue" . CONTINUE)
      ("default" . DEFAULT)
      ("do" . DO)
      ("double" . DOUBLE)
      ("else" . ELSE)
      ("extends" . EXTENDS)
      ("final" . FINAL)
      ("finally" . FINALLY)
      ("float" . FLOAT)
      ("for" . FOR)
      ("goto" . GOTO)
      ("if" . IF)
      ("implements" . IMPLEMENTS)
      ("import" . IMPORT)
      ("instanceof" . INSTANCEOF)
      ("int" . INT)
      ("interface" . INTERFACE)
      ("long" . LONG)
      ("native" . NATIVE)
      ("new" . NEW)
      ("package" . PACKAGE)
      ("private" . PRIVATE)
      ("protected" . PROTECTED)
      ("public" . PUBLIC)
      ("return" . RETURN)
      ("short" . SHORT)
      ("static" . STATIC)
      ("strictfp" . STRICTFP)
      ("super" . SUPER)
      ("switch" . SWITCH)
      ("synchronized" . SYNCHRONIZED)
      ("this" . THIS)
      ("throw" . THROW)
      ("throws" . THROWS)
      ("transient" . TRANSIENT)
      ("try" . TRY)
      ("void" . VOID)
      ("volatile" . VOLATILE)
      ("while" . WHILE)
      ("@author" . _AUTHOR)
      ("@version" . _VERSION)
      ("@param" . _PARAM)
      ("@return" . _RETURN)
      ("@exception" . _EXCEPTION)
      ("@throws" . _THROWS)
      ("@see" . _SEE)
      ("@since" . _SINCE)
      ("@serial" . _SERIAL)
      ("@serialData" . _SERIALDATA)
      ("@serialField" . _SERIALFIELD)
      ("@deprecated" . _DEPRECATED)
      )
   '(
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")
     ("boolean" summary "Primitive logical quantity type (true or false)")
     ("break" summary "break [<label>] ;")
     ("byte" summary "Integral primitive type (-128 to 127)")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("char" summary "Integral primitive type ('\u0000' to '\uffff') (0 to 65535)")
     ("class" summary "Class declaration: class <name>")
     ("const" summary "Unused reserved word")
     ("continue" summary "continue [<label>] ;")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("do" summary "do <stmt> while (<expr>);")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("extends" summary "SuperClass|SuperInterfaces declaration: extends <name> [, ...]")
     ("final" summary "Class|Member declaration modifier: final {class|<type>} <name> ...")
     ("finally" summary "try {<stmts>} ... finally {<stmts>}")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("goto" summary "Unused reserved word")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("implements" summary "Class SuperInterfaces declaration: implements <name> [, ...]")
     ("import" summary "Import package declarations: import <package>")
     ("int" summary "Integral primitive type (-2147483648 to 2147483647)")
     ("interface" summary "Interface declaration: interface <name>")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("native" summary "Method declaration modifier: native <type> <name> ...")
     ("package" summary "Package declaration: package <name>")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("return" summary "return [<expr>] ;")
     ("short" summary "Integral primitive type (-32768 to 32767)")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("strictfp" summary "Declaration modifier: strictfp {class|interface|<type>} <name> ...")
     ("switch" summary "switch(<expr>) {[case <const-expr>: <stmts> ...] [default: <stmts>]}")
     ("synchronized" summary "synchronized (<expr>) ... | Method decl. modifier: synchronized <type> <name> ...")
     ("throw" summary "throw <expr> ;")
     ("throws" summary "Method|Constructor declaration: throws <classType>, ...")
     ("transient" summary "Field declaration modifier: transient <type> <name> ...")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("void" summary "Method return type: void <name> ...")
     ("volatile" summary "Field declaration modifier: volatile <type> <name> ...")
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("@author" javadoc (seq 1 usage (type)))
     ("@version" javadoc (seq 2 usage (type)))
     ("@param" javadoc (seq 3 usage (function) with-name t))
     ("@return" javadoc (seq 4 usage (function)))
     ("@exception" javadoc (seq 5 usage (function) with-name t))
     ("@throws" javadoc (seq 6 usage (function) with-name t))
     ("@see" javadoc (seq 7 usage (type function variable) opt t with-ref t))
     ("@since" javadoc (seq 8 usage (type function variable) opt t))
     ("@serial" javadoc (seq 9 usage (variable) opt t))
     ("@serialData" javadoc (seq 10 usage (function) opt t))
     ("@serialField" javadoc (seq 11 usage (variable) opt t))
     ("@deprecated" javadoc (seq 12 usage (type function variable) opt t))
     ))
  "Java keywords.")

;; Do not change here the value of the following constant!  It is
;; automatically generated from the BNF file.
(defconst wisent-java-tokens
  '((literal
     (IDENTIFIER . "any symbol")
     (NUMBER_LITERAL . "any number")
     (STRING_LITERAL . "any string")
     (BOOLEAN_LITERAL . "true|false")
     (NULL_LITERAL . "null"))
    (operator
     (COMP . "~")
     (OROR . "||")
     (OREQ . "|=")
     (OR . "|")
     (XOREQ . "^=")
     (XOR . "^")
     (QUESTION . "?")
     (URSHIFTEQ . ">>>=")
     (URSHIFT . ">>>")
     (RSHIFTEQ . ">>=")
     (RSHIFT . ">>")
     (GTEQ . ">=")
     (GT . ">")
     (EQEQ . "==")
     (EQ . "=")
     (LTEQ . "<=")
     (LSHIFTEQ . "<<=")
     (LSHIFT . "<<")
     (LT . "<")
     (SEMICOLON . ";")
     (COLON . ":")
     (DIVEQ . "/=")
     (DIV . "/")
     (DOT . ".")
     (MINUSEQ . "-=")
     (MINUSMINUS . "--")
     (MINUS . "-")
     (COMMA . ",")
     (PLUSEQ . "+=")
     (PLUSPLUS . "++")
     (PLUS . "+")
     (MULTEQ . "*=")
     (MULT . "*")
     (ANDEQ . "&=")
     (ANDAND . "&&")
     (AND . "&")
     (MODEQ . "%=")
     (MOD . "%")
     (NOTEQ . "!=")
     (NOT . "!"))
    (paren
     (RBRACK . "]")
     (LBRACK . "[")
     (RBRACE . "}")
     (LBRACE . "{")
     (RPAREN . ")")
     (LPAREN . "(")))
  "Java tokens.")

(defconst wisent-java-number-regexp
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
  "Lexer regexp to match Java number terminals.
Following is the specification of Java number literals.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

(defconst wisent-java-operator-regexp
  ;; Because many operators start with the same characters they are
  ;; sorted so that the longest ones are matched first.  I tried
  ;; `regexp-opt' (Emacs 21) but the optimized regexp failed to match
  ;; some operators like "~" and "+" :-(
  (concat "\\("
          (mapconcat #'(lambda (op)
                         (regexp-quote (cdr op)))
                     (sort (copy-sequence
                            (semantic-flex-token-value
                             wisent-java-tokens 'operator))
                           #'(lambda (op1 op2)
                               (let ((ops1 (cdr op1))
                                     (ops2 (cdr op2)))
                                 (or (< (length ops2) (length ops1))
                                     (string-lessp ops2 ops1)))))
                     "\\|")
          "\\)")
  "Lexer regexp to match Java operator terminals.")

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
          (setq x (semantic-flex-start tk))
          (goto-char x)
          (if (looking-at wisent-java-number-regexp)
              (progn
                (setq y (match-end 0))
                ;; Adjust input stream.
                (while (and tk (<= (semantic-flex-end tk) y))
                  (setq is (cdr is)
                        tk (car is)))
                (setq lex (cons 'NUMBER_LITERAL
                                (cons
                                 (buffer-substring-no-properties x y)
                                 (cons x y))))))))
       
       ;; Punctuation
       ;; -----------
       ((eq ft 'punctuation)
        (save-excursion
          (setq x (semantic-flex-start tk))
          (goto-char x)
          (or (looking-at wisent-java-operator-regexp)
              (error "Invalid punctuation %s in input"
                     (semantic-flex-text tk)))
          (setq y   (match-end 0)
                lex (buffer-substring-no-properties x y))
          ;; Adjust input stream.
          (while (and tk (<= (semantic-flex-end tk) y))
            (setq is (cdr is)
                  tk (car is)))
          (setq lex (cons (semantic-flex-token-key
                           wisent-java-tokens 'operator lex)
                          (cons lex (cons x y))))))
       
       ;; Parens
       ;; ------
       ((memq ft '(open-paren close-paren))
        (or (setq x (semantic-flex-text tk)
                  y (semantic-flex-token-key
                     wisent-java-tokens 'paren x))
            (error "Invalid %s %s in input" ft x))
        (setq lex (cons y (cons x (cdr tk)))
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
            cache (wisent-parse semantic-toplevel-bovine-table
                                #'wisent-java-lex
                                #'ignore ;; Don't report syntax errors
                                ))
      (working-status t))
    (semantic-overlay-list cache)
    cache))

;; Do not change here the code automatically generated from the BNF
;; file!
(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser."
  ;; Code generated from wisent-java.bnf
  (setq semantic-toplevel-bovine-table wisent-java-parser-tables
        semantic-toplevel-bovine-table-source "wisent-java.bnf")
  (setq semantic-flex-keywords-obarray wisent-java-keywords)
  (progn
    ;; semantic overloaded functions
    (semantic-install-function-overrides
     '((prototype-nonterminal . semantic-java-prototype-nonterminal)
       (find-documentation    . semantic-java-find-documentation))
     t ;; They can be changed in mode hook by more specific ones
     )
    (setq
     ;; Override the default parser to setup the alternate LALR one.
     semantic-bovinate-toplevel-override 'wisent-java-bovinate-toplevel
     ;; Java is case sensitive
     semantic-case-fold nil
     ;; function to use when creating items in imenu
     semantic-imenu-summary-function 'semantic-prototype-nonterminal
     ;; function to use for creating the imenu
     imenu-create-index-function 'semantic-create-imenu-index
     ;; Character used to separation a parent/child relationship
     semantic-type-relation-separator-character '(".")
     semantic-command-separation-character ";"
     document-comment-start "/**"
     document-comment-line-prefix " *"
     document-comment-end " */"
     ;; speedbar and imenu buckets name
     semantic-symbol->name-assoc-list '((type     . "Classes")
                                        (variable . "Variables")
                                        (function . "Methods")
                                        (include  . "Imports")
                                        (package  . "Package"))
     ;; Semantic navigation inside 'type children
     senator-step-at-token-ids '(function variable)
     )
    ;; Needed by `semantic-find-doc-snarf-comment'.
    (set (make-local-variable 'block-comment-end) "\\s-*\\*/")
    )
 
 ;; End code generated from wisent-java.bnf
 (semantic-java-doc-setup))

;; Replace the default setup by this new one.
(remove-hook 'java-mode-hook #'semantic-default-java-setup)
(add-hook    'java-mode-hook #'wisent-java-default-setup)

;;;;
;;;; Useful to debug the parser
;;;;

(defun wisent-java-parse ()
  "Parse the current buffer."
  (interactive)
  (let ((gc-cons-threshold 10000000)
        (bname (format "%s [LALR]" (buffer-name)))
        semantic-flex-depth ast clock)
    (garbage-collect)
    (message "Parsing buffer...")
    (setq clock (float-time)
          wisent-java-lex-istream
          (semantic-flex (point-min) (point-max)))
    (working-status-forms bname "done"
      (setq ast   (wisent-parse semantic-toplevel-bovine-table
                                #'wisent-java-lex
                                #'wisent-java-parse-error)
            clock (- (float-time) clock))
      (working-status t))
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
  (let ((terminals (aref semantic-toplevel-bovine-table 3))
        (bname (format "%s [LALR-LEX]" (buffer-name)))
        semantic-flex-depth lex tok)
    (setq wisent-java-lex-istream (semantic-flex-buffer))
    (working-status-forms bname "done"
      (while wisent-java-lex-istream
        (setq lex (wisent-java-lex)
              tok (wisent-translate (car lex) terminals))
        (with-current-buffer
            (get-buffer-create "*wisent-java-lex-buffer*")
          (insert
           (format "(%s[%s]\t\t\t. %S)\n"
                   (car lex) tok (cdr lex)))))
      (working-status t))
    (pop-to-buffer "*wisent-java-lex-buffer*")))

(provide 'wisent-java)

;;; wisent-java.el ends here
