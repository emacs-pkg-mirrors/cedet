;;; wisent-java.el --- Java LALR parser for Emacs

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-java.el,v 1.38 2003/04/02 10:16:50 ponced Exp $

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

(require 'wisent-bovine)
(require 'wisent-java-lex)
(require 'semantic-java)
(eval-when-compile
  (require 'semantic-util)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator)
  (require 'document))

;;;;
;;;; Global stuff
;;;;

(defconst wisent-java-parser-tables
  ;;DO NOT EDIT! Generated from wisent-java.wy - 2003-04-02 12:05+0200
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ URSHIFT URSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP NULL_LITERAL BOOLEAN_LITERAL IDENTIFIER STRING_LITERAL NUMBER_LITERAL ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE _AUTHOR _VERSION _PARAM _RETURN _EXCEPTION _THROWS _SEE _SINCE _SERIAL _SERIALDATA _SERIALFIELD _DEPRECATED)
       nil
       (goal
        ((compilation_unit)
         (wisent-raw-tag
          (semantic-tag "goal" 'goal 'tree $1))))
       (literal
        ((NULL_LITERAL))
        ((BOOLEAN_LITERAL))
        ((STRING_LITERAL))
        ((NUMBER_LITERAL)))
       (type
        ((reference_type))
        ((primitive_type)))
       (primitive_type
        ((BOOLEAN))
        ((numeric_type)))
       (numeric_type
        ((floating_point_type))
        ((integral_type)))
       (integral_type
        ((CHAR))
        ((LONG))
        ((INT))
        ((SHORT))
        ((BYTE)))
       (floating_point_type
        ((DOUBLE))
        ((FLOAT)))
       (reference_type
        ((array_type))
        ((class_or_interface_type)))
       (class_or_interface_type
        ((name)))
       (class_type
        ((class_or_interface_type)))
       (interface_type
        ((class_or_interface_type)))
       (array_type
        ((name dims)
         (concat $1 $2))
        ((primitive_type dims)
         (concat $1 $2)))
       (name
        ((qualified_name))
        ((simple_name)))
       (simple_name
        ((IDENTIFIER)))
       (qualified_name
        ((name DOT IDENTIFIER)
         (concat $1 "." $3)))
       (compilation_unit
        ((package_declaration_opt import_declarations_opt type_declarations_opt)
         (nconc $1 $2 $3)))
       (package_declaration_opt
        (nil)
        ((package_declaration)))
       (import_declarations_opt
        (nil)
        ((import_declarations)
         (apply 'nconc
                (nreverse $1))))
       (type_declarations_opt
        (nil)
        ((type_declarations)
         (apply 'nconc
                (nreverse $1))))
       (import_declarations
        ((import_declarations import_declaration)
         (cons $2 $1))
        ((import_declaration)
         (list $1)))
       (type_declarations
        ((type_declarations type_declaration)
         (cons $2 $1))
        ((type_declaration)
         (list $1)))
       (package_declaration
        ((PACKAGE name SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-package $2 nil))))
        ((error)
         (wisent-skip-token)))
       (import_declaration
        ((IMPORT name SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-include $2 nil))))
        ((IMPORT name DOT MULT SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-include
            (concat $2 $3 $4)
            nil))))
        ((error)
         (wisent-skip-token)))
       (type_declaration
        ((SEMICOLON)
         nil)
        ((interface_declaration))
        ((class_declaration))
        ((error)
         (wisent-skip-token)))
       (modifiers_opt
        (nil)
        ((modifiers)
         (nreverse $1)))
       (modifiers
        ((modifiers modifier)
         (cons $2 $1))
        ((modifier)
         (list $1)))
       (modifier
        ((STRICTFP))
        ((VOLATILE))
        ((TRANSIENT))
        ((SYNCHRONIZED))
        ((NATIVE))
        ((FINAL))
        ((ABSTRACT))
        ((STATIC))
        ((PRIVATE))
        ((PROTECTED))
        ((PUBLIC)))
       (class_declaration
        ((modifiers_opt CLASS IDENTIFIER superc_opt interfaces_opt class_body)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-type $3 $2 $6
                                  (if
                                      (or $4 $5)
                                      (cons $4 $5))
                                  'typemodifiers $1)))))
       (superc
        ((EXTENDS class_type)
         (identity $2)))
       (superc_opt
        (nil)
        ((superc)))
       (interfaces
        ((IMPLEMENTS interface_type_list)
         (identity $2)))
       (interfaces_opt
        (nil)
        ((interfaces)
         (nreverse $1)))
       (interface_type_list
        ((interface_type_list COMMA interface_type)
         (cons $3 $1))
        ((interface_type)
         (list $1)))
       (class_body
        ((LBRACE class_body_declarations_opt RBRACE)
         (identity $2)))
       (class_body_declarations_opt
        (nil)
        ((class_body_declarations)
         (apply 'nconc
                (nreverse $1))))
       (class_body_declarations
        ((class_body_declarations class_body_declaration)
         (cons $2 $1))
        ((class_body_declaration)
         (list $1)))
       (class_body_declaration
        ((block)
         nil)
        ((constructor_declaration))
        ((static_initializer)
         nil)
        ((class_member_declaration))
        ((error)
         (wisent-skip-token)))
       (class_member_declaration
        ((interface_declaration))
        ((class_declaration))
        ((method_declaration))
        ((field_declaration)))
       (field_declarations_opt
        (nil)
        ((field_declarations)
         (wisent-raw-tag
          (semantic-tag "goal" 'goal 'tree
                        (apply 'nconc
                               (nreverse $1))))))
       (field_declarations
        ((field_declarations field_declaration_maybe)
         (cons $2 $1))
        ((field_declaration_maybe)
         (list $1)))
       (field_declaration_maybe
        ((field_declaration))
        ((error)
         (wisent-skip-token)))
       (field_declaration
        ((modifiers_opt type variable_declarators SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $3 $2 nil 'typemodifiers $1)))))
       (variable_declarators
        ((variable_declarators COMMA variable_declarator)
         (cons $3 $1))
        ((variable_declarator)
         (list $1)))
       (variable_declarator
        ((variable_declarator_id EQ variable_initializer)
         (cons $1 $region))
        ((variable_declarator_id)
         (cons $1 $region)))
       (variable_declarator_id
        ((variable_declarator_id LBRACK RBRACK)
         (concat $1 "[]"))
        ((IDENTIFIER)))
       (variable_initializer
        ((array_initializer))
        ((expression)))
       (method_declaration
        ((modifiers_opt VOID method_declarator throwsc_opt method_body)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function
            (car $3)
            $2
            (cdr $3)
            'typemodifiers $1 'throws $4))))
        ((modifiers_opt type method_declarator throwsc_opt method_body)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function
            (car $3)
            $2
            (cdr $3)
            'typemodifiers $1 'throws $4)))))
       (method_declarator
        ((method_declarator LBRACK RBRACK)
         (cons
          (concat
           (car $1)
           "[]")
          (cdr $1)))
        ((IDENTIFIER LPAREN formal_parameter_list_opt RPAREN)
         (cons $1 $3)))
       (formal_parameter_list_opt
        (nil)
        ((formal_parameter_list)
         (apply 'nconc
                (nreverse $1))))
       (formal_parameter_list
        ((formal_parameter_list COMMA formal_parameter)
         (cons $3 $1))
        ((formal_parameter)
         (list $1)))
       (formal_parameter
        ((formal_parameter_modifier_opt type variable_declarator_id)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $3 $2 nil 'typemodifiers $1)))))
       (formal_parameter_modifier_opt
        (nil)
        ((FINAL)
         (list $1)))
       (throwsc_opt
        (nil)
        ((throwsc)))
       (throwsc
        ((THROWS class_type_list)
         (nreverse $2)))
       (class_type_list
        ((class_type_list COMMA class_type)
         (cons $3 $1))
        ((class_type)
         (list $1)))
       (method_body
        ((SEMICOLON))
        ((block)))
       (static_initializer
        ((STATIC block)))
       (constructor_declaration
        ((modifiers_opt constructor_declarator throwsc_opt constructor_body)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function
            (car $2)
            nil
            (cdr $2)
            'typemodifiers $1 'throws $3)))))
       (constructor_declarator
        ((simple_name LPAREN formal_parameter_list_opt RPAREN)
         (cons $1 $3)))
       (constructor_body
        ((LBRACE RBRACE))
        ((LBRACE error)
         (wisent-skip-block)))
       (explicit_constructor_invocation
        ((primary DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
        ((primary DOT THIS LPAREN argument_list_opt RPAREN SEMICOLON))
        ((SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
        ((THIS LPAREN argument_list_opt RPAREN SEMICOLON)))
       (interface_declaration
        ((modifiers_opt INTERFACE IDENTIFIER extends_interfaces_opt interface_body)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-type $3 $2 $5
                                  (if $4
                                      (cons nil $4))
                                  'typemodifiers $1)))))
       (extends_interfaces_opt
        (nil)
        ((extends_interfaces)
         (nreverse $1)))
       (extends_interfaces
        ((extends_interfaces COMMA interface_type)
         (cons $3 $1))
        ((EXTENDS interface_type)
         (list $2)))
       (interface_body
        ((LBRACE interface_member_declarations_opt RBRACE)
         (identity $2)))
       (interface_member_declarations_opt
        (nil)
        ((interface_member_declarations)
         (apply 'nconc
                (nreverse $1))))
       (interface_member_declarations
        ((interface_member_declarations interface_member_declaration)
         (cons $2 $1))
        ((interface_member_declaration)
         (list $1)))
       (interface_member_declaration
        ((interface_declaration))
        ((class_declaration))
        ((abstract_method_declaration))
        ((constant_declaration))
        ((error)
         (wisent-skip-token)))
       (constant_declaration
        ((field_declaration)))
       (abstract_method_declaration
        ((modifiers_opt VOID method_declarator throwsc_opt SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function
            (car $3)
            $2
            (cdr $3)
            'typemodifiers $1 'throws $4))))
        ((modifiers_opt type method_declarator throwsc_opt SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function
            (car $3)
            $2
            (cdr $3)
            'typemodifiers $1 'throws $4)))))
       (array_initializer
        ((LBRACE RBRACE))
        ((LBRACE error)
         (wisent-skip-block)))
       (variable_initializers
        ((variable_initializers COMMA variable_initializer))
        ((variable_initializer)))
       (block
           ((LBRACE RBRACE))
         ((LBRACE error)
          (wisent-skip-block)))
       (primary
        ((array_creation_expression))
        ((primary_no_new_array)))
       (primary_no_new_array
        ((name DOT THIS))
        ((name DOT CLASS))
        ((array_type DOT CLASS))
        ((VOID DOT CLASS))
        ((primitive_type DOT CLASS))
        ((array_access))
        ((method_invocation))
        ((field_access))
        ((class_instance_creation_expression))
        ((LPAREN expression RPAREN))
        ((THIS))
        ((literal)))
       (class_instance_creation_expression
        ((primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN class_body))
        ((primary DOT NEW IDENTIFIER LPAREN argument_list_opt RPAREN))
        ((NEW class_type LPAREN argument_list_opt RPAREN class_body))
        ((NEW class_type LPAREN argument_list_opt RPAREN)))
       (argument_list_opt
        (nil)
        ((argument_list)))
       (argument_list
        ((argument_list COMMA expression))
        ((expression)))
       (array_creation_expression
        ((NEW class_or_interface_type dim_exprs array_initializer))
        ((NEW class_or_interface_type dim_exprs))
        ((NEW primitive_type dim_exprs array_initializer))
        ((NEW primitive_type dim_exprs)))
       (dim_exprs
        ((dim_exprs dim_expr))
        ((dim_expr)))
       (dim_expr
        ((LBRACK RBRACK))
        ((LBRACK error)
         (wisent-skip-block)))
       (dims_opt
        (nil
         (identity ""))
        ((dims)))
       (dims
        ((dims LBRACK RBRACK)
         (concat $1 $2 $3))
        ((LBRACK RBRACK)
         (concat $1 $2)))
       (field_access
        ((name DOT SUPER DOT IDENTIFIER))
        ((SUPER DOT IDENTIFIER))
        ((primary DOT IDENTIFIER)))
       (method_invocation
        ((name DOT SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
        ((SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
        ((primary DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
        ((name LPAREN argument_list_opt RPAREN)))
       (array_access
        ((primary_no_new_array LBRACK expression RBRACK))
        ((name LBRACK expression RBRACK)))
       (postfix_expression
        ((postdecrement_expression))
        ((postincrement_expression))
        ((name))
        ((primary)))
       (postincrement_expression
        ((postfix_expression PLUSPLUS)))
       (postdecrement_expression
        ((postfix_expression MINUSMINUS)))
       (unary_expression
        ((unary_expression_not_plus_minus))
        ((MINUS unary_expression))
        ((PLUS unary_expression))
        ((predecrement_expression))
        ((preincrement_expression)))
       (preincrement_expression
        ((PLUSPLUS unary_expression)))
       (predecrement_expression
        ((MINUSMINUS unary_expression)))
       (unary_expression_not_plus_minus
        ((cast_expression))
        ((NOT unary_expression))
        ((COMP unary_expression))
        ((postfix_expression)))
       (cast_expression
        ((LPAREN name dims RPAREN unary_expression_not_plus_minus))
        ((LPAREN expression RPAREN unary_expression_not_plus_minus))
        ((LPAREN primitive_type dims_opt RPAREN unary_expression)))
       (multiplicative_expression
        ((multiplicative_expression MOD unary_expression))
        ((multiplicative_expression DIV unary_expression))
        ((multiplicative_expression MULT unary_expression))
        ((unary_expression)))
       (additive_expression
        ((additive_expression MINUS multiplicative_expression))
        ((additive_expression PLUS multiplicative_expression))
        ((multiplicative_expression)))
       (shift_expression
        ((shift_expression URSHIFT additive_expression))
        ((shift_expression RSHIFT additive_expression))
        ((shift_expression LSHIFT additive_expression))
        ((additive_expression)))
       (relational_expression
        ((relational_expression INSTANCEOF reference_type))
        ((relational_expression GTEQ shift_expression))
        ((relational_expression LTEQ shift_expression))
        ((relational_expression GT shift_expression))
        ((relational_expression LT shift_expression))
        ((shift_expression)))
       (equality_expression
        ((equality_expression NOTEQ relational_expression))
        ((equality_expression EQEQ relational_expression))
        ((relational_expression)))
       (and_expression
        ((and_expression AND equality_expression))
        ((equality_expression)))
       (exclusive_or_expression
        ((exclusive_or_expression XOR and_expression))
        ((and_expression)))
       (inclusive_or_expression
        ((inclusive_or_expression OR exclusive_or_expression))
        ((exclusive_or_expression)))
       (conditional_and_expression
        ((conditional_and_expression ANDAND inclusive_or_expression))
        ((inclusive_or_expression)))
       (conditional_or_expression
        ((conditional_or_expression OROR conditional_and_expression))
        ((conditional_and_expression)))
       (conditional_expression
        ((conditional_or_expression QUESTION expression COLON conditional_expression))
        ((conditional_or_expression)))
       (assignment_expression
        ((assignment))
        ((conditional_expression)))
       (assignment
        ((left_hand_side assignment_operator assignment_expression)))
       (left_hand_side
        ((array_access))
        ((field_access))
        ((name)))
       (assignment_operator
        ((OREQ))
        ((XOREQ))
        ((ANDEQ))
        ((URSHIFTEQ))
        ((RSHIFTEQ))
        ((LSHIFTEQ))
        ((MINUSEQ))
        ((PLUSEQ))
        ((MODEQ))
        ((DIVEQ))
        ((MULTEQ))
        ((EQ)))
       (expression_opt
        (nil)
        ((expression)))
       (expression
        ((assignment_expression)))
       (constant_expression
        ((expression))))
     '(goal package_declaration import_declaration class_declaration field_declarations_opt field_declaration method_declaration formal_parameter constructor_declaration interface_declaration abstract_method_declaration)))
  "Wisent LALR(1) grammar for Semantic.
Tweaked for Semantic needs.  That is to avoid full parsing of
unnecessary stuff to improve performance.")

(defconst wisent-java-keywords
  ;;DO NOT EDIT! Generated from wisent-java.wy - 2003-04-02 12:05+0200
  (semantic-lex-make-keyword-table
   '(("abstract" . ABSTRACT)
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
     ("@deprecated" . _DEPRECATED))
   '(("@deprecated" javadoc
      (seq 12 usage
           (type function variable)
           opt t))
     ("@serialField" javadoc
      (seq 11 usage
           (variable)
           opt t))
     ("@serialData" javadoc
      (seq 10 usage
           (function)
           opt t))
     ("@serial" javadoc
      (seq 9 usage
           (variable)
           opt t))
     ("@since" javadoc
      (seq 8 usage
           (type function variable)
           opt t))
     ("@see" javadoc
      (seq 7 usage
           (type function variable)
           opt t with-ref t))
     ("@throws" javadoc
      (seq 6 usage
           (function)
           with-name t))
     ("@exception" javadoc
      (seq 5 usage
           (function)
           with-name t))
     ("@return" javadoc
      (seq 4 usage
           (function)))
     ("@param" javadoc
      (seq 3 usage
           (function)
           with-name t))
     ("@version" javadoc
      (seq 2 usage
           (type)))
     ("@author" javadoc
      (seq 1 usage
           (type)))
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("volatile" summary "Field declaration modifier: volatile <type> <name> ...")
     ("void" summary "Method return type: void <name> ...")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("transient" summary "Field declaration modifier: transient <type> <name> ...")
     ("throws" summary "Method|Constructor declaration: throws <classType>, ...")
     ("throw" summary "throw <expr> ;")
     ("synchronized" summary "synchronized (<expr>) ... | Method decl. modifier: synchronized <type> <name> ...")
     ("switch" summary "switch(<expr>) {[case <const-expr>: <stmts> ...] [default: <stmts>]}")
     ("strictfp" summary "Declaration modifier: strictfp {class|interface|<type>} <name> ...")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("short" summary "Integral primitive type (-32768 to 32767)")
     ("return" summary "return [<expr>] ;")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("package" summary "Package declaration: package <name>")
     ("native" summary "Method declaration modifier: native <type> <name> ...")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("interface" summary "Interface declaration: interface <name>")
     ("int" summary "Integral primitive type (-2147483648 to 2147483647)")
     ("import" summary "Import package declarations: import <package>")
     ("implements" summary "Class SuperInterfaces declaration: implements <name> [, ...]")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("goto" summary "Unused reserved word")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("finally" summary "try {<stmts>} ... finally {<stmts>}")
     ("final" summary "Class|Member declaration modifier: final {class|<type>} <name> ...")
     ("extends" summary "SuperClass|SuperInterfaces declaration: extends <name> [, ...]")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("do" summary "do <stmt> while (<expr>);")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("continue" summary "continue [<label>] ;")
     ("const" summary "Unused reserved word")
     ("class" summary "Class declaration: class <name>")
     ("char" summary "Integral primitive type ('u0000' to 'uffff') (0 to 65535)")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("byte" summary "Integral primitive type (-128 to 127)")
     ("break" summary "break [<label>] ;")
     ("boolean" summary "Primitive logical quantity type (true or false)")
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")))
  "Java keywords.")

(defconst wisent-java-tokens
  ;;DO NOT EDIT! Generated from wisent-java.wy - 2003-04-02 12:05+0200
  (wisent-lex-make-token-table
   '(("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)
      (BOOLEAN_LITERAL . "true")
      (BOOLEAN_LITERAL . "false")
      (NULL_LITERAL . "null"))
     ("punctuation"
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
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "(")))
   'nil)
  "Java tokens.")

(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser."
  ;;DO NOT EDIT! Generated from wisent-java.wy - 2003-04-02 12:05+0200
  (progn
    (semantic-install-function-overrides
     '((parse-stream . wisent-parse-stream)))
    (setq semantic-parser-name "LALR"
          semantic-toplevel-bovine-table wisent-java-parser-tables
          semantic-debug-parser-source "wisent-java.wy"
          semantic-flex-keywords-obarray wisent-java-keywords
          semantic-lex-types-obarray wisent-java-tokens)
    ;; Collect unmatched syntax lexical tokens
    (semantic-make-local-hook 'wisent-discarding-token-functions)
    (add-hook 'wisent-discarding-token-functions
              'wisent-collect-unmatched-syntax nil t)
    (setq
     ;; Lexical analysis
     semantic-lex-number-expression semantic-java-number-regexp
     semantic-lex-depth nil
     semantic-lex-analyzer 'wisent-java-lexer
     ;; Parsing
     semantic-tag-expand-function 'wisent-java-expand-tag
     ;; Environment
     semantic-imenu-summary-function 'semantic-prototype-nonterminal
     imenu-create-index-function 'semantic-create-imenu-index
     semantic-type-relation-separator-character '(".")
     semantic-command-separation-character ";"
     document-comment-start "/**"
     document-comment-line-prefix " *"
     document-comment-end " */"
     ;; speedbar and imenu buckets name
     semantic-symbol->name-assoc-list-for-type-parts
     ;; in type parts
     '((type     . "Classes")
       (variable . "Variables")
       (function . "Methods"))
     semantic-symbol->name-assoc-list
     ;; everywhere
     (append semantic-symbol->name-assoc-list-for-type-parts
             '((include  . "Imports")
               (package  . "Package")))
     ;; navigation inside 'type children
     senator-step-at-token-ids '(function variable)
     )
    ;; Setup javadoc stuff
    (semantic-java-doc-setup)))

;; semantic overloaded functions
(semantic-install-function-overrides
 '((prototype-nonterminal . semantic-java-prototype-nonterminal)
   (find-documentation    . semantic-java-find-documentation)
   (get-local-variables   . wisent-java-get-local-variables)
   )
 t ;; They can be changed in mode hook by more specific ones
 'java-mode)

(defun wisent-java-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand special tags of class 'goal into a list of tags.  Each 'goal
tag has an attribute `tree' whose value is a list of already expanded
tags in reverse order.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME START . END).  NAME is a variable name.  START and END
are the bounds in the declaration, related to this variable NAME."
  (let ((class (semantic-tag-class tag))
        elts elt clone start end xpand)
    (cond
     ;; Expand a goal tag
     ((eq class 'goal)
      (nreverse (semantic-tag-get-attribute tag 'tree)))
     ;; Expand multiple names in the same variable declaration.
     ((and (eq class 'variable)
           (consp (setq elts (semantic-tag-name tag))))
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              start (if elts  (cadr elt) (semantic-tag-start tag))
              end   (if xpand (cddr elt) (semantic-tag-end   tag))
              clone (semantic-tag-clone tag (car elt))
              xpand (cons clone xpand))
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand))))

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
;;;; Local context
;;;;

(defun wisent-java-get-local-variables ()
  "Get local values from a specific context.
Parse the current context for `field_declarations_opt' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declarations_opt
                       nil t)
                      vars))))
    vars))

;;;;
;;;; Semantic integration of the Java LALR parser
;;;;

;; Replace the default setup by this new one.
(remove-hook 'java-mode-hook #'semantic-default-java-setup)
(add-hook    'java-mode-hook #'wisent-java-default-setup)

(provide 'wisent-java)

;;; wisent-java.el ends here
