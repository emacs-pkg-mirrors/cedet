;;; semantic-java.el --- Semantic details for Java

;;; Copyright (C) 1999, 2000, 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; X-RCS: $Id: semantic-java.el,v 1.25.2.1 2002/12/26 11:06:33 ponced Exp $

;; This file is not part of GNU Emacs.

;; semantic-java is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

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
;; Setup the Semantic Bovinator for Java.  See also the grammar in
;; java.bnf.

;;; History:
;; 

;;; Code:
(require 'semantic)

(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator))

;; Generated parser table
(defvar semantic-toplevel-java-bovine-table
`((bovine-toplevel
 ( package_declaration)
 ( import_declaration)
 ( type_declaration)
 ) ; end bovine-toplevel
 (literal
 ( number)
 ( qualified_name)
 ( string)
 ) ; end literal
 (type
 ( reference_type
  ,(semantic-lambda
  (nth 0 vals)))
 ( primitive_type
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end type
 (primitive_type
 ( BOOLEAN)
 ( BYTE)
 ( SHORT)
 ( INT)
 ( LONG)
 ( CHAR)
 ( FLOAT)
 ( DOUBLE)
 ) ; end primitive_type
 (reference_type
 ( array_type
  ,(semantic-lambda
  (nth 0 vals)))
 ( qualified_name
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end reference_type
 (array_type
 ( primitive_type dims
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ( qualified_name dims
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ) ; end array_type
 (qualified_name
 ( symbol punctuation "\\." qualified_name
  ,(semantic-lambda
  (list ( concat (nth 0 vals) (nth 1 vals) ( car (nth 2 vals))))))
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end qualified_name
 (package_declaration
 ( PACKAGE qualified_name punctuation ";"
  ,(semantic-lambda
  (nth 1 vals) (list 'package nil nil)))
 ) ; end package_declaration
 (import_declaration
 ( IMPORT qualified_name punctuation ";"
  ,(semantic-lambda
  (nth 1 vals) (list 'include nil nil)))
 ( IMPORT qualified_name punctuation "\\." punctuation "*" punctuation ";"
  ,(semantic-lambda
  (list ( concat ( car (nth 1 vals)) (nth 2 vals) (nth 3 vals)) 'include nil nil)))
 ) ; end import_declaration
 (type_declaration
 ( punctuation ";")
 ( class_declaration)
 ( interface_declaration)
 ) ; end type_declaration
 (modifiers_opt
 ( modifiers
  ,(semantic-lambda
  (nth 0 vals)))
 ()
 ) ; end modifiers_opt
 (modifiers
 ( modifier modifiers
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 1 vals))))
 ( modifier
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end modifiers
 (modifier
 ( PUBLIC)
 ( PROTECTED)
 ( PRIVATE)
 ( STATIC)
 ( ABSTRACT)
 ( FINAL)
 ( NATIVE)
 ( SYNCHRONIZED)
 ( TRANSIENT)
 ( VOLATILE)
 ( STRICTFP)
 ) ; end modifier
 (class_declaration
 ( modifiers_opt CLASS qualified_name class_parents class_body
  ,(semantic-lambda
  (nth 2 vals) (list 'type "class" (nth 4 vals) (nth 3 vals) ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals)) nil)))
 ) ; end class_declaration
 (class_parents
 ( super interfaces
  ,(semantic-lambda
  ( append (nth 0 vals) (nth 1 vals))))
 ( interfaces super
  ,(semantic-lambda
  ( append (nth 1 vals) (nth 0 vals))))
 ( super
  ,(semantic-lambda
  (nth 0 vals)))
 ( interfaces
  ,(semantic-lambda
  ( cons nil (nth 0 vals))))
 ()
 ) ; end class_parents
 (super
 ( EXTENDS qualified_name
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end super
 (interfaces
 ( IMPLEMENTS qualified_name_list
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end interfaces
 (qualified_name_list
 ( qualified_name punctuation "," qualified_name_list
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 2 vals))))
 ( qualified_name
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end qualified_name_list
 (class_body
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'class_body_declarations)
 ))
 ) ; end class_body
 (class_body_declarations
 ( class_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( interface_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( field_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( method_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( constructor_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end class_body_declarations
 (field_declaration
 ( modifiers_opt type variable_declarators punctuation ";"
  ,(semantic-lambda
  (nth 2 vals) (list 'variable) (nth 1 vals) (list nil ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals)) nil)))
 ) ; end field_declaration
 (field_declaration_multi
 ( modifiers_opt type variable_declarator punctuation ","
  ,(semantic-lambda
  (nth 2 vals)))
 ( modifiers_opt type variable_declarator punctuation ";"
  ,(semantic-lambda
  (nth 2 vals)))
 ( variable_declarator punctuation ","
  ,(semantic-lambda
  (nth 0 vals)))
 ( variable_declarator punctuation ";"
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end field_declaration_multi
 (variable_declarators
 ( variable_declarator variable_declarators_opt
  ,(semantic-lambda
  (list ( cons ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ) ; end variable_declarators
 (variable_declarators_opt
 ( punctuation "," variable_declarators
  ,(semantic-lambda
  (nth 1 vals)))
 ()
 ) ; end variable_declarators_opt
 (variable_declarator
 ( variable_declarator_id variable_assign_opt
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end variable_declarator
 (variable_assign_opt
 ( punctuation "=" variable_initializer)
 ()
 ) ; end variable_assign_opt
 (variable_declarator_id
 ( symbol dims
  ,(semantic-lambda
  (list ( concat (nth 0 vals) ( car (nth 1 vals))))))
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end variable_declarator_id
 (variable_initializer
 ( array_initializer)
 ( expression)
 ) ; end variable_initializer
 (method_declaration
 ( method_header method_body
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end method_declaration
 (method_header
 ( modifiers_opt method_type symbol formal_parameter_list_opt throws_opt
  ,(semantic-lambda
  (list (nth 2 vals) 'function) (nth 1 vals) (list (nth 3 vals) ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals) 'throws (nth 4 vals)) nil)))
 ) ; end method_header
 (method_type
 ( VOID
  ,(semantic-lambda
  (list (nth 0 vals))))
 ( type
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end method_type
 (formal_parameter_list_opt
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'formal_parameter_list)
 ))
 ()
 ) ; end formal_parameter_list_opt
 (formal_parameter_list
 ( formal_parameter punctuation ","
  ,(semantic-lambda
  (nth 0 vals)))
 ( formal_parameter
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end formal_parameter_list
 (formal_parameter-modifier
 ( FINAL)
 ()
 ) ; end formal_parameter-modifier
 (formal_parameter
 ( formal_parameter-modifier type variable_declarator_id
  ,(semantic-lambda
  (nth 2 vals) (list 'variable) (nth 1 vals) (list nil ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals)) nil)))
 ) ; end formal_parameter
 (throws_opt
 ( throws
  ,(semantic-lambda
  (nth 0 vals)))
 ()
 ) ; end throws_opt
 (throws
 ( THROWS qualified_name_list
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end throws
 (method_body
 ( punctuation ";")
 ( block)
 ) ; end method_body
 (constructor_declaration
 ( modifiers_opt symbol formal_parameter_list_opt throws_opt constructor_body
  ,(semantic-lambda
  (list (nth 1 vals) 'function nil (nth 2 vals) ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals) 'throws (nth 3 vals)) nil)))
 ) ; end constructor_declaration
 (constructor_body
 ( block)
 ) ; end constructor_body
 (interface_declaration
 ( modifiers_opt INTERFACE symbol interface_parents interface_body
  ,(semantic-lambda
  (list (nth 2 vals) 'type "interface" (nth 4 vals) (nth 3 vals) ( semantic-bovinate-make-assoc-list 'typemodifiers (nth 0 vals)) nil)))
 ) ; end interface_declaration
 (interface_parents
 ( EXTENDS qualified_name_list
  ,(semantic-lambda
  (nth 1 vals)))
 ()
 ) ; end interface_parents
 (interface_body
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'interface_body_declarations)
 ))
 ) ; end interface_body
 (interface_body_declarations
 ( class_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( interface_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ( method_header punctuation ";"
  ,(semantic-lambda
  (nth 0 vals)))
 ( field_declaration
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end interface_body_declarations
 (array_initializer
 ( semantic-list "\\`{")
 ) ; end array_initializer
 (block
 ( semantic-list "\\`{")
 ) ; end block
 (primary
 ( array_creation_expression)
 ( primary_no_new_array primary_dim_opt)
 ) ; end primary
 (primary_dim_opt
 ( semantic-list "\\`\\[")
 ()
 ) ; end primary_dim_opt
 (primary_no_new_array
 ( qualified_name semantic-list "\\`(")
 ( class_instance_creation_expression)
 ( semantic-list "\\`(")
 ( array_type punctuation "\\." CLASS)
 ( literal)
 ) ; end primary_no_new_array
 (class_instance_creation_expression
 ( NEW qualified_name semantic-list "\\`(" semantic-list "\\`{")
 ( NEW qualified_name semantic-list "\\`(")
 ) ; end class_instance_creation_expression
 (array_creation_expression
 ( NEW array_type array_initializer)
 ( NEW array_type)
 ) ; end array_creation_expression
 (dims_opt
 ( dims
  ,(semantic-lambda
  (nth 0 vals)))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end dims_opt
 (dims
 ( semantic-list "\\`\\[" dims_opt
  ,(semantic-lambda
  (list ( concat "[]" ( car (nth 1 vals))))))
 ) ; end dims
 (field_access
 ( primary punctuation "\\." symbol)
 ( qualified_name)
 ) ; end field_access
 (postfix_expression
 ( primary postfix_operator_opt)
 ) ; end postfix_expression
 (postfix_operator_opt
 ( punctuation "[-+]" punctuation "[-+]")
 ()
 ) ; end postfix_operator_opt
 (unary_expression
 ( punctuation "[-+^!]" unary_expression)
 ( punctuation "[-+]" punctuation "[-+]" unary_expression)
 ( semantic-list "\\`(" unary_expression)
 ( postfix_expression)
 ) ; end unary_expression
 (operator
 ( punctuation "[-+*/%=<>^~&|!?:.]")
 ( INSTANCEOF)
 ) ; end operator
 (operators
 ( operator operators)
 ( operator)
 ) ; end operators
 (operators_expression_opt
 ( operators expression)
 ()
 ) ; end operators_expression_opt
 (expression
 ( unary_expression operators_expression_opt)
 ) ; end expression
 )
                   "Java language specification.")

;; Generated keyword table
(defvar semantic-java-keyword-table
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

(defconst semantic-java-number-regexp
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

;;;;
;;;; Prototype handler
;;;;

(defun semantic-java-prototype-function (token &optional parent color)
  "Return a function (method) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (let ((name (semantic-token-name token))
        (type (semantic-token-type token))
        (args (semantic-token-function-args token))
        (argp "")
        arg argt)
    (while args
      (setq arg  (car args)
            args (cdr args))
      (if (semantic-token-p arg)
          (setq argt (if color
                         (semantic-colorize-text
                          (semantic-token-type arg) 'type)
                       (semantic-token-type arg))
                argp (concat argp argt (if args "," "")))))
    (if color
        (progn
          (if type
              (setq type (semantic-colorize-text type 'type)))
          (setq name (semantic-colorize-text name 'function))))
    (concat (or type "") (if type " " "") name "(" argp ")")))

(defun semantic-java-prototype-variable (token &optional parent color)
  "Return a variable (field) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (concat (if color
              (semantic-colorize-text
               (semantic-token-type token) 'type)
            (semantic-token-type token))
          " "
          (if color
              (semantic-colorize-text
               (semantic-token-name token) 'variable)
            (semantic-token-name token))))

(defun semantic-java-prototype-type (token &optional parent color)
  "Return a type (class/interface) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (concat (semantic-token-type token)
          " "
          (if color
              (semantic-colorize-text
               (semantic-token-name token) 'type)
            (semantic-token-name token))))

(defun semantic-java-prototype-nonterminal (token &optional parent color)
  "Return a prototype for TOKEN.
Override `semantic-prototype-nonterminal'.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in."
  (let ((fprot (intern-soft
                (format "semantic-java-prototype-%s"
                        (semantic-token-token token)))))
    (if (fboundp fprot)
        (funcall fprot token parent color)
      (semantic-prototype-nonterminal-default token parent color))))

;;;;
;;;; Specific nonterminal handler
;;;;

(defun semantic-expand-java-nonterminal (token)
  "Expand TOKEN into a list of equivalent nonterminals, or nil.
Handle multiple variable declarations in the same statement."
  (if (eq (semantic-token-token token) 'variable)
      (let ((name (semantic-token-name token)))
        (if (listp name)
            (let ((multi (cdr name)))
              
              ;; Always replace the list of variable names by the first
              ;; name to get a valid token!  There is nothing more to
              ;; do if there is only one variable in the list.
              (setcar token (car name))
              
              (if multi
                  ;; There are multiple names in the same variable
                  ;; declaration.
                  (let ((ty (semantic-token-type                 token))
                        (dv (semantic-token-variable-default     token))
                        (xs (semantic-token-variable-extra-specs token))
                        (ds (semantic-token-docstring            token))
                        (pr (semantic-token-properties           token))
                        ;; Reparse the declaration using the special
                        ;; nonterminal 'field_declaration_multi to get
                        ;; the START/END values of each variable.
                        (nl (semantic-bovinate-from-nonterminal-full
                             (semantic-token-start token)
                             (semantic-token-end   token)
                             'field_declaration_multi
                             0))
                        tok vl)
                    ;; Merge in new 'variable tokens each reparsed
                    ;; token name and overlay with other values from
                    ;; the initial token.
                    (while nl
                      (setq tok (car nl)
                            nl  (cdr nl)
                            vl  (cons
                                 (list
                                  (semantic-token-name tok)
                                  'variable
                                  ty    ; type
                                  dv    ; default value
                                  xs    ; extra specs
                                  ds    ; docstring
                                  pr    ; properties
                                  (semantic-token-overlay tok))
                                 vl)))
                    (if vl
                        ;; Cleanup the no more needed initial token.
                        (semantic-deoverlay-token token))
                    vl)))))))

;;;;
;;;; Javadoc handler
;;;;

(defsubst semantic-java-skip-spaces-backward ()
  "Move point backward, skipping Java whitespaces."
  (skip-chars-backward " \n\r\t"))

(defsubst semantic-java-skip-spaces-forward ()
  "Move point forward, skipping Java whitespaces."
  (skip-chars-forward " \n\r\t"))

(defun semantic-java-find-documentation (&optional token nosnarf)
  "Find documentation from TOKEN and return it as a clean string.
Java has documentation set in a comment preceding TOKEN's
definition.  Optional argument NOSNARF means to only return the flex
token for it.  If NOSNARF is 'flex, then only return the flex token.
Override `semantic-find-documentation'."
  (if (or token (setq token (semantic-current-nonterminal)))
      (save-excursion
        (set-buffer (semantic-token-buffer token))
        ;; Move the point at token start
        (goto-char (semantic-token-start token))
        (semantic-java-skip-spaces-forward)
        ;; If the point already at "/**" (this occurs after a doc fix)
        (if (looking-at "/\\*\\*")
            nil
          ;; Skip previous spaces
          (semantic-java-skip-spaces-backward)
          ;; Ensure point is after "*/" (javadoc block comment end)
          (condition-case nil
              (backward-char 2)
            (error nil))
          (when (looking-at "\\*/")
            ;; Move the point backward across the comment
            (forward-char 2)            ; return just after "*/"
            (forward-comment -1)        ; to skip the entire block
            ))
        ;; Verify the point is at "/**" (javadoc block comment start)
        (if (looking-at "/\\*\\*")
            (let ((p (point))
                  (c (semantic-find-doc-snarf-comment 'flex)))
              (when c
                ;; Verify that the token just following the doc
                ;; comment is the current one!
                (goto-char (semantic-flex-end c))
                (semantic-java-skip-spaces-forward)
                (when (eq token (semantic-current-nonterminal))
                  (goto-char p)
                  (semantic-find-doc-snarf-comment nosnarf))))))))

;;;;
;;;; Javadoc elements
;;;;

(defvar semantic-java-doc-line-tags nil
  "Valid javadoc line tags.
Ordered following Sun's Tag Convention at
<http://java.sun.com/j2se/javadoc/writingdoccomments/index.html>")

(defvar semantic-java-doc-with-name-tags nil
  "Javadoc tags which have a name.")

(defvar semantic-java-doc-with-ref-tags nil
  "Javadoc tags which have a reference.")

;; Optional javadoc tags by token category
;;
(defvar semantic-java-doc-extra-type-tags nil
  "Optional tags used in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-extra-function-tags nil
  "Optional tags used in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-extra-variable-tags nil
  "Optional tags used in field documentation.
Ordered following Sun's Tag Convention.")

;; All javadoc tags by token category
;;
(defvar semantic-java-doc-type-tags nil
  "Tags allowed in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-function-tags nil
  "Tags allowed in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-variable-tags nil
  "Tags allowed in field documentation.
Ordered following Sun's Tag Convention.")

(defmacro semantic-java-doc-tag (name)
  "Return doc tag from NAME.
That is @NAME."
  `(concat "@" ,name))

(defsubst semantic-java-doc-tag-name (tag)
  "Return name of the doc TAG symbol.
That is TAG `symbol-name' without the leading '@'."
  (substring (symbol-name tag) 1))

(defun semantic-java-doc-keyword-before-p (k1 k2)
  "Return non-nil if javadoc keyword K1 is before K2."
  (let* ((t1   (semantic-java-doc-tag k1))
         (t2   (semantic-java-doc-tag k2))
         (seq1 (and (semantic-flex-keyword-p t1)
                    (plist-get (semantic-flex-keyword-get t1 'javadoc)
                               'seq)))
         (seq2 (and (semantic-flex-keyword-p t2)
                    (plist-get (semantic-flex-keyword-get t2 'javadoc)
                               'seq))))
    (if (and (numberp seq1) (numberp seq2))
        (<= seq1 seq2)
      ;; Unknown tags (probably custom ones) are always after official
      ;; ones and are not themselves ordered.
      (or (numberp seq1)
          (and (not seq1) (not seq2))))))

(defun semantic-java-doc-keywords-map (fun &optional property)
  "Run function FUN for each javadoc keyword.
Return the list of FUN results.  If optional PROPERTY is non-nil, only
call FUN for javadoc keywords which have a value for PROPERTY.  FUN
receives two arguments: the javadoc keyword and its associated
'javadoc property list.  It can return any value.  Nil values are
removed from the result list."
  (delq nil
        (mapcar
         #'(lambda (k)
             (let* ((tag   (semantic-java-doc-tag k))
                    (plist (semantic-flex-keyword-get tag 'javadoc)))
               (if (or (not property) (plist-get plist property))
                   (funcall fun k plist))))
         semantic-java-doc-line-tags)))

(defun semantic-java-doc-setup ()
  "Lazy initialization of javadoc elements."
  (or semantic-java-doc-line-tags
      (setq semantic-java-doc-line-tags
            (sort (mapcar #'semantic-java-doc-tag-name
                          (semantic-flex-keywords 'javadoc))
                  #'semantic-java-doc-keyword-before-p)))

  (or semantic-java-doc-with-name-tags
      (setq semantic-java-doc-with-name-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-name)))

  (or semantic-java-doc-with-ref-tags
      (setq semantic-java-doc-with-ref-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-ref)))

  (or semantic-java-doc-extra-type-tags
      (setq semantic-java-doc-extra-type-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-extra-function-tags
      (setq semantic-java-doc-extra-function-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-extra-variable-tags
      (setq semantic-java-doc-extra-variable-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-type-tags
      (setq semantic-java-doc-type-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k)))))

  (or semantic-java-doc-function-tags
      (setq semantic-java-doc-function-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k)))))

  (or semantic-java-doc-variable-tags
      (setq semantic-java-doc-variable-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k)))))
  
  )

;;;;
;;;; Local context
;;;;

(defun semantic-java-get-local-variables ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `field_declaration'
to collect tokens, such as local variables or prototypes.
This function is a Java specific `get-local-variables' override."
  ;; The working status is to let the parser work properly
  (working-status-forms "Local" "done"
    (let ((semantic-bovination-working-type nil)
          ;; We want nothing to do with funny syntaxing while doing this.
          (semantic-unmatched-syntax-hook nil)
          ;; Disable parsing messages
          (working-status-dynamic-type nil)
          (vars nil))
      (while (not (semantic-up-context (point) 'function))
        (save-excursion
          (forward-char 1)
          (setq vars
                (append (semantic-bovinate-region-until-error
                         (point)
                         (save-excursion (semantic-end-of-context) (point))
                         'field_declaration)
                        vars))))
      vars)))

;;;;
;;;; Mode Hook
;;;;

;;;###autoload
(defun semantic-default-java-setup ()
  "Set up a buffer for semantic parsing of the Java language."

  ;; semantic overloaded functions
  (semantic-install-function-overrides
   '((prototype-nonterminal . semantic-java-prototype-nonterminal)
     (find-documentation    . semantic-java-find-documentation)
     (get-local-variables   . semantic-java-get-local-variables)
     )
   t ;; They can be changed in mode hook by more specific ones
   )

  ;; Code generated from java.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-java-bovine-table
        semantic-toplevel-bovine-table-source "java.bnf")
  (setq semantic-flex-keywords-obarray semantic-java-keyword-table)
  (progn
    (setq
     ;; Java numbers
     semantic-number-expression semantic-java-number-regexp
     ;; Java is case sensitive
     semantic-case-fold nil
     ;; special handling of multiple variable declarations/statement
     semantic-expand-nonterminal 'semantic-expand-java-nonterminal
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
     semantic-symbol->name-assoc-list-for-type-parts
     ;; In type parts
     '((type     . "Classes")
       (variable . "Variables")
       (function . "Methods"))
     semantic-symbol->name-assoc-list
     ;; Everywhere
     (append semantic-symbol->name-assoc-list-for-type-parts
             '((include  . "Imports")
               (package  . "Package")))
     ;; Semantic navigation inside 'type children
     senator-step-at-token-ids '(function variable)
     )
    )
 
  ;; End code generated from java.bnf

  (semantic-java-doc-setup)
 )

(add-hook 'java-mode-hook 'semantic-default-java-setup)

(provide 'semantic-java)

;;; semantic-java.el ends here
