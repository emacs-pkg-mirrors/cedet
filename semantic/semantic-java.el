;;; semantic-java.el --- Semantic details for Java

;;; Copyright (C) 1999, 2000, 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; X-RCS: $Id: semantic-java.el,v 1.1 2001/01/31 16:50:21 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-ex is free software; you can redistribute it and/or modify
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
;; Setup the Semantic Bovinator for Java.  See also the grammar in
;; java-1.2.bnf.

;;; History:
;; 

;;; Code:
(require 'semantic)

;; Generated parser table
(defvar semantic-toplevel-java-bovine-table
`((bovine-toplevel
 ( package_declaration)
 ( import_declaration)
 ( type_declaration)
 ) ; end bovine-toplevel
 (number
 ( symbol "[0-9]" punctuation "\\." symbol "[0-9Ee]" punctuation "[-+]" symbol "[0-9fFdD]")
 ( symbol "[0-9]" punctuation "\\." symbol "[0-9EefFdD]")
 ( symbol "[0-9fFdD]")
 ) ; end number
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
 (variable_declarators
 ( variable_declarator variable_declarators_opt
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end variable_declarators
 (variable_declarators_opt
 ( punctuation "," variable_declarators)
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
 ( NEW qualified_name dims array_initializer)
 ( NEW qualified_name dims)
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
      )
   '(
     ))
  "Java keywords.")

;; Default Java token prototypes
(defun semantic-java-prototype-nonterminal (token)
  "Return a prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let* ((categ (semantic-token-token token))
         (fprot (intern (format "semantic-java-prototype-%s"
                                categ))))
    (if (fboundp fprot)
        (funcall fprot token)
      (semantic-abbreviate-nonterminal token))))

(defun semantic-java-prototype-function (token)
  "Return a function (method) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (let ((name (semantic-token-name token))
        (type (semantic-token-type token))
        (args (semantic-token-function-args token))
        (argp "")
        arg)
    (while args
      (setq arg  (car args)
            args (cdr args))
      (if (semantic-token-p arg)
          (setq argp (concat argp
                             (semantic-token-type arg)
                             (if args "," "")))))
    (concat (or type "") (if type " " "") name "(" argp ")")))

(defun semantic-java-prototype-variable (token)
  "Return a variable (field) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (concat (semantic-token-type token)
          " "
          (semantic-token-name token)))

(defun semantic-java-prototype-type (token)
  "Return a type (class/interface) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (concat (semantic-token-type token)
          " "
          (semantic-token-name token)))

(defun semantic-java-prototype-include (token)
  "Return an include (import) prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (semantic-token-name token))

(defun semantic-java-prototype-package (token)
  "Return a package prototype for TOKEN.
See also `semantic-prototype-nonterminal'."
  (semantic-token-name token))

;; Mode Hook
(defun semantic-default-java-setup ()
  "Set up a buffer for semantic parsing of the Java language."

  ;; function to use when creating items in imenu.
  (setq semantic-imenu-summary-function
        'semantic-prototype-nonterminal)

  ;; function to use for creating the imenu
  (setq imenu-create-index-function
        'semantic-create-imenu-index)

  ;; speedbar and imenu buckets name.
  (setq semantic-symbol->name-assoc-list
        '((type     . "Classes")
          (variable . "Variables")
          (function . "Methods")
          (include  . "Imports")
          (package  . "Package")))

  ;; semantic overloaded functions
  (setq semantic-override-table
        '((prototype-nonterminal . semantic-java-prototype-nonterminal)))

  ;; Code generated from java-1.2.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-java-bovine-table)
  (setq semantic-flex-keywords-obarray semantic-java-keyword-table)
  (progn
    ;; Character used to separation a parent/child relationship
    (set (make-local-variable
          'semantic-type-relation-separator-character)
         ".")
    ;; Java is case sensitive
    (setq semantic-case-fold nil)
    )
 
 ;; End code generated from java-1.2.bnf
 )

(add-hook 'java-mode-hook 'semantic-default-java-setup)

(provide 'semantic-java)

;;; semantic-java.el ends here
