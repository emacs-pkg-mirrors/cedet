;;; wisent-java-tags.el --- Java LALR parser for Emacs

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-java-tags.el,v 1.12 2002/08/04 16:17:09 ponced Exp $

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
  (eval-when-compile
    ;;DO NOT EDIT! Generated from wisent-java-tags.wy - 2002-08-04 17:55+0200
    (wisent-compile-grammar
     '((LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ URSHIFT URSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP IDENTIFIER STRING_LITERAL NUMBER_LITERAL ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE _AUTHOR _VERSION _PARAM _RETURN _EXCEPTION _THROWS _SEE _SINCE _SERIAL _SERIALDATA _SERIALFIELD _DEPRECATED)
       nil
       (compilation_unit
        ((package_declaration))
        ((import_declaration))
        ((type_declaration)))
       (package_declaration
        ((PACKAGE qualified_name SEMICOLON)
         (wisent-token $2 'package nil nil)))
       (import_declaration
        ((IMPORT qualified_name SEMICOLON)
         (wisent-token $2 'include nil nil))
        ((IMPORT qualified_name DOT MULT SEMICOLON)
         (wisent-token
          (concat $2 $3 $4)
          'include nil nil)))
       (type_declaration
        ((SEMICOLON)
         nil)
        ((class_declaration))
        ((interface_declaration)))
       (class_declaration
        ((modifiers_opt CLASS qualified_name superc_opt interfaces_opt class_body)
         (wisent-token $3 'type $2 $6
                       (if
                           (or $4 $5)
                           (cons $4 $5))
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil)))
       (superc_opt
        (nil)
        ((EXTENDS qualified_name)
         (identity $2)))
       (interfaces_opt
        (nil)
        ((IMPLEMENTS qualified_name_list)
         (nreverse $2)))
       (class_body
        ((BRACE_BLOCK)
         (semantic-bovinate-from-nonterminal-full
          (car $region1)
          (cdr $region1)
          'class_member_declaration)))
       (class_member_declaration
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((block)
         nil)
        ((static_initializer)
         nil)
        ((constructor_declaration))
        ((interface_declaration))
        ((class_declaration))
        ((method_declaration))
        ((field_declaration)))
       (interface_declaration
        ((modifiers_opt INTERFACE IDENTIFIER extends_interfaces_opt interface_body)
         (wisent-token $3 'type $2 $5
                       (if $4
                           (cons nil $4))
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil)))
       (extends_interfaces_opt
        (nil)
        ((EXTENDS qualified_name_list)
         (identity $2)))
       (interface_body
        ((BRACE_BLOCK)
         (semantic-bovinate-from-nonterminal-full
          (car $region1)
          (cdr $region1)
          'interface_member_declaration)))
       (interface_member_declaration
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((interface_declaration))
        ((class_declaration))
        ((method_declaration))
        ((field_declaration)))
       (static_initializer
        ((STATIC block)))
       (constructor_declaration
        ((modifiers_opt constructor_declarator throwsc_opt constructor_body)
         (wisent-token
          (car $2)
          'function nil
          (cdr $2)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $3)
          nil)))
       (constructor_declarator
        ((IDENTIFIER formal_parameter_list)
         (cons $1 $2)))
       (constructor_body
        ((block)))
       (method_declaration
        ((modifiers_opt VOID method_declarator throwsc_opt method_body)
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil))
        ((modifiers_opt type method_declarator throwsc_opt method_body)
         (wisent-token
          (car $3)
          'function $2
          (cdr $3)
          (semantic-bovinate-make-assoc-list 'typemodifiers $1 'throws $4)
          nil)))
       (method_declarator
        ((IDENTIFIER formal_parameter_list dims_opt)
         (cons
          (concat $1 $3)
          $2)))
       (throwsc_opt
        (nil)
        ((THROWS qualified_name_list)
         (nreverse $2)))
       (qualified_name_list
        ((qualified_name_list COMMA qualified_name)
         (cons $3 $1))
        ((qualified_name)
         (list $1)))
       (method_body
        ((SEMICOLON))
        ((block)))
       (block
           ((BRACE_BLOCK)))
       (formal_parameter_list
        ((PAREN_BLOCK)
         (semantic-bovinate-from-nonterminal-full
          (car $region1)
          (cdr $region1)
          'formal_parameters)))
       (formal_parameters
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((formal_parameter COMMA))
        ((formal_parameter RPAREN)))
       (formal_parameter
        ((formal_parameter_modifier_opt type variable_declarator_id)
         (wisent-token $3 'variable $2 nil
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil)))
       (formal_parameter_modifier_opt
        (nil)
        ((FINAL)
         (list $1)))
       (field_declaration
        ((modifiers_opt type variable_declarators SEMICOLON)
         (wisent-token $3 'variable $2 nil
                       (semantic-bovinate-make-assoc-list 'typemodifiers $1)
                       nil)))
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
        ((IDENTIFIER dims_opt)
         (concat $1 $2)))
       (variable_initializer
        ((expression)))
       (expression
        ((expression term))
        ((term)))
       (term
        ((literal))
        ((operator))
        ((primitive_type))
        ((IDENTIFIER))
        ((BRACK_BLOCK))
        ((PAREN_BLOCK))
        ((BRACE_BLOCK))
        ((NEW))
        ((CLASS))
        ((THIS))
        ((SUPER)))
       (literal
        ((STRING_LITERAL))
        ((NUMBER_LITERAL)))
       (operator
        ((NOT))
        ((PLUS))
        ((PLUSPLUS))
        ((MINUS))
        ((MINUSMINUS))
        ((NOTEQ))
        ((MOD))
        ((MODEQ))
        ((AND))
        ((ANDAND))
        ((ANDEQ))
        ((MULT))
        ((MULTEQ))
        ((PLUSEQ))
        ((MINUSEQ))
        ((DOT))
        ((DIV))
        ((DIVEQ))
        ((COLON))
        ((LT))
        ((LSHIFT))
        ((LSHIFTEQ))
        ((LTEQ))
        ((EQ))
        ((EQEQ))
        ((GT))
        ((GTEQ))
        ((RSHIFT))
        ((RSHIFTEQ))
        ((URSHIFT))
        ((URSHIFTEQ))
        ((QUESTION))
        ((XOR))
        ((XOREQ))
        ((OR))
        ((OREQ))
        ((OROR))
        ((COMP))
        ((INSTANCEOF)))
       (primitive_type
        ((BOOLEAN))
        ((CHAR))
        ((LONG))
        ((INT))
        ((SHORT))
        ((BYTE))
        ((DOUBLE))
        ((FLOAT)))
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
       (type
        ((qualified_name dims_opt)
         (concat $1 $2))
        ((primitive_type dims_opt)
         (concat $1 $2)))
       (qualified_name
        ((qualified_name DOT IDENTIFIER)
         (concat $1 $2 $3))
        ((IDENTIFIER)))
       (dims_opt
        (nil
         (identity ""))
        ((dims)))
       (dims
        ((dims BRACK_BLOCK)
         (concat $1 "[]"))
        ((BRACK_BLOCK)
         (identity "[]"))))
     '(compilation_unit package_declaration import_declaration class_declaration field_declaration method_declaration formal_parameter constructor_declaration interface_declaration class_member_declaration interface_member_declaration formal_parameters))
    )
  "Wisent LALR(1) grammar for Semantic.
Tweaked for Semantic needs.  That is to avoid full parsing of
unnecessary stuff to improve performance.")

(defconst wisent-java-keywords
  (identity
   ;;DO NOT EDIT! Generated from wisent-java-tags.wy - 2002-08-04 17:55+0200
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
   )
  "Java keywords.")

(defconst wisent-java-tokens
  (identity
   ;;DO NOT EDIT! Generated from wisent-java-tags.wy - 2002-08-04 17:55+0200
   (wisent-flex-make-token-table
    '(("number"
       (NUMBER_LITERAL))
      ("string"
       (STRING_LITERAL))
      ("symbol"
       (IDENTIFIER))
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
      ("semantic-list"
       (BRACK_BLOCK . "^\\[")
       (BRACE_BLOCK . "^{")
       (PAREN_BLOCK . "^("))
      ("close-paren"
       (RBRACK . "]")
       (RBRACE . "}")
       (RPAREN . ")"))
      ("open-paren"
       (LBRACK . "[")
       (LBRACE . "{")
       (LPAREN . "(")))
    'nil)
   )
  "Java tokens.")

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
Uses the bovinator with the special top-symbol `field_declaration'
to collect tokens, such as local variables or prototypes.
This function is a Java specific `get-local-variables' override."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-bovinate-from-nonterminal-full
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration
                       0)
                      vars))))
    vars))

;;;;
;;;; Semantic integration of the Java LALR parser
;;;;

(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser."
  ;;DO NOT EDIT! Generated from wisent-java-tags.wy - 2002-08-04 17:55+0200
  (progn
    (semantic-install-function-overrides
     '((bovinate-nonterminal . wisent-bovinate-nonterminal)))
    (setq semantic-bovinate-parser-name "LALR"
          semantic-toplevel-bovine-table wisent-java-parser-tables
          semantic-flex-keywords-obarray wisent-java-keywords
          wisent-flex-tokens-obarray wisent-java-tokens)
    ;; Collect unmatched syntax lexical tokens
    (semantic-make-local-hook 'wisent-discarding-token-functions)
    (add-hook 'wisent-discarding-token-functions
              'wisent-collect-unmatched-syntax nil t)
    (setq
     ;; Lexical analysis
     semantic-lex-number-expression semantic-java-number-regexp
     semantic-lex-analyzer 'wisent-java-tags-lexer
     wisent-lexer-function 'wisent-lex
     ;; Parsing
     semantic-expand-nonterminal 'wisent-java-expand-nonterminal
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
    (semantic-java-doc-setup))
  )

;; semantic overloaded functions
(semantic-install-function-overrides
 '((prototype-nonterminal . semantic-java-prototype-nonterminal)
   (find-documentation    . semantic-java-find-documentation)
   (get-local-variables   . wisent-java-get-local-variables)
   )
 t ;; They can be changed in mode hook by more specific ones
 'java-mode)

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

;; Replace the default setup by this new one.
(remove-hook 'java-mode-hook #'semantic-default-java-setup)
(add-hook    'java-mode-hook #'wisent-java-default-setup)

(provide 'wisent-java-tags)

;;; wisent-java-tags.el ends here
