;;; wisent-c-wy.el --- Generated parser support file

;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Created: 2003-08-01 10:41:31+0200
;; Keywords: syntax
;; X-RCS: $Id: wisent-c-wy.el,v 1.1 2003/08/02 08:16:29 ponced Exp $
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
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-c.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;
(require 'semantic-ast)

;;; Declarations
;;
(defconst wisent-c-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("typedef" . TYPEDEF)
     ("enum" . ENUM)
     ("struct" . STRUCT)
     ("union" . UNION)
     ("auto" . AUTO)
     ("register" . REGISTER)
     ("extern" . EXTERN)
     ("asm" . ASM)
     ("inline" . INLINE)
     ("static" . STATIC)
     ("const" . CONST)
     ("restrict" . RESTRICT)
     ("volatile" . VOLATILE)
     ("char" . CHAR)
     ("short" . SHORT)
     ("int" . INT)
     ("long" . LONG)
     ("signed" . SIGNED)
     ("unsigned" . UNSIGNED)
     ("float" . FLOAT)
     ("double" . DOUBLE)
     ("void" . VOID)
     ("_Bool" . BOOL)
     ("_Complex" . COMPLEX)
     ("_Imaginary" . IMAGINARY)
     ("case" . CASE)
     ("default" . DEFAULT)
     ("if" . IF)
     ("else" . ELSE)
     ("switch" . SWITCH)
     ("while" . WHILE)
     ("do" . DO)
     ("for" . FOR)
     ("break" . BREAK)
     ("continue" . CONTINUE)
     ("goto" . GOTO)
     ("return" . RETURN)
     ("sizeof" . SIZEOF))
   'nil)
  "Table of language keywords.")

(defconst wisent-c-wy--token-table
  (wisent-lex-make-token-table
   '(("punctuation"
      (HASH . "#")
      (HASHHASH . "##")
      (ARROW . "->")
      (DECR . "--")
      (INCR . "++")
      (OROR . "||")
      (ANDAND . "&&")
      (GE . ">=")
      (LE . "<=")
      (NE . "!=")
      (EQEQ . "==")
      (LTLT_EQ . "<<=")
      (GTGT_EQ . ">>=")
      (GTGT . ">>")
      (LTLT . "<<")
      (GT . ">")
      (LT . "<")
      (OR_EQ . "|=")
      (AND_EQ . "&=")
      (HAT_EQ . "^=")
      (PERCENT_EQ . "%=")
      (SLASH_EQ . "/=")
      (STAR_EQ . "*=")
      (MINUS_EQ . "-=")
      (PLUS_EQ . "+=")
      (NOT . "!")
      (TILDE . "~")
      (OR . "|")
      (AND . "&")
      (HAT . "^")
      (PERCENT . "%")
      (SLASH . "/")
      (STAR . "*")
      (MINUS . "-")
      (PLUS . "+")
      (DOT . ".")
      (QUESTION . "?")
      (COLON . ":")
      (SEMIC . ";")
      (EQ . "=")
      (COMMA . ",")
      (ELIPSIS . "..."))
     ("semantic-list"
      (BRACK_BLOCK . "[]")
      (BRACE_BLOCK . "{}")
      (PAREN_BLOCK . "()"))
     ("close-paren"
      (RBRACE . "}")
      (RBRACK . "]")
      (RPAREN . ")")
      (RPAREN-DECL . ")"))
     ("open-paren"
      (LBRACE . "{")
      (LBRACK . "[")
      (LPAREN . "(")
      (LPAREN-DECL . "("))
     ("string"
      (CHAR_OR_STRING))
     ("number"
      (NUMBER))
     ("symbol"
      (PP_INCLUDE . "#include")
      (TYPEDEFNAME . "a typedef name")
      (IDENTIFIER . "an ordinary identifier")))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-c-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((IDENTIFIER TYPEDEFNAME PP_INCLUDE NUMBER CHAR_OR_STRING LPAREN-DECL RPAREN-DECL LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK ELIPSIS COMMA EQ SEMIC COLON QUESTION DOT PLUS MINUS STAR SLASH PERCENT HAT AND OR TILDE NOT PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ HAT_EQ AND_EQ OR_EQ LT GT LTLT GTGT GTGT_EQ LTLT_EQ EQEQ NE LE GE ANDAND OROR INCR DECR ARROW HASHHASH HASH TYPEDEF ENUM STRUCT UNION AUTO REGISTER EXTERN ASM INLINE STATIC CONST RESTRICT VOLATILE CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID BOOL COMPLEX IMAGINARY CASE DEFAULT IF ELSE SWITCH WHILE DO FOR BREAK CONTINUE GOTO RETURN SIZEOF)
       nil
       (assignment-expr
        ((constant-expr)))
       (constant-expr
        ((term))
        ((constant-expr term)))
       (term
        ((operand))
        ((operator)))
       (operand
        ((identifier-or-typedef-name))
        ((NUMBER))
        ((CHAR_OR_STRING))
        ((skip-paren-block))
        ((skip-brack-block))
        ((skip-brace-block)))
       (operator
        ((AND))
        ((ANDAND))
        ((AND_EQ))
        ((ARROW))
        ((COLON))
        ((DECR))
        ((DOT))
        ((EQ))
        ((EQEQ))
        ((GE))
        ((GT))
        ((GTGT))
        ((GTGT_EQ))
        ((HAT))
        ((HAT_EQ))
        ((INCR))
        ((LE))
        ((LT))
        ((LTLT))
        ((LTLT_EQ))
        ((MINUS))
        ((MINUS_EQ))
        ((NE))
        ((NOT))
        ((OR))
        ((OROR))
        ((OR_EQ))
        ((PERCENT))
        ((PERCENT_EQ))
        ((PLUS))
        ((PLUS_EQ))
        ((QUESTION))
        ((SIZEOF))
        ((SLASH))
        ((SLASH_EQ))
        ((STAR))
        ((STAR_EQ))
        ((TILDE)))
       (declaration
        ((declaration-0)
         (let
             ((decl
               (semantic-ast-get $1 :decl-list))
              (type
               (semantic-ast-get-string $1 :type))
              (spec
               (semantic-ast-get $1 :specifiers)))
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag-new-variable decl type nil 'typemodifiers spec))))))
       (declaration-0
        ((declaration-specifiers init-declarator-list SEMIC)
         (semantic-ast-merge $1 $2))
        ((declaration-specifiers SEMIC)
         (let
             ((decl
               (semantic-ast-add nil :id "*anonymous*" :location $region)))
           (semantic-ast-add $1 :decl-list decl))))
       (declaration-specifiers
        ((storage-class-specifier declaration-specifiers)
         (semantic-ast-add $2 :specifiers $1))
        ((storage-class-specifier)
         (semantic-ast-add nil :specifiers $1))
        ((type-specifier declaration-specifiers)
         (semantic-ast-add $2 :type $1))
        ((type-specifier)
         (semantic-ast-add nil :type $1))
        ((type-qualifier declaration-specifiers)
         (semantic-ast-add $2 :specifiers $1))
        ((type-qualifier)
         (semantic-ast-add nil :specifiers $1))
        ((function-specifier declaration-specifiers)
         (semantic-ast-add $2 :specifiers $1))
        ((function-specifier)
         (semantic-ast-add nil :specifiers $1)))
       (init-declarator-list
        ((init-declarator)
         (semantic-ast-add nil :decl-list $1))
        ((init-declarator-list COMMA init-declarator)
         (semantic-ast-add $1 :decl-list $3)))
       (init-declarator
        ((declarator)
         (semantic-ast-add $1 :location $region))
        ((declarator EQ initializer)
         (semantic-ast-add $1 :location $region)))
       (storage-class-specifier
        ((TYPEDEF))
        ((EXTERN))
        ((STATIC))
        ((AUTO))
        ((REGISTER)))
       (type-specifier
        ((simple-type-specifier))
        ((struct-or-union-specifier))
        ((enum-specifier)))
       (simple-type-specifier
        ((VOID))
        ((CHAR))
        ((SHORT))
        ((INT))
        ((LONG))
        ((FLOAT))
        ((DOUBLE))
        ((SIGNED))
        ((UNSIGNED))
        ((BOOL))
        ((COMPLEX))
        ((IMAGINARY))
        ((typedef-name)))
       (struct-or-union-specifier
        ((struct-or-union skip-brace-block)
         (semantic-ast-add nil :type "*anonymous*" :specifiers $1))
        ((struct-or-union identifier-or-typedef-name skip-brace-block)
         (semantic-ast-add nil :type $2 :specifiers $1))
        ((struct-or-union identifier-or-typedef-name)
         (semantic-ast-add nil :type $2 :specifiers $1)))
       (struct-or-union
        ((STRUCT)
         (progn
           (setq wisent-c-identifier t)
           $1))
        ((UNION)
         (progn
           (setq wisent-c-identifier t)
           $1)))
       (enum-specifier
        ((enum skip-brace-block)
         (semantic-ast-add nil :type "*anonymous*" :specifiers $1))
        ((enum identifier-or-typedef-name skip-brace-block)
         (semantic-ast-add nil :type $2 :specifiers $1))
        ((enum identifier-or-typedef-name)
         (semantic-ast-add nil :type $2 :specifiers $1)))
       (enum
        ((ENUM)
         (progn
           (setq wisent-c-identifier t)
           $1)))
       (identifier-or-typedef-name
        ((IDENTIFIER))
        ((typedef-name)))
       (typedef-name
        ((TYPEDEFNAME)))
       (type-qualifier
        ((CONST))
        ((RESTRICT))
        ((VOLATILE)))
       (function-specifier
        ((INLINE)))
       (declarator
        ((pointer direct-declarator)
         (semantic-ast-merge $1 $2))
        ((direct-declarator)))
       (direct-declarator
        ((IDENTIFIER)
         (semantic-ast-add nil :id $1))
        ((LPAREN-DECL declarator RPAREN-DECL)
         (progn $2))
        ((direct-declarator skip-brack-block))
        ((direct-declarator LPAREN parameter-list-opt RPAREN)
         (semantic-ast-merge $1 $3)))
       (pointer
        ((STAR type-qualifier-list)
         (semantic-ast-add $2 :pointer $1))
        ((STAR)
         (semantic-ast-add nil :pointer $1))
        ((STAR type-qualifier-list pointer)
         (semantic-ast-merge
          (semantic-ast-add $2 :pointer $1)
          $3))
        ((STAR pointer)
         (semantic-ast-add $2 :pointer $1)))
       (type-qualifier-list
        ((type-qualifier)
         (semantic-ast-add nil :specifiers $1))
        ((type-qualifier-list type-qualifier)
         (semantic-ast-add $2 :specifiers $1)))
       (parameter-list-opt
        (nil
         (semantic-ast-put nil :parms nil))
        ((parameter-list)
         (semantic-ast-put nil :parms $1)))
       (parameter-list
        ((parameter-declaration))
        ((parameter-list COMMA parameter-declaration)
         (nconc $1 $3)))
       (parameter-declaration
        ((ELIPSIS)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable "..." "va_list" nil))))
        ((declaration-specifiers parameter-declarator)
         (let*
             ((ast
               (semantic-ast-merge $1 $2))
              (decl
               (semantic-ast-get ast :decl-list))
              (type
               (semantic-ast-get-string ast :type))
              (spec
               (semantic-ast-get ast :specifiers)))
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag-new-variable decl type nil 'typemodifiers spec)))))
        ((declaration-specifiers)
         (let
             ((ast $1))
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag-new-variable "*empty*"
                                        (semantic-ast-get-string ast :type)
                                        nil 'typemodifiers
                                        (semantic-ast-get ast :specifiers)))))))
       (parameter-declarator
        ((declarator)
         (semantic-ast-add nil :decl-list
                           (semantic-ast-add $1 :location $region)))
        ((abstract-declarator)
         (semantic-ast-add nil :decl-list
                           (semantic-ast-add $1 :id "*abstract*" :location $region))))
       (abstract-declarator
        ((pointer))
        ((pointer direct-abstract-declarator)
         (semantic-ast-merge $1 $2))
        ((direct-abstract-declarator)))
       (direct-abstract-declarator
        ((LPAREN-DECL abstract-declarator RPAREN-DECL)
         (progn $2))
        ((direct-abstract-declarator skip-brack-block))
        ((skip-brack-block))
        ((direct-abstract-declarator LPAREN parameter-list-opt RPAREN)
         (semantic-ast-merge $1 $3))
        ((LPAREN parameter-list-opt RPAREN)
         (progn $2)))
       (initializer
        ((assignment-expr)))
       (compound-statement
        ((skip-brace-block)))
       (goal
        ((translation-unit)
         (wisent-raw-tag
          (semantic-tag "goal" 'goal :tree $1))))
       (translation-unit
        ((external-declaration))
        ((translation-unit external-declaration)
         (nconc $2 $1)))
       (external-declaration
        ((function-definition))
        ((declaration))
        ((include))
        ((error)
         (wisent-c-skip 'statement)))
       (function-definition
        ((function-definition-0)
         (let*
             ((name
               (semantic-ast-get1 $1 :id))
              (type
               (semantic-ast-get-string $1 :type))
              (args
               (semantic-ast-get1 $1 :parms))
              (spec
               (semantic-ast-get $1 :specifiers)))
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag-new-function name type args 'typemodifiers spec))))))
       (function-definition-0
        ((declaration-specifiers declarator declaration-list compound-statement)
         (let
             ((ast
               (semantic-ast-merge $1 $2)))
           (semantic-ast-put ast :parms
                             (nreverse $3))))
        ((declaration-specifiers declarator compound-statement)
         (semantic-ast-merge $1 $2)))
       (declaration-list
        ((declaration))
        ((declaration-list declaration)
         (nconc $2 $1)))
       (include
        ((include-0)
         (when $1
           (wisent-cook-tag
            (wisent-raw-tag
             (semantic-tag-new-include $1 nil))))))
       (include-0
        ((PP_INCLUDE LT pp-tokens GT)
         (concat $2 $3 $4))
        ((PP_INCLUDE CHAR_OR_STRING)
         (progn $2))
        ((PP_INCLUDE error)
         (wisent-c-skip 'line)))
       (pp-tokens
        ((pp-token))
        ((pp-tokens pp-token)
         (concat $1 $2)))
       (pp-token
        ((any-symbol))
        ((DOT))
        ((SLASH)))
       (any-symbol
        ((IDENTIFIER))
        ((TYPEDEFNAME))
        ((TYPEDEF))
        ((ENUM))
        ((STRUCT))
        ((UNION))
        ((AUTO))
        ((REGISTER))
        ((EXTERN))
        ((ASM))
        ((INLINE))
        ((STATIC))
        ((CONST))
        ((RESTRICT))
        ((VOLATILE))
        ((CHAR))
        ((SHORT))
        ((INT))
        ((LONG))
        ((SIGNED))
        ((UNSIGNED))
        ((FLOAT))
        ((DOUBLE))
        ((VOID))
        ((BOOL))
        ((COMPLEX))
        ((IMAGINARY))
        ((CASE))
        ((DEFAULT))
        ((IF))
        ((ELSE))
        ((SWITCH))
        ((WHILE))
        ((DO))
        ((FOR))
        ((BREAK))
        ((CONTINUE))
        ((GOTO))
        ((RETURN))
        ((SIZEOF)))
       (skip-paren-block
        ((LPAREN RPAREN)
         nil)
        ((LPAREN-DECL RPAREN-DECL)
         nil)
        ((LPAREN error)
         (wisent-skip-block))
        ((LPAREN-DECL error)
         (wisent-skip-block)))
       (skip-brace-block
        ((LBRACE RBRACE)
         nil)
        ((LBRACE error)
         (wisent-skip-block)))
       (skip-brack-block
        ((LBRACK RBRACK)
         nil)
        ((LBRACK error)
         (wisent-skip-block))))
     '(goal)))
  "Parser table.")

(defun wisent-c-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic-toplevel-bovine-table wisent-c-wy--parse-table
        semantic-debug-parser-source "wisent-c.wy"
        semantic-flex-keywords-obarray wisent-c-wy--keyword-table
        semantic-lex-types-obarray wisent-c-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Epilogue
;;




(provide 'wisent-c-wy)

;;; wisent-c-wy.el ends here
