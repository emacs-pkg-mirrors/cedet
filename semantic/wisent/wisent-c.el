;;; wisent-c.el -- LALR parser that produces Semantic tags for C
;;
;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 13 Jun 2003
;; Keywords: syntax
;; X-RCS: $Id: wisent-c.el,v 1.1 2003/07/07 21:04:47 ponced Exp $
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

;;; History:
;;

;;; Code:
(require 'wisent-bovine)
(require 'semantic-ast)

;;; Compatibility
;;
(if (fboundp 'c-end-of-macro)
    (defalias 'wisent-c-end-of-macro 'c-end-of-macro)
  ;; From cc-mode 5.30
  (defun wisent-c-end-of-macro ()
    "Go to the end of a preprocessor directive.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash.

This function does not do any hidden buffer changes."
    (while (progn
             (end-of-line)
             (when (and (eq (char-before) ?\\)
                        (not (eobp)))
               (forward-char)
               t))))
  )

;;; Analyzers
;;
(define-lex-regex-analyzer wisent-c-lex-symbol
  "Detect and create identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0))
        'IDENTIFIER)
    (match-beginning 0)
    (match-end 0))))

(define-lex-simple-regex-analyzer wisent-c-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUMBER)

(define-lex-regex-analyzer wisent-c-lex-string
  "Detect and create character or string tokens."
  "L?\\(\\s\"\\)"
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'CHAR_OR_STRING (point)
    (save-excursion
      ;; Skip L prefix if present.
      (goto-char (match-beginning 1))
      (semantic-lex-unterminated-syntax-protection 'CHAR_OR_STRING
        (forward-sexp 1)
        (point))))))

(define-lex-block-analyzer wisent-c-lex-blocks
  "Detect and create open, close or block tokens."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACK_BLOCK ("[" LBRACK) ("]" RBRACK))
  )

;; C Preprocessor
(define-lex-simple-regex-analyzer wisent-c-lex-pp-include
  "Detect and create preprocessor include tokens."
  "^\\s-*#\\s-*include\\>" 'PP_INCLUDE)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-if-0
  "Block out code matched in an #if 0 condition."
  "^\\s-*#\\s-*if\\s-*0\\s-*$"
  (beginning-of-line)
  (c-forward-conditional 1)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-unused
  "Skip unused preprocessor directives."
  (concat "^\\s-*#\\s-*"
          (regexp-opt '(
                        "define"
                        "undef"
                        "if"
                        "ifdef"
                        "ifndef"
                        "elif"
                        "else"
                        "endif"
                        "line"
                        "error"
                        "pragma"
                        )
                      t)
          "\\>")
  (wisent-c-end-of-macro)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-newline
  "Skip backslash ending a preprocessor line.
Go to the next line."
  "\\\\\\s-*\n"
  (setq semantic-lex-end-point (match-end 0)))

;;; Lexers
;;
(define-lex wisent-c-lexer
  "Lexical analyzer that handles C buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-c-lex-pp-skip-if-0
  wisent-c-lex-pp-skip-unused
  wisent-c-lex-pp-skip-newline
  wisent-c-lex-pp-include
  wisent-c-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  wisent-c-lex-string
  wisent-c-lex-symbol
  semantic-lex-punctuation-type
  wisent-c-lex-blocks
  semantic-lex-default-action)

(defvar wisent-c-identifier  nil)
(defvar wisent-c-paren-stack nil)

(defun wisent-c-init-parser-context ()
  "Setup a context for the LR parser engine."
  (setq wisent-c-identifier  nil
        wisent-c-paren-stack nil))

(defun wisent-c-lex ()
  "Return the next available lexical token in Wisent's form.
Change class of IDENTIFIER tokens that are typedef to TYPEDEFNAME."
  (let* ((token (wisent-lex))
         (class (semantic-lex-token-class token))
         )
    (cond
     ((memq class '(TYPEDEF VOID CHAR SHORT INT LONG FLOAT DOUBLE
                            SIGNED UNSIGNED BOOL COMPLEX IMAGINARY
                            TYPEDEFNAME RBRACE))
      (setq wisent-c-identifier t)
      )
     ((memq class '(STRUCT UNION ENUM RBRACK SEMIC))
      (setq wisent-c-identifier nil)
      )
     ((eq class 'IDENTIFIER)
      (unless wisent-c-identifier
        (setcar token 'TYPEDEFNAME))
      (setq wisent-c-identifier (not wisent-c-identifier))
      )
     ((eq class 'COMMA)
      (setq wisent-c-identifier
            (not (eq 'LPAREN (car wisent-c-paren-stack))))
      )
     ((eq class 'LPAREN)
      (when wisent-c-identifier
        (setcar token 'LPAREN-DECL))
      (push (car token) wisent-c-paren-stack)
      )
     ((eq class 'RPAREN)
      (when (eq 'LPAREN-DECL (pop wisent-c-paren-stack))
        (setcar token 'RPAREN-DECL))
      (setq wisent-c-identifier nil)
      )
;;      ((eq class 'LBRACE)
;;       (push wisent-c-identifier wisent-c-paren-stack)
;;       (setq wisent-c-identifier nil)
;;       )
;;      ((eq class 'RBRACE)
;;       (setq wisent-c-identifier (pop wisent-c-paren-stack))
;;       )
     )
    token))

;; (defun wisent-c-lex ()
;;   "Return the next available lexical token in Wisent's form.
;; Change class of IDENTIFIER tokens that are typedef to TYPEDEFNAME."
;;   (let* ((token (wisent-lex))
;;          (class (semantic-lex-token-class token))
;;          )
;;     (if (eq t (car wisent-c-paren-stack))
;;         (cond ((eq class 'RBRACE)
;;                (pop wisent-c-paren-stack))
;;               ((eq class 'RBRACK)
;;                (pop wisent-c-paren-stack)
;;                (setq wisent-c-identifier nil))
;;               )
;;       (cond
;;        ((memq class '(TYPEDEF VOID CHAR SHORT INT LONG FLOAT DOUBLE
;;                               SIGNED UNSIGNED BOOL COMPLEX IMAGINARY
;;                               TYPEDEFNAME))
;;         (setq wisent-c-identifier t)
;;         )
;;        ((eq class 'IDENTIFIER)
;;         (unless wisent-c-identifier
;;           (setcar token 'TYPEDEFNAME))
;;         (setq wisent-c-identifier (not wisent-c-identifier))
;;         )
;;        ((memq class '(STRUCT UNION ENUM SEMIC))
;;         (setq wisent-c-identifier nil)
;;         )
;;        ((eq class 'COMMA)
;;         (setq wisent-c-identifier
;;               (not (eq 'LPAREN (pop wisent-c-paren-stack))))
;;         )
;;        ((eq class 'LPAREN)
;;         (when wisent-c-identifier
;;           (setcar token 'LPAREN-DECL))
;;         (push (car token) wisent-c-paren-stack)
;;         )
;;        ((eq class 'RPAREN)
;;         (when (eq 'LPAREN-DECL (pop wisent-c-paren-stack))
;;           (setcar token 'RPAREN-DECL))
;;         (setq wisent-c-identifier nil)
;;         )
;;        ((memq class '(LBRACE LBRACK))
;;         (push t wisent-c-paren-stack)
;;         )
;;        ))
;;     token))

(defun wisent-c-lex-buffer ()
  "Run `wisent-c-lexer' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-flex-keywords-obarray wisent-c-keyword-table
        semantic-lex-types-obarray wisent-c-token-table
        semantic-lex-depth nil
        semantic-lex-analyzer 'wisent-c-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer
        (get-buffer-create "*wisent-c-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Parser
;;
(defconst wisent-c-parser-automaton
  ;;DO NOT EDIT! Generated from wisent-c.wy - 2003-07-07 23:01+0200
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
         $2)
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
         $2)
        ((direct-abstract-declarator skip-brack-block))
        ((skip-brack-block))
        ((direct-abstract-declarator LPAREN parameter-list-opt RPAREN)
         (semantic-ast-merge $1 $3))
        ((LPAREN parameter-list-opt RPAREN)
         $2))
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
         (if $1
             (wisent-cook-tag
              (wisent-raw-tag
               (semantic-tag-new-include $1 nil))))))
       (include-0
        ((PP_INCLUDE LT pp-tokens GT)
         (concat $2 $3 $4))
        ((PP_INCLUDE CHAR_OR_STRING)
         $2)
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
  "Parser automaton.")

(defconst wisent-c-keyword-table
  ;;DO NOT EDIT! Generated from wisent-c.wy - 2003-07-07 23:01+0200
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
  "Keywords.")

(defconst wisent-c-token-table
  ;;DO NOT EDIT! Generated from wisent-c.wy - 2003-07-07 23:01+0200
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
  "Tokens.")

;;;###autoload
(defun wisent-c-default-setup ()
  "Setup buffer for parse."
  ;;DO NOT EDIT! Generated from wisent-c.wy - 2003-07-07 23:01+0200
  (progn
    (semantic-install-function-overrides
     '((parse-stream . wisent-parse-stream)))
    (setq semantic-parser-name "LALR"
          semantic-toplevel-bovine-table wisent-c-parser-automaton
          semantic-debug-parser-source "wisent-c.wy"
          semantic-flex-keywords-obarray wisent-c-keyword-table
          semantic-lex-types-obarray wisent-c-token-table)
    ;; Collect unmatched syntax lexical tokens
    (semantic-make-local-hook 'wisent-discarding-token-functions)
    (add-hook 'wisent-discarding-token-functions
              'wisent-collect-unmatched-syntax nil t)
    )
  (semantic-make-local-hook 'wisent-pre-parse-hook)
  (add-hook 'wisent-pre-parse-hook
            'wisent-c-init-parser-context nil t)
  (setq
   ;; Lexical analysis
   ;;semantic-lex-number-expression wisent-c-number-regexp
   semantic-lex-depth nil
   semantic-lex-analyzer 'wisent-c-lexer
   wisent-lexer-function 'wisent-c-lex
   ;; Parsing
   semantic-tag-expand-function 'wisent-c-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '("." "->")
   semantic-command-separation-character ";"
   document-comment-start "/*"
   document-comment-line-prefix " *"
   document-comment-end " */"
   ;; speedbar and imenu buckets name
   ;; everywhere
   semantic-symbol->name-assoc-list
   '((type     . "Types")
     (variable . "Variables")
     (function . "Functions")
     (include  . "Includes")
     )
   ;; in type parts
   semantic-symbol->name-assoc-list-for-type-parts
   '((type     . "Types")
     (variable . "Attributes")
     (function . "Methods")
     (label    . "Labels")
     )
   ;; navigation inside 'type children
   senator-step-at-token-ids '(function variable)
   ))

;;;###autoload
(add-hook 'c-mode-hook 'wisent-c-default-setup)

(defun wisent-c-expand-tag (tag)
  "Expand TAG into a list of derived tags, or nil.

Expand special tags of class 'goal.  Each 'goal tag has an attribute
`:tree' whose value is a list of already expanded tags.

Expand tags issued from compound definitions, that is variable tags
whose name is a list of abstract syntax trees that contain the
following nodes:

  :id         - a C identifier.
  :specifiers - type qualifiers like const, restrict and volatile.
  :parms      - list of arguments for a compound function prototype.
  :location   - the compound declaration bounds."
  (cond
   ;; Expand a goal tag.
   ((semantic-tag-of-class-p tag 'goal)
    (semantic-tag-get-attribute tag :tree)
    )
   ;; Expand compound declarations.
   ((consp (semantic-tag-name tag))
    (let ((items (semantic-tag-name tag))
          (tmods (semantic-tag-get-attribute tag 'typemodifiers))
          ast name args spec bnds start end clone xpand)
      (while items
        ;; For each compound declaration, derive TAG to a new variable
        ;; or function tag (when an arglist exists).  Give it the name
        ;; of the compound item, and merge the attributes of TAG with
        ;; those of the compound item.  Finally, set the bounds of the
        ;; derived tag to those of the compound declaration.
        (setq ast   (car items)
              items (cdr items)
              name  (semantic-ast-get1 ast :id)
              args  (semantic-ast-get  ast :parms)
              spec  (semantic-ast-get  ast :specifiers)
              bnds  (semantic-ast-get1 ast :location)
              start (if items (car bnds) (semantic-tag-start tag))
              end   (if xpand (cdr bnds) (semantic-tag-end   tag))
              clone (if args
                        (semantic-tag-new-function name type (car args))
                      (semantic-tag-clone tag name))
              xpand (cons clone xpand))
        ;; Merge the attributes.
        (semantic-tag-put-attribute
         clone 'typemodifiers (append tmods spec))
        ;; Set the bounds.
        (semantic-tag-set-bounds clone start end))
      (nreverse xpand))
    )))

(defun wisent-c-skip (&optional range)
  "Safely skip the given RANGE and try to continue parsing.
The optional argument RANGE can be the symbol `statement' to skip the
current C statement, or the symbol `line' to skip the current line.
By default, skip the current token.
Used in error recovery semantic actions."
  (let* ((start (nth 2 wisent-input))
         (end (save-excursion
                (goto-char start)
                (cond
                 ((eq range 'line)
                  (end-of-line))
                 ((eq range 'statement)
                  (c-end-of-statement))
                 (t
                  (goto-char (nth 2 wisent-input))))
                (point))))
    (wisent-error (format "Skipping invalid '%s' from %s to %s"
                          $nterm start end))
    ;; Read input until end is reached or EOI.
    (while (and (not (eq (car wisent-input) wisent-eoi-term))
                (<= (nth 2 wisent-input) end))
      (run-hook-with-args 'wisent-discarding-token-functions
                          wisent-input)
      (setq wisent-input (wisent-lexer)))
    (unless (eq wisent-eoi-term (car wisent-input))
      (wisent-errok))
    (wisent-set-region start (1+ end))
    nil))

(provide 'wisent-c)

;;; wisent-c.el ends here
