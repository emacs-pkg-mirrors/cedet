;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.57.2.11 2003/04/04 12:19:37 berndl Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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

;;; History:
;; 

(require 'semantic)
(require 'backquote)

(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator)
  (require 'cc-mode))

;;; Code:
(defvar semantic-toplevel-c-bovine-table
`((bovine-toplevel
 ( macro)
 ( type)
 ( define)
 ( var-or-fun)
 ( extern-c)
 ( template)
 ( using)
 ) ; end declaration
 (bovine-inner-scope
 ( define)
 ( codeblock-var-or-fun)
 ( type)
 ) ; end codeblock
 (extern-c-contents
 ( open-paren
  ,(semantic-lambda
  (list nil)))
 ( bovine-toplevel)
 ( close-paren
  ,(semantic-lambda
  (list nil)))
 ) ; end extern-c-contents
 (extern-c
 ( EXTERN string "\"C\"" semantic-list
  ,(semantic-lambda
  (list 'extern
 (semantic-bovinate-from-nonterminal-full (car (nth 2 vals)) (cdr (nth 2 vals)) 'extern-c-contents)
 )))
 ( EXTERN string "\"C\""
  ,(semantic-lambda
  (list nil)))
 ) ; end extern-c
 (macro-expression-list
 ( expression macro-expression-list punctuation "\\b;\\b"
  ,(semantic-lambda
  (list nil)))
 ( expression
  ,(semantic-lambda
  (list nil)))
 ) ; end macro-expression-list
 (macro-def
 ( macro-expression-list
  ,(semantic-lambda
  (list nil)))
 ( expression
  ,(semantic-lambda
  (list nil)))
 ()
 ) ; end macro-def
 (macro
 ( punctuation "\\b#\\b" macro-or-include
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end macro
 (macro-or-include
 ( DEFINE symbol opt-define-arglist macro-def
  ,(semantic-lambda
  (list (nth 1 vals) 'variable nil (nth 2 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ( INCLUDE system-include
  ,(semantic-lambda
  (list ( substring (nth 1 vals) 1 ( 1- ( length (nth 1 vals)))) 'include t nil)))
 ( INCLUDE string
  ,(semantic-lambda
  (list ( read (nth 1 vals)) 'include nil nil)))
 ) ; end macro-or-include
 (opt-define-arglist
 ( semantic-list
  ,(semantic-lambda
  (list nil)))
 ()
 ) ; end opt-define-arglist
 (define
 ( punctuation "\\b#\\b" DEFINE symbol opt-define-arglist macro-def
  ,(semantic-lambda
  (list (nth 2 vals) 'variable nil (nth 3 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ) ; end define
 (unionparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'classsubparts)
 ))
 ) ; end unionparts
 (opt-symbol
 ( symbol)
 ()
 ) ; end opt-symbol
 (classsubparts
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( class-protection opt-symbol punctuation "\\b:\\b"
  ,(semantic-lambda
  (nth 0 vals) (list 'label)))
 ( var-or-fun)
 ( type)
 ( define
  ,(semantic-lambda
  (nth 0 vals) (list 'protection)))
 ( template)
 ()
 ) ; end classsubparts
 (opt-class-parents
 ( punctuation "\\b:\\b" class-parents opt-template-specifier
  ,(semantic-lambda
  (list (nth 1 vals))))
 (
  ,(semantic-lambda
 ))
 ) ; end opt-class-parents
 (class-parents
 ( opt-class-protection opt-class-declmods namespace-symbol punctuation "\\b,\\b" class-parents
  ,(semantic-lambda
  ( cons (nth 2 vals) (nth 4 vals))))
 ( opt-class-protection opt-class-declmods namespace-symbol
  ,(semantic-lambda
  (nth 2 vals)))
 ) ; end class-parents
 (opt-class-declmods
 ( class-declmods opt-class-declmods
  ,(semantic-lambda
  (list nil)))
 ()
 ) ; end opt-class-declmods
 (class-declmods
 ( VIRTUAL)
 ) ; end class-declmods
 (class-protection
 ( PUBLIC)
 ( PRIVATE)
 ( PROTECTED)
 ) ; end class-protection
 (opt-class-protection
 ( class-protection
  ,(semantic-lambda
  (nth 0 vals)))
 ()
 ) ; end opt-class-protection
 (namespaceparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'namespacesubparts)
 ))
 ) ; end namespaceparts
 (namespacesubparts
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( type)
 ( var-or-fun)
 ( define)
 ( class-protection punctuation "\\b:\\b"
  ,(semantic-lambda
  (list (nth 0 vals) 'protection)))
 ( template)
 ( using)
 ()
 ) ; end namespacesubparts
 (enumparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'enumsubparts)
 ))
 ) ; end enumparts
 (enumsubparts
 ( symbol opt-assign
  ,(semantic-lambda
  (list (nth 0 vals) 'variable "int") (nth 1 vals) (list ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( punctuation "\\b,\\b"
  ,(semantic-lambda
  (list nil)))
 ) ; end enumsubparts
 (opt-name
 ( symbol)
 (
  ,(semantic-lambda
  (list "")))
 ) ; end opt-name
 (opt-class-declmods
 ( symbol declespec semantic-list)
 ( symbol)
 ()
 ) ; end opt-class-declmods
 (typesimple
 ( struct-or-class opt-name opt-template-specifier opt-class-parents semantic-list
  ,(semantic-lambda
  (nth 1 vals) (list 'type) (nth 0 vals) (list ( let ( ( semantic-c-classname ( cons ( car (nth 1 vals)) ( car (nth 0 vals)))))
 (semantic-bovinate-from-nonterminal-full (car (nth 4 vals)) (cdr (nth 4 vals)) 'classsubparts)
 ) (nth 3 vals) ( semantic-bovinate-make-assoc-list (append 'template-specifier) (nth 2 vals)) nil)))
 ( struct-or-class opt-name opt-template-specifier opt-class-parents
  ,(semantic-lambda
  (nth 1 vals) (list 'type) (nth 0 vals) (list nil (nth 3 vals) ( semantic-bovinate-make-assoc-list (append 'template-specifier) (nth 2 vals)) nil)))
 ( UNION opt-name unionparts
  ,(semantic-lambda
  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( ENUM opt-name enumparts
  ,(semantic-lambda
  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( TYPEDEF declmods typeformbase cv-declmods typedef-symbol-list
  ,(semantic-lambda
  (list (nth 4 vals) 'type (nth 0 vals) nil (nth 2 vals) nil nil)))
 ) ; end typesimple
 (typedef-symbol-list
 ( typedefname punctuation "\\b,\\b" typedef-symbol-list
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 2 vals))))
 ( typedefname
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end typedef-symbol-list
 (typedefname
 ( opt-stars symbol opt-bits opt-array
  ,(semantic-lambda
  (list (nth 0 vals) (nth 1 vals))))
 ) ; end typedefname
 (struct-or-class
 ( STRUCT)
 ( CLASS)
 ) ; end struct-or-class
 (type
 ( typesimple punctuation "\\b;\\b"
  ,(semantic-lambda
  (nth 0 vals)))
 ( NAMESPACE symbol namespaceparts
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( NAMESPACE namespaceparts
  ,(semantic-lambda
  (list "unnamed" 'type (nth 0 vals) (nth 1 vals) nil nil nil)))
 ) ; end type
 (using
 ( USING typeformbase punctuation "\\b;\\b"
  ,(semantic-lambda
  (list nil)))
 ( USING NAMESPACE typeformbase punctuation "\\b;\\b"
  ,(semantic-lambda
  (list nil)))
 ) ; end using
 (template
 ( TEMPLATE template-specifier opt-friend template-definition
  ,(semantic-lambda
  ( semantic-c-reconstitute-template (nth 3 vals) (nth 1 vals))))
 ) ; end template
 (opt-friend
 ( FRIEND)
 ()
 ) ; end opt-friend
 (opt-template-specifier
 ( template-specifier
  ,(semantic-lambda
  (nth 0 vals)))
 (
  ,(semantic-lambda
 ))
 ) ; end opt-template-specifier
 (template-specifier
 ( punctuation "\\b<\\b" template-specifier-types punctuation "\\b>\\b"
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end template-specifier
 (template-specifier-types
 ( template-var template-specifier-type-list
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 1 vals))))
 ()
 ) ; end template-specifier-types
 (template-specifier-type-list
 ( punctuation "\\b,\\b" template-specifier-types
  ,(semantic-lambda
  (nth 1 vals)))
 (
  ,(semantic-lambda
 ))
 ) ; end template-specifier-type-list
 (template-var
 ( template-type opt-template-equal
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) ( cdr (nth 0 vals)))))
 ( string
  ,(semantic-lambda
  (list (nth 0 vals))))
 ( number
  ,(semantic-lambda
  (list (nth 0 vals))))
 ( opt-stars opt-ref namespace-symbol
  ,(semantic-lambda
  (nth 2 vals)))
 ) ; end template-var
 (opt-template-equal
 ( punctuation "\\b=\\b" symbol punctuation "\\b<\\b" template-specifier-types punctuation "\\b>\\b"
  ,(semantic-lambda
  (list (nth 1 vals))))
 ()
 ) ; end opt-template-equal
 (template-type
 ( CLASS symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type "class" nil nil)))
 ( STRUCT symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type "struct" nil nil)))
 ( TYPENAME symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type "class" nil nil)))
 ( declmods typeformbase cv-declmods opt-stars opt-ref variablearg-opt-name
  ,(semantic-lambda
  (list ( car (nth 1 vals)) 'type nil nil ( semantic-bovinate-make-assoc-list 'const ( if ( member "const" ( append (nth 0 vals) (nth 2 vals))) t nil) 'typemodifiers ( delete "const" ( append (nth 0 vals) (nth 2 vals))) 'reference ( car (nth 4 vals)) 'pointer ( car (nth 3 vals))))))
 ) ; end template-type
 (template-definition
 ( type
  ,(semantic-lambda
  (nth 0 vals)))
 ( var-or-fun
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end template-definition
 (opt-stars
 ( punctuation "\\b\\*\\b" opt-starmod opt-stars
  ,(semantic-lambda
  (list ( 1+ ( car (nth 2 vals))))))
 (
  ,(semantic-lambda
  (list 0)))
 ) ; end opt-stars
 (opt-starmod
 ( STARMOD opt-starmod
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 1 vals))))
 (
  ,(semantic-lambda
 ))
 ) ; end opt-starmod
 (STARMOD
 ( CONST)
 ) ; end STARMOD
 (declmods
 ( DECLMOD declmods
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 1 vals))))
 ( DECLMOD
  ,(semantic-lambda
  (nth 0 vals)))
 (
  ,(semantic-lambda
 ))
 ) ; end declmods
 (DECLMOD
 ( EXTERN)
 ( STATIC)
 ( CVDECLMOD)
 ( INLINE)
 ( REGISTER)
 ( FRIEND)
 ( TYPENAME)
 ( METADECLMOD)
 ( VIRTUAL)
 ) ; end DECLMOD
 (metadeclmod
 ( METADECLMOD
  ,(semantic-lambda
 ))
 (
  ,(semantic-lambda
 ))
 ) ; end metadeclmod
 (CVDECLMOD
 ( CONST)
 ( VOLATILE)
 ) ; end CVDECLMOD
 (cv-declmods
 ( CVDECLMOD cv-declmods
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 1 vals))))
 ( CVDECLMOD
  ,(semantic-lambda
  (nth 0 vals)))
 (
  ,(semantic-lambda
 ))
 ) ; end cv-declmods
 (METADECLMOD
 ( VIRTUAL)
 ( MUTABLE)
 ) ; end METADECLMOD
 (opt-ref
 ( punctuation "\\b&\\b"
  ,(semantic-lambda
  (list 1)))
 (
  ,(semantic-lambda
  (list 0)))
 ) ; end opt-ref
 (typeformbase
 ( typesimple
  ,(semantic-lambda
  (nth 0 vals)))
 ( STRUCT symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals))))
 ( UNION symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals))))
 ( ENUM symbol
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals))))
 ( builtintype
  ,(semantic-lambda
  (nth 0 vals)))
 ( namespace-symbol
  ,(semantic-lambda
  (nth 0 vals) (list 'type "class")))
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end typeformbase
 (signedmod
 ( UNSIGNED)
 ( SIGNED)
 ) ; end signedmod
 (builtintype-types
 ( VOID)
 ( CHAR)
 ( WCHAR)
 ( SHORT)
 ( INT)
 ( LONG INT
  ,(semantic-lambda
  (list ( concat (nth 0 vals) " " (nth 1 vals)))))
 ( FLOAT)
 ( DOUBLE)
 ( LONG DOUBLE
  ,(semantic-lambda
  (list ( concat (nth 0 vals) " " (nth 1 vals)))))
 ( LONG LONG
  ,(semantic-lambda
  (list ( concat (nth 0 vals) " " (nth 1 vals)))))
 ( LONG)
 ) ; end builtintype-types
 (builtintype
 ( signedmod builtintype-types
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) " " ( car (nth 1 vals))))))
 ( builtintype-types
  ,(semantic-lambda
  (nth 0 vals)))
 ( signedmod
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) " int"))))
 ) ; end builtintype
 (codeblock-var-or-fun
 ( declmods typeformbase declmods opt-ref var-or-func-decl
  ,(semantic-lambda
  ( semantic-c-reconstitute-token (nth 4 vals) (nth 0 vals) (nth 1 vals))))
 ) ; end codeblock-var-or-fun
 (var-or-fun
 ( codeblock-var-or-fun
  ,(semantic-lambda
  (nth 0 vals)))
 ( declmods var-or-func-decl
  ,(semantic-lambda
  ( semantic-c-reconstitute-token (nth 1 vals) (nth 0 vals) nil)))
 ) ; end var-or-fun
 (var-or-func-decl
 ( func-decl
  ,(semantic-lambda
  (nth 0 vals)))
 ( var-decl
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end var-or-func-decl
 (func-decl
 ( opt-stars opt-class opt-destructor functionname opt-template-specifier opt-under-p arg-list opt-post-fcn-modifiers opt-throw opt-initializers fun-or-proto-end
  ,(semantic-lambda
  (nth 3 vals) (list 'function (nth 1 vals) (nth 2 vals) (nth 6 vals) (nth 8 vals) (nth 7 vals)) (nth 0 vals) (nth 10 vals)))
 ) ; end func-decl
 (var-decl
 ( varnamelist punctuation "\\b;\\b"
  ,(semantic-lambda
  (list (nth 0 vals) 'variable)))
 ) ; end var-decl
 (opt-under-p
 ( UNDERP
  ,(semantic-lambda
  (list nil)))
 ( UNDERUNDERP
  ,(semantic-lambda
  (list nil)))
 ()
 ) ; end opt-under-p
 (opt-initializers
 ( punctuation "\\b:\\b" namespace-symbol semantic-list opt-initializers)
 ( punctuation "\\b,\\b" namespace-symbol semantic-list opt-initializers)
 ()
 ) ; end opt-initializers
 (opt-post-fcn-modifiers
 ( post-fcn-modifiers opt-post-fcn-modifiers
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 1 vals))))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-post-fcn-modifiers
 (post-fcn-modifiers
 ( REENTRANT)
 ( CONST)
 ) ; end post-fcn-modifiers
 (opt-throw
 ( THROW semantic-list
 ,(lambda (vals start end)
 
 (semantic-bovinate-from-nonterminal (car (nth 1 vals)) (cdr (nth 1 vals)) 'throw-exception-list)
 ))
 ()
 ) ; end opt-throw
 (throw-exception-list
 ( namespace-symbol punctuation "\\b,\\b" throw-exception-list
  ,(semantic-lambda
  ( cons ( car (nth 0 vals)) (nth 2 vals))))
 ( namespace-symbol close-paren ")"
  ,(semantic-lambda
  (nth 0 vals)))
 ( open-paren "(" throw-exception-list
  ,(semantic-lambda
  (nth 1 vals)))
 ( close-paren ")"
  ,(semantic-lambda
 ))
 ) ; end throw-exception-list
 (opt-bits
 ( punctuation "\\b:\\b" number
  ,(semantic-lambda
  (list (nth 1 vals))))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-bits
 (opt-array
 ( semantic-list "\\[.*\\]$" opt-array
  ,(semantic-lambda
  (list ( cons 1 ( car (nth 1 vals))))))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-array
 (opt-assign
 ( punctuation "\\b=\\b" expression
  ,(semantic-lambda
  (list (nth 1 vals))))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-assign
 (opt-restrict
 ( symbol "\\<\\(__\\)?restrict\\>")
 ()
 ) ; end opt-restrict
 (varname
 ( opt-stars opt-restrict namespace-symbol opt-bits opt-array opt-assign
  ,(semantic-lambda
  (nth 2 vals) (nth 0 vals) (nth 3 vals) (nth 4 vals) (nth 5 vals)))
 ) ; end varname
 (variablearg
 ( declmods typeformbase cv-declmods opt-ref variablearg-opt-name
  ,(semantic-lambda
  (list ( list (nth 4 vals)) 'variable (nth 1 vals) nil ( semantic-bovinate-make-assoc-list 'const ( if ( member "const" ( append (nth 0 vals) (nth 2 vals))) t nil) 'typemodifiers ( delete "const" ( append (nth 0 vals) (nth 2 vals))) 'reference ( car (nth 3 vals))) nil)))
 ) ; end variablearg
 (variablearg-opt-name
 ( varname
  ,(semantic-lambda
  (nth 0 vals)))
 ( opt-stars
  ,(semantic-lambda
  (list "") (nth 0 vals) (list nil nil nil)))
 ) ; end variablearg-opt-name
 (varnamelist
 ( varname punctuation "\\b,\\b" varnamelist
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 2 vals))))
 ( varname
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end varnamelist
 (namespace-symbol
 ( symbol opt-template-specifier punctuation "\\b:\\b" punctuation "\\b:\\b" namespace-symbol
  ,(semantic-lambda
  (list ( concat (nth 0 vals) "::" ( car (nth 4 vals))))))
 ( symbol opt-template-specifier
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end namespace-symbol
 (namespace-opt-class
 ( symbol punctuation "\\b:\\b" punctuation "\\b:\\b" namespace-opt-class
  ,(semantic-lambda
  (list ( concat (nth 0 vals) "::" ( car (nth 3 vals))))))
 ( symbol opt-template-specifier punctuation "\\b:\\b" punctuation "\\b:\\b"
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end namespace-opt-class
 (opt-class
 ( namespace-opt-class
  ,(semantic-lambda
  (nth 0 vals)))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-class
 (opt-destructor
 ( punctuation "\\b~\\b"
  ,(semantic-lambda
  (list t)))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-destructor
 (arg-list
 ( semantic-list "^(" knr-arguments
  ,(semantic-lambda
  (nth 1 vals)))
 ( semantic-list "^("
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'arg-sub-list)
 ))
 ( semantic-list "^(void)$"
  ,(semantic-lambda
 ))
 ) ; end arg-list
 (knr-arguments
 ( variablearg punctuation "\\b;\\b" knr-arguments
  ,(semantic-lambda
  ( cons ( car ( semantic-expand-c-nonterminal (nth 0 vals))) (nth 2 vals))))
 ( variablearg punctuation "\\b;\\b"
  ,(semantic-lambda
  (list ( car ( semantic-expand-c-nonterminal (nth 0 vals))))))
 ) ; end knr-arguments
 (arg-sub-list
 ( variablearg
  ,(semantic-lambda
  (nth 0 vals)))
 ( punctuation "\\b\\.\\b" punctuation "\\b\\.\\b" punctuation "\\b\\.\\b" close-paren ")"
  ,(semantic-lambda
  (list "...")))
 ( punctuation "\\b,\\b"
  ,(semantic-lambda
  (list nil)))
 ( open-paren "("
  ,(semantic-lambda
  (list nil)))
 ( close-paren ")"
  ,(semantic-lambda
  (list nil)))
 ) ; end arg-sub-list
 (operatorsym
 ( punctuation "\\b<\\b" punctuation "\\b<\\b"
  ,(semantic-lambda
  (list "<<")))
 ( punctuation "\\b>\\b" punctuation "\\b>\\b"
  ,(semantic-lambda
  (list ">>")))
 ( punctuation "\\b=\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "==")))
 ( punctuation "\\b<\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "<=")))
 ( punctuation "\\b>\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list ">=")))
 ( punctuation "\\b!\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "!=")))
 ( punctuation "\\b-\\b" punctuation "\\b>\\b"
  ,(semantic-lambda
  (list "->")))
 ( semantic-list "()"
  ,(semantic-lambda
  (list "()")))
 ( semantic-list "\\[\\]"
  ,(semantic-lambda
  (list "[]")))
 ( punctuation "\\b<\\b")
 ( punctuation "\\b>\\b")
 ( punctuation "\\b\\*\\b")
 ( punctuation "\\b\\+\\b")
 ( punctuation "\\b-\\b")
 ( punctuation "\\b/\\b")
 ( punctuation "\\b=\\b")
 ( punctuation "\\b!\\b")
 ) ; end operatorsym
 (functionname
 ( OPERATOR operatorsym
  ,(semantic-lambda
  (nth 1 vals)))
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end functionname
 (fun-or-proto-end
 ( punctuation "\\b;\\b"
  ,(semantic-lambda
  (list t)))
 ( semantic-list
  ,(semantic-lambda
  (list nil)))
 ( punctuation "\\b=\\b" number "^0$" punctuation "\\b;\\b"
  ,(semantic-lambda
  (list 'pure-virtual)))
 ) ; end fun-or-proto-end
 (type-cast
 ( semantic-list
 ,(lambda (vals start end)
 
 (semantic-bovinate-from-nonterminal (car (nth 0 vals)) (cdr (nth 0 vals)) 'type-cast-list)
 ))
 ) ; end type-cast
 (type-cast-list
 ( open-paren typeformbase close-paren)
 ) ; end type-cast-list
 (function-call
 ( namespace-symbol semantic-list)
 ) ; end function-call
 (string-seq
 ( string string-seq
  ,(semantic-lambda
  (list ( concat (nth 0 vals) ( car (nth 1 vals))))))
 ( string
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end string-seq
 (expression
 ( number
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( function-call
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( namespace-symbol
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( string-seq
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( type-cast expression
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( semantic-list
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ( punctuation "[-+*/%^|&]" expression
  ,(semantic-lambda
  (list ( identity start) ( identity end))))
 ) ; end expression
 )
 "C language specification.")

(defvar semantic-flex-c-extensions
  '(("^\\s-*#if\\s-*0$" . semantic-flex-c-if-0)
    ("^#\\(if\\(def\\)?\\|el\\(if\\|se\\)\\|endif\\)" . semantic-flex-c-if)
    ("<[^\n>]+>" . semantic-flex-c-include-system)
    ("\\(\\\\\n\\)" . semantic-flex-backslash-newline)
    ("L\\s\"" . semantic-flex-wide-char-literal)
    )
  "Extensions to the flexer for C and C++.")

(defun semantic-flex-wide-char-literal ()
  "Identify wide-char string- and char-literals like L\"string\" and L'c'
and return them as string-token."
  (let ((start (point)))
    (forward-char 1)
    ;; now we should be onto the string-delimiter direct after the L
    (forward-sexp 1)
    (cons 'string (cons start (point)))))

(defun semantic-flex-backslash-newline ()
  "If there is a \ ending a line, then it isn't really a newline. Move cursor
direct after the \"backslashed\"-newline and return nothing."
  (goto-char (match-end 1))
  nil)

(defun semantic-flex-c-include-system ()
  "Move cursor to the end of system includes and return it.
Identifies the system include by looking backwards."
  (if (and (save-excursion
	     (beginning-of-line)
	     (looking-at "#\\s-*include\\s-+<"))
	   (= (match-end 0) (1+ (point))))
      ;; We found a system include.
      (let ((start (point)))
	;; This should always pass
	(re-search-forward ">")
	;; We have the whole thing.
	(cons 'system-include (cons start (point)))
	)
    ;; No system include, do nothing but return what the real
    ;; parser would have returned.
    (forward-char 1)
    (cons 'punctuation (cons (1- (point)) (point)))
    ))

(defun semantic-flex-c-if-0 ()
  "Move cursor to the matching endif, and return nothing."
  (beginning-of-line)
  (c-forward-conditional 1)
  nil)

(defun semantic-flex-c-if ()
  "Move the cursor and return nil when a #if is found."
  ;; Future enhancement: Enable only the then or else clause depending on
  ;; some mysterious knowledge.
  (if (bolp) (end-of-line))
  nil)

(defun semantic-expand-c-nonterminal (nonterm)
  "Expand NONTERM into a list of equivalent nonterminals, or nil."
  (cond ((eq (car nonterm) 'extern)
	 ;; We have hit an exter "C" command with a list after it.
	 (car (cdr nonterm))
	 )
	((listp (car nonterm))
	 (cond ((eq (semantic-token-token nonterm) 'variable)
		;; The name part comes back in the form of:
		;; ( NAME NUMSTARS BITS ARRAY ASSIGN )
		(let ((vl nil)
		      (basety (semantic-token-type nonterm))
		      (ty "")
		      (mods (semantic-token-variable-extra-spec nonterm 'typemodifiers))
		      (suffix "")
		      (lst (semantic-token-name nonterm))
		      (default nil)
		      (cur nil))
		  (while lst
		    (setq suffix "" ty "")
		    (setq cur (car lst))
		    (if (nth 2 cur)
			(setq suffix (concat ":" (nth 2 cur))))
		    (if (= (length basety) 1)
			(setq ty (car basety))
		      (setq ty basety))
		    (setq default (nth 4 cur))
		    (setq vl (cons
			      (list
			       (car cur) ;name
			       'variable
			       ty	;type
			       (if default
				   (buffer-substring-no-properties
				    (car default) (car (cdr default))))
			       (semantic-bovinate-make-assoc-list
				'const (semantic-token-variable-const nonterm)
				'suffix suffix
				'typemodifiers mods
				'dereference (length (nth 3 cur))
				'pointer (nth 1 cur)
				)
			       (semantic-token-docstring nonterm) ;doc
			       (semantic-token-properties nonterm) ;properties
			       (semantic-token-overlay nonterm))
			      vl))
		    (setq lst (cdr lst)))
		  vl))
	       ((eq (semantic-token-token nonterm) 'type)
		;; We may someday want to add an extra check for a type
		;; of type "typedef".
		;; Each elt of NAME is ( STARS NAME )
		(let ((vl nil)
		      (names (semantic-token-name nonterm)))
		  (while names
		    (setq vl (cons (list
				    (nth 1 (car names)) ; name
				    'type
				    "typedef"
				    (semantic-token-type-parts nonterm)
				    ;; parent is just tbe name of what
				    ;; is passed down as a nonterminal.
				    (list
				     (semantic-token-name
				      (semantic-token-type-parent nonterm)))
				    (semantic-bovinate-make-assoc-list
				     'pointer
				     (let ((stars (car (car (car names)))))
				       (if (= stars 0) nil stars))
				     ;; This specifies what the typedef
				     ;; is expanded out as.  Just the
				     ;; name shows up as a parent of this
				     ;; typedef.
				     'typedef
				     (semantic-token-type-parent nonterm))
				    (semantic-token-docstring nonterm)
				    (semantic-token-properties nonterm)
				    (semantic-token-overlay nonterm))
				   vl))
		    (setq names (cdr names)))
		  vl))
	       ((and (listp (car nonterm))
		     (eq (semantic-token-token (car nonterm)) 'variable))
		;; Argument lists come in this way.  Append all the expansions!
		(let ((vl nil))
		  (while nonterm
		    (setq vl (append (semantic-expand-c-nonterminal (car vl))
				     vl)
			  nonterm (cdr nonterm)))
		  vl))
	       (t nil)))
	(t nil)))

(defvar semantic-c-classname nil
  "At parse time, assign a class or struct name text here.
It is picked up by `semantic-c-reconstitute-token' to determine
if something is a constructor.  Value should be:
  ( TYPENAME .  TYPEOFTYPE)
where typename is the name of the type, and typeoftype is \"class\"
or \"struct\".")

(defun semantic-c-reconstitute-token (tokenpart declmods typedecl)
  "Reconstitute a token TOKENPART with DECLMODS and TYPEDECL.
This is so we don't have to match the same starting text several times.
Optional argument STAR and REF indicate the number of * and & in the typedef."
  (cond ((eq (nth 1 tokenpart) 'variable)
	 (list (car tokenpart)
	       'variable
	       (or typedecl "int")	;type
	       nil			;default value (filled with expand)
	       (semantic-bovinate-make-assoc-list
		'const (if (member "const" declmods) t nil)
		'typemodifiers (delete "const" declmods)
		)
	       nil)
	 )
	((eq (nth 1 tokenpart) 'function)
	 ;; We should look at part 4 (the arglist) here, and throw an
	 ;; error of some sort if it contains parser errors so that we
	 ;; don't parser function calls, but that is a little beyond what
	 ;; is available for data here.
	 (let ((constructor
		(and (or (and semantic-c-classname
			      (string= (car semantic-c-classname)
				       (car tokenpart)))
			 (and (stringp (car (nth 2 tokenpart)))
			      (string= (car (nth 2 tokenpart)) (car tokenpart)))
			 )
		     (not (car (nth 3 tokenpart))))))
	   (list (car tokenpart)
		 'function
		 (or typedecl		;type
		     (cond ((car (nth 3 tokenpart) )
			    "void")	; Destructors have no return?
			   (constructor
			    ;; Constructors return an object.			  ;; in our
			    (list (or (car semantic-c-classname)
				      (car (nth 2 tokenpart)))
				  'type
				  (or (cdr semantic-c-classname)
				      "class")))
			   (t "int")))
		 (nth 4 tokenpart)	;arglist
		 (semantic-bovinate-make-assoc-list
		  'const (if (member "const" declmods) t nil)
		  'typemodifiers (delete "const" declmods)
		  'parent (car (nth 2 tokenpart))
		  'destructor (if (car (nth 3 tokenpart) ) t)
		  'constructor (if constructor t)
		  'pointer (nth 7 tokenpart)
		  ;; Even though it is "throw" in C++, we use
		  ;; `throws' as a common name for things that toss
		  ;; exceptions about.
		  'throws (nth 5 tokenpart)
		  ;; Reemtrant is a C++ thingy.  Add it here
		  'reentrant (if (member "reentrant" (nth 6 tokenpart)) t)
		  ;; A function post-const is funky.  Try stuff
		  'methodconst (if (member "const" (nth 6 tokenpart)) t)
		  ;; prototypes are functions w/ no body
		  'prototype (if (nth 8 tokenpart) t)
		  ;; Pure virtual
		  'pure-virtual (if (eq (nth 8 tokenpart) 'pure-virtual) t)
		  )
		 nil))
	 )
	))

(defun semantic-c-reconstitute-template (def specifier)
  "Reconstitute the token DEF with the template SPECIFIER."
  (semantic-token-add-extra-spec def 'template (or specifier ""))
  def)

(defvar semantic-c-keyword-table
  (semantic-flex-make-keyword-table 
   `( ("include" . INCLUDE)
      ("define" . DEFINE)
      ("extern" . EXTERN)
      ("static" . STATIC)
      ("const" . CONST)
      ("volatile" . VOLATILE)
      ("register" . REGISTER)
      ("signed" . SIGNED)
      ("unsigned" . UNSIGNED)
      ("inline" . INLINE)
      ("virtual" . VIRTUAL)
      ("mutable" . MUTABLE)
      ("struct" . STRUCT)
      ("union" . UNION)
      ("enum" . ENUM)
      ("typedef" . TYPEDEF)
      ("class" . CLASS)
      ("typename" . TYPENAME)
      ("namespace" . NAMESPACE)
      ("using" . USING)
      ("template" . TEMPLATE)
      ("throw" . THROW)
      ("reentrant" . REENTRANT)
      ("operator" . OPERATOR)
      ("public" . PUBLIC)
      ("private" . PRIVATE)
      ("protected" . PROTECTED)
      ("friend" . FRIEND)
      ("if" . IF)
      ("else" . ELSE)
      ("do" . DO)
      ("while" . WHILE)
      ("for" . FOR)
      ("switch" . SWITCH)
      ("case" . CASE)
      ("default" . DEFAULT)
      ("return" . RETURN)
      ("break" . BREAK)
      ("continue" . CONTINUE)
      ("sizeof" . SIZEOF)
      ("void" . VOID)
      ("char" . CHAR)
      ("wchar_t" . WCHAR)
      ("short" . SHORT)
      ("int" . INT)
      ("long" . LONG)
      ("float" . FLOAT)
      ("double" . DOUBLE)
      ("_P" . UNDERP)
      ("__P" . UNDERUNDERP)
      )
   '(
     ("extern" summary "Declaration Modifier: extern <type> <name> ...")
     ("static" summary "Declaration Modifier: static <type> <name> ...")
     ("const" summary "Declaration Modifier: const <type> <name> ...")
     ("volatile" summary "Declaration Modifier: volatile <type> <name> ...")
     ("register" summary "Declaration Modifier: register <type> <name> ...")
     ("signed" summary "Numeric Type Modifier: signed <numeric type> <name> ...")
     ("unsigned" summary "Numeric Type Modifier: unsigned <numeric type> <name> ...")
     ("inline" summary "Function Modifier: inline <return  type> <name>(...) {...};")
     ("virtual" summary "Method Modifier: virtual <type> <name>(...) ...")
     ("mutable" summary "Member Declaration Modifier: mutable <type> <name> ...")
     ("struct" summary "Structure Type Declaration: struct [name] { ... };")
     ("union" summary "Union Type Declaration: union [name] { ... };")
     ("enum" summary "Enumeration Type Declaration: enum [name] { ... };")
     ("typedef" summary "Arbitrary Type Declaration: typedef <typedeclaration> <name>;")
     ("class" summary "Class Declaration: class <name>[:parents] { ... };")
     ("typename" summary "typename is used to handle a qualified name as a typename;")
     ("namespace" summary "Namespace Declaration: namespace <name> { ... };")
     ("using" summary "using <namespace>;")
     ("template" summary "template <class TYPE ...> TYPE_OR_FUNCTION")
     ("throw" summary "<type> <methoddef> (<method args>) throw (<exception>) ...")
     ("reentrant" summary "<type> <methoddef> (<method args>) reentrant ...")
     ("if" summary "if (<condition>) { code } [ else { code } ]")
     ("else" summary "if (<condition>) { code } [ else { code } ]")
     ("do" summary " do { code } while (<condition>);")
     ("while" summary "do { code } while (<condition>); or while (<condition>) { code };")
     ("for" summary "for(<init>; <condition>; <increment>) { code }")
     ("switch" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("case" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("default" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("return" summary "return <value>;")
     ("break" summary "Non-local exit within a loop or switch (for, do/while, switch): break;")
     ("continue" summary "Non-local continue within a lool (for, do/while): continue;")
     ("sizeof" summary "Compile time macro: sizeof(<type or variable>) // size in bytes")
     ("void" summary "Built in typeless type: void")
     ("char" summary "Integral Character Type: (0 to 256)")
     ("wchar_t" summary "Wide Character Type")
     ("short" summary "Integral Primitive Type: (-32768 to 32767)")
     ("int" summary "Integral Primitive Type: (-2147483648 to 2147483647)")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("_P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ("__P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ))
  "Some keywords used in C.")


;;; Override methods & Variables
;;
(defcustom semantic-default-c-path '("/usr/include" "/usr/dt/include"
					 "/usr/X11R6/include")
  "Default set of include paths for C code.
Used by `semantic-inc' to define an include path.  This should
probably do some sort of search to see what is actually on the local
machine."
  :group 'c
  :type '(repeat (string :tag "Path")))

(defun semantic-c-nonterminal-protection (token &optional parent)
  "Return the protection of TOKEN in PARENT.
Override function for `semantic-nonterminal-protection'."
  (let ((mods (semantic-token-type-modifiers token))
	(prot nil))
    ;; Check the modifiers for protection if we are not a child
    ;; of some class type.
    (when (or (not parent) (not (eq (semantic-token-token parent) 'type)))
      (while (and (not prot) mods)
	(if (stringp (car mods))
	    (let ((s (car mods)))
	      ;; A few silly defaults to get things started.
	      (cond ((or (string= s "extern")
			 (string= s "export"))
		     'public)
		    ((string= s "static")
		     'private))))
	(setq mods (cdr mods))))
    ;; If we have a typed parent, look for :public style labels.
    (when (and parent (eq (semantic-token-token parent) 'type))
      (let ((pp (semantic-token-type-parts parent)))
	(while (and pp (not (eq (car pp) token)))
	  (when (eq (semantic-token-token (car pp)) 'label)
	    (setq prot
		  (cond ((string= (semantic-token-name (car pp)) "public")
			 'public)
			((string= (semantic-token-name (car pp)) "private")
			 'private)
			((string= (semantic-token-name (car pp)) "protected")
			 'protected)))
	    )
	  (setq pp (cdr pp)))))
    (when (and (not prot) (eq (semantic-token-token parent) 'type))
      (setq prot
	    (cond ((string= (semantic-token-type parent) "class") 'private)
		  ((string= (semantic-token-type parent) "struct") 'public)
		  (t 'unknown))))
    (or prot 'public)))

(defun semantic-c-nonterminal-children (token)
  "Return children for the C token TOKEN."
  (if (and (eq (semantic-token-token token) 'type)
	   (string= (semantic-token-type token) "typedef"))
      ;; A typedef can contain a parent who has positional children,
      ;; but that parent will not have a position.  Do this funny hack
      ;; to make sure we can apply overlays properly.
      (semantic-nonterminal-children (semantic-token-type-parent token))
    (semantic-nonterminal-children-default token)))

(defun semantic-c-nonterminal-template (token)
  "Return the template specification for TOKEN, or nil."
  (semantic-token-extra-spec token 'template))

(defun semantic-c-nonterminal-template-specifier (token)
  "Return the template specifier specification for TOKEN, or nil."
  (semantic-token-extra-spec token 'template-specifier))

(defun semantic-c-template-string-body (templatespec)
  "Convert TEMPLATESPEC into a string.
This might be a string, or a list of tokens."
  (cond ((stringp templatespec)
	 templatespec)
	((semantic-token-p templatespec)
	 (semantic-abbreviate-nonterminal templatespec))
	((listp templatespec)
	 (mapconcat 'semantic-abbreviate-nonterminal templatespec ", "))))

(defun semantic-c-template-string (token &optional parent color)
  "Return a string representing the TEMPLATE attribute of TOKEN.
This string is prefixed with a space, or is the empty string.
Argument PARENT specifies a parent type.
Argument COLOR specifies that the string should be colorized."
  (let ((t2 (semantic-c-nonterminal-template-specifier token))
	(t1 (semantic-c-nonterminal-template token))
	(pt1 (if parent (semantic-c-nonterminal-template parent)))
	(pt2 (if parent (semantic-c-nonterminal-template-specifier parent)))
	)
    (cond (t2 ;; we have a template with specifier
	   (concat " <"
		   ;; Fill in the parts here
		   (semantic-c-template-string-body t2)
		   ">"))
	  (t1 ;; we have a template without specifier
	   " <>")
	  (t
	   ""))))

(defun semantic-c-concise-prototype-nonterminal (token &optional parent color)
  "Return an abbreviated string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-abbreviate-nonterminal-default'."
  ;; If we have special template things, append.
  (concat  (semantic-concise-prototype-nonterminal-default token parent color)
	   (semantic-c-template-string token parent color)))

(defun semantic-c-uml-prototype-nonterminal (token &optional parent color)
  "Return an uml string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-abbreviate-nonterminal-default'."
  ;; If we have special template things, append.
  (concat  (semantic-uml-prototype-nonterminal-default token parent color)
	   (semantic-c-template-string token parent color)))

(defun semantic-c-nonterminal-abstract (token &optional parent)
  "Return non-nil if TOKEN is considered abstract.
PARENT is token's parent.
In C, a method is abstract if it is `virtual', which is already
handled.  A class is abstract iff it's destructor is virtual."
  (cond
   ((eq (semantic-token-token token) 'type)
    (or (semantic-find-nonterminal-by-extra-spec 'pure-virtual
						 (semantic-nonterminal-children token)
						 nil nil)
	(let* ((ds (semantic-find-nonterminal-by-extra-spec
		    'destructor
		    (semantic-nonterminal-children token)
		    nil nil))
	       (cs (semantic-find-nonterminal-by-extra-spec
		    'constructor
		    (semantic-nonterminal-children token)
		    nil nil)))
	  (and ds (member "virtual" (semantic-token-modifiers (car ds)))
	       cs (eq 'protected (semantic-nonterminal-protection cs token))
	       )
	  )))
   ((eq (semantic-token-token token) 'function)
    (semantic-token-extra-spec token 'pure-virtual))
   (t (semantic-nonterminal-abstract-default token parent))))

(defun semantic-c-analyze-dereference-metatype (type)
  "Dereference TYPE as described in `semantic-analyze-dereference-metatype'.
If TYPE is a typedef, get TYPE's type by name or token, and return."
  (if (and (eq (semantic-token-token type) 'type)
	   (string= (semantic-token-type type) "typedef"))
      (semantic-token-type-extra-spec type 'typedef)
    type))

(defun semantic-c-analyze-type-constants (type)
  "When TYPE is a token for an enum, return it's parts.
These are constants which are of type TYPE."
  (if (and (eq (semantic-token-token type) 'type)
	   (string= (semantic-token-type type) "enum"))
      (semantic-token-type-parts type)))

;;;###autoload
(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (semantic-install-function-overrides
   '((nonterminal-protection . semantic-c-nonterminal-protection)
     (nonterminal-children . semantic-c-nonterminal-children)
     (concise-prototype-nonterminal . semantic-c-concise-prototype-nonterminal)
     (uml-prototype-nonterminal . semantic-c-uml-prototype-nonterminal)
     (nonterminal-abstract . semantic-c-nonterminal-abstract)
     (analyze-dereference-metatype . semantic-c-analyze-dereference-metatype)
     (analyze-type-constants . semantic-c-analyze-type-constants)
     ))
  ;; Code generated from c.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-c-bovine-table
        semantic-toplevel-bovine-table-source "c.bnf")
  (setq semantic-flex-keywords-obarray semantic-c-keyword-table)
  (setq semantic-equivalent-major-modes '(c-mode c++-mode))
  (setq semantic-expand-nonterminal 'semantic-expand-c-nonterminal
        semantic-flex-extensions semantic-flex-c-extensions
        semantic-dependency-include-path semantic-default-c-path
        semantic-orphaned-member-metaparent-type "struct"
        semantic-symbol->name-assoc-list
        '((type     . "Types")
	  (variable . "Variables")
	  (function . "Functions")
	  (include  . "Includes"))
        semantic-symbol->name-assoc-list-for-type-parts
        '((type     . "Types")
	  (variable . "Attributes")
	  (function . "Methods")
	  (label    . "Labels")
	  )
        imenu-create-index-function 'semantic-create-imenu-index
        semantic-type-relation-separator-character '("." "->")
        semantic-command-separation-character ";"
        document-comment-start "/*"
        document-comment-line-prefix " *"
        document-comment-end " */"
        ;; Semantic navigation inside 'type children
        senator-step-at-token-ids '(function variable)
        )
 
 ;; End code generated from c.bnf
)

(add-hook 'c-mode-hook 'semantic-default-c-setup)
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(provide 'semantic-c)

;;; semantic-c.el ends here
