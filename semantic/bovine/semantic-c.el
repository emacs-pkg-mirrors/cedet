;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.23 2003/08/01 17:37:22 zappo Exp $

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
  ;;DO NOT EDIT! Generated from c.by - 2003-08-01 13:34-0400
  `(
    (bovine-toplevel ;;declaration
     (macro)
     (type)
     (define)
     (var-or-fun)
     (extern-c)
     (template)
     (using)
     ) ;; end declaration

    (bovine-inner-scope	;;codeblock
     (define)
     (codeblock-var-or-fun)
     (type)
     ) ;; end codeblock

    (extern-c-contents
     (open-paren
      ,(semantic-lambda
	(list nil))
      )
     (bovine-toplevel)
     (close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end extern-c-contents

    (extern-c
     (EXTERN
      string
      "\"C\""
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 "C"
	 'extern :members
	 (semantic-parse-region
	  (car
	   (nth 2 vals))
	  (cdr
	   (nth 2 vals))
	  'extern-c-contents
	  1)))
      )
     (EXTERN
      string
      "\"C\""
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end extern-c

    (macro-expression-list
     (expression
      macro-expression-list
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list nil))
      )
     (expression
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end macro-expression-list

    (macro-def
     (macro-expression-list
      ,(semantic-lambda
	(list nil))
      )
     (expression
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end macro-def

    (macro
     (punctuation
      "\\b[#]\\b"
      macro-or-include
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end macro

    (macro-or-include
     (DEFINE
       symbol
       opt-define-arglist
       macro-def
       ,(semantic-lambda
	 (semantic-tag-new-variable
	  (nth 1 vals) nil
	  (nth 3 vals)
	  'const t))
       )
     (INCLUDE
      system-include
      ,(semantic-lambda
	(semantic-tag-new-include
	 (substring
	  (nth 1 vals)
	  1
	  (1-
	   (length
	    (nth 1 vals)))) t))
      )
     (INCLUDE
      string
      ,(semantic-lambda
	(semantic-tag-new-include
	 (read
	  (nth 1 vals)) nil))
      )
     ) ;; end macro-or-include

    (opt-define-arglist
     (semantic-list
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-define-arglist

    (define
      (punctuation
       "\\b[#]\\b"
       DEFINE
       symbol
       opt-define-arglist
       macro-def
       ,(semantic-lambda
	 (semantic-tag-new-variable
	  (nth 2 vals) nil
	  (nth 3 vals)
	  'const t))
       )
      )	;; end define

    (unionparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'classsubparts
	 1))
      )
     ) ;; end unionparts

    (opt-symbol
     (symbol)
     ( ;;EMPTY
      )
     ) ;; end opt-symbol

    (classsubparts
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (class-protection
      opt-symbol
      punctuation
      "\\b[:]\\b"
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 0 vals))
	 'label))
      )
     (var-or-fun)
     (type)
     (define)
     (template)
     ( ;;EMPTY
      )
     ) ;; end classsubparts

    (opt-class-parents
     (punctuation
      "\\b[:]\\b"
      class-parents
      opt-template-specifier
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-class-parents

    (class-parents
     (opt-class-protection
      opt-class-declmods
      namespace-symbol
      punctuation
      "\\b[,]\\b"
      class-parents
      ,(semantic-lambda
	(cons
	 (nth 2 vals)
	 (nth 4 vals)))
      )
     (opt-class-protection
      opt-class-declmods
      namespace-symbol
      ,(semantic-lambda
	(nth 2 vals))
      )
     ) ;; end class-parents

    (opt-class-declmods
     (class-declmods
      opt-class-declmods
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-class-declmods

    (class-declmods
     (VIRTUAL)
     ) ;; end class-declmods

    (class-protection
     (PUBLIC)
     (PRIVATE)
     (PROTECTED)
     ) ;; end class-protection

    (opt-class-protection
     (class-protection
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-class-protection

    (namespaceparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'namespacesubparts
	 1))
      )
     ) ;; end namespaceparts

    (namespacesubparts
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (type)
     (var-or-fun)
     (define)
     (class-protection
      punctuation
      "\\b[:]\\b"
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (nth 0 vals))
	 'label))
      )
     (template)
     (using)
     ( ;;EMPTY
      )
     ) ;; end namespacesubparts

    (enumparts
     (semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'enumsubparts
	 1))
      )
     ) ;; end enumparts

    (enumsubparts
     (symbol
      opt-assign
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals)
	 "int"
	 (car
	  (nth 1 vals))
	 'const t))
      )
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\b[,]\\b"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end enumsubparts

    (opt-name
     (symbol)
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 ""))
      )
     ) ;; end opt-name

    (opt-class-declmods
     (symbol
      declespec
      semantic-list)
     (symbol)
     ( ;;EMPTY
      )
     ) ;; end opt-class-declmods

    (typesimple
     (struct-or-class
      opt-class
      opt-name
      opt-template-specifier
      opt-class-parents
      semantic-list
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (car
	  (nth 0 vals))
	 (let
	     (
	      (semantic-c-classname
	       (cons
		(car
		 (nth 2 vals))
		(car
		 (nth 0 vals)))))
	   (semantic-parse-region
	    (car
	     (nth 5 vals))
	    (cdr
	     (nth 5 vals))
	    'classsubparts
	    1))
	 (nth 4 vals)
	 'template-specifier
	 (nth 3 vals)
	 'parent
	 (car
	  (nth 1 vals))))
      )
     (struct-or-class
      opt-class
      opt-name
      opt-template-specifier
      opt-class-parents
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (car
	  (nth 0 vals)) nil
	 (nth 4 vals)
	 'template-specifier
	 (nth 3 vals)
	 'parent
	 (car
	  (nth 1 vals))))
      )
     (UNION
      opt-class
      opt-name
      unionparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (nth 0 vals)
	 (nth 3 vals) nil
	 'parent
	 (car
	  (nth 1 vals))))
      )
     (ENUM
      opt-class
      opt-name
      enumparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 2 vals))
	 (nth 0 vals)
	 (nth 3 vals) nil
	 'parent
	 (car
	  (nth 1 vals))))
      )
     (TYPEDEF
      declmods
      typeformbase
      cv-declmods
      typedef-symbol-list
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 4 vals)
	 (nth 0 vals) nil
	 (nth 2 vals)))
      )
     ) ;; end typesimple

    (typedef-symbol-list
     (typedefname
      punctuation
      "\\b[,]\\b"
      typedef-symbol-list
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (typedefname
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end typedef-symbol-list

    (typedefname
     (opt-stars
      symbol
      opt-bits
      opt-array
      ,(semantic-lambda
	(list
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ) ;; end typedefname

    (struct-or-class
     (STRUCT)
     (CLASS)
     ) ;; end struct-or-class

    (type
     (typesimple
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(nth 0 vals))
      )
     (NAMESPACE
      symbol
      namespaceparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals)
	 (nth 2 vals) nil))
      )
     (NAMESPACE
      namespaceparts
      ,(semantic-lambda
	(semantic-tag-new-type
	 "unnamed"
	 (nth 0 vals)
	 (nth 1 vals) nil))
      )
     ) ;; end type

    (using
     (USING
      typeformbase
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list nil))
      )
     (USING
      NAMESPACE
      typeformbase
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end using

    (template
     (TEMPLATE
      template-specifier
      opt-friend
      template-definition
      ,(semantic-lambda
	(semantic-c-reconstitute-template
	 (nth 3 vals)
	 (nth 1 vals)))
      )
     ) ;; end template

    (opt-friend
     (FRIEND)
     ( ;;EMPTY
      )
     ) ;; end opt-friend

    (opt-template-specifier
     (template-specifier
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-template-specifier

    (template-specifier
     (punctuation
      "\\b[<]\\b"
      template-specifier-types
      punctuation
      "\\b[>]\\b"
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end template-specifier

    (template-specifier-types
     (template-var
      template-specifier-type-list
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      )
     ) ;; end template-specifier-types

    (template-specifier-type-list
     (punctuation
      "\\b[,]\\b"
      template-specifier-types
      ,(semantic-lambda
	(nth 1 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end template-specifier-type-list

    (template-var
     (template-type
      opt-template-equal
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))))
      )
     (string
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (number
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (opt-stars
      opt-ref
      namespace-symbol
      ,(semantic-lambda
	(nth 2 vals))
      )
     ) ;; end template-var

    (opt-template-equal
     (punctuation
      "\\b[=]\\b"
      symbol
      punctuation
      "\\b[<]\\b"
      template-specifier-types
      punctuation
      "\\b[>]\\b"
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-template-equal

    (template-type
     (CLASS
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "class" nil nil))
      )
     (STRUCT
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "struct" nil nil))
      )
     (TYPENAME
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "class" nil nil))
      )
     (declmods
      typeformbase
      cv-declmods
      opt-stars
      opt-ref
      variablearg-opt-name
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 1 vals)) nil nil nil
	 'const
	 (if
	     (member
	      "const"
	      (append
	       (nth 0 vals)
	       (nth 2 vals))) t nil)
	 'typemodifiers
	 (delete
	  "const"
	  (append
	   (nth 0 vals)
	   (nth 2 vals)))
	 'reference
	 (car
	  (nth 4 vals))
	 'pointer
	 (car
	  (nth 3 vals))))
      )
     ) ;; end template-type

    (template-definition
     (type
      ,(semantic-lambda
	(nth 0 vals))
      )
     (var-or-fun
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end template-definition

    (opt-stars
     (punctuation
      "\\b[*]\\b"
      opt-starmod
      opt-stars
      ,(semantic-lambda
	(list
	 (1+
	  (car
	   (nth 2 vals)))))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 0))
      )
     ) ;; end opt-stars

    (opt-starmod
     (STARMOD
      opt-starmod
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end opt-starmod

    (STARMOD
     (CONST)
     ) ;; end STARMOD

    (declmods
     (DECLMOD
      declmods
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     (DECLMOD
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end declmods

    (DECLMOD
     (EXTERN)
     (STATIC)
     (CVDECLMOD)
     (INLINE)
     (REGISTER)
     (FRIEND)
     (TYPENAME)
     (METADECLMOD)
     (VIRTUAL)
     ) ;; end DECLMOD

    (metadeclmod
     (METADECLMOD
      ,(semantic-lambda)
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end metadeclmod

    (CVDECLMOD
     (CONST)
     (VOLATILE)
     ) ;; end CVDECLMOD

    (cv-declmods
     (CVDECLMOD
      cv-declmods
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 1 vals)))
      )
     (CVDECLMOD
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end cv-declmods

    (METADECLMOD
     (VIRTUAL)
     (MUTABLE)
     ) ;; end METADECLMOD

    (opt-ref
     (punctuation
      "\\b[&]\\b"
      ,(semantic-lambda
	(list
	 1))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list
	 0))
      )
     ) ;; end opt-ref

    (typeformbase
     (typesimple
      ,(semantic-lambda
	(nth 0 vals))
      )
     (STRUCT
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (UNION
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (ENUM
      symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 (nth 0 vals) nil nil))
      )
     (builtintype
      ,(semantic-lambda
	(nth 0 vals))
      )
     (namespace-symbol
      ,(semantic-lambda
	(semantic-tag-new-type
	 (car
	  (nth 0 vals))
	 "class" nil nil))
      )
     (symbol
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end typeformbase

    (signedmod
     (UNSIGNED)
     (SIGNED)
     ) ;; end signedmod

    (builtintype-types
     (VOID)
     (CHAR)
     (WCHAR)
     (SHORT
      INT
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (SHORT)
     (INT)
     (LONG
      INT
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (FLOAT)
     (DOUBLE)
     (LONG
      DOUBLE
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (LONG
      LONG
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  " "
	  (nth 1 vals))))
      )
     (LONG)
     ) ;; end builtintype-types

    (builtintype
     (signedmod
      builtintype-types
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  " "
	  (car
	   (nth 1 vals)))))
      )
     (builtintype-types
      ,(semantic-lambda
	(nth 0 vals))
      )
     (signedmod
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  " int")))
      )
     ) ;; end builtintype

    (codeblock-var-or-fun
     (declmods
      typeformbase
      declmods
      opt-ref
      var-or-func-decl
      ,(semantic-lambda
	(semantic-c-reconstitute-token
	 (nth 4 vals)
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ) ;; end codeblock-var-or-fun

    (var-or-fun
     (codeblock-var-or-fun
      ,(semantic-lambda
	(nth 0 vals))
      )
     (declmods
      var-or-func-decl
      ,(semantic-lambda
	(semantic-c-reconstitute-token
	 (nth 1 vals)
	 (nth 0 vals) nil))
      )
     ) ;; end var-or-fun

    (var-or-func-decl
     (func-decl
      ,(semantic-lambda
	(nth 0 vals))
      )
     (var-decl
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end var-or-func-decl

    (func-decl
     (opt-stars
      opt-class
      opt-destructor
      functionname
      opt-template-specifier
      opt-under-p
      arg-list
      opt-post-fcn-modifiers
      opt-throw
      opt-initializers
      fun-or-proto-end
      ,(semantic-lambda
	(nth 3 vals)
	(list
	 'function
	 (nth 1 vals)
	 (nth 2 vals)
	 (nth 6 vals)
	 (nth 8 vals)
	 (nth 7 vals))
	(nth 0 vals)
	(nth 10 vals))
      )
     ) ;; end func-decl

    (var-decl
     (varnamelist
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list
	 (nth 0 vals)
	 'variable))
      )
     ) ;; end var-decl

    (opt-under-p
     (UNDERP
      ,(semantic-lambda
	(list nil))
      )
     (UNDERUNDERP
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-under-p

    (opt-initializers
     (punctuation
      "\\b[:]\\b"
      namespace-symbol
      semantic-list
      opt-initializers)
     (punctuation
      "\\b[,]\\b"
      namespace-symbol
      semantic-list
      opt-initializers)
     ( ;;EMPTY
      )
     ) ;; end opt-initializers

    (opt-post-fcn-modifiers
     (post-fcn-modifiers
      opt-post-fcn-modifiers
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-post-fcn-modifiers

    (post-fcn-modifiers
     (REENTRANT)
     (CONST)
     ) ;; end post-fcn-modifiers

    (opt-throw
     (THROW
      semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 1 vals))
	  (cdr
	   (nth 1 vals))
	  'throw-exception-list))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-throw

    (throw-exception-list
     (namespace-symbol
      punctuation
      "\\b[,]\\b"
      throw-exception-list
      ,(semantic-lambda
	(cons
	 (car
	  (nth 0 vals))
	 (nth 2 vals)))
      )
     (namespace-symbol
      close-paren
      ")"
      ,(semantic-lambda
	(nth 0 vals))
      )
     (symbol
      close-paren
      ")"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (open-paren
      "("
      throw-exception-list
      ,(semantic-lambda
	(nth 1 vals))
      )
     (close-paren
      ")"
      ,(semantic-lambda)
      )
     ) ;; end throw-exception-list

    (opt-bits
     (punctuation
      "\\b[:]\\b"
      number
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-bits

    (opt-array
     (semantic-list
      "\\[.*\\]$"
      opt-array
      ,(semantic-lambda
	(list
	 (cons
	  1
	  (car
	   (nth 1 vals)))))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-array

    (opt-assign
     (punctuation
      "\\b[=]\\b"
      expression
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-assign

    (opt-restrict
     (symbol
      "\\<\\(__\\)?restrict\\>")
     ( ;;EMPTY
      )
     ) ;; end opt-restrict

    (varname
     (opt-stars
      opt-restrict
      namespace-symbol
      opt-bits
      opt-array
      opt-assign
      ,(semantic-lambda
	(nth 2 vals)
	(nth 0 vals)
	(nth 3 vals)
	(nth 4 vals)
	(nth 5 vals))
      )
     ) ;; end varname

    (variablearg
     (declmods
      typeformbase
      cv-declmods
      opt-ref
      variablearg-opt-name
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (list
	  (nth 4 vals))
	 (nth 1 vals) nil
	 'const
	 (if
	     (member
	      "const"
	      (append
	       (nth 0 vals)
	       (nth 2 vals))) t nil)
	 'typemodifiers
	 (delete
	  "const"
	  (append
	   (nth 0 vals)
	   (nth 2 vals)))
	 'reference
	 (car
	  (nth 3 vals))))
      )
     ) ;; end variablearg

    (variablearg-opt-name
     (varname
      ,(semantic-lambda
	(nth 0 vals))
      )
     (opt-stars
      ,(semantic-lambda
	(list
	 "")
	(nth 0 vals)
	(list nil nil nil))
      )
     ) ;; end variablearg-opt-name

    (varnamelist
     (varname
      punctuation
      "\\b[,]\\b"
      varnamelist
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (varname
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end varnamelist

    (namespace-symbol
     (symbol
      opt-template-specifier
      punctuation
      "\\b[:]\\b"
      punctuation
      "\\b[:]\\b"
      namespace-symbol
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  "::"
	  (car
	   (nth 4 vals)))))
      )
     (symbol
      opt-template-specifier
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end namespace-symbol

    (namespace-opt-class
     (symbol
      punctuation
      "\\b[:]\\b"
      punctuation
      "\\b[:]\\b"
      namespace-opt-class
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  "::"
	  (car
	   (nth 3 vals)))))
      )
     (symbol
      opt-template-specifier
      punctuation
      "\\b[:]\\b"
      punctuation
      "\\b[:]\\b"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end namespace-opt-class

    (opt-class
     (namespace-opt-class
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-class

    (opt-destructor
     (punctuation
      "\\b[~]\\b"
      ,(semantic-lambda
	(list t))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end opt-destructor

    (arg-list
     (semantic-list
      "^("
      knr-arguments
      ,(semantic-lambda
	(nth 1 vals))
      )
     (semantic-list
      "^("
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 0 vals))
	 (cdr
	  (nth 0 vals))
	 'arg-sub-list
	 1))
      )
     (semantic-list
      "^(void)$"
      ,(semantic-lambda)
      )
     ) ;; end arg-list

    (knr-arguments
     (variablearg
      punctuation
      "\\b[;]\\b"
      knr-arguments
      ,(semantic-lambda
	(cons
	 (car
	  (semantic-expand-c-tag
	   (nth 0 vals)))
	 (nth 2 vals)))
      )
     (variablearg
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list
	 (car
	  (semantic-expand-c-tag
	   (nth 0 vals)))))
      )
     ) ;; end knr-arguments

    (arg-sub-list
     (variablearg
      ,(semantic-lambda
	(nth 0 vals))
      )
     (punctuation
      "\\b[.]\\b"
      punctuation
      "\\b[.]\\b"
      punctuation
      "\\b[.]\\b"
      close-paren
      ")"
      ,(semantic-lambda
	(semantic-tag-new-variable
	 "..."
	 "vararg" nil))
      )
     (punctuation
      "\\b[,]\\b"
      ,(semantic-lambda
	(list nil))
      )
     (open-paren
      "("
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      ")"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end arg-sub-list

    (operatorsym
     (punctuation
      "\\b[<]\\b"
      punctuation
      "\\b[<]\\b"
      ,(semantic-lambda
	(list
	 "<<"))
      )
     (punctuation
      "\\b[>]\\b"
      punctuation
      "\\b[>]\\b"
      ,(semantic-lambda
	(list
	 ">>"))
      )
     (punctuation
      "\\b[=]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda
	(list
	 "=="))
      )
     (punctuation
      "\\b[<]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda
	(list
	 "<="))
      )
     (punctuation
      "\\b[>]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda
	(list
	 ">="))
      )
     (punctuation
      "\\b[!]\\b"
      punctuation
      "\\b[=]\\b"
      ,(semantic-lambda
	(list
	 "!="))
      )
     (punctuation
      "\\b[-]\\b"
      punctuation
      "\\b[>]\\b"
      ,(semantic-lambda
	(list
	 "->"))
      )
     (semantic-list
      "()"
      ,(semantic-lambda
	(list
	 "()"))
      )
     (semantic-list
      "\\[\\]"
      ,(semantic-lambda
	(list
	 "[]"))
      )
     (punctuation
      "\\b[<]\\b")
     (punctuation
      "\\b[>]\\b")
     (punctuation
      "\\b[*]\\b")
     (punctuation
      "\\b[+]\\b")
     (punctuation
      "\\b[-]\\b")
     (punctuation
      "\\b[/]\\b")
     (punctuation
      "\\b[=]\\b")
     (punctuation
      "\\b[!]\\b")
     ) ;; end operatorsym

    (functionname
     (OPERATOR
      operatorsym
      ,(semantic-lambda
	(nth 1 vals))
      )
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'function-pointer))
      )
     (symbol
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end functionname

    (function-pointer
     (open-paren
      "("
      punctuation
      "\\b[*]\\b"
      symbol
      close-paren
      ")"
      ,(semantic-lambda
	(list
	 (concat
	  "*"
	  (nth 2 vals))))
      )
     ) ;; end function-pointer

    (fun-or-proto-end
     (punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list t))
      )
     (semantic-list
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\b[=]\\b"
      number
      "^0$"
      punctuation
      "\\b[;]\\b"
      ,(semantic-lambda
	(list
	 'pure-virtual))
      )
     ) ;; end fun-or-proto-end

    (type-cast
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'type-cast-list))
      )
     ) ;; end type-cast

    (type-cast-list
     (open-paren
      typeformbase
      close-paren)
     ) ;; end type-cast-list

    (function-call
     (namespace-symbol
      semantic-list)
     ) ;; end function-call

    (string-seq
     (string
      string-seq
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  (car
	   (nth 1 vals)))))
      )
     (string
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end string-seq

    (expression
     (number
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (function-call
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (namespace-symbol
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (string-seq
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (type-cast
      expression
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (semantic-list
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     (punctuation
      "[-+*/%^|&]"
      expression
      ,(semantic-lambda
	(list
	 (identity start)
	 (identity end)))
      )
     ) ;; end expression
    )
  )

(define-lex-regex-analyzer semantic-lex-c-if-0
  "Block out code matched in an #if 0 condition."
  "^\\s-*#if\\s-*0$"
  (beginning-of-line)
  (c-forward-conditional 1)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer semantic-lex-c-if
  "Ignore various forms of #if/#else/#endif conditionals."
  "^#\\(if\\(def\\)?\\|el\\(if\\|se\\)\\|endif\\)"
  (when (bolp) (end-of-line))
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-analyzer semantic-lex-c-include-system
  "Identify system include strings, and return special tokens."
  (and (looking-at "<[^\n>]+>")
       (save-excursion
	 (beginning-of-line)
	 (looking-at "\\s-*#\\s-*include\\s-+<"))
       (= (match-end 0) (1+ (point))))
  ;; We found a system include.
  (let ((start (point)))
    ;; This should always pass
    (re-search-forward ">")
    ;; We have the whole thing.
    (semantic-lex-push-token
     (semantic-lex-token 'system-include start (point)))
    )
  )

(define-lex-regex-analyzer semantic-lex-c-ignore-ending-backslash
  "Skip backslash ending a line.
Go to the next line."
  "\\\\\\s-*\n"
  (setq semantic-lex-end-point (match-end 0)))

(define-lex-regex-analyzer semantic-lex-c-string
  "Detect and create a C string token."
  "L?\\(\\s\"\\)"
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'string (point)
    (save-excursion
      ;; Skip L prefix if present.
      (goto-char (match-beginning 1))
      (semantic-lex-unterminated-syntax-protection 'string
	(forward-sexp 1)
	(point))
      ))))

(define-lex semantic-c-lexer
  "Lexical Analyzer for C code."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-c-if-0
  semantic-lex-c-if
  semantic-lex-c-include-system
  semantic-lex-c-ignore-ending-backslash
  semantic-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  semantic-lex-c-string
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(defun semantic-expand-c-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil."
  (cond ((eq (semantic-tag-class tag) 'extern)
	 ;; We have hit an exter "C" command with a list after it.
	 (let* ((mb (semantic-tag-get-attribute tag :members))
		(ret mb))
	   (while mb
	     (let ((mods (semantic-tag-get-attribute (car mb) 'typemodifiers)))
	       (setq mods (cons "extern" (cons "\"C\"" mods)))
	       (semantic-tag-put-attribute (car mb) 'typemodifiers mods))
	     (setq mb (cdr mb)))
	   ret))
	((listp (car tag))
	 (cond ((eq (semantic-tag-class tag) 'variable)
		;; The name part comes back in the form of:
		;; ( NAME NUMSTARS BITS ARRAY ASSIGN )
		(let ((vl nil)
		      (basety (semantic-tag-type tag))
		      (ty "")
		      (mods (semantic-tag-get-attribute tag 'typemodifiers))
		      (suffix "")
		      (lst (semantic-tag-name tag))
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
			      (semantic-tag-new-variable
			       (car cur) ;name
			       ty	;type
			       (if default
				   (buffer-substring-no-properties
				    (car default) (car (cdr default))))
			       'const (semantic-tag-variable-constant-p tag)
			       'suffix suffix
			       'typemodifiers mods
			       'dereference (length (nth 3 cur))
			       'pointer (nth 1 cur)
			       :documentation (semantic-tag-docstring tag) ;doc
			       )
			      vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq lst (cdr lst)))
		  vl))
	       ((eq (semantic-tag-class tag) 'type)
		;; We may someday want to add an extra check for a type
		;; of type "typedef".
		;; Each elt of NAME is ( STARS NAME )
		(let ((vl nil)
		      (names (semantic-tag-name tag)))
		  (while names
		    (setq vl (cons (semantic-tag-new-type
				    (nth 1 (car names)) ; name
				    "typedef"
				    (semantic-tag-type-members tag)
				    ;; parent is just tbe name of what
				    ;; is passed down as a tag.
				    (list
				     (semantic-tag-name
				      (semantic-tag-type-superclasses tag)))
				    'pointer
				    (let ((stars (car (car (car names)))))
				      (if (= stars 0) nil stars))
				    ;; This specifies what the typedef
				    ;; is expanded out as.  Just the
				    ;; name shows up as a parent of this
				    ;; typedef.
				    'typedef
				    (semantic-token-type-parent tag)
				    :documentation
				    (semantic-tag-docstring tag))
				   vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq names (cdr names)))
		  vl))
	       ((and (listp (car tag))
		     (eq (semantic-tag-class (car tag)) 'variable))
		;; Argument lists come in this way.  Append all the expansions!
		(let ((vl nil))
		  (while tag
		    (setq vl (append (semantic-tag-components (car vl))
				     vl)
			  tag (cdr tag)))
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
  (when (and (listp typedecl)
	     (= 1 (length typedecl))
	     (stringp (car typedecl)))
    (setq typedecl (car typedecl)))
  (cond ((eq (nth 1 tokenpart) 'variable)
	 (semantic-tag-new-variable
	  (car tokenpart)
	  (or typedecl "int")	;type
	  nil			;default value (filled with expand)
	  'const (if (member "const" declmods) t nil)
	  'typemodifiers (delete "const" declmods)
	  )
	 )
	((eq (nth 1 tokenpart) 'function)
	 ;; We should look at part 4 (the arglist) here, and throw an
	 ;; error of some sort if it contains parser errors so that we
	 ;; don't parser function calls, but that is a little beyond what
	 ;; is available for data here.
	 (let* ((constructor
		 (and (or (and semantic-c-classname
			       (string= (car semantic-c-classname)
					(car tokenpart)))
			  (and (stringp (car (nth 2 tokenpart)))
			       (string= (car (nth 2 tokenpart)) (car tokenpart)))
			  )
		      (not (car (nth 3 tokenpart)))))
		(fcnpointer (string-match "^\\*" (car tokenpart)))
		(fnname (if fcnpointer
			    (substring (car tokenpart) 1)
			  (car tokenpart))))
	   (if fcnpointer
	       ;; Function pointers are really variables.
	       (semantic-tag-new-variable
		fnname
		typedecl
		nil
		;; It is a function pointer
		'functionpointer t
		)
	     ;; The function
	     (semantic-tag-new-function
	      fnname
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
	      (nth 4 tokenpart)		;arglist
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
	      )))
	 )
	))

(defun semantic-c-reconstitute-template (tag specifier)
  "Reconstitute the token TAG with the template SPECIFIER."
  (semantic-tag-put-attribute tag 'template (or specifier ""))
  tag)

(defvar semantic-c-keyword-table
  ;;DO NOT EDIT! Generated from c.by - 2003-08-01 13:34-0400
  (semantic-lex-make-keyword-table
   '(("include" . INCLUDE)
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
     ("__P" . UNDERUNDERP))
   '(("__P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ("_P" summary "Common macro to eliminate prototype compatibility on some compilers")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("int" summary "Integral Primitive Type: (-2147483648 to 2147483647)")
     ("short" summary "Integral Primitive Type: (-32768 to 32767)")
     ("wchar_t" summary "Wide Character Type")
     ("char" summary "Integral Character Type: (0 to 256)")
     ("void" summary "Built in typeless type: void")
     ("sizeof" summary "Compile time macro: sizeof(<type or variable>) // size in bytes")
     ("continue" summary "Non-local continue within a loop (for, do/while): continue;")
     ("break" summary "Non-local exit within a loop or switch (for, do/while, switch): break;")
     ("return" summary "return <value>;")
     ("default" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("case" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("switch" summary "switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("for" summary "for(<init>; <condition>; <increment>) { code }")
     ("while" summary "do { code } while (<condition>); or while (<condition>) { code };")
     ("do" summary " do { code } while (<condition>);")
     ("else" summary "if (<condition>) { code } [ else { code } ]")
     ("if" summary "if (<condition>) { code } [ else { code } ]")
     ("reentrant" summary "<type> <methoddef> (<method args>) reentrant ...")
     ("throw" summary "<type> <methoddef> (<method args>) throw (<exception>) ...")
     ("template" summary "template <class TYPE ...> TYPE_OR_FUNCTION")
     ("using" summary "using <namespace>;")
     ("namespace" summary "Namespace Declaration: namespace <name> { ... };")
     ("typename" summary "typename is used to handle a qualified name as a typename;")
     ("class" summary "Class Declaration: class <name>[:parents] { ... };")
     ("typedef" summary "Arbitrary Type Declaration: typedef <typedeclaration> <name>;")
     ("enum" summary "Enumeration Type Declaration: enum [name] { ... };")
     ("union" summary "Union Type Declaration: union [name] { ... };")
     ("struct" summary "Structure Type Declaration: struct [name] { ... };")
     ("mutable" summary "Member Declaration Modifier: mutable <type> <name> ...")
     ("virtual" summary "Method Modifier: virtual <type> <name>(...) ...")
     ("inline" summary "Function Modifier: inline <return  type> <name>(...) {...};")
     ("unsigned" summary "Numeric Type Modifier: unsigned <numeric type> <name> ...")
     ("signed" summary "Numeric Type Modifier: signed <numeric type> <name> ...")
     ("register" summary "Declaration Modifier: register <type> <name> ...")
     ("volatile" summary "Declaration Modifier: volatile <type> <name> ...")
     ("const" summary "Declaration Modifier: const <type> <name> ...")
     ("static" summary "Declaration Modifier: static <type> <name> ...")
     ("extern" summary "Declaration Modifier: extern <type> <name> ...")))
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

(defun semantic-c-format-tag-name (tag &optional parent color)
  "Convert TAG to a string that is the print name for TAG.
Optional PARENT and COLOR are ignored."
  (let ((name (semantic-format-tag-name-default tag parent color))
	(fnptr (semantic-tag-get-attribute tag 'functionpointer))
	)
    (if (not fnptr)
	name
      (concat "(*" name ")"))
    ))

(defun semantic-c-tag-protection (token &optional parent)
  "Return the protection of TOKEN in PARENT.
Override function for `semantic-tag-protection'."
  (let ((mods (semantic-tag-modifiers token))
	(prot nil))
    ;; Check the modifiers for protection if we are not a child
    ;; of some class type.
    (when (or (not parent) (not (eq (semantic-tag-class parent) 'type)))
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
    (when (and parent (eq (semantic-tag-class parent) 'type))
      (let ((pp (semantic-tag-type-members parent)))
	(while (and pp (not (eq (car pp) token)))
	  (when (eq (semantic-tag-class (car pp)) 'label)
	    (setq prot
		  (cond ((string= (semantic-tag-name (car pp)) "public")
			 'public)
			((string= (semantic-tag-name (car pp)) "private")
			 'private)
			((string= (semantic-tag-name (car pp)) "protected")
			 'protected)))
	    )
	  (setq pp (cdr pp)))))
    (when (and (not prot) (eq (semantic-tag-class parent) 'type))
      (setq prot
	    (cond ((string= (semantic-tag-type parent) "class") 'private)
		  ((string= (semantic-tag-type parent) "struct") 'public)
		  (t 'unknown))))
    (or prot
	(if parent
	    'public
	  nil))))

(defun semantic-c-tag-components (tag)
  "Return components for TAG."
  (if (and (eq (semantic-tag-class tag) 'type)
	   (string= (semantic-tag-type tag) "typedef"))
      ;; A typedef can contain a parent who has positional children,
      ;; but that parent will not have a position.  Do this funny hack
      ;; to make sure we can apply overlays properly.
      (semantic-tag-components (semantic-tag-type-superclasses tag))
    (semantic-tag-components-default tag)))

(defun semantic-c-format-tag-type (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
Adds pointer and reference symbols to the default.
Argument COLOR adds color to the text."
  (let* ((type (semantic-format-tag-type-default tag color))
	 (point (semantic-tag-get-attribute tag 'pointer))
	 (ref (semantic-tag-get-attribute tag 'reference))
	 )
    (if ref (setq ref "&"))
    (if point (setq point (make-string point ?*)) "")
    (when type
      (concat type ref point))
    ))

(defun semantic-c-tag-template (tag)
  "Return the template specification for TAG, or nil."
  (semantic-tag-get-attribute tag 'template))

(defun semantic-c-tag-template-specifier (tag)
  "Return the template specifier specification for TAG, or nil."
  (semantic-tag-get-attribute tag 'template-specifier))

(defun semantic-c-template-string-body (templatespec)
  "Convert TEMPLATESPEC into a string.
This might be a string, or a list of tokens."
  (cond ((stringp templatespec)
	 templatespec)
	((semantic-tag-p templatespec)
	 (semantic-format-tag-abbreviate templatespec))
	((listp templatespec)
	 (mapconcat 'semantic-format-tag-abbreviate templatespec ", "))))

(defun semantic-c-template-string (token &optional parent color)
  "Return a string representing the TEMPLATE attribute of TOKEN.
This string is prefixed with a space, or is the empty string.
Argument PARENT specifies a parent type.
Argument COLOR specifies that the string should be colorized."
  (let ((t2 (semantic-c-tag-template-specifier token))
	(t1 (semantic-c-tag-template token))
	(pt1 (if parent (semantic-c-tag-template parent)))
	(pt2 (if parent (semantic-c-tag-template-specifier parent)))
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

(defun semantic-c-format-tag-concise-prototype (token &optional parent color)
  "Return an abbreviated string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-format-tag-abbreviate-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-concise-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(defun semantic-c-format-tag-uml-prototype (token &optional parent color)
  "Return an uml string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-abbreviate-tag-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-uml-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(defun semantic-c-tag-abstract (tag &optional parent)
  "Return non-nil if TAG is considered abstract.
PARENT is tag's parent.
In C, a method is abstract if it is `virtual', which is already
handled.  A class is abstract iff it's destructor is virtual."
  (cond
   ((eq (semantic-tag-class tag) 'type)
    (or (semantic-brute-find-tag-by-attribute 'pure-virtual
					      (semantic-tag-components tag)
					      )
	(let* ((ds (semantic-brute-find-tag-by-attribute
		    'destructor
		    (semantic-tag-components tag)
		    ))
	       (cs (semantic-brute-find-tag-by-attribute
		    'constructor
		    (semantic-tag-components tag)
		    )))
	  (and ds (member "virtual" (semantic-tag-modifiers (car ds)))
	       cs (eq 'protected (semantic-tag-protection cs tag))
	       )
	  )))
   ((eq (semantic-tag-class tag) 'function)
    (semantic-tag-get-attribute tag 'pure-virtual))
   (t (semantic-tag-abstract-default tag parent))))

(defun semantic-c-analyze-dereference-metatype (type)
  "Dereference TYPE as described in `semantic-analyze-dereference-metatype'.
If TYPE is a typedef, get TYPE's type by name or tag, and return."
  (if (and (eq (semantic-tag-class type) 'type)
	   (string= (semantic-tag-type type) "typedef"))
      (semantic-tag-get-attribute type 'typedef)
    type))

(defun semantic-c-analyze-type-constants (type)
  "When TYPE is a tag for an enum, return it's parts.
These are constants which are of type TYPE."
  (if (and (eq (semantic-tag-class type) 'type)
	   (string= (semantic-tag-type type) "enum"))
      (semantic-tag-type-members type)))

;;;###autoload
(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  ;;DO NOT EDIT! Generated from c.by - 2003-08-01 13:34-0400
  (progn
    (setq semantic-toplevel-bovine-table semantic-toplevel-c-bovine-table
	  semantic-debug-parser-source "c.by"
	  semantic-debug-parser-class 'semantic-bovine-debug-parser
	  semantic-flex-keywords-obarray semantic-c-keyword-table
	  semantic-equivalent-major-modes '(c-mode c++-mode)
	  )
    (setq semantic-tag-expand-function 'semantic-expand-c-tag
	  semantic-dependency-include-path semantic-default-c-path
	  semantic-orphaned-member-metaparent-type "struct"
	  semantic-symbol->name-assoc-list
	  '((type     . "Types")
	    (variable . "Variables")
	    (function . "Functions")
	    (include  . "Includes")
	    )
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
	  ))
  (semantic-install-function-overrides
   '((tag-protection . semantic-c-tag-protection)
     (tag-components . semantic-c-tag-components)
     (format-tag-concise-prototype . semantic-c-format-tag-concise-prototype)
     (format-tag-uml-prototype . semantic-c-format-tag-uml-prototype)
     (format-tag-type . semantic-c-format-tag-type)
     (tag-abstract . semantic-c-tag-abstract)
     (analyze-dereference-metatype . semantic-c-analyze-dereference-metatype)
     (analyze-type-constants . semantic-c-analyze-type-constants)
     (format-tag-name . semantic-c-format-tag-name)
     )
   t ;; Set as t for now while developing.
   )
  (setq semantic-lex-analyzer #'semantic-c-lexer))

;;;###autoload
(add-hook 'c-mode-hook 'semantic-default-c-setup)
;;;###autoload
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(provide 'semantic-c)

;;; semantic-c.el ends here
