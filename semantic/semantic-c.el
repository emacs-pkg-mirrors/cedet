;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.22 2001/04/07 14:28:54 zappo Exp $

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
;; Use the semantic bovinator in a couple languages as examples.
;;
;; <Add more here>

;;; History:
;; 

(require 'semantic)
(require 'backquote)

;;; Code:
(defvar semantic-toplevel-c-bovine-table
`((bovine-toplevel
 ( macro)
 ( type)
 ( var-or-fun)
 ( define)
 ) ; end declaration
 (bovine-inner-scope
 ( define)
 ( var-or-fun)
 ( type)
 ) ; end codeblock
 (macro
 ( punctuation "\\b#\\b" macro-or-include
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end macro
 (macro-or-include
 ( DEFINE symbol opt-expression
  ,(semantic-lambda
  (list (nth 1 vals) 'variable nil (nth 2 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ( INCLUDE punctuation "\\b<\\b" filename punctuation "\\b>\\b"
  ,(semantic-lambda
  (nth 2 vals) (list 'include t nil)))
 ( INCLUDE string
  ,(semantic-lambda
  (list ( read (nth 1 vals)) 'include nil nil)))
 ) ; end macro-or-include
 (define
 ( punctuation "\\b#\\b" DEFINE symbol opt-expression
  ,(semantic-lambda
  (list (nth 1 vals) 'variable nil (nth 2 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ) ; end define
 (filename
 ( symbol punctuation "\\b\\.\\b" symbol
  ,(semantic-lambda
  (list ( concat (nth 0 vals) (nth 1 vals) (nth 2 vals)))))
 ( symbol punctuation "\\b/\\b" filename
  ,(semantic-lambda
  (list ( concat (nth 0 vals) (nth 1 vals) ( car (nth 2 vals))))))
 ) ; end filename
 (structparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'structsubparts)
 ))
 ) ; end structparts
 (structsubparts
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( variable)
 ( define)
 ) ; end structsubparts
 (classparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'classsubparts)
 ))
 ) ; end classparts
 (classsubparts
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( var-or-fun)
 ( define)
 ( opt-class-protection punctuation "\\b:\\b"
  ,(semantic-lambda
  (nth 0 vals) (list 'protection)))
 ()
 ) ; end classsubparts
 (opt-class-parents
 ( punctuation "\\b:\\b" opt-class-protection symbol
  ,(semantic-lambda
  (list (nth 2 vals))))
 (
  ,(semantic-lambda
 ))
 ) ; end opt-class-parents
 (opt-class-protection
 ( PUBLIC)
 ( PRIVATE)
 ( PROTECTED)
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
 ( define)
 ( opt-class-protection punctuation "\\b:\\b"
  ,(semantic-lambda
  (list (nth 0 vals) 'protection)))
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
  (list (nth 0 vals) 'enum)))
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ) ; end enumsubparts
 (opt-name
 ( symbol)
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-name
 (typesimple
 ( STRUCT opt-name structparts
  ,(semantic-lambda
  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( UNION opt-name structparts
  ,(semantic-lambda
  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( ENUM opt-name enumparts
  ,(semantic-lambda
  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ( CLASS symbol opt-class-parents classparts
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals) (nth 3 vals)) (nth 2 vals) (list nil nil)))
 ( TYPEDEF typeform symbol
  ,(semantic-lambda
  (list (nth 2 vals) 'type (nth 0 vals) nil (nth 1 vals) nil nil)))
 ( NAMESPACE symbol namespaceparts
  ,(semantic-lambda
  (list (nth 1 vals) 'type (nth 0 vals) (nth 2 vals) nil nil nil)))
 ) ; end typesimple
 (type
 ( typesimple punctuation "\\b;\\b"
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end type
 (opt-stars
 ( punctuation "\\b\\*\\b" opt-stars
  ,(semantic-lambda
  (list ( 1+ ( car (nth 1 vals))))))
 (
  ,(semantic-lambda
  (list 0)))
 ) ; end opt-stars
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
 ( CONST)
 ( VOLATILE)
 ( SIGNED)
 ( UNSIGNED)
 ( VIRTUAL)
 ) ; end DECLMOD
 (typeform
 ( typeformbase opt-stars opt-ref
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end typeform
 (opt-ref
 ( punctuation "\\b&\\b")
 ()
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
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end typeformbase
 (var-or-fun
 ( declmods typeform var-or-func-decl
  ,(semantic-lambda
  ( semantic-c-reconstitute-token (nth 2 vals) (nth 0 vals) (nth 1 vals))))
 ( declmods var-or-func-decl
  ,(semantic-lambda
  ( semantic-c-reconstitute-token (nth 1 vals) (nth 0 vals) nil)))
 ) ; end var-or-fun
 (var-or-func-decl
 ( opt-class opt-destructor functionname arg-list fun-or-proto-end
  ,(semantic-lambda
  (nth 2 vals) (list 'function (nth 0 vals) (nth 1 vals) (nth 3 vals))))
 ( varnamelist punctuation "\\b;\\b"
  ,(semantic-lambda
  (list (nth 0 vals) 'variable)))
 ) ; end var-or-func-decl
 (opt-bits
 ( punctuation "\\b:\\b" symbol
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
 ( opt-stars opt-restrict symbol opt-bits opt-array opt-assign
  ,(semantic-lambda
  (list (nth 2 vals)) (nth 0 vals) (nth 3 vals) (nth 4 vals) (nth 5 vals)))
 ) ; end varname
 (variablearg
 ( declmods typeform varname
  ,(semantic-lambda
  (list ( car (nth 2 vals)) 'variable (nth 1 vals) nil ( semantic-bovinate-make-assoc-list 'const ( if ( member "const" (nth 0 vals)) t nil) 'typemodifiers ( delete "const" (nth 0 vals))) nil)))
 ) ; end variablearg
 (varnamelist
 ( varname punctuation "\\b,\\b" varnamelist
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 2 vals))))
 ( varname
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end varnamelist
 (opt-class
 ( symbol punctuation "\\b:\\b" punctuation "\\b:\\b"
  ,(semantic-lambda
  (list (nth 0 vals))))
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
 ( symbol "\\<__?P\\>" semantic-list
 ,(lambda (vals start end)
 
 (semantic-bovinate-from-nonterminal (car (nth 1 vals)) (cdr (nth 1 vals)) 'arg-list-p)
 ))
 ( semantic-list "^(" knr-arguments
  ,(semantic-lambda
  (nth 1 vals)))
 ( semantic-list "^("
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'arg-sub-list)
 ))
 ) ; end arg-list
 (knr-arguments
 ( variablearg punctuation "\\b;\\b" knr-arguments
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 2 vals))))
 ( variablearg punctuation "\\b;\\b"
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end knr-arguments
 (arg-list-p
 ( open-paren "(" semantic-list close-paren ")"
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 1 vals)) (cdr (nth 1 vals)) 'arg-sub-list)
 ))
 ) ; end arg-list-p
 (arg-sub-list
 ( variablearg
  ,(semantic-lambda
  (nth 0 vals)))
 ( punctuation "\\b\\.\\b" punctuation "\\b\\.\\b" punctuation "\\b\\.\\b" close-paren ")"
  ,(semantic-lambda
  (list "...")))
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
 ( punctuation "\\b<\\b")
 ( punctuation "\\b>\\b")
 ( punctuation "\\b\\*\\b")
 ( punctuation "\\b\\+\\b")
 ( punctuation "\\b-\\b")
 ( punctuation "\\b/\\b")
 ( punctuation "\\b=\\b")
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
 ) ; end fun-or-proto-end
 (opt-expression
 ( expression)
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end opt-expression
 (expression
 ( symbol
  ,(semantic-lambda
 ))
 ( punctuation "[!*&~]" symbol
  ,(semantic-lambda
 ))
 ( semantic-list
  ,(semantic-lambda
 ))
 ) ; end expression
 )
   "C language specification.")

(defvar semantic-flex-c-extensions
  '(("^#\\(if\\(def\\)?\\|else\\|endif\\)" . semantic-flex-c-if))
  "Extensions to the flexer for C.")

(defun semantic-flex-c-if ()
  "Move the cursor and return nil when a #if is found."
  ;; Future enhancement: Enable only the then or else clause depending on
  ;; some mysterious knowledge.
  (if (bolp) (end-of-line))
  nil)

(defun semantic-expand-c-nonterminal (nonterm)
  "Expand NONTERM into a list of equivalent nonterminals, or nil."
  (if (listp (car nonterm))
      (cond ((eq (semantic-token-token nonterm) 'variable)
	     ;; The name part comes back in the form of:
	     ;; ( NAME NUMSTARS BITS ARRAY ASSIGN )
	     (let ((vl nil)
		   (basety (semantic-token-type nonterm))
		   (ty "")
		   (mods (semantic-token-variable-extra-spec nonterm 'typemodifiers))
		   (suffix "")
		   (lst (semantic-token-name nonterm))
		   (cur nil)
		   (cnt 0))
	       (while lst
		 (setq suffix "" ty "")
		 (setq cur (car lst))
		 (if (nth 2 cur)
		     (setq suffix (concat ":" (nth 2 cur))))
		 (if (= (length basety) 1)
		     (progn
		       (setq ty (car basety))
		       (if (nth 1 cur)
			   (setq ty (concat ty (make-string (nth 1 cur) ?*)))))
		   (setq ty basety))
		 (setq vl (cons
			   (list
			    (car cur)	;name
			    'variable
			    ty		;type
			    (nth 4 cur) ;default value
			    (semantic-bovinate-make-assoc-list
			     'const (semantic-token-variable-const nonterm)
			     'suffix suffix
			     'typemodifiers mods
			     'dereference (length (nth 3 cur))
			     )
			    (semantic-token-docstring nonterm) ;doc
			    (semantic-token-overlay nonterm))
			   vl))
		 (setq lst (cdr lst)))
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
	    (t nil))
    nil))

(defun semantic-c-reconstitute-token (tokenpart declmods typedecl)
  "Reconstitute a token TOKENPART with DECLMODS and TYPEDECL.
This is so we don't have to match the same starting text several times."
  (cond ((eq (nth 1 tokenpart) 'variable)
	 (list (car tokenpart)
	       'variable
	       (or typedecl "int")	;type
	       nil			;default value (filled with expand)
	       (semantic-bovinate-make-assoc-list
		'const (if (member "const" declmods) t nil)
		'typemodifiers (delete "const" declmods))
	       nil)
	 )
	((eq (nth 1 tokenpart) 'function)
	 (list (car tokenpart)
	       'function
	       (or typedecl "int")	;type
	       (nth 4 tokenpart)	;arglist
	       (semantic-bovinate-make-assoc-list
		'const (if (member "const" declmods) t nil)
		'typemodifiers (delete "const" declmods)
		'parent (car (nth 2 tokenpart))
		'destructor (car (nth 3 tokenpart) ) )
	       nil)
	 )
	))


(defcustom semantic-default-c-path '("/usr/include" "/usr/dt/include"
					 "/usr/X11R6/include")
  "Default set of include paths for C code.
Used by `semantic-inc' to define an include path.  This should
probably do some sort of search to see what is actually on the local
machine."
  :group 'c
  :type '(repeat (string :tag "Path")))

(defcustom semantic-default-c-built-in-types
  '("void" "char" "int"  "float" "double"
    ;; Some psuedo types.
    "const" "volatile" "static" "unsigned" "signed"
    )
  "Default set of built in types for C."
  :group 'c
  :type '(repeat (string :tag "Type")))

(defvar semantic-c-keyword-table
  (semantic-flex-make-keyword-table 
   `( ("include" . INCLUDE)
      ("define" . DEFINE)
      ("extern" . EXTERN)
      ("static" . STATIC)
      ("const" . CONST)
      ("volatile" . VOLATILE)
      ("signed" . SIGNED)
      ("unsigned" . UNSIGNED)
      ("virtual" . VIRTUAL)
      ("struct" . STRUCT)
      ("union" . UNION)
      ("enum" . ENUM)
      ("typedef" . TYPEDEF)
      ("class" . CLASS)
      ("namespace" . NAMESPACE)
      ("operator" . OPERATOR)
      ("public" . PUBLIC)
      ("private" . PRIVATE)
      ("protected" . PROTECTED)
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
      )
   '(
     ("extern" summary "Declaration Modifier: extern <type> <declaration>")
     ("static" summary "Declaration Modifier: static <type> <declaration>")
     ("const" summary "Declaration Modifier: const <type> <declaration>")
     ("volatile" summary "Declaration Modifier: volatile <type> <declaration>")
     ("signed" summary "Numeric Type Modifier: signed <numeric type> ...")
     ("unsigned" summary "Numeric Type Modifier: unsigned <numeric type> ...")
     ("virtual" summary "Method Modifier: virtual <type> <fnname>(...)")
     ("struct" summary "Type Declaration: struct [name] { ... };")
     ("union" summary "Type Declaration: union [name] { ... };")
     ("enum" summary "Type Declaration: enum [name] { ... };")
     ("typedef" summary "Type Declaration: typedef <typedeclaration> <name>")
     ("class" summary "Type Declaration: class <name>[:parents] { ... };")
     ("namespace" summary "Type Declaration: namespace <name> { ... };")
     ("if" summary "Conditional: if <condition> {code} [ else { coe } ]")
     ("else" summary "Conditional: if <condition> {code} [ else { coe } ]")
     ("do" summary "Loop: do { code } while <condition>;")
     ("while" summary "Loop: do { code } while <condition>; or while <condition> { code };")
     ("for" summary "Loop: for(<init>; <condition>; <increment>) { code }")
     ("switch" summary "Conditional: switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("case" summary "Conditional: switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("default" summary "Conditional: switch (<variable>) { case <constvalue>: code; ... default: code; }")
     ("return" summary "return <value>;")
     ("break" summary "Non-local exit: break;")
     ("continue" summary "Non-local continue: continue;")
     ("sizeof" summary "Function: sizeof(<type or variable>) // size in bytes")
     ))
  "Some keywords used in C.")

(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (setq semantic-default-built-in-types semantic-default-c-built-in-types)
  ;; Code generated from c.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-c-bovine-table
	semantic-toplevel-bovine-table-source "c.bnf")
  (setq semantic-flex-keywords-obarray semantic-c-keyword-table)
  (setq semantic-equivalent-major-modes '(c-mode c++-mode))
  (setq semantic-expand-nonterminal 'semantic-expand-c-nonterminal
	semantic-flex-extensions semantic-flex-c-extensions
	semantic-dependency-include-path semantic-default-c-path
	imenu-create-index-function 'semantic-create-imenu-index
	semantic-type-relation-separator-character '("." "->")
	semantic-command-separation-character ";"
	document-comment-start "/*"
	document-comment-line-prefix " *"
	document-comment-end " */"
	)
 
 ;; End code generated from c.bnf
)

(add-hook 'c-mode-hook 'semantic-default-c-setup)
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(provide 'semantic-c)

;;; semantic-c.el ends here
