;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.16 2001/02/02 04:15:28 zappo Exp $

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
 ( include)
 ( macro)
 ( type)
 ( function)
 ( variable)
 ( define)
 ) ; end declaration
 (bovine-inner-scope
 ( macro)
 ( define)
 ( variable)
 ( prototype)
 ( type)
 ) ; end codeblock
 (include
 ( punctuation "\\b#\\b" INCLUDE punctuation "<" filename punctuation ">"
  ,(semantic-lambda
  (nth 3 vals) (list 'include t nil)))
 ( punctuation "\\b#\\b" INCLUDE string
  ,(semantic-lambda
  (list ( read (nth 2 vals)) 'include nil nil)))
 ) ; end include
 (filename
 ( symbol punctuation "\\b\\.\\b" symbol
  ,(semantic-lambda
  (list ( concat (nth 0 vals) (nth 1 vals) (nth 2 vals)))))
 ( symbol punctuation "/" filename
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
 ( variable)
 ( define)
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ) ; end structsubparts
 (classparts
 ( semantic-list
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'classsubparts)
 ))
 ) ; end classparts
 (classsubparts
 ( variable)
 ( define)
 ( function)
 ( opt-class-protection punctuation "\\b:\\b"
  ,(semantic-lambda
  (list nil)))
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
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
 ( symbol symbol "\\b\\\\(_\\+\\\\)\\?\\\\(extern\\\\|static\\\\|const\\\\|volatile\\\\|signed\\\\|unsigned\\\\|virtual\\\\)\\b" declmods
  ,(semantic-lambda
  ( cons (nth 0 vals) (nth 1 vals))))
 ( symbol symbol "\\b\\\\(_\\+\\\\)\\?\\\\(extern\\\\|static\\\\|const\\\\|volatile\\\\|signed\\\\|unsigned\\\\|virtual\\\\)\\b"
  ,(semantic-lambda
  (list (nth 0 vals))))
 (
  ,(semantic-lambda
 ))
 ) ; end declmods
 (typeform
 ( typeformbase opt-stars
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end typeform
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
 (macro
 ( punctuation "\\b#\\b" DEFINE symbol opt-expression
  ,(semantic-lambda
  (list (nth 2 vals) 'variable nil (nth 3 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ) ; end macro
 (variable
 ( variabledef punctuation "\\b;\\b"
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end variable
 (variabledef
 ( declmods typeform varnamelist
  ,(semantic-lambda
  (list (nth 2 vals) 'variable (nth 1 vals) nil ( semantic-bovinate-make-assoc-list 'const ( if ( member "const" (nth 0 vals)) t nil) 'typemodifiers ( delete "const" (nth 0 vals))) nil)))
 ) ; end variabledef
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
 ( semantic-list knr-arguments
  ,(semantic-lambda
  (nth 1 vals)))
 ( semantic-list
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
 ) ; end operatorsym
 (functionname
 ( OPERATOR operatorsym
  ,(semantic-lambda
  (nth 1 vals)))
 ( symbol
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end functionname
 (functiondef
 ( declmods typeform opt-class opt-destructor functionname arg-list
  ,(semantic-lambda
  (nth 4 vals) (list 'function (nth 1 vals) (nth 5 vals) ( semantic-bovinate-make-assoc-list 'const ( if ( member "const" (nth 0 vals)) t nil) 'typemodifiers ( delete "const" (nth 0 vals)) 'parent ( car (nth 2 vals)) 'destructor ( car (nth 3 vals))) nil)))
 ) ; end functiondef
 (prototype
 ( functiondef punctuation "\\b;\\b"
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end prototype
 (function
 ( functiondef fun-or-proto-end
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end function
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
      ("struct" . STRUCT)
      ("union" . UNION)
      ("enum" . ENUM)
      ("typedef" . TYPEDEF)
      ("class" . CLASS)
      ("operator" . OPERATOR)
      ("public" . PUBLIC)
      ("private" . PRIVATE)
      ("protected" . PROTECTED)
      )
   '(
     ("struct" type t)
     ("union" type t)
     ("typedef" type t)
     ("class" type t)
     ))
  "Some keywords used in C.")

(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (setq semantic-default-built-in-types semantic-default-c-built-in-types)
  ;; Code generated from c.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-c-bovine-table)
  (setq semantic-flex-keywords-obarray semantic-c-keyword-table)
  (setq semantic-expand-nonterminal 'semantic-expand-c-nonterminal
	semantic-flex-extensions semantic-flex-c-extensions
	semantic-dependency-include-path semantic-default-c-path
	imenu-create-index-function 'semantic-create-imenu-index
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
