;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.1 2000/06/13 14:26:23 zappo Exp $

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
     ( comment)
     ( function)
     ( variable)
     ( prototype)
     ( type)
     ( define)
     ) ; end bovine-toplevel
    (include
     ( punctuation "#" symbol "include" punctuation "<" filename punctuation ">"
		   ,(lambda (vals start end)
		      (append  (nth 3 vals) (list 'include 't nil)
			       (list start end))))
     ( punctuation "#" symbol "include" string
		   ,(lambda (vals start end)
		      (append  (list ( read (nth 2 vals)) 'include nil nil)
			       (list start end))))
     ) ; end include
    (filename
     ( symbol punctuation "\\." symbol
	      ,(lambda (vals start end)
		 (append  (list ( concat (nth 0 vals) (nth 1 vals) (nth 2 vals)))
			  (list start end))))
     ( symbol punctuation "/" filename
	      ,(lambda (vals start end)
		 (append  (list ( concat (nth 0 vals) (nth 1 vals) ( car (nth 2 vals))))
			  (list start end))))
     ) ; end filename
    (structparts
     ( semantic-list
       ,(lambda (vals start end)
	  (append
	   (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'structsubparts)
	   
	   (list start end))))
     ) ; end structparts
    (structsubparts
     ( variable)
     ( define)
     ) ; end structsubparts
    (enumparts
     ( semantic-list
       ,(lambda (vals start end)
	  (append
	   (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'enumsubparts)
	   
	   (list start end))))
     ) ; end enumparts
    (enumsubparts
     ( symbol opt-assign
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals))
			  (list start end))))
     ) ; end enumsubparts
    (opt-name
     ( symbol)
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end opt-name
    (typesimple
     ( symbol "struct\\|union" opt-name structparts
	      ,(lambda (vals start end)
		 (append  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil)
			  (list start end))))
     ( symbol "enum" opt-name enumparts
	      ,(lambda (vals start end)
		 (append  (nth 1 vals) (list 'type (nth 0 vals) (nth 2 vals) nil nil)
			  (list start end))))
     ( symbol "typedef" typeform symbol
	      ,(lambda (vals start end)
		 (append  (list (nth 2 vals) 'type (nth 0 vals) nil (nth 1 vals) nil)
			  (list start end))))
     ) ; end typesimple
    (type
     ( typesimple punctuation ";"
		  ,(lambda (vals start end)
		     (append  (nth 0 vals)
			      (list start end))))
     ) ; end type
    (opt-stars
     ( punctuation "*" opt-stars
		   ,(lambda (vals start end)
		      (append  (list ( 1+ ( car (nth 1 vals))))
			       (list start end))))
     (
      ,(lambda (vals start end)
	 (append  (list 0)
		  (list start end))))
     ) ; end opt-stars
    (declmods
     ( symbol "\\(__\\)?\\(extern\\|static\\|const\\|volitile\\|signed\\|unsigned\\)+")
     (
      ,(lambda (vals start end)
	 (append  (list "")
		  (list start end))))
     ) ; end declmods
    (typeform
     ( typeformbase opt-stars
		    ,(lambda (vals start end)
		       (append  (nth 0 vals)
				(list start end))))
     ) ; end typeform
    (typeformbase
     ( typesimple
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( symbol "struct\\|union\\|enum" symbol
	      ,(lambda (vals start end)
		 (append  (list (nth 1 vals) 'type (nth 0 vals))
			  (list start end))))
     ( symbol
       ,(lambda (vals start end)
	  (append  (list (nth 0 vals))
		   (list start end))))
     ) ; end typeformbase
    (opt-bits
     ( punctuation ":" symbol
		   ,(lambda (vals start end)
		      (append  (list (nth 1 vals))
			       (list start end))))
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end opt-bits
    (opt-array
     ( semantic-list "^\\[.*\\]$" opt-array
		     ,(lambda (vals start end)
			(append  (list ( cons 1 ( car (nth 1 vals))))
				 (list start end))))
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end opt-array
    (opt-assign
     ( punctuation "=" expression
		   ,(lambda (vals start end)
		      (append  (list (nth 1 vals))
			       (list start end))))
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end opt-assign
    (macro
     ( punctuation "#" symbol "define" symbol opt-expression
		   ,(lambda (vals start end)
		      (append  (list (nth 2 vals) 'variable nil 't (nth 3 vals) nil nil)
			       (list start end))))
     ) ; end macro
    (variable
     ( variabledef punctuation ";"
		   ,(lambda (vals start end)
		      (append  (nth 0 vals)
			       (list start end))))
     ) ; end variable
    (variabledef
     ( declmods typeform varnamelist
		,(lambda (vals start end)
		   (append  (list (nth 2 vals) 'variable (nth 1 vals) ( string-match "const" ( car (nth 0 vals))) nil nil nil)
			    (list start end))))
     ) ; end variabledef
    (opt-restrict
     ( symbol "\\(__\\)?restrict")
     ()
     ) ; end opt-restrict
    (varname
     ( opt-stars opt-restrict symbol opt-bits opt-array opt-assign
		 ,(lambda (vals start end)
		    (append  (list (nth 2 vals)) (nth 0 vals) (nth 3 vals) (nth 4 vals) (nth 5 vals)
			     (list start end))))
     ) ; end varname
    (variablearg
     ( declmods typeform varname
		,(lambda (vals start end)
		   (append  (list ( car (nth 2 vals)) 'variable (nth 1 vals) ( string-match "const" ( car (nth 0 vals))) nil nil nil)
			    (list start end))))
     ) ; end variablearg
    (varnamelist
     ( varname punctuation "," varnamelist
	       ,(lambda (vals start end)
		  (append  ( cons (nth 0 vals) (nth 2 vals))
			   (list start end))))
     ( varname
       ,(lambda (vals start end)
	  (append  (list (nth 0 vals))
		   (list start end))))
     ) ; end varnamelist
    (arg-list
     ( symbol "__P" semantic-list
	      ,(lambda (vals start end)
		 
		 (semantic-bovinate-from-nonterminal (car (nth 1 vals)) (cdr (nth 1 vals)) 'arg-list-p)
		 ))
     ( semantic-list knr-arguments
		     ,(lambda (vals start end)
			(append  (nth 1 vals)
				 (list start end))))
     ( semantic-list
       ,(lambda (vals start end)
	  (append
	   (semantic-bovinate-from-nonterminal-full (car (nth 0 vals)) (cdr (nth 0 vals)) 'arg-sub-list)
	   
	   (list start end))))
     ) ; end arg-list
    (knr-arguments
     ( variablearg punctuation ";" knr-arguments
		   ,(lambda (vals start end)
		      (append  ( cons (nth 0 vals) (nth 2 vals))
			       (list start end))))
     ( variablearg punctuation ";"
		   ,(lambda (vals start end)
		      (append  (list (nth 0 vals))
			       (list start end))))
     ) ; end knr-arguments
    (arg-list-p
     ( open-paren "(" semantic-list close-paren ")"
		  ,(lambda (vals start end)
		     (append
		      (semantic-bovinate-from-nonterminal-full (car (nth 1 vals)) (cdr (nth 1 vals)) 'arg-sub-list)
		      
		      (list start end))))
     ) ; end arg-list-p
    (arg-sub-list
     ( variablearg
       ,(lambda (vals start end)
	  (append  (nth 0 vals)
		   (list start end))))
     ( punctuation "\\." punctuation "\\." punctuation "\\." close-paren ")"
		   ,(lambda (vals start end)
		      (append  (list "...")
			       (list start end))))
     ) ; end arg-sub-list
    (functiondef
     ( declmods typeform symbol arg-list
		,(lambda (vals start end)
		   (append  (list (nth 2 vals) 'function (nth 1 vals) (nth 3 vals) nil)
			    (list start end))))
     ) ; end functiondef
    (prototype
     ( functiondef punctuation ";"
		   ,(lambda (vals start end)
		      (append  (nth 0 vals)
			       (list start end))))
     ) ; end prototype
    (function
     ( functiondef semantic-list
		   ,(lambda (vals start end)
		      (append  (nth 0 vals)
			       (list start end))))
     ) ; end function
    (opt-expression
     ( expression)
     (
      ,(lambda (vals start end)
	 (append  (list nil)
		  (list start end))))
     ) ; end opt-expression
    (expression
     ( symbol
       ,(lambda (vals start end)
	  (append  (list nil)
		   (list start end))))
     ( punctuation "[!*&~]" symbol
		   ,(lambda (vals start end)
		      (append  (list nil)
			       (list start end))))
     ( semantic-list
       ,(lambda (vals start end)
	  (append  (list nil)
		   (list start end))))
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
		   (mods "")
		   (lst (semantic-token-name nonterm))
		   (cur nil)
		   (cnt 0))
	       (while lst
		 (setq mods "" ty "")
		 (setq cur (car lst))
		 (if (nth 2 cur)
		     (setq mods (concat ":" (nth 2 cur))))
		 (if (nth 3 cur)
		     (setq mods (concat mods
					"[" (int-to-string
					     (length (nth 3 cur))) "]")))
		 (if (= (length basety) 1)
		     (progn
		       (setq ty (car basety))
		       (if (nth 1 cur)
			   (setq ty (concat ty (make-string (nth 1 cur) ?*)))))
		   (setq ty basety))
		 (setq vl (cons (list (car cur)
				      'variable
				      ty
				      (semantic-token-variable-const nonterm)
				      (nth 4 cur)
				      mods
				      (semantic-token-docstring nonterm)
				      (semantic-token-start nonterm)
				      (semantic-token-end nonterm))
				vl))
		 (setq lst (cdr lst)))
	       vl))
	    ((and (listp (car nonterm))
		  (eq (semantic-token-token (car nonterm)) 'variable))
	     ;; Argument lists come in this way.  Append all the expandsions!
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

(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
   (setq semantic-toplevel-bovine-table semantic-toplevel-c-bovine-table
	 semantic-expand-nonterminal 'semantic-expand-c-nonterminal
	 semantic-flex-extensions semantic-flex-c-extensions
	 semantic-dependency-include-path semantic-default-c-path
	 semantic-default-built-in-types semantic-default-c-built-in-types
	 imenu-create-index-function 'semantic-create-imenu-index
	 ;; For documentation
	 document-comment-start "/*"
	 document-comment-line-prefix " *"
	 document-comment-end " */"
	 ))

(add-hook 'c-mode-hook 'semantic-default-c-setup)

(provide 'semantic-c)

;;; semantic-c.el ends here
