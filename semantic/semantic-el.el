;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: goofy
;; X-RCS: $Id: semantic-el.el,v 1.3 1999/05/07 02:39:52 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
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

;;; Code:
(defvar semantic-toplevel-elisp-bovine-table
  '((bovine-toplevel
     (semantic-list (lambda (vals start end)
		      (semantic-bovinate-from-nonterminal
		       start end 'extract-toplevel)))
     (extract-toplevel))
    ;; When parsing at depth 0, we need to extract elements from semantic
    ;; lists at bovine-toplevel.  This symbol provides the needed redirection.
    (extract-toplevel
     (function) (variable) (include) (code) (comment))
    ;; A function is anything that starts with a (defun
    (function
     (open-paren symbol "defun\\|defmacro" symbol arg-list; string
		 (lambda (vals start end)
		   (list (nth 2 vals) 'function nil (nth 3 vals) nil;(nth 4 vals)
			 start end))))
    ;; A variable can be a defvar or defconst.
    (variable
     (open-paren symbol "defvar\\|defconst" symbol expression string
		 (lambda (vals start end)
		   (list (nth 2 vals) 'variable nil
			 (if (string= (nth 1 vals) "defconst") t nil)
			 nil;(nth 4 vals)
			 start end))))
    ;; In elisp, and include is just the require statement.
    (include
     (open-paren symbol "require" quote symbol
		 (lambda (vals start end)
		   (list (nth 3 vals) 'include nil start end))))
    ;; Some random code stuck in there.
    (code
     (open-paren symbol
		 (lambda (vals start end)
		   (let ((sym (if (nth 1 vals) (intern-soft (nth 1 vals)))))
		     (if (and sym (fboundp sym))
			 (list (nth 1 vals) 'code start end)
		       )))))
    ;; Quotes are oft optional in some cases
    (quote (punctuation "'"))
    ;; Something that can be evaluated out to something.
    (expression
     (quote expression (lambda (vals start end)
			 (list (car (cdr vals)) start end)))
     (semantic-list) (symbol) (string))
    ;; An argument list to a function
    (arg-list
     (semantic-list (lambda (vals start end)
		      (semantic-bovinate-from-nonterminal start end 'argsyms)
		      ))
     ;; If it's already opened, what to do??
     )
    ;; This guys is some number of argument symbols...
    (argsyms
     (open-paren close-paren (lambda (vals start end)
			       (list nil start end)))
     (open-paren argsyms (lambda (vals start end)
			   (append (car (cdr vals)) (list start end))))
     (symbol argsyms (lambda (vals start end)
		       (append (cons (car vals) (car (cdr vals)))
			       (list start end))))
     (symbol close-paren (lambda (vals start end)
			   (list (car vals) start end))))
    )
  "Top level bovination table for elisp.")

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq semantic-toplevel-bovine-table
		  semantic-toplevel-elisp-bovine-table)))

(defun semantic-bovinate-interactive ()
  "Bovinate the toplevel of this buffer, and dis play the results."
  (interactive)
  (message "%S" (semantic-bovinate-toplevel nil t)))

(defvar semantic-toplevel-c-bovine-table
  '(;; BOVINE-TOPLEVEL : INCLUDE
    ;;                 | TYPE
    ;;                 | VARIABLE
    ;;                 | PROTOTYPE
    ;;                 | FUNCTION
    ;;                 | DEFINE
    ;;                 | COMMENT
    ;;                 ;
    (bovine-toplevel
     (include) (type) (variable) (prototype) (function) (comment))
    ;; INCLUDE : "#" "include" "<" NAME ">"
    ;;         | "#" "include" STRING
    ;;         ;
    (include
     (punctuation "#" symbol "include" punctuation "<" symbol
                  punctuation "." symbol "h" punctuation ">"
                  (lambda (vals start end)
                    (list (nth 3 vals) 'include start end)))
     (punctuation "#" symbol "include" string
                  (lambda (vals start end)
                    (list (nth 3 vals) 'include start end))))
    ;; STRUCTPARTS : "{" "}"
    ;;             | "{" STRUCTPARTS
    ;;             | VARIABLE STRUCTPARTS
    ;;             | VARIABLE "}"
    ;;             ;
    (structparts-opt
     (structparts)
     ((lambda (vals start end) (list nil start end))))
    (structparts
     ;; do this later.
     nil)
    ;; ENUMPARTS : "{" "}"
    ;;           | "{" ENUMPARTS
    ;;           | NAME OPT-ASSIGN ","
    ;;           | NAME OPT-ASSIGN "}"
    ;;           ;
    (enumparts-opt
     (enumparts)
     ((lambda (vals start end) (list nil start end))))
    (enumparts
     ;; Do this later.
     nil)
    ;; TYPESIMPLE : "struct" opt-name "{" STRUCTPARTS "}"
    ;;            | "union" opt-name "{" STRUCTPARTS "}"
    ;;            | "typedef" TYPE name
    ;;            ;
    (typesimple
     (symbol "struct\\|union" opt-name structparts
             (lambda (vals start end)
               (list (nth 1 vals) 'type (nth 0 vals) (nth 3 vals) nil nil
                     start end)))
     (symbol "enum" opt-name enumparts
             (lambda (vals start end)
               (list (nth 1 vals) 'type (nth 0 vals) (nth 3 vals) nil nil
                     start end)))
     (symbol "typedef" type symbol
             (lambda (vals start end)
               (list (nth 2 vals) 'type (nth 1 vals) nil nil nil start end)))
     )
    ;; TYPE : TYPESIMPLE ";"
    (type (typesimple punctuation ";"
                      (lambda (vals start end)
                        (append (car vals) (list start end)))))
    ;; OPT_STARS : /* empty */
    ;;           | "*" OPT_STARS
    ;;           ;
    (opt-stars
     (punctuation "*" opt-stars (lambda (vals start end)
                                  (list (1+ (car (nth 1 vals)))
                                        'opt-stars start end)))
     ((lambda (vals start end) (list 0 start end))))
    ;; DECLMODS : /* empty */ | "extern" | "static" | "const" | "volitile" ;
    (declmods
     (symbol "extern\\|static\\|const\\|volitile") ; auto-lambda
     ((lambda (vals start end) (list "" start end))))
    ;; TYPEFORM : TYPESPEC OPT_STARS
    (typeform
     (declmods typespec opt-stars (lambda (vals start end)
                                     ;; what I really want to do is append
                                     ;; (make-string #stars ?\*) to the name
                                     ;; of the type from TYPESPEC
                                     ;; Also add the declmods
                                     (append (nth 1 vals) (list start end)))))
    ;; TYPESPEC : "struct" NAME STRUCTPARTS
    ;;          | "struct" NAME
    ;;          | "union" NAME STRUCTPARTS
    ;;          | "union" NAME
    ;;          | "enum" NAME
    ;;          | "enum" NAME ENUMPARTS
    ;;          | NAME
    ;;          ;
    (typespec
     (symbol "struct\\|union" symbol structparts-opt
	     (lambda (vals start end)
	       (list (concat
		      (nth 0 vals) " "
		      (nth 1 vals))
		     start end)))
     (symbol "enum" symbol enumparts-opt
	     (lambda (vals start end)
	       (list (concat
		      (nth 0 vals) " "
		      (nth 1 vals))
		     start end)))
     (symbol) ; auto-lambda
     )
    ;; OPT-ARRAY : /* empty */
    ;;           | "[" symbol-or-number "]" OPT-ARRAY
    ;;           ;
    (opt-array
     (open-paren "[" symbol close-paren "]" opt-array
                 (lambda (vals start end)
                   (list (1+ (car (nth 3 vals))) start end)))
     ((lambda (vals start end) (list 0 start end)))
     ((lambda (vals start end) nil)))
    ;; OPT-ASSIGN : /* empty */
    ;;            | "=" EXPRESSION;
    ;;            ;
    (opt-assign
     (punctuation "=" expression (lambda (vals start end)
                                   (append (nth 1 vals)
                                           (list start end))))
     ((lambda (vals start end) nil)))
    ;; VARIABLE : DECLMODS TYPEFORM VARNAMELIST ";"
    ;;          | "#" "define" NAME expression        /* This is cheating... */
    ;;          ;
    (variable
     (typeform varnamelist punctuation ";"
               (lambda (vals start end)
                 (let ((vnl (nth 1 vals)))
                   (while vnl
                     ;; For now we are tossing out vector info.
                     ;; eventually merge this in.
                     (setcar (nthcdr 2 (car vnl))
                             (nth 0 vals))
                     (setq vnl (cdr vnl))))
                 (list 'explode (nth 1 vals) start end)))
     (punctuation "#" symbol "define" symbol expression
                  (lambda (vals start end)
                    (list (nth 2 vals) 'variable t (nth 3 vals) nil start end)
                    )))
    ;; VARNAMELIST : NAME OPT_ARRAY OPT_ASSIGN ","
    ;;             | NAME OPT_ARRAY OPT_ASSIGN ";"
    (varnamelist
     (symbol opt-array opt-assign punctuation ","
             (lambda (vals start end)
               (list (car vals) 'variable (car (nth 2 vals)) nil
                     (car (nth 2 vals)) nil start end)))
     (symbol opt-array opt-assign
             (lambda (vals start end)
               (list (car vals) 'variable (car (nth 2 vals)) nil
                     (car (nth 2 vals)) nil start end))))
    ;; ARG-LIST : "(" ARG-LIST
    ;;          | VARIABLE ARG-LIST
    ;;          | VARIABLE ")"
    ;;          ;
    (arg-list
     (semantic-list
      (lambda (vals start end)
        (semantic-bovinate-from-nonterminal start end 'arg-list)))
     
     (open-paren "(" arg-list
                 (lambda (vals start end)
                   (append (nth 1 vals) (list start end))))
     (variable arg-list
               (lambda (vals start end)
                 (list (cons (car vals) (nth 1 vals)) start end)))
     (variable close-paren ")"
               (lambda (vals start end)
                 (list (list (car vals)) start end))))
    ;; FUNCTIONDEF : TYPEFORM NAME "(" ARGLIST ")"
    ;;             ;
    (functiondef
     (typeform symbol arg-list
               (lambda (vals start end)
                 (list (nth 1 vals) 'function
                       (nth 0 vals) (nth 2 vals) nil
                       start end))))
    ;; PROTOTYPE : FUNCTIONDEF ";" ;
    (prototype
     (functiondef punctuation ";"
                  (lambda (vals start end)
                    (append (car vals) (list start end)))))
    ;; FUNCTION : FUNCTIONDEF semantic-list
    ;;          ;
    (function
     (functiondef semantic-list (lambda (vals start end)
                                  (append (car vals) (list start end)))))
    ;; EXPRESSION : symbol
    ;;            | "!*&~" symbol
    ;;            | "(" EXPRESSION ")"
    ;;            | EXPRESSION "+-*/%^|&" EXPRESSION
    ;;            ;
    ;; Resulting expression token is a tree in the form of the
    ;; text.
    (expression
     (symbol)
     (semantic-list)                    ;covers groups too.  Kool.
     (punctuation "[!*&~-+]+" symbol
                  (lambda (vals start end)
                    (list (nth 0 vals) (nth 1 vals) start end)))
     (open-paren "(" expression close-paren ")"
                 (lambda (vals start end)
                   (append (nth 1 vals) (list start end))))
;     (expression punctuation "[-+*/%^|&]+" expression
;                (lambda (vals start end)
;                  (list (nth 1 vals) (nth 0 vals) (nth 2 vals) start end)))
     )
    ;; End of the list
    )
  "Toplevel bovine table definition for the C language.")

(add-hook 'c-mode-hook
          (lambda ()
            (setq semantic-toplevel-bovine-table
                  semantic-toplevel-c-bovine-table)))

(provide 'semantic-ex)

;;; semantic-ex.el ends here
