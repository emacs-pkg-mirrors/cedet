;;; semantic-make.el --- Makefile parsing rules.

;; Copyright (C) 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-make.el,v 1.14 2002/05/07 01:31:14 zappo Exp $

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
;; Use the Semantic Bovinator to parse Makefiles.
;; Concocted as an experiment for nonstandard languages.

(require 'semantic)
(require 'backquote)

;;; Code:
(defvar semantic-toplevel-make-bovine-table
`((bovine-toplevel
 ( variable)
 ( rule)
 ( conditional)
 ( include)
 ( whitespace
  ,(semantic-lambda
  (list nil)))
 ( newline
  ,(semantic-lambda
  (list nil)))
 ) ; end Makefile
 (variable
 ( symbol opt-whitespace equals opt-whitespace element-list
  ,(semantic-lambda
  (list (nth 0 vals) 'variable nil (nth 4 vals) nil nil)))
 ) ; end variable
 (rule
 ( targets opt-whitespace colons opt-whitespace element-list commands
  ,(semantic-lambda
  (list (nth 0 vals) 'function nil (nth 4 vals) nil nil)))
 ) ; end rule
 (targets
 ( target opt-whitespace targets
  ,(semantic-lambda
  (list ( car (nth 0 vals)) ( car (nth 2 vals)))))
 ( target
  ,(semantic-lambda
  (list ( car (nth 0 vals)))))
 ) ; end targets
 (target
 ( sub-target target
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) ( car (nth 2 vals))))))
 ( sub-target
  ,(semantic-lambda
  (list ( car (nth 0 vals)))))
 ) ; end target
 (sub-target
 ( symbol)
 ( string)
 ( varref)
 ) ; end sub-target
 (conditional
 ( IF whitespace symbol newline
  ,(semantic-lambda
  (list nil)))
 ( IFDEF whitespace symbol newline
  ,(semantic-lambda
  (list nil)))
 ( IFNDEF whitespace symbol newline
  ,(semantic-lambda
  (list nil)))
 ( IFEQ whitespace expression newline
  ,(semantic-lambda
  (list nil)))
 ( IFNEQ whitespace expression newline
  ,(semantic-lambda
  (list nil)))
 ( ELSE newline
  ,(semantic-lambda
  (list nil)))
 ( ENDIF newline
  ,(semantic-lambda
  (list nil)))
 ) ; end conditional
 (expression
 ( semantic-list)
 ) ; end expression
 (include
 ( INCLUDE whitespace element-list
  ,(semantic-lambda
  (list (nth 2 vals) 'include nil nil)))
 ) ; end include
 (equals
 ( punctuation "\\b:\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
 ))
 ( punctuation "\\b\\+\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
 ))
 ( punctuation "\\b=\\b"
  ,(semantic-lambda
 ))
 ) ; end equals
 (colons
 ( punctuation "\\b:\\b" punctuation "\\b:\\b"
  ,(semantic-lambda
 ))
 ( punctuation "\\b:\\b"
  ,(semantic-lambda
 ))
 ) ; end colons
 (element-list
 ( elements newline
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end element-list
 (elements
 ( element whitespace elements
  ,(semantic-lambda
  (list (nth 0 vals)) (nth 2 vals)))
 ( element
  ,(semantic-lambda
  (list (nth 0 vals))))
 ()
 ) ; end elements
 (element
 ( sub-element element
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ()
 ) ; end element
 (sub-element
 ( symbol)
 ( string)
 ( punctuation)
 ( semantic-list
  ,(semantic-lambda
  (list ( buffer-substring-no-properties ( identity start) ( identity end)))))
 ) ; end sub-element
 (varref
 ( punctuation "\\b\\$\\b" semantic-list
  ,(semantic-lambda
  (list ( buffer-substring-no-properties ( identity start) ( identity end)))))
 ) ; end varref
 (commands
 ( shell-command newline commands
  ,(semantic-lambda
  (list (nth 0 vals)) (nth 1 vals)))
 (
  ,(semantic-lambda
 ))
 ) ; end commands
 (opt-whitespace
 ( whitespace
  ,(semantic-lambda
  (list nil)))
 ()
 ) ; end opt-whitespace
 )
 "Table for parsing Makefiles.")

(defvar semantic-make-keyword-table
  (semantic-flex-make-keyword-table 
   `( ("if" . IF)
      ("ifdef" . IFDEF)
      ("ifndef" . IFNDEF)
      ("ifeq" . IFEQ)
      ("ifneq" . IFNEQ)
      ("else" . ELSE)
      ("endif" . ENDIF)
      ("include" . INCLUDE)
      )
   '(
     ("if" summary "Conditional: if (expression) ... else ... endif")
     ("else" summary "Conditional: if (expression) ... else ... endif")
     ("endif" summary "Conditional: if (expression) ... else ... endif")
     ("ifdef" summary "Conditional: ifdef (expression) ... else ... endif")
     ("ifndef" summary "Conditional: ifndef (expression) ... else ... endif")
     ("ifeq" summary "Conditional: ifeq (expression) ... else ... endif")
     ("ifneq" summary "Conditional: ifneq (expression) ... else ... endif")
     ("include" summary "Macro: include filename1 filename2 ...")
     ))
  "Keyword table for Makefiles.")

(defvar semantic-flex-make-extensions
  '(("^\\(\t\\)" . semantic-flex-make-command)
    ("\\(\\\\\n\t*\\)" . semantic-flex-nonewline))
  "Extensions to the flexer for make.")

(defun semantic-flex-make-command ()
  "Move the cursor and return nil when a tab starting line is found.
These command lines continue to additional lines when the end with \\"
  (let ((start (match-end 0)))
    (while (progn (end-of-line)
		  (save-excursion (forward-char -1) (looking-at "\\\\")))
      (forward-char 1))
    (cons 'shell-command (cons start (point)))))

(defun semantic-flex-nonewline ()
  "If there is a \ ending a line, then it isn't really a newline."
  (goto-char (match-end 0))
  (cons 'whitespace (cons (match-beginning 0) (match-end 0))) )

(defun semantic-expand-make-nonterminal (token)
  "Expand TOKEN into a list of equivalent nonterminals, or nil."
  (cond
   ((eq (semantic-token-token token) 'function)
    (let ((name (semantic-token-name token)))
      (if (listp name)
	  (let ((multi (cdr name)))

	    ;; Always replace the list of function names by the first
	    ;; name to get a valid token!  There is nothing more to
	    ;; do if there is only one function in the list.
	    (setcar token (car name))

	    (if multi
		;; There are multiple names in the same function
		;; declaration.
		(let ((ty (semantic-token-type                 token))
		      (al (semantic-token-function-args        token))
		      (xs (semantic-token-function-extra-specs token))
		      (ds (semantic-token-docstring            token))
		      (pr (semantic-token-properties           token))
		      (nl name)
		      tok vl)
		  ;; Merge in new 'function tokens each reparsed
		  ;; token name and overlay with other values from
		  ;; the initial token.
		  (while nl
		    (setq tok (car nl)
			  nl  (cdr nl)
			  vl  (cons
			       (list
				tok
				'function
				ty	; type
				al	; arg list
				xs	; extra specs
				ds	; docstring
				pr	; properties
				(semantic-token-overlay token))
			       vl)))
		  (if vl
		      ;; Cleanup the no more needed initial token.
		      (semantic-deoverlay-token token))
		  vl))))))
   ((eq (semantic-token-token token) 'include)
    (let* ((name (semantic-token-name token))
	   (multi (cdr name)))
      ;; NAME is always going to be a list.  Delist it and create
      ;; one include entry for each file on the include line.

      ;; Always replace the list of function names by the first
      ;; name to get a valid token!  There is nothing more to
      ;; do if there is only one function in the list.
      (setcar token (car (car name)))

      (if multi
	  ;; There are multiple names in the same function
	  ;; declaration.
	  (let ((sy (semantic-token-include-system token))
		(ds (semantic-token-docstring            token))
		(pr (semantic-token-properties           token))
		(nl multi)
		(vl (cons token nil))
		tok)
	    ;; Merge in new 'function tokens each reparsed
	    ;; token name and overlay with other values from
	    ;; the initial token.
	    (while nl
	      (setq tok (car nl)
		    nl  (cdr nl)
		    vl  (cons
			 (list
			  (car tok)
			  'include
			  sy		; system
			  ds		; docstring
			  pr		; properties
			  (semantic-token-overlay token))
			 vl)))
	    vl))))
    ))

;;;###autoload
(defun semantic-default-make-setup ()
  "Set up a Makefile buffer for parsing with semantic."
  (setq semantic-flex-extensions semantic-flex-make-extensions)
  ;; Code generated from make.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-make-bovine-table
	semantic-toplevel-bovine-table-source "make.bnf")
  (setq semantic-flex-keywords-obarray semantic-make-keyword-table)
  (setq semantic-symbol->name-assoc-list '((variable . "Variables")
					   (function . "Rules")
					   (include . "Dependencies"))
	semantic-number-expression nil
	semantic-case-fold t
	semantic-expand-nonterminal 'semantic-expand-make-nonterminal
	semantic-flex-syntax-modifications '((?. "_")
					     (?= ".")
					     (?/ "_")
					     (?$ ".")
					     )
	semantic-flex-enable-newlines t
	semantic-flex-enable-whitespace t
	imenu-create-index-function 'semantic-create-imenu-index
	)
 
  ;; End code generated from make.bnf
  )

(add-hook 'makefile-mode-hook 'semantic-default-make-setup)



(provide 'semantic-make)

;;; semantic-make.el ends here
