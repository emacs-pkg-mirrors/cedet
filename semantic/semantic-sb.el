;;; semantic-sb.el --- Semantic tag display for speedbar

;;; Copyright (C) 1999 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: syntax
;; X-RCS: $Id: semantic-sb.el,v 1.3 1999/05/18 14:08:36 zappo Exp $

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
;; Convert a bovinated token list into speedbar buttons.

;;; History:
;; 

(require 'semantic)
(require 'speedbar)

;;; Code:

;;; Button Generation
;;
;;  Here are some button groups:
;;
;;  +> Function (+)
;;    @ return_type
;;    ( v arg1
;;    | v arg2
;;    ) v arg3
;;
;;  +> Variable[1] =
;;    @ type
;;    = default value
;;
;;  +>  -> click to see additional information
;;  (#) -> Number of arguments to a function

(defun semantic-sb-one-button (token depth &optional prefix)
  "Insert TOKEN as a speedbar button at DEPTH.
Optional PREFIX is used to specify special marker characters."
  (let* ((type (semantic-token-token token))
	 (edata (cond ((eq type 'type)
		        (semantic-token-type-parts token))
		      ((eq type 'variable)
		       (semantic-token-variable-default token))
		      ((eq type 'function)
		       (semantic-token-function-args token))))
	 (start (point))
	 (end (progn
		(insert (int-to-string depth) ":")
		(point))))
    (put-text-property start end 'invisible t)
    (insert-char ?  (1- depth) nil)
    ;; take care of edata = (nil) -- a yucky but hard to clean case
    (if (and edata (listp edata) (and (<= (length edata) 1) (not (car edata))))
	(setq edata nil))
    ;; types are a bit unique.  Variable types can have special meaning.
    (if (eq type 'type)
	(let ((name (semantic-token-name token)))
	  (if (semantic-token-type token)
	      (setq name (concat (semantic-token-type token) " " name)))
	  (if edata
	      (speedbar-insert-button (if prefix (concat " +" prefix) " +>")
				      'speedbar-button-face
				      'speedbar-highlight-face
				      'semantic-sb-show-extra
				      token t)
	    (speedbar-insert-button (if prefix (concat "  " prefix) " =>")
				    nil nil nil nil t))
	  (speedbar-insert-button name
				  'speedbar-tag-face
				  'speedbar-highlight-face
				  'semantic-sb-token-jump
				  token t))
      (if (or (and (semantic-token-type token)
		   (or (not (listp (semantic-token-type token)))
		       (car (semantic-token-type token))))
	      edata)
	  (speedbar-insert-button (if prefix (concat " +" prefix) " +>")
				  'speedbar-button-face
				  'speedbar-highlight-face
				  'semantic-sb-show-extra
				  token t)
	(speedbar-insert-button (if prefix (concat "  " prefix) " =>")
				nil nil nil nil t))
      (speedbar-insert-button (semantic-token-name token)
			      'speedbar-tag-face
			      'speedbar-highlight-face
			      'semantic-sb-token-jump
			      token t)
      (cond ((eq type 'variable)
	     ;; Place array dims here if apropriate.
	     (if (semantic-token-variable-default token)
		 (speedbar-insert-button "=" nil nil nil nil t)))
	    ((eq type 'function)
	     (speedbar-insert-button "()" nil nil nil nil t))))))
  
(defun semantic-sb-speedbar-data-line (depth button text &optional
					     text-fun text-data)
  "Insert a semantic token data element.
DEPTH is the current depth.  BUTTON is the text for the button.
TEXT is the actual info with TEXT-FUN to occur when it happens.
Argument TEXT-DATA is the token data to pass to TEXT-FUN."
  (let ((start (point))
	(end (progn
	       (insert (int-to-string depth) ":")
	       (point))))
    (put-text-property start end 'invisible t)
    (insert-char ?  depth nil)
    (speedbar-insert-button button nil nil nil nil t)
    (speedbar-insert-button text
			    'speedbar-tag-face
			    (if text-fun 'speedbar-highlight-face)
			    text-fun text-data t)
    ))

(defun semantic-sb-maybe-token-to-button (obj indent &optional prefix)
  "Convert OBJ, which was returned from the bovinator, into a button.
This OBJ might be a plain string (simple type or untyped variable)
or a complete bovinator type.
Argument INDENT is the indentation used when making the button.
Optional PREFIX is the character to use when marking the line."
  (let ((myprefix (or prefix ">")))
    (if (stringp obj)
	(semantic-sb-speedbar-data-line indent myprefix obj)
      (if (listp obj)
	  (progn
	    (if (and (stringp (car obj))
		     (= (length obj) 1))
		(semantic-sb-speedbar-data-line indent myprefix (car obj))
	      (semantic-sb-one-button obj indent prefix)))))))

(defun semantic-sb-insert-details (token indent)
  "Insert details about TOKEN at level INDENT."
  (let ((tt (semantic-token-token token))
	(type (semantic-token-type token)))
    (cond ((eq tt 'type)
	   (let ((parts (semantic-token-type-parts token)))
	     ;; Lets expect PARTS to be a list of either strings,
	     ;; or variable tokens.
	     (while parts
	       (semantic-sb-maybe-token-to-button (car parts) indent)
	       (setq parts (cdr parts)))))
	  ((eq tt 'variable)
	   (if type (semantic-sb-maybe-token-to-button type indent "@"))
	   ;; default value here
	   )
	  ((eq tt 'function)
	   (if type
	       (semantic-sb-speedbar-data-line
		indent "@"
		(if (stringp type) type
		  (semantic-token-name type))))
	   ;; Arguments to the function
	   (let ((args (semantic-token-function-args token)))
	     (if (and args (car args))
		 (progn
		   (semantic-sb-maybe-token-to-button (car args) indent "(")
		   (setq args (cdr args))
		   (while (> (length args) 1)
		     (semantic-sb-maybe-token-to-button (car args)
							indent
							"|")
		     (setq args (cdr args)))
		   (if args
		       (semantic-sb-maybe-token-to-button
			(car args) indent ")"))
		   )))))
    ))

(defun semantic-sb-show-extra (text token indent)
  "Display additional information about the token as an expansion.
TEXT TOKEN and INDENT are the details."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (save-restriction
	       (narrow-to-region (point) (point))
	       ;; Add in stuff specific to this type of token.
	       (semantic-sb-insert-details token (1+ indent))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun semantic-sb-token-jump (text token indent)
  "Jump to the location specified in token.
TEXT TOKEN and INDENT are the details."
  (let ((file (speedbar-line-path indent)))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)
    (goto-char (semantic-token-start token))
    (run-hooks 'speedbar-visiting-tag-hook)
    ;;(recenter)
    (speedbar-maybee-jump-to-attached-frame)
    ))

(defun semantic-sb-expand-group (text token indent)
  "Expand a group which has semantic tokens.
TEXT TOKEN and INDENT are the details."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (save-restriction
	       (narrow-to-region (point) (point))
	       (while token
		 (semantic-sb-one-button (car token) (1+ indent))
		 (setq token (cdr token)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun semantic-sb-buttons (level tokens)
  "Create buttons for a speedbar display at LEVEL using TOKENS."
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((buckets (semantic-sb-buckets tokens))
	  (names '(nil "Types" "Variables" "Functions"))
	  tmp)
      (while buckets
	(setq tmp (car buckets)
	      buckets (cdr buckets)
	      names (cdr names))
	(if tmp
	    (speedbar-make-tag-line 'curly ?+ 'semantic-sb-expand-group
				    tmp
				    (car names)
				    nil nil 'speedbar-tag-face
				    (1+ level)))))))

(defun semantic-sb-buckets (tokens)
  "Sort TOKENS into a group of buckets based on type, and toss the rest."
  (let ((vars nil) (funs nil) (types nil) toktype)
    (while tokens
      (setq toktype (semantic-token-token (car tokens)))
      (cond ((eq toktype  'variable)
	     (setq vars (cons (car tokens) vars)))
	    ((eq toktype 'function)
	     (setq funs (cons (car tokens) funs)))
	    ((eq toktype 'type)
	     (setq types (cons (car tokens) types)))
	    (t nil))
      (setq tokens (cdr tokens)))
    (list (nreverse types) (nreverse vars) (nreverse funs))))

(provide 'semantic-sb)

;;; semantic-sb.el ends here
