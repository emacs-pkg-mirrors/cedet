;;; semantic-sb.el --- Semantic tag display for speedbar

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: syntax
;; X-RCS: $Id: semantic-sb.el,v 1.34 2002/05/07 01:31:14 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-sb is free software; you can redistribute it and/or modify
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
;; Convert a bovinated token list into speedbar buttons.

;;; History:
;; 

(require 'semantic)
(require 'speedbar)

(defcustom semantic-sb-autoexpand-length 1
  "*Length of a semantic bucket to autoexpand in place.
This will replace the named bucket that would have usually occured here."
  :group 'speedbar
  :type 'integer)

(defcustom semantic-sb-button-token->text-function 'semantic-abbreviate-nonterminal
  "*Function called to create the text for a but from a token."
  :group 'speedbar
  :type semantic-token->text-custom-list)

(defcustom semantic-sb-info-token->text-function 'semantic-summarize-nonterminal
  "*Function called to create the text for info display from a token."
  :group 'speedbar
  :type semantic-token->text-custom-list)

;;; Code:

;;; Button Generation
;;
;;  Here are some button groups:
;;
;;  +> Function ()
;;     @ return_type
;;    +( arg1
;;    +| arg2
;;    +) arg3
;;
;;  +> Variable[1] =
;;    @ type
;;    = default value
;;
;;  +> keywrd Type
;;   +> type part
;;
;;  +>  -> click to see additional information

(defun semantic-sb-one-button (token depth &optional prefix)
  "Insert TOKEN as a speedbar button at DEPTH.
Optional PREFIX is used to specify special marker characters."
  (let* ((type (semantic-token-token token))
	 (edata (cond ((eq type 'type)
		        (semantic-token-type-parts token))
		      ((eq type 'variable)
		       (semantic-token-variable-default token))
		      ((eq type 'function)
		       (semantic-token-function-args token))
		      ))
	 (abbrev (funcall semantic-sb-button-token->text-function token))
	 (start (point))
	 (end (progn
		(insert (int-to-string depth) ":")
		(point))))
    (insert-char ?  (1- depth) nil)
    (put-text-property end (point) 'invisible nil)
    ;; take care of edata = (nil) -- a yucky but hard to clean case
    (if (and edata (listp edata) (and (<= (length edata) 1) (not (car edata))))
	(setq edata nil))
    ;; types are a bit unique.  Variable types can have special meaning.
    (if edata
	(speedbar-insert-button (if prefix (concat " +" prefix) " +>")
				'speedbar-button-face
				'speedbar-highlight-face
				'semantic-sb-show-extra
				token t)
      (speedbar-insert-button (if prefix (concat "  " prefix) " =>")
			      nil nil nil nil t))
    (speedbar-insert-button abbrev
			    'speedbar-tag-face
			    'speedbar-highlight-face
			    'semantic-sb-token-jump
			    token t)
    ;; This is very bizarre.  When this was just after the insertion
    ;; of the depth: text, the : would get erased, but only for the
    ;; auto-expanded short- buckets.  Move back for a later version
    ;; version of Emacs 21 CVS
    (put-text-property start end 'invisible t)
    ))
  
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
    (put-text-property end (point) 'invisible nil)
    (speedbar-insert-button button nil nil nil nil t)
    (speedbar-insert-button text
			    'speedbar-tag-face
			    (if text-fun 'speedbar-highlight-face)
			    text-fun text-data t)
    ))

(defun semantic-sb-maybe-token-to-button (obj indent &optional
					      prefix modifiers)
  "Convert OBJ, which was returned from the bovinator, into a button.
This OBJ might be a plain string (simple type or untyped variable)
or a complete bovinator type.
Argument INDENT is the indentation used when making the button.
Optional PREFIX is the character to use when marking the line.
Optional MODIFIERS is additional text needed for variables."
  (let ((myprefix (or prefix ">")))
    (if (stringp obj)
	(semantic-sb-speedbar-data-line indent myprefix obj)
      (if (listp obj)
	  (progn
	    (if (and (stringp (car obj))
		     (= (length obj) 1))
		(semantic-sb-speedbar-data-line indent myprefix
						(concat
						 (car obj)
						 (or modifiers "")))
	      (semantic-sb-one-button obj indent prefix)))))))

(defun semantic-sb-insert-details (token indent)
  "Insert details about TOKEN at level INDENT."
  (let ((tt (semantic-token-token token))
	(type (semantic-token-type token)))
    (cond ((eq tt 'type)
	   (let ((parts (semantic-token-type-parts token))
		 (newparts nil))
	     ;; Lets expect PARTS to be a list of either strings,
	     ;; or variable tokens.
	     (when (semantic-token-p (car parts))
	       ;; Bucketize into groups
	       (setq newparts (semantic-bucketize parts))
	       (when (> (length newparts) semantic-sb-autoexpand-length)
		 ;; More than one bucket, insert inline
		 (semantic-insert-bovine-list (1- indent) newparts)
		 (setq parts nil))
	       ;; Dump the strings in.
	       (while parts
		 (semantic-sb-maybe-token-to-button (car parts) indent)
		 (setq parts (cdr parts))))))
	  ((eq tt 'variable)
	   (if type
	       (let ((mods (semantic-token-variable-extra-spec token 'typemodifiers)))
		 (semantic-sb-maybe-token-to-button type indent "@" mods)))
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

(defun semantic-sb-detail-parent ()
  "Return the first parent token of the current like that includes a location."
  (save-excursion
    (beginning-of-line)
    (let ((dep (if (looking-at "[0-9]+:")
		   (1- (string-to-int (match-string 0)))
		 0)))
      (re-search-backward (concat "^"
				  (int-to-string dep)
				  ":")
			  nil t))
    (beginning-of-line)
    (if (looking-at "[0-9]+: +[-+][>()@|] \\([^\n]+\\)$")
	(let ((prop nil))
	  (goto-char (match-beginning 1))
	  (setq prop (get-text-property (point) 'speedbar-token))
	  (if (numberp (semantic-token-start prop))
	      prop
	    (semantic-sb-detail-parent)))
      nil)))

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
  (let ((file (speedbar-line-path indent))
	(parent (semantic-sb-detail-parent)))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    (semantic-find-nonterminal token parent)
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    ;; (speedbar-set-timer dframe-update-speed)
    ;;(recenter)
    (speedbar-maybee-jump-to-attached-frame)
    (run-hooks 'speedbar-visiting-tag-hook)))

(defun semantic-sb-expand-group (text token indent)
  "Expand a group which has semantic tokens.
TEXT TOKEN and INDENT are the details."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (save-restriction
	       (narrow-to-region (point-min) (point))
	       (semantic-sb-buttons-plain (1+ indent) token)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun semantic-sb-buttons-plain (level tokens)
  "Create buttons at LEVEL using TOKENS."
  (let ((sordid (speedbar-create-tag-hierarchy tokens)))
    (while sordid
      (cond ((null (car-safe sordid)) nil)
	    ((consp (car-safe (cdr-safe (car-safe sordid))))
	     ;; A group!
	     (speedbar-make-tag-line 'curly ?+ 'semantic-sb-expand-group
				     (cdr (car sordid))
				     (car (car sordid))
				     nil nil 'speedbar-tag-face
				     level))
	    (t ;; Assume that this is a token.
	     (semantic-sb-one-button (car sordid) level)))
      (setq sordid (cdr sordid)))))

(defun semantic-insert-bovine-list (level lst)
  "At LEVEL, insert the bovine parsed list LST.
Use arcane knowledge about the semantic tokens in the tagged elements
to create much wiser decisions about how to sort and group these items."
  (semantic-sb-buttons level lst))

(defun semantic-sb-buttons (level lst)
  "Create buttons at LEVEL using LST sorting into type buckets."
  (save-restriction
    (narrow-to-region (point-min) (point))
    (let (tmp)
      (while lst
	(setq tmp (car lst))
	(if (cdr tmp)
	    (if (<= (length (cdr tmp)) semantic-sb-autoexpand-length)
		(semantic-sb-buttons-plain (1+ level) (cdr tmp))
	      (speedbar-make-tag-line 'curly ?+ 'semantic-sb-expand-group
				      (cdr tmp)
				      (car (car lst))
				      nil nil 'speedbar-tag-face
				      (1+ level))))
	(setq lst (cdr lst))))))

(defun semantic-fetch-dynamic-bovine (file)
  "Load FILE into a buffer, and generate tags using the Semantic Bovinator.
Returns the tag list, or t for an error."
  (let ((out nil))
    (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p)
	     (not speedbar-power-click)
	     ;; If the database is loaded and running, try to get
	     ;; tokens from it.
	     (setq out (semanticdb-file-stream file)))
	;; Successful DB query.
	nil
      ;; No database, do it the old way.
      (save-excursion
	(set-buffer (find-file-noselect file))
	(if (or (not (featurep 'semantic))
		(not semantic-toplevel-bovine-table))
	    (setq out t)
	  (if speedbar-power-click (semantic-clear-toplevel-cache))
	  (setq out (semantic-bovinate-toplevel)))))
    (if (listp out)
	(condition-case nil
	    (progn
	      ;; This brings externally defind methods into
	      ;; their classes, and creates meta classes for
	      ;; orphans.
	      (setq out (semantic-adopt-external-members out))
	      ;; Dump all the tokens into buckets.
	      (semantic-bucketize out))
	  (error t))
      t)))

;; Link ourselves into the tagging process.
(add-to-list 'speedbar-dynamic-tags-function-list
	     '(semantic-fetch-dynamic-bovine  . semantic-insert-bovine-list))

(provide 'semantic-sb)

;;; semantic-sb.el ends here
