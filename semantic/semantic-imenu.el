;;; semantic-imenu.el --- Use the Bovinator as a imenu tag generateor

;;; Copyright (C) 2000 Paul Kinnucan & Eric Ludlam

;; Author: Paul Kinnucan, Eric Ludlam
;; X-RCS: $Id: semantic-imenu.el,v 1.15 2000/09/27 02:05:24 zappo Exp $

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
;; This support function can be used in any buffer which supports
;; the bovinator to create the imenu index.
;;
;; To use this in a buffer, do this in a hook.
;;
;; (add-hook 'mode-hook
;;           (lambda ()
;;             (setq imenu-create-index-function 'semantic-create-imenu-index)
;;             ))

(require 'semantic)
(require 'imenu)

(defcustom semantic-imenu-summary-function 'semantic-abbreviate-nonterminal
  "*Function to use when creating items in Imenu.
Some useful functions are:
`semantic-abbreviate-nonterminal'
`semantic-summerize-nonterminal'
`semantic-prototype-nonterminal'"
  :group 'imenu
  :type 'function)
(make-variable-buffer-local 'semantic-imenu-summary-function)

(defcustom semantic-imenu-bucketize-file t
  "*Non-nil if tokens in a file are to be grouped into buckets."
  :group 'imenu
  :group 'semantic
  :type 'bool)
(make-variable-buffer-local 'semantic-imenu-bucketize-file)

(defcustom semantic-imenu-buckets-to-submenu t
  "*Non-nil if buckets of tokens are to be turned into submenus.
This option is ignored if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'bool)
(make-variable-buffer-local 'semantic-imenu-buckets-to-submenu)

(defcustom semantic-imenu-bucketize-type-parts t
  "*Non-nil if elements of a type should be placed grouped into buckets.
Nil means to keep them in the same order.
Overriden to nil if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'bool)
(make-variable-buffer-local 'semantic-imenu-bucketize-type-parts)

(defcustom semantic-imenu-sort-bucket-function nil
  "*Function to use when sorting tags in the buckets of functions."
  :group 'imenu
  :group 'semantic
  :type 'function)
(make-variable-buffer-local 'semantic-imenu-sort-bucket-function)

;;; Code:
(defun semantic-imenu-goto-function (name position &optional rest)
  "Move point associated with NAME to POSITION.
Used to override function `imenu-default-goto-function' so that we can continue
to use overlays to maintain the current position.
Optional argument REST is some extra stuff."
  (imenu-default-goto-function name (semantic-overlay-start position) rest))


;;;###autoload
(defun semantic-create-imenu-index (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index.
Optional argument STREAM STREAM is an optional stream of tokens used to create menus."
  (setq imenu-default-goto-function 'semantic-imenu-goto-function)
  (let ((tokens (or stream (semantic-bovinate-toplevel nil t t))))
    (if semantic-imenu-bucketize-file
	(let ((buckets (semantic-bucketize
			tokens semantic-imenu-sort-bucket-function))
	      item name
	      depend-index
	      index)
	  (cond
	   ((null buckets)
	    nil)
	   ((or (cdr-safe buckets) ;; if buckets has more than one item in it.
                (not semantic-imenu-buckets-to-submenu)) ;; to force separators between buckets
	    (while buckets
	      (setq name (car (car buckets))
		    item (cdr (car buckets)))
	      (if semantic-imenu-buckets-to-submenu
		  (progn
		    ;; Make submenus
		    (if item
			(setq index
			      (cons (cons name
					  (semantic-create-imenu-subindex item))
				    index))))
		;; Glom everything together with "---" between
		(if item
		    (setq index
			  (append index
				  ;; do not create a menu separator in the parent menu
				  ;; when creating a sub-menu
				  (if (eq (semantic-token-token (car item)) 'type)
				      (semantic-create-imenu-subindex item)
				    (cons
				     '("---")
				     (semantic-create-imenu-subindex item)))))
		  ))
	      (setq buckets (cdr buckets)))
	    (if semantic-imenu-buckets-to-submenu
		(nreverse index)
	      index))
	   (t
	    (setq name (car (car buckets))
		  item (cdr (car buckets)))
	    (semantic-create-imenu-subindex item))))
      ;; Else, group everything together
      (semantic-create-imenu-subindex tokens t))))
    

(defun semantic-create-imenu-subindex (tokens &optional notypecheck)
  "From TOKENS, create an imenu index of interesting things.
Optional argument NOTYPECHECK specifies not to make subgroups under types."
  (let (index token parts)
    (while tokens
      (setq token (car tokens))
      (if (and (not notypecheck)
	       (eq (semantic-token-token token) 'type))
          ;; to keep an homogeneous menu organisation, type menu items
          ;; always have a sub-menu with at least the *typedef* item
          ;; (even if the token has no type parts)
          (setq parts (semantic-token-type-parts token)
                index (cons (cons
                             (funcall semantic-imenu-summary-function token)
                             ;; Add a menu for getting at the type definitions
			     (cons (cons "*typedef*" (semantic-token-overlay token))
                                   (if parts
                                       (if (and semantic-imenu-bucketize-type-parts
                                                semantic-imenu-bucketize-file)
                                           (semantic-create-imenu-index parts)
                                         (semantic-create-imenu-subindex
                                          (reverse parts))))))
                            index))
        (setq index (cons (cons (funcall semantic-imenu-summary-function token)
                                (semantic-token-overlay token))
                          index)))
      (setq tokens (cdr tokens)))
    ;; Imenu wasn't capturing this, so add the code from imenu.el
    ;; into this sub-sub section.
    (if imenu-sort-function
	(sort (let ((res nil)
		    (oldlist index))
		;; Copy list method from the cl package `copy-list'
		(while (consp oldlist) (push (pop oldlist) res))
		(if res		; in case, e.g. no functions defined
		    (prog1 (nreverse res) (setcdr res oldlist))))
	      imenu-sort-function)
      (nreverse index))))

;;; Interactive Utilities
;;
(defun semantic-imenu-toggle-bucketize-file ()
  "Toggle the ability of imenu to bucketize the current file."
  (interactive)
  (setq semantic-imenu-bucketize-file (not semantic-imenu-bucketize-file))
  ;; Force a rescan
  (setq imenu--index-alist nil))

(defun semantic-imenu-toggle-buckets-to-submenu ()
  "Toggle the ability of imenu to turn buckets into submenus."
  (interactive)
  (setq semantic-imenu-buckets-to-submenu (not semantic-imenu-buckets-to-submenu))
  ;; Force a rescan
  (setq imenu--index-alist nil))

(defun semantic-imenu-toggle-bucketize-type-parts ()
  "Toggle the ability of imenu to bucketize the current file."
  (interactive)
  (setq semantic-imenu-bucketize-type-parts (not semantic-imenu-bucketize-type-parts))
  ;; Force a rescan
  (setq imenu--index-alist nil))

;;; Which function support
;;
;; The which-function library will display the current function in the
;; mode line.  It tries do do this through imenu.  With a semantic parsed
;; buffer, there is a much more efficient way of doing this.
;; Advise `which-function' so that we optionally use semantic tokens
;; instead, and get better stuff.
(require 'advice)

(defvar semantic-which-function 'semantic-default-which-function
  "Function to convert semantic tokens into `which-function' text.")

(defun semantic-default-which-function (tokenlist)
  "Converts TOKENLIST into a string usable by `which-function'.
Returns the first token name in the list, unless it is a type,
in which case it concatenates them together."
  (cond ((eq (length tokenlist) 1)
	 (semantic-abbreviate-nonterminal (car tokenlist)))
	((eq (semantic-token-token (car tokenlist)) 'type)
	 (concat (semantic-token-name (car tokenlist)) "."
		 ;; recurse until we no longer have a type
		 ;; or any tokens left.
		 (semantic-default-which-function (cdr tokenlist))))
	(t (semantic-abbreviate-nonterminal (car tokenlist)))))

(defadvice which-function (around semantic-which activate)
  "Choose the function to display via semantic if it is currently active."
  (if (and (featurep 'semantic) semantic-toplevel-bovine-cache)
      (let ((ol (semantic-find-nonterminal-by-overlay)))
	(setq ad-return-value (funcall semantic-which-function ol)))
    ad-do-it))

(provide 'semantic-imenu)

;;; semantic-imenu.el ends here
