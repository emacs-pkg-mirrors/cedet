;;; semantic-imenu.el --- Use the Bovinator as a imenu tag generateor

;;; Copyright (C) 2000 Paul Kinnucan & Eric Ludlam

;; Author: Paul Kinnucan, Eric Ludlam
;; X-RCS: $Id: semantic-imenu.el,v 1.7 2000/09/19 13:45:07 zappo Exp $

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

(defcustom semantic-imenu-bucketize-file t
  "*Non-nil if tokens in a file are to be grouped into buckets."
  :group 'imenu
  :group 'semantic
  :type 'bool)

(defcustom semantic-imenu-buckets-to-submenu t
  "*Non-nil if buckets of tokens are to be turned into submenus.
This option is ignored if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'bool)

(defcustom semantic-imenu-bucketize-type-parts t
  "*Non-nil if elements of a type should be placed grouped into buckets.
Nil means to keep them in the same order.
Overriden to nil if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'bool)

;;; Code:
;;;###autoload
(defun semantic-create-imenu-index (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index.
Optional argument STREAM STREAM is an optional stream of tokens used to create menus."
  (let ((tokens (or stream (semantic-bovinate-toplevel nil t t))))
    (if semantic-imenu-bucketize-file
	(let ((buckets (semantic-bucketize tokens))
	      item name
	      depend-index
	      index)
	  (cond
	   ((null buckets)
	    nil)
	   ((cdr-safe buckets) ;; if buckets has more than one item in it.
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
				  (cons
				   '("---")
				   (semantic-create-imenu-subindex item))))
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
	       (eq (semantic-token-token token) 'type)
               (setq parts (semantic-token-type-parts token)))
          (setq index (cons (cons
                             (funcall semantic-imenu-summary-function token)
                             ;; Add a menu for getting at the type definitions
			     (cons (cons "*typedef*" (semantic-token-start token))
				   (if (and semantic-imenu-bucketize-type-parts
					    semantic-imenu-bucketize-file)
				       (semantic-create-imenu-index parts)
				     (semantic-create-imenu-subindex
				      (reverse parts)))))
                            index))
        (setq index (cons (cons (funcall semantic-imenu-summary-function token)
                                (semantic-token-start token))
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
      index)))

(provide 'semantic-imenu)

;;; semantic-imenu.el ends here
