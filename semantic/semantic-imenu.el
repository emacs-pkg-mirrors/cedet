;;; semantic-imenu.el --- Use the Bovinator as a imenu tag generateor

;;; Copyright (C) 2000 Paul Kinnucan & Eric Ludlam

;; Author: Paul Kinnucan, Eric Ludlam
;; X-RCS: $Id: semantic-imenu.el,v 1.4 2000/07/04 18:33:53 zappo Exp $

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

(defcustom semantic-imenu-bucketize-type-parts t
  "*Non-nil if elements of a type should be placed grouped into buckets.
Nil means to keep them in the same order."
  :group 'imenu
  :type 'bool)

;;; Code:
(defun semantic-create-imenu-index (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index."
  (let* ((tokens (or stream (semantic-bovinate-toplevel nil t t)))
	 (buckets (semantic-bucketize tokens))
	 item name
	 depend-index
	 index)
  (while buckets
    (setq name (car (car buckets))
	  item (cdr (car buckets))
	  index (if item
		    (cons (cons name (semantic-create-imenu-subindex item))
			  index)
		  index)
	  buckets (cdr buckets)))
  (nreverse index)))
	    
(defun semantic-create-imenu-subindex (tokens)
  "From TOKENS, create an imenu index of interesting things."
  (let (index token parts)
    (while tokens
      (setq token (car tokens))
      (if (and (eq (semantic-token-token token) 'type)
               (setq parts (semantic-token-type-parts token)))
          (setq index (cons (cons
                             (funcall semantic-imenu-summary-function
				      token)
                             (if semantic-imenu-bucketize-type-parts
                                 (semantic-create-imenu-index parts)
                               (semantic-create-imenu-subindex parts)))
                            index))
        (setq index (cons (cons (funcall semantic-imenu-summary-function token)
                                (semantic-token-end token))
                          index)))
      (setq tokens (cdr tokens)))
    (nreverse index)))

(provide 'semantic-imenu)

;;; semantic-imenu.el ends here
