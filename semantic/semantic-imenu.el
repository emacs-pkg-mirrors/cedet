;;; semantic-imenu.el --- Use the Bovinator as a imenu tag generateor

;;; Copyright (C) 2000 Paul Kinnucan & Eric Ludlam

;; Author: Paul Kinnucan, Eric Ludlam
;; X-RCS: $Id: semantic-imenu.el,v 1.1 2000/06/13 14:26:39 zappo Exp $

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

;;; Code:
(defun semantic-create-imenu-index ()
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index."
  (let* ((tokens (semantic-bovinate-toplevel nil t t))
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
  (let (index token)
    (while tokens
      (setq token (car tokens))
      (if (eq (semantic-token-token token) 'type)
	  (setq index (cons (cons
			     (semantic-abbreviate-nonterminal token)
			     (semantic-create-imenu-subindex
			      (semantic-token-type-parts token)))
			    index))
	(setq index (cons (cons (semantic-abbreviate-nonterminal token)
				(semantic-token-end token))
			  index)))
      (setq tokens (cdr tokens)))
    (nreverse index)))

(provide 'semantic-imenu)

;;; semantic-imenu.el ends here
