;;; semantic-imenu.el --- Use the Bovinator as a imenu tag generator

;;; Copyright (C) 2000, 2001 Paul Kinnucan & Eric Ludlam
;;; Copyright (C) 2001 Eric Ludlam

;; Author: Paul Kinnucan, Eric Ludlam
;; X-RCS: $Id: semantic-imenu.el,v 1.25 2001/02/20 20:23:48 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-imenu is free software; you can redistribute it and/or modify
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
(eval-when-compile (require 'semanticdb)
		   (require 'working))
(condition-case nil
    (progn
      (require 'imenu)
      (if (featurep 'xemacs)
	  ;; From David Ponce <david@dponce.com>
	  ;; Advice the XEmacs imenu function `imenu--create-menu-1' to
	  ;; handle menu separators.
	  (defadvice imenu--create-menu-1 (around semantic activate)
	    ;; Create a menu separator item if first argument (title)
	    ;; contains only hyphens and the second argument (list) is nil.
	    (if (and (null (ad-get-arg 1))
		     (not (string-match "[^-]" (ad-get-arg 0))))
		(setq ad-return-value "-")
	      ad-do-it)))
      )
  (error nil))


(defcustom semantic-imenu-summary-function 'semantic-abbreviate-nonterminal
  "*Function to use when creating items in Imenu.
Some useful functions are:
`semantic-abbreviate-nonterminal'
`semantic-summarize-nonterminal'
`semantic-prototype-nonterminal'"
  :group 'imenu
  :type 'function)
(make-variable-buffer-local 'semantic-imenu-summary-function)

(defcustom semantic-imenu-bucketize-file t
  "*Non-nil if tokens in a file are to be grouped into buckets."
  :group 'imenu
  :group 'semantic
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-bucketize-file)

(defcustom semantic-imenu-buckets-to-submenu t
  "*Non-nil if buckets of tokens are to be turned into submenus.
This option is ignored if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-buckets-to-submenu)

(defcustom semantic-imenu-expand-type-parts t
  "*Non-nil if types should have submenus with parts in it."
  :group 'imenu
  :group 'semantic
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-expand-type-parts)

(defcustom semantic-imenu-bucketize-type-parts t
  "*Non-nil if elements of a type should be placed grouped into buckets.
Nil means to keep them in the same order.
Overriden to nil if `semantic-imenu-bucketize-file' is nil."
  :group 'imenu
  :group 'semantic
  :type 'boolean)
(make-variable-buffer-local 'semantic-imenu-bucketize-type-parts)

(defcustom semantic-imenu-sort-bucket-function nil
  "*Function to use when sorting tags in the buckets of functions."
  :group 'imenu
  :group 'semantic
  :type 'function)
(make-variable-buffer-local 'semantic-imenu-sort-bucket-function)

(defcustom semantic-imenu-index-directory t
  "*Non nil to index the entire directory for tags.
Doesn't actually parse the entire directory, but displays tags for all files
currently listed in the current Semantic database.
This variable has no meaning if semanticdb is not active."
  :group 'imenu
  :group 'semantic
  :type 'boolean)

(defcustom semantic-imenu-auto-rebuild-directory-indexes t
  "*If non-nil automatically rebuild directory index imenus.
That is when a directory index imenu is updated, automatically rebuild
other buffer local ones based on the same semanticdb."
  :group 'semantic
  :type 'boolean)

(defvar semantic-imenu-directory-current-file nil
  "When building a file index, this is the file name currently being built.")

(defvar semantic-imenu-auto-rebuild-running nil
  "Non-nil if `semantic-imenu-rebuild-directory-indexes' is running.")

(defvar semantic-imenu-expandable-token 'type
  "Tokens of this token type will be given submenu with children.
By default, a `type' has interesting children.  In Texinfo, however,
a `section' has interesting children.")
(make-variable-buffer-local 'semantic-imenu-expandable-token)

;;; Code:
(defun semantic-imenu-token-overlay (token)
  "Return the overlay belonging to TOKEN.
If TOKEN doesn't have an overlay, and instead as a vector of positions,
concoct a combination of file name, and position."
  (let ((o (semantic-token-overlay token)))
    (if (not (semantic-overlay-p o))
	(let ((v (make-vector 3 nil)))
	  (aset v 0 semantic-imenu-directory-current-file)
	  (aset v 1 (aref o 0))
	  (aset v 2 (aref o 1))
	  v)
      o)))

(defun semantic-imenu-goto-function (name position &optional rest)
  "Move point associated with NAME to POSITION.
Used to override function `imenu-default-goto-function' so that we can continue
to use overlays to maintain the current position.
Optional argument REST is some extra stuff."
  (if (semantic-overlay-p position)
      (let ((os (semantic-overlay-start position))
	    (ob (semantic-overlay-buffer position)))
	(if os
	    (progn
	      (if (not (eq ob (current-buffer)))
		  (switch-to-buffer ob))
	      (imenu-default-goto-function name os rest))
	  ;; This should never happen, but check anyway.
	  (message "Imenu is out of date, try again. (internal bug)")
	  (setq imenu--index-alist nil)))
    ;; When the POSITION is actually a pair of numbers in an array, then
    ;; the file isn't loaded into the current buffer.
    (if (vectorp position)
	(let ((file (aref position 0))
	      (pos (aref position 1)))
	  (find-file file)
	  (imenu-default-goto-function name pos rest))
      ;; When the POSITION is the symbol 'file-only' it means that this
      ;; is a directory index entry and there is no tokens in this
      ;; file. So just jump to the beginning of the file.
      (if (eq position 'file-only)
	  (progn
	    (find-file name)
	    (imenu-default-goto-function name (point-min) rest))
	(message "Semantic Imenu override problem. (Internal bug)")
	(setq imenu--index-alist nil)))
    ))

(defun semantic-imenu-flush-fcn (&optional ignore)
  "This function is called as a hook to clear the imenu cache.
This is added to `semantic-before-toplevel-cache-flush-hook' and
`semantic-clean-token-hooks'.  IGNORE arguments."
  (if (eq imenu-create-index-function 'semantic-create-imenu-index)
      (setq imenu--index-alist nil))
  (remove-hook 'semantic-before-toplevel-cache-flush-hook
	       'semantic-imenu-flush-fcn)
  (remove-hook 'semantic-clean-token-hooks
	       'semantic-imenu-flush-fcn)
  )

;;;###autoload
(defun semantic-create-imenu-index (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index.
Optional argument STREAM is an optional stream of tokens used to create menus."
  (setq imenu-default-goto-function 'semantic-imenu-goto-function)
  (add-hook 'semantic-before-toplevel-cache-flush-hook
	    'semantic-imenu-flush-fcn nil t)
  (add-hook 'semantic-clean-token-hooks
	    'semantic-imenu-flush-fcn nil t)
  (if (and semantic-imenu-index-directory (featurep 'semanticdb)
	   (semanticdb-minor-mode-p))
      (semantic-create-imenu-directory-index stream)
    (semantic-create-imenu-index-1 stream)))

(defun semantic-create-imenu-directory-index (&optional stream)
  "Create an IMENU tag index based on all files active in semanticdb.
Optional argument STREAM is the stream of tokens for the current buffer."
  (if (not semanticdb-current-database)
      (semantic-create-imenu-index-1 stream)
    ;; We have a database, list all files, with the current file on top.
    (let ((index (list
		  (cons (oref semanticdb-current-table file)
			(or (semantic-create-imenu-index-1 stream)
			    ;; No tokens in this file
			    'file-only))))
	  (tables (oref semanticdb-current-database tables)))
      (working-status-forms "Imenu Directory Index" "done"
	(while tables
	  (let ((semantic-imenu-directory-current-file
		 (oref (car tables) file))
		tokens)
	    (when (and (not (eq (car tables) semanticdb-current-table))
		       (semanticdb-live-p (car tables))
		       (eq major-mode (oref (car tables) major-mode))
		       )
	      (setq tokens (oref (car tables) tokens)
		    index (cons (cons semantic-imenu-directory-current-file
				      (or (and tokens
					       ;; don't pass nil stream because
					       ;; it will use the current
					       ;; buffer
					       (semantic-create-imenu-index-1
						(oref (car tables) tokens)))
					  ;; no tokens in the file
					  'file-only))
				index)))
	    (setq tables (cdr tables)))
	  (working-dynamic-status))
	(working-dynamic-status t))
      
      ;; If enabled automatically rebuild other imenu directory
      ;; indexes based on the same Semantic database
      (or (not semantic-imenu-auto-rebuild-directory-indexes)
          ;; If auto rebuild already in progress does nothing
          semantic-imenu-auto-rebuild-running
          (unwind-protect
              (progn
                (setq semantic-imenu-auto-rebuild-running t)
                (semantic-imenu-rebuild-directory-indexes
                 semanticdb-current-database))
            (setq semantic-imenu-auto-rebuild-running nil)))
      
      (nreverse index))))

(defun semantic-create-imenu-index-1 (&optional stream)
  "Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic Bovinator to create the index.
Optional argument STREAM is an optional stream of tokens used to create menus."
  (let ((tokens (or stream (semantic-bovinate-toplevel t))))
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
				  (if (eq (semantic-token-token (car item))
					  semantic-imenu-expandable-token)
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
      (semantic-create-imenu-subindex tokens))))
    

(defun semantic-create-imenu-subindex (tokens)
  "From TOKENS, create an imenu index of interesting things."
  (let ((notypecheck (not semantic-imenu-expand-type-parts))
	index token parts)
    (while tokens
      (setq token (car tokens))
      (if (and (not notypecheck)
	       (eq (semantic-token-token token)
		   semantic-imenu-expandable-token))
          ;; to keep an homogeneous menu organisation, type menu items
          ;; always have a sub-menu with at least the *typedef* item
          ;; (even if the token has no type parts)
          (setq parts (semantic-nonterminal-children token)
                index
		(cons
		 (cons
		  (funcall semantic-imenu-summary-function token)
		  ;; Add a menu for getting at the type definitions
		  (if (and parts
			   ;; Note to self: enable menu items for sub parts
			   ;; even if they are not proper tokens.
			   (semantic-token-p (car parts)))
		      (cons (cons "*definition*" 
				  (semantic-imenu-token-overlay token))
			    (if (and semantic-imenu-bucketize-type-parts
				     semantic-imenu-bucketize-file)
				(semantic-create-imenu-index-1 parts)
			      (semantic-create-imenu-subindex
			       (reverse parts))))
		    ;; There were no parts, or something like that, so
		    ;; instead just put the definition here.
		    (semantic-imenu-token-overlay token)
		    ))
		 index))
        (setq index (cons (cons (funcall semantic-imenu-summary-function token)
                                (semantic-imenu-token-overlay token))
                          index)))
      (setq tokens (cdr tokens)))
    (setq index
	  ;; Imenu wasn't capturing this, so add the code from imenu.el
	  ;; into this sub-sub section.
	  (if imenu-sort-function
	      (sort (let ((res nil)
			  (oldlist index))
		      ;; Copy list method from the cl package `copy-list'
		      (while (consp oldlist) (push (pop oldlist) res))
		      (if res	  ; in case, e.g. no functions defined
			  (prog1 (nreverse res) (setcdr res oldlist))))
		    imenu-sort-function)
	    (nreverse index)))
    (if (> (length index) imenu-max-items)
	(let ((count 0))
	  (setq index
		(mapcar
		 (function
		  (lambda (menu)
		    (cons (format "From: %s" (caar menu)) menu)))
		 (imenu--split index imenu-max-items)))))
    index))

;;; directory imenu rebuilding.
;;
(defun semantic-imenu-rebuild-directory-indexes (db)
  "Rebuild directory index imenus based on Semantic database DB."
  (let ((l (buffer-list))
        b)
    (while l
      (setq b (car l)
            l (cdr l))
      (if (and (not (eq b (current-buffer)))
               (buffer-live-p b))
          (with-current-buffer b
            ;; If there is a buffer local Semantic index directory
            ;; imenu
            (when (and (eq imenu-create-index-function
                           'semantic-create-imenu-index)
                       semanticdb-current-database
                       (eq semanticdb-current-database db))
              (message "Building %s Semantic directory index imenu"
                       (buffer-name b))
              ;; Rebuild the imenu
              (imenu--cleanup)
              (setq imenu--index-alist nil)
              (funcall
               (if (fboundp 'imenu-menu-filter)
                   ;; XEmacs imenu
                   'imenu-menu-filter
                 ;; Emacs imenu
                 'imenu-update-menubar))))))))

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
	((eq (semantic-token-token (car tokenlist))
	     semantic-imenu-expandable-token)
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
