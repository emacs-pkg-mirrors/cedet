;;; semantic-texi.el --- Semantic details for Texinfo files

;;; Copyright (C) 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-texi.el,v 1.1 2001/02/20 21:34:11 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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
;; Example using semantic's `semantic-toplevel-bovinate-override'
;; using texinfo files.
;;

(require 'semantic)

(defvar semantic-texi-super-regex
  "^@\\(chapter\\|\\(sub\\)*section\\|unnumbered\\(\\(sub\\)*sec\\)?\\|\
\\(chap\\|\\(sub\\)+\\|major\\)?heading\\|appendix\\(\\(sub\\)*sec\\)?\\|\
centerchap\\|def\\(var\\|un\\|fn\\)x?\\)"
  "Regular expression used to find special sections in a Texinfo file.")

(defvar semantic-texi-name-field-list
  '( ("defvar" . 1)
     ("defun" . 1)
     ("deffn" . 2)
     )
  "List of definition commands, and the field position.
The field position is the field number (based at 1) where the
name of this section is.")

;;; Code:
(defun semantic-texi-bovinate-toplevel (checkcache)
  "Parse the current texinfo buffer for bovine tokens.
If CHECKCACHE is non-nil, then check to see if the cache needs
to be flushed.  (ignored)
Each token returned is of the form:
 (\"NAME\" section children DOC OVERLAY)
or
 (\"NAME\" def DOC OVERLAY)"
  (mapcar 'semantic-texi-raw-to-cooked-token  (semantic-texi-bovinate-headings))
  )

(defun semantic-texi-raw-to-cooked-token (token)
  "Cook the texinfo token TOKEN."
  (let ((chil (semantic-texi-nonterminal-children token)))
    (if chil
	(setcar (nthcdr 2 token)
		(mapcar 'semantic-texi-raw-to-cooked-token chil)))
    (car (semantic-raw-to-cooked-token token))))

(defun semantic-texi-bovinate-headings ()
  "Parse the current texinfo buffer for all bovine tokens now."
  (let ((pass1 nil))
    ;; First search and snarf.
    (save-excursion
      (goto-char (point-min))
      (working-status-forms (file-name-nondirectory buffer-file-name) "done"
	(while (re-search-forward semantic-texi-super-regex nil t)
	  (setq pass1 (cons (match-beginning 0) pass1))
	  (working-status)
	  )
	(working-status t)))
    (setq pass1 (nreverse pass1))
    ;; Now, make some tokens while creating a set of children.
    (car (semantic-texi-recursive-combobulate-list pass1 0))
    ))

(defun semantic-texi-recursive-combobulate-list (sectionlist level)
  "Re-arrange SECTIONLIST to be a hierarchical token list starting at LEVEL.
Return the re-arranged new list, with all remaining tokens from
SECTIOLIST starting at ELT 2.  Sections not are not dealt with as soon as a
token with greater section value than LEVEL is found."
  (let ((newl nil)
	(oldl sectionlist)
	tmp)
    (save-excursion
      (catch 'level-jump
	(while oldl
	  (goto-char (car oldl))
	  (if (looking-at "@\\(\\w+\\)")
	      (let* ((word (match-string 1))
		     (levelmatch (assoc word texinfo-section-list))
		     text begin end tmp
		     )
		;; Get out of here if there if we made it that far.
		(if (and levelmatch (<= (car (cdr levelmatch)) level))
		    (throw 'level-jump t))
		(setq begin (point))
		;; Recombobulate
		(if levelmatch
		    (progn
		      ;; When there is a match, the descriptive text
		      ;; consists of the rest of the line.
		      (goto-char (match-end 1))
		      (skip-chars-forward " \t")
		      (setq text (buffer-substring-no-properties
				  (point)
				  (progn (end-of-line) (point))))
		      ;; Next, recurse into the body to find the end.
		      (setq tmp (semantic-texi-recursive-combobulate-list
				 (cdr oldl) (car (cdr levelmatch))))
		      ;; Build a token
		      (setq newl (cons
				  (list text 'section (car tmp) nil begin (point))
				  newl))
		      ;; continue
		      (setq oldl (cdr tmp))
		      )
		  ;; No match means we have a def*, so get the name from
		  ;; it based on the type of thingy we found.
		  (setq levelmatch (assoc word semantic-texi-name-field-list)
			tmp (or (cdr levelmatch) 1))
		  (forward-sexp tmp)
		  (skip-chars-forward " \t")
		  (setq text (buffer-substring-no-properties
			      (point)
			      (progn (forward-sexp 1) (point))))
		  ;; Seek the end of this definition
		  (goto-char begin)
		  (semantic-texi-forward-deffn)
		  (setq newl (cons
			      (list text 'def nil begin (point))
			      newl))
		  ;; continue
		  (setq oldl (cdr oldl)))
		)
	    (error "Problem finding section in semantic/texi parser"))
	  ;; (setq oldl (cdr oldl))
	  )))
    (cons newl oldl)))

(defun semantic-texi-forward-deffn ()
  "Move forward over one deffn type definition.
The cursor should be on the @ sign."
  (when (looking-at "@\\(\\w+\\)")
    (let* ((type (match-string 1))
	   (seek (concat "^@end\\s-+" (regexp-quote type))))
      (re-search-forward seek nil t))))

(defun semantic-texi-nonterminal-children (nonterm)
  "Return children belonging to NONTERM."
  (if (eq (semantic-token-token nonterm) 'section)
      (nth 2 nonterm)
    nil))

(defun semantic-default-texi-setup ()
  "Set up a buffer for parsing of Texinfo files."
  ;; This will use our parser.
  (setq semantic-toplevel-bovinate-override #'semantic-texi-bovinate-toplevel
	imenu-create-index-function 'semantic-create-imenu-index
	semantic-command-separation-character "@"
	semantic-symbol->name-assoc-list '((section . "Section")
					   (def . "Definition")
					   )
	semantic-imenu-expandable-token 'section
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-parts nil
	)
  (semantic-install-function-overrides
   '((nonterminal-children . semantic-texi-nonterminal-children)
     )
   t)
  )

(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)

(provide 'semantic-texi)

;;; semantic-texi.el ends here
