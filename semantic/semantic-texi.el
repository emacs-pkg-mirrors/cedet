;;; semantic-texi.el --- Semantic details for Texinfo files

;;; Copyright (C) 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-texi.el,v 1.8.2.3 2002/12/26 11:07:13 ponced Exp $

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
(require 'texinfo)

(eval-when-compile
  (require 'semanticdb)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator))

(defvar semantic-texi-super-regex
  "^@\\(chapter\\|\\(sub\\)*section\\|unnumbered\\(\\(sub\\)*sec\\)?\\|\
\\(chap\\|\\(sub\\)+\\|major\\)?heading\\|appendix\\(\\(sub\\)*sec\\)?\\|\
centerchap\\|def\\(var\\|un\\|fn\\|opt\\)x?\\)"
  "Regular expression used to find special sections in a Texinfo file.")

(defvar semantic-texi-name-field-list
  '( ("defvar" . 1)
     ("defvarx" . 1)
     ("defun" . 1)
     ("defunx" . 1)
     ("defopt" . 1)
     ("deffn" . 2)
     ("deffnx" . 2)
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
  ;;(semantic-texi-bovinate-headings)
  (let ((lst (mapcar 'semantic-texi-raw-to-cooked-token
		     (semantic-texi-bovinate-headings))))
    (semantic-overlay-list lst)
    lst)
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
  "Rearrange SECTIONLIST to be a hierarchical token list starting at LEVEL.
Return the rearranged new list, with all remaining tokens from
SECTIONLIST starting at ELT 2.  Sections not are not dealt with as soon as a
token with greater section value than LEVEL is found."
  (let ((newl nil)
	(oldl sectionlist)
	)
    (save-excursion
      (catch 'level-jump
	(while oldl
	  (goto-char (car oldl))
	  (if (looking-at "@\\(\\w+\\)")
	      (let* ((word (match-string 1))
		     (levelmatch (assoc word texinfo-section-list))
		     text begin tmp
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
    (cons (nreverse newl) oldl)))

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

(defun semantic-texi-insert-foreign-token (token tokenfile)
  "Insert TOKEN from a foreign buffer in TOKENFILE.
Assume TOKENFILE is a source buffer, and create a documentation
thingy from it using the `document' tool."
  ;; This makes sure that TOKEN will be in an active buffer.
  (let ((b (find-file-noselect tokenfile)))
    ;; Now call the document insert thingy.
    (require 'document)
    (document-insert-texinfo token b)))

;;;###autoload
(defun semantic-default-texi-setup ()
  "Set up a buffer for parsing of Texinfo files."
  ;; This will use our parser.
  (setq semantic-bovinate-toplevel-override #'semantic-texi-bovinate-toplevel
	imenu-create-index-function 'semantic-create-imenu-index
	semantic-command-separation-character "@"
	semantic-type-relation-separator-character '(":")
	semantic-symbol->name-assoc-list '((section . "Section")
					   (def . "Definition")
					   )
	semantic-imenu-expandable-token 'section
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-parts nil
	senator-step-at-start-end-token-ids '(section)
	)
  (semantic-install-function-overrides
   '((nonterminal-children . semantic-texi-nonterminal-children)
     (insert-foreign-token . semantic-texi-insert-foreign-token)
     )
   t)
  )

(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)


;;; Special features of Texinfo token streams
;;
;; This section provides specialized access into texinfo files.
;; Because texinfo files often directly refer to functions and programs
;; it is useful to access the texinfo file from the C code for document
;; maintainance.
(defun semantic-texi-associated-files (&optional buffer)
  "Find texinfo files associated with BUFFER."
  (save-excursion
    (if buffer (set-buffer buffer))
    (cond ((and (fboundp 'ede-documentation-files) ede-minor-mode (ede-current-project))
	   ;; When EDE is active, ask it.
	   (ede-documentation-files)
	   )
	  ((and (featurep 'semanticdb) (semanticdb-minor-mode-p))
	   ;; See what texinfo files we have loaded in the database
	   (let ((tabs (oref semanticdb-current-database tables))
		 (r nil))
	     (while tabs
	       (if (eq (oref (car tabs) major-mode) 'texinfo-mode)
		   (setq r (cons (oref (car tabs) file) r)))
	       (setq tabs (cdr tabs)))
	     r))
	  (t
	   (directory-files default-directory nil "\\.texi$"))
	  )))

;; Turns out this might not be useful.
;; Delete later if that is true.
(defun semantic-texi-find-documentation (name &optional type)
  "Find the function or variable NAME of TYPE in the texinfo source.
NAME is a string representing some functional symbol.
TYPE is a string, such as \"variable\" or \"Command\" used to find
the correct definition in case NAME qualifies as several things.
When this function exists, POINT is at the definition.
If the doc was not found, an error is thrown.
Note: TYPE not yet implemented."
  (let ((f (semantic-texi-associated-files))
	stream
	match)
    (while (and f (not match))
      (when (not stream)
	(save-excursion
	  (set-buffer (find-file-noselect (car f)))
	  (setq stream (semantic-bovinate-toplevel t))))
      (setq match (semantic-find-nonterminal-by-name name stream t nil))
      (when match
	(set-buffer (semantic-token-buffer match))
	(goto-char (semantic-token-start match)))
      (setq f (cdr f)))))

(defun semantic-texi-update-doc-from-texi (&optional token)
  "Update the documentation in the texinfo deffn class token TOKEN.
The current buffer must be a texinfo file containing TOKEN.
If TOKEN is nil, determine a token based on the current position."
  (interactive)
  (if (not (or (featurep 'semanticdb) (semanticdb-minor-mode-p)))
      (error "Texinfo updating only works when `semanticdb' is being used"))
  (semantic-bovinate-toplevel t)
  (when (not token)
    (beginning-of-line)
    (setq token (semantic-current-nonterminal)))
  (if (not (eq (semantic-token-token token) 'def))
      (error "Only deffns (or defun or defvar) can be updated"))
  (let* ((name (semantic-token-name token))
	 (toks (mapcar
                #'cdr
                ;; `semanticdb-find-nonterminal-by-name' returns a
                ;; list ((DB-TABLE . TOKEN) ...)
                (semanticdb-find-nonterminal-by-name name nil t nil t t)))
	 (docstring nil)
	 (doctok nil))
    (save-excursion
      (while (and toks (not docstring))
	(set-buffer (semantic-token-buffer (car toks)))
	(when (not (eq major-mode 'texinfo-mode))
	  (setq docstring (semantic-find-documentation (car toks))
		doctok (if docstring (car toks) nil)))
	(setq toks (cdr toks))))
    (if (not docstring)
	(error "Could not find documentation for %s" (semantic-token-name token)))
    ;; If we have a string, do the replacement.
    (delete-region (semantic-token-start token)
		   (semantic-token-end token))
    ;; Use useful functions from the document library.
    (require 'document)
    (document-insert-texinfo doctok (semantic-token-buffer doctok))
    ))

(defun semantic-texi-update-doc-from-source (&optional token)
  "Update the documentation for the source TOKEN.
The current buffer must be a non-texinfo source file containing TOKEN.
If TOKEN is nil, determine the token based on the current position.
The current buffer must include TOKEN."
  (interactive)
  (if (eq major-mode 'texinfo-mode)
      (error "Not a source file"))
  (semantic-bovinate-toplevel t)
  (when (not token)
    (setq token (semantic-current-nonterminal)))
  (when (not (semantic-find-documentation token))
    (error "Cannot find interesting documentation to use for %s"
	   (semantic-token-name token)))
  (let* ((name (semantic-token-name token))
	 (texi (semantic-texi-associated-files))
	 (doctok nil)
	 (docbuff nil))
    (while (and texi (not doctok))
      (set-buffer (find-file-noselect (car texi)))
      (setq doctok (semantic-find-nonterminal-by-name
		    name (semantic-bovinate-toplevel t) t nil)
	    docbuff (if doctok (current-buffer) nil))
      (setq texi (cdr texi)))
    (if (not doctok)
	(error "Token %s is not yet documented.  Use the `document' command"
	       name))
    ;; Ok, we should have everything we need.  Do the deed.
    (if (get-buffer-window docbuff)
	(set-buffer docbuff)
      (switch-to-buffer docbuff))
    (goto-char (semantic-token-start doctok))
    (delete-region (semantic-token-start doctok)
		   (semantic-token-end doctok))
    ;; Use useful functions from the document library.
    (require 'document)
    (document-insert-texinfo token (semantic-token-buffer token))
    ))

(defun semantic-texi-update-doc (&optional token)
  "Update the documentation for TOKEN.
If the current buffer is a texinfo file, then find the source doc, and
update it.  If the current buffer is a source file, then get the
documentation for this item, find the existing doc in the associated
manual, and update that."
  (interactive)
  (cond ((eq major-mode 'texinfo-mode)
	 (semantic-texi-update-doc-from-texi token))
	(t
	 (semantic-texi-update-doc-from-source token))))

(defun semantic-texi-goto-source (&optional token)
  "Jump to the source for the definition in the texinfo file TOKEN.
If TOKEN is nil, it is derived from the deffn under POINT."
  (interactive)
  (if (not (or (featurep 'semanticdb) (semanticdb-minor-mode-p)))
      (error "Texinfo updating only works when `semanticdb' is being used"))
  (semantic-bovinate-toplevel t)
  (when (not token)
    (beginning-of-line)
    (setq token (semantic-current-nonterminal)))
  (if (not (eq (semantic-token-token token) 'def))
      (error "Only deffns (or defun or defvar) can be updated"))
  (let* ((name (semantic-token-name token))
	 (toks (mapcar
                #'cdr
                ;; `semanticdb-find-nonterminal-by-name' returns a
                ;; list ((DB-TABLE . TOKEN) ...)
                (semanticdb-find-nonterminal-by-name name nil t nil t t)))
	 (done nil)
	 )
    (save-excursion
      (while (and toks (not done))
	(set-buffer (semantic-token-buffer (car toks)))
	(when (not (eq major-mode 'texinfo-mode))
	  (switch-to-buffer (semantic-token-buffer (car toks)))
	  (goto-char (semantic-token-start (car toks)))
	  (setq done t))
	(setq toks (cdr toks))))))

(provide 'semantic-texi)

;;; semantic-texi.el ends here
