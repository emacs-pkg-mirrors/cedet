;;; semanticdb-el.el --- Semantic database extensions for Emacs Lisp

;;; Copyright (C) 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-el.el,v 1.11 2003/08/29 16:09:18 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
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
;; 
;;; Commentary:
;;
;; There are a lot of Emacs Lisp functions and variables available for
;; the asking.  This adds on to the semanticdb programming interface to
;; allow all loaded Emacs Lisp functions to be queried via semanticdb.
;;
;; This allows you to use programs written for Semantic using the database
;; to also work in Emacs Lisp with no compromises.
;;

(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )
;;; Code:

;;; Classes:
(defclass semanticdb-table-emacs-lisp (semanticdb-search-results-table)
  ((major-mode :initform emacs-lisp-mode)
   )
  "A table for returning search results from Emacs.")

(defclass semanticdb-project-database-emacs-lisp
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-emacs-lisp
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Emacs core.")

;; Create the database, and add it to searchable databases for Emacs Lisp mode.
(defvar-mode-local emacs-lisp-mode semanticdb-project-system-databases
  (list 
   (semanticdb-project-database-emacs-lisp "Emacs"))
  "Search Emacs core for symbols.")

;;; Filename based methods
;;
(defmethod semanticdb-file-table ((obj semanticdb-project-database-emacs-lisp) filename)
  "From OBJ, return FILENAME's associated table object.
For Emacs Lisp, creates a specialized table."
  ;; Scan for it on the load path.  Make a nice table for it.
  (if (slot-boundp obj 'tables)
      (car (oref obj tables))
    (let ((newtable (semanticdb-table-emacs-lisp filename)))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      newtable)))

(defmethod semanticdb-get-tags ((table semanticdb-table-emacs-lisp ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-emacs-lisp) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (eq major-mode 'emacs-lisp-mode)))

;;; Conversion
;;
(defun semanticdb-elisp-sym-function-arglist (sym)
  "Get the argument list for SYM.
Deal with all different forms of function.
This was snarfed out of eldoc."
  (let* ((prelim-def
	  (let ((sd (and (fboundp sym)
			 (symbol-function sym))))
	    (and (symbolp sd)
		 (condition-case err
		     (setq sd (indirect-function sym))
		   (error (setq sd nil))))
	    sd))
         (def (if (eq (car-safe prelim-def) 'macro)
                  (cdr prelim-def)
                prelim-def))
         (arglist (cond ((null def) nil)
			((byte-code-function-p def)
                         (cond ((fboundp 'compiled-function-arglist)
                                (funcall 'compiled-function-arglist def))
                               (t
                                (aref def 0))))
                        ((eq (car-safe def) 'lambda)
                         (nth 1 def))
                        (t nil))))
    arglist))

(defun semanticdb-elisp-sym->tag (sym &optional toktype)
  "Convert SYM into a semantic tag.
TOKTYPE is a hint to the type of tag desired."
  (if (stringp sym)
      (setq sym (intern-soft sym)))
  (when sym
    (cond ((and (eq toktype 'function) (fboundp sym))
	   (semantic-tag-new-function
	    (symbol-name sym)
	    nil	;; return type
	    (semantic-elisp-desymbolify
	     (semanticdb-elisp-sym-function-arglist sym)) ;; arg-list
	    'user-visible (interactive-form sym)
	    ))
	  ((and (eq toktype 'variable) (boundp sym))
	   (semantic-tag-new-variable
	    (symbol-name sym)
	    nil	;; type
	    nil	;; value - ignore for now
	    ))
	  ((and (eq toktype 'type) (class-p sym))
	   (semantic-tag-new-type
	    (symbol-name sym)
	    "class"
	    (semantic-elisp-desymbolify
	     (aref (class-v semanticdb-project-database)
		   class-public-a)) ;; slots
	    (semantic-elisp-desymbolify (class-parents sym)) ;; parents
	    ))
	  ((not toktype)
	   ;; Figure it out on our own.
	   (cond ((class-p sym)
		  (semanticdb-elisp-sym->tag sym 'type))
		 ((fboundp sym)
		  (semanticdb-elisp-sym->tag sym 'function))
		 ((boundp sym)
		  (semanticdb-elisp-sym->tag sym 'variable))
		 (t nil))
	   )
	  (t nil))))

;;; Search Overrides
;;
(defvar semanticdb-elisp-mapatom-collector nil
  "Variable used to collect mapatoms output.")

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-emacs-lisp) name)
  "Find all tags name NAME in TABLE.
Uses `inter-soft' to match NAME to emacs symbols.
Return a list of tags."
  ;; No need to search.  Use `intern-soft' which does the same thing for us.
  (let* ((sym (intern-soft name))
	 (fun (semanticdb-elisp-sym->tag sym 'function))
	 (var (semanticdb-elisp-sym->tag sym 'variable))
	 (typ (semanticdb-elisp-sym->tag sym 'type))
	 (taglst nil)a
	 )
    (when (or fun var typ)
      ;; If the symbol is any of these things, build the search table.
      (when var	(setq taglst (cons var taglst)))
      (when typ	(setq taglst (cons typ taglst)))
      (when fun	(setq taglst (cons fun taglst)))
      taglst
      )))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-emacs-lisp) regex)
  "Find all tags with name matching REGEX in TABLE.
Uses `apropos-internal' to find matches.
Return a list of tags."
  (delq nil (mapcar 'semanticdb-elisp-sym->tag
		    (apropos-internal regex))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-emacs-lisp) prefix)
  "In TABLE, find all occurances of tags matching PREFIX.
Returns a table of all matching tags."
  (delq nil (mapcar 'semanticdb-elisp-sym->tag
		    (all-completions prefix obarray))))

;;; Deep Searches
;;
;; For Emacs Lisp deep searches are like top level searches.
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-emacs-lisp) name)
  "Find all tags name NAME in TABLE.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-method table name))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-emacs-lisp) regex)
  "Find all tags with name matching REGEX in TABLE.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-regexp-method table regex))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-emacs-lisp) prefix)
  "In TABLE, find all occurances of tags matching PREFIX.
Like `semanticdb-find-tags-for-completion-method' for Emacs Lisp."
  (semanticdb-find-tags-for-completion-method table prefix))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-emacs-lisp) type)
  "Find all nonterminals which are child elements of TYPE
Return a list of tags."
  ;; EIEIO is the only time this matters
  (when (featurep 'eieio)
    (let* ((class (intern-soft type))
	   (taglst (when class
		     (delq nil
			   (mapcar 'semanticdb-elisp-sym->tag
				   ;; Fancy eieio function that knows all about
				   ;; built in methods belonging to CLASS.
				   (eieio-all-generic-functions class)))))
	   )
      taglst)))

(provide 'semanticdb-el)

;;; semanticdb-el.el ends here
