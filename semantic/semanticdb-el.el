;;; semanticdb-el.el --- Semantic database extensions for Emacs Lisp

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-el.el,v 1.1 2002/08/20 16:55:19 zappo Exp $

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

(defun semanticdb-elisp-sym->nonterm (sym &optional toktype)
  "Convert SYM into a semantic token.
TOKTYPE is a hint to the type of token desired."
  (cond ((and (eq toktype 'function) (fboundp sym))
	 (list (symbol-name sym) 'function
	       nil ;; return type
	       (semantic-elisp-desymbolify
		(semanticdb-elisp-sym-function-arglist sym)) ;; arg-list
	       (semantic-bovinate-make-assoc-list
		'user-visible (interactive-form sym)
		) ;; assoc-list
	       nil ;; doc
	       ))
	((and (eq toktype 'variable) (boundp sym))
	 (list (symbol-name sym) 'variable
	       nil ;; type
	       nil ;; value - ignore for now
	       nil ;; assoc-list
	       nil ;; doc
	       ))
	((and (eq toktype 'type) (class-p sym))
	 (list (symbol-name sym) 'type "class"
	       (semantic-elisp-desymbolify
		(aref (class-v semanticdb-project-database)
		      class-public-a)) ;; slots
	       (semantic-elisp-desymbolify (class-parents sym)) ;; parents
	       nil ;; assoc
	       nil ;; doc
	       ))
	((not toktype)
	 ;; Figure it out on our own.
	 (cond ((class-p sym)
		(semanticdb-elisp-sym->nonterm sym 'type))
	       ((fboundp sym)
		(semanticdb-elisp-sym->nonterm sym 'function))
	       ((boundp sym)
		(semanticdb-elisp-sym->nonterm sym 'variable))
	       (t nil))
	 )
	(t nil)))

;;; Search Overrides
;;
(defvar semanticdb-elisp-mapatom-collector nil
  "Variable used to collect mapatoms output.")

(defmethod semanticdb-find-nonterminal-by-token-method
  ((database semanticdb-project-database-emacs-lisp)
   token search-parts search-includes diff-mode find-file-match)
  "In DB, find all occurances of nonterminals with token TOKEN in DATABASE.
See `semanticdb-find-symbol-by-function-method' for details on,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-symbol-by-function-method
   database
   ;; Match each atom against the feature we want.
   (cond ((eq token 'function)
	  (lambda (atom)
	    (when (fboundp atom)
	      (setq semanticdb-elisp-mapatom-collector
		    (cons (semanticdb-elisp-sym->nonterm atom 'function)
			  semanticdb-elisp-mapatom-collector)))))
	 ((eq token 'variable)
	  (lambda (atom)
	    (when (boundp atom)
	      (setq semanticdb-elisp-mapatom-collector
		    (cons (semanticdb-elisp-sym->nonterm atom 'function)
			  semanticdb-elisp-mapatom-collector)))))
	 ((eq token 'type)
	  (lambda (atom)
	    (when (or (class-p atom)
		      )
	      (setq semanticdb-elisp-mapatom-collector
		    (cons (semanticdb-elisp-sym->nonterm atom 'function)
			  semanticdb-elisp-mapatom-collector)))))
	 (t nil))
   search-parts search-includes diff-mode find-file-match)
  )

(defmethod semanticdb-find-nonterminal-by-name-method
  ((database semanticdb-project-database-emacs-lisp)
   name search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name NAME in DATABASE.
Uses `inter-soft' to match NAME to emacs symbols.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN) ...)."
  ;; No need to search.  Use `intern-soft' which does the same thing for us.
  (let* ((sym (intern-soft name))
	 (fun (semanticdb-elisp-sym->nonterm sym 'function))
	 (var (semanticdb-elisp-sym->nonterm sym 'variable))
	 (typ (semanticdb-elisp-sym->nonterm sym 'type))
	 (toklst nil)
	 (newtable nil))
    (when (or fun var typ)
      ;; If the symbol is any of these things, build the search table.
      (setq newtable (semanticdb-table-emacs-lisp "name search"))
      (oset newtable parent-db database)
      (when var	(setq toklst (cons var toklst)))
      (when typ	(setq toklst (cons typ toklst)))
      (when fun	(setq toklst (cons fun toklst)))
      (oset newtable tokens toklst)
      (list (cons newtable toklst))
      )))

(defmethod semanticdb-find-nonterminal-by-name-regexp-method
  ((database semanticdb-project-database-emacs-lisp)
   regex search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name matching REGEX in DATABASE.
Uses `apropos-internal' to find matches.a
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (let ((syms (apropos-internal regex))
	(toklst nil)
	(newtable nil)
	)
    (when syms
      (while syms
	(setq toklst (cons (semanticdb-elisp-sym->nonterm (car syms))
			   toklst)
	      syms (cdr syms)))
      ;; Set up the table.
      (setq newtable (semanticdb-table-emacs-lisp "name regexp search"))
      (oset newtable parent-db database)
      (oset newtable tokens toklst)
      ;; Return the new table.
      (list (cons newtable toklst))
      )
    ))

(defmethod semanticdb-find-nonterminal-by-type-method
  ((database semanticdb-project-database-emacs-lisp)
   type search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a type of TYPE in DATABASE.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  ;; We never have any types in Emacs Lisp.
  ;; While we could derive this information, it is not really
  ;; worth it.
  nil
  )

(defmethod semanticdb-find-nonterminal-by-property-method
  ((database semanticdb-project-database-emacs-lisp)
   property value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a PROPERTY equal to VALUE in DATABASE.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  ;; Properties are added after a symbol is parsed, by a user, for example.
  ;; As such, these properties cannot be kept, so we won't allow
  ;; searching either.
  nil
  )

(defmethod semanticdb-find-nonterminal-by-extra-spec-method
  ((database semanticdb-project-database-emacs-lisp)
   spec search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC in database.
See `semanticdb-find-nonterminal-by-function' for details on DATABASE,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  ;; There is currently but one supported extra spec.
  ;; Once we have more supported, it might be worth enhancing
  ;; this function.
  nil
;;  (semanticdb-find-symbol-by-function-method
;;   database
;;   (lambda (atom)
;;     
;;     )
;;   search-parts search-includes diff-mode find-file-match)
  )

(defmethod semanticdb-find-nonterminal-by-extra-spec-value-method
  ((database semanticdb-project-database-emacs-lisp)
   spec value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC equal to VALUE in database.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  ;; See above.
  nil
  )

(defmethod semanticdb-find-symbol-by-function-method
  ((database semanticdb-project-database-emacs-lisp)
   predicate &optional search-parts search-includes diff-mode find-file-match)
  "In DATABASE, find all occurances of nonterminals which match PREDICATE.
PREDICATE accepts one Emacs Lisp symbol, and returns a semantic token.
The PREDICATE must all append found tokens to `semanticdb-elisp-mapatom-collector'
which gives each routine an opportunity to effect what kind of token
is created.
When SEARCH-PARTS is non-nil the search will include children of tokens.
SEARCH-INCLUDES is ignored.
When DIFF-MODE is non-nil, search databases which are in `emacs-lisp-mode'.
A mode is the `major-mode' that file was in when it was last parsed.
FIND-FILE-MATCH is is ignored.
Return a list of matches."
  (if (not (or diff-mode (eq major-mode 'emacs-lisp-mode)))
      nil
    (let ((newtable nil)
	  (semanticdb-elisp-mapatom-collector nil)
	  )
      ;; Map across all atoms in `obarray', the master obarray for all emacs
      ;; while running FUNCTION.
      (mapatoms (lambda (sym)
		  (funcall predicate sym)
		  ))
      ;; Interpret collection, and add to the database table.
      (when semanticdb-elisp-mapatom-collector
	(setq newtable (semanticdb-table-emacs-lisp "search-results"))
	(oset newtable parent-db database)
	(oset newtable tokens semanticdb-elisp-mapatom-collector)
	;; We must return a list of all matching tables.
	;; That is why we have two lists here.
	(list (cons newtable semanticdb-elisp-mapatom-collector)))
      )))

(defmethod semanticdb-find-nonterminal-by-function-method
  ((database semanticdb-project-database-emacs-lisp)
   function &optional search-parts search-includes diff-mode find-file-match)
  "In DATABASE, find all occurances of nonterminals which match FUNCTION.
When SEARCH-PARTS is non-nil the search will include children of tokens.
SEARCH-INCLUDES is ignored.
When DIFF-MODE is non-nil, search databases which are in `emacs-lisp-mode'.
A mode is the `major-mode' that file was in when it was last parsed.
FIND-FILE-MATCH is is ignored.
Return a list of matches."
  (semanticdb-find-symbol-by-function-method
   database
   (lambda (atom)
     (let ((token (semanticdb-elisp-sym->nonterm atom)))
       (when token
	 (if (funcall function
		      ;; This TOTALLY sucks as a mechanism for
		      ;; searching Emacs Lisp.  Make sure if such
		      ;; is needed by users, that we make a new
		      ;; search above which will work better.
		      ;; Otherwise, all such searches will be very
		      ;; slow. :(
		      token)
	     (setq semanticdb-elisp-mapatom-collector
		   (cons token semanticdb-elisp-mapatom-collector))
	     )
	 ))
     )
   search-parts search-includes diff-mode find-file-match))

(provide 'semanticdb-el)

;;; semanticdb-el.el ends here
