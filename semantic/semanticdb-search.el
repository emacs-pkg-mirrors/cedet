;;; semanticdb-search.el --- Searching through semantic databases.

;;; Copyright (C) 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-search.el,v 1.3 2002/08/11 02:01:58 zappo Exp $

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
;; Databases of various forms call all be searched.  These routines
;; cover many common forms of searching.
;;

(require 'semanticdb)

;;; Code:
;;

;;; Utils
;;
;; Convenience routines for searches
(defun semanticdb-collect-find-results (databases function)
  "Collect results across DATABASES for FUNCTION.
If DATABASES is nil, search a range of associated databases calculated by
`semanticdb-current-database-list'.  DATABASES is a list of variable
`semanticdb-project-database' objects.
FUNCTION should accept one argument, the database being searched."
  (let* ((dbs (or databases
		  ;; Calculate what database to use.
		  ;; Something simple and dumb for now.
		  (or (semanticdb-current-database-list)
		      (list (semanticdb-current-database)))))
	 (case-fold-search semantic-case-fold)
	 (res (mapcar
	       (lambda (db)
		 (funcall function db))
	       dbs))
	 out)
    ;; Flatten the list.  The DB is unimportant at this stage.
    (setq res (apply 'append res))
    (setq out nil)
    ;; Move across results, and throw out empties.
    (while res
      (if (car res)
	  (setq out (cons res out)))
      (setq res (cdr res)))
    ;; Results
    out))

;;; Programatic interfaces
;;
;; These routines all perform different types of searches, and are
;; interfaces to the database methods used to also perform those searches.


;;;###autoload
(defun semanticdb-find-nonterminal-by-token
  (token &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-token-method
      db token search-parts search-includes diff-mode find-file-match))))

;;;###autoload
(defun semanticdb-find-nonterminal-by-name
  (name &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-name-method
      db name search-parts search-includes diff-mode find-file-match))))

;;;###autoload
(defun semanticdb-find-nonterminal-by-name-regexp
  (regex &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-name-regexp-method
      db regex search-parts search-includes diff-mode find-file-match))))


;;;###autoload
(defun semanticdb-find-nonterminal-by-type
  (type &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-type-method
      db type search-parts search-includes diff-mode find-file-match))))


;;;###autoload
(defun semanticdb-find-nonterminal-by-property
  (property value &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-property-method
      db property value search-parts search-includes diff-mode find-file-match))))


;;;###autoload
(defun semanticdb-find-nonterminal-by-extra-spec
  (spec &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-extra-spec-method
      db spec search-parts search-includes diff-mode find-file-match))))


;;;###autoload
(defun semanticdb-find-nonterminal-by-extra-spec-value
  (spec value &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-extra-spec-value-method
      db spec value search-parts search-includes diff-mode find-file-match))))


;;;###autoload
(defun semanticdb-find-nonterminal-by-function
  (function &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals which match FUNCTION.
Search in all DATABASES.  If DATABASES is nil, search a range of
associated databases calculated by `semanticdb-current-database-list'.
DATABASES is a list of variable `semanticdb-project-database' objects.
When SEARCH-PARTS is non-nil the search will include children of tokens.
When SEARCH-INCLUDES is non-nil, the search will include dependency files.
When DIFF-MODE is non-nil, search databases which are of a different mode.
A Mode is the `major-mode' that file was in when it was last parsed.
When FIND-FILE-MATCH is non-nil, the make sure any found token's file is
in an Emacs buffer.
Return a list ((DB-TABLE . TOKEN-OR-TOKEN-LIST) ...)."
  (semanticdb-collect-find-results
   databases
   (lambda (db)
     (semanticdb-find-nonterminal-by-function-method
      db function search-parts search-includes diff-mode find-file-match))))

;;; Search Methods
;;
;; These are the base routines for searching semantic databases.
;; Overload these with your subclasses to participate in the searching
;; mechanism.
(defmethod semanticdb-find-nonterminal-by-token-method
  ((database semanticdb-project-database) token search-parts search-includes diff-mode find-file-match)
  "In DB, find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function-method' for details on,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (let ((goofy-token-name token))
    (semanticdb-find-nonterminal-by-function-method
     database (lambda (stream sp si)
		(semantic-find-nonterminal-by-token goofy-token-name stream sp si))
     search-parts search-includes diff-mode find-file-match)))

(defmethod semanticdb-find-nonterminal-by-name-method
  ((database semanticdb-project-database) name search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-name name stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-name-regexp-method
  ((database semanticdb-project-database) regex search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-name-regexp regex stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-type-method
  ((database semanticdb-project-database) type search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-type type stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-property-method
  ((database semanticdb-project-database) property value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-property property value stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-extra-spec-method
  ((database semanticdb-project-database) spec search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-extra-spec spec stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-extra-spec-value-method
  ((database semanticdb-project-database) spec value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function-method
   database
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-extra-spec-value spec value stream sp si))
   search-parts search-includes diff-mode find-file-match))

(defmethod semanticdb-find-nonterminal-by-function-method
  ((database semanticdb-project-database)
   function &optional search-parts search-includes diff-mode find-file-match)
  "In DATABASE, find all occurances of nonterminals which match FUNCTION.
When SEARCH-PARTS is non-nil the search will include children of tokens.
When SEARCH-INCLUDES is non-nil, the search will include dependency files.
When DIFF-MODE is non-nil, search databases which are of a different mode.
A mode is the `major-mode' that file was in when it was last parsed.
When FIND-FILE-MATCH is non-nil, the make sure any found token's file is
in an Emacs buffer.
Return a list of matches."
  (let* ((ret nil)
	 (files (oref database tables))
	 (found nil)
	 (orig-buffer (current-buffer)))
    (while files
      (when (or diff-mode
		(semanticdb-equivalent-mode (car files) orig-buffer))
	;; This can cause unneeded refreshes while typing with
	;; senator-eldoc mode.
	;;(semanticdb-refresh-table (car files))
	(setq found (funcall function
			     (oref (car files) tokens)
			     search-parts
			     search-includes
			     )))
      (if found
	  (progn
	    ;; When something is found, make sure we read in that buffer if it
	    ;; had not already been loaded.
	    (if find-file-match
		(save-excursion (semanticdb-set-buffer (car files))))
	    ;; In theory, the database is up-to-date with what is in the file, and
	    ;; these tokens are ready to go.
	    ;; There is a bug lurking here I don't have time to fix.
	    (setq ret (cons (cons (car files) found) ret))
	    (setq found nil)))
      (setq files (cdr files)))
    (nreverse ret)))

(provide 'semanticdb-search)

;;; semanticdb-search.el ends here
