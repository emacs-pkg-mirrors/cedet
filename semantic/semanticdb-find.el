;;; semanticdb-find.el --- Searching through semantic databases.

;;; Copyright (C) 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-find.el,v 1.8.2.1 2003/10/29 15:07:01 zappo Exp $

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
;; Databases of various forms can all be searched.
;; There are a few types of searches that can be done:
;;
;;   Basic Name Search:
;;    These searches scan a database table  collection for tags based
;;    on name.
;;
;;   Basic Brute Search:
;;    These searches allow searching on specific attributes of tags,
;;    such as name, type, or other attribute.
;;
;;   Advanced Search:
;;    These are searches that were needed to accomplish some
;;    specialized tasks as discovered in utilities.  Advanced searches
;;    include matching methods defined outside some parent class.
;;
;;    The reason for advanced searches are so that external
;;    repositories such as the Emacs obarray, or java .class files can
;;    quickly answer these needed questions without dumping the entire
;;    symbol list into Emacs for a regular semanticdb search.
;;
;;   Generic Brute Search:
;;    The generic search, `semanticdb-find-nonterminal-by-function'
;;
;; How databases are decided upon is another important aspect of a
;; database search.  When it comes to searching for a name, there are
;; these types of searches:
;;
;;   Basic Search:
;;    Basic search means that tags looking for a given name start
;;    with a specific search path.  Names are sought on that path
;;    until it is empty or items on the path can no longer be found.
;;
;;   Brute Search:
;;    Brute search means that all tables in all databases in a given
;;    project are searched.  Brute searches are the search style as
;;    written for semantic version 1.x.
;;
;; How does the search path work?
;;
;;  A basic search starts with three parameters:
;;
;;     (FINDME &optional PATH FIND-FILE-MATCH)
;;
;;  FINDME is key to be searched for dependent on the type of search.
;;  PATH is an indicator of which tables are to be searched.
;;  FIND-FILE-MATCH indicates that any time a match is found, the
;;  file associated with the tag should be read into a file.
;;
;;  The PATH argument is then the most interesting argument.  It can
;;  have these values:
;;
;;    nil - Take the current buffer, and use it's include list
;;    buffer - Use that buffer's include list.
;;    filename - Use that file's include list.  If the file is not
;;        in a buffer, see of there is a semanticdb table for it.  If
;;        not, read that file into a buffer.
;;    tag - Get that tag's buffer of file file.  See above.
;;    table - Search that table, and it's include list.
;;
;; Application:
;;
;; Here are applications where different searches are needed which
;; exist as of semantic 1.4.x
;;
;; eldoc - popup help
;;   => Requires basic search using default path.  (Header files ok)
;; tag jump - jump to a named tag
;;   => Requires a brute search useing whole project. (Source files only)
;; completion - Completing symbol names in a smart way
;;   => Basic search (headers ok)
;; type analysis - finding type definitions for variables & fcns
;;   => Basic search (headers ok)
;; Class browser - organize types into some structure
;;   => Brute search, or custom navigation.

;; TODO:
;;  During a search, load any unloaded DB files based on paths in the
;;  current project.

(require 'semanticdb)

;;; Code:

;;; Path Translations
;;
;;; OVERLOAD Functions
;;
;; These routines needed to be overloaded by specific language modes.
;; They are needed for translating an INCLUDE tag into a semanticdb
;; TABLE object.
(define-overload semanticdb-find-translate-path (path brutish)
  "Translate PATH into a list of semantic tables.
Path translation involves identifying the PATH input argument
in one of the following ways:
 nil - Take the current buffer, and use it's include list
 buffer - Use that buffer's include list.
 filename - Use that file's include list.  If the file is not
     in a buffer, see of there is a semanticdb table for it.  If
     not, read that file into a buffer.
 tag - Get that tag's buffer of file file.  See above.
 table - Search that table, and it's include list.
In addition, once the base path is found, there is the possibility of
each added table adding yet more tables to the path, so this routine
can return a lengthy list.
If argument BRUTISH is non-nil, then instead of using the include
list, use all tables found in the parent project of the table
identified by translating PATH.  Such searches use brute force to
scan every available table.
This routine uses `semanticdb-find-table-for-include' to translate
specific include tags into a semanticdb table."
  )

(defun semanticdb-find-translate-path-default (path brutish)
  "Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'."
  (if brutish
      (semanticdb-find-translate-path-brutish-default path)
    (semanticdb-find-translate-path-includes-default path)))

(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
	 (cond ((null path) semanticdb-current-database)
	       ((semanticdb-table-p path) (oref path parent-db))
	       (t (let ((tt (semantic-something-to-tag-table path)))
		    (save-excursion
		      (set-buffer (semantic-tag-buffer (car tt)))
		      semanticdb-current-database))))))
    (apply
     #'append
     (mapcar
      (lambda (db) (oref db tables))
      (semanticdb-current-database-list (oref basedb reference-directory)))))
  )

(defun semanticdb-find-translate-path-includes-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((includetags
	 (cond ((null path)
		(semantic-find-tags-included (current-buffer)))
	       ((semanticdb-table-p path)
		(semantic-find-tags-included (semanticdb-get-tags path)))
	       (t (semantic-find-tags-included path))))
	(matchedtables (list semanticdb-current-table))
	nexttable)
    ;; Loop over all include tags adding to matchedtables
    (while includetags
      (setq nexttable (semanticdb-find-table-for-include (car includetags)))
      ;; (message "Scanning %s" (semantic-tag-name (car includetags)))
      (when (and nexttable
		 (not (memq nexttable matchedtables))
		 (semanticdb-equivalent-mode nexttable (current-buffer))
		 )
	;; Add to list of tables
	(push nexttable matchedtables)
	;; Queue new includes to list
	(setq includetags (append includetags
				  (semantic-find-tags-included
				   (semanticdb-get-tags nexttable)))))
      (setq includetags (cdr includetags)))
    (nreverse matchedtables)))

;;;###autoload
(define-overload semanticdb-find-table-for-include (includetag &optional table)
  "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE as defined by `semantic-something-to-tag-table' to identify
where the tag came from.  TABLE is optional if INCLUDETAG has an
overlay of :filename attribute."
  )

(defun semanticdb-find-table-for-include-default (includetag &optional table)
  "Default implementation of `semanticdb-find-table-for-include'.
Uses `semanticdb-current-database-list' as the search path.
INCLUDETAG and TABLE are documented in `semanticdb-find-table-for-include'."
  (if (not (eq (semantic-tag-class includetag) 'include))
      (signal 'wrong-type-argument (list includetag 'include)))

  ;; Note, some languages (like Emacs or Java) use include tag names
  ;; that don't represent files!  We want to have file names.
  (let ((name (semantic-tag-include-filename includetag))
	(roots (semanticdb-current-database-list))
	(tmp nil)
	(ans nil))
    (cond
     ;; Relative path name
     ((file-exists-p (expand-file-name name))
      (setq ans (semanticdb-file-table-object name)))

     ;; On the path somewhere
;;;; THIS NEEDS WORK!
;     ((setq tmp (semantic-dependency-tag-file includetag))
;      (setq ans (semanticdb-file-table-object tmp))
;      ;; If we don't load this, then finding include tags within
;      ;; the table could fail!
;      (when ans (save-excursion (semanticdb-set-buffer ans))))
     (t
      ;; Somewhere in our project hierarchy
      ;; Remember: Roots includes system databases which can create
      ;; specialized tables we can search.
      (while (and (not ans) roots)
	(let* ((ref (if (slot-boundp (car roots) 'reference-directory)
			(oref (car roots) reference-directory)))
	       (fname (cond ((null ref) nil)
			    ((file-exists-p (concat ref name))
			     (concat ref name))
			    ((file-exists-p (concat ref (file-name-nondirectory name)))
			     (concat ref (file-name-nondirectory name))))))
	  (if ref
	      ;; There is an actual file.  Grab it.
	      (setq ans (semanticdb-file-table-object name))
	    ;; No reference directory  Probably a system database
	    ;; NOTE: Systemdb will need to override `semanticdb-file-table'.
	    (setq ans (semanticdb-file-table
		       (car roots)
		       ;; Use name direct from tag.  System DB will expect it
		       ;; in the original form.
		       (semantic-tag-name includetag)))))

	(setq roots (cdr roots))))
     )
    ans))

;;; API Functions
;;
;; These routines are apropriate for applications to use.

;;; Top level Searches
;;
(defun semanticdb-find-tags-collector (function &optional path find-file-match
						brutish)
  "Search for all tags returned by FUNCTION over PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.
If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree."
  (let (found match)
    (dolist (table (semanticdb-find-translate-path path brutish))
      ;; If FIND-FILE-MATCH is non-nil, skip tables of class
      ;; `semanticdb-search-results-table', since those are system
      ;; databases and not associated with a file.
      (unless (and find-file-match
		   (obj-of-class-p table semanticdb-search-results-table))
	(when (setq match (funcall function table))
	  (when find-file-match
	    (save-excursion (semanticdb-set-buffer table)))
	  (push (cons table match) found))))
    found))

;;;###autoload
(defun semanticdb-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-by-name-method table name))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-by-name-regexp-method table regexp))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-for-completion-method table prefix))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-by-class-method table class))
   path find-file-match))

;;; Deep Searches
;;
;;;###autoload
(defun semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-deep-find-tags-by-name-method table name))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-deep-find-tags-by-name-regexp-method table regexp))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-deep-find-tags-for-completion-method table prefix))
   path find-file-match))

;;; Brutish Search Routines
;;
;;;###autoload
(defun semanticdb-brute-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-deep-find-tags-by-name-method table name))
   path find-file-match t))

;;; Specialty Search Routines
;;
;;;###autoload
(defun semanticdb-find-tags-external-children-of-type
  (type &optional path find-file-match)
  "Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-external-children-of-type-method table type))
   path find-file-match))

;;; METHODS
;;
;; Default methods for semanticdb database and table objects.
;; Override these with system databases to as new types of back ends.

;;; Top level Searches
(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table) name)
  "In TABLE, find all occurances of tags with NAME.
Returns a table of all matching tags."
  (semantic-find-tags-by-name name (semanticdb-get-tags table)))

(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table) regexp)
  "In TABLE, find all occurances of tags matching REGEXP.
Returns a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (semanticdb-get-tags table)))

(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table) prefix)
  "In TABLE, find all occurances of tags matching PREFIX.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix (semanticdb-get-tags table)))

(defmethod semanticdb-find-tags-by-class-method ((table semanticdb-table) class)
  "In TABLE, find all occurances of tags of CLASS.
Returns a table of all matching tags."
  (semantic-find-tags-by-class class (semanticdb-get-tags table)))

(defmethod semanticdb-find-tags-external-children-of-type-method
   ((table semanticdb-table) parent)
   "In TABLE, find all occurances of tags whose TYPE is PARENT.
Returns a table of all matching tags."
   (semantic-find-tags-external-children-of-type
    parent (semanticdb-get-tags table)))

;;; Deep Searches
(defmethod semanticdb-deep-find-tags-by-name-method ((table semanticdb-table) name)
  "In TABLE, find all occurances of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Return a table of all matching tags."
  (semantic-find-tags-by-name
   name
   (semantic-flatten-tags-table
    (semanticdb-get-tags table))))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method ((table semanticdb-table) regexp)
  "In TABLE, find all occurances of tags matching REGEXP.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Return a table of all matching tags."
  (semantic-find-tags-by-name-regexp
   regexp
   (semantic-flatten-tags-table
    (semanticdb-get-tags table))))

(defmethod semanticdb-deep-find-tags-for-completion-method ((table semanticdb-table) prefix)
  "In TABLE, find all occurances of tags matching PREFIX.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Return a table of all matching tags."
  (semantic-find-tags-for-completion
   prefix
   (semantic-flatten-tags-table
    (semanticdb-get-tags table))))

(provide 'semanticdb-find)

;;; semanticdb-find.el ends here
