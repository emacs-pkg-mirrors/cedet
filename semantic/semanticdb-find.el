;;; semanticdb-find.el --- Searching through semantic databases.

;;; Copyright (C) 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-find.el,v 1.1 2003/04/04 02:47:32 zappo Exp $

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

(require 'semanticdb)

;;; Code:

;;; API Functions
;;
;; These routines are apropriate for applications to use.
(defun semanticdb-find-tags-collector (function &optional path find-file-match)
  "Search for all tags returned by FUNCTION over PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (let ((tables (semanticdb-find-translate-path path))
	(found nil))
    (while tables
      (let ((match (funcall function (car tables))))
	(when match
	  (when find-file-match
	    (save-excursion (semanticdb-set-buffer (car tables))))
	  (setq found (cons (cons (car tables) match) found))
	  ))
      (setq tables (cdr tables)))
    found))

(defun semanticdb-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-by-name-method table name))
   path find-file-match))

(defun semanticdb-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-by-name-regexp-method table regexp))
   path find-file-match))

(defun semanticdb-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table)
     (semanticdb-find-tags-for-completion-method table prefix))
   path find-file-match))

;;; OVERLOAD Functions
;;
;; These routines needed to be overloaded by specific language modes.
;; They are needed for translating an INCLUDE tag into a semanticdb
;; TABLE object.
(define-overload semanticdb-find-translate-path (path)
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
This routine uses `semanticdb-find-table-for-include' to translate
specific include tags into a semanticdb table."
  )

(defun semanticdb-find-translate-path-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((includetags
	 (cond ((null path)
		(semantic-find-tags-included (current-buffer)))
	       ((semanticdb-table-p path)
		(semantic-find-tags-included (semanticdb-get-tags path)))
	       (t (semantic-find-tags-included path))))
	(matchedtables (list semanticdb-current-table))
	)
    ;; Loop over all include tags adding to matchedtables
    (while includetags
      (let* ((nexttable (semanticdb-find-table-for-include (car includetags)))
	     (moreincludes nil))
	;; (message "Scanning %s" (semantic-tag-name (car includetags)))
	(when (and nexttable (not (memq nexttable matchedtables)))
	  (setq moreincludes (semantic-find-tags-included (semanticdb-get-tags nexttable)))
	  ;; Add to list of tables
	  (add-to-list 'matchedtables nexttable t)
	  ;; Add new includes to list
	  (setq includetags (append includetags moreincludes))
	  ))
      (setq includetags (cdr includetags))
      )
    matchedtables))

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
	(ans nil))
    ;; Relative path name
    (if (file-exists-p (expand-file-name name))
	(setq ans (semanticdb-file-table-object name))
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
    ans))

;;; METHODS
;;
;; Default methods for semanticdb database and table objects.
;; Override these with system databases to as new types of back ends.

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

(provide 'semanticdb-find)

;;; semanticdb-find.el ends here
