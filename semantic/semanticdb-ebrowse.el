;;; semanticdb-ebrowse.el --- Semanticdb backend using ebrowse.

;;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-ebrowse.el,v 1.1 2005/05/01 02:25:40 zappo Exp $

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
;; Be able to read in custom-created ebrowse BROWSE files into a
;; semanticdb back end.
;;
;; Add these databases to the 'system' search.
;; Possibly use ebrowse for local parsing too.
;;
;; When real details are needed out of the tag system from ebrowse,
;; we will need to delve into the originating source and parse those
;; files the usual way.
;;


;;; DEV NOTES:
;;
;; These are notes to myself to keep track of where I am.
;;
;; Done:
;;
;; A DB is created with ebrowse data stored in it.
;; 'Tables' are stripped out of the ebrowse struct.
;;
;; Next Steps:
;;
;; Learn how to search through an ebrowse 'tree' structure.
;; Learn how to use the ebrowse hash table and obarray.
;;
;; Other:
;;
;; This file was derived from the semanticdb backend skeleton.
;; Many aspects of the skeleton still need to be stripped out
;; or implemented.
;;
;; Add no autoload cookies until this implementation works.

(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )
(require 'ebrowse)
;;; Code:

;;; Classes:
(defclass semanticdb-table-ebrowse (semanticdb-table)
  ((major-mode :initform c++-mode)
   (ebrowse-tree :initform nil
		   :initarg :ebrowse-tree
		   )
   )
  "A table for returning search results from ebrowse.")

(defclass semanticdb-project-database-ebrowse 
  (semanticdb-project-database)
  ((tracking-symbol :initform semanticdb-ebrowse-database-list)
   (new-table-class :initform semanticdb-table-ebrowse
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (ebrowse-struct :initform nil
		   :initarg :ebrowse-struct
		   )
   )
  "Database representing ebrowse.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
;;(defvar-mode-local c++-mode semanticdb-find-default-throttle
;;  '(project system omniscience)
;;  "Search project files, then search this omniscience database.
;;It is not necessary to to system or recursive searching because of
;;the omniscience database.")

;;; EBROWSE code
;;
;; These routines deal with part of the ebrowse interface.
(defun semanticdb-ebrowse-get-ebrowse-structure (dir)
  "Return the ebrowse structure for directory DIR."
  (let* ((semanticdb-default-save-directory
	  semanticdb-default-system-save-directory)
	 (B (semanticdb-file-name-directory
	     'semanticdb-project-database-file
	     (concat dir "BROWSE")))
	 (buf (get-buffer-create "*semanticdb ebrowse*")))
    (when (file-exists-p B)
      (set-buffer buf)
      (erase-buffer)
      (insert-file-contents B)
      (prog1
	  (list B (ebrowse-read))
	(erase-buffer)))))

;;; Methods for creating a database or tables
;;

(defvar semanticdb-ebrowse-database-list nil
  "List of ebrowse databases.
Debugging purposes.")


(defmethod semanticdb-create-database :STATIC ((dbe semanticdb-project-database-ebrowse)
					       directory)
  "Create a new semantic database for DIRECTORY based on ebrowse.
If there is no database for DIRECTORY available, then
{not implemented yet} create one.  Return nil if that is not possible."
  (let* ((ebrowse-data (semanticdb-ebrowse-get-ebrowse-structure directory))
	 (dat (car (cdr ebrowse-data)))
	 (ebd (car dat))
	 (db nil)
	 )
    (setq db (make-instance
	      dbe
	      directory
	      :tables (semanticdb-ebrowse-strip-tables dbe dat)
	      :ebrowse-struct ebd
	      ))
    (oset db reference-directory directory)
    ))

(defmethod semanticdb-ebrowse-strip-tables :STATIC ((dbe semanticdb-project-database-ebrowse)
						    data)
  "For the ebrowse database DBE, strip all tables from DATA."
  (let ((table-list nil)
	(T (car (cdr data))))
    (while T
      
      (let* ((tree (car T))
	     (class (ebrowse-ts-class tree))
	     (newtable (make-instance
			(oref-default dbe new-table-class)
			:file (ebrowse-cs-source-file class)
			:ebrowse-tree tree
			)))
	(setq table-list (cons newtable table-list))
	)
      (setq T (cdr T)))
    (nreverse table-list)))

;;; Filename based methods
;;
(defmethod semanticdb-file-table ((obj semanticdb-project-database-ebrowse) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See not for `semanticdb-get-database-tables'.
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-ebrowse ))
  "Return the list of tags belonging to TABLE."
  ;; NOTE: Omniscient databases probably don't want to keep large tabes
  ;;       lolly-gagging about.  Keep internal Emacs tables empty and
  ;;       refer to alternate databases when you need something.
  nil)

;;(defmethod semanticdb-equivalent-mode ((table semanticdb-table-ebrowse) &optional buffer)
;;  "Return non-nil if TABLE's mode is equivalent to BUFFER.
;;Equivalent modes are specified by by `semantic-equivalent-major-modes'
;;local variable."
;;  (save-excursion
;;    (set-buffer buffer)
;;    (eq (or mode-local-active-mode major-mode) 'ebrowse-mode)))

;;; Usage
;;
;; Unlike other tables, an omniscent database does not need to
;; be associated with a path.  Use this routine to always add ourselves
;; to a search list.
(define-mode-local-override semanticdb-find-translate-path ebrowse-mode
  (path brutish)
  "Return a list of semanticdb tables asociated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
	  ;; so that our ELisp database only shows up once when
	  ;; we append it in this iteration.
	  (let ((semanticdb-search-system-databases nil)
		)
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
	default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-ebrowse) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-ebrowse) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE    
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-ebrowse) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-ebrowse) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; Note: This search method could be considered optional in an
    ;;       omniscient database.  It may be unwise to return all tags
    ;;       that exist for a language that are a variable or function.
    ;;
    ;; If it is optional, you can just delete this method.
    nil))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above. 
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-ebrowse) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for ebrowse."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-ebrowse) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for ebrowse."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-ebrowse) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for ebrowse."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-ebrowse) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; OPTIONAL: This could be considered an optional function.  It is
    ;;       used for `semantic-adopt-external-members' and may not
    ;;       be possible to do in your language.
    ;;
    ;; If it is optional, you can just delete this method.
    ))

(provide 'semanticdb-el)

;;; semanticdb-el.el ends here
