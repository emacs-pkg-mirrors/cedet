;;; semanticdb-java.el --- Semantic database extensions for Java

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-java.el,v 1.1 2003/03/06 18:38:35 zappo Exp $

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
;; A majority of Java's useful functionality is inside class files.
;; Often these class files do not have parsable source available.  In
;; order to get full advantage of speedbar extensions, access to
;; these class files is needed.
;;
;; The `semantic-project-database-java' class inherits from the
;; the database base class.  It uses the JDEE BeanShell installation to
;; query the class files, and create token compatible with Semantic.
;;

(require 'semanticdb-search)

;;; Code:

;;; Classes:
(defclass semanticdb-table-java (semanticdb-search-results-table)
  ((major-mode :initform java-mode)
   )
  "A table for returning search results from Emacs.")

(defclass semanticdb-project-database-java
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-java
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Java pre-compiled class libraries.")

;; Create the database, and add it to searchable databases for Emacs Lisp mode.
(defvar-mode-local java-mode semanticdb-project-system-databases
  (list 
   ;; Create a global instance for all Java source files.
   (semanticdb-project-database-java "Java"))
  "Search Java Class files for for symbols.")

;;; Search Overrides
;;
(defmethod semanticdb-find-nonterminal-by-token-method
  ((database semanticdb-project-database-java)
   token search-parts search-includes diff-mode find-file-match)
  "In DB, find all occurances of nonterminals with token TOKEN in DATABASE.
See `semanticdb-find-symbol-by-function-method' for details on,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)
  )

(defmethod semanticdb-find-nonterminal-by-name-method
  ((database semanticdb-project-database-java)
   name search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name NAME in DATABASE.
Uses `inter-soft' to match NAME to emacs symbols.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)
)

(defmethod semanticdb-find-nonterminal-by-name-regexp-method
  ((database semanticdb-project-database-java)
   regex search-parts search-includes diff-mode find-file-match)
  "Find all occurrences of nonterminals with name matching REGEX in DATABASE.
Uses `apropos-internal' to find matches.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes))

(defmethod semanticdb-find-nonterminal-by-type-method
  ((database semanticdb-project-database-java)
   type search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a type of TYPE in DATABASE.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)
  )

(defmethod semanticdb-find-nonterminal-by-property-method
  ((database semanticdb-project-database-java)
   property value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a PROPERTY equal to VALUE in DATABASE.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)
  )

(defmethod semanticdb-find-nonterminal-by-extra-spec-method
  ((database semanticdb-project-database-java)
   spec search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC in database.
See `semanticdb-find-nonterminal-by-function' for details on DATABASE,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)  )

(defmethod semanticdb-find-nonterminal-by-extra-spec-value-method
  ((database semanticdb-project-database-java)
   spec value search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC equal to VALUE in database.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-beanshell-method
   database
   (concat "semantic search by token " (symbol-name token))
   search-parts search-includes)
  )

(defmethod semanticdb-find-nonterminal-by-beanshell-method
  ((database semanticdb-project-database-java)
   predicate &optional search-parts search-includes diff-mode find-file-match)
  "In DATABASE, find all occurances of nonterminals which match PREDICATE.
PREDICATE accepts one Emacs Lisp symbol, and returns a semantic token.
The PREDICATE must all append found tokens to `semanticdb-elisp-mapatom-collector'
which gives each routine an opportunity to effect what kind of token
is created.
When SEARCH-PARTS is non-nil the search will include children of tokens.
SEARCH-INCLUDES is ignored.
When DIFF-MODE is non-nil, search databases which are in `java-mode'.
A mode is the `major-mode' that file was in when it was last parsed.
FIND-FILE-MATCH is is ignored.
Return a list of matches."
  (if (not (or diff-mode (eq major-mode 'java-mode)))
      nil
    (let ((newtable nil)
	  (answer (semanticdb-beanshell-search predicate))
	  )

      ;; Interpret collection, and add to the database table.
      (when answer
	(setq newtable (semanticdb-table-java "search-results"))
	(oset newtable parent-db database)
	(oset newtable tokens answer)
	;; We must return a list of all matching tables.
	;; That is why we have two lists here.
	(list (cons newtable semanticdb-elisp-mapatom-collector)))
      )))

(defmethod semanticdb-find-nonterminal-by-function-method
  ((database semanticdb-project-database-java)
   function &optional search-parts search-includes diff-mode find-file-match)
  "In DATABASE, find all occurances of nonterminals which match FUNCTION.
When SEARCH-PARTS is non-nil the search will include children of tokens.
SEARCH-INCLUDES is ignored.
When DIFF-MODE is non-nil, search databases which are in `java-mode'.
A mode is the `major-mode' that file was in when it was last parsed.
FIND-FILE-MATCH is is ignored.
Return a list of matches."
  (if nil
      ;; I kind of doubt that this can be done efficiently, so lets
      ;; skip it for new.
      nil
      ))

(provide 'semanticdb-el)

;;; Bean Shell Queries
;;
(defun semanticdb-beanshell-search (querytext)
  "Query the beanshell with QUERYTEXT.
Return a list of semantic compatible tokens."

  )

;;; semanticdb-el.el ends here
