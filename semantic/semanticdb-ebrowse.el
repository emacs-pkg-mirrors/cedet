;;; semanticdb-ebrowse.el --- Semanticdb backend using ebrowse.

;;; Copyright (C) 2005, 2006, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>, Joakim Verona
;; Keywords: tags
;; X-RCS: $Id: semanticdb-ebrowse.el,v 1.4 2007/01/12 03:32:14 zappo Exp $

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This program was started by Eric Ludlam, and Joakim Verona finished
;; the implementation by adding searches and fixing bugs.
;;
;; Read in custom-created ebrowse BROWSE files into a semanticdb back
;; end.
;;
;; Add these databases to the 'system' search.
;; Possibly use ebrowse for local parsing too.
;;
;; When real details are needed out of the tag system from ebrowse,
;; we will need to delve into the originating source and parse those
;; files the usual way.
;;
;; COMMANDS:
;; `semanticdb-create-ebrowse-database' - Call EBROWSE to create a
;;       system database for some directory.  In general, use this for
;;       system libraries, such as /usr/include, or the like.
;;
;; `semanticdb-load-ebrowse-caches' - Load all the EBROWSE caches from
;;       your semanticdb system database directory.  Once they are
;;       loaded, they become searchable as omnipotent databases for
;;       all C++ files.
;;

(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )
(require 'ebrowse)
;;; Code:
(defvar semanticdb-ebrowse-default-file-name "BROWSE"
  "The EBROWSE file name used for system caches.")

;;;###autoload
(defun semanticdb-create-ebrowse-database (dir)
  "Create an EBROSE database for directory DIR.
The database file is stored in ~/.semanticdb, or whichever directory
is specified by `semanticdb-default-system-save-directory'."
  (interactive "DDirectory: ")
  (let* ((savein (semanticdb-ebrowse-file-for-directory dir))
	 (filebuff (get-buffer-create "*SEMANTICDB EBROWSE TMP*"))
	 (files (directory-files dir))
	 (mma auto-mode-alist)
	 (regexp nil)
	 )
    ;; Calculate the regular expression for matching files
    (while mma
      (when (or (eq (cdr (car mma)) 'c++-mode)
		(eq (cdr (car mma)) 'c-mode))
	;; (setq regexps (cons (car (car mma)) regexps))
	(if (stringp regexp)
	    (setq regexp (concat regexp "\\|"))
	  (setq regexp ""))
	(setq regexp (concat regexp "\\(" (car (car mma)) "\\)"))
	)
      (setq mma (cdr mma)))
    ;; some C++ headers have no extension.  How annoying.
    ;; This regexp assumes
    (setq regexp (concat regexp "\\|\\(\\(^\\|/\\)\\w+$\\)"))
    ;; Create the input to the ebrowse command
    (save-excursion
      (set-buffer filebuff)
      (setq default-directory dir)
      (mapcar (lambda (f)
		(when (string-match regexp f)
		  (insert f)
		  (insert " ")))
	      files)
      ;; Cleanup the ebrowse output buffer.
      (save-excursion
	(set-buffer (get-buffer-create "*EBROWSE OUTPUT*"))
	(erase-buffer))
      ;; Call the EBROWSE command.
      (message "Creating ebrowse file: %s ..." savein)
      (call-process-region (point-min) (point-max)
			   "ebrowse" nil "*EBROWSE OUTPUT*" nil
			   (concat "--output-file=" savein)
			   "--very-verbose")
      )
    ;; Create a short LOADER program for loading in this database.
    (let ((lf (find-file-noselect (concat savein "-load.el"))))
      (save-excursion
	(set-buffer lf)
	(erase-buffer)
	(insert "(semanticdb-create-database "
		"semanticdb-project-database-ebrowse \""
		dir
		"\")\n")
	(save-buffer))
      (message "Creating ebrowse file: %s ... done" savein)
      ;; Reload that database
      (load lf nil t)
      )))

;;;###autoload
(defun semanticdb-load-ebrowse-caches ()
  "Load all semanticdb controlled EBROWSE caches."
  (interactive)
  (let ((f (directory-files semanticdb-default-system-save-directory
			    t (concat semanticdb-ebrowse-default-file-name "-load.el$") t)))
    (while f
	;;(semanticdb-load-database (car f)))
      (load (car f) nil t)
      ;; NOTE FOR THE FUTURE: Verify the system was not expanded for
      ;; each.  This may be slow.
      (setq f (cdr f)))
    ))

;;; SEMANTIC Database related Code
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
  ((new-table-class :initform semanticdb-table-ebrowse
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (system-include-p :initform nil
		     :initarg :system-include
		     :documentation
		     "Flag indicating this database represents a system include directory.")
   (ebrowse-struct :initform nil
		   :initarg :ebrowse-struct
		   )

   (mytables :initform nil
	     :initarg :mytables
	     )


   )
  "Database representing ebrowse.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
(defvar-mode-local c++-mode semanticdb-find-default-throttle
  '(project system recursive)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")


;JAVE this just instantiates a default empty ebrowse struct? 
; how would new instances wind up here?
; the ebrowse class isnt singleton, unlike the emacs lisp one
(defvar-mode-local c++-mode semanticdb-project-system-databases
  ()
  "Search Ebrowse for symbols.")

(defmethod semanticdb-get-database-tables ((dbe semanticdb-project-database-ebrowse))
  "Get the list of tables for an EBROWSE database.

JAVE: my own implementation because ive broken the original implementation"
  (mapcar #'cdr (oref dbe mytables)) ; un-assoc the mytables list and return it
)  

(defmethod semanticdb-needs-refresh-p ((table semanticdb-table-ebrowse))
  "EBROWSE database do not need to be refreshed.

JAVE: stub for needs-refresh, because, how do we know if BROWSE files
      are out of date?

EML: Our database should probably remember the timestamp/checksum of
     the most recently read EBROWSE file, and use that."
  nil
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; EBROWSE code
;;
;; These routines deal with part of the ebrowse interface.
(defun semanticdb-ebrowse-file-for-directory (dir)
  "Return the file name for DIR where the ebrowse BROWSE file is.
This file should reside in `semanticdb-default-system-save-directory'."
  (let* ((semanticdb-default-save-directory
	  semanticdb-default-system-save-directory)
	 (B (semanticdb-file-name-directory
	     'semanticdb-project-database-file
	     (concat dir semanticdb-ebrowse-default-file-name)))
	 )
    B))

(defun semanticdb-ebrowse-get-ebrowse-structure (dir)
  "Return the ebrowse structure for directory DIR.
This assumes semantic manages the BROWSE files, so they are assumed to live
where semantic cache files live, depending on your settings.

for instance: /home/<username>/.semanticdb/!usr!include!BROWSE"
  (let* ((B (semanticdb-ebrowse-file-for-directory dir))
	 (buf (get-buffer-create "*semanticdb ebrowse*")))
    (message "semanticdb-ebrowse %s" B)
    (when (file-exists-p B)
      (set-buffer buf)
      (erase-buffer)
      (insert-file-contents B)
      (prog1
	  (list B (ebrowse-read))
	(erase-buffer)))))

;;; Methods for creating a database or tables
;;
(defmethod semanticdb-create-database :STATIC ((dbeC semanticdb-project-database-ebrowse)
					       directory)
  "Create a new semantic database for DIRECTORY based on ebrowse.
If there is no database for DIRECTORY available, then
{not implemented yet} create one.  Return nil if that is not possible."
  ;; MAKE SURE THAT THE FILE LOADED DOESN'T ALREADY EXIST.
  (let ((dbs semanticdb-database-list)
	(found nil))
    (while (and (not found) dbs)
      (when (semanticdb-project-database-ebrowse-p (car dbs))
	(when (string= (oref (car dbs) reference-directory) directory)
	  (setq found (car dbs))))
      (setq dbs (cdr dbs)))
    ;;STATIC means DBE cant be used as object, only as a class
    (let* ((ebrowse-data (semanticdb-ebrowse-get-ebrowse-structure directory))
	   (dat (car (cdr ebrowse-data)))
	   (ebd (car dat))
	   (db nil)
	   )
      (if found
	  (setq db found)
	(setq db (make-instance
		  dbeC
		  directory
		  :ebrowse-struct ebd
		  ))
	(oset db reference-directory directory))
      ;;(oset db tables nil)
      (oset db tables (semanticdb-ebrowse-strip-tables db dat)) ;only possible after object creation, tables inited to nil.

      ;; Once our database is loaded, if we are a system DB, we
      ;; add ourselves to the include list for C++.
      (semantic-add-system-include directory 'c++-mode)

      db)))


(defmethod semanticdb-ebrowse-strip-tables  ((dbe semanticdb-project-database-ebrowse)
						    data)
  "For the ebrowse database DBE, strip all tables from DATA."
;JAVE what it actually seems to do is split the original tree in "tables" associated with files
; im not sure it actually works:
;   the filename slot sometimes gets to be nil,
;      apparently for classes which definition cant be found, yet needs to be included in the tree
;      like library baseclasses
;   a file can define several classes
  (let ((T (car (cdr data))));1st comes a header, then the tree
    (while T

      (let* ((tree (car T))
	     (class (ebrowse-ts-class tree)); root class of tree
             (filename (ebrowse-cs-source-file class))) ;the file that defines the root class
        (if filename ;filename gets to be nil sometimes. hmm. just ignore this case for now(happens if ebrowse doesnt now were definition is)
            (semanticdb-ebrowse-add-etree-to-table dbe tree filename)
          )
	)
      (setq T (cdr T)))
))

;;; Filename based methods
;;

(defmethod semanticdb-ebrowse-add-etree-to-table  ((dbe semanticdb-project-database-ebrowse) tree filename)
  "Add the partial ebrowse TREE to the table pertaining to FILENAME in database DBE.
If there is no table object for FILENAME, create it. Otherwise append it."
  (let*
      ((existingtable (semanticdb-file-table dbe filename)))
    (if existingtable
        (semanticdb-file-table-add existingtable tree) ;we have a table already so add this etree to it
      (let*
          ((newtable (make-instance
                      (oref-default dbe new-table-class)
                      :file filename
                      )))
        (semanticdb-file-table-add newtable tree) ;we didnt have a table so we made one and added this etree to it
        (oset newtable parent-db dbe)
        (oset dbe mytables (cons (cons filename  newtable) (oref dbe mytables)))) ;put newtable on the assoclist
      )))


(defmethod semanticdb-file-table-add ((table semanticdb-table-ebrowse) tree)
  "Add TREE to TABLE.
a table belongs to a file, and can have many trees"
  (oset table ebrowse-tree  (cons tree (oref table ebrowse-tree))
   ))

(defmethod semanticdb-file-table ((dbe semanticdb-project-database-ebrowse) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See note for `semanticdb-get-database-tables'.
  (cdr (assoc (file-name-nondirectory filename) (oref dbe mytables)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (message "semanticdb-find-tags-by-name-method name -- %s" name)
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    " YOUR IMPLEMENTATION HERE"
    ;;JAVE currently a "table" is a list of ebrowse trees, slot ebrowse-tree, associated with a file
    ;TODO here:
    ; - search until name is found(no apropriate ebrowse utils where found)
    ;   ebrowse-view/find-file-and-search-pattern (only works on buffers)?
    ;   ebrowse-for-all-trees ? ebrowse-position ?

    ; - convert all found structures to semantic format and return them
    (jvebrowse-find table name nil)

    )
  )

(defun jvebrowse-find (table name doregex)
  (append (jvebrowse-find2 table name doregex   #'ebrowse-ts-member-functions )
          (jvebrowse-find2 table name doregex   #'ebrowse-ts-member-variables ))
  )

(defun jvebrowse-find2 (table name doregex memberfunction)

  (let*
        ( (etrees (oref table ebrowse-tree))
          (result nil)
          (filename (oref table file))
          )
      (while etrees;this style of imperative iteration is imho lame and i should find a better functional counterpart JAVE
        (setq result (append
                      (jvebrowse-find-name name (funcall memberfunction  (car etrees)) doregex);only search member functions, for now...
                      result))
        (setq etrees (cdr etrees))
        )
      ;result is a list of ebrowse-ms structures, and should now be converted to semantic tags:
      ;for each result:
      ; (semantic-tag-new-function name return_type :filename where_tag_is_defined )
      ; the definition-point need probably be propagated through the tag-overlay part
      (mapcar (lambda (x)
                (let*
                    ((tag (semantic-tag-new-function
                           (ebrowse-ms-name x) nil nil :filename filename  ))
                     (defpoint (ebrowse-ms-definition-point x))
                     )
                  (semantic--tag-set-overlay ; this is an internal function...
                   tag
                   (vector defpoint defpoint)) ;... and im unsure of this
                tag)
                )
              result)
    )
  )

;;; easy to understand recursive version
;; crashes
;; (defun jvebrowse-find-name (name  members doregex)
;;   "find all NAME on MEMBERS
;; MEMBERS is of the type returned by ebrowse-ts-member-* on a ebrowse tree struct
;; "
;;   (cond
;;    ((null (car members)) nil)
;;    ( (if doregex
;;          (string-match name (aref (car members) 1))
;;          (string-equal name (aref (car members) 1)))
;;      (cons (car members)   (jvebrowse-find-name  name (cdr members) doregex) )
;;      )
;;    (t (jvebrowse-find-name  name (cdr members) doregex))
;;    )
;;   )


;iterative redefine version
(defun jvebrowse-find-name (name  members doregex)
  "Find all NAME on MEMBERS.
Optional DOREGEX specifies if a regex search should be done.
MEMBERS is of the type returned by ebrowse-ts-member-* on a ebrowse tree struct"
  (let*
      ((result nil))
    (while members
      (if (if doregex
              (string-match name (aref (car members) 1))
            (string-equal name (aref (car members) 1)))
          (setq result (cons (car members) result))
        )
      (setq members (cdr members))
      )
    result)
  )




(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-ebrowse) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (jvebrowse-find table regex t)
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-ebrowse) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    (jvebrowse-find table (concat "^" prefix ".*") t)
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

    ;JAVE hack, will currently return all functions in the table
    ; i think class can be 'function 'variable or 'type
    (cond
     ((eq class 'function)
      (jvebrowse-find2 table ".*" t #'ebrowse-ts-member-functions))
     ((eq class 'variable)
      (jvebrowse-find2 table ".*" t #'ebrowse-ts-member-variables))
     )
    ;

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;; TESTING
;;
;; This is a complex bit of stuff.  Here are some tests for the
;; system.

(defun semanticdb-ebrowse-run-tests ()
  "Run some tests of the semanticdb-ebrowse system.
All systems are different.  Ask questions along the way."
  (interactive)
  (let ((doload nil))
    (when (y-or-n-p "Create a system database to test with? ")
      (call-interactively 'semanticdb-create-ebrowse-database)
      (setq doload t))
    ;;  Should we load in caches
    (when (if doload
	      (y-or-n-p "New database created.  Reload system databases? ")
	    (y-or-n-p "Load in all system databases? "))
      (semanticdb-load-ebrowse-caches)))
  ;; Ok, databases were creatd.  Lets try some searching.
  (when (not (or (eq major-mode 'c-mode)
		 (eq major-mode 'c++-mode)))
    (error "Please make your default buffer be a C or C++ file, then
run the test again..")
    )
  
  )
  

(provide 'semanticdb-ebrowse)

;;; semanticdb-ebrowse.el ends here
