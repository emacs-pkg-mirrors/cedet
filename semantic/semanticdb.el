;;; semanticdb.el --- Semantic token database manager

;;; Copyright (C) 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb.el,v 1.41 2002/06/14 13:11:46 zappo Exp $

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
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.
;;

(require 'eieio-base)
(require 'semantic)

;;; Variables:
(defgroup semanticdb nil
  "Parser Generator Persistent Database interface."
  :group 'semantic
  )

;;;###autoload
(defcustom semanticdb-global-mode nil
  "*If non-nil enable the use of `semanticdb-minor-mode'."
  :group 'semantic
  :type 'boolean
  :require 'semanticdb
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semanticdb-minor-mode (if val 1 -1))
         (custom-set-default sym val)))

(defcustom semanticdb-default-file-name "semantic.cache"
  "*File name of the semantic token cache."
  :group 'semanticdb
  :type 'string)

(defcustom semanticdb-default-save-directory nil
  "*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory. If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory."
  :group 'semanticdb
  :type '(choice :tag "Default-Directory"
                 :menu-tag "Default-Directory"
                 (const :tag "Use current directory" :value nil)
                 (directory)))

(defcustom semanticdb-persistent-path '(project)
  "*List of valid paths that semanticdb will cache tokens to.
When `global-semanticdb-minor-mode' is active, token lists will
be saved to disk when Emacs exits.  Not all directories will have
tokens that should be saved.
The value should be a list of valid paths.  A path can be a string,
indicating a directory in which to save a variable.  An element in the
list can also be a symbol.  Valid symbols are `never', which will
disable any saving anywhere, `always', which enables saving
everywhere, or `project', which enables saving in any directory that
passes a list of predicates in `semantic-project-predicates'."
  :group 'semanticdb
  :type nil)

(defcustom semanticdb-mode-hooks nil
  "*Hooks run whenever `global-semanticdb-minor-mode' is run.
Use `semanticdb-minor-mode-p' to determine if the mode has been turned
on or off."
  :group 'semanticdb
  :type 'hook)

(defcustom semanticdb-save-database-hooks nil
  "*Hooks run after a database is saved.
Each function is called with one argument, the object representing
the database recently written."
  :group 'semanticdb
  :type 'hook)

(defvar semanticdb-database-list nil
  "List of all active databases.")

(defvar semanticdb-semantic-init-hook-overload nil
  "Semantic init hook overload.
Tools wanting to specify the file names of the semantic database
use this.")

(defvar semanticdb-current-database nil
  "For a given buffer, this is the currently active database.")
(make-variable-buffer-local 'semanticdb-current-database)

(defvar semanticdb-current-table nil
  "For a given buffer, this is the currently active database table.")
(make-variable-buffer-local 'semanticdb-current-table)

;;; Classes:
(defclass semanticdb-project-database (eieio-persistent
				       eieio-instance-tracker)
  ((tracking-symbol :initform semanticdb-database-list)
   (file-header-line :initform ";; SEMANTICDB Tags save file")
   (reference-directory :type string
			:documentation "Directory this database refers to.
When a cache directory is specified, then this refers to the directory
this database contains symbols for.")
   (tables :initarg :tables
	   :type list
	   :documentation "List of `semantic-db-table' objects."))
  "Database of file tables.")

(defclass semanticdb-table ()
  ((parent-db :documentation "Database Object containing this table.")
   (file :initarg :file
	 :documentation "File name relative to the parent database.
This is for the file whose tags are stored in this TABLE object.")
   (pointmax :initarg :pointmax
	     :initform nil
	     :documentation "Size of buffer when written to disk.
Checked on retrieval to make sure the file is the same.")
   (major-mode :initarg :major-mode
	       :initform nil
	       :documentation "Major mode this table belongs to.
Sometimes it is important for a program to know if a given table has the
same major mode as the current buffer.")
   (tokens :initarg :tokens
	   :documentation "The tokens belonging to this table.")
   (unmatched-syntax :initarg :unmatched-syntax
		     :documentation
		     "List of vectors specifying unmatched syntax.")
   )
  "A single table of tokens belonging to a given file.")

;;; Code:
(defun semanticdb-create-database (filename)
  "Create a semantic database in FILENAME and return it.
If FILENAME has already been loaded, return it.
If FILENAME exists, then load that database, and return it.
If FILENAME doesn't exist, create a new one."
  (let ((db (if (file-exists-p filename)
		(or (semanticdb-file-loaded-p filename)
		    (semanticdb-load-database filename)))))
    (unless db
      (setq db (semanticdb-project-database (file-name-nondirectory filename)
					    :file filename
					    :tables nil)))
    db))

(defun semanticdb-file-loaded-p (filename)
  "Return the project belonging to FILENAME if it was already loaded."
  (eieio-instance-tracker-find filename 'file 'semanticdb-database-list))

(defun semanticdb-get-database (filename)
  "Get a database for FILENAME.
If one isn't found, create one."
  (or (semanticdb-file-loaded-p filename)
      (semanticdb-create-database filename)))

(defun semanticdb-load-database (filename)
  "Load the database FILENAME."
  (condition-case foo
      (let* ((r (eieio-persistent-read filename))
	     (c (oref r tables)))
	(while c
	  ;; Restore the parent-db connection
	  (oset (car c) parent-db r)
	  (setq c (cdr c)))
	r)
    (error (message "Cache Error: %s, Restart" foo)
	   nil)))

(defmethod semanticdb-live-p ((obj semanticdb-project-database))
  "Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories."
  (let ((full-dir (file-name-directory (oref obj file))))
    (file-exists-p full-dir)))

(defmethod semanticdb-file-table ((obj semanticdb-project-database) filename)
  "From OBJ, return FILENAMEs associated table object."
  (object-assoc (file-name-nondirectory filename) 'file (oref obj tables)))

(defun semanticdb-save-db (&optional DB)
  "Write out the database DB to its file.
If DB is not specified, then use the current database."
  (let ((objname (oref DB file)))
    (when (and (semanticdb-live-p DB)
	       (semanticdb-write-directory-p DB))
      (message "Saving token summary for %s..." objname)
      (condition-case foo
	  (eieio-persistent-save (or DB semanticdb-current-database))
	(file-error ; System error saving?  Ignore it.
	 (message "Error saving %s" objname))
	(error
	 (if (and (listp foo)
		  (stringp (nth 1 foo))
		  (string-match "write-protected" (nth 1 foo)))
	     (message (nth 1 foo))
	   (error "%S" foo))))
      (run-hook-with-args 'semanticdb-save-database-hooks
			  (or DB semanticdb-current-database))
      (message "Saving token summary for %s...done" objname))
    ))

(defun semanticdb-save-all-db ()
  "Save all semantic token databases."
  (interactive)
  (message "Saving token summaries...")
  (mapcar 'semanticdb-save-db semanticdb-database-list)
  (message "Saving token summaries...done"))

(defmethod semanticdb-full-filename ((obj semanticdb-table))
  "Fetch the full filename that OBJ refers to."
  (concat (file-name-directory (oref (oref obj parent-db) reference-directory))
	  (oref obj file)))

(defmethod semanticdb-live-p ((obj semanticdb-table))
  "Return non-nil if the file associated with OBJ is live.
Live files are either buffers in Emacs, or files existing on the filesystem."
  (let ((full-filename (semanticdb-full-filename obj)))
    (or (get-file-buffer full-filename)
	(file-exists-p full-filename))))

(defmethod object-write ((obj semanticdb-table))
  "When writing a table, we have to make sure we deoverlay it first.
Restore the overlays after writting.
Argument OBJ is the object to write."
  (if (semanticdb-live-p obj)
      (let ((b (get-file-buffer (semanticdb-full-filename obj))))
	(save-excursion
	  (if b (progn (set-buffer b)
		       (condition-case nil
			   (semantic-deoverlay-cache)
			 (error
			  (condition-case nil
			      (semantic-clear-toplevel-cache)
			    (error
			     (semantic-set-toplevel-bovine-cache nil)))))
		       (oset obj pointmax (point-max)))))
	(call-next-method)
	(save-excursion
	  (if b (progn (set-buffer b) (semantic-overlay-cache))))
	)))

(defmethod semanticdb-set-buffer ((obj semanticdb-table))
  "Set the current buffer to be a buffer owned by OBJ.
If OBJ's file is not loaded, read it in first."
  (set-buffer (find-file-noselect (semanticdb-full-filename obj))))

(defmethod semanticdb-refresh-table ((obj semanticdb-table))
  "If the token list associated with OBJ is loaded, refresh it.
This will call `semantic-bovinate-toplevel' if that file is in memory."
  (let ((ff (semanticdb-full-filename obj)))
    (if (get-file-buffer ff)
	(save-excursion
	  (semanticdb-set-buffer obj)
	  (semantic-bovinate-toplevel t)))))

;;; Directory Project support
(defvar semanticdb-project-predicates nil
  "List of predicates to try that indicate a directory belongs to a project.
This list is used when `semanticdb-persistent-path' contains the value
'project.  If the predicate list is nil, then presume all paths are valid.

Project Management software (such as EDE and JDE) should add their own
predicates with `add-hook' to this variable, and semanticdb will save token
caches in directories controlled by them.")

(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  (let ((path semanticdb-persistent-path))
    (catch 'found
      (while path
	(cond ((stringp (car path))
	       (if (string= (oref obj reference-directory) (car path))
		   (throw 'found t)))
	      ((eq (car path) 'project)
	       (let ((predicates semanticdb-project-predicates))
		 (if predicates
		     (while predicates
		       (if (funcall (car predicates)
				    (oref obj reference-directory))
			   (throw 'found t))
		       (setq predicates (cdr predicates)))
		   ;; If the mode is 'project, and there are no project
		   ;; modes, then just always save the file.  If users
		   ;; wish to restrict the search, modify
		   ;; `semanticdb-persistent-path' to include desired paths.
		   (if (= (length semanticdb-persistent-path) 1)
		       (throw 'found t))
		   )))
	      ((eq (car path) 'never)
	       (throw 'found nil))
	      ((eq (car path) 'always)
	       (throw 'found t))
	      (t (error "Invalid path %S" (car path))))
	(setq path (cdr path)))
      nil)
    ))

;;; Filename manipulation
;;
(defun semanticdb-cache-filename (path)
  "Return a file to a cache file belonging to PATH.
This could be a cache file in the current directory, or an encoded file
name in a secondary directory."
  (if semanticdb-default-save-directory
      (let ((file path))
        (when (memq system-type '(windows-nt ms-dos))
          ;; Normalize DOSish file names: convert all slashes to
          ;; directory-sep-char, downcase the drive letter, if any,
          ;; and replace the leading "x:" with "/drive_x".
          (or (file-name-absolute-p file)
              (setq file (expand-file-name file))) ; make defaults explicit
          ;; Replace any invalid file-name characters (for the
          ;; case of backing up remote files).
          (setq file (expand-file-name (convert-standard-filename file)))
          (setq dir-sep-string (char-to-string directory-sep-char))
          (if (eq (aref file 1) ?:)
              (setq file (concat dir-sep-string
                                 "drive_"
                                 (char-to-string (downcase (aref file 0)))
                                 (if (eq (aref file 2) directory-sep-char)
                                     ""
                                   dir-sep-string)
                                 (substring file 2)))))
        ;; Make the name unique by substituting directory
        ;; separators.  It may not really be worth bothering about
        ;; doubling `!'s in the original name...
        (setq file (subst-char-in-string
                    directory-sep-char ?!
                    (replace-regexp-in-string "!" "!!" file)))
        ;; Now create a filename for the cache file in
        ;; `semanticdb-default-save-directory'.
     (expand-file-name
         (concat (file-name-as-directory semanticdb-default-save-directory)
                 file)))
    (concat (file-name-directory (buffer-file-name))
         semanticdb-default-file-name)))

;;; Hooks:
;;
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `find-file-hooks'.
Sets up the semanticdb environment."
  (let ((cdb nil)
	(ctbl nil))
    ;; Allow a database override function
    (when (not (and semanticdb-semantic-init-hook-overload
		    (setq cdb (run-hooks 'semanticdb-semantic-init-hook-overload))))
      (setq cdb
	    (semanticdb-get-database
	     (semanticdb-cache-filename default-directory)))
      )
    ;; Do this outside of the find to make sure that when people upgrade
    ;; that they get this set properly.
    (oset cdb reference-directory default-directory)
    ;; Get the current DB for this directory
    (setq semanticdb-current-database cdb)
    ;; Get a table for this file.
    (setq ctbl (semanticdb-file-table cdb (buffer-file-name)))
    (unless ctbl
      ;; Create a table if none exists.
      (setq ctbl
 	    (semanticdb-table
	     (file-name-nondirectory (buffer-file-name))
	     :file (file-name-nondirectory (buffer-file-name))
	     ))
      (oset ctbl parent-db cdb)
      (object-add-to-list semanticdb-current-database
			  'tables
			  ctbl
			  t))
    ;; Local state
    (setq semanticdb-current-table ctbl)
    (oset semanticdb-current-table major-mode major-mode)
    ;; Try to swap in saved tokens
    (if (or (not (slot-boundp ctbl 'tokens)) (not (oref ctbl tokens))
	    (/= (or (oref ctbl pointmax) 0) (point-max))
	    )
	(semantic-clear-toplevel-cache)
      (condition-case nil
          (semantic-set-unmatched-syntax-cache
           (oref ctbl unmatched-syntax))
        (unbound-slot
         ;; Old version of the semanticdb table can miss the unmatched
         ;; syntax slot.  If so, just clear the unmatched syntax cache.
         (semantic-clear-unmatched-syntax-cache)))
      (semantic-set-toplevel-bovine-cache  (oref ctbl tokens))
      (semantic-overlay-cache)
      )
    ))

(defun semanticdb-post-bovination (new-table)
  "Function run after a bovination.
Argument NEW-TABLE is the new table of tokens."
  (if semanticdb-current-table
      (oset semanticdb-current-table tokens new-table)))

(defun semanticdb-post-bovination-unmatched-syntax (new-un-tax)
  "Function run after a bovination w/ unmatched syntax.
Argument NEW-UN-TAX is the new unmatched syntax table."
  (if semanticdb-current-table
      (oset semanticdb-current-table unmatched-syntax new-un-tax)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, an store
it in our database.  If that buffer has not cache, ignore it, we'll
handle it later if need be."
  (if (and (semantic-active-p)
	   semantic-toplevel-bovine-cache
	   semanticdb-current-table)
      (progn
	(oset semanticdb-current-table pointmax (point-max))
	(condition-case nil
	    (semantic-deoverlay-cache)
	  ;; If this messes up, just clear the system
	  (error
	   (semantic-clear-toplevel-cache)
	   (message "semanticdb: Failed to deoverlay token cache."))))
    ))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-db-hooks)
    (semanticdb-post-bovination semantic-after-toplevel-cache-change-hook)
    (semanticdb-post-bovination-unmatched-syntax semantic-unmatched-syntax-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

;;;###autoload
(defun global-semanticdb-minor-mode (&optional arg)
  "Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (if (not arg)
      (if (semanticdb-minor-mode-p)
	  (setq arg -1)
	(setq arg 1)))
  (let ((fn 'add-hook)
	(h semanticdb-hooks))
    (if (< arg 0)
	(setq semanticdb-global-mode nil
              fn 'remove-hook)
      (setq semanticdb-global-mode t))
    ;(message "ARG = %d" arg)
    (while h
      (funcall fn (car (cdr (car h))) (car (car h)))
      (setq h (cdr h)))
    ;; Call a hook
    (run-hooks 'semanticdb-mode-hooks)
    ))

(defun semanticdb-toggle-global-mode ()
  "Toggle use of the Semantic Database feature.
Update the environment of Semantic enabled buffers accordingly."
  (interactive)
  (if (semanticdb-minor-mode-p)
      ;; Save databases before disabling semanticdb.
      (semanticdb-save-all-db))
  ;; Toggle semanticdb minor mode.
  (global-semanticdb-minor-mode))

;;; Utilities
;;
;; What is the current database, are two tables of an equivalent mode,
;; and what databases are a part of the same project.
(defun semanticdb-current-database ()
  "Return the currently active database."
  (or semanticdb-current-database
      (and default-directory
	   (semanticdb-get-database (concat default-directory
					    semanticdb-default-file-name))
	   )
      nil))

(defmethod semanticdb-equivalent-mode ((table semanticdb-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (if buffer (set-buffer buffer))
    (or
     ;; nil means the same as major-mode
     (and (not semantic-equivalent-major-modes)
	  (eq major-mode (oref table major-mode)))
     (and semantic-equivalent-major-modes
	  (member (oref table major-mode) semantic-equivalent-major-modes)))
    ))

(defcustom semanticdb-project-roots nil
  "*List of directories, where each directory is the root of some project.
All subdirectories of a root project are considered a part of one project.
Values in this string can be overriden by project management programs
via the `semanticdb-project-root-functions' variable."
  :group 'semanticdb
  :type '(repeat string))

(defvar semanticdb-project-root-functions nil
  "List of functions used to determine a given directories project root.
Functions in this variable can override `semanticdb-project-roots'.
Functions set in the variable are given one argument (a directory) and
must return a string, (the root directory).  This variable should be used
by project management programs like EDE or JDE.")

(defun semanticdb-current-database-list ()
  "Return a list of databases associated with the current buffer.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'."
  (let ((root nil)			; found root directory
	(dbs nil)			; collected databases
	(func semanticdb-project-root-functions) ;special project functions
	(roots semanticdb-project-roots) ;all user roots
	(adb semanticdb-database-list)	; all databases
	)
    (while (and func (not root))
      (setq root (funcall (car func) default-directory)
	    func (cdr func)))
    (while (and roots (not root))
      (if (string-match (concat "^"
				(regexp-quote
				 (expand-file-name (car roots))))
			(expand-file-name default-directory))
	  (setq root (car roots)))
      (setq roots (cdr roots)))
    (if root
        (let ((regexp (concat "^" (regexp-quote (expand-file-name root)))))
          (while (and root adb)
            (if (string-match regexp (oref (car adb) file))
                (setq dbs (cons (car adb) dbs)))
            (setq adb (cdr adb)))))
    dbs))

;;; Search routines
;;
(defun semanticdb-find-nonterminal-by-token
  (token &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (let ((goofy-token-name-thing token))
    (semanticdb-find-nonterminal-by-function
     (lambda (stream sp si)
       (semantic-find-nonterminal-by-token goofy-token-name-thing
					   stream sp si))
     databases search-parts search-includes diff-mode find-file-match)))

(defun semanticdb-find-nonterminal-by-name
  (name &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-name name stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-name-regexp
  (regex &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-name-regexp regex stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-type
  (type &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-type type stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-property
  (property value &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-property property value stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-extra-spec
  (spec &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-extra-spec spec stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-extra-spec-value
  (spec value &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, and FIND-FILE-MATCH.
Return a list ((DB-TABLE . TOKEN-LIST) ...)."
  (semanticdb-find-nonterminal-by-function
   (lambda (stream sp si)
     (semantic-find-nonterminal-by-extra-spec-value spec value stream sp si))
   databases search-parts search-includes diff-mode find-file-match))

(defun semanticdb-find-nonterminal-by-function
  (function &optional databases search-parts search-includes diff-mode find-file-match)
  "Find all occurances of nonterminals which match FUNCTION.
Search in all DATABASES.  If DATABASES is nil, search a range of
associated databases.
When SEARCH-PARTS is non-nil the search will include children of tokens.
When SEARCH-INCLUDES is non-nil, the search will include dependency files.
When DIFF-MODE is non-nil, search databases which are of a different mode.
A Mode is the `major-mode' that file was in when it was last parsed.
When FIND-FILE-MATCH is non-nil, the make sure any found token's file is
in an Emacs buffer.
Return a list ((DB-TABLE . TOKEN-OR-TOKEN-LIST) ...)."
  (if (not databases)
      ;; Calculate what database to use.
      ;; Something simple and dumb for now.
      (setq databases (or (semanticdb-current-database-list)
			  (list (semanticdb-current-database)))))
  (let ((ret nil)
        (case-fold-search semantic-case-fold))
    (while databases
      (let* ((files (oref (car databases) tables))
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
		;; When something is found, make sure we read in that buffer if it had
		;; not already been loaded.
		(if find-file-match
		    (save-excursion (semanticdb-set-buffer (car files))))
		;; In theory, the database is up-to-date with what is in the file, and
		;; these tokens are ready to go.
		;; There is a bug lurking here I don't have time to fix.
		(setq ret (cons (cons (car files) found) ret))
		(setq found nil)))
	  (setq files (cdr files))))
      (setq databases (cdr databases)))
    (nreverse ret)))

(defun semanticdb-file-stream (file)
  "Return a list of tokens belonging to FILE.
If file has database tokens available in the database, return them.
If file does not have tokens available, then load the file, and create them."
  (let* ((fo (semanticdb-get-database (concat (file-name-directory file)
					      semanticdb-default-file-name)))
	 (to nil))
    (if fo (setq to (semanticdb-file-table fo file)))
    (if to
	(oref to tokens) ;; get them.
      ;; We must load the file.
      (save-excursion
	(set-buffer (find-file-noselect file))
	;; Find file should automatically do this for us.
	(if semanticdb-current-table
	    (oref semanticdb-current-table tokens)
	  ;; if not, just do it.
	  (semantic-bovinate-toplevel t))))
    ))

;;; Validate the semantic database
;;
(defun semanticdb-table-oob-sanity-check (cache)
  "Validate that CACHE tokens do not have any overlays in them."
  (while cache
    (when (semantic-overlay-p (semantic-token-overlay cache))
      (message "Token %s has an erroneous overlay!"
	       (semantic-summarize-nonterminal (car cache))))
    (semanticdb-table-oob-sanity-check
     (semantic-nonterminal-children (car cache) t))
    (setq cache (cdr cache))))

(defun semanticdb-table-sanity-check (&optional table)
  "Validate the current semanticdb TABLE."
  (interactive)
  (if (not table) (setq table semanticdb-current-table))
  (let* ((full-filename (semanticdb-full-filename table))
	 (buff (get-file-buffer full-filename)))
    (if buff
	(save-excursion
	  (set-buffer buff)
	  (semantic-sanity-check))
      ;; We can't use the usual semantic validity check, so hack our own.
      (semanticdb-table-oob-sanity-check (oref table tokens)))))

(defun semanticdb-database-sanity-check ()
  "Validate the current semantic database."
  (interactive)
  (let ((tables (oref semanticdb-current-database tables)))
    (while tables
      (semanticdb-table-sanity-check (car tables))
      (setq tables (cdr tables)))
    ))

(provide 'semanticdb)

;;; semanticdb.el ends here
