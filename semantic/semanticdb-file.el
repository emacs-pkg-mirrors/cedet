;;; semanticdb-file.el --- Save a semanticdb to a cache file.

;;; Copyright (C) 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-file.el,v 1.1 2002/08/11 01:35:40 zappo Exp $

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
;; A set of semanticdb classes for persistently saving caches on disk.
;;

(require 'semanticdb)

;;; Settings
;;
(defcustom semanticdb-default-file-name "semantic.cache"
  "*File name of the semantic token cache."
  :group 'semanticdb
  :type 'string)

(defcustom semanticdb-default-save-directory nil
  "*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
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

(defcustom semanticdb-save-database-hooks nil
  "*Hooks run after a database is saved.
Each function is called with one argument, the object representing
the database recently written."
  :group 'semanticdb
  :type 'hook)

;;; Classes
;;
(defclass semanticdb-project-database-file (semanticdb-project-database
					    eieio-persistent)
  ((file-header-line :initform ";; SEMANTICDB Tags save file")
   )
  "Database of file tables saved to disk.")

;;; Code:
;;
(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database-file)
					       directory)
  "Create a new semantic database for DIRECTORY and return it.
If a database for DIRECTORY has already been loaded, return it.
If a database for DIRECTORY exists, then load that database, and return it.
If DIRECTORY doesn't exist, create a new one."
  (let* ((fn (semanticdb-cache-filename directory))
	 (db (or (semanticdb-file-loaded-p fn)
		 (if (file-exists-p fn)
		     (semanticdb-load-database fn)))))
    (unless db
      (setq db (semanticdb-project-database-file
		(file-name-nondirectory fn)
		:file fn :tables nil)))
    db))

(defun semanticdb-file-loaded-p (filename)
  "Return the project belonging to FILENAME if it was already loaded."
  (eieio-instance-tracker-find filename 'file 'semanticdb-database-list))

(defmethod semanticdb-live-p ((obj semanticdb-project-database))
  "Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories."
  (let ((full-dir (file-name-directory (oref obj file))))
    (file-exists-p full-dir)))


;;; File IO
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

(defmethod semanticdb-save-db ((DB semanticdb-project-database-file))
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

;;; State queries

(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database-file))
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
      (call-next-method))
    ))

;;; Filename manipulation
;;
(defun semanticdb-cache-filename (path)
  "Return a file to a cache file belonging to PATH.
This could be a cache file in the current directory, or an encoded file
name in a secondary directory."
  (if semanticdb-default-save-directory
      (let ((file path) dir-sep-string)
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


(provide 'semanticdb-file)

;;; semanticdb-file.el ends here
