;;; semanticdb.el --- Semantic token database manager

;;; Copyright (C) 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb.el,v 1.2 2000/12/07 04:46:00 zappo Exp $

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

;;; Commentary:
;;
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.
;;
;; Eventually, use EDE to create databases on a per target basis, and
;; then use target dependencies to have them reference each other.

(require 'eieio)

;;; Variables:
(defcustom semanticdb-default-file-name "semantic.cache"
  "*File name of the semantic token cache."
  :group 'semantic
  :type 'string)

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

;;; APIU Code:
(defun semanticdb-file-stream (file)
  "Return a list of tokens belonging to FILE.
If file is loaded, return it's tokens, calling `semantic-bovinate-toplevel'.
If file is not loaded, and tokens are available in the database, return them.
If file is not loaded, and not tokens are available in the data base,
load the file, and call `semantic-bovinate-toplevel'."
  (message "Not yet implemented.")
  )

;;; Classes:
(defclass semanticdb-project-database (eieio-speedbar-directory-button
				       eieio-persistent
				       eieio-instance-tracker)
  ((tracking-symbol :initform semanticdb-database-list)
   (file-header-line :initform ";; SEMANTICDB Tags save file")
   (tables :initarg :tables
	   :type list
	   :documentation "List of `semantic-db-table' objects."))
  "Database of file tables.")

(defclass semanticdb-table (eieio-speedbar-file-button)
  ((file :initarg :file
	 :documentation "File name relative to the parent database.
This is for the file whose tags are stored in this TABLE object.")
   (tokens :initarg :tokens
	   :documentation "The tokens belonging to this table."))
  "A single table of tokens belonging to a given file.")

;;; Storage Code:
(defun semanticdb-create-database (filename)
  "Create a semantic database in FILENAME and return it.
If FILENAME has already been loaded, return it.
If FILENAME exists, then load that database, and return it.
If FILENAME doesn't exist, create a new one."
  (if (file-exists-p filename)
      (or (semanticdb-file-loaded-p filename)
	  (semanticdb-load-database filename))
    (semanticdb-project-database (file-name-nondirectory filename)
				 :file filename
				 :tables nil)))

(defun semanticdb-get-database (filename)
  "Get a database for FILENAME.
If one isn't found, create one."
  (or (eieio-instance-tracker-find filename 'file 'semanticdb-database-list)
      (semanticdb-create-database filename)))

(defun semanticdb-load-database (filename)
  "Load the database FILENAME."
  (eieio-persistent-read filename))

(defun semanticdb-file-loaded-p (filename)
  "Return the project belonging to FILENAME if it was already loaded."
  (object-assoc filename 'file semanticdb-database-list))

(defun semanticdb-save-db (&optional DB)
  "Write out the database DB to its file.
If DB is not specified, then use the current database."
  (eieio-persistent-save (or DB semanticdb-current-database)))

(defun semanticdb-save-all-db ()
  "Save all semantic token databases."
  (mapcar 'semanticdb-save-db semanticdb-database-list))

(defmethod object-write ((obj semanticdb-table))
  "When writing a table, we have to make sure we deoverlay it first.
Restore the overlays after writting.
Argument OBJ is the object to write."
  (let ((b (get-file-buffer (oref obj file))))
    (save-excursion
      (if b (progn (set-buffer b) (semantic-deoverlay-cache))))
    (call-next-method)
    (save-excursion
      (if b (progn (set-buffer b) (semantic-overlay-cache))))
    ))

;;; hooks and Hats:
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `find-file-hooks'.
Sets up the semanticdb environment."
  (let ((cdb nil)
	(ctbl nil))
    (if (not (and semanticdb-semantic-init-hook-overload
		  (setq cdb (run-hooks semanticdb-semantic-init-hook-overload))))
	(setq cdb
	      (semanticdb-get-database
	       (concat (file-name-directory (buffer-file-name))
		       semanticdb-default-file-name))))
    (setq semanticdb-current-database cdb)
    (setq ctbl (object-assoc (eieio-persistent-path-relative
			      semanticdb-current-database (buffer-file-name))
			     'file
			     (oref semanticdb-current-database
				   tables)))
    (unless ctbl
      (setq ctbl
 	    (semanticdb-table
	     (eieio-persistent-path-relative
	      semanticdb-current-database (buffer-file-name))
	     :file (eieio-persistent-path-relative
		    semanticdb-current-database (buffer-file-name))
	     ))
      (object-add-to-list semanticdb-current-database
			  'tables
			  ctbl
			  t))
    (setq semanticdb-current-table ctbl)
    (if (or (not (slot-boundp ctbl 'tokens)) (not (oref ctbl tokens)))
	(semantic-bovinate-toplevel t)
      (semantic-set-toplevel-bovine-cache  (oref ctbl tokens))
      (semantic-overlay-cache))
    ))

(defun semanticdb-post-bovination ()
  "Function run after a bovination."
  (if semanticdb-current-table
      (oset semanticdb-current-table tokens semantic-toplevel-bovine-cache)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, an store
it in our database.  If that buffer has not cache, ignore it, we'll
handle it later if need be."
  (if (and semantic-toplevel-bovine-table
	   semantic-toplevel-bovine-cache)
      (semantic-deoverlay-cache)))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-hooks)
    (semanticdb-post-bovination semantic-after-toplevel-bovinate-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

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
	(setq fn 'remove-hook))
    ;(message "ARG = %d" arg)
    (while h
      (funcall fn (car (cdr (car h))) (car (car h)))
      (setq h (cdr h)))))

(provide 'semanticdb)

;;; semanticdb.el ends here
