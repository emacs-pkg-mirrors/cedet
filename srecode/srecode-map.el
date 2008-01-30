;;; srecode-map.el --- Manage a template file map

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-map.el,v 1.2 2008/01/30 16:07:05 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Read template files, and build a map of where they can be found.
;; Save the map to disk, and refer to it when bootstrapping a new
;; Emacs session with srecode.

(require 'semantic)

;;; Code:
(defun srecode-map-base-template-dir ()
  "Find the base template directory for SRecode."
  (let* ((lib (locate-library "srecode.el"))
	 (dir (file-name-directory lib)))
    (expand-file-name "templates/" dir)
    ))


;;; Current MAP
;;

(defvar srecode-current-map nil
  "The current map for global SRecode templtes.")

(defcustom srecode-map-save-file (expand-file-name "~/.srecode/srecode-map")
  "The save location for SRecode's map file."
  :group 'srecode
  :type 'file)

(defclass srecode-map (eieio-persistent)
  ((fileheaderline :initform ";; SRECODE TEMPLATE MAP")
   (files :initarg :files
	  :initform nil
	  :type list
	  :documentation
	  "An alist of files and the major-mode that they cover.")
   ;; @todo - build an application map
   )
  "A map of srecode templates.")

(defmethod srecode-map-entry-for-file ((map srecode-map) file)
  "Return then entry in MAP for FILE."
  (assoc file (oref map files)))

(defmethod srecode-map-entries-for-mode ((map srecode-map) mode)
  "Return the entries in MAP for major MODE."
  (let ((ans nil))
    (dolist (f (oref map files))
      (when (eq (cdr f) mode)
	(setq ans (cons f ans))))
    ans))

(defmethod srecode-map-delete-file-entry ((map srecode-map) file)
  "Update MAP to exclude FILE from the file list."
  (let ((entry (srecode-map-entry-for-file map file)))
    (when entry
      (object-remove-from-list map 'files entry))))

(defmethod srecode-map-update-file-entry ((map srecode-map) file mode)
  "Update a MAP entry for FILE to be used with MODE."
  (let ((entry (srecode-map-entry-for-file map file)))
    (cond 
     ;; It is already a match.. do nothing.
     ((and entry (eq (cdr entry) mode))
      nil)
     ;; We have a non-matching entry.  Change the cdr.
     (entry
      (setcdr entry mode))
     ;; No entry, just add it to the list.
     (t
      (object-add-to-list map 'files (cons file mode))
      ))))


;;; MAP Updating
;;
;;;###autoload
(defun srecode-get-maps (&optional reset)
  "Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map."
  (interactive "P")
  ;; Always update the map, but only do a full reset if
  ;; the user asks for one.
  (srecode-map-update-map (not reset))

  (if (interactive-p)
      ;; Dump this map.
      (with-output-to-temp-buffer "*SRECODE MAP*"
	(princ " -- SRECODE MAP --\n")
	(princ "Mode\t\t\tFilename\n")
	(princ "------\t\t\t------------------\n")
	(dolist (fe (oref srecode-current-map files))
	  (prin1 (cdr fe))
	  (princ "\t")
	  (when (> (* 2 8) (length (symbol-name (cdr fe))))
	    (princ "\t"))
	  (when (> 8 (length (symbol-name (cdr fe))))
	    (princ "\t"))
	  (princ (car fe))
	  (princ "\n")
	  )
	(princ "\n\nUse:\n\n M-x customize-variable RET srecode-map-load-path RET\n")
	(princ "\n To change the path where SRecode loads templates from.")
	)
    ;; Eventually, I want to return many maps to search through.
    (list srecode-current-map)))

(defun srecode-map-update-map (&optional fast)
  "Update the current map from `srecode-map-load-path'.
Scans all the files on the path, and makes sure we have entries
for them.
If option FAST is non-nil, then only parse a file for the mode-string
if that file is NEW, otherwise assume the mode has not changed."
  (interactive)

  ;; 1) Do we even have a MAP or save file?
  (when (and (not srecode-current-map)
	     (not (file-exists-p srecode-map-save-file)))
    (when (not (file-exists-p (file-name-directory srecode-map-save-file)))
      ;; No map, make the dir?
      (if (y-or-n-p (format "Create dir %s? "
			    (file-name-directory srecode-map-save-file)))
	  (make-directory (file-name-directory srecode-map-save-file))
	;; No make, change save file
	(customize-variable 'srecode-map-save-file)
	(error "Change your SRecode map file")))
    ;; Have a dir.  Make the object.
    (setq srecode-current-map
	  (srecode-map "SRecode Map"
		       :file srecode-map-save-file)))

  ;; 2) Do we not have a current map?  If so load.
  (when (not srecode-current-map)
    (setq srecode-current-map
	  (eieio-persistent-read srecode-map-save-file))
    )

  ;;
  ;; We better have a MAP object now.
  ;;

  ;; 3) - Purge dead files from the file list.
  (dolist (entry (copy-list (oref srecode-current-map files)))
    (when (not (file-exists-p (car entry)))
      (srecode-map-delete-file-entry srecode-current-map (car entry))
      ))
  ;; 4) - Find new files and add them to the map.
  (dolist (dir srecode-map-load-path)
    (dolist (f (directory-files dir t "\\.srt$"))
      (srecode-map-validate-file-for-mode f fast)
      ))
  
  (eieio-persistent-save srecode-current-map)
  )

(defun srecode-map-validate-file-for-mode (file fast)
  "Read and validate FILE via the parser.  Return the mode.
Argument FAST implies that the file should not be reparsed if there
is already an entry for it."
  (when (or (not fast)
	    (not (srecode-map-entry-for-file srecode-current-map file)))
    (let ((buff-orig (get-file-buffer file)))
      (save-excursion
	(if buff-orig
	    (set-buffer buff-orig)
	  (set-buffer (get-buffer-create " *srecode-map-tmp*"))
	  (insert-file-contents file nil nil nil t)
	  ;; Force it to be ready to parse.
	  (srecode-template-mode)
	  (semantic-new-buffer-fcn)
	  )

	(semantic-fetch-tags)
	(let* ((mode-tag
		(semantic-find-first-tag-by-name "mode" (current-buffer)))
	       (val nil))
	  (if mode-tag
	      (setq val (car (semantic-tag-variable-default mode-tag)))
	    (error "There should be a mode declaration in %s" file))

	  (srecode-map-update-file-entry srecode-current-map
					 file
					 (read val))
	  )
	))))


;;; THE PATH
;;
;; We need to do this last since the setter needs the above code.

(defun srecode-map-load-path-set (sym val)
  "Set SYM to the new VAL, then update the srecode map."
  (set-default sym val)
  (srecode-map-update-map t))

(defcustom srecode-map-load-path
  (list (srecode-map-base-template-dir)
	(expand-file-name "~/.srecode/")
	)
  "*Global load path for SRecode template files."
  :group 'srecode
  :type '(repeat file)
  :set 'srecode-map-load-path-set)


(provide 'srecode-map)
;;; srecode-map.el ends here
