;;; ede-proj-elisp.el --- EDE Generic Project Emacs Lisp support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-elisp.el,v 1.7 2000/07/12 14:14:22 zappo Exp $

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Handle Emacs Lisp in and EDE Project file.

(eval-and-compile (require 'ede-proj))

;;; Code:
(defclass ede-proj-target-elisp (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (aux-packages :initarg :aux-packages
		 :initform nil
		 :type list
		 :custom (repeat string)
		 :documentation "Additional packages needed.
There should only be one toplevel package per auxiliary tool needed.
These packages location is found, and added to the compile time
load path."
   ))
  "This target consists of a group of lisp files.
A lisp target may be one general program with many separate lisp files in it.")

(defmethod ede-want-file-p ((obj ede-proj-target-elisp) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.el$" file))

(defun ede-proj-elisp-packages-to-loadpath (packages)
  "Convert a list of PACKAGES, to a list of load path."
  (let ((paths nil))
    (while packages
      (if (not (locate-library (car packages)))
	  (error "Cannot find package %s" (car packages)))
      (setq paths (cons (file-name-directory (locate-library (car packages)))
			paths)
	    packages (cdr packages)))
    paths))

(defmethod project-compile-target ((obj ede-proj-target-elisp))
  "Compile all sources in a Lisp target OBJ."
  (let ((cb (current-buffer)))
    (mapcar (lambda (src)
	      (let ((elc (concat (file-name-sans-extension src) ".elc")))
		(set-buffer cb)
		(if (or (not (file-exists-p elc))
			(file-newer-than-file-p src elc))
		    (byte-compile-file src))))
	    (oref obj source)))
  (message "All Emacs Lisp sources are up to date in %s" (object-name obj)))

;;; Makefile generation functions
;;
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-elisp))
  "Return the variable name for THIS's sources."
  (cond ((ede-proj-automake-p)
	 "lisp_LISP")
	(t (concat (ede-pmake-varname this) "_LISP"))))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-elisp))
  "Insert variables needed by target THIS."
  (call-next-method this)
  (insert "EMACS=" (file-name-nondirectory (car command-line-args)) "\n")
  (if (oref this aux-packages)
      (insert "LOADPATH=" (mapconcat (lambda (a) a)
				     (ede-proj-elisp-packages-to-loadpath
				      (oref this aux-packages))
				     " ")
	      "\n")))

(defmethod ede-proj-makefile-garbage-patterns ((this ede-proj-target-elisp))
  "Return a list of patterns that are considred garbage to THIS.
These are removed with make clean."
  '("*.elc")
  )

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-elisp))
  "Insert rules to build THIS set of Emacs Lisp files."
  (call-next-method)
  (insert (ede-name this) ":\n"
	  "\t@echo \"(add-to-list 'load-path \\\"$(PWD)\\\")\" > "
	  (ede-name this) "-compile-script\n")
  (if (oref this aux-packages)
      (progn
	(insert "\t@for loadpath in ${LOADPATH}; do \\\n")
	(insert "\t  echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> "
		(ede-name this) "-compile-script; \\\n")
	(insert "\t  done\n")))
  (insert "\t@echo \"(setq debug-on-error t)\" >> "
	  (ede-name this) "-compile-script\n")
  (insert "\t$(EMACS) -batch -l " (ede-name this) "-compile-script "
	  "-f batch-byte-compile  $(" (ede-proj-makefile-sourcevar this)
	  ")\n"))

(defun ede-proj-elisp-add-path (path)
  "Add path PATH into the file if it isn't already there."
  (goto-char (point-min))
  (if (re-search-forward (concat "(cons \\\""
				 (regexp-quote path))
			 nil t)
      nil;; We have it already
    (if (re-search-forward "(cons nil" nil t)
	(progn
	  ;; insert stuff here
	  (end-of-line)
	  (insert "\n"
		  "   echo \"(setq load-path (cons \\\""
		  path
		  "\\\" load-path))\" >> script")
	  )
      (error "Don't know how to update load path"))))

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-elisp))
  "Tweak the configure file (current buffer) to accomodate THIS."
  (autoconf-insert-new-macro "AM_PATH_LISPDIR")
  ;; Ok, now we have to tweak the autoconf provided `elisp-comp' program.
  (let ((ec (ede-expand-filename this "elisp-comp")))
    (if (not (file-exists-p ec))
	(message "There may be compile errors.  Rerun a second time.")
      (save-excursion
	(if (file-symlink-p ec)
	    (progn
	      ;; Desymlinkafy
	      (rename-file ec (concat ec ".tmp"))
	      (copy-file (concat ec ".tmp") ec)
	      (delete-file (concat ec ".tmp"))))
	(set-buffer (find-file-noselect ec t))
	(ede-proj-elisp-add-path "..")
	(let ((paths (ede-proj-elisp-packages-to-loadpath
		      (oref this aux-packages))))
	  ;; Add in the current list of paths
	  (while paths
	    (ede-proj-elisp-add-path (car paths))
	    (setq paths (cdr paths))))
	(save-buffer)) )))

(defmethod ede-proj-flush-autoconf ((this ede-proj-target-elisp))
  "Flush the configure file (current buffer) to accomodate THIS."
  ;; Remove crufty old paths from elisp-compile
  (let ((ec (ede-expand-filename this "elisp-comp"))
	(paths (ede-proj-elisp-packages-to-loadpath
		(oref this aux-packages))))
    (if (file-exists-p ec)
	(save-excursion
	  (set-buffer (find-file-noselect ec t))
	  (goto-char (point-min))
	  (while (re-search-forward "(cons \\([^ ]+\\) load-path)"
				    nil t)
	    (let ((path (match-string 1)))
	      (if (string= path "nil")
		  nil
		(delete-region (save-excursion (beginning-of-line) (point))
			       (save-excursion (end-of-line)
					       (forward-char 1)
					       (point))))))))))

(provide 'ede-proj-elisp)

;;; ede-proj-elisp.el ends here
