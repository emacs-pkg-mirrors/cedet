;;; ede-pconf.el --- configure.in maintenance for EDE

;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project
;; RCS: $Id: ede-pconf.el,v 1.3 2000/07/11 23:11:12 zappo Exp $

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
;; Code generator for autoconf configure.in, and support files.

(eval-and-compile (require 'ede))
(require 'autoconf-edit)

;;; Code:
(defmethod ede-proj-configure-file ((this ede-proj-project))
  "The configure.in script used by project THIS."
  (ede-expand-filename (ede-toplevel this) "configure.in"))

(defmethod ede-proj-configure-synchronize ((this ede-proj-project))
  "Synchronize what we know about project THIS into configure.in."
  (let ((b (find-file-noselect (ede-proj-configure-file this)))
	(td (file-name-directory (ede-proj-configure-file this)))
	(targs (oref this targets))
	(postcmd ""))
    ;; First, make sure we have a file.
    (if (not (file-exists-p (ede-proj-configure-file this)))
	(autoconf-new-program b (oref this name) "Project.ede"))
    (set-buffer b)
    ;; Next, verify all targets of all subobjects.
    (autoconf-set-version (oref this version))
    (autoconf-set-output '("Makefile"))
     ;;
     ;; NOTE TO SELF.  TURN THIS INTO THE OFFICIAL LIST
     ;;
    (ede-proj-dist-makefile this)
    ;; Loop over all targets to clean and then add themselves in.
    (while targs
      (ede-proj-flush-autoconf (car targs))
      (setq targs (cdr targs)))
    (setq targs (oref this targets))
    (while targs
      (ede-proj-tweek-autoconf (car targs))
      (setq targs (cdr targs)))
    ;; Now save
    (save-buffer)
    ;; Verify aclocal
    (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						 "aclocal.m4")))
	(setq postcmd "aclocal;autoconf;autoheader;")
      ;; Verify the configure script...
      (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						   "configure")))
	  (setq postcmd "autoconf;autoheader;")
	(if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						     "config.h.in")))
	    (setq postcmd "autoheader;"))))
    ;; Verify Makefile.in, and --add-missing files (cheaply)
    (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						 "Makefile.in")))
	(progn
	  (setq postcmd (concat postcmd "automake"))
	  (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						       "COPYING")))
	      (setq postcmd (concat postcmd " --add-missing")))
	  (setq postcmd (concat postcmd ";")))
      (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						   "COPYING")))
	  (setq postcmd (concat postcmd "automake --add-missing;"))))
    (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						 "AUTHORS")))
	(save-excursion
	  (find-file (ede-expand-filename (ede-toplevel this)
					  "AUTHORS"))
	  (insert (user-full-name) " <" (user-login-name) ">")
	  (save-buffer)
	  (if (not
	       (y-or-n-p "I had to create the AUTHORS file for you.  Ok? "))
	      (error "Quit"))))
    ;; Verify that we have a make system.
    (if (or (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						     "Makefile")))
	    ;; Now is this one of our old Makefiles?
	    (save-excursion
	      (set-buffer (find-file-noselect
			   (ede-expand-filename (ede-toplevel this)
						"Makefile") t))
	      (goto-char (point-min))
	      ;; Here is the unique piece for our makefiles.
	      (re-search-forward "For use with: make" nil t)))
	(setq postcmd (concat postcmd "./configure;")))
    (if (not (string= "" postcmd))
	(progn
	  (compile postcmd)
	  (switch-to-buffer "*Help*")
	  (erase-buffer)
	  (insert "Preparing build environment

Rerun the previous ede command when automake and autoconf are completed.")
	  (goto-char (point-min))
	  (error "Preparing build environment: Rerun your command when done")
	  ))))

(defmethod ede-proj-configure-recreate ((this ede-proj-project))
  "Delete project THISes configure script and start over."
  (if (not (ede-proj-configure-file this))
      (error "Could not determine configure.in for %S" (object-name this)))
  (let ((b (get-file-buffer (ede-proj-configure-file this))))
    ;; Destroy all evidence of the old configure.in
    (delete-file (ede-proj-configure-file this))
    (if b (kill-buffer b)))
  (ede-proj-configure-synchronize this))

(defmethod ede-proj-tweek-autoconf ((this ede-proj-target))
  "Tweek the configure file (current buffer) to accomodate THIS."
  nil)

(defmethod ede-proj-flush-autoconf ((this ede-proj-target))
  "Flush the configure file (current buffer) to accomodate THIS.
By flushing, remove any cruft that may be in the file.  Subsequent
calls to `ede-proj-tweek-autoconf' can restore items removed by flush."
  nil)

(provide 'ede-pconf)

;;; ede-pconf.el ends here
