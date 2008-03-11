;;; cedet-build.el --- Build CEDET within Emacs.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-build.el,v 1.1 2008/03/11 01:06:37 zappo Exp $

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
;; Build all the CEDET parts through EDE.
;;
;;; USAGE:
;;
;; Step 1:  Start emacs like this:
;;
;;     emacs -q -l cedet-build.el -f cedet-build
;;
;; Step 2: Check Output.
;;

;;; Code:

(defvar cedet-build-location
  (let ((dir (file-name-directory
	      (or load-file-name (buffer-file-name)))))
    ;; (add-to-list 'load-path dir)
    dir)
  "Root of the CEDET tree.")


(defun cedet-build-msg (fmt &rest args)
  "Show a build message."
  (save-excursion
    (set-buffer "*CEDET BYTECOMPILE*")
    (goto-char (point-max))
    (insert (apply 'format fmt args))
    (sit-for 0)))

(defun cedet-build ()
  "Build CEDET via EDE."
  (setq inhibit-splash-screen t)
  ;; Make sure CEDET is loaded
  (if (featurep 'cedet)
      (error "To use cedet-build, start Emacs with -q"))

  ;; Setup a logging buffer
  (switch-to-buffer "*CEDET BYTECOMPILE*")
  (delete-other-windows)
  (cedet-build-msg "CEDET BYTE COMPILATION STATUS:\n\n")
  (cedet-build-msg "STEP 1: Byte compile EIEIO...")

  ;; Get EIEIO built first.
  (save-excursion
    (load-file "common/inversion.el")
    (load-file "eieio/eieio-comp.el")
    (byte-compile-file "eieio/eieio.el")
    )
  (cedet-build-msg "done\n\n")

  ;; Fire up CEDET and EDE
  (cedet-build-msg "Loading common/cedet.el ...")
  (save-excursion
    (load-file (expand-file-name "common/cedet.el" cedet-build-location)))

  (cedet-build-msg "done\nTurning on EDE ...")
  (save-excursion
    (global-ede-mode 1))
  (cedet-build-msg "done.\n\n")

  ;; Load in the Makefile
  (let ((buf (find-file-noselect
	      (expand-file-name "Makefile" cedet-build-location)))
	(pkgs nil)
	(subdirs nil)
	)
    (save-excursion
      (set-buffer buf)
      ;; Force a parse
      (semantic-fetch-tags)
      ;; Find the variable
      (setq pkgs (semantic-find-first-tag-by-name "CEDET_PACKAGES"
						  (current-buffer)))
      (setq subdirs (semantic-tag-variable-default pkgs))
      (message "To build subdirs: %S" subdirs)
      (dolist (d subdirs)
	;; For each directory, get the project, and then targets
	;; and run a build on them.
	(cedet-build-msg "Building project %s\n" d)

	(let ((proj (ede-current-project (file-name-as-directory
					  (expand-file-name
					   d cedet-build-location))))
	      )
	  (dolist (targ (oref proj targets))
	    (when (and (or (ede-proj-target-elisp-p targ)
			   (ede-proj-target-elisp-autoloads-p targ))
		       (condition-case nil
			   (oref targ :partofall)
			 (error nil)))

	      (cedet-build-msg "   Target %s..." (object-name targ))

	      ;; If it is an autoload or elisp target, then
	      ;; do that work here.
	      (let ((ans (project-compile-target targ)))
		(if (and (consp ans)
			 (numberp (car ans)))
		    (cedet-build-msg "%d compiled, %d up to date.\n"
				     (car ans) (cdr ans))
		  (cedet-build-msg "done.\n"))
		))
	    ))
	))))


(provide 'cedet-build)
;;; cedet-build.el ends here
