;;; ede-pconf.el --- configure.in maintenance for EDE


;; This is incomplete and non-function!



;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-pconf.el,v 1.2 2000/07/07 20:28:34 zappo Exp $

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

(require 'autoconf-edit)

;;; Code:
(defmethod ede-proj-configure-file ((this ede-proj-project))
  "The configure.in script used by project THIS."
  (let ((rp (ede-toplevel this)))
    (concat (file-name-directory (oref rp file)) "configure.in")))

(defmethod ede-proj-configure-synchronize ((this ede-proj-project))
  "Synchronize what we know about project THIS into configure.in."
  (let ((b (find-file-noselect (ede-proj-configure-file this)))
	(targs (oref this targets)))
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
    ;; Loop over all targets to add themselves in.
    (while targs
      (ede-proj-tweek-autoconf (car targs))
      (setq targs (cdr targs)))
    ;; Now save
    (save-buffer)))

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

(provide 'ede-pconf)

;;; ede-pconf.el ends here
