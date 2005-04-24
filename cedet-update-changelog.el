;;; cedet-update-changelog --- Utility for updating changelogs in CEDET.

;;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: cedet-update-changelog.el,v 1.1 2005/04/24 01:21:20 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; I rebuild the ChangeLog from CVS log files using rcs2log for each release.
;; This automates the process, and fixes up the bad email addresses that
;; are created by rcs2log.


;;; History:
;; 

(require 'cedet)
;;; Code:

(defvar cuc-dirs
  (let ((pack cedet-packages)
	(dirs nil))
    (while pack
      (setq dirs
	    (cons (file-name-directory (locate-library
					(symbol-name (car (car pack)))))
		  dirs)
	    pack
	    (cdr pack)))
    dirs)
  "List of directories we need to change the ChangeLog in.")

(defun cuc-update-changelog (dir)
  "Update the changelog in DIR."
  (find-file (concat dir "ChangeLog"))
  (goto-char (point-min))
  (sit-for 0)
  (message "Calling rcs2log on %s..."
	   (file-name-nondirectory (directory-file-name dir)))
  (call-process "rcs2log" nil (current-buffer) nil
		"-c" "ChangeLog")
  (cuc-fixup-ChangeLog-names)
  (save-buffer))

(defun cuc-update-all-changelogs ()
  "Update all ChangeLogs for CEDET."
  (interactive)
  (let ((d cuc-dirs))
    (if (not (y-or-n-p
	      (format "Update ChangeLogs in %s? " (car d))))
	(error "Ok"))
    (while d
      (cuc-update-changelog (car d))
      (setq d (cdr d)))))

(defun cuc-fixup-ChangeLog-names ()
  "Update the names in the current ChangeLog."
  (interactive)
  (save-excursion
    ;; Eric's Name
    (goto-char (point-min))
    (while (re-search-forward (regexp-quote "<zappo@projectile>")
			      nil t)
      (replace-match "<zappo@gnu.org>" t t))
    ;; David's Name
    (goto-char (point-min))
    (while (re-search-forward (regexp-quote "ponced  <ponced@projectile>")
			      nil t)
      (replace-match "David Ponce  <david@dponce.com>" t t))
    ))
