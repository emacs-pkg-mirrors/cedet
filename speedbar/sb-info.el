;;; sb-info --- Speedbar support for Info

;; Copyright (C) 1997 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-info.el,v 1.1 1997/11/01 13:50:25 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide Info specific support,
;; showing links and addresses in the side-bar.
;;
;;   To provide special service to all the modes supported by this file,
;; put the following in your .emacs file.
;;
;; (require 'speedbspec)
;;
;;   This will load in the known functions, and the mode-enabling code
;; into 'change-major-mode-hook.
;;
;;   This file requires speedbar.

;;; Change log:
;; 0.1 - first revision copied from speedbspec.el V 0.1.1

(require 'speedbspec)

;;; Code:
(defvar Info-last-speedbar-node nil
  "Last node viewed with speedbar in the form '(NODE FILE).")

(defvar Info-speedbar-menu-items
  '(["Browse Item On Line" speedbar-edit-line t])
  "Additional menu-items to add to speedbar frame.")

(defun Info-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for."
  (goto-char (point-min))
  (if (and (looking-at "<Directory>")
	   (save-excursion
	     (set-buffer buffer)
	     (and (equal (car Info-last-speedbar-node) Info-current-node)
		  (equal (cdr Info-last-speedbar-node) Info-current-file))))
      nil
    (erase-buffer)
    (speedbar-insert-button "<Directory>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-directory)
    (speedbar-insert-button "<Top>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-top-node)
    (speedbar-insert-button "<Last>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-last)
    (speedbar-insert-button "<Up>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-up)
    (speedbar-insert-button "<Next>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-next)
    (speedbar-insert-button "<Prev>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-prev)
    (let ((completions nil))
      (save-excursion
	(set-buffer buffer)
	(setq Info-last-speedbar-node
	      (cons Info-current-node Info-current-file))
	(goto-char (point-min))
	;; Always skip the first one...
	(re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	  (setq completions (cons (buffer-substring (match-beginning 1)
						    (match-end 1))
				  completions))))
      (setq completions (nreverse completions))
      (while completions
	(speedbar-make-tag-line nil nil nil nil
				(car completions) 'Info-speedbar-menu
				nil 'info-node 0)
	(setq completions (cdr completions))))))

(defun Info-speedbar-button (text token indent)
  "Called when user clicks <Directory> from speedbar.
TEXT, TOKEN, and INDENT are unused."
  (speedbar-with-attached-buffer
   (funcall token)
   (setq Info-last-speedbar-node nil)
   (speedbar-update-contents)))

(defun Info-speedbar-menu (text token indent)
  "Goto the menu node specified in TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (Info-menu text)
   (setq Info-last-speedbar-node nil)
   (speedbar-update-contents)))

(provide 'sb-info)
;;; sb-info.el ends here
