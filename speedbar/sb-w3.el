;;; sb-w3 --- Speedbar support for w3.

;; Copyright (C) 1997, 1998 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-w3.el,v 1.2 1998/03/06 16:28:27 zappo Exp $
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
;; files are displayed.  These functions provide w3 specific support,
;; showing links and addresses in the side-bar.
;;
;;   This file requires speedbar.

;;; Change log:
;; 0.1   - first revision copied from speedbspec.el V 0.1.1
;; 0.1.1 - Removed dependency on speedbspec

;;; Code:
(defvar w3-speedbar-last-buffer nil
  "The last buffer shown by w3-speedbar.")

(defun w3-speedbar-buttons (buffer)
  "Create speedbar buttons for the current web BUFFER displayed in w3 mode."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "History:") (equal w3-speedbar-last-buffer buffer))
	nil
      (setq w3-speedbar-last-buffer buffer)
      (erase-buffer)
      (let ((links (save-excursion (set-buffer buffer) (w3-only-links)))
	    (part nil))
	(insert "History:\n")
	;; This taken out of w3 which was used to create the history list,
	;; and is here modified to create the speedbar buttons
	(cl-maphash
	 (function
	  (lambda (url desc)
	    (speedbar-insert-button (w3-speedbar-shorten-button url)
				    'speedbar-directory-face 'highlight
				    'w3-speedbar-link url)))
	 url-history-list)
	(insert "Links:\n")
	(while links
	  (setq part (car (cdr (member 'href (car links))))
		links (cdr links))
	  (speedbar-insert-button (w3-speedbar-shorten-button part)
				  'speedbar-file-face 'highlight
				  'w3-speedbar-link part))))))
    
(defun w3-speedbar-shorten-button (button)
  "Takes text BUTTON and shortens it as much as possible."
  ;; I should make this more complex, but I'm not sure how...
  (let ((fnnd (file-name-nondirectory button)))
    (if (< 0 (length fnnd))
	fnnd
      (if (string-match "\\(ht\\|f\\)tp://" button)
	  (setq button (substring button (match-end 0))))
      (if (string-match "/$" button)
	  (setq button (substring button 0 (match-beginning 0))))
      button)))

(defun w3-speedbar-link (text token indent)
  "Follow link described by TEXT which has the URL TOKEN.
INDENT is not used."
  (speedbar-with-attached-buffer (w3-fetch token)))


(provide 'sb-w3)
;;; sb-w3.el ends here
