;;; sb-info --- Speedbar support for Info

;; Copyright (C) 1997, 1998 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-info.el,v 1.5 1998/05/07 17:18:55 zappo Exp $
;;
;; This file is patch of GNU Emacs.
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
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide Info specific support,
;; showing links and addresses in the side-bar.
;;
;;   To enable in emacs 20.2 or earlier, add this to your .emacs file.
;;   (autoload 'Info-speedbar-buttons "sb-info"
;;             "Info specific speedbar button generator.")
;;
;;   This file requires speedbar and Info.

;;; Change log:
;; 0.1   - first revision copied from speedbspec.el V 0.1.1
;; 0.1.1 - No longer require speedbspec
;; 0.2   - Added a speedbar major mode for displaying Info nodes.

(require 'speedbar)
(require 'info)

;;; Code:

(defvar info-speedbar-menu-items
  '()
  "Easymenu style list of menu items when in info brows mode.")

;;; Info hierarchy display method
(defun Info-speedbar-browser ()
  "Initialize speedbar to display an info node browser.
This will add a speedbar major display mode."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Make sure our special speedbar major mode is loaded
  (speedbar-add-expansion-list '("Info" info-speedbar-menu-items
				 nil Info-speedbar-hierarchy-buttons))
  ;; Now, throw us into RPM mode on speedbar.
  (speedbar-change-initial-expansion-list "Info")
  )

(defun Info-speedbar-hierarchy-buttons (directory depth &optional node)
  "Display an Info directory hierarchy in speedbar.
DIRECTORY is the current directory in the attached frame.
DEPTH is the current indentation depth.
NODE is an optional argument that is used to represent the
specific node to expand."
  ;; We cannot use the generic list code, that depends on all leaves
  ;; being known at creation time.
  (if (not node)
      (speedbar-with-writable (insert "Info Nodes:\n")))
  (let ((completions (Info-speedbar-fetch-file-nodes
		      (or node '"(dir)top"))))
    (if completions
	(speedbar-with-writable
	  (while completions
	    (speedbar-make-tag-line 'bracket ?+ 'Info-speedbar-expand-node
				    (cdr (car completions))
				    (car (car completions))
				    'Info-speedbar-goto-node
				    (cdr (car completions))
				    'info-xref depth)
	    (setq completions (cdr completions)))
	  t)
      nil)))

(defun Info-speedbar-goto-node (text node indent)
  "When user clicks on TEXT, goto an info NODE.
The INDENT level is ignored."
  (select-frame speedbar-attached-frame)
  (let* ((buff (or (get-buffer "*info*")
		   (info)))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if speedbar-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(select-frame speedbar-attached-frame)
	(switch-to-buffer buff)))
    (let ((junk (string-match "^(\\([^)]+\\))\\([^.]+\\)$" node))
	  (file (match-string 1 node))
	  (node (match-string 2 node)))
      (Info-find-node file node))))

(defun Info-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node (NAME . FILE).
INDENT is the current indentation depth."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (Info-speedbar-hierarchy-buttons nil (1+ indent) token)))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

(defun Info-speedbar-fetch-file-nodes (nodespec)
  "Fetch the subnodes from the info NODESPEC.
NODESPEC is a string of the form: (file)node.
Optional THISFILE represends the filename of"
  (save-excursion
    ;; Set up a buffer we can use to fake-out Info.
    (set-buffer (get-buffer-create "*info-browse-tmp*"))
    (if (not (equal major-mode 'Info-mode))
	(Info-mode))
    ;; Get the node into this buffer
    (let ((junk (string-match "^(\\([^)]+\\))\\([^.]+\\)$" nodespec))
	  (file (match-string 1 nodespec))
	  (node (match-string 2 nodespec)))
      (Info-find-node file node))
    ;; Scan the created buffer
    (goto-char (point-min))
    (let ((completions nil)
	  (thisfile (progn (string-match "^(\\([^)]+\\))" nodespec)
			   (match-string 1 nodespec))))
      ;; Always skip the first one...
      (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
      (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(let ((name (match-string 1)))
	  (if (looking-at " *\\(([^)]+)[^.\n]+\\)\\.")
	      (setq name (cons name (match-string 1)))
	    (if (looking-at " *\\(([^)]+)\\)\\.")
		(setq name (cons name (concat (match-string 1) "Top")))
	      (setq name (cons name (concat "(" thisfile ")" name)))))
	  (setq completions (cons name completions))))
      (nreverse completions))))

;;; Info mode node listing
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
