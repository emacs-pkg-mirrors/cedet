;;; semantic-decorate.el --- Utilities for decorating/highlighting tokens.

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-decorate.el,v 1.1 2003/07/03 00:56:22 zappo Exp $

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
;; Text representing a semantic tag is wrapped in an overlay.
;; This overlay can be used for highlighting, or setting other
;; editing properties on a tag, such as "read only."
;;

(require 'semantic)

;;; Code:
;;;###autoload
(defun semantic-highlight-tag (tag &optional face)
  "Specify that TAG should be highlighted.
Optional FACE specifies the face to use."
  (let ((o (semantic-tag-overlay tag)))
    (semantic-overlay-put o 'old-face
			  (cons (semantic-overlay-get o 'face)
				(semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'face (or face 'highlight))
    ))

;;;###autoload
(defun semantic-unhighlight-tag (tag)
  "Unhighlight TAG, restoring it's previous face."
  (let ((o (semantic-tag-overlay tag)))
    (semantic-overlay-put o 'face (car (semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'old-face (cdr (semantic-overlay-get o 'old-face)))
    ))

(defun semantic-momentary-unhighlight-tag (tag)
  "Unhighlight TAG, restoring it's previous face."
  (semantic-unhighlight-tag tag)
  (remove-hook 'pre-command-hook
	       `(lambda () (semantic-momentary-unhighlight-tag ',tag))))

;;;###autoload
(defun semantic-momentary-highlight-tag (tag &optional face)
  "Highlight TAG, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used."
  (semantic-highlight-tag tag face)
  (add-hook 'pre-command-hook
	    `(lambda () (semantic-momentary-unhighlight-tag ',tag))))

;;;###autoload
(defun semantic-set-tag-face (tag face)
  "Specify that TAG should use FACE for display."
  (semantic-overlay-put (semantic-tag-overlay tag) 'face face))

;;;###autoload
(defun semantic-set-tag-invisible (tag &optional visible)
  "Enable the text in TAG to be made invisible.
If VISIBLE is non-nil, make the text visible."
  (semantic-overlay-put (semantic-tag-overlay tag) 'invisible
			(not visible)))

;;;###autoload
(defun semantic-tag-invisible-p (tag)
  "Return non-nil if TAG is invisible."
  (semantic-overlay-get (semantic-tag-overlay tag) 'invisible))

;;;###autoload
(defun semantic-set-tag-intangible (tag &optional tangible)
  "Enable the text in TAG to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-put (semantic-tag-overlay tag) 'intangible
			(not tangible)))

;;;###autoload
(defun semantic-tag-intangible-p (tag)
  "Return non-nil if TAG is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-get (semantic-tag-overlay tag) 'intangible))

(defun semantic-overlay-signal-read-only
  (overlay after start end &optional len)
  "Hook used in modification hooks to prevent modification.
Allows deletion of the entire text.
Argument OVERLAY, AFTER, START, END, and LEN are passed in by the system."
  ;; Stolen blithly from cpp.el in Emacs 21.1
  (if (and (not after)
	   (or (< (semantic-overlay-start overlay) start)
	       (> (semantic-overlay-end overlay) end)))
      (error "This text is read only")))

;;;###autoload
(defun semantic-set-tag-read-only (tag &optional writable)
  "Enable the text in TAG to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable.
instead of read-only."
  (let ((o (semantic-tag-overlay tag))
	(hook (if writable nil '(semantic-overlay-signal-read-only))))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-put o 'read-only (not writable))
      (semantic-overlay-put o 'modification-hooks hook)
      (semantic-overlay-put o 'insert-in-front-hooks hook)
      (semantic-overlay-put o 'insert-behind-hooks hook))))

;;;###autoload
(defun semantic-tag-read-only-p (tag)
  "Return non-nil if the current TAG is marked read only."
  (let ((o (semantic-tag-overlay tag)))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-get o 'read-only)
      (member 'semantic-overlay-signal-read-only
              (semantic-overlay-get o 'modification-hooks)))))

;;; backwards compatability

(semantic-alias-obsolete 'semantic-highlight-token
			 'semantic-highlight-tag)
(semantic-alias-obsolete 'semantic-unhighlight-token
			 'semantic-unhighlight-tag)
(semantic-alias-obsolete 'semantic-momentary-unhighlight-token
			 'semantic-momentary-unhighlight-tag)
(semantic-alias-obsolete 'semantic-momentary-highlight-token
			 'semantic-momentary-highlight-tag)
(semantic-alias-obsolete 'semantic-set-token-face
			 'semantic-set-tag-face)
(semantic-alias-obsolete 'semantic-set-token-invisible
			 'semantic-set-tag-invisible)
(semantic-alias-obsolete 'semantic-token-invisible-p
			 'semantic-tag-invisible-p)
(semantic-alias-obsolete 'semantic-set-token-intangible
			 'semantic-set-tag-intangible)
(semantic-alias-obsolete 'semantic-token-intangible-p
			 'semantic-tag-intangible-p)
(semantic-alias-obsolete 'semantic-set-token-read-only
			 'semantic-set-tag-read-only)
(semantic-alias-obsolete 'semantic-token-read-only-p
			 'semantic-tag-read-only-p)

(provide 'semantic-decorate)

;;; semantic-decorate.el ends here
