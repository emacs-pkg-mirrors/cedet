;;; semantic-mode.el --- Semantic minor mode

;;; Copyright (C) 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-mode.el,v 1.1 2000/09/22 02:01:57 zappo Exp $

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
;; Semantic-mode is a minor mode which provides services assuming that
;; the current buffer is parsable by the bovinator.
;;
;; One set of services is the ability to do some simple settings on
;; the overlays that define the boundary of the tokens.
;;
;; A second set of services is to provide some simple querying utilities
;; via commands.
;;
;; Lastly, samantic mode will reparse a buffer, by tracking the tokens
;; overlays, so that the buffer must be parsed less often.

(require 'semantic)
(require 'imenu)
(require 'facemenu)

;;; Code:
(defcustom semantic-minor-mode-hooks nil
  "*Hooks run when function `semantic-minor-mode' is run."
  :group 'semantic
  :type 'hook)

(defface semantic-intangible-face '((((class color) (background light))
				     (:foreground "gray25"))
				    (((class color) (background light))
				     (:foreground "gray75")))
  "Face placed on intangible text."
  :group 'semantic-faces)

(defface semantic-read-only-face '((((class color) (background light))
				    (:background "gray25"))
				   (((class color) (background light))
				    (:background "gray75")))
  "Face placed on read-only text."
  :group 'semantic-faces)

(defvar semantic-minor-mode nil
  "Non-nil when a buffer is controlled by Semantic Minor Mode.")
(make-variable-buffer-local 'semantic-minor-mode)

(defvar semantic-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap used in semantic minor mode.")

(if semantic-mode-map
    (easy-menu-define
     semantic-minor-menu semantic-mode-map "Semantic Minor Mode Menu"
     '("Parse"
       [ "Reparse If Needed" semantic-mode-parse t ]
       [ "Force Reparse" semantic-mode-reparse t ]
       "-" ;; Do stuff
       ("Token Properties"
	[ "Hide Token" semantic-mode-make-invisible t ]
	[ "Show Token" semantic-mode-make-visible t ]
	[ "Read Only Token" semantic-mode-toggle-read-only t ]
	[ "Intangible Token" semantic-mode-toggle-intangible t ]
	[ "Set Token Face" semantic-mode-set-face t ]
	[ "Set Token Foreground" semantic-mode-set-foreground t ]
	[ "Set Token Background" semantic-mode-set-background t ]
	[ "Remove all properties" semantic-mode-clear-token t ] )
       ("Imenu Config"
	[ "Bin tokens by type" semantic-imenu-toggle-bucketize-file
	  :active t
	  :style toggle :selected semantic-imenu-bucketize-file ]
	[ "Bins are submenus" semantic-imenu-toggle-buckets-to-submenu
	  :active t
	  :style toggle :selected semantic-imenu-buckets-to-submenu ]
	[ "Bin tokens in type" semantic-imenu-toggle-bucketize-type-parts
	  :active t
	  :style toggle :selected semantic-imenu-bucketize-type-parts ]
	)
       "-" ;; Token menu
       [ "Tokens" imenu t ]
       )))

;; Allow re-insertion of a new keymap
(let ((a (assoc 'semantic-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a semantic-mode-map)
    (checkdoc-add-to-list 'minor-mode-map-alist (cons 'semantic-minor-mode
						      semantic-mode-map))))

;;;###autoload
(defun semantic-minor-mode (&optional arg)
  "Toggle semantic minor mode, a mode for tracking buffer parsing.
With prefix ARG, turn semantic minor mode on iff ARG is positive.

When in Semantic minor mode, the buffer will be parsed, and reparsed
during editing in small chunks, during idle time.

Semantic minor mode also allows modification of text properties on
tokens, highlighting them, or making functions read only.

\\<semantic-mode-map>
\\{semantic-mode-map}"
  (interactive "P")
  (setq semantic-minor-mode
	(not (or (and (null arg) semantic-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (if semantic-minor-mode
      ;; If turned on, do this:
      (progn
	(semantic-mode-parse)
	)
    ;; If turned off, do this:
    )
  (run-hooks 'semantic-minor-mode-hooks))


;;; User Commands
(defun semantic-mode-parse ()
  "Parse the current buffer only if it needs it."
  (interactive)
  (semantic-bovinate-toplevel 0 t t))

(defun semantic-mode-reparse ()
  "Force a reparse of the current buffer."
  (interactive)
  (semantic-clear-toplevel-cache)
  (semantic-bovinate-toplevel 0 t t))

(defun semantic-mode-make-visible (token)
  "Select an invisible TOKEN to be made visible."
  (interactive (list
		;; Query for an invisible token to revisiblize
		"foo"))
  (error "Not implemented yet.")
  )

(defun semantic-mode-make-invisible (&optional token)
  "Make the current TOKEN invisible."
  (interactive)
  (error "This segvs Emacs 21, investigate")
  (semantic-set-token-invisible (or token (semantic-smallest-token))))

(defun semantic-mode-toggle-read-only (&optional token)
  "Toggle the read-only status of the current TOKEN."
  (interactive)
  (let* ((tok (or token (semantic-smallest-token)))
	 (o (semantic-token-overlay tok))
	 (read (member 'semantic-overlay-signal-read-only
		       (semantic-overlay-get o 'modification-hooks))))
    (semantic-set-token-read-only tok read)
    (semantic-mode-set-face
     (if read nil 'semantic-read-only-face))))

(defun semantic-mode-toggle-intangible (&optional token)
  "Toggle the tangibility of the current TOKEN."
  (interactive)
  (let* ((tok (or token (semantic-smallest-token)))
	 (tang (semantic-overlay-get (semantic-token-overlay tok) 'intangible)))
    (semantic-set-token-intangible tok tang)
    (semantic-mode-set-face
     (if tang nil 'semantic-intangible-face))))

(defun semantic-mode-set-face (face &optional token)
  "Set the foreground FACE of the current TOKEN."
  (interactive (list (read-face-name "Face")))
  (semantic-set-token-face (or token (semantic-smallest-token)) face))

(defun semantic-mode-set-foreground (color &optional token)
  "Set the foreground COLOR of the current TOKEN."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Foreground color: ")))
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (semantic-mode-set-face face)))

(defun semantic-mode-set-background (color &optional token)
  "Set the background COLOR of the current TOKEN."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Background color: ")))
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (semantic-mode-set-face face)))

(defun semantic-mode-clear-token (&optional token)
  "Clear all properties from TOKEN."
  (interactive)
  (semantic-set-token-face (or token (semantic-smallest-token)) nil))
  
;;; Utility functions
;;
(defun semantic-invisible-tokens ()
  "Return a list of tokens that are invisible."
  )

(defun semantic-smallest-token (&optional point)
  "Return the smallest token part at POINT."
  (let ((ol (semantic-find-nonterminal-by-overlay (or point (point)))))
    (car ol)))

(defun semantic-mode-toggle-token-property (token prop)
  "For TOKEN, return the opposite boolean value for PROP."
)
    

;;; Dynamic reparse
;;


(provide 'semantic-mode)

;;; semantic-mode.el ends here
