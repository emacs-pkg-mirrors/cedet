;;; semantic-util-modes.el --- Semantic minor modes

;;; Copyright (C) 2001 Eric M. Ludlam
;;; Copyright (C) 2001 David Ponce

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Author: David Ponce <david@dponce.com>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util-modes.el,v 1.4 2001/10/24 01:11:49 zappo Exp $

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
;; Utility minor modes of the Semantic Bovinator.
;;

;;; History:
;; 

;;; Code:
(require 'semantic-util)

;;;;
;;;; Registry
;;;;

(if (fboundp 'add-minor-mode)

    ;; Emacs 21 & XEmacs
    (defalias 'semantic-add-minor-mode 'add-minor-mode)

  ;; Emacs 20
  (defun semantic-add-minor-mode (toggle name &optional keymap)
    "Register a new Semantic minor mode.
TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.
It is also an interactive function to toggle the mode.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added
to `minor-mode-map-alist'."
  (or (assq toggle minor-mode-alist)
      (setq minor-mode-alist (cons (list toggle name)
                                   minor-mode-alist)))
    
  (or (not keymap)
      (assq toggle minor-mode-map-alist)
      (setq minor-mode-map-alist (cons (cons toggle keymap)
                                       minor-mode-map-alist))))
    
  )

(defun semantic-toggle-minor-mode-globally (mode &optional arg)
  "Toggle minor mode MODE in every Semantic enabled buffer.
Return non-nil if MODE is turned on in every Semantic enabled buffer.
If ARG is positive, enable, if it is negative, disable.  If ARG is
nil, then toggle.  Otherwise do nothing.  MODE must be a valid minor
mode defined in `minor-mode-alist' and must be too an interactive
function used to toggle the mode."
  (or (and (fboundp mode) (assq mode minor-mode-alist))
      (error "Semantic minor mode %s not found" mode))
  (if (not arg)
      (if (memq mode semantic-init-hooks)
	  (setq arg -1)
	(setq arg 1)))
  ;; Add or remove the MODE toggle function from
  ;; `semantic-init-hooks'.  Then turn MODE on or off in every
  ;; Semantic enabled buffer.
  (cond
   ;; Turn off if ARG < 0
   ((< arg 0)
    (remove-hook 'semantic-init-hooks mode)
    (semantic-map-buffers #'(lambda () (funcall mode -1)))
    nil)
   ;; Turn on if ARG > 0
   ((> arg 0)
    (add-hook 'semantic-init-hooks mode)
    (semantic-map-buffers #'(lambda () (funcall mode 1)))
    t)
   ;; Otherwise just check MODE state
   (t
    (memq mode semantic-init-hooks))
   ))


;;;;
;;;; Minor mode to show modified (dirty) tokens
;;;;

;;;###autoload
(defun global-semantic-show-dirty-mode (&optional arg)
  "Toggle global use of `semantic-show-dirty' mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-dirty-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-dirty-mode arg)))

;;;###autoload
(defcustom global-semantic-show-dirty-mode nil
  "*If non-nil enable global use of show-dirty mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-dirty-mode (if val 1 -1))))

(defcustom semantic-show-dirty-mode-hook nil
  "Hook run at the end of function `semantic-show-dirty-mode'."
  :group 'semantic
  :type 'hook)

(defcustom semantic-show-dirty-mode-on-hook nil
  "Hook run when show-dirty minor mode is turned on."
  :group 'semantic
  :type 'hook)
  
(defcustom semantic-show-dirty-mode-off-hook nil
  "Hook run when show-dirty minor mode is turned off."
  :group 'semantic
  :type 'hook)

(defface semantic-dirty-token-face
  '((((class color) (background dark))
     (:background "gray10"))
    (((class color) (background light))
     (:background "gray90")))
  "*Face used to show dirty tokens in `semantic-show-dirty-token-mode'."
  :group 'semantic)

(defun semantic-show-dirty-token-hook-fcn (token start end)
  "Function set into `semantic-dirty-token-hooks'.
This will highlight TOKEN as dirty.
START and END define the region changed, but are not used."
  (semantic-highlight-token token 'semantic-dirty-token-face))

(defun semantic-show-clean-token-hook-fcn (token)
  "Function set into `semantic-clean-token-hooks'.
This will unhighlight TOKEN from being dirty."
  (semantic-unhighlight-token token))

(defvar semantic-show-dirty-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for show-dirty minor mode.")

(defvar semantic-show-dirty-mode nil
  "Non-nil if show-dirty minor mode is enabled.
Use the command `semantic-show-dirty-mode' to change this variable.")
(make-variable-buffer-local 'semantic-show-dirty-mode)

(defun semantic-show-dirty-mode-setup ()
  "Setup `semantic-show-dirty-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-show-dirty-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-dirty-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        (make-local-hook 'semantic-dirty-token-hooks)
        (make-local-hook 'semantic-clean-token-hooks)
        (make-local-hook 'after-save-hook)
        (add-hook 'semantic-dirty-token-hooks
                  'semantic-show-dirty-token-hook-fcn nil t)
        (add-hook 'semantic-clean-token-hooks
                  'semantic-show-clean-token-hook-fcn nil t)
        (add-hook 'after-save-hook
                  'semantic-rebovinate-quickly-hook nil t)
        )
    ;; Remove hooks
    (remove-hook 'semantic-dirty-token-hooks
                 'semantic-show-dirty-token-hook-fcn t)
    (remove-hook 'semantic-clean-token-hooks
                 'semantic-show-clean-token-hook-fcn t)
    (remove-hook 'after-save-hook
                 'semantic-rebovinate-quickly-hook t))
  semantic-show-dirty-mode)

;;;###autoload
(defun semantic-show-dirty-mode (&optional arg)
  "Minor mode for highlighting dirty tokens.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-dirty-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-show-dirty-mode 0 1))))
  (setq semantic-show-dirty-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-show-dirty-mode)))
  (semantic-show-dirty-mode-setup)
  (run-hooks 'semantic-show-dirty-mode-hook
             (if semantic-show-dirty-mode
                 'semantic-show-dirty-mode-on-hook
               'semantic-show-dirty-mode-off-hook))
  (if (interactive-p)
      (message "show-dirty minor mode %sabled"
               (if semantic-show-dirty-mode "en" "dis")))
;;  (force-mode-line-update)
  semantic-show-dirty-mode)

(semantic-add-minor-mode 'semantic-show-dirty-mode
                         ""
                         semantic-show-dirty-mode-map)


;;;;
;;;; Minor mode to show unmatched-syntax elements
;;;;

;;;###autoload
(defun global-semantic-show-unmatched-syntax-mode (&optional arg)
  "Toggle global use of `semantic-show-unmatched-syntax-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-unmatched-syntax-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-unmatched-syntax-mode arg)))

;;;###autoload
(defcustom global-semantic-show-unmatched-syntax-mode nil
  "*If non-nil enable global use of show-unmatched-syntax mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-unmatched-syntax-mode (if val 1 -1))))

(defcustom semantic-show-unmatched-syntax-mode-hook nil
  "Hook run at the end of function `semantic-show-unmatched-syntax-mode'."
  :group 'semantic
  :type 'hook)

(defcustom semantic-show-unmatched-syntax-mode-on-hook nil
  "Hook called when show-unmatched-syntax minor mode is turned on."
  :group 'semantic
  :type 'hook)
  
(defcustom semantic-show-unmatched-syntax-mode-off-hook nil
  "Hook called when show-unmatched-syntax minor mode is turned off."
  :group 'semantic
  :type 'hook)

(defface semantic-unmatched-syntax-face
  '((((class color) (background dark))
     (:underline "red"))
    (((class color) (background light))
     (:underline "red")))
  "Face used to show unmatched-syntax in.
The face is used in  `semantic-show-unmatched-syntax-mode'."
  :group 'semantic)

(defun semantic-showing-unmatched-syntax-p ()
  "Return non-nil if any highlighted unmatched-syntax elements exits.
That is any unmatched-syntax overlay is found in current buffer."
  (let ((ol (semantic-overlay-lists))
        found)
    (setq ol (nconc (car ol) (cdr ol)))
    (while (and ol (not found))
      (setq found (eq (semantic-overlay-get (car ol) 'semantic)
                      'unmatched)
            ol    (cdr ol)))
    found))

(defun semantic-show-unmatched-syntax (syntax)
  "Function set into `semantic-unmatched-syntax-hooks'.
This will highlight elements in SYNTAX as unmatched-syntax."
  ;; This is called during parsing.  Highlight the unmatched syntax,
  ;; and then add a semantic property to that overlay so we can add it
  ;; to the official list of semantic supported overlays.  This gets
  ;; it cleaned up for errors, buffer cleaning, and the like.
  (while syntax
    (let ((o (semantic-make-overlay (semantic-flex-start (car syntax))
				    (semantic-flex-end (car syntax)))))
      (semantic-overlay-put o 'semantic 'unmatched)
      (semantic-overlay-put o 'face 'semantic-unmatched-syntax-face)
      (semantic-overlay-stack-add o)
      )
    (setq syntax (cdr syntax))))

(defun semantic-next-unmatched-syntax (point &optional bound)
  "Find the next overlay for unmatched syntax after POINT.
Do not search past BOUND if non-nil."
  (save-excursion
    (goto-char point)
    (let ((os point) (ol nil))
      (while (and os (< os (or bound (point-max))) (not ol))
	(setq os (semantic-overlay-next-change os))
	(when os
	  ;; Get overlays at position
	  (setq ol (semantic-overlays-at os))
	  ;; find the overlay that belongs to semantic
	  ;; and starts at the found position.
	  (while (and ol (listp ol))
	    (if (and (semantic-overlay-get (car ol) 'semantic)
		     (eq (semantic-overlay-get (car ol) 'semantic) 'unmatched)
		     (= (semantic-overlay-start (car ol)) os))
		(setq ol (car ol)))
	    (when (listp ol) (setq ol (cdr ol))))))
      ol)))

(defun semantic-clean-unmatched-syntax-in-region (beg end)
  "Remove all unmatched syntax overlays between BEG and END."
  (let ((ol (semantic-overlays-in beg end)))
    (while ol
      (if (equal (semantic-overlay-get (car ol) 'semantic) 'unmatched)
	  (semantic-overlay-delete (car ol)))
      (setq ol (cdr ol)))))

(defun semantic-clean-token-of-unmatched-syntax (token)
  "Clean the area covered by TOKEN of unmatched syntax markers."
  (semantic-clean-unmatched-syntax-in-region
   (semantic-token-start token) (semantic-token-end token)))

(defun semantic-hide-unmatched-syntax ()
  "Un-highlight unmatched-syntax elements.
That is delete unmatched-syntax overlays found in current buffer."
  (let ((ol (semantic-overlay-lists))
        o)
    (setq ol (nconc (car ol) (cdr ol)))
    (while ol
      (setq o  (car ol)
            ol (cdr ol))
      (if (eq (semantic-overlay-get o 'semantic) 'unmatched)
          (semantic-overlay-delete o)))))

(defvar semantic-show-unmatched-syntax-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-c,`" 'semantic-show-unmatched-syntax-next)
    km)
  "Keymap for command `semantic-show-unmatched-syntax-mode'.")

(defvar semantic-show-unmatched-syntax-mode nil
  "Non-nil if show-unmatched-syntax minor mode is enabled.
Use the command `semantic-show-unmatched-syntax-mode' to change this
variable.")
(make-variable-buffer-local 'semantic-show-unmatched-syntax-mode)

(defun semantic-show-unmatched-syntax-mode-setup ()
  "Setup the `semantic-show-unmatched-syntax' minor mode.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-show-unmatched-syntax-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-unmatched-syntax-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
        (make-local-hook 'semantic-unmatched-syntax-hook)
        (add-hook 'semantic-unmatched-syntax-hook
                  'semantic-show-unmatched-syntax nil t)
	(make-local-hook 'semantic-pre-clean-token-hooks)
	(add-hook 'semantic-pre-clean-token-hooks
		  'semantic-clean-token-of-unmatched-syntax nil t)
        ;; Parse the current buffer if needed
        (or (semantic-showing-unmatched-syntax-p)
            (condition-case nil
                (progn
                  (semantic-clear-toplevel-cache)
                  (semantic-bovinate-toplevel))
              (quit
               (message "\
semantic-show-unmatched-syntax-mode: parsing of buffer canceled.")))
            ))
    ;; Remove hooks
    (remove-hook 'semantic-unmatched-syntax-hook
                 'semantic-show-unmatched-syntax t)
    (remove-hook 'semantic-pre-clean-token-hooks
		 'semantic-clean-token-of-unmatched-syntax t)
    ;; Cleanup unmatched-syntax highlighting
    (semantic-hide-unmatched-syntax))
  semantic-show-unmatched-syntax-mode)
  
;;;###autoload
(defun semantic-show-unmatched-syntax-mode (&optional arg)
  "Minor mode to highlight unmatched-syntax tokens.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-show-unmatched-syntax-mode 0 1))))
  (setq semantic-show-unmatched-syntax-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-show-unmatched-syntax-mode)))
  (semantic-show-unmatched-syntax-mode-setup)
  (run-hooks 'semantic-show-unmatched-syntax-mode-hook
             (if semantic-show-unmatched-syntax-mode
                 'semantic-show-unmatched-syntax-mode-on-hook
               'semantic-show-unmatched-syntax-mode-off-hook))
  (if (interactive-p)
      (message "show-unmatched-syntax minor mode %sabled"
               (if semantic-show-unmatched-syntax-mode "en" "dis")))
;;  (force-mode-line-update)
  semantic-show-unmatched-syntax-mode)

(semantic-add-minor-mode 'semantic-show-unmatched-syntax-mode
                         ""
                         semantic-show-unmatched-syntax-mode-map)

(defun semantic-show-unmatched-syntax-next ()
  "Move forward to the next occurance of unmatched syntax."
  (interactive)
  (let ((o (semantic-next-unmatched-syntax (point))))
    (if o
	(goto-char (semantic-overlay-start o)))))

(provide 'semantic-util-modes)

;;; semantic-util-modes.el ends here
