;;; semantic-util-modes.el --- Semantic minor modes

;;; Copyright (C) 2000, 2001, 2003 Eric M. Ludlam
;;; Copyright (C) 2001 David Ponce

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Author: David Ponce <david@dponce.com>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util-modes.el,v 1.17.4.3 2003/04/11 07:53:24 berndl Exp $

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
(require 'working)

;;; Compatibility
(if (fboundp 'propertize)
    (defalias 'semantic-propertize 'propertize)
  (defsubst semantic-propertize (string &rest properties)
    "Return a copy of STRING with text properties added.
Dummy implementation for compatibility which just return STRING and
ignore PROPERTIES."
    string)
  )

;;;;
;;;; Semantic minor modes stuff
;;;;
(defcustom semantic-update-mode-line t
  "*If non-nil, show enabled minor modes in the mode line.
Only minor modes that are not turned on globally are shown in the mode
line."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         ;; Update status of all Semantic enabled buffers
         (semantic-map-buffers
          #'semantic-mode-line-update)))

(defcustom semantic-mode-line-prefix
  (semantic-propertize "S" 'face 'bold)
  "*Prefix added to minor mode indicators in the mode line."
  :group 'semantic
  :type 'string
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default)

(defvar semantic-minor-modes-status nil
  "String showing Semantic minor modes which are locally enabled.
It is displayed in the mode line.")
(make-variable-buffer-local 'semantic-minor-modes-status)

(defvar semantic-minor-mode-alist nil
  "Alist saying how to show Semantic minor modes in the mode line.
Like variable `minor-mode-alist'.")

(defun semantic-mode-line-update ()
  "Update display of Semantic minor modes in the mode line.
Only minor modes that are locally enabled are shown in the mode line."
  (setq semantic-minor-modes-status nil)
  (if semantic-update-mode-line
      (let ((ml semantic-minor-mode-alist)
            mm ms see)
        (while ml
          (setq mm (car ml)
                ms (cadr mm)
                mm (car mm)
                ml (cdr ml))
          (when (and (symbol-value mm)
                     ;; Only show local minor mode status
                     (not (memq mm semantic-init-hooks)))
            (and ms
                 (symbolp ms)
                 (setq ms (symbol-value ms)))
            (and (stringp ms)
                 (not (member ms see)) ;; Don't duplicate same status
                 (setq see (cons ms see)
                       ms (if (string-match "^[ ]*\\(.+\\)" ms)
                              (match-string 1 ms)))
                 (setq semantic-minor-modes-status
                       (if semantic-minor-modes-status
                           (concat semantic-minor-modes-status "/" ms)
                         ms)))))
        (if semantic-minor-modes-status
            (setq semantic-minor-modes-status
                  (concat
                   " "
                   (if (string-match "^[ ]*\\(.+\\)"
                                     semantic-mode-line-prefix)
                       (match-string 1 semantic-mode-line-prefix)
                     "S")
                   "/"
                   semantic-minor-modes-status)))))
  (working-mode-line-update))

(defun semantic-add-minor-mode (toggle name &optional keymap)
  "Register a new Semantic minor mode.
TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.
It is also an interactive function to toggle the mode.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added to
`minor-mode-map-alist'."
  ;; Add a dymmy semantic minor mode to display the status
  (or (assq 'semantic-minor-modes-status minor-mode-alist)
      (setq minor-mode-alist (cons (list 'semantic-minor-modes-status
                                         'semantic-minor-modes-status)
                                   minor-mode-alist)))
  (if (fboundp 'add-minor-mode)
      ;; Emacs 21 & XEmacs
      (add-minor-mode toggle "" keymap)
    ;; Emacs 20
    (or (assq toggle minor-mode-alist)
        (setq minor-mode-alist (cons (list toggle "") minor-mode-alist)))
    (or (not keymap)
        (assq toggle minor-mode-map-alist)
        (setq minor-mode-map-alist (cons (cons toggle keymap)
                                         minor-mode-map-alist))))
  ;; Record how to display this minor mode in the mode line
  (let ((mm (assq toggle semantic-minor-mode-alist)))
    (if mm
        (setcdr mm (list name))
      (setq semantic-minor-mode-alist (cons (list toggle name)
                                       semantic-minor-mode-alist)))))

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
  "Toggle global use of option `semantic-show-dirty-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-dirty-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-dirty-mode arg)))

;;;###autoload
(defcustom global-semantic-show-dirty-mode nil
  "*If non-nil, enable global use of show-dirty mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-dirty-mode (if val 1 -1))))

(defcustom semantic-show-dirty-mode-hook nil
  "*Hook run at the end of function `semantic-show-dirty-mode'."
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
  "Setup option `semantic-show-dirty-mode'.
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
        (semantic-make-local-hook 'semantic-dirty-token-hooks)
        (semantic-make-local-hook 'semantic-clean-token-hooks)
        (semantic-make-local-hook 'after-save-hook)
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
  (run-hooks 'semantic-show-dirty-mode-hook)
  (if (interactive-p)
      (message "show-dirty minor mode %sabled"
               (if semantic-show-dirty-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-show-dirty-mode)

(semantic-add-minor-mode 'semantic-show-dirty-mode
                         "d"
                         semantic-show-dirty-mode-map)


;;;;
;;;; Minor mode to show unmatched-syntax elements
;;;;

;;;###autoload
(defun global-semantic-show-unmatched-syntax-mode (&optional arg)
  "Toggle global use of option `semantic-show-unmatched-syntax-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-unmatched-syntax-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-unmatched-syntax-mode arg)))

;;;###autoload
(defcustom global-semantic-show-unmatched-syntax-mode nil
  "*If non-nil, enable global use of show-unmatched-syntax mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-unmatched-syntax-mode (if val 1 -1))))

(defcustom semantic-show-unmatched-syntax-mode-hook nil
  "*Hook run at the end of function `semantic-show-unmatched-syntax-mode'."
  :group 'semantic
  :type 'hook)

(defface semantic-unmatched-syntax-face
  '((((class color) (background dark))
     (:underline "red"))
    (((class color) (background light))
     (:underline "red")))
  "*Face used to show unmatched syntax in.
The face is used in  `semantic-show-unmatched-syntax-mode'."
  :group 'semantic)

(defsubst semantic-unmatched-syntax-overlay-p (overlay)
  "Return non-nil if OVERLAY is an unmatched syntax one."
  (eq (semantic-overlay-get overlay 'semantic) 'unmatched))

(defun semantic-showing-unmatched-syntax-p ()
  "Return non-nil if an unmatched syntax overlay was found in buffer."
  (let ((ol (semantic-overlays-in (point-min) (point-max)))
        found)
    (while (and ol (not found))
      (setq found (semantic-unmatched-syntax-overlay-p (car ol))
            ol    (cdr ol)))
    found))

(defun semantic-clean-unmatched-syntax-in-region (beg end)
  "Remove all unmatched syntax overlays between BEG and END."
  (let ((ol (semantic-overlays-in beg end)))
    (while ol
      (if (semantic-unmatched-syntax-overlay-p (car ol))
	  (semantic-overlay-delete (car ol)))
      (setq ol (cdr ol)))))

(defsubst semantic-clean-unmatched-syntax-in-buffer ()
  "Remove all unmatched syntax overlays found in current buffer."
  (semantic-clean-unmatched-syntax-in-region
   (point-min) (point-max)))

(defsubst semantic-clean-token-of-unmatched-syntax (token)
  "Clean the area covered by TOKEN of unmatched syntax markers."
  (semantic-clean-unmatched-syntax-in-region
   (semantic-token-start token) (semantic-token-end token)))

(defun semantic-show-unmatched-syntax (syntax)
  "Function set into `semantic-unmatched-syntax-hook'.
This will highlight elements in SYNTAX as unmatched syntax."
  ;; This is called when `semantic-show-unmatched-syntax-mode' is
  ;; enabled.  Highlight the unmatched syntax, and then add a semantic
  ;; property to that overlay so we can add it to the official list of
  ;; semantic supported overlays.  This gets it cleaned up for errors,
  ;; buffer cleaning, and the like.
  (semantic-clean-unmatched-syntax-in-buffer) ;Clear previous highlighting
  (if syntax
      (let (o)
        (while syntax
          (setq o (semantic-make-overlay (semantic-flex-start (car syntax))
                                         (semantic-flex-end (car syntax))))
          (semantic-overlay-put o 'semantic 'unmatched)
          (semantic-overlay-put o 'face 'semantic-unmatched-syntax-face)
          (setq syntax (cdr syntax))))
    ))

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
	    (and (semantic-unmatched-syntax-overlay-p (car ol))
                 (setq ol (car ol)))
	    (if (listp ol)
                (setq ol (cdr ol))))))
      ol)))

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
        (semantic-make-local-hook 'semantic-unmatched-syntax-hook)
        (add-hook 'semantic-unmatched-syntax-hook
                  'semantic-show-unmatched-syntax nil t)
	(semantic-make-local-hook 'semantic-pre-clean-token-hooks)
	(add-hook 'semantic-pre-clean-token-hooks
		  'semantic-clean-token-of-unmatched-syntax nil t)
        ;; Show unmatched syntax elements
        (semantic-show-unmatched-syntax
         (semantic-bovinate-unmatched-syntax)))
    ;; Remove hooks
    (remove-hook 'semantic-unmatched-syntax-hook
                 'semantic-show-unmatched-syntax t)
    (remove-hook 'semantic-pre-clean-token-hooks
		 'semantic-clean-token-of-unmatched-syntax t)
    ;; Cleanup unmatched-syntax highlighting
    (semantic-clean-unmatched-syntax-in-buffer))
  semantic-show-unmatched-syntax-mode)
  
;;;###autoload
(defun semantic-show-unmatched-syntax-mode (&optional arg)
  "Minor mode to highlight unmatched syntax tokens.
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
  (run-hooks 'semantic-show-unmatched-syntax-mode-hook)
  (if (interactive-p)
      (message "show-unmatched-syntax minor mode %sabled"
               (if semantic-show-unmatched-syntax-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-show-unmatched-syntax-mode)

(semantic-add-minor-mode 'semantic-show-unmatched-syntax-mode
                         "u"
                         semantic-show-unmatched-syntax-mode-map)

(defun semantic-show-unmatched-syntax-next ()
  "Move forward to the next occurrence of unmatched syntax."
  (interactive)
  (let ((o (semantic-next-unmatched-syntax (point))))
    (if o
	(goto-char (semantic-overlay-start o)))))

;;;;
;;;; Minor mode to auto reparse buffer
;;;;

(require 'timer)

(defvar semantic-auto-parse-timer nil
  "Timer used to schedule automatic reparse.")

(defcustom semantic-auto-parse-no-working-message t
  "*If non-nil, disable display of working messages during parse."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-auto-parse-working-in-modeline-flag nil
  "*Non-nil means show working messages in the mode line.
Typically, parsing will show messages in the minibuffer.
This will move the parse message into the modeline."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-auto-parse-idle-time 4
  "*Time in seconds of idle time before auto-reparse.
This time should be short enough to ensure that auto-parse will be
run as soon as Emacs is idle."
  :group 'semantic
  :type 'number
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp semantic-auto-parse-timer)
           (cancel-timer semantic-auto-parse-timer)
           (setq semantic-auto-parse-timer nil)
           (semantic-auto-parse-setup-timer))))

(defcustom semantic-auto-parse-max-buffer-size 0
  "*Maximum size in bytes of buffers automatically reparsed.
If this value is less than or equal to 0, buffers are automatically
reparsed regardless of their size."
  :group 'semantic
  :type 'number)

;;;###autoload
(defcustom global-semantic-auto-parse-mode nil
  "*If non-nil, enable global use of auto-parse mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-auto-parse-mode (if val 1 -1))))

;;;###autoload
(defun global-semantic-auto-parse-mode (&optional arg)
  "Toggle global use of option `semantic-auto-parse-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-auto-parse-mode
        (semantic-toggle-minor-mode-globally
         'semantic-auto-parse-mode arg)))

(defcustom semantic-auto-parse-mode-hook nil
  "*Hook run at the end of function `semantic-show-dirty-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-auto-parse-mode nil
  "Non-nil if auto-parse minor mode is enabled.
Use the command `semantic-auto-parse-mode' to change this variable.")
(make-variable-buffer-local 'semantic-auto-parse-mode)

(defsubst semantic-auto-parse-enabled-p ()
  "Return non-nil if auto-parse is enabled for this buffer.
See also the variable `semantic-auto-parse-max-buffer-size'."
  (if semantic-auto-parse-mode
      (or (<= semantic-auto-parse-max-buffer-size 0)
          (< (buffer-size) semantic-auto-parse-max-buffer-size))))

(defun semantic-auto-parse-bovinate ()
  "Automatically reparse current buffer.
Called after `semantic-auto-parse-idle-time' seconds of Emacs idle
time.  Does nothing if option `semantic-auto-parse-mode' is not enabled or
current buffer doesn't need reparsing or if its size exceeds the
`semantic-auto-parse-max-buffer-size' threshold."
  (if (semantic-auto-parse-enabled-p)
      (let ((semantic-bovination-working-type nil)
	    (inhibit-quit nil)
	    (working-use-echo-area-p
	     (not semantic-auto-parse-working-in-modeline-flag))
            (working-status-dynamic-type
             (if semantic-auto-parse-no-working-message
                 nil
               working-status-dynamic-type))
	    (working-status-percentage-type
	     (if semantic-auto-parse-no-working-message
		 nil
	       working-status-percentage-type))
	    (semantic-flex-unterminated-syntax-end-function
	     (lambda (syntax start end) (throw 'auto-parse syntax)))
	    )
	(when (catch 'auto-parse
		(save-excursion
		  (semantic-bovinate-toplevel t))
		nil)
	  ;; The reparse failed, no status has been set up that
	  ;; things are really bad.  If auto-parse needs to do
	  ;; something in this case, this is where we do it, otherwise
	  ;; wait for the next timer, and see if the buffer has
	  ;; been fixed up enough to do something useful.
	  ;;(message "Auto-reparse sillyness")
	  nil
	  ))))

(defun semantic-auto-parse-setup-timer ()
  "Lazy initialization of the auto parse idle timer."
  (or (timerp semantic-auto-parse-timer)
      (setq semantic-auto-parse-timer
            (run-with-idle-timer
             semantic-auto-parse-idle-time t
             #'semantic-auto-parse-bovinate))))

(defun semantic-auto-parse-mode-setup ()
  "Setup option `semantic-auto-parse-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-auto-parse-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-auto-parse-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        (semantic-auto-parse-setup-timer)))
  semantic-auto-parse-mode)

;;;###autoload
(defun semantic-auto-parse-mode (&optional arg)
  "Minor mode to auto parse buffer following a change.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-auto-parse-mode 0 1))))
  (setq semantic-auto-parse-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-auto-parse-mode)))
  (semantic-auto-parse-mode-setup)
  (run-hooks 'semantic-auto-parse-mode-hook)
  (if (interactive-p)
      (message "auto-parse minor mode %sabled"
               (if semantic-auto-parse-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-auto-parse-mode)

(semantic-add-minor-mode 'semantic-auto-parse-mode
                         "a"
                         nil)


;;;;
;;;; Minor mode to show useful things about tokens
;;;;

(eval-when-compile (require 'eldoc))

;;;###autoload
(defun global-semantic-summary-mode (&optional arg)
  "Toggle global use of option `semantic-summary-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-summary-mode
        (semantic-toggle-minor-mode-globally
         'semantic-summary-mode arg)))

;;;###autoload
(defcustom global-semantic-summary-mode nil
  "*If non-nil, enable global use of summary mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-summary-mode (if val 1 -1))))

(defcustom semantic-summary-mode-hook nil
  "*Hook run at the end of function `semantic-summary-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-summary-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for summary minor mode.")

(defvar semantic-summary-mode nil
  "Non-nil if summary minor mode is enabled.
Use the command `semantic-summary-mode' to change this variable.")
(make-variable-buffer-local 'semantic-summary-mode)

(defun semantic-summary-mode-setup ()
  "Setup option `semantic-summary-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-summary-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-summary-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Enable eldoc mode
        (eldoc-mode 1)
        )
    ;; Disable eldoc mode
    (eldoc-mode -1))
  semantic-summary-mode)

;;;###autoload
(defun semantic-summary-mode (&optional arg)
  "Minor mode to show useful things about tokens in echo area.
Enables/disables option `eldoc-mode' which supplies the support functions for
this minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-summary-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-summary-mode 0 1))))
  (setq semantic-summary-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-summary-mode)))
  (semantic-summary-mode-setup)
  (run-hooks 'semantic-summary-mode-hook)
  (if (interactive-p)
      (message "Summary minor mode %sabled"
               (if semantic-summary-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-summary-mode)

(semantic-add-minor-mode 'semantic-summary-mode
                         "" ;; Eldoc provides the mode indicator
                         semantic-summary-mode-map)

;;;;
;;;; Minor mode to make function decls sticky.
;;;;

;;;###autoload
(defun global-semantic-stickyfunc-mode (&optional arg)
  "Toggle global use of option `semantic-stickyfunc-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-stickyfunc-mode
        (semantic-toggle-minor-mode-globally
         'semantic-stickyfunc-mode arg)))

;;;###autoload
(defcustom global-semantic-stickyfunc-mode nil
  "*If non-nil, enable global use of stickyfunc mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-stickyfunc-mode (if val 1 -1))))

(defcustom semantic-stickyfunc-mode-hook nil
  "*Hook run at the end of function `semantic-stickyfunc-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-stickyfunc-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for stickyfunc minor mode.")

(defvar semantic-stickyfunc-mode nil
  "Non-nil if stickyfunc minor mode is enabled.
Use the command `semantic-stickyfunc-mode' to change this variable.")
(make-variable-buffer-local 'semantic-stickyfunc-mode)

(defun semantic-stickyfunc-mode-setup ()
  "Setup option `semantic-stickyfunc-mode'.
For semantic enabled buffers, make the function declaration for the top most
function \"sticky\".  This is accomplished by putting the first line of
text for that function in Emacs 21's header line."
  (if semantic-stickyfunc-mode
      (progn
	(if (not (and (featurep 'semantic) (semantic-active-p)))
	    (progn
	      ;; Disable minor mode if semantic stuff not available
	      (setq semantic-stickyfunc-mode nil)
	      (error "Buffer %s was not set up for parsing"
		     (buffer-name))))
	(if (not (boundp 'default-header-line-format))
	    (progn
	      ;; Disable if there are no header lines to use.
	      (setq semantic-stickyfunc-mode nil)
	      (error "Sticky Function mode requires Emacs 21")))
	;; Enable the mode
	(setq header-line-format
	      '("    "
		(:eval (semantic-stickyfunc-fetch-stickyline))))
        )
    ;; Disable sticky func mode
    (setq header-line-format nil))
  semantic-stickyfunc-mode)

;;;###autoload
(defun semantic-stickyfunc-mode (&optional arg)
  "Minor mode to show useful things about tokens in echo area.
Enables/disables making the header line of functions sticky.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-stickyfunc-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-stickyfunc-mode 0 1))))
  (setq semantic-stickyfunc-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-stickyfunc-mode)))
  (semantic-stickyfunc-mode-setup)
  (run-hooks 'semantic-stickyfunc-mode-hook)
  (if (interactive-p)
      (message "Stickyfunc minor mode %sabled"
               (if semantic-stickyfunc-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-stickyfunc-mode)

(defvar semantic-stickyfunc-sticky-classes
  '(function type)
  "List of tag classes which sticky func will display in the header line.")


(defun semantic-stickyfunc-fetch-stickyline ()
  "Make the function at the top of the current window sticky.
Capture it's function declaration, and place it in the header line.
If there is no function, disable the header line."
  (let ((str
	 (save-excursion
	   (goto-char (window-start (selected-window)))
	   (forward-line -1)
	   (end-of-line)
	   ;; Capture this function
	   (let ((tag (semantic-current-nonterminal)))
	     (if (or (not tag)
		     (not (member (semantic-token-token tag)
				  semantic-stickyfunc-sticky-classes)))
		 ;; Set it to be the text under the header line
		 (buffer-substring (point-at-bol) (point-at-eol))
	       ;; Get it
	       (goto-char (semantic-token-start tag))
               ;; Klaus Berndl <klaus.berndl@sdm.de>: 
               ;; goto the tag name; this is especially needed for languages
               ;; like c++ where a often used style is like:
               ;;     void
               ;;     ClassX::methodM(arg1...)
               ;;     {
               ;;       ...
               ;;     }
               ;; Without going to the tag-name we would get"void" in the
               ;; header line which is IMHO not really useful
               (search-forward (semantic-token-name tag) nil t)
               (buffer-substring (point-at-bol) (point-at-eol))
	       ))))
	(start 0))
    (while (string-match "%" str start)
      (setq str (replace-match "%%" t t str 0)
	    start (1+ (match-end 0)))
      )
    str))

(semantic-add-minor-mode 'semantic-stickyfunc-mode
                         "" ;; Don't need indicator.  It's quite visible
                         semantic-stickyfunc-mode-map)

(provide 'semantic-util-modes)

;;; semantic-util-modes.el ends here
