;;; semantic-util-modes.el --- Semantic minor modes

;;; Copyright (C) 2000, 2001, 2002, 2003 Eric M. Ludlam
;;; Copyright (C) 2001 David Ponce

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Author: David Ponce <david@dponce.com>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util-modes.el,v 1.35 2003/08/28 07:44:14 ponced Exp $

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
(require 'working)
(require 'semantic)

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
;;;; Minor mode to highlight areas that a user edits.
;;;;

;;;###autoload
(defun global-semantic-highlight-edits-mode (&optional arg)
  "Toggle global use of option `semantic-highlight-edits-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-highlight-edits-mode
        (semantic-toggle-minor-mode-globally
         'semantic-highlight-edits-mode arg)))

;;;###autoload
(defcustom global-semantic-highlight-edits-mode nil
  "*If non-nil enable global use of variable `semantic-highlight-edits-mode'.
When this mode is enabled, changes made to a buffer are highlighted
until the buffer is reparsed."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-highlight-edits-mode (if val 1 -1))))

(defcustom semantic-highlight-edits-mode-hook nil
  "*Hook run at the end of function `semantic-highlight-edits-mode'."
  :group 'semantic
  :type 'hook)

(defface semantic-highlight-edits-face
  '((((class color) (background dark))
     ;; Put this back to something closer to black later.
     (:background "gray20"))
    (((class color) (background light))
     (:background "gray90")))
  "*Face used to show dirty tokens in `semantic-highlight-edits-mode'."
  :group 'semantic)

(defun semantic-highlight-edits-new-change-hook-fcn (overlay)
  "Function set into `semantic-edits-new-change-hook'.
Argument OVERLAY is the overlay created to mark the change.
This function will set the face property on this overlay."
  (semantic-overlay-put overlay 'face 'semantic-highlight-edits-face))

(defvar semantic-highlight-edits-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for highlight-edits minor mode.")

(defvar semantic-highlight-edits-mode nil
  "Non-nil if highlight-edits minor mode is enabled.
Use the command `semantic-highlight-edits-mode' to change this variable.")
(make-variable-buffer-local 'semantic-highlight-edits-mode)

(defun semantic-highlight-edits-mode-setup ()
  "Setup option `semantic-highlight-edits-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-highlight-edits-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-highlight-edits-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        (semantic-make-local-hook 'semantic-edits-new-change-hooks)
        (add-hook 'semantic-edits-new-change-hooks
                  'semantic-highlight-edits-new-change-hook-fcn nil t)
        )
    ;; Remove hooks
    (remove-hook 'semantic-edits-new-change-hooks
		 'semantic-highlight-edits-new-change-hook-fcn t)
    )
  semantic-highlight-edits-mode)

;;;###autoload
(defun semantic-highlight-edits-mode (&optional arg)
  "Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-highlight-edits-mode 0 1))))
  (setq semantic-highlight-edits-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-highlight-edits-mode)))
  (semantic-highlight-edits-mode-setup)
  (run-hooks 'semantic-highlight-edits-mode-hook)
  (if (interactive-p)
      (message "highlight-edits minor mode %sabled"
               (if semantic-highlight-edits-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-highlight-edits-mode)

(semantic-add-minor-mode 'semantic-highlight-edits-mode
                         "e"
                         semantic-highlight-edits-mode-map)


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
  "*If non-nil, enable global use of `semantic-show-unmatched-syntax-mode'.
When this mode is enabled, syntax in the current buffer which the
semantic parser cannot match is highlighted with a red underline."
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
   (semantic-tag-start token) (semantic-tag-end token)))

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
          (setq o (semantic-make-overlay (semantic-lex-token-start (car syntax))
                                         (semantic-lex-token-end (car syntax))))
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
When a parser executes, some elements in the buffer may not match any
parser rules.  These text characters are considered unmatched syntax.
Often time, the display of unmatched syntax can expose coding
problems before the compiler is run.

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
  "*Hook run at the end of function `semantic-auto-parse-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-auto-parse-mode nil
  "Non-nil if auto-parse minor mode is enabled.
Use the command `semantic-auto-parse-mode' to change this variable.")
(make-variable-buffer-local 'semantic-auto-parse-mode)

(defvar semantic-before-auto-parse-hooks nil
  "Hooks run before option `semantic-auto-parse-mode' begins parsing.")

(defvar semantic-after-auto-parse-hooks nil
  "Hooks run after option `semantic-auto-parse-mode' has parsed.")

(defsubst semantic-auto-parse-enabled-p ()
  "Return non-nil if auto-parse is enabled for this buffer.
See also the variable `semantic-auto-parse-max-buffer-size'."
  (if semantic-auto-parse-mode
      (and (not semantic-debug-enabled)
	   (not semantic-lex-debug)
	   (or (<= semantic-auto-parse-max-buffer-size 0)
	       (< (buffer-size) semantic-auto-parse-max-buffer-size)))))

(defun semantic-auto-parse-bovinate ()
  "Automatically reparse current buffer.
Called after `semantic-auto-parse-idle-time' seconds of Emacs idle
time.  Does nothing if option `semantic-auto-parse-mode' is not enabled or
current buffer doesn't need reparsing or if its size exceeds the
`semantic-auto-parse-max-buffer-size' threshold."
  (when (semantic-auto-parse-enabled-p)
    ;; Disable the auto parse timer while re-parsing
    (semantic-auto-parse-kill-timer)
    (let* ((semantic-bovination-working-type nil)
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
           (semantic-lex-unterminated-syntax-end-function
            (lambda (syntax start end) (throw 'auto-parse syntax)))
	   ;; When flex is deleted, delete this also.
           (semantic-flex-unterminated-syntax-end-function
            (lambda (syntax start end) (throw 'auto-parse syntax)))
           )
      ;; Let people hook into this, but don't let them hose
      ;; us over!
      (condition-case nil
          (run-hooks 'semantic-before-auto-parse-hooks)
        (error nil))

      (unwind-protect
          ;; Perform the parsing.
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
            nil)
        ;; Let people hook into this, but don't let them hose
        ;; us over!
        (condition-case nil
            (run-hooks 'semantic-after-auto-parse-hooks)
          (error nil))))
    ;; Enable again the auto parse timer
    (semantic-auto-parse-setup-timer)
    ;; Return nil.
    nil))

(defun semantic-auto-parse-setup-timer ()
  "Lazy initialization of the auto parse idle timer."
  (or (timerp semantic-auto-parse-timer)
      (setq semantic-auto-parse-timer
            (run-with-idle-timer
             semantic-auto-parse-idle-time t
             #'semantic-auto-parse-bovinate))))

(defun semantic-auto-parse-kill-timer ()
  "Kill the auto parse idle timer."
  (if (timerp semantic-auto-parse-timer)
      (cancel-timer semantic-auto-parse-timer))
  (setq semantic-auto-parse-timer nil))

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
When this mode is off, a buffer is only rescanned for tokens when
some command requests the list of available tokens.  When auto-parse
is enabled, Emacs periodically checks to see if the buffer is out of
date, and reparses while the user is idle (not typing.)

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
;;;; Minor mode to display the parser state in the modeline.
;;;;

;;;###autoload
(defcustom global-semantic-show-parser-state-mode nil
  "*If non-nil enable global use of `semantic-show-parser-state-mode'.
When enabled, the current parse state of the current buffer is displayed
in the mode line. See `semantic-show-parser-state-marker' for details
on what is displayed."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-parser-state-mode (if val 1 -1))))

;;;###autoload
(defun global-semantic-show-parser-state-mode (&optional arg)
  "Toggle global use of option `semantic-show-parser-state-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-parser-state-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-parser-state-mode arg)))

(defcustom semantic-show-parser-state-mode-hook nil
  "*Hook run at the end of function `semantic-show-parser-state-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-show-parser-state-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap for show-parser-state minor mode.")

(defvar semantic-show-parser-state-mode nil
  "Non-nil if show-parser-state minor mode is enabled.
Use the command `semantic-show-parser-state-mode' to change this variable.")
(make-variable-buffer-local 'semantic-show-parser-state-mode)

(defun semantic-show-parser-state-mode-setup ()
  "Setup option `semantic-show-parser-state-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-show-parser-state-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-parser-state-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
	;; Set up mode line

	(when (not
	       (memq 'semantic-show-parser-state-string mode-line-modified))
	  (setq mode-line-modified
		(append mode-line-modified
			'(semantic-show-parser-state-string))))
	;; Add hooks
        (semantic-make-local-hook 'semantic-edits-new-change-hooks)
        (add-hook 'semantic-edits-new-change-hooks
                  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-edits-incremental-reparse-failed-hooks)
	(add-hook 'semantic-edits-incremental-reparse-failed-hooks
		  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
	(add-hook 'semantic-after-partial-cache-change-hook
		  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-before-auto-parse-hooks)
	(add-hook 'semantic-before-auto-parse-hooks
		  'semantic-show-parser-state-auto-marker nil t)
	(semantic-make-local-hook 'semantic-after-auto-parse-hooks)
	(add-hook 'semantic-after-auto-parse-hooks
		  'semantic-show-parser-state-marker nil t)
	(semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
	(add-hook 'semantic-after-toplevel-cache-change-hook
		  'semantic-show-parser-state-marker nil t)
	(semantic-show-parser-state-marker)
        )
    ;; Remove parts of mode line
    (setq mode-line-modified
	  (delq 'semantic-show-parser-state-string mode-line-modified))
    ;; Remove hooks
    (remove-hook 'semantic-edits-new-change-hooks
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-edits-incremental-reparse-failed-hooks
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-after-partial-cache-change-hook
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-before-auto-parse-hooks
		 'semantic-show-parser-state-auto-marker t)
    (remove-hook 'semantic-after-auto-parse-hooks
		 'semantic-show-parser-state-marker t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
		 'semantic-show-parser-state-marker t)
    )
  semantic-show-parser-state-mode)

;;;###autoload
(defun semantic-show-parser-state-mode (&optional arg)
  "Minor mode for displaying parser cache state in the modeline.
The cache can be in one of three states.  They are
Up to date, Partial reprase needed, and Full reparse needed.
The state is indicated in the modeline with the following characters:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `@'  ->  Auto-parse in progress (not set here.)
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-show-parser-state-mode 0 1))))
  (setq semantic-show-parser-state-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-show-parser-state-mode)))
  (semantic-show-parser-state-mode-setup)
  (run-hooks 'semantic-show-parser-state-mode-hook)
  (if (interactive-p)
      (message "show-parser-state minor mode %sabled"
               (if semantic-show-parser-state-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-show-parser-state-mode)

(semantic-add-minor-mode 'semantic-show-parser-state-mode
                         ""
                         semantic-show-parser-state-mode-map)

(defvar semantic-show-parser-state-string nil
  "String showing the parser state for this buffer.
See `semantic-show-parser-state-marker' for details.")
(make-variable-buffer-local 'semantic-show-parser-state-string)

(defun semantic-show-parser-state-marker (&rest ignore)
  "Set `semantic-show-parser-state-string' to indicate parser state.
This marker is one of the following:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `@'  ->  Auto-parse in progress (not set here.)
Arguments IGNORE are ignored, and accepted so this can be used as a hook
in many situations."
  (setq semantic-show-parser-state-string
	(cond ((semantic-parse-tree-needs-rebuild-p)
	       "!")
	      ((semantic-parse-tree-needs-update-p)
	       "~")
	      (t
               "-")))
  ;;(message "Setup mode line indicator to [%s]" semantic-show-parser-state-string)
  (semantic-mode-line-update))

(defun semantic-show-parser-state-auto-marker ()
  "Hook function run before an autoparse.
Set up `semantic-show-parser-state-marker' to show `@'
to indicate a parse in progress."
  (unless (semantic-parse-tree-up-to-date-p)
    (setq semantic-show-parser-state-string "@")
    (semantic-mode-line-update)
    ;; For testing.
    ;;(sit-for 1)
    ))


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
  "*If non-nil, enable global use of `semantic-summary-mode'.
When summary mode is enabled, the Emacs tool `eldoc-mode' is turned
on, and augmented to display semantic tags.  In idle time, a
message will be displayed showing details about the tag the cursor is on."
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
and the current buffer was set up for parsing.
When enabled, makes `eldoc' available for non-Emacs Lisp buffers.
Return non-nil if the minor mode is enabled."
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
  "Minor mode to show useful things about tags in echo area.
Enables/disables option `eldoc-mode' which supplies the support functions for
this minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
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
  "*If non-nil, enable global use of `semantic-stickyfunc-mode'.
This minor mode only works for Emacs 21 or later.
When enabled, the header line is enabled, and the first line
of the current function or method is displayed in it.
This makes it appear that the first line of that tag is
`sticky' to the top of the window."
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

(defcustom semantic-stickyfunc-indent-string
  (if window-system
      "    "
    "")
  "*String used to indent the stickyfunc header.
Customize this string to match the space used by scrollbars and
fringe so it does not appear that the code is moving left/right
when it lands in the sticky line.")

(defvar semantic-stickyfunc-old-hlf nil
  "Value of the header line when entering sticky func mode.")
(make-variable-buffer-local 'semantic-stickyfunc-old-hlf)

(defconst semantic-stickyfunc-header-line-format
  '(:eval (list semantic-stickyfunc-indent-string
                (semantic-stickyfunc-fetch-stickyline)))
  "The header line format used by sticky func mode.")

(defun semantic-stickyfunc-mode-setup ()
  "Setup option `semantic-stickyfunc-mode'.
For semantic enabled buffers, make the function declaration for the top most
function \"sticky\".  This is accomplished by putting the first line of
text for that function in Emacs 21's header line."
  (if semantic-stickyfunc-mode
      (progn
	(unless (and (featurep 'semantic) (semantic-active-p))
	  ;; Disable minor mode if semantic stuff not available
	  (setq semantic-stickyfunc-mode nil)
	  (error "Buffer %s was not set up for parsing" (buffer-name)))
	(unless (boundp 'default-header-line-format)
	  ;; Disable if there are no header lines to use.
	  (setq semantic-stickyfunc-mode nil)
	  (error "Sticky Function mode requires Emacs 21"))
	;; Enable the mode
	;; Save previous buffer local value of header line format.
	(kill-local-variable 'semantic-stickyfunc-old-hlf)
	(when (local-variable-p 'header-line-format)
	  (setq semantic-stickyfunc-old-hlf header-line-format))
	(setq header-line-format semantic-stickyfunc-header-line-format)
	)
    ;; Disable sticky func mode
    ;; Restore previous buffer local value of header line format if
    ;; the current one is the sticky func one.
    (when (eq header-line-format semantic-stickyfunc-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'semantic-stickyfunc-old-hlf)
	(setq header-line-format semantic-stickyfunc-old-hlf))))
  semantic-stickyfunc-mode)

;;;###autoload
(defun semantic-stickyfunc-mode (&optional arg)
  "Minor mode to show the title of a tag in the header line.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') has a header line, meaning the
first line which describes the rest of the construct.  This first
line is what is displayed in the Emacs 21 header line.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
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
(make-variable-buffer-local 'semantic-stickyfunc-sticky-classes)

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
	   (let* ((tags (nreverse (semantic-find-tag-by-overlay (point))))
		  (tag (progn
			 ;; Get rid of non-matching tags.
			 (while (and tags
				     (not (member
					   (semantic-tag-class (car tags))
					   semantic-stickyfunc-sticky-classes))
				     )
			   (setq tags (cdr tags)))
			 (car tags))))
	     ;; TAG is nil if there was nothing of the apropriate type there.
	     (if (not tag)
		 ;; Set it to be the text under the header line
		 (buffer-substring (point-at-bol) (point-at-eol))
	       ;; Get it
	       (goto-char (semantic-tag-start tag))
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
               (search-forward (semantic-tag-name tag) nil t)
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


;;;;
;;;; Minor mode to show some sort of boundary line in front of tags.
;;;;

;;;###autoload
(defun global-semantic-show-tag-boundaries-mode (&optional arg)
  "Toggle global use of option `semantic-show-tag-boundaries-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-show-tag-boundaries-mode
        (semantic-toggle-minor-mode-globally
         'semantic-show-tag-boundaries-mode arg)))

;;;###autoload
(defcustom global-semantic-show-tag-boundaries-mode nil
  "*If non-nil, enable global use of `semantic-show-tag-boundaries-mode'.
When this mode is enabled, a boundary line is displayed at the beginning
of some tags to highlight where they start."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-show-tag-boundaries-mode (if val 1 -1))))

(defcustom semantic-show-tag-boundaries-hook nil
  "*Hook run at the end of function `semantic-show-tag-boundaries-mode'."
  :group 'semantic
  :type 'hook)

(defface semantic-tag-boundary-face
  '((((class color) (background dark))
     (:overline "cyan"))
    (((class color) (background light))
     (:overline "blue")))
  "*Face used to show unmatched syntax in.
The face is used in  `semantic-show-tag-boundaries-mode'."
  :group 'semantic)

(defvar semantic-show-tag-boundaries-mode nil
  "Non-nil if show-unmatched-syntax minor mode is enabled.
Use the command `semantic-show-tag-boundaries-mode' to change this
variable.")
(make-variable-buffer-local 'semantic-show-tag-boundaries-mode)

(defun semantic-show-tag-boundaries-setup ()
  "Setup the `semantic-show-unmatched-syntax' minor mode.
The minor mode can be turned on only if the semantic feature is available
and the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (if semantic-show-tag-boundaries-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-show-tag-boundaries-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
	(semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
	(add-hook 'semantic-after-partial-cache-change-hook
		  'semantic-stb-reparse-hook nil t)
	(semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
	(add-hook 'semantic-after-toplevel-cache-change-hook
		  'semantic-stb-reparse-hook nil t)
	(semantic-stb-reparse-hook (semantic-bovinate-toplevel))
	)
    ;; Cleanup tag boundaries highlighting
    (semantic-stb-clear-boundaries (semantic-bovinate-toplevel))
    ;; Remove hooks
    (remove-hook 'semantic-after-partial-cache-change-hook
		 'semantic-stb-reparse-hook t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
		 'semantic-stb-reparse-hook t)
    )
  semantic-show-tag-boundaries-mode)
  
;;;###autoload
(defun semantic-show-tag-boundaries-mode (&optional arg)
  "Minor mode to display a boundary in front of tags.
The boundary is displayed using an overline in Emacs 21.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
;;
;;\\{semantic-show-tag-boundaries-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-show-tag-boundaries-mode 0 1))))
  (setq semantic-show-tag-boundaries-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-show-tag-boundaries-mode)))
  (semantic-show-tag-boundaries-setup)
  (run-hooks 'semantic-show-tag-boundaries-hook)
  (if (interactive-p)
      (message "show-tag-boundary-mode minor mode %sabled"
               (if semantic-show-tag-boundaries-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-show-tag-boundaries-mode)

(define-overload semantic-tag-boundary-p (tag)
  "Return non-nil of TAG should have a boundary placed on it.")

(defun semantic-tag-boundary-p-default (tag)
  "Non nil of TAG is a type, or a non-prototype function."
  (let ((c (semantic-tag-class tag)))

    (and
     (or
      ;; All types get a line?
      (eq c 'type)
      ;; Functions which aren't prototypes get a line.
      (and (eq (semantic-tag-class tag) 'function)
	   (not (semantic-tag-get-attribute tag 'prototype)))
      )
     ;; Nothing smaller than a few lines
     (> (- (semantic-tag-end tag) (semantic-tag-start tag)) 150)
     ;; Random truth
     t
     )))

(defun semantic-tag-boundary-overlay-p (ol)
  "Non nil of OL is an overlay that is a tag boundary."
  (semantic-overlay-get ol 'semantic-stb))

(defun semantic-stb-clear-boundaries (tag-list)
  "Clear boundaries off from TAG-LIST."
  (while tag-list
    (semantic-tag-delete-secondary-overlay (car tag-list)
					   'semantic-stb)

    ;; recurse over children
    (semantic-stb-clear-boundaries
     (semantic-tag-components-with-overlays (car tag-list)))
    
    (setq tag-list (cdr tag-list)))
  )

(defun semantic-stb-reparse-hook (tag-list)
  "Called when the new tags TAG-LIST are created in a buffer.
Adds decorations on them to help show tag boundaries."
  (while tag-list
    ;; Only line up certain classes of tag.
    (when (semantic-tag-boundary-p (car tag-list))
      (semantic-stb-highlight-tag-line1 (car tag-list)))
    ;; recurse over children
    (semantic-stb-reparse-hook
     (semantic-tag-components-with-overlays (car tag-list)))

    (setq tag-list (cdr tag-list)))
  )

(defun semantic-stb-highlight-tag-line1 (tag)
  "Highlight the first line of TAG as a boundary."
  (let ((o (semantic-tag-create-secondary-overlay tag)))
    ;; We do not use the unlink property because we do not want to
    ;; save the highlighting informatin in the DB.
    (semantic-overlay-put o 'face 'semantic-tag-boundary-face)
    (semantic-overlay-put o 'semantic-stb t)
    (semantic-overlay-move o (semantic-tag-start tag)
			   (save-excursion
			     (set-buffer (semantic-tag-buffer tag))
			     (goto-char (semantic-tag-start tag))
			     (end-of-line)
			     (forward-char 1)
			     (point)))
    
    ))

(semantic-add-minor-mode 'semantic-show-tag-boundaries-mode
                         ""
                         nil)


;;;;
;;;; Minor mode to show some sort of boundary line in front of tags.
;;;;

;;;###autoload
(defun global-semantic-highlight-by-attribute-mode (&optional arg)
  "Toggle global use of option `semantic-highlight-by-attribute-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-highlight-by-attribute-mode
        (semantic-toggle-minor-mode-globally
         'semantic-highlight-by-attribute-mode arg)))

;;;###autoload
(defcustom global-semantic-highlight-by-attribute-mode nil
  "*If non-nil, enable global use of `semantic-highlight-by-attribute-mode'.
When this mode is enabled tags are auto-highlighted based on attributes of the tag.
See `semantic-highlight-attribute-alist' for customization."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-highlight-by-attribute-mode (if val 1 -1))))

(defcustom semantic-highlight-by-attribute-hook nil
  "*Hook run at the end of function `semantic-highlight-by-attribute-mode'."
  :group 'semantic
  :type 'hook)

(defcustom semantic-highlight-attribute-alist
  '(
    ((lambda (tag) (and (member (semantic-tag-class tag) '(function variable))
			(eq (semantic-tag-protection tag) 'private))) .
     semantic-highlight-attribute-face-private)
    ((lambda (tag) (and (member (semantic-tag-class tag) '(function variable))
			(eq (semantic-tag-protection tag) 'protected))) .
     semantic-highlight-attribute-face-protected)
    )
  "Association list of functions testing attributes of a tag and a faces.
Each function must take one argument, the tag.
Each face will be applied to those tags for which the function returns true."
  :group 'semantic
  :type '(list (cons function face)))

(defface semantic-highlight-attribute-face-private
  '((((class color) (background dark))
     (:background "#500000"))
    (((class color) (background light))
     (:overline "#8aaaaa")))
  "*Face used to show privatly scoped tags in.
The face is used in  `semantic-highlight-by-attribute-mode'."
  :group 'semantic)

(defface semantic-highlight-attribute-face-protected
  '((((class color) (background dark))
     (:background "#000050"))
    (((class color) (background light))
     (:overline "#aaaaa8")))
  "*Face used to show protected scoped tags in.
The face is used in  `semantic-highlight-by-attribute-mode'."
  :group 'semantic)

(defvar semantic-highlight-by-attribute-mode nil
  "Non-nil if show-unmatched-syntax minor mode is enabled.
Use the command `semantic-highlight-by-attribute-mode' to change this
variable.")
(make-variable-buffer-local 'semantic-highlight-by-attribute-mode)

(defun semantic-highlight-by-attribute-setup ()
  "Setup the `semantic-show-unmatched-syntax' minor mode.
The minor mode can be turned on only if the semantic feature is available
and the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (if semantic-highlight-by-attribute-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-highlight-by-attribute-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
	(semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
	(add-hook 'semantic-after-partial-cache-change-hook
		  'semantic-hba-reparse-hook nil t)
	(semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
	(add-hook 'semantic-after-toplevel-cache-change-hook
		  'semantic-hba-reparse-hook nil t)
	(semantic-hba-reparse-hook (semantic-bovinate-toplevel))
	)
    ;; Cleanup tag boundaries highlighting
    (semantic-hba-clear-highlighting (semantic-bovinate-toplevel))
    ;; Remove hooks
    (remove-hook 'semantic-after-partial-cache-change-hook
		 'semantic-hba-reparse-hook t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
		 'semantic-hba-reparse-hook t)
    )
  semantic-highlight-by-attribute-mode)
  
;;;###autoload
(defun semantic-highlight-by-attribute-mode (&optional arg)
  "Minor mode to highlight tags based on some attribute.
By default, the protection of a tag will give it a different
background color.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
;;
;;\\{semantic-highlight-by-attribute-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-highlight-by-attribute-mode 0 1))))
  (setq semantic-highlight-by-attribute-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-highlight-by-attribute-mode)))
  (semantic-highlight-by-attribute-setup)
  (run-hooks 'semantic-highlight-by-attribute-hook)
  (if (interactive-p)
      (message "highlight-by-attribute minor mode %sabled"
               (if semantic-highlight-by-attribute-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-highlight-by-attribute-mode)

(defun semantic-hba-clear-highlighting (tag-list)
  "Clear highlighting off from TAG-LIST."
  (while tag-list

    ;; This isn't very smart, but should be ok for the short term.
    ;; What to do when our face is deep in the stack of highlights
    ;; for this tag?
    (semantic-unhighlight-tag (car tag-list))
    
    (setq tag-list (cdr tag-list)))
  )

(defun semantic-hba-reparse-hook (tag-list)
  "Called when the new tags TAG-LIST are created in a buffer.
Adds decorations on them to help show tag boundaries."
  (while tag-list
    ;; Only line up certain classes of tag.
    (semantic-hba-highlight-tag (car tag-list))
    ;; recurse over children
    (semantic-hba-reparse-hook
     (semantic-tag-components-with-overlays (car tag-list)))

    (setq tag-list (cdr tag-list)))
  )

(defun semantic-hba-highlight-tag (tag)
  "Highlight TAG based on it's attributes."
  (let ((aa semantic-highlight-attribute-alist))
    (while aa
      (when (funcall (car (car aa)) tag)
	(semantic-highlight-tag tag (cdr (car aa))))
      (setq aa (cdr aa))))
  )

(semantic-add-minor-mode 'semantic-highlight-by-attribute-mode
                         ""
                         nil)


(provide 'semantic-util-modes)

;;; semantic-util-modes.el ends here
