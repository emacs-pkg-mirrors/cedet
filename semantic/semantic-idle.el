;;; semantic-idle.el --- Schedule parsing tasks in idle time

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-idle.el,v 1.1 2003/12/18 02:17:15 zappo Exp $

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
;; Originally, `semantic-auto-parse-mode' handled refreshing the
;; tags in a buffer in idle time.  Other activities can be scheduled
;; in idle time, all of which require up-to-date tag tables.
;; Having a specialized idle time scheduler that first refreshes
;; the tags buffer, and then enables other idle time tasks reduces
;; the amount of work needed.  Any specialized idle tasks need not
;; ask for a fresh tags list.
;;

(require 'semantic-util-modes)
(require 'timer)

;;; Code:

;;; TIMER RELATED FUNCTIONS
;;
(defvar semantic-idle-scheduler-timer nil
  "Timer used to schedule tasks in idle time.")

(defcustom semantic-idle-scheduler-idle-time 4
  "*Time in seconds of idle time before auto-reparse.
This time should be short enough to ensure that idle-scheduler will be
run as soon as Emacs is idle."
  :group 'semantic
  :type 'number
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp semantic-idle-scheduler-timer)
           (cancel-timer semantic-idle-scheduler-timer)
           (setq semantic-idle-scheduler-timer nil)
           (semantic-idle-scheduler-setup-timer))))

(defun semantic-idle-scheduler-setup-timer ()
  "Lazy initialization of the auto parse idle timer."
  ;; REFRESH THIS FUNCTION for XEMACS FOIBLES
  (or (timerp semantic-idle-scheduler-timer)
      (setq semantic-idle-scheduler-timer
            (run-with-idle-timer
             semantic-idle-scheduler-idle-time t
             #'semantic-idle-scheduler-function))))

(defun semantic-idle-scheduler-kill-timer ()
  "Kill the auto parse idle timer."
  (if (timerp semantic-idle-scheduler-timer)
      (cancel-timer semantic-idle-scheduler-timer))
  (setq semantic-idle-scheduler-timer nil))


;;; MINOR MODE
;;
;; The minor mode portion of this code just sets up the minor mode
;; which does the initial scheduling of the idle timers.
;;
;;;###autoload
(defcustom global-semantic-idle-scheduler-mode nil
  "*If non-nil, enable global use of idle-scheduler mode."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-idle-scheduler-mode (if val 1 -1))))

;;;###autoload
(defun global-semantic-idle-scheduler-mode (&optional arg)
  "Toggle global use of option `semantic-idle-scheduler-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-idle-scheduler-mode
        (semantic-toggle-minor-mode-globally
         'semantic-idle-scheduler-mode arg)))

(defcustom semantic-idle-scheduler-mode-hook nil
  "*Hook run at the end of function `semantic-idle-scheduler-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-idle-scheduler-mode nil
  "Non-nil if idle-scheduler minor mode is enabled.
Use the command `semantic-idle-scheduler-mode' to change this variable.")
(make-variable-buffer-local 'semantic-idle-scheduler-mode)

(defsubst semantic-idle-scheduler-enabled-p ()
  "Return non-nil if idle-scheduler is enabled for this buffer.
See also the variable `semantic-idle-scheduler-max-buffer-size'."
  (if semantic-idle-scheduler-mode
      (and (not semantic-debug-enabled)
	   (not semantic-lex-debug))))

(defun semantic-idle-scheduler-mode-setup ()
  "Setup option `semantic-idle-scheduler-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if semantic-idle-scheduler-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-idle-scheduler-mode nil)
            (error "Buffer %s was not set up idle time scheduling"
                   (buffer-name)))
        (semantic-idle-scheduler-setup-timer)))
  semantic-idle-scheduler-mode)

;;;###autoload
(defun semantic-idle-scheduler-mode (&optional arg)
  "Minor mode to auto parse buffer following a change.
When this mode is off, a buffer is only rescanned for tokens when
some command requests the list of available tokens.  When idle-scheduler
is enabled, Emacs periodically checks to see if the buffer is out of
date, and reparses while the user is idle (not typing.)

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-idle-scheduler-mode 0 1))))
  (setq semantic-idle-scheduler-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-idle-scheduler-mode)))
  (semantic-idle-scheduler-mode-setup)
  (run-hooks 'semantic-idle-scheduler-mode-hook)
  (if (interactive-p)
      (message "idle-scheduler minor mode %sabled"
               (if semantic-idle-scheduler-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-idle-scheduler-mode)

(semantic-add-minor-mode 'semantic-idle-scheduler-mode
                         "a"
                         nil)



;;; SERVICES services
;;
;; These are services for managing idle services.
;;
(defvar semantic-idle-scheduler-queue nil
  "List of functions to execute during idle time.
These functions will be called in the current buffer after that
buffer has had its tags made up to date.  These functions
will not be called if there are errors parsing the
current buffer.")

(defun semantic-idle-scheduler-add (function)
  "Schedule FUNCTION to occur during idle time."
  (add-to-list 'semantic-idle-scheduler-queue function))

(defun semantic-idle-scheduler-remove (function)
  "Unschedule FUNCTION to occur during idle time."
  (setq semantic-idle-scheduler-queue
	(delete function semantic-idle-scheduler-queue)))

;;; IDLE Function
;;
;; This is the idle function which handles reparsing, and also
;; manages services that depend on tag values.
(defvar semantic-idle-scheduler-input-function
  (list (lambda () (not (sit-for 0 0 t)))
	(lambda () (not (sit-for 0))))
  "List of functions to use to calculate if there is user input waiting.")

(defun semantic-idle-scheduler-user-input-waiting-p ()
  "Return non-nil if there is user input waiting for us."
  (when semantic-idle-scheduler-input-function
    (condition-case nil
	(funcall (car semantic-idle-scheduler-input-function))
      (error
       (setq semantic-idle-scheduler-input-function
	     (cdr semantic-idle-scheduler-input-function))
       (semantic-idle-scheduler-user-input-waiting-p))
      )))

(defun semantic-idle-scheduler-function ()
  "Function run when after `semantc-idle-scheduler-idle-time'.
This function will reparse the current buffer, and if successful,
call additional functions registered with the timer calls."
  ;; First, reparse the current buffer.
  (let ((inhibit-quit nil)
	(lexok (semantic-idle-scheduler-refresh-tags))
	(queue semantic-idle-scheduler-queue)
	)
    
    ;; Evaluate all other services.  Stop on keypress.
    (when lexok
      (while (and queue (semantic-idle-scheduler-user-input-waiting-p))
	(funcall (car queue))
	))
    ))


;;; REPARSING
;;
;; Reparsing is installed as semantic idle service.
;; This part ALWAYS happens, and other services occur
;; afterwards.

(defcustom semantic-idle-scheduler-no-working-message t
  "*If non-nil, disable display of working messages during parse."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-idle-scheduler-working-in-modeline-flag nil
  "*Non-nil means show working messages in the mode line.
Typically, parsing will show messages in the minibuffer.
This will move the parse message into the modeline."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-idle-scheduler-working-in-modeline-flag nil
  "*Non-nil means show working messages in the mode line.
Typically, parsing will show messages in the minibuffer.
This will move the parse message into the modeline."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-idle-scheduler-max-buffer-size 0
  "*Maximum size in bytes of buffers automatically reparsed.
If this value is less than or equal to 0, buffers are automatically
reparsed regardless of their size."
  :group 'semantic
  :type 'number)

(defvar semantic-before-idle-scheduler-reparse-hooks nil
  "Hooks run before option `semantic-idle-scheduler' begins parsing.
If any hook throws an error, this variable is reset to nil.")

(defvar semantic-after-idle-scheduler-reparse-hooks nil
  "Hooks run after option `semantic-idle-scheduler' has parsed.
If any hook throws an error, this variable is reset to nil.")

(defun semantic-idle-scheduler-refresh-tags ()
  "Refreshes the current buffer's tags.
This is called by `semantic-idle-scheduler-function' to update the
tags in the current buffer.

Return non-nil if the refresh was successful.
Return nil if there is some sort of syntax error preventing a full
reparse.

Does nothing if the current buffer doesn't need reparsing or if its
size exceeds the `semantic-idle-scheduler-max-buffer-size' threshold."

  (when (semantic-idle-scheduler-enabled-p)
    ;; Disable the auto parse timer while re-parsing
    (semantic-idle-scheduler-kill-timer)
    (let* ((semantic-bovination-working-type nil)
           (inhibit-quit nil)
           (working-use-echo-area-p
            (not semantic-idle-scheduler-working-in-modeline-flag))
           (working-status-dynamic-type
            (if semantic-idle-scheduler-no-working-message
                nil
              working-status-dynamic-type))
           (working-status-percentage-type
            (if semantic-idle-scheduler-no-working-message
                nil
              working-status-percentage-type))
           (semantic-lex-unterminated-syntax-end-function
            (lambda (syntax start end) (throw 'idle-scheduler syntax)))
	   ;; When flex is deleted, delete this also.
           (semantic-flex-unterminated-syntax-end-function
            (lambda (syntax start end) (throw 'idle-scheduler syntax)))
	   (lexically-safe t)
           )
      ;; Let people hook into this, but don't let them hose
      ;; us over!
      (condition-case nil
          (run-hooks 'semantic-before-idle-scheduler-reparse-hooks)
        (error (setq semantic-before-idle-scheduler-reparse-hooks nil)))

      (unwind-protect
          ;; Perform the parsing.
          (when (catch 'idle-scheduler
                  (save-excursion
                    (semantic-bovinate-toplevel t))
                  nil)
            ;; If we are here, it is because the lexical step failed,
	    ;; proably due to unterminated lists or something like that.

	    ;; We do nothing, and just wait for the next idle timer
	    ;; to go off.  In the meantime, remember this, and make sure
	    ;; no other idle services can get executed.
            (setq lexically-safe nil))
        ;; Let people hook into this, but don't let them hose
        ;; us over!
        (condition-case nil
            (run-hooks 'semantic-after-idle-scheduler-reparse-hooks)
          (error (setq semantic-after-idle-scheduler-reparse-hooks nil))))
      ;; Enable again the auto parse timer
      (semantic-idle-scheduler-setup-timer)
      ;; Return if we are lexically safe
      lexically-safe)))

(provide 'semantic-idle)

;;; semantic-idle.el ends here
