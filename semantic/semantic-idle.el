;;; semantic-idle.el --- Schedule parsing tasks in idle time

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-idle.el,v 1.4 2003/12/23 02:19:44 zappo Exp $

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

(defcustom semantic-idle-scheduler-idle-time 2
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
                         "ARP"
                         nil)

(semantic-alias-obsolete 'semantic-auto-parse-mode
			 'semantic-idle-scheduler-mode)
(semantic-alias-obsolete 'global-semantic-auto-parse-mode
			 'global-semantic-idle-scheduler-mode)


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

;;;###autoload
(defun semantic-idle-scheduler-add (function)
  "Schedule FUNCTION to occur during idle time."
  (add-to-list 'semantic-idle-scheduler-queue function))

;;;###autoload
(defun semantic-idle-scheduler-remove (function)
  "Unschedule FUNCTION to occur during idle time."
  (setq semantic-idle-scheduler-queue
	(delete function semantic-idle-scheduler-queue)))

;;; IDLE Function
;;
;; This is the idle function which handles reparsing, and also
;; manages services that depend on tag values.
(defun semantic-idle-scheduler-function ()
  "Function run when after `semantc-idle-scheduler-idle-time'.
This function will reparse the current buffer, and if successful,
call additional functions registered with the timer calls."
  (when (semantic-idle-scheduler-enabled-p)

    ;; Disable the auto parse timer while re-parsing
    (semantic-idle-scheduler-kill-timer)

    (semantic-exit-on-input 'idle-timer
      (condition-case err
	  ;; First, reparse the current buffer.
	  (let ((inhibit-quit nil)
		(lexok (semantic-idle-scheduler-refresh-tags))
		(buffers (buffer-list))
		(queue semantic-idle-scheduler-queue)
		)

	    ;; Now loop over other buffers, trying to update them as well.
	    (save-excursion
	      (while buffers
		(semantic-throw-on-input 'parsing-all-buffers)
		(set-buffer (car buffers))
		(when semantic-idle-scheduler-mode
		  (semantic-idle-scheduler-refresh-tags)
		  )
		(setq buffers (cdr buffers))))
      
	    ;; Evaluate all other services.  Stop on keypress.
	    (when lexok
	      (while queue
		(semantic-throw-on-input 'idle-queue)
		(funcall (car queue))
		))
	    )
	(error (message "idle error: %S" err))))
    
    ;; Enable again the auto parse timer
    (semantic-idle-scheduler-setup-timer)))

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
		  (semantic-bovinate-toplevel t)
		  nil)
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
    ;; Return if we are lexically safe
    lexically-safe))


;;; IDLE SERVICES
;;
;; Idle Services are minor modes which enable or disable a services in
;; the idle scheduler.  Creating a new services only requires calling
;; `semantic-create-idle-services' which does all the setup
;; needed to create the minor mode that will enable or disable
;; a services.  The services must provide a single function.

(defmacro define-semantic-idle-service (name doc &rest forms)
  "Create a new idle services with NAME.
DOC will be a documentation string describing FORMS.
FORMS will be called during idle time after the current buffer's
semantic tag information has been updated.
This routines creates the following functions and variables:"
  (let ((global (intern (concat "global-" (symbol-name name) "-mode")))
	(mode 	(intern (concat (symbol-name name) "-mode")))
	(hook 	(intern (concat (symbol-name name) "-mode-hook")))
	(map  	(intern (concat (symbol-name name) "-mode-map")))
	(setup 	(intern (concat (symbol-name name) "-mode-setup")))
	(func 	(intern (concat (symbol-name name) "-idle-function")))
	)

    `(progn
       (defun ,global (&optional arg)
	 ,(concat "Toggle global use of option `" (symbol-name mode) "'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.")
	 (interactive "P")
	 (setq ,global
	       (semantic-toggle-minor-mode-globally
		',mode arg)))

       (defcustom ,global nil
	 (concat "*If non-nil, enable global use of `" (symbol-name ',mode) "'.
" ,doc)
	 :group 'semantic
	 :type 'boolean
	 :require 'semantic-util-modes
	 :initialize 'custom-initialize-default
	 :set (lambda (sym val)
		(,global (if val 1 -1))))

       (defcustom ,hook nil
	 (concat "*Hook run at the end of function `" (symbol-name ',mode) "'.")
	 :group 'semantic
	 :type 'hook)

       (defvar ,map
	 (let ((km (make-sparse-keymap)))
	   km)
	 (concat "Keymap for `" (symbol-name ',mode) "'."))

       (defvar ,mode nil
	 (concat "Non-nil if summary minor mode is enabled.
Use the command `" (symbol-name ',mode) "' to change this variable."))
       (make-variable-buffer-local ',mode)

       (defun ,setup ()
	 ,(concat "Setup option `" (symbol-name mode) "'.
The minor mode can be turned on only if semantic feature is available
and the idle scheduler is active.
Return non-nil if the minor mode is enabled.")
	 (if ,mode
	     (if (not (and (featurep 'semantic) (semantic-active-p)))
		 (progn
		   ;; Disable minor mode if semantic stuff not available
		   (setq ,mode nil)
		   (error "Buffer %s was not set up for parsing"
			  (buffer-name)))
	       ;; Enable eldoc mode
	       (semantic-idle-scheduler-add #',func)
	       )
	   ;; Disable eldoc mode
	   (semantic-idle-scheduler-remove #',func)
	   )
	 ,mode)

;;;###autoload
       (defun ,mode (&optional arg)
	 ,(concat doc "
This is a minor mode which performs actions during idle time.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.")
	 (interactive
	  (list (or current-prefix-arg
		    (if ,mode 0 1))))
	 (setq ,mode
	       (if arg
		   (>
		    (prefix-numeric-value arg)
		    0)
		 (not ,mode)))
	 (,setup)
	 (run-hooks ,hook)
	 (if (interactive-p)
	     (message "%s %sabled"
		      (symbol-name ',mode)
		      (if ,mode "en" "dis")))
	 (semantic-mode-line-update)
	 ,mode)

       (semantic-add-minor-mode ',mode
				""	; idle schedulers are quiet?
				,map)
    
       (defun ,func ()
	 ,doc
	 ,@forms)
    
       )))
(put 'define-semantic-idle-service 'lisp-indent-function 1)


;;; SUMMARY MODE
;;
;; Use ELDOC services to show useful info about symbol under point.
(require 'semantic-ctxt)

(defcustom semantic-idle-summary-function 'semantic-format-tag-summarize
  "*Function to use when displaying tag information during idle time.
Some useful functions are found in `semantic-format-tag-functions'."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defsubst semantic-idle-summary-find-current-symbol-tag (sym)
  "Search for a semantic tag with name SYM.
Return the tag found or nil if not found."
  (car (if (and (featurep 'semanticdb) semanticdb-current-database)
           (cdar (semanticdb-deep-find-tags-by-name sym))
         (semantic-deep-find-tags-by-name sym (current-buffer)))))

(defun semantic-idle-summary-current-symbol-info-default ()
  "Return a string message describing the current context."
  (let (sym found)
    (and
     ;; 1- Look for a tag with current symbol name
     (setq sym (car (semantic-ctxt-current-symbol)))
     (not (setq found (senator-find-current-symbol-tag sym)))
     ;; 2- Look for a keyword with that name
     (semantic-lex-keyword-p sym)
     (not (setq found (semantic-lex-keyword-get sym 'summary)))
     ;; 3- Look for a tag with current function name
     (setq sym (car (semantic-ctxt-current-function)))
     (not (setq found (senator-find-current-symbol-tag sym)))
     ;; 4- Look for a keyword with that name
     (semantic-lex-keyword-p sym)
     (setq found (semantic-lex-keyword-get sym 'summary)))
    found))

(define-semantic-idle-service semantic-idle-summary
  "Display a tag summary of the lexcial token under the cursor.
The means for getting the current tag to display information can
be override with `idle-summary-current-symbol-info'"
  (unless (eq major-mode 'emacs-lisp-mode)

    ;; Double overload symbols for backward compatibility
    (let ((s (or (semantic-fetch-overload 'idle-summary-current-symbol-info)
		 (semantic-fetch-overload 'eldoc-current-symbol-info)))
	  found str
	  )
      
      (if s (setq found (funcall s))
	(setq found (semantic-idle-summary-current-symbol-info-default)))

      (setq str (cond ((stringp found)
		       found)
		      ((semantic-tag-p found)
		       (funcall semantic-idle-summary-function
				found nil t))
		      (t nil)
		      ))

      (unless (and (boundp 'eldoc-echo-area-use-multiline-p)
		   eldoc-echo-area-use-multiline-p)
	(let ((w (1- (window-width (minibuffer-window)))))
	  (if (> (length str) w)
	      (setq str (substring str 0 w)))))
      
      (eldoc-message str))  
    ))

(put 'semantic-idle-summary-function 'semantic-overload
     'idle-summary-current-symbol-info)
 
(semantic-alias-obsolete 'semantic-summary-mode
			 'semantic-idle-summary-mode)
(semantic-alias-obsolete 'global-semantic-summary-mode
			 'global-semantic-idle-summary-mode)

(provide 'semantic-idle)

;;; semantic-idle.el ends here
