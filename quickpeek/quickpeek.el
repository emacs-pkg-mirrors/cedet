;;; quickpeek.el --- display info about current cursor context

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.4
;; Keywords: tools
;; X-RCS: $Id: quickpeek.el,v 1.4 2000/02/09 17:42:28 zappo Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; A discussion appeared in newsgroups recently discussing Emacs'
;; ability to pop up information about the current context of the
;; cursor in C++.  Needless to say, any such tool would also be useful
;; for other languages, thus arose this tool, quickpeek.
;;
;; quickpeek can be independently configured on a per-language basis
;; to identify, and display useful information about the current
;; context.  When there is nothing useful to do, it does something
;; else.

;;; How to support Lazy Look in a Major mode.
;;
;; Supporting Lazy Look in a major mode can be both easy, or complex
;; depending on the number of features you want to provide your users.
;;
;; At the simplest level, you need to set the variable
;; `quickpeek-info-function' to a function of your own devising.  This
;; is quickpeek's hook into your mode for display purposes.
;;
;; The return value of the `quickpeek-info-function' is a list.  The
;; first element of the list must be a quickpeek forms function.  You
;; can write your own form function (discussed later) or you can use
;; one of several built in functions.  The rest of the list elements
;; are parameters to the insert form.
;;
;; The most useful insert form is `quickpeek-functional-form', though
;; `quickpeek-simple-form' and `quickpeek-goofy-form' are also
;; useable.  Please see documentation on these functions for the
;; meaning of their parameters.  In many cases, the strings inserted
;; can follow the conventions of `quickpeek-plain-string-insert' which
;; permits addition of faces and (in the future) other formatting
;; features.  It will also strip most whitespace, so you must be careful.
;;
;; To write your own insert form, simply decide on the parameters,
;; and when it is called, assume that the current buffer is the
;; quickpeek frame.  You can then insert text, or add widgets.
;; Some good insert form helpers are: `quickpeek-insert-completions'
;; and `quickpeek-plain-string-insert'.
;;
;; Finding the info: Helper functions
;;
;; When writing a function to scan the buffers it is important to know
;; how the syntax tables work.  quickpeek depends on font-lock, and
;; its syntax table.  Font lock has gone through the trouble of
;; providing simplified searching syntax tables, specifically where
;; symbols such as _ become word constituants so you can search for
;; symbols with \\w.  By default, when your info function is called,
;; symbol constituants become word constituants through the font lock
;; maintained tables.
;;
;; If through some means you need to scan additional files, use the
;; function `quickpeek-with-alternate-syntax-table' to wrap your code
;; with this syntax table to simplify your regular expressions.
;;
;; In addition, if you want to use tags to find symbols for context
;; display, use the functions: `quickpeek-find-tag-stealthy' which
;; does not have any side effects, and returns a cons cell containing
;; the buffer and point of the symbol you want.
;;
;; Lastly, use `quickpeek-thing', and `quickpeek-thing-bounds' to
;; fetch a sexp under the point.  This simplifies getting the current
;; word for searching purposes.

;;; History:
;;
;; 0.4   Rename as quickpeek to avoid confusion with lazy-lock.
;;       Fixed CASE and constant colors.
;;
;; 0.3   Enable functions where return type and name are on different
;;         lines in the c language.
;;       Added `quickpeek-with-alternate-syntax-table', and use it.
;;       Made `thing-at-point' cover functions.
;;       Added `quickpeek-use-tags' flag variable.thing
;;       Added `quickpeek-tags-completion' as a local convenience.
;;       Added 'quickpeek-facep' macro as multi-platform convenience.
;;
;; 0.2   Improved the info gathering fn to report errors more effectivly.
;;       Improved lisp info gathering fn to exclude completion lists for
;;         very short symbols.
;;       Made the c info gatherer do something useful.  Added a
;;         non-desructive tag finder.
;;       Added some display types, including an error and simple type.
;;       Added a generic complex string inserttion function utility.
;;         This permits color use in the lazy frame.
;;       Now require's font lock.
;;       Fixed corrupt syntax table in main buffer.
;;
;; 0.1   First Release as lazy-look

(require 'frame)
(require 'widget)
;; This is to make it easiser to set the colors to useful things.
(require 'font-lock)

;;; Code:
(defvar quickpeek-xemacsp (or (featurep 'xemacs)
			      (string-match "XEmacs" emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defvar quickpeek-xemacs20p (and quickpeek-xemacsp
				 (= emacs-major-version 20)))

(defvar quickpeek-info-function 'quickpeek-default-info-function
  "A symbol representing what function to call when collecting info.
The info function takes no arguments, and starts in the current
buffer.  This function must return a list of the form:
  (FORM REST )
Form is a symbol representing a `quickpeek' compatible insert function.
At the moment the only FORM available is `quickpeek-functional-form',
and `quickpeek-file-form'.  See documentation for these functions as
to the organization of REST.")
(make-variable-buffer-local 'quickpeek-info-function)

(defcustom quickpeek-frame-parameters '((minibuffer . nil)
					(width . 80)
					(height . 4)
					(border-width . 0)
					(menu-bar-lines . 0)
					(unsplittable . t))
  "*Parameters to use when creating the `quickpeek' frame in Emacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame `quickpeek'
is attached to."
  :group 'quickpeek
  :type '(repeat (sexp :tag "Parameter:")))

;; These values by Hrvoje Niksic <hniksic@srce.hr>
(defcustom quickpeek-frame-plist
  '(minibuffer nil width 80 height 4 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p nil)
  "*Parameters to use when creating the `quickpeek' frame in XEmacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame `quickpeek'
is attached to."
  :group 'quickpeek
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value"))))

;; Make sure these are different from speedbar.
(defcustom quickpeek-update-speed
  (if quickpeek-xemacsp
      (if quickpeek-xemacs20p
	  4				; 1 is too obrusive in XEmacs
	6)				; when no idleness, need long delay
    1)
  "*Idle time in seconds needed before `quickpeek' will update itself.
Updates occur to allow `quickpeek' to display directory information
relevant to the buffer you are currently editing."
  :group 'quickpeek
  :type 'integer)

(defvar quickpeek-update-flag (and
			      (or (fboundp 'run-with-idle-timer)
				  (fboundp 'start-itimer)
				  (boundp 'post-command-idle-hook))
			      window-system)
  "*Non-nil means to automatically update the display.
When this is nil then `quickpeek' will not follow the attached frame's path.
When `quickpeek' is active, use:

\\<quickpeek-key-map> `\\[quickpeek-toggle-updates]'

to toggle this value.")

(defcustom quickpeek-use-tags t
  "*Use tags table for languages that use them as a symbol database."
  :group 'quickpeek
  :type 'boolean)

(defvar quickpeek-syntax-table nil
  "Syntax-table used on the `quickpeek'.")

(if quickpeek-syntax-table
    nil
  (setq quickpeek-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " quickpeek-syntax-table)
;  (modify-syntax-entry ?\" " " quickpeek-syntax-table)
  (modify-syntax-entry ?( "()" quickpeek-syntax-table)
  (modify-syntax-entry ?) ")( " quickpeek-syntax-table)
  (modify-syntax-entry ?{ ")}" quickpeek-syntax-table)
  (modify-syntax-entry ?} "({ " quickpeek-syntax-table)
  (modify-syntax-entry ?[ "(]" quickpeek-syntax-table)
  (modify-syntax-entry ?] ")[ " quickpeek-syntax-table))

(defvar quickpeek-key-map nil
  "Keymap used in `quickpeek' buffer.")

(if quickpeek-key-map
    nil
  (setq quickpeek-key-map (make-keymap))
  (suppress-keymap quickpeek-key-map t)

  ;; control
  (define-key quickpeek-key-map "g" 'quickpeek-refresh)
  (define-key quickpeek-key-map "t" 'quickpeek-toggle-updates)
  (define-key quickpeek-key-map "q" 'quickpeek-close-frame)
  (define-key quickpeek-key-map "Q" 'delete-frame)
  )

(defvar quickpeek-frame nil
  "The frame used for lazy look mode.")
(defvar quickpeek-cached-frame nil
  "The frame used for lazy look mode, then hidden.")
(defvar quickpeek-buffer nil
  "The buffer used for lazy look mode.")
(defvar quickpeek-timer nil
  "The `quickpeek' timer used for updating the buffer.")
(defvar quickpeek-marker (make-marker)
  "Remember where we last looked from.")

;;;###autoload
(defalias 'quickpeek 'quickpeek-frame-mode)
;;;###autoload
(defun quickpeek-frame-mode (&optional arg)
  "Initialize `quickpeek'.
If optional ARG is less than 0, turn off this mode, positive turn on.
If nil, then toggle."
  (interactive "P")
  ;; toggle frame on and off.
  (if (not arg) (if (and (frame-live-p quickpeek-frame)
			 (frame-visible-p quickpeek-frame))
		    (setq arg -1) (setq arg 1)))
  ;; turn the frame off on neg number
  (if (and (numberp arg) (< arg 0))
      (progn
	(run-hooks 'quickpeek-before-delete-hook)
	(if (and quickpeek-frame (frame-live-p quickpeek-frame))
	    (progn
	      (setq quickpeek-cached-frame quickpeek-frame)
	      (make-frame-invisible quickpeek-frame)))
	(setq quickpeek-frame nil)
	(quickpeek-set-timer nil)
	;; Used to delete the buffer.  This has the annoying affect of
	;; preventing whatever took its place from ever appearing
	;; as the default after a C-x b was typed
	;;(if (bufferp quickpeek-buffer)
	;;    (kill-buffer quickpeek-buffer))
	)
    (run-hooks 'quickpeek-before-popup-hook)
    ;; Get the frame to work in
    (if (frame-live-p quickpeek-cached-frame)
	(progn
	  (setq quickpeek-frame quickpeek-cached-frame)
	  (make-frame-visible quickpeek-frame)
	  ;; Get the buffer to play with
	  (quickpeek-mode)
	  (select-frame quickpeek-frame)
	  (if (not (eq (current-buffer) quickpeek-buffer))
	      (switch-to-buffer quickpeek-buffer))
	  (set-window-dedicated-p (selected-window) t)
	  (raise-frame quickpeek-frame)
	  (quickpeek-set-timer quickpeek-update-speed)
	  )
      (if (frame-live-p quickpeek-frame)
	  (raise-frame quickpeek-frame)
	(setq quickpeek-frame
	      (if quickpeek-xemacsp
		  ;; Only guess height if it is not specified.
		  (if (member 'height quickpeek-frame-plist)
		      (make-frame quickpeek-frame-plist)
		    (make-frame (nconc (list 'height
					     (quickpeek-needed-height))
				       quickpeek-frame-plist)))
		(make-frame quickpeek-frame-parameters)))
	;; Put the buffer into the frame
	(save-window-excursion
	  ;; Get the buffer to play with
	  (quickpeek-mode)
	  (select-frame quickpeek-frame)
	  (switch-to-buffer quickpeek-buffer)
	  (set-window-dedicated-p (selected-window) t))
	(if (and (or (null window-system) (eq window-system 'pc))
		 (fboundp 'set-frame-name))
	    (progn
	      (select-frame quickpeek-frame)
	      (set-frame-name "Quickpeek")))
	(quickpeek-set-timer quickpeek-update-speed)))))

;;;###autoload
(defun quickpeek-get-focus ()
  "Change frame focus to or from the `quickpeek' frame.
If the selected frame is not `quickpeek', then `quickpeek' frame is
selected.  If the `quickpeek' frame is active, then select the attached frame."
  (interactive)
  (if (eq (selected-frame) quickpeek-frame)
      (other-frame 1)
    ;; If updates are off, then refresh the frame (they want it now...)
    (if (not quickpeek-update-flag)
	(let ((quickpeek-update-flag t))
	  (quickpeek-timer-fn)))
    ;; make sure we have a frame
    (if (not (frame-live-p quickpeek-frame)) (quickpeek-frame-mode 1))
    ;; go there
    (select-frame quickpeek-frame)
    )
  (other-frame 0))

(defun quickpeek-close-frame ()
  "Turn off a currently active `quickpeek'."
  (interactive)
  (quickpeek-frame-mode -1)
  (other-frame 1))

(defun quickpeek-mode ()
  "Major mode for displaying lazy information in the `quickpeek' frame.
This frame can be placed anywhere on your desktop, and will attempt to
display information about the current selected buffer."
  ;; NOT interactive
  (save-excursion
    (setq quickpeek-buffer (set-buffer (get-buffer-create " QUICKPEEK")))
    (kill-all-local-variables)
    (setq major-mode 'quickpeek-mode)
    (setq mode-name "Quickpeek")
    (set-syntax-table quickpeek-syntax-table)
    (use-local-map quickpeek-key-map)
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines t)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format "Quickpeek")
    ;; Set this up special just for the quickpeek buffer
    ;; Terminal minibuffer stuff does not require this.
    (if (and window-system (not (eq window-system 'pc))
	     (null default-minibuffer-frame))
	(progn
	  (make-local-variable 'default-minibuffer-frame)
	  (setq default-minibuffer-frame (car (frame-list)))))
    ;; Correct use of `temp-buffer-show-function': Bob Weiner
    (if (and (boundp 'temp-buffer-show-hook)
	     (boundp 'temp-buffer-show-function))
	(progn (make-local-variable 'temp-buffer-show-hook)
	       (setq temp-buffer-show-hook temp-buffer-show-function)))
    (make-local-variable 'temp-buffer-show-function)
    (setq temp-buffer-show-function 'quickpeek-temp-buffer-show-function)
;     (if quickpeek-xemacsp
; 	(progn
; 	  ;; Argh!  mouse-track-click-hook doesn't understand the
; 	  ;; make-local-hook conventions.
; 	  (make-local-variable 'mouse-track-click-hook)
; 	  (add-hook 'mouse-track-click-hook
; 		    (lambda (event count)
; 		      (if (/= (event-button event) 1)
; 			  nil		; Do normal operations.
; 			(cond ((eq count 1)
; 			       (quickpeek-quick-mouse event))
; 			      ((or (eq count 2)
; 				   (eq count 3))
; 			       (mouse-set-point event)
; 			       (quickpeek-do-function-pointer)
; 			       (quickpeek-quick-mouse event)))
; 			;; Don't do normal operations.
; 			t)))))
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook (lambda () (let ((skilling (boundp 'skilling)))
					     (if skilling
						 nil
					       (if (eq (current-buffer)
						       quickpeek-buffer)
						   (quickpeek-frame-mode -1)))))
	      t t)
    (widget-minor-mode)
    (toggle-read-only 1)
    (quickpeek-set-mode-line-format)
;     (if quickpeek-xemacsp
; 	(progn
; 	  (make-local-variable 'mouse-motion-handler)
; 	  (setq mouse-motion-handler 'quickpeek-track-mouse-xemacs))
;       (if quickpeek-track-mouse-flag
; 	  (progn
; 	    (make-local-variable 'track-mouse)
; 	    (setq track-mouse t)))	;this could be messy.
;       (setq auto-show-mode nil))	;no auto-show for Emacs
    (run-hooks 'quickpeek-mode-hook))
  (quickpeek-update-contents)
  quickpeek-buffer)

(defun quickpeek-temp-buffer-show-function (buffer)
  "Placed in the variable `temp-buffer-show-function' in `quickpeek-mode'.
If a user requests help using \\[help-command] <Key> the temp BUFFER will be
redirected into a window on the attached frame."
  (pop-to-buffer buffer nil)
  (other-window -1)
  ;; Fix for using this hook on some platforms: Bob Weiner
  (cond ((not quickpeek-xemacsp)
	 (run-hooks 'temp-buffer-show-hook))
	((fboundp 'run-hook-with-args)
	 (run-hook-with-args 'temp-buffer-show-hook buffer))
	((and (boundp 'temp-buffer-show-hook)
	      (listp temp-buffer-show-hook))
	 (mapcar (function (lambda (hook) (funcall hook buffer)))
		 temp-buffer-show-hook))))

(defmacro quickpeek-frame-width ()
  "Return the width of the `quickpeek' frame in characters.
nil if it doesn't exist."
  '(frame-width quickpeek-frame))

(defun quickpeek-set-mode-line-format ()
  "Set the format of the modeline based on the `quickpeek' environment.
This gives visual indications of what is up.  It EXPECTS the `quickpeek'
frame and window to be the currently active frame and window."
  (if (and (frame-live-p quickpeek-frame)
	   (or (not quickpeek-xemacsp)
	       (specifier-instance has-modeline-p)))
      (let ((buff (current-buffer))
	    (nsupp (eq quickpeek-info-function
		       'quickpeek-default-info-function)))
	(save-excursion
	  (set-buffer quickpeek-buffer)
	  (setq frame-title-format
		(format "Quickpeek: %s @ %d%s" (buffer-name buff)
			(or (marker-position quickpeek-marker)
			    0)
			(if nsupp " (Unsupported Mode)" "")))
	  (let* ((w (or (quickpeek-frame-width) 20))
		 (p1 "<<")
		 (p5 ">>")
		 (p3 (if quickpeek-update-flag "QUICK PEEK" "SLOW POKE"))
		 (blank (- w (length p1) (length p3) (length p5)
			   (if line-number-mode 4 0)))
		 (p2 (if (> blank 0)
			 (make-string (/ blank 2) ? )
		       ""))
		 (p4 (if (> blank 0)
			 (make-string (+ (/ blank 2) (% blank 2)) ? )
		       ""))
		 (tf
		  (if line-number-mode
		      (list (concat p1 p2 p3) '(line-number-mode " %3l")
			    (concat p4 p5))
		    (list (concat p1 p2 p3 p4 p5)))))
	    (if (not (equal mode-line-format tf))
		(progn
		  (setq mode-line-format tf)
		  (force-mode-line-update))))))))

(defun quickpeek-toggle-updates ()
  "Toggle automatic update for the `quickpeek' frame."
  (interactive)
  (if quickpeek-update-flag
      (quickpeek-disable-update)
    (quickpeek-enable-update)))

(defun quickpeek-enable-update ()
  "Enable automatic updating in `quickpeek' via timers."
  (interactive)
  (setq quickpeek-update-flag t)
  (quickpeek-set-mode-line-format)
  (quickpeek-set-timer quickpeek-update-speed))

(defun quickpeek-disable-update ()
  "Disable automatic updating and stop consuming resources."
  (interactive)
  (setq quickpeek-update-flag nil)
  (quickpeek-set-mode-line-format)
  (quickpeek-set-timer nil))


;;; Utility functions
;;
(defun quickpeek-set-timer (timeout)
  "Apply a timer with TIMEOUT, or remove a timer if TIMOUT is nil.
TIMEOUT is the number of seconds until the `quickpeek' timer is called
again.  When TIMEOUT is nil, turn off all timeouts.
This function will also enable or disable the `vc-checkin-hook' used
to track file check ins, and will change the mode line to match
`quickpeek-update-flag'."
  (cond
   ;; XEmacs
   (quickpeek-xemacsp
    (if quickpeek-timer
	(progn (delete-itimer quickpeek-timer)
	       (setq quickpeek-timer nil)))
    (if timeout
	(if (and quickpeek-xemacsp
		 (or (>= emacs-major-version 20)
		     (>= emacs-minor-version 15)))
	    (setq quickpeek-timer (start-itimer "quickpeek"
					       'quickpeek-timer-fn
					       timeout
					       timeout
					       t))
	  (setq quickpeek-timer (start-itimer "quickpeek"
					     'quickpeek-timer-fn
					     timeout
					     nil)))))
   ;; Post 19.31 Emacs
   ((fboundp 'run-with-idle-timer)
    (if quickpeek-timer
	(progn (cancel-timer quickpeek-timer)
	       (setq quickpeek-timer nil)))
    (if timeout
	(setq quickpeek-timer
	      (run-with-idle-timer timeout t 'quickpeek-timer-fn))))
   ;; Emacs 19.30 (Thanks twice: ptype@dra.hmg.gb)
   ((fboundp 'post-command-idle-hook)
    (if timeout
	(add-hook 'post-command-idle-hook 'quickpeek-timer-fn)
      (remove-hook 'post-command-idle-hook 'quickpeek-timer-fn)))
   ;; Older or other Emacsen with no timers.  Set up so that its
   ;; obvious this emacs can't handle the updates
   (t
    (setq quickpeek-update-flag nil)))
  ;; change this if it changed for some reason
  (quickpeek-set-mode-line-format))

(defmacro quickpeek-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'quickpeek-with-writable 'lisp-indent-function 0)

(defun quickpeek-select-window (buffer)
  "Select a window in which BUFFER is shown.
If it is not shown, force it to appear in the default window."
  (let ((win (get-buffer-window buffer (marker-buffer quickpeek-marker))))
    (if win
	(select-window win)
      (set-window-buffer (selected-window) buffer))))

(defun quickpeek-timer-fn ()
  "Run whenever Emacs is idle to update the `quickpeek' item."
  (if (not (frame-live-p quickpeek-frame))
      (quickpeek-set-timer nil)
    ;; Save all the match data so that we don't mess up executing fns
    (save-match-data
      ;; Only do stuff if the frame is visible, not an icon, and if
      ;; it is currently flagged to do something.
      (if (and quickpeek-update-flag
	       (frame-visible-p quickpeek-frame)
	       (not (equal (point-marker) quickpeek-marker))
	       (not (eq (frame-visible-p quickpeek-frame) 'icon))
	       (not (eq (selected-frame) quickpeek-frame))
	       (not (eq (selected-window) (minibuffer-window))))
	  (let ((af (selected-frame)))
	    (save-window-excursion
	      ;; Other frame is magic for the previously reffed frame.
	      (if (eq af quickpeek-frame) (other-frame -1))
	      (quickpeek-update-contents)))))))

(defun quickpeek-refresh ()
  "Refresh the current `quickpeek' display, disposing of any cached data."
  (interactive)
  (let ((dm (and (boundp 'deactivate-mark) deactivate-mark)))
    (message "Refreshing quickpeek...")
    (quickpeek-update-contents)
    ;; Reset the timer in case it got really hosed for some reason...
    (quickpeek-set-timer quickpeek-update-speed)
    (message "Refreshing quickpeek...done")
    (if (boundp 'deactivate-mark) (setq deactivate-mark dm))))

(defun quickpeek-do-completion (newstring)
  "Insert NEWSTRING in place of the current symbol."
  (set-buffer (marker-buffer quickpeek-marker))
  (goto-char quickpeek-marker)
  (let ((b (quickpeek-thing-bounds)))
    (delete-region (car b) (cdr b))
    (insert newstring))
  (sit-for 0)
  (quickpeek-update-contents))

(defmacro quickpeek-with-alternate-syntax-table (&rest forms)
  "Execute  FORMS with the alternate syntax table specified in font-lock."
  (let ((ostvar (make-symbol "old-syntax-table")))
    `(let ((,ostvar (syntax-table)))
       (unwind-protect
	   (progn
	     (if (and (boundp 'font-lock-syntax-table)
		      font-lock-syntax-table)
		 (set-syntax-table font-lock-syntax-table))
	     ,@forms)
	 (set-syntax-table ,ostvar)))))
(put 'quickpeek-with-alternate-syntax-table 'lisp-indent-function 0)

(defun quickpeek-update-contents ()
  "Updated the contents of the `quickpeek' buffer."
  (interactive)
  (save-excursion
    (condition-case fu
	(quickpeek-with-alternate-syntax-table
	  (let ((info (save-match-data (quickpeek-collect-data))))
	    (save-excursion
	      (set-buffer quickpeek-buffer)
	      (quickpeek-with-writable
		(erase-buffer)
		(apply (car info) (cdr info))))))
      (error
       (save-excursion
	 (set-buffer quickpeek-buffer)
	 (quickpeek-with-writable
	   (erase-buffer)
	   (if fu
	       (quickpeek-error-form fu)
	     (quickpeek-goofy-form)))))))
  (quickpeek-set-mode-line-format))

;;; Data collection core
;;
(defun quickpeek-collect-data ()
  "Buffer `major-mode' specialized method for collecting data."
  (prog1
      (save-excursion (funcall quickpeek-info-function))
    (move-marker quickpeek-marker (point) (current-buffer))))

(defun quickpeek-default-info-function ()
  "Collect and return information about the current buffer."
  (list 'quickpeek-goofy-form
	))

(defmacro quickpeek-facep (face)
  "Return non-nil if FACE is a valid face."
  `(if (fboundp 'find-face) (find-face ,face) (facep ,face)))
	
(defun quickpeek-plain-string-insert (str)
  "Insert STR as a `quickpeek' string.
This string can be a string, or a list of insert specifiers.
An insert specifier will be of this form:
  (SPEC1 SPEC2 ...)
Where carriage returns are not allowed in any SPEC.  A spec can be
a another spec, or a cons cell of the form (TEXT . FACE) where FACE is
a valid or the symbol 'leave-faces, meaning to not strip faces off of
the text inserted."
  (cond ((stringp str)
	 (remove-text-properties 0 (length str) '(face) str)
	 (insert (quickpeek-cleanup-string str)))
	((listp str)
	 (cond ((and (stringp (car str)) (quickpeek-facep (cdr str)))
		(let ((p (point)))
		  (insert (quickpeek-cleanup-string (car str)))
		  (put-text-property p (point) 'face (cdr str))))
	       ((and (stringp (car str)) (eq 'leave-faces (cdr str)))
		(insert (quickpeek-cleanup-string (car str))))
	       (t
		(mapcar 'quickpeek-plain-string-insert str))))
	(t (error))))

(defun quickpeek-error-form (error-detail)
  "Display some error that occured.
Argument ERROR-DETAIL is the list returned in a `condition-case' variable."
  ;; Lot this puppy with message
  (message "%S" error-detail)
  ;; Stick it into the frame.
  (insert "An error occured in quickpeek while collecting data:\n"
	  (format "%S" error-detail)))

(defun quickpeek-goofy-form ()
  "Insert something strange into the `quickpeek' buffer."
  (yow 1)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward "\\<\\([A-Z]+\\)\\>" nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'bold))))

(defun quickpeek-simple-form (one two three)
  "Insert three lines of text into the `quickpeek' buffer.
Arguments ONE, TWO, and THREE are text lines to insert."
  (quickpeek-plain-string-insert one)
  (insert "\n")
  (quickpeek-plain-string-insert two)
  (insert "\n")
  (quickpeek-plain-string-insert three))

(defun quickpeek-insert-completions (completion-list)
  "Insert a COMPLETION-LIST on one line of a `quickpeek' buffer."
  (while completion-list
    (widget-create 'push-button
		   :value (car completion-list)
		   :notify (lambda (widget &rest ignore)
			     (quickpeek-do-completion
			      (widget-get widget :value)))
		   (car completion-list))
    (setq completion-list (cdr completion-list))
    (if completion-list
	(progn
	  (insert " ")
	  ;; 73 = 80 - (length " More") - 2 brackets.
	  (if (> (+ (current-column) (length (car completion-list))) 72)
	      (progn
		(apply 'widget-create 'menu-choice
		       :tag "More"
		       :format "%[[%t]%]"
		       :value (car completion-list)
		       :help-echo "Choose additional completions."
		       :notify (lambda (widget &rest ignore)
				 (quickpeek-do-completion
				  (widget-get widget :value)))
		       (mapcar (lambda (c)
				 (list 'choice-item c))
			       completion-list))
		(setq completion-list nil)))))))

(defun quickpeek-functional-form (top-level-sexp-summary
				  context-summary completion-list)
  "Insert details about a `quickpeek'ed buffer.
TOP-LEVEL-SEXP-SUMMARY is a summary for the top level sexp the
cursor is in (ie, the function name/params).
CONTEXT-SUMMARY is info about the current context.  (ei, definition of
the cursor variable or function).
COMPLETION-LIST is a list of possible completions for the current
context.  This would be completable function names or variables.  The
completion list is not due to be exhaustive, there are better methods
for that.  It is meant to be localized for local variables, methods,
or whatever is apropriate."
  (quickpeek-plain-string-insert top-level-sexp-summary)
  (insert "\n")
  (quickpeek-plain-string-insert context-summary)
  (insert "\n")
  (quickpeek-insert-completions completion-list))

(defun quickpeek-file-form (file completion-list)
  "Insert details about a `quickpeek'ed buffer.
FILE is the name of the file.
COMPLETION-LIST is a list of possible completions for the current
context.  This would be completable pragmas, or directives.  The
completion list is not due to be exhaustive, there are better methods
for that.  It is meant to be localized for local variables, methods,
or whatever is apropriate."
  (insert "File: " file "\n")
  (quickpeek-insert-completions completion-list))

;;; Handy utilities
;;
(eval-and-compile
  (condition-case nil
      (require 'thingatpt)
    (error (require 'thing))))

(defun quickpeek-thing-bounds (&optional type)
  "Return the bounds of the thing under point.
Optional TYPE can be something passed down to the thing functions
available under Emacs (thingatpt) or XEmacs (thing)."
  (if (not type) (setq type 'sexp))
  (if (featurep 'thingatpt)
      (bounds-of-thing-at-point type)
    ;; The xemacs version
    (let ((newsym (intern (concat "thing-" (symbol-name type)))))
      (funcall newsym (point)))))

(defun quickpeek-thing (&optional type)
  "Return the thing under point.
Optional TYPE can be something passed down to the thing functions
available under Emacs (thingatpt) or XEmacs (thing)."
  (let ((b (quickpeek-thing-bounds type)))
    (if b
	(buffer-substring-no-properties (car b) (cdr b))
      nil)))

(defun quickpeek-thing-beginning (&optional type)
  "Move to the beginning of the thing point is on."
  (let ((b (quickpeek-thing-bounds type)))
    (goto-char (car b))))

(defun quickpeek-cleanup-string (string)
  "Remove execess whitespace from STRING."
  (if (string-match "^\\s-+" string)
      (setq string (replace-match "" nil t string)))
  ;(if (string-match "\\s-+$" string)
  ;    (setq string (replace-match "" nil t string)))
  (while (string-match "\\s-\\s-+\\|\n" string)
    (setq string (replace-match " " nil t string)))
  string)

(defun quickpeek-beginning-of-defun ()
  "Move to the beginning of a defun using `up-list'."
  ;; This irritating combo works on multiple situations
  ;; and seems to be the best I can do at the moment.
  (beginning-of-line)
  (let ((p (point)))
    (condition-case nil
	(while t (up-list -1))
      (error))
    (if (= (point) p)
	;; In this case, we must assume beginning of buffer.
	(goto-char (point-min)))))

(defun quickpeek-in-non-code ()
  "Return t if the cursor is in non-code.  (ie; comment ot string)."
  (let* ((bod (save-excursion (quickpeek-beginning-of-defun) (point)))
	 (ps (parse-partial-sexp bod (point))))
    (or (nth 3 ps) (nth 4 ps) (nth 5 ps) (nth 7 ps))))

;;; Quick peek Tag handling
;;
(eval-when-compile (require 'etags))
(defun quickpeek-find-tag-stealthy (tag)
  "Return a dotted pair (BUFFER . POS) where TAG can be found.
Uses the tags table, but does not set the mark."
  (require 'etags)
  (if (string= tag "")
      (error "Empty reference tag in `quickpeek-find-tag-stealthy'!"))
  (if (not quickpeek-use-tags)
      nil
    (if (fboundp 'find-tag-internal)
	;; XEmacs has this convenient function
	(find-tag-internal tag)
      ;; the following code has been mostly stolen from etags.el
      ;; in order to support returning a tag value.
      (save-excursion
	(let ((order-preds '(tag-exact-file-name-match-p
			     tag-exact-match-p
			     tag-symbol-match-p
			     tag-word-match-p
			     tag-any-match-p))
	      (inhibit-quit nil)
	      (first t)
	      order file tag-info goto-func
	      buff line
	      ;; Override this variable so we can change it
	      ;; by loading an scanning alternate tags tables
	      ;; without changing the user selected table.
	      (tags-file-name tags-file-name)
	      tl
	      (ltf (concat default-directory "TAGS"))
	      (ret nil))
	  (if (and (file-exists-p ltf) (not (member ltf tl)))
	      (visit-tags-table ltf))
	  (setq tl tags-table-list)
	  (while (and (not ret) tl)
	    (visit-tags-table-buffer (car tl))
	    (catch 'qualified-match-found
	      (while (or first (visit-tags-table-buffer t))
		(if first (progn (goto-char (point-min))
				 (setq first nil)))
		(setq order order-preds)
		(while order
		  (while (search-forward tag nil t)
		    (and (funcall (car order) tag)
			 (throw 'qualified-match-found nil)))
		  (setq order (cdr order))
		  (goto-char (point-min))))
	      ;; We only get here if there was no match.  Yikes!
	      ;; Force caller of us to use condition case for now.
	      (error "No tag found"))
	    ;; Found a tag.  Extract it.
	    (beginning-of-line)
	    ;; Expand the filename, use the tags table buffer's default-directory.
	    (setq file (expand-file-name (file-of-tag))
		  tag-info (etags-snarf-tag))
	    ;; Get the local value in the tags table buffer before switching.
	    (setq goto-func goto-tag-location-function)
	    ;; Find the right line in the specified file.
	    (setq buff (set-buffer (find-file-noselect file)))
	    ;; go there.
	    (save-excursion (funcall goto-func tag-info)
			    (setq line (point)))
	    ;; Return this junk
	    (setq ret (cons buff line))
	    (setq tl (cdr tl)))
	  ret)))))

(defun quickpeek-tags-completion (word)
  "Perform completion of WORD using tags tables."
  (if (not quickpeek-use-tags)
      nil
    (require 'etags)
    (cond ((boundp 'tags-complete-tag)
	   ;; Expand this as necessary since this is still somewhat incomplete.
	   (tags-complete-tag word nil t))
	  ((boundp 'find-tag-tag)
	   ;; XEmacs has this function which contains within it a function
	   ;; resembling this:
	   (all-completions word tag-completion-table nil))
	  (t
	   ;; There appears to be no convenience function for XEmacs.  Shame!
	   nil))))

;;; Temporary hack to get everything rolling.
(require 'qp-elisp)
(require 'qp-c)

(provide 'quickpeek)

;;; quickpeek.el ends here

