;;; e-config - configureation for emacs
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: e-config.el,v 1.4 1996/11/10 14:16:03 zappo Exp $
;;; Keywords: OO, dialog, configure
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;      
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;      
;;; e-config can be found in the eieio or etalk distributions on:
;;;  ftp://ftp.ultranet.com/pub/zappo
;;;
;;; Commentary:
;;;   Uses dlg-config to edit options for the emacs editor.  Because
;;; large dialogs slow down the system, each individual dialog will
;;; be as small as possible.  Each function mearly creates on dialog
;;; buffer.  See dlg-config or dialog for details on how a dialog works.
;;; These functions can be considered only a screen definitions.
;;;           
(require 'dlg-config)


;;;
;;; General Interface Options
;;;
(defun econfig-interface ()
  "Creates a configure window with variables modifying the visual interface
for emacs."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group (create-widget "Interface Options" widget-frame)
    (create-widget "linenumber" widget-toggle-button
		   :label-value "Display Line Number in Modeline"
		   :state (data-object-symbol "line-number-mode"
					      :symbol 'line-number-mode))
    (create-widget "colnumber" widget-toggle-button
		   :label-value "Display Column Number in Modeline"
		   :state (data-object-symbol "column-number-mode"
					      :symbol 'column-number-mode))
    (create-widget "truncatelines" widget-toggle-button
		   :label-value "Truncate Lines"
		   :state (data-object-symbol-default "truncate-lines"
						      :symbol 'truncate-lines))
    (create-widget "suggkey" widget-toggle-button
		   :label-value "Suggest Key Bindings"
		   :state (data-object-symbol "Suggest Key Bindings"
					      :symbol 'suggest-key-bindings))
    (create-widget "visiblebel" widget-toggle-button
		   :label-value "Visible Bell"
		   :state (data-object-symbol "visible-bell"
					      :symbol 'visible-bell))
    (create-widget "inversedisplay" widget-toggle-button
		   :label-value "Inverse Video"
		   :state (data-object-symbol "inverse-video"
					      :symbol 'inverse-video))
    (create-widget "mdlineinverse" widget-toggle-button
		   :label-value "Modeline Inverse Video"
		   :state (data-object-symbol "mode-line-inverse-video"
					      :symbol 'mode-line-inverse-video))
    (create-widget "srchhighlight" widget-toggle-button
		   :label-value "Search Highlights Current Match"
		   :state (data-object-symbol "search-highlight"
					      :symbol 'search-highlight))
    (create-widget "qrhighlight" widget-toggle-button
		   :label-value "Query Replace Highlight"
		   :state (data-object-symbol "query-replace-highlight"
					      :symbol 'query-replace-highlight))
    (create-widget "recursmini" widget-toggle-button
		   :label-value "Enable Recursive Minibuffers"
		   :state (data-object-symbol "enable-recursive-minibuffers"
					      :symbol 'enable-recursive-minibuffers))
    (create-widget "Scroll Step:" widget-labeled-text
		   :unit "lines" :text-length 10 
		   :value (data-object-symbol-string-to-int 
			   "scroll-step" :symbol 'scroll-step))
    )
  (dialog-build-group (create-widget "Startup Commands" widget-frame)

    (create-widget "display-time" widget-toggle-button
		   :label-value "Display time in modeline"
		   :state (data-object-command-option
			   "display-time"
			   :command "(display-time)"))
    (create-widget "type-break" widget-toggle-button
		   :label-value "Typing break mode (remind you to rest)"
		   :state (data-object-symbol "type-break"
					      :symbol 'type-break-mode))
    (create-widget "paren" widget-toggle-button
		   :label-value "Highlight Parenthesis"
		   :state (data-object-symbol-feature
			   "paren"
			   :symbol 'paren
			   :unload-commands 
			   (if (> emacs-minor-version 34)
			       '(show-paren-mode -1) nil)
			   ))
    (create-widget "autoshow" widget-toggle-button
		   :label-value "Automatically Scroll Horizontally"
		   :state (data-object-symbol-feature
			   "auto-show"
			   :symbol 'auto-show
			   :unload-commands '(auto-show-mode -1)))
    (create-widget "resizemini" widget-toggle-button
		   :label-value "Magically Resize Minibuffer when needed."
		   :state (data-object-symbol-feature
			   "rsz-mini"
			   :symbol 'rsz-mini
			   :unload-commands '(progn
					       (remove-hook 'minibuffer-setup-hook
							    'resize-minibuffer-setup)
					       (resize-minibuffer-mode -1))))
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Editing Options
;;;
(defun econfig-editing ()
  "Creates a configure window with variables modifying the editing interface
for emacs."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group (create-widget "Behavior Options" widget-frame)

    (create-widget "Mouse Yanks to Cursor (not mouse)" widget-toggle-button
		   :state (data-object-symbol "mouse-yank-at-point"
					      :symbol 'mouse-yank-at-point))
    (create-widget "Next-line adds newline at end of buffer"
		   widget-toggle-button
		   :state (data-object-symbol "next-line-add-newlines"
					      :symbol 'next-line-add-newlines))
    (create-widget "Adaptive Fill Mode" widget-toggle-button
		   :state (data-object-symbol "adaptive-fill-mode"
					      :symbol 'adaptive-fill-mode))
    (create-widget "Require Final Newline" widget-toggle-button
		   :state (data-object-symbol "require-final-newline"
					      :symbol 'require-final-newline))

    (create-widget "Auto fill in all text modes" widget-toggle-button
		   :state (data-object-symbol-hook
			   "auto-fill"
			   :symbol 'text-mode-hook
			   :command "turn-on-auto-fill"))
    )
  (dialog-build-group (create-widget "Suggested Disabled Commands" widget-frame)

    (create-widget "Evaluate Expression  < M-: >" widget-toggle-button
		   :state (data-object-symbol-disabled
			   "eval-expression" :symbol 'eval-expression))
    (create-widget "Narrow to region     < C-x n n >" widget-toggle-button
		   :state (data-object-symbol-disabled
			   "eval-expression" :symbol 'eval-expression))
    (create-widget "Set Goal Column      < C-x C-n >" widget-toggle-button
		   :state (data-object-symbol-disabled
			   "set-goal-column" :symbol 'set-goal-column))
    (create-widget "Erase buffer         < M-x erase-buffer >" widget-toggle-button
		   :state (data-object-symbol-disabled
			   "erase-buffer" :symbol 'erase-buffer))
    )
  (dialog-build-group (create-widget "Backup File Methods" widget-frame)

    (create-widget "By Copying (off uses move)" widget-toggle-button
		   :state (data-object-symbol "by-copying"
					      :symbol 'backup-by-copying))
    (create-widget "By Copying When Linked" widget-toggle-button
		   :state (data-object-symbol 
			   "by-copying-when-linked"
			   :symbol 'backup-by-copying-when-linked))
    (create-widget "By Copying When Owner/Group Mismatch " widget-toggle-button
		   :state (data-object-symbol
			   "by-copying-when-mismatch"
			   :symbol 'backup-by-copying-when-mismatch))
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Programming Options
;;;
(defun econfig-programmer ()
  "Creates a configure window with variables modifying variables
useful for programmers."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group (create-widget "Program Editing Options" widget-frame)

    (create-widget "Enable Local Variables" widget-toggle-button
		   :state (data-object-symbol "enable-local-variables"
					      :symbol 'enable-local-variables))

    (create-widget "Check for copyright update" widget-toggle-button
		   :state (data-object-command-option
			   "copyright"
			   ;; Override the default.. this is more dependable
			   :value (member 'copyright-update write-file-hooks)
			   :command "(load-library \"copyright\")(add-hook 'write-file-hooks 'copyright-update)"
			   :disable-command "(remove-hook 'write-file-hooks 'copyright-update"))
    ;; compile stuff
    (require 'compile)

    (create-widget "Compile Finish Command:" widget-labeled-text
		   :text-length 50
		   :value (data-object-symbol-lisp-expression
			   "compilation-finish-command"
			   :symbol 'compilation-finish-function))

    (create-widget "Compile Command       :"widget-labeled-text
		   :text-length 50
		   :value (data-object-symbol "compile-command"
					      :symbol 'compile-command))

    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Mail configurations
;;;
(defun econfig-mail-showfrom (style)
  "Return a string which is how the mail address would be shown"
  (cond ((or (eq style 2) (eq style 'angles))
	 (concat (user-full-name) " <" user-mail-address ">"))
	((or (eq style 1) (eq style 'parens))
	 (concat user-mail-address " (" (user-full-name) ")"))
	(t user-mail-address)))

(defun econfig-mail ()
  "Creates a configure window with variables modifying variables
useful for sending email."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'rmail)
  (dialog-build-group (create-widget "Rmail Options" widget-frame)

    (create-widget "Rmail File               :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol "rmail-file-name"
					      :symbol 'rmail-file-name))
    
    (create-widget "Secondary File Directory :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol
			   "rmail-secondary-file-directory"
			   :symbol 'rmail-secondary-file-directory))
    
    (create-widget "Default Secondary File   :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol
			   "rmail-default-rmail-file"
			   :symbol 'rmail-default-rmail-file))
    
    (create-widget "Delete messages after saving to secondary file"
		   widget-toggle-button
		   :state (data-object-symbol
			   "rmail-delete-after-output"
			   :symbol 'rmail-delete-after-output))

    (create-widget "Summary motion scrolls messages" widget-toggle-button
		   :state (data-object-symbol
			   "rmail-summary-scroll-between-messages"
			   :symbol 'rmail-summary-scroll-between-messages))

    )
  (require 'sendmail)
  (dialog-build-group (create-widget "Sending Mail Options" widget-frame)
    
    (let* ((uma (data-object-symbol "mail-addresss"
				    :symbol 'user-mail-address))
	   (opt-list '("nil" "'parens" "'angles"))
	   (fsdo (data-object-symbol-list-index
		  "from-style"
		  :symbol 'mail-from-style
		  :value (cond ((eq mail-from-style 'angles) 2)
			       ((eq mail-from-style 'parens) 1)
			       (t 0))
		  :string-list opt-list))
	   (emdo (data-object "example-mail-name" 
			      :value (econfig-mail-showfrom mail-from-style))))

      (create-widget "Mail Address   :" widget-labeled-text :text-length 50
		     :value uma)
      
      (create-widget "Reply-to       :" widget-labeled-text :text-length 50
		     :value (data-object-symbol "reply-to"
						:symbol 'mail-default-reply-to))
    
      (create-widget "From Style     :" widget-label)

      (create-widget "name-type" widget-option-button
		     :x -2 :y t :option-list opt-list :state fsdo)

      (create-widget "Looks Like:" widget-label :y -1)
      (create-widget "example-label" widget-label :x -2 :y t 
		     ;; set max width
		     :width (+ 3 (length (user-full-name))
			       (length user-mail-address))
		     :justification 'left
		     :face 'bold
		     :label-value emdo)

      ;; This translates the address from one type to the other
      (create-widget "address-translator" widget-gadget-translator
		     :watch fsdo :change emdo
		     :translate-function 
		     (lambda (a b) 
		       (set-value b (econfig-mail-showfrom (get-value a)) 
				  this)))
      (create-widget "address-translator" widget-gadget-translator
		     :watch uma :change emdo
		     :translate-function 
		     (lambda (a b) 
		       (let ((user-mail-address (get-value a)))
			 (set-value b (econfig-mail-showfrom mail-from-style)
				    this))))
      )

    (create-widget "Signature File :" widget-labeled-text :text-length 50
		   :value (data-object-symbol "sig-file"
					      :symbol 'mail-signature-file))
    
    (create-widget "Auto load signature file" widget-toggle-button
		   :state (data-object-symbol "mail-signature"
					      :symbol 'mail-signature))
    (create-widget  "Spellcheck outbound messages" widget-toggle-button
		    :state (data-object-symbol-hook
			    "spellcheck"
			    :symbol 'mail-send-hook
			    :command "(lambda () (if (y-or-n-p \"Spell message?\") (ispell-message)))"
			    ))

    (create-widget "Citation Prefix:" widget-labeled-text :text-length 10
		   :value (data-object-symbol "cite-pref"
					      :symbol 'mail-yank-prefix))
    (create-widget "cite-pref" widget-label
		   :x 3 :y -1 :label-value
		   "This appears before quoted text. Usually `>'")

    )
  (dlg-end)
  (dialog-refresh)
  )


;;;
;;; Calendar/diary options
;;;
(defun econfig-calendar ()
  "Creates a configure window with variables modifying variables
useful calendar mode."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group (create-widget "Calendar Options" widget-frame)

    (create-widget "Latitude :" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   "calendar-latitude"
			   :symbol 'calendar-latitude))

    (create-widget "Longitude:" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   "calendar-longitude"
			   :symbol 'calendar-longitude))


    )
  (dialog-build-group (create-widget "Holiday Options" widget-frame)

    (create-widget "Show all Christian Holidays" widget-toggle-button
		   :state (data-object-symbol 
			   "all-christian-calendar-holidays"
			   :symbol 'all-christian-calendar-holidays))

    (create-widget "Show all Hebrew Holidays" widget-toggle-button
		   :state (data-object-symbol
			   "all-hebrew-calendar-holidays"
			   :symbol 'all-hebrew-calendar-holidays))

    (create-widget "Show all Islamic Holidays" widget-toggle-button
		   :state (data-object-symbol
			   "all-islamic-calendar-holidays"
			   :symbol 'all-islamic-calendar-holidays))

    (create-widget "Show Holidays at Startup" widget-toggle-button
		   :state (data-object-symbol 
			   "view-calendar-holidays-initially"
			   :symbol 'view-calendar-holidays-initially))

    )
  (dialog-build-group (create-widget "Appointment Options" widget-frame)

    (create-widget "Diary File:" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol "diaryfile"
					      :symbol 'diary-file))

    (create-widget "Warn of impending appointments" widget-toggle-button
		   :state (data-object-symbol-hook
			   "diary-hook"
			   :symbol 'diary-hook
			   :command "appt-make-list"))
    (create-widget "Appointments Audible" widget-toggle-button
		   :state (data-object-symbol "appt-audible"
					      :symbol 'appt-audible))
    (create-widget "Appointments Displayed" widget-toggle-button
		   :state (data-object-symbol "apt-display-diary"
					      :symbol 'appt-display-diary))
    (create-widget "Display Appointment Time in Modeline" widget-toggle-button
		   :state (data-object-symbol "apt-display-mode-line"
					      :symbol 'appt-display-mode-line))
    (create-widget "Show Appointments at Startup" widget-toggle-button
		   :state (data-object-symbol "view-diary-entries-initially"
					      :symbol 'view-diary-entries-initially))
    
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; ps print options
;;;
(defun econfig-ps-print ()
  "Creates a configure window with variables modifying variables
useful for ps-print."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'ps-print)
  (dialog-build-group (create-widget "Postscript Printing Options" widget-frame)

    (create-widget "Print header on each page." widget-toggle-button
		   :state (data-object-symbol "ps-print-header"
					      :symbol 'ps-print-header))
     
    (create-widget "Print gaudy frame around header." widget-toggle-button
		   :state (data-object-symbol "ps-print-header-frame"
					      :symbol 'ps-print-header-frame))
    
    (create-widget "Print with color." widget-toggle-button
		   :state (data-object-symbol "ps-print-color-p"
					      :symbol 'ps-print-color-p))

    (create-widget "Printed Font Size:" widget-labeled-text
		   :unit "Pts" :text-length 5
		   :value (data-object-symbol-string-to-int "fontds-size"
					      :symbol 'ps-font-size))

    (create-widget "Print command    :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol "ps-lpr-command"
					      :symbol 'ps-lpr-command))
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Font locking options
;;;
(defun econfig-flock-options ()
  "Creates a configure window with variables modifying how font lock is used."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group (create-widget "Font Lock Options" widget-frame)

    (create-widget  "Always activate font-lock" widget-toggle-button
		    :state 
		    (if (or (> emacs-major-version 19)
			    (> emacs-minor-version 31))
			(data-object-command-option
			 "font-lock"
			 :command "(global-font-lock-mode t)"
			 :disable-command "(global-font-lock-mode nil)")
		      (data-object-symbol-hook
		       "font-lock"
		       :symbol 'find-file-hooks
		       :command "turn-on-font-lock")))

    (create-widget "Always use maximum decoration" widget-toggle-button
		   :state (data-object-symbol 
			   "font-lock-maximum-decoration"
			   :symbol 'font-lock-maximum-decoration))

    (dialog-build-group (create-widget "Font Lock Enhancers" widget-radio-frame
				       ;; In this situation, lets
				       ;; turn off some of the sides, and
				       ;; it will behave like a separator
				       :box-sides [nil nil t nil]
				       :position 'top-right
				       :x 0
				       :state
				       (data-object-symbol-list-index
					"lazy-or-fast"
					:symbol 'font-lock-support-mode
					:string-list 
					'("nil" "'lazy-lock-mode"
					  "'fast-lock-mode")
					:value 
					(cond 
					 ((eq mail-from-style 'fast-lock-mode)
					  2)
					 ((eq mail-from-style 'lazy-lock-mode)
					  1)
					 (t 0))))

      (create-widget "Use Nothing (No locking enhancers)" widget-radio-button)

      (create-widget "Use Lazy-Lock (deferred font locking)" widget-radio-button)

      (create-widget "Use Fast-Lock (cached font lock info)" widget-radio-button)

      ))
  (require 'lazy-lock)
  (dialog-build-group (create-widget "Lazy Lock Options" widget-frame)

    (create-widget "Defer Time   :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   "defer-time"
			   :symbol 'lazy-lock-defer-time))
    
    (create-widget "Stealth Mode verbosity" widget-toggle-button
		   :state (data-object-symbol
			   "lazy-lock-stealth-verbose"
			   :symbol 'lazy-lock-stealth-verbose))

    (create-widget "Stealth Time :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   "stealth-time"
			   :symbol 'lazy-lock-stealth-time))

    (create-widget "Stealth Lines:" widget-labeled-text
		   :text-length 5 :unit "Lines"
		   :value (data-object-symbol-string-to-int 
			   "stealth-lines"
			   :symbol 'lazy-lock-stealth-lines))
    )
  (require 'fast-lock)
  (dialog-build-group (create-widget "Fast Lock Options" widget-frame)

    (create-widget  "Save font cache for files belonging to others" 
		    widget-toggle-button
		    :state (data-object-symbol "fast-lock-save-others"
					       :symbol 'fast-lock-save-others))
    )
  (dlg-end)
  (dialog-refresh)
  )

(defun econfig-font-lock-faces ()
  "Edit list of font lock used faces"
  (interactive)
  (require 'font-lock)
  (dlg-faces '(font-lock-comment-face
	       font-lock-function-name-face
	       font-lock-string-face
	       font-lock-keyword-face
	       font-lock-reference-face
	       font-lock-variable-name-face
	       font-lock-type-face))
  )

(defun econfig-calendar-faces ()
  "Edit list of faces associated with the calendar and diary"
  (interactive)
  (require 'calendar)
  (dlg-faces '(calendar-today-face
	       diary-face
	       holiday-face)))

(defun econfig-info-faces ()
  "Edit list of faces associated with INFO"
  (interactive)
  (if (not (featurep 'info))
      (error "You must use info (C-h i) before it's faces are available."))
  (dlg-faces '(info-node
	       info-xref
	       info-menu-5)))

;;; end of lisp
(provide 'e-config)
