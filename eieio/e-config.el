;;; e-config - configureation for emacs
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: e-config.el,v 1.1 1996/09/21 16:03:20 zappo Exp $
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
  (dlg-init)
  (let ((oframe (create-widget "Interface Options" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Interface Options"))
	)
    (create-widget "linenumber" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Display Line Number in Modeline"
		   :state (data-object-symbol "line-number-mode"
					      :value line-number-mode
					      :symbol 'line-number-mode))
    (create-widget "colnumber" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Display Column Number in Modeline"
		   :state (data-object-symbol "column-number-mode"
					      :value column-number-mode
					      :symbol 'column-number-mode))
    (create-widget "truncatelines" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Truncate Lines"
		   :state (data-object-symbol-default "truncate-lines"
						      :value truncate-lines
						      :symbol 'truncate-lines))
    (create-widget "suggkey" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Suggest Key Bindings"
		   :state (data-object-symbol "Suggest Key Bindings"
					      :value suggest-key-bindings
					      :symbol 'suggest-key-bindings))
    (create-widget "visiblebel" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Visible Bell"
		   :state (data-object-symbol "visible-bell"
					      :value visible-bell
					      :symbol 'visible-bell))
    (create-widget "inversedisplay" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Inverse Video"
		   :state (data-object-symbol "inverse-video"
					      :value inverse-video
					      :symbol 'inverse-video))
    (create-widget "mdlineinverse" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Modeline Inverse Video"
		   :state (data-object-symbol "mode-line-inverse-video"
					      :value mode-line-inverse-video
					      :symbol 'mode-line-inverse-video))
    (create-widget "srchhighlight" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Search Highlights Current Match"
		   :state (data-object-symbol "search-highlight"
					      :value search-highlight
					      :symbol 'search-highlight))
    (create-widget "qrhighlight" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Query Replace Highlight"
		   :state (data-object-symbol "query-replace-highlight"
					      :value query-replace-highlight
					      :symbol 'query-replace-highlight))
    (create-widget "recursmini" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Enable Recursive Minibuffers"
		   :state (data-object-symbol "enable-recursive-minibuffers"
					      :value enable-recursive-minibuffers
					      :symbol 'enable-recursive-minibuffers))
    (create-widget "scroll-step" widget-label oframe
		   :x 1 :y -1 :label-value "Scroll Step:")
    (create-widget "scroll-step" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int "scroll-step"
					      :symbol 'scroll-step
					      :value (int-to-string scroll-step)))
    )
  (let ((oframe (create-widget "Toggle Frame startup commands" widget-frame 
			       widget-toplevel-shell
			       :x 1 :y -3
			       :frame-label "Startup Commands"))
	)

    (create-widget "display-time" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Display time in modeline"
		   :state (data-object-command-option
			   "display-time"
			   :value (dlg-quick-find "^\\s-*(display-time)" 
						  dlg-config-file)
			   :command "(display-time)"))
    (create-widget "type-break" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Typing break mode (remind you to rest)"
		   :state (data-object-symbol
			   "type-break"
			   :value type-break-mode
			   :symbol 'type-break-mode))
    (create-widget "paren" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Highlight Parenthesis"
		   :state (data-object-symbol-feature
			   "paren"
			   :value (featurep 'paren)
			   :symbol 'paren
			   ;;:unload-commands '(show-paren-mode -1)
			   :unload-commands nil
			   ))
    (create-widget "autoshow" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Auto Change Horizontal View"
		   :state (data-object-symbol-feature
			   "auto-show"
			   :value (featurep 'auto-show)
			   :symbol 'auto-show
			   :unload-commands '(auto-show-mode -1)))
    (create-widget "resizemini" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Magically Resize Minibuffer"
		   :state (data-object-symbol-feature
			   "rsz-mini"
			   :value (featurep 'rsz-mini)
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
  (dlg-init)
  (let ((oframe (create-widget "Toggle Frame buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Behavior Options"))
	)
    (create-widget "mouseyank" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Mouse Yanks to Cursor (not mouse)"
		   :state (data-object-symbol "mouse-yank-at-point"
					      :value mouse-yank-at-point
					      :symbol 'mouse-yank-at-point))
    (create-widget "nxtnewline" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Next-line adds newline at end of buffer"
		   :state (data-object-symbol "next-line-add-newlines"
					      :value next-line-add-newlines
					      :symbol 'next-line-add-newlines))
    (create-widget "adaptfill" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Adaptive Fill Mode"
		   :state (data-object-symbol "adaptive-fill-mode"
					      :value adaptive-fill-mode
					      :symbol 'adaptive-fill-mode))
    (create-widget "newline" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Require Final Newline"
		   :state (data-object-symbol "require-final-newline"
					      :value require-final-newline
					      :symbol 'require-final-newline))
    (create-widget "autofilltxt" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Auto fill in all text modes"
		   :state (data-object-symbol-hook
			   "auto-fill"
			   :value (member 'turn-on-auto-fill text-mode-hook)
			   :symbol 'text-mode-hook
			   :command "turn-on-auto-fill"))
    )
  (let ((oframe (create-widget "Toggle Frame disabled functions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Suggested Disabled Commands"))
	)
    (create-widget "eval-expression" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Evaluate Expression < M-: >"
		   :state (data-object-symbol-disabled
			   "eval-expression"
			   :value (get 'eval-expression 'disabled)
			   :symbol 'eval-expression))
    (create-widget "narrow-to-region" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Narrow to region   < C-x n n >"
		   :state (data-object-symbol-disabled
			   "eval-expression"
			   :value (get 'narrow-to-region 'disabled)
			   :symbol 'eval-expression))
    (create-widget "set-goal-column" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Set Goal Column    < C-x C-n >"
		   :state (data-object-symbol-disabled
			   "set-goal-column"
			   :value (get 'set-goal-column 'disabled)
			   :symbol 'set-goal-column))
    (create-widget "erase-buffer" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Erase buffer       < M-x erase-buffer >"
		   :state (data-object-symbol-disabled
			   "erase-buffer"
			   :value (get 'erase-buffer 'disabled)
			   :symbol 'erase-buffer))
    )
  (let ((oframe (create-widget "Toggle Frame backup functions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Backup File Methods"))
	)
    (create-widget "by-copying" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "By Copying (off uses move)"
		   :state (data-object-symbol
			   "by-copying"
			   :value backup-by-copying
			   :symbol 'backup-by-copying))
    (create-widget "by-copying-when-linked" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "By Copying When Linked"
		   :state (data-object-symbol
			   "by-copying-when-linked"
			   :value backup-by-copying-when-linked
			   :symbol 'backup-by-copying-when-linked))
    (create-widget "by-copying-when-mismatch" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "By Copying When Owner/Group Mismatch"
		   :state (data-object-symbol
			   "by-copying-when-mismatch"
			   :value backup-by-copying-when-mismatch
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
  (dlg-init)
  (let ((oframe (create-widget "Toggle Frame buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Program Editing Options"))
	)
    (create-widget "localvar" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Enable Local Variables"
		   :state (data-object-symbol "enable-local-variables"
					      :value enable-local-variables
					      :symbol 'enable-local-variables))


    (create-widget "upd-copyright" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Check for copyright update"
		   :state (data-object-command-option
			   "copyright"
			   :value (member 'copyright-update write-file-hooks)
			   :command "(load-library \"copyright\")(add-hook 'write-file-hooks 'copyright-update)"
			   :disable-command "(remove-hook 'write-file-hooks 'copyright-update"))
    ;; compile stuff
    (require 'compile)

    (create-widget "compile-finish" widget-label oframe
		   :x 1 :y -1 :label-value "Compile Finish Command:")
    (create-widget "compile-command" widget-text-field oframe
		   :width 40 :height 1 :x -2 :y t 
		   :value (data-object-symbol-lisp-expression
			   "compilation-finish-command"
			   :symbol 'compilation-finish-function
			   :value
			   (format "%S" compilation-finish-function)))

    (create-widget "compile-label" widget-label oframe
		   :x 1 :y -1 :label-value "Compile Command       :")
    (create-widget "compile-command" widget-text-field oframe
		   :width 40 :height 1 :x -2 :y t 
		   :value (data-object-symbol "compile-command"
					      :symbol 'compile-command
					      :value
					      compile-command))
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Mail configurations
;;;
(defun econfig-mail ()
  "Creates a configure window with variables modifying variables
useful for sending email."
  (interactive)
  (dlg-init)
  (require 'rmail)
  (let ((oframe (create-widget "rmail options" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Rmail Options"))
	)
    (create-widget "rmail-file-name" widget-label oframe
		   :x 1 :y 1 :label-value "Rmail File               :")
    (create-widget "rmail-file-name-txt" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "rmail-file-name"
			   :symbol 'rmail-file-name
			   :value rmail-file-name))
    
    (create-widget "rmail-secondary-file-directory" widget-label oframe
		   :x 1 :y -1 :label-value "Secondary File Directory :")
    (create-widget "rmail-secondary-file-directory-txt" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "rmail-secondary-file-directory"
			   :symbol 'rmail-secondary-file-directory
			   :value rmail-secondary-file-directory))
    
    (create-widget "rmail-default-rmail-file" widget-label oframe
		   :x 1 :y -1 :label-value "Default Secondary File   :")
    (create-widget "rmail-default-rmail-file" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "rmail-default-rmail-file"
			   :symbol 'rmail-default-rmail-file
			   :value rmail-default-rmail-file))
    
    (create-widget "rmail-delete-after-output" widget-toggle-button oframe
		   :x 1 :y -2 :label-value "Delete messages after saving to secondary file"
		   :state (data-object-symbol
			   "rmail-delete-after-output"
			   :value rmail-delete-after-output
			   :symbol 'rmail-delete-after-output))

    (create-widget "rmail-summary-scroll-between-messages:" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Summary motion scrolls messages"
		   :state (data-object-symbol
			   "rmail-summary-scroll-between-messages"
			   :value rmail-summary-scroll-between-messages
			   :symbol 'rmail-summary-scroll-between-messages))

    )
  (require 'sendmail)
  (let ((oframe (create-widget "mail options" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Sending Mail Options"))
	)
    (create-widget "user-mail-address" widget-label oframe
		   :x 1 :y 1 :label-value "Mail Address   :")
    (create-widget "user-mail-address-txt" widget-text-field oframe
		   :width 30 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "mail-addresss"
			   :symbol 'user-mail-address
			   :value user-mail-address))

    (create-widget "default-reply-to" widget-label oframe
		   :x 1 :y -1 :label-value "Reply-to       :")
    (create-widget "default-reply-t" widget-text-field oframe
		   :width 30 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "reply-to"
			   :symbol 'mail-default-reply-to
			   :value mail-default-reply-to))
    
    (let ((opt-list '("nil" "'parens" "'angles")))
      (create-widget "name type" widget-label oframe
		     :x 1 :y -2 :height 1 :label-value "From Style     :")
      (create-widget "name-type" widget-option-button oframe
		     :x -2 :y t :option-list opt-list
		     :state (data-object-symbol-list-index
			     "from-style"
			     :symbol 'mail-from-style
			     :value (cond ((eq mail-from-style 'angles) 2)
					  ((eq mail-from-style 'parens) 1)
					  (t 0))
			     :string-list opt-list)))

    (create-widget "sig-file" widget-label oframe
		   :x 1 :y -3 :label-value "Signature File :")
    (create-widget "sig-file-t" widget-text-field oframe
		   :width 30 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "sig-file"
			   :symbol 'mail-signature-file
			   :value mail-signature-file))
    
    (create-widget "mail-signature" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Auto load signature file"
		   :state (data-object-symbol
			   "mail-signature"
			   :value mail-signature
			   :symbol 'mail-signature))
    (create-widget "spellcheck" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Spellcheck outbound messages"
		       :state (data-object-symbol-hook
			       "spellcheck"
			       :value (member '(lambda ()
						 (if (y-or-n-p "Spell message?")
						     (ispell-message)))
					      mail-send-hook)
			       :symbol 'mail-send-hook
			       :command "(lambda () (if (y-or-n-p \"Spell message?\") (ispell-message)))"
			       ))


    (create-widget "cite-pref" widget-label oframe
		   :x 1 :y -2 :label-value "Citation Prefix:")
    (create-widget "cite-pref-t" widget-text-field oframe
		   :width 30 :height 1 :x -2 :y t 
		   :value (data-object-symbol
			   "cite-pref"
			   :symbol 'mail-yank-prefix
			   :value mail-yank-prefix))
    (create-widget "cite-pref" widget-label oframe
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
  (dlg-init)
  (let ((oframe (create-widget "Toggle Frame caloptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Calendar Options"))
	)
    (create-widget "cal-lat" widget-label oframe
		   :x 1 :y 1 :label-value "Latitude :")
    (create-widget "cal-lat-txt" widget-text-field oframe
		   :width 10 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int "calendar-latitude"
					      :symbol 'calendar-latitude
					      :value 
					      (if (numberp calendar-latitude)
						  (int-to-string calendar-latitude)
						nil)))
    (create-widget "cal-lat-unit" widget-label oframe
		   :x -3 :y t :label-value "Degrees")
    
    (create-widget "cal-lon" widget-label oframe
		   :x 1 :y -1 :label-value "Longitude:")
    (create-widget "cal-lat-txt" widget-text-field oframe
		   :width 10 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int "longitude"
					      :symbol 'calendar-longitude
					      :value 
					      (if (numberp calendar-longitude)
						  (int-to-string calendar-longitude)
						nil)))
    
    (create-widget "cal-lon-unit" widget-label oframe
		   :x -3 :y t :label-value "Degrees")
    )
  (let ((oframe (create-widget "Toggle Frame holoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Holiday Options"))
	)
    (create-widget "christian" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Show all Christian Holidays"
		   :state (data-object-symbol "all-christian-calendar-holidays"
					      :value all-christian-calendar-holidays
					      :symbol 'all-christian-calendar-holidays))

    (create-widget "hebrew" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Show all Hebrew Holidays"
		   :state (data-object-symbol "all-hebrew-calendar-holidays"
					      :value all-hebrew-calendar-holidays
					      :symbol 'all-hebrew-calendar-holidays))

    (create-widget "islamic" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Show all Islamic Holidays"
		   :state (data-object-symbol "all-islamic-calendar-holidays"
					      :value all-islamic-calendar-holidays
					      :symbol 'all-islamic-calendar-holidays))

    (create-widget "showhol" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Show Holidays at Startup"
		   :state (data-object-symbol "view-calendar-holidays-initially"
					      :value view-calendar-holidays-initially
					      :symbol 'view-calendar-holidays-initially))

    )
  (let ((oframe (create-widget "Toggle Frame apptoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Appointment Options"))
	)
    (create-widget "diaryfile" widget-label oframe
		   :x 1 :y 1 :label-value "Diary File:")
    (create-widget "diaryfile-txt" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol "diaryfile"
					      :symbol 'diary-file
					      :value diary-file))
    (create-widget "do-appointments" widget-toggle-button oframe
		   :x 1 :y -2 :label-value "Warn of impending appointments"
		   :state (data-object-symbol-hook
			   "diary-hook"
			   :value (member 'appt-make-list  diary-hook)
			   :symbol 'diary-hook
			   :command "appt-make-list"))
    (create-widget "noisy" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Appointments Audible"
		   :state (data-object-symbol "appt-audible"
					      :value appt-audible
					      :symbol 'appt-audible))
    (create-widget "visible" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Appointments Displayed"
		   :state (data-object-symbol "apt-display-diary"
					      :value appt-display-diary
					      :symbol 'appt-display-diary))
    (create-widget "modeline" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Display Appointment Time in Modeline"
		   :state (data-object-symbol "apt-display-mode-line"
					      :value appt-display-mode-line
					      :symbol 'appt-display-mode-line))
    (create-widget "showappts" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Show Appointments at Startup"
		   :state (data-object-symbol "view-diary-entries-initially"
					      :value view-diary-entries-initially
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
  (dlg-init)
  (require 'ps-print)
  (let ((oframe (create-widget "Toggle Frame psprintoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Postscript Printing Options"))
	)
    (create-widget "header" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Print header on each page."
		   :state (data-object-symbol "ps-print-header"
					      :value ps-print-header
					      :symbol 'ps-print-header))
     
    (create-widget "headerframe" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Print gaudy frame around header."
		   :state (data-object-symbol "ps-print-header-frame"
					      :value ps-print-header-frame
					      :symbol 'ps-print-header-frame))
    
   (create-widget "color" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Print with color."
		   :state (data-object-symbol "ps-print-color-p"
					      :value ps-print-color-p
					      :symbol 'ps-print-color-p))

    (create-widget "ps-font-size" widget-label oframe
		   :x 1 :y -2 :label-value "Printed Font Size:")
    (create-widget "ps-font-size-txt" widget-text-field oframe
		   :width 5 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int "fontds-size"
					      :symbol 'ps-font-size
					      :value 
					      (if (numberp ps-font-size)
						  (int-to-string ps-font-size)
						10)))
    (create-widget "ps-font-size-unit" widget-label oframe
		   :x -3 :y t :label-value "Pts")

    (create-widget "psprintcmd" widget-label oframe
		   :x 1 :y -2 :label-value "Print command    :")
    (create-widget "psprintcmd-txt" widget-text-field oframe
		   :width 20 :height 1 :x -2 :y t 
		   :value (data-object-symbol "ps-lpr-command"
					      :symbol 'ps-lpr-command
					      :value ps-lpr-command))
    
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
  (dlg-init)
  (let ((oframe (create-widget "Toggle Frame buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Font Lock Options"))
	)
    (if (or (> emacs-major-version 19)
	    (> emacs-minor-version 31))
	(create-widget "alwayson" widget-toggle-button oframe
		       :x 1 :y 1 :label-value "Always activate font-lock"
		       :state (data-object-command-option
			       "font-lock"
			       :value (dlg-quick-find 
				       "^\\s-*(global-font-lock-mode t)" 
				       dlg-config-file)
			       :command "(global-font-lock-mode t)"
			       :disable-command "(global-font-lock-mode nil)"))
    
      (create-widget "alwayson" widget-toggle-button oframe
		     :x 1 :y 1 :label-value "Always activate font-lock"
		     :state (data-object-symbol-hook
			     "font-lock"
			     :value (member 'turn-on-font-lock find-file-hooks)
			     :symbol 'find-file-hooks
			     :command "turn-on-font-lock")))

    (create-widget "maxdecor" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Always use maximum decoration"
		   :state (data-object-symbol "font-lock-maximum-decoration"
					      :value font-lock-maximum-decoration
					      :symbol 'font-lock-maximum-decoration))

    (create-widget "lazylock" widget-toggle-button oframe
		   :x 1 :y -2 :label-value "Use Lazy-Lock (deferred font locking)"
		   :state (data-object-symbol-hook 
			   "font-lock-support-mode-lazy"
			   :value (or
				   (equal font-lock-support-mode 'lazy-lock-mode)
				   (member 'lazy-lock-mode font-lock-support-mode))
			   :symbol 'font-lock-support-mode
			   :command "lazy-lock-mode"))

    (create-widget "fastlock" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Use Fast-Lock (cached font lock info)"
		   :state (data-object-symbol-hook 
			   "font-lock-support-mode-fast"
			   :value (or
				   (equal font-lock-support-mode 'fast-lock-mode)
				   (member 'fast-lock-mode font-lock-support-mode))
			   :symbol 'font-lock-support-mode
			   :command "fast-lock-mode"))
    )
  (require 'lazy-lock)
  (let ((oframe (create-widget "lazy-lock-buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Lazy Lock Options"))
	)
    (create-widget "defer-time" widget-label oframe
		   :x 1 :y 1 :label-value "Defer Time   :")
    (create-widget "defer-time-txt" widget-text-field oframe
		   :width 5 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int 
			   "defer-time"
			   :symbol 'lazy-lock-defer-time
			   :value 
			   (number-to-string lazy-lock-defer-time)))
    (create-widget "defer-time-unit" widget-label oframe
		   :x -3 :y t :label-value "Seconds")
    
    (create-widget "stealthverbose" widget-toggle-button oframe
		   :x 1 :y -1 :label-value "Stealth Mode verbosity"
		   :state (data-object-symbol "lazy-lock-stealth-verbose"
					      :value lazy-lock-stealth-verbose
					      :symbol 'lazy-lock-stealth-verbose))

    (create-widget "stealth-time" widget-label oframe
		   :x 1 :y -1 :label-value "Stealth Time :")
    (create-widget "stealth-time-txt" widget-text-field oframe
		   :width 5 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int 
			   "stealth-time"
			   :symbol 'lazy-lock-stealth-time
			   :value 
			   (number-to-string lazy-lock-stealth-time)))
    (create-widget "stealth-time-unit" widget-label oframe
		   :x -3 :y t :label-value "Seconds")

    (create-widget "stealth-lines" widget-label oframe
		   :x 1 :y -1 :label-value "Stealth Lines:")
    (create-widget "stealth-lines-txt" widget-text-field oframe
		   :width 5 :height 1 :x -2 :y t 
		   :value (data-object-symbol-string-to-int 
			   "stealth-lines"
			   :symbol 'lazy-lock-stealth-lines
			   :value 
			   (number-to-string lazy-lock-stealth-lines)))
    (create-widget "stealth-lines-unit" widget-label oframe
		   :x -3 :y t :label-value "Lines")
    

    )
  (require 'fast-lock)
  (let ((oframe (create-widget "fast-lock-buffoptions" widget-frame 
			       widget-toplevel-shell
			       :x 2 :y -3
			       :frame-label "Fast Lock Options"))
	)

    (create-widget "saveothers" widget-toggle-button oframe
		   :x 1 :y 1 :label-value "Save font cache for files belonging to others"
		   :state (data-object-symbol "fast-lock-save-others"
					      :value fast-lock-save-others
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
