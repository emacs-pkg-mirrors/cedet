;;; e-config - configureation for emacs
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: e-config.el,v 1.6 1996/12/19 21:16:58 zappo Exp $
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
  (dialog-build-group "Interface Options"
    (create-widget  "Display Line Number in Modeline" widget-toggle-button
		   :state (data-object-symbol 'line-number-mode))
    (create-widget  "Display Column Number in Modeline" widget-toggle-button
		   :state (data-object-symbol 'column-number-mode))
    (create-widget "Truncate Lines" widget-toggle-button
		   :state (data-object-symbol-default 'truncate-lines))
    (create-widget "Suggest Key Bindings" widget-toggle-button
		   :state (data-object-symbol 'suggest-key-bindings))
    (create-widget "Visible Bell" widget-toggle-button
		   :state (data-object-symbol 'visible-bell))
    (create-widget "Inverse Video" widget-toggle-button
		   :state (data-object-symbol 'inverse-video))
    (create-widget "Modeline Inverse Video" widget-toggle-button
		   :state (data-object-symbol 'mode-line-inverse-video))
    (create-widget "Search Highlights Current Match" widget-toggle-button
		   :state (data-object-symbol 'search-highlight))
    (create-widget "Query Replace Highlight" widget-toggle-button
		   :state (data-object-symbol 'query-replace-highlight))
    (create-widget "Enable Recursive Minibuffers" widget-toggle-button
		   :state (data-object-symbol 'enable-recursive-minibuffers))
    (create-widget "Scroll Step:" widget-labeled-text
		   :unit "lines" :text-length 10 
		   :value (data-object-symbol-string-to-int 'scroll-step
							    :float-p nil))
    )
  (dialog-build-group "Startup Commands"

    (create-widget "Display time in modeline" widget-toggle-button
		   :state (data-object-command-option "(display-time)"))
    (create-widget "Typing break mode (remind you to rest)" widget-toggle-button
		   :state (data-object-symbol 'type-break-mode))
    (create-widget "Highlight Parenthesis" widget-toggle-button
		   :state (data-object-symbol-feature
			   'paren
			   :unload-commands 
			   (if (> emacs-minor-version 34)
			       '(show-paren-mode -1) nil)
			   ))
    (create-widget "Automatically Scroll Horizontally" widget-toggle-button
		   :state (data-object-symbol-feature
			   'auto-show
			   :unload-commands '(auto-show-mode -1)))
    (create-widget "Magically Resize Minibuffer when needed." widget-toggle-button
		   :state (data-object-symbol-feature
			   'rsz-mini
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
  (dialog-build-group "Behavior Options"

    (create-widget "Mouse Yanks to Cursor (not mouse)" widget-toggle-button
		   :state (data-object-symbol 'mouse-yank-at-point))
    (create-widget "Next-line adds newline at end of buffer"
		   widget-toggle-button
		   :state (data-object-symbol 'next-line-add-newlines))
    (create-widget "Adaptive Fill Mode" widget-toggle-button
		   :state (data-object-symbol 'adaptive-fill-mode))
    (create-widget "Require Final Newline" widget-toggle-button
		   :state (data-object-symbol 'require-final-newline))

    (create-widget "Auto fill in all text modes" widget-toggle-button
		   :state (data-object-symbol-hook
			   'text-mode-hook
			   :command "turn-on-auto-fill"))
    )
  (dialog-build-group "Suggested Disabled Commands"

    (create-widget "Evaluate Expression  < M-: >" widget-toggle-button
		   :state (data-object-symbol-disabled 'eval-expression))
    (create-widget "Narrow to region     < C-x n n >" widget-toggle-button
		   :state (data-object-symbol-disabled 'eval-expression))
    (create-widget "Set Goal Column      < C-x C-n >" widget-toggle-button
		   :state (data-object-symbol-disabled 'set-goal-column))
    (create-widget "Erase buffer         < M-x erase-buffer >" widget-toggle-button
		   :state (data-object-symbol-disabled 'erase-buffer))
    )
  (dialog-build-group "Backup File Methods"

    (create-widget "By Copying (off uses move)" widget-toggle-button
		   :state (data-object-symbol 'backup-by-copying))
    (create-widget "By Copying When Linked" widget-toggle-button
		   :state (data-object-symbol 'backup-by-copying-when-linked))
    (create-widget "By Copying When Owner/Group Mismatch " widget-toggle-button
		   :state (data-object-symbol 'backup-by-copying-when-mismatch))
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
  (dialog-build-group "Program Editing Options"

    (create-widget "Enable Local Variables" widget-toggle-button
		   :state (data-object-symbol 'enable-local-variables))

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
			   'compilation-finish-function))

    (create-widget "Compile Command       :"widget-labeled-text
		   :text-length 50
		   :value (data-object-symbol 'compile-command))

    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Mail configurations
;;;
(defun econfig-rmail ()
  "Creates a configure window with variables modifying variables
useful for sending email."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'rmail)
  (dialog-build-group "Rmail Options"

    (create-widget "Rmail File               :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-file-name))
    
    (create-widget "Secondary File Directory :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-secondary-file-directory))
    
    (create-widget "Default Secondary File   :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-default-rmail-file))
    
    (create-widget "Delete messages after saving to secondary file"
		   widget-toggle-button
		   :state (data-object-symbol 'rmail-delete-after-output))

    (create-widget "Summary motion scrolls messages" widget-toggle-button
		   :state (data-object-symbol
			   'rmail-summary-scroll-between-messages))

    )
  (dlg-end)
  (dialog-refresh)
  )

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
  (require 'sendmail)
  (dialog-build-group "Sending Mail Options"
    
    (let* ((uma (data-object-symbol 'user-mail-address))
	   (opt-list '("nil" "'parens" "'angles"))
	   (fsdo (data-object-symbol-list-index
		  'mail-from-style
		  :value (cond ((eq mail-from-style 'angles) 2)
			       ((eq mail-from-style 'parens) 1)
			       (t 0))
		  :string-list opt-list))
	   (emdo (data-object "example-mail-name" 
			      :value (econfig-mail-showfrom mail-from-style))))

      (create-widget "Mail Address   :" widget-labeled-text :text-length 50
		     :value uma)
      
      (create-widget "Reply-to       :" widget-labeled-text :text-length 50
		     :value (data-object-symbol 'mail-default-reply-to))
    
      (create-widget "From Style     :" widget-label)

      (create-widget "name-type" widget-option-button
		     :title "Name Format"
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
		   :value (data-object-symbol 'mail-signature-file))
    
    (create-widget "Auto load signature file" widget-toggle-button
		   :state (data-object-symbol 'mail-signature))
    (create-widget  "Spellcheck outbound messages" widget-toggle-button
		    :state (data-object-symbol-hook
			    'mail-send-hook
			    :command 
			    "(lambda () (if (y-or-n-p \"Spell message?\") (ispell-message)))"
			    ))

    (create-widget "Citation Prefix:" widget-labeled-text :text-length 10
		   :value (data-object-symbol 'mail-yank-prefix))
    (create-widget "cite-pref" widget-label
		   :x 3 :y -1 :label-value
		   "This appears before quoted text. It is `>' for most mailers.")

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
  (dialog-build-group "Calendar Options"

    (create-widget "Latitude :" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   'calendar-latitude))

    (create-widget "Longitude:" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   'calendar-longitude))


    )
  (dialog-build-group "Holiday Options"

    (create-widget "Show all Christian Holidays" widget-toggle-button
		   :state (data-object-symbol 
			   'all-christian-calendar-holidays))

    (create-widget "Show all Hebrew Holidays" widget-toggle-button
		   :state (data-object-symbol
			   'all-hebrew-calendar-holidays))

    (create-widget "Show all Islamic Holidays" widget-toggle-button
		   :state (data-object-symbol
			   'all-islamic-calendar-holidays))

    (create-widget "Show Holidays at Startup" widget-toggle-button
		   :state (data-object-symbol 
			   'view-calendar-holidays-initially))

    )
  (dialog-build-group "Appointment Options"

    (create-widget "Diary File:" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'diary-file))

    (create-widget "Warn of impending appointments" widget-toggle-button
		   :state (data-object-symbol-hook
			   'diary-hook :command "appt-make-list"))
    (create-widget "Appointments Audible" widget-toggle-button
		   :state (data-object-symbol 'appt-audible))
    (create-widget "Appointments Displayed" widget-toggle-button
		   :state (data-object-symbol 'appt-display-diary))
    (create-widget "Display Appointment Time in Modeline" widget-toggle-button
		   :state (data-object-symbol 'appt-display-mode-line))
    (create-widget "Show Appointments at Startup" widget-toggle-button
		   :state (data-object-symbol 'view-diary-entries-initially))
    
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
  (dialog-build-group "Postscript Printing Options"

    (create-widget "Print header on each page." widget-toggle-button
		   :state (data-object-symbol 'ps-print-header))

    (create-widget "Print page numbers (Must have headers on)."
		   widget-toggle-button
		   :state (data-object-symbol 'ps-show-n-of-n))
     
    (create-widget "Print gaudy frame around header." widget-toggle-button
		   :state (data-object-symbol 'ps-print-header-frame))
    
    (create-widget "Print with color." widget-toggle-button
		   :state (data-object-symbol 'ps-print-color-p))

    (create-widget "Auto-detect faces for bold, italic, and underline."
		   widget-toggle-button
		   :state (data-object-symbol 'ps-auto-font-detect))

    (create-widget "Printed Font Size :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int
			   'ps-font-size))

    (create-widget "Note: You must change the character width and height
whenever you change the font size, or the font family." widget-label
                   :x 5 :y -1 :face 'bold)

    (create-widget "Character Width   :" widget-labeled-text
		   :unit "Pts" :text-length 10 :y -1
		   :value (data-object-symbol-string-to-int 'ps-avg-char-width))
    (create-widget "Space Width       :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int 'ps-space-width))
    (create-widget "Line Height       :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int 'ps-line-height))


    (create-widget "Note: All font families listed are optimized
to work with Ghostscript" widget-label :face 'bold-italic :x 5)

    (create-widget "Printed Font Family:" widget-option-text
		   :y -1
		   :text-length 30 :option-list 
		   '("Courier"
		     "CharterBT-Roman"
		     "Times-Roman"
		     "Helvetica"
		     "ZapfChancery"
		     "Palatino-Roman"
		     "NewCenturySchlbk-Roman"
		     "Utopia-Regular"
		     )
		   :value (data-object-symbol 'ps-font))

    (create-widget "Bold Font Family   :" widget-option-text
		   :box-face 'bold
		   :text-length 30 :option-list
		   '("Courier-Bold"
		     "Charter-Bold"
		     "Times-Bold"
		     "Helvetica-Bold"
		     "ZapfChancery-Bold"
		     "Palatino-Bold"
		     "NewCenturySchlbk-Bold"
		     "Utopia-Bold"
		     )
		   :value (data-object-symbol 'ps-font-bold))

    (create-widget "Italic Font Family :" widget-option-text
		   :box-face 'italic
		   :text-length 30 :option-list
		   '("Courier-Oblique"
		     "Charter-Italic"
		     "Times-Italic"
		     "Helvetica-Oblique"
		     "ZapfChancery-Oblique"
		     "Palatino-Italic"
		     "NewCenturySchlbk-Italic"
		     "Utopia-Italic"
		     )
		   :value (data-object-symbol 'ps-font-italic))

    (create-widget "Bold Italic Font   :" widget-option-text
		   :box-face 'bold-italic
		   :text-length 30 :option-list 
		   '("Courier-BoldOblique"
		     "Charter-BoldItalic"
		     "Times-BoldItalic"
		     "Helvetica-BoldOblique"
		     ;; Offer both since there is no bold-oblique
		     "ZapfChancery-Bold"
		     "ZapfChancery-Oblique"
		     "Palatino-BoldItalic"
		     "NewCenturySchlbk-BoldItalic"
		     "Utopia-BoldItalic"
		     )
		   :value (data-object-symbol 'ps-font-bold-italic))

    (create-widget "Print command     :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'ps-lpr-command))
    (create-widget "lpr parameters    :" widget-labeled-text
		   :text-length 30
		   :value (data-object-symbol-translated 
			   'ps-lpr-switches
			   :set-lambda 'dlg-string-to-list
			   :get-lambda 'dlg-list-to-string))
    (create-widget "Paper Size        :" widget-label)
    (let* ((opt-list '("'ps-letter" "'ps-legal" "'ps-a4"))
	   (opt-dat (data-object-symbol-list-index
		     'ps-paper-type
		     :value (cond ((eq ps-paper-type 'ps-letter) 0)
				  ((eq ps-paper-type 'ps-legal) 1)
				  ((eq ps-paper-type 'ps-a4) 2)
				  (t 0))
		     :string-list opt-list)))

      (create-widget "paper-size" widget-option-button
		     :title "Paper Size"
		     :x -2 :y t :option-list opt-list :state opt-dat)
      )
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
  (dialog-build-group "Font Lock Options"

    (create-widget  "Always activate font-lock" widget-toggle-button
		    :state 
		    (if (or (> emacs-major-version 19)
			    (> emacs-minor-version 31))
			(data-object-command-option
			 "(global-font-lock-mode t)"
			 :disable-command "(global-font-lock-mode nil)")
		      (data-object-symbol-hook
		       'find-file-hooks
		       :command "turn-on-font-lock")))

    (create-widget "Always use maximum decoration" widget-toggle-button
		   :state (data-object-symbol 
			   'font-lock-maximum-decoration))

    (dialog-build-group (create-widget 
			 "Font Lock Enhancers" widget-radio-frame
			 ;; In this situation, lets
			 ;; turn off some of the sides, and
			 ;; it will behave like a separator
			 :box-sides [nil nil t nil]
			 :position 'top-right
			 :x 0
			 :state (data-object-symbol-list-index
				 'font-lock-support-mode
				 :string-list 
				 '("nil" "'lazy-lock-mode"
				   "'fast-lock-mode")
				 :value 
				 (cond 
				  ((eq font-lock-support-mode 'fast-lock-mode)
				   2)
				  ((eq font-lock-support-mode 'lazy-lock-mode)
				   1)
				  (t 0))))

      (create-widget "Use Nothing (No locking enhancers)" widget-radio-button)

      (create-widget "Use Lazy-Lock (deferred font locking)" widget-radio-button)

      (create-widget "Use Fast-Lock (cached font lock info)" widget-radio-button)

      ))
  (require 'lazy-lock)
  (dialog-build-group "Lazy Lock Options"

    (create-widget "Defer Time   :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-defer-time))
    
    (create-widget "Stealth Mode verbosity" widget-toggle-button
		   :state (data-object-symbol
			   'lazy-lock-stealth-verbose))

    (create-widget "Stealth Time :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-stealth-time))

    (create-widget "Stealth Lines:" widget-labeled-text
		   :text-length 5 :unit "Lines"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-stealth-lines))
    )
  (require 'fast-lock)
  (dialog-build-group "Fast Lock Options"

    (create-widget  "Save font cache for files belonging to others" 
		    widget-toggle-button
		    :state (data-object-symbol 'fast-lock-save-others))
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
