;;; speedbar - quick access to files and tags
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: speedbar.el,v 1.3 1996/10/01 02:52:55 zappo Exp $
;;; Version: 0.2
;;; Keywords: file, etags, tools
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

;;; Commentary: 
;;;   The speedbar provides a frame in which files, and locations in
;;; files are displayed.  These items can be clicked on in order to
;;; make the last active frame display that file location.
;;;
;;;   To use speedbar, put the following in an init file.
;;;
;;;   (require 'speedbar)
;;;   (speedbar-frame-mode 1)
;;;
;;;   This will automatically create the speedbar frame.
;;;   If you want to choose it from a menu or something, do this:
;;;
;;;   (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;;;   (define-key-after (lookup-key global-map [menu-bar tools])
;;;      [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])
;;;
;;;   XEmacs users.  You can autoload it, but you will actually have
;;; to type M-x speedbar-frame-mode RET to make it work as I have not
;;; code segment to give you to cut in.
;;;
;;;   Once a speedbar frame is active, it takes advantage of idle time
;;; to keep it's contents updated.  The contents is usually a list of
;;; files in the directory of the currently active buffer.  When
;;; applicable, tags from in given files are expanded.
;;;
;;;   Speedbar uses multiple methods for creating tags to jump to.
;;; When the variable `speedbar-use-imenu-package' is set, then
;;; speedbar will first try to use imenu to get tags.  If the mode of
;;; the buffer doesn't support imenu, then etags is used.  Using Imenu
;;; has the advantage that tags are cached, so opening and closing
;;; files is faster.  Speedbar-imenu will also load the file into a
;;; non-selected buffer so clicking the file later will be faster.
;;;
;;;   To add new files types into the speedbar, modify
;;; `speedbar-file-regexp' to include the extension of the file type
;;; you wish to include.  If speedbar complains that the file type is
;;; not supported, that means there is no built in support from imenu,
;;; and the etags part wasn't set up right.
;;;
;;;   To add new file types to imenu, see the documentation in the
;;; file imenu.el that comes with emacs.  To add new file types which
;;; etags supports, but speedbar does not, you need to modify 
;;; `speedbar-fetch-etags-parse-list'.  This variable is an
;;; association list with each element of the form: 
;;;   (extension-regex . parse-one-line)
;;; The extension-regex would be something like "\\.c" for a .c file,
;;; and the parse-one-line would be either a regular expression where
;;; match tag 1 is the element you wish displayed as a tag.  If you
;;; need to do something more complex, then you can also write a
;;; function which parses one line, and put its symbol there instead.
;;;
;;;    If the updates are going to slow for you, modify the variable
;;; `speedbar-show-directories' to nil to turn off that feature (which
;;; is kind of slow)  Alternatly, change `speedbar-update-speed' to a
;;; longer idle time before updates.
;;;
;;;    If you use directories, you will probably notice that you will
;;; navigate to a directory which is eventually replaced after you go
;;; back to editing a file (unless you pull up a new file.)  The delay
;;; time before this happens is in `speedbar-navigating-speed', and
;;; defaults to 20 seconds.
;;;
;;;    XEmacs users may want to change the default timeouts for
;;; `speedbar-update-speed' to something longer as XEmacs doesn't have
;;; idle timers, the speedbar timer keeps going off arbitrarilly while
;;; you're typeing.  It's quite pesky.
;;;
;;;    To get speedbar-configure-faces to work, you will need to
;;; download my eieio package from my ftp site:
;;; ftp://ftp.ultranet.com/pub/zappo/eieio-*.tar.gz
;;;

;;; HISTORY:
;;; 0.1   Initial Revision
;;; 0.2   Fixed problem with x-pointer-shape causing future frames not
;;;         to be created.
;;;       Fixed annoying habit of `speedbar-update-contents' to make
;;;         it possible to accidentally kill the speedbar buffer.
;;;       Clicking directory names now only changes the contents of
;;;         the speedbar, and does not cause a dired mode to appear.
;;;         Clicking the <+> next to the directory does cause dired to
;;;         be run.
;;;       Added XEmacs support, which means timer support moved to a
;;;         platform independant call.
;;;       Added imenu support.  Now modes are supported by imenu
;;;         first, and etags only if the imenu call doesn't work.
;;;         Imenu is a little faster than etags, and is more emacs
;;;         friendly.
;;;       Added more user control variables described in the commentary.
;;;       Added smart recentering when nodes are opened and closed.

(defvar speedbar-xemacsp (string-match "XEmacs" emacs-version))

(defvar speedbar-syntax-table nil
  "Syntax-table used on the speedbar")

(if speedbar-syntax-table
    nil
  (setq speedbar-syntax-table (make-syntax-table))
  (modify-syntax-entry ?[ " " speedbar-syntax-table)
  (modify-syntax-entry ?] " " speedbar-syntax-table))
 

(defvar speedbar-key-map nil
  "Keymap used in speedbar buffer.")
(defvar speedbar-menu-map nil
  "Keymap used in speedbar menu buffer.")

(if speedbar-key-map
    nil
  (setq speedbar-key-map (make-keymap))
  (suppress-keymap speedbar-key-map t)
  (if (string-match "XEmacs" emacs-version)
      (progn
	;; bind mouse bindings so we can manipulate the items on each line
	(define-key speedbar-key-map 'button1 'speedbar-click)
	(define-key speedbar-key-map 'button2 'speedbar-click)
	(define-key speedbar-key-map 'button3 'speedbar-click)
	;; Xemacs users.  You probably want your own toolbar for
	;; the speedbar frame or mode or whatever.  Make some buttons
	;; and mail me how to do it!
	;; Also, how do you disable all those menu items?  Email me that too
	;; as it would be most helpful.
	)
    ;; bind mouse bindings so we can manipulate the items on each line
    (define-key speedbar-key-map [mouse-1] 'speedbar-click)
    (define-key speedbar-key-map [mouse-2] 'speedbar-click)
    (define-key speedbar-key-map [mouse-3] 'speedbar-click)
    (define-key speedbar-key-map [down-mouse-1] 'speedbar-quick-mouse)
    (define-key speedbar-key-map [down-mouse-2] 'speedbar-quick-mouse)
    (define-key speedbar-key-map [down-mouse-3] 'speedbar-quick-mouse)
    ;; disable all menus - we don't have a lot of space to play with
    ;; in such a skinny frame.
    (define-key speedbar-key-map [menu-bar buffer] 'undefined)
    (define-key speedbar-key-map [menu-bar files] 'undefined)
    (define-key speedbar-key-map [menu-bar tools] 'undefined)
    (define-key speedbar-key-map [menu-bar edit] 'undefined)
    (define-key speedbar-key-map [menu-bar search] 'undefined)
    (define-key speedbar-key-map [menu-bar help-menu] 'undefined)
    ;; Create a menu for speedbar
    (setq speedbar-menu-map (make-sparse-keymap))
    (define-key speedbar-key-map [menu-bar speedbar] 
      (cons "Speedbar" speedbar-menu-map))
    (define-key speedbar-menu-map [close] 
      (cons "Close" 'speedbar-close-frame))
    (define-key speedbar-menu-map [clonfigure] 
      (cons "Configure Faces" 'speedbar-configure-faces))
    (define-key speedbar-menu-map [Update] 
      (cons "Update" 'speedbar-update-contents))
    ))

(defvar speedbar-buffer nil
  "The buffer displaying the speedbar.")
(defvar speedbar-frame nil
  "The frame displaying speedbar.")
(defvar speedbar-timer nil
  "The speedbar timer used for updating the buffer.")
(defvar speedbar-attached-frame nil
  "The frame which started speedbar mode.  This is the frame from
which all data displayed in the speedbar is gathered, and in which files
and such are displayed.")
(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer")

(defvar speedbar-sort-tags nil
  "*Sort tags before displaying on the screen")

(defvar speedbar-show-directories t
  "*Display directories to expand.  This will slow down the update process
on large directories.")

;; Xemacs timers aren't based on idleness.  Therefore tune it down a little
;; or suffer mightilly!
(defvar speedbar-update-speed (if speedbar-xemacsp 5 1)
  "*Time in seconds of idle time needed before speedbar will update
it's buffer to match what you've been doing in your other frame.")
(defvar speedbar-navigating-speed 10
  "*Idle time to wait before re-running the timer proc to pick up any new
activity if the user has started navigating directories in the speedbar.")

(defvar speedbar-width 20
  "*Initial size of the speedbar window")

(defvar speedbar-scrollbar-width 7
  "*Initial sizeo of the speedbar scrollbar.  The thinner, the more
display room you will have.")

(defvar speedbar-raise-lower t
  "*Non-nil means speedbar will auto raise and lower itself.  When this
is set, you can have only a tiny strip visible under your main emacs,
and it will raise and lower itself when you put the pointer in it.")

(defvar speedbar-file-regexp "\\.\\([CchH]\\|c\\(++\\|pp\\)\\|el\\|tex\\(i\\(nfo\\)?\\)?\\|emacs\\)$"
  "*Regular expresson matching files we are allowed to display")

(defvar speedbar-use-imenu-package (not speedbar-xemacsp)
  "*Optionally use the imenu package instead of etags for parsing.  This
is experimental for performace testing.")

(defvar speedbar-fetch-etags-parse-list
  '(("\\.\\([cChH]\\|c++\\|cpp\\|cc\\)$" . speedbar-parse-c-or-c++tag)
    ("\\.el\\|\\.emacs" .
     "defun\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
    ("\\.tex$" . speedbar-parse-tex-string)
    )
  "*Alist matching extension vs an expression which will extract the
symbol name we wish to display as match 1.  To add a new file type, you
would want to add a new association to the list, where the car
is the file match, and the cdr is the way to extract an element from
the tags output.  If the output is complex, use a function symbol
instead of regexp.  The function should expect to be at the beginning
of a line in the etags buffer.

This variable is ignored if `speedbar-use-imenu-package' is `t'")

(defvar speedbar-fetch-etags-command "etags"
  "*Command used to create an etags file.

This variable is ignored if `speedbar-use-imenu-package' is `t'")
(defvar speedbar-fetch-etags-arguments '("-D" "-I" "-o" "-")
  "*List of arguments to use with `speedbar-fetch-etags-command' to create
an etags output buffer.

This variable is ignored if `speedbar-use-imenu-package' is `t'")

;; Hey there xemacs users.  I'm not sure how to make faces have default
;; colors, so if someone out there would be nice, send me a patch, or
;; just set their colors in your .Xdefaults.
(cond (speedbar-xemacsp
       (make-face 'speedbar-button-face)
       ;;(make-face 'speedbar-file-face)
       (copy-face 'bold 'speedbar-file-face)
       (make-face 'speedbar-directory-face)
       (make-face 'speedbar-tag-face)
       ;;(make-face 'speedbar-selected-face)
       (copy-face 'underline 'speedbar-selected-face)
       ;;(make-face 'speedbar-highlight-face)
       (copy-face 'highlight 'speedbar-highlight-face)

       ;; Would an xemacs knowledgable person please email me a way to
       ;; make these faces have nice colors as seen below in the emacs
       ;; section.
       )
      (window-system
       (require 'faces)

       ;; Make the faces first
       (make-face 'speedbar-button-face)
       (make-face 'speedbar-file-face)
       (make-face 'speedbar-directory-face)
       (make-face 'speedbar-tag-face)
       (make-face 'speedbar-selected-face)
       (make-face 'speedbar-highlight-face)

       (condition-case nil
	   (progn
	     ;; Now try to make them different colors
	     (cond ((face-differs-from-default-p 'speedbar-button-face))
		   ((x-display-color-p) (set-face-foreground 'speedbar-button-face 
							     "light green"))
		   (t (copy-face 'bold 'speedbar-button-face)))

	     (cond ((face-differs-from-default-p 'speedbar-file-face))
		   ((x-display-color-p) (set-face-foreground 'speedbar-file-face
							     "cyan"))
		   (t (copy-face 'bold 'speedbar-file-face)))

	     (cond ((face-differs-from-default-p 'speedbar-directory-face))
		   ((x-display-color-p) (set-face-foreground 'speedbar-directory-face
							     "light blue"))
		   (t (copy-face 'bold 'speedbar-directory-face)))
       
	     (cond ((face-differs-from-default-p 'speedbar-tag-face))
		   ((x-display-color-p) (set-face-foreground 'speedbar-tag-face
							     "yellow"))
		   (t (copy-face 'italic 'speedbar-tag-face)))
       
	     (cond ((face-differs-from-default-p 'speedbar-selected-face))
		   ((x-display-color-p)
		    (set-face-foreground 'speedbar-selected-face "red")
		    (set-face-underline-p 'speedbar-selected-face t))
		   (t (copy-face 'bold 'speedbar-selected-face)))
       
	     (cond ((face-differs-from-default-p 'speedbar-highlight-face))
		   ((x-display-color-p)
		    (set-face-background 'speedbar-highlight-face "sea green"))
		   (t (copy-face 'highlight 'speedbar-highlight-face)))
	     )				; condition case
	 (error (message "Error updating some faces.  Using defaults")))
       )
      (t (message "Error loading faces for some reason...")))

;;;###autoload
(defun speedbar-frame-mode (&optional arg)
  "Enable or disable use of a speedbar.  Positive number means turn
on, nil means toggle."
  (interactive "p")
  (if (not window-system)
      (error "Speedbar is not useful outside of a windowing environement"))
  (if (and (numberp arg) (< arg 0))
      (progn
	(if (and speedbar-frame (frame-live-p speedbar-frame))
	    (delete-frame speedbar-frame))
	(speedbar-set-timer nil)
	(setq speedbar-frame nil)
	(if (bufferp speedbar-buffer)
	    (kill-buffer speedbar-buffer)))
    ;; Set this as our currently attached frame
    (setq speedbar-attached-frame (selected-frame))
    ;; Get the buffer to play with
    (speedbar-mode)
    ;; Get the frame to work in
    (if (and speedbar-frame (frame-live-p speedbar-frame))
	(raise-frame speedbar-frame)
      (setq speedbar-frame (make-frame (list 
					;; Xemacs fails to delete speedbar
					;; if minibuffer is off.
					(cons 'minibuffer 
					      (if speedbar-xemacsp t nil))
					(cons 'width speedbar-width)
					(cons 'height (frame-height))
					(cons 'scroll-bar-width speedbar-scrollbar-width)
					(cons 'auto-raise speedbar-raise-lower)
					(cons 'auto-lower speedbar-raise-lower)
					'(border-width . 0)
					)))
      ;; I moved the pointer shape selection, because when it was done
      ;; above, future frame creation failed without the pointers being
      ;; explicitly set.  This prevents this from happening.

      ;; I was wrong... I can't do it without messing everything up.
      ;(save-excursion
	;(select-frame speedbar-frame)
	;(condition-case nil
	;    (let ((ops x-pointer-shape)
	;	  (ostps x-sensitive-text-pointer-shape))
	;      (setq x-pointer-shape x-pointer-top-left-arrow
	;	    x-sensitive-text-pointer-shape x-pointer-hand2)
	;      (set-mouse-color nil)
	;      (setq x-pointer-shape ops
	;	    x-sensitive-text-pointer-shape ostps))
	;  (error (message "This emacs can't set the cursor shapes.")))
      ;)
    ;; reset the selection variable
    (setq speedbar-last-selected-file nil)
    ;; Put the buffer into the frame
    (save-window-excursion
      (select-frame speedbar-frame)
      (switch-to-buffer speedbar-buffer)
      (setq default-minibuffer-frame speedbar-attached-frame))
    (speedbar-set-timer speedbar-update-speed)
    )))

(defun speedbar-close-frame ()
  "Turn off speedbar mode"
  (interactive)
  (speedbar-frame-mode -1))

(defun speedbar-mode ()
  "Create and return a SPEEDBAR buffer."
  (setq speedbar-buffer (set-buffer (get-buffer-create "SPEEDBAR")))
  (kill-all-local-variables)
  (setq major-mode 'speedbar-mode)
  (setq mode-name "SB")
  (use-local-map speedbar-key-map)
  (set-syntax-table speedbar-syntax-table)
  (setq mode-line-format
	'(" *SPEEDBAR* " (line-number-mode "[L%l]")))
  (speedbar-update-contents)
  )

(defun speedbar-set-timer (timeout)
  "Unset an old timer (if there is one) and activate a new timer with the
given timeout value."
  (cond 
   ;; Xemacs
   (speedbar-xemacsp
    (if speedbar-timer 
	(progn (delete-itimer speedbar-timer)
	       (setq speedbar-timer nil)))
    (if timeout
	(setq speedbar-timer (start-itimer "speedbar"
					   'speedbar-timer-fn
					   timeout
					   nil))))
   ;; GNU emacs
   (t
    (if speedbar-timer 
	(progn (cancel-timer speedbar-timer)
	       (setq speedbar-timer nil)))
    (if timeout
	(setq speedbar-timer 
	      (run-with-idle-timer timeout nil 'speedbar-timer-fn))))
   ))

(defun speedbar-update-contents ()
  "Update the contents of the speedbar buffer."
  (interactive)
  (setq speedbar-last-selected-file nil)
  (let ((cbd default-directory))
    (save-excursion
      (set-buffer speedbar-buffer)
      (toggle-read-only -1)
      (setq default-directory cbd)
      (delete-region (point-min) (point-max))
      (let ((fl (directory-files default-directory nil speedbar-file-regexp nil))
	    (dl (if speedbar-show-directories
		    (let ((dlf (directory-files default-directory nil))
			  (ndl nil))
		      (while dlf
			(if (file-directory-p (car dlf))
			    (setq ndl (cons (car dlf) ndl)))
			(setq dlf (cdr dlf)))
		      ;; the cdr removes the rogue `.' reference
		      (cdr (sort ndl 'string<)))
		  nil)))
	(while dl
	  (let ((start (point))
		(end (progn  (insert "<+>") (point))))
	    (put-text-property start end 'mouse-face 'speedbar-highlight-face)
	    (put-text-property start end 'face 'speedbar-button-face)
	    )
	  (insert " ")
	  (let ((start (point))
		(end (progn (insert (car dl)) (point))))
	    (put-text-property start end 'mouse-face 'speedbar-highlight-face)
	    (put-text-property start end 'face 'speedbar-directory-face)
	    )
	  (insert "\n")
	  (setq dl (cdr dl)))
	(while fl
	  (let ((start (point))
		(end (progn  (insert "[+]") (point))))
	    (put-text-property start end 'mouse-face 'speedbar-highlight-face)
	    (put-text-property start end 'face 'speedbar-button-face)
	    )
	  (insert " ")
	  (let ((start (point))
		(end (progn (insert (car fl)) (point))))
	    (put-text-property start end 'mouse-face 'speedbar-highlight-face)
	    (put-text-property start end 'face 'speedbar-file-face)
	    )
	  (insert "\n")
	  (setq fl (cdr fl))))
      (toggle-read-only 1)
      )))

(defun speedbar-timer-fn ()
  "Run whenever emacs is idle to update the speedbar item"
  (if (not (and speedbar-frame 
		(frame-live-p speedbar-frame)
		speedbar-attached-frame 
		(frame-live-p speedbar-attached-frame)))
      (speedbar-set-timer nil)
    (unwind-protect
	(if (frame-visible-p speedbar-frame)
	    (let ((af (selected-frame)))
	      (save-window-excursion
		(select-frame speedbar-attached-frame)
		;; make sure we at least choose a window to
		;; get a good directory from
		(if (string-match "\\*Minibuf-[0-9]+\\*" (buffer-name))
		    (other-window 1))
		;; Update all the contents if directories change!
		(if (or (string= (expand-file-name default-directory)
				 (save-excursion
				   (set-buffer speedbar-buffer)
				   (expand-file-name default-directory)))
			(not (buffer-file-name))
			(not (string-match speedbar-file-regexp (buffer-file-name)))
			(eq af speedbar-frame))
		    nil
		  (message "Updating speedbar to: %s..." default-directory)
		  (speedbar-update-contents)
		  (message "Updating speedbar to: %s...done" default-directory)))))
      ;; Reset the timer
      (speedbar-set-timer speedbar-update-speed)
      ;; Ok, un-underline old file, underline current file
      (speedbar-update-current-file))))
      

(defun speedbar-update-current-file ()
  "Find out what the current file is, and update our visuals to indicate
what it is."
  (let* ((lastf (selected-frame))
	 (newcf (save-excursion
		  (select-frame speedbar-attached-frame)
		  (let ((rf (if (buffer-file-name)
				(file-name-nondirectory (buffer-file-name))
			      nil)))
		    (select-frame lastf)
		    rf)))
	(lastb (current-buffer)))
    (if (and newcf (not (string= newcf speedbar-last-selected-file)))
	(progn
	  (select-frame speedbar-frame)
	  (set-buffer speedbar-buffer)
	  (toggle-read-only -1)
	  (goto-char (point-min))
	  (if (and 
	       speedbar-last-selected-file
	       (re-search-forward 
		(concat "\\<" (regexp-quote speedbar-last-selected-file) "\\>")
		nil t))
	      (put-text-property (match-beginning 0)
				 (match-end 0)
				 'face
				 'speedbar-file-face))
	  (goto-char (point-min))
	  (if (re-search-forward 
	       (concat "\\<" (regexp-quote newcf) "\\>") nil t)
	      (put-text-property (match-beginning 0)
				 (match-end 0)
				 'face 
				 'speedbar-selected-face))
	  (setq speedbar-last-selected-file newcf)
	  (toggle-read-only 1)
	  (beginning-of-line)
	  (forward-char 3)
	  (set-buffer lastb)
	  (select-frame lastf)))))

(defun speedbar-quick-mouse (e)
  "Since mouse events are strange, this will keep the mouse nicely
positioned."
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (forward-char 3)
  )

(defun speedbar-quick-fetch (e)
  "Using event E, move the cursor to the right spot, then return an
alist describing what we the user clicked on.  The alist will be of
the form: ( type . data ),  where type can be `expand', `contract', `file'
`directory' `dirload' or `tag'.  Data for expand, and file is the file
name. Contract has no data, and tag has the ( file . tag ) value."
  (mouse-set-point e)
  (let* ((cc (current-column))
	 (ct (save-excursion
	       (beginning-of-line)
	       (cond ((looking-at "[[<]\\([-+]\\)[]>]") 'file)
		     (t 'tag))))
	 (rt (cond ((and (eq ct 'file) (< cc 4))
		    (save-excursion
		      (beginning-of-line)
		      (cond ((looking-at "\\[\\+")
			     'expand)
			    ((looking-at "<\\+")
			     'dirload)
			    (t
			     'contract))))
		   ((and (eq ct 'file) (> cc 3))
		    (cond ((save-excursion (beginning-of-line)
					   (looking-at "\\["))
			   'file)
			  (t 'directory)))
		   (t 'tag)))
	 (rv (save-excursion
	       (cond ((member rt '(expand dirload file directory))
		      (beginning-of-line)
		      (buffer-substring-no-properties
		       (+ (point) 4)
		       (save-excursion (end-of-line) (point))))
		     ((eq rt 'contract) nil)
		     ((eq rt 'tag)
		      (cons (save-excursion
			      (re-search-backward "^\\[-\\] \\([^\n]+\\)$"
						  nil t)
			      (buffer-substring-no-properties
			       (match-beginning 1)
			       (match-end 1)))
			    (get-text-property (point) 'position)))
		     (t 'hi)))))
    ;; lastly, move the cursor to the little spot between
    ;; items on the line.
    (beginning-of-line)
    (forward-char 3)
    ;; and return our item
    (cons rt rv)))

(defun speedbar-click (e)
  "When the user clicks mouse 1 on our speedbar, we must decide what
we want to do!  Clicking on a [+] expands that node.  Clicking on [-]
retracts that node.  Clicking on a file name will open that file, or
goto that buffer (in the last frame that was active).  Clicking on a
tag goes right to that tag."
  (interactive "e")
  (let ((todo (speedbar-quick-fetch e)))
    (cond ((eq (car todo) 'expand)
	   (save-excursion
	     (toggle-read-only -1)
	     (beginning-of-line)
	     (forward-char 1)
	     (delete-char 1)
	     (insert "-")
	     (put-text-property (1-(point)) (point) 'mouse-face 
				'speedbar-highlight-face)
	     (put-text-property (1- (point)) (point) 'face 
				'speedbar-button-face)
	     (end-of-line)
	     (forward-char 1)
	     ;; Insert the new tags.  if we use imenu, and imenu does
	     ;; not return t (meaning error, not a null list), then
	     ;; use etags instead.  If the user doesn't want to use imenu,
	     ;; then use etags parsing. (Which is a little more accurate.
	     (speedbar-insert-taglist 
	      (if speedbar-use-imenu-package
		  (let ((tim (speedbar-fetch-dynamic-imenu (cdr todo))))
		    (if (eq tim t)
			(speedbar-fetch-dynamic-etags (cdr todo))
		      tim))
		(speedbar-fetch-dynamic-etags (cdr todo))))
	     (speedbar-center-buffer-smartly)
	     (toggle-read-only 1)))
	  ((eq (car todo) 'contract)
	   (save-excursion
	     (toggle-read-only -1)
	     (beginning-of-line)
	     (forward-char 1)
	     (delete-char 1)
	     (insert "+")
	     (put-text-property (1- (point)) (point) 'mouse-face 
				'speedbar-highlight-face)
	     (put-text-property (1- (point)) (point) 'face 
				'speedbar-button-face)
	     (end-of-line)
	     (forward-char 1)
	     (delete-region (point)
			    (save-excursion
			      (if (re-search-forward "^[[<]" nil t)
				  (match-beginning 0)
				(point-max))))
	     (speedbar-center-buffer-smartly)
	     (toggle-read-only -1)))
	  ((eq (car todo) 'file)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (cdr todo)))
	     (speedbar-update-current-file)
	     ;; Reset the timer with a new timeout when cliking a file
	     ;; in case the user was navigating directories, we can cancel
	     ;; that other timer.
	     (speedbar-set-timer speedbar-update-speed)
	     ))
	  ((eq (car todo) 'directory)
	   (setq default-directory 
		 (concat (expand-file-name (concat default-directory
						   (cdr todo)))
			 "/"))
	   ;; Because we leave speedbar as the current buffer,
	   ;; update contents will change directory without
	   ;; having to touch the attached frame.
	   (speedbar-update-contents)
	   (speedbar-set-timer speedbar-navigating-speed)
	   )
	  ((eq (car todo) 'dirload)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (cdr todo)))
	     (speedbar-update-contents)
	     ;; make the timer go faster in case it was pushed out.
	     (speedbar-set-timer speedbar-update-speed)
	     ))
	  ((eq (car todo) 'tag)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (car (cdr todo))))
	     (goto-char (cdr (cdr todo)))
	     ;; Is there a command that recenters well?
	     (speedbar-update-current-file)
	     ;; Reset the timer with a new timeout when cliking a file
	     ;; in case the user was navigating directories, we can cancel
	     ;; that other timer.
	     (speedbar-set-timer speedbar-update-speed)
	     ))
	  (t (message "%S" todo)))
    ))

(defun speedbar-insert-taglist (taglist)
  "Starting at point, insert data from the taglist into the current
buffer.  Put faces on everything so that when we click on them later
we can identify them."
  (save-excursion
    (while taglist
      (let ((start (point))
	    (end (progn (insert " >> ") (point))))
	(put-text-property start end 'face 'default)
	(put-text-property start end 'mouse-face nil))
      (let ((start (point))
	    (end (progn (insert (car (car taglist))) (point))))
	(insert "\n")
	(put-text-property start end 'mouse-face 'speedbar-highlight-face)
	(put-text-property start end 'face 'speedbar-tag-face)
	(put-text-property start end 'position (cdr (car taglist)))
	)
      (setq taglist (cdr taglist)))))

(defun speedbar-center-buffer-smartly ()
  "Look at the buffer, and center it so that which the user is most
interested in (as far as we can tell) is all visible.  This assumes
that the cursor is on a file, or tag of a file which the user is
interested in."
  (if (<= (count-lines (point-min) (point-max)) 
	  (window-height (selected-window)))
      ;; whole buffer fits
      (let ((cp (point)))
	(goto-char (point-min))
	(recenter 0)
	(goto-char cp))
    (let (start end)
      (save-excursion
	(end-of-line)
	(if (re-search-backward "[[<]\\([-+]\\)[]>]" nil t)
	    (setq start (point))
	  (setq start nil)))
      (save-excursion
	(if (re-search-forward "[[<]\\([-+]\\)[]>]" nil t)
	    (setq end (point))
	  (setq end (point-max))))
      (if (not start)
	  (message "Centering error.")
	;; too big
	(let ((nl (count-lines start end))
	      (cp (point)))
	  (if (> nl (window-height (selected-window)))
	      (progn (goto-char start)
		     (recenter 1))
	    ;; we can fit everything on the screen, but...
	    (if (and (pos-visible-in-window-p start (selected-window))
		     (pos-visible-in-window-p end (selected-window)))
		;; we are all set!
		nil
	      ;; we need to do something...
	      (goto-char start)
	      (let ((newcent (/ (- (window-height (selected-window)) nl) 2))
		    (lte (count-lines start (point-max))))
		(if (and (< (+ newcent lte) (window-height (selected-window)))
			 (> (- (window-height (selected-window)) lte 1)
			    newcent))
		    (setq newcent (- (window-height (selected-window))
				     lte 1)))
		(recenter newcent))))
	  (goto-char cp))))))


(defun speedbar-fetch-dynamic-imenu (file)
  "Use the imenu package to load in file, and extract all the items
tags we wish to display in the speedbar package."
  (require 'imenu)
  (save-excursion
    (set-buffer (find-file-noselect file))
    (condition-case nil
	(let ((tl (cdr (imenu--make-index-alist t))))
	  (while (and tl (not (numberp (cdr (car tl)))))
	    (setq tl (cdr tl)))
	  tl)
	(error t))))

(defun speedbar-fetch-dynamic-etags (file)
  "For the complete file definition FILE, run etags as a subprocess,
fetch it's output, and create a list of symbols extracted, and their
position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (if (get-buffer "*etags tmp*")
	      (kill-buffer "*etags tmp*"))	;kill to clean it up
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (apply 'call-process speedbar-fetch-etags-command nil 
		 (current-buffer) nil 
		 (append speedbar-fetch-etags-arguments (list file)))
	  (goto-char (point-min))
	  (let ((expr 
		 (let ((exprlst speedbar-fetch-etags-parse-list)
		       (ans nil))
		   (while (and (not ans) exprlst)
		     (if (string-match (car (car exprlst)) file)
			 (setq ans (car exprlst)))
		     (setq exprlst (cdr exprlst)))
		   (cdr ans))))
	    (if expr
		(let (tnl)
		  (while (not (save-excursion (end-of-line) (eobp)))
		    (save-excursion
		      (setq tnl (speedbar-extract-one-symbol expr)))
		    (if tnl (setq newlist (cons tnl newlist)))
		    (forward-line 1)))
	      (message "Sorry, no support for a file of that extension"))))
      )
    (if speedbar-sort-tags
	(sort newlist '(lambda (a b) (string< (car a) (car b))))
      (reverse newlist))))

(defun speedbar-extract-one-symbol (expr)
  "At point in current buffer, return nil, or one alist of the form
of a dotted pair: ( symbol . position ) from etags output.  Parse the
output using the regular expression EXPR"
  (let* ((sym (if (stringp expr)
		  (if (save-excursion
			(re-search-forward expr (save-excursion 
						  (end-of-line)
						  (point)) t))
		      (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
		(funcall expr)))
	 (pos (let ((j (re-search-forward "[\C-?\C-a]\\([0-9]+\\),\\([0-9]+\\)"
					  (save-excursion
					    (end-of-line)
					    (point))
					  t)))
		(if (and j sym)
		    (1+ (string-to-int (buffer-substring-no-properties
					(match-beginning 2) 
					(match-end 2))))
		  0))))
    (if (/= pos 0)
	(cons sym pos)
      nil)))

(defun speedbar-parse-c-or-c++tag ()
  "Parse a c or c++ tag, which tends to be a little complex."
  (save-excursion
    (let ((bound (save-excursion (end-of-line) (point))))
      (cond ((re-search-forward "\C-?\\([^\C-a]+\\)\C-a" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t]+\\)\\s-+new(" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t(]+\\)\\s-*(\C-?" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    (t nil))
      )))

(defun speedbar-parse-tex-string ()
  "Parse a tex string.  Only find data which is relevant"
  (save-excursion
    (let ((bound (save-excursion (end-of-line) (point))))
      (cond ((re-search-forward "\\(section\\|chapter\\|cite\\)\\s-*{[^\C-?}]*}?" bound t)
	     (buffer-substring-no-properties (match-beginning 0)
					     (match-end 0)))
	    (t nil)))))

(defun speedbar-configure-faces ()
  "Configure faces for the speedbar program using e-config."
  (interactive)
  (require 'dlg-config)
  (save-excursion
    (select-frame speedbar-attached-frame)
    (dlg-faces '(speedbar-button-face
		 speedbar-file-face
		 speedbar-directory-face
		 speedbar-tag-face
		 speedbar-highlight-face
		 speedbar-selected-face))))

;;; end of lisp
(provide 'speedbar)