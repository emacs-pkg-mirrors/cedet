;;; speedbar - quick access to files and tags
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: speedbar.el,v 1.1 1996/09/27 01:52:24 zappo Exp $
;;; Version: 0.1
;;; Keywords: file, etags
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
;;;
;;;   Once a speedbar frame is active, it takes advantage of idle time
;;; to keep it's contents updated.  The contents is usually a list of
;;; files in the directory of the currently active buffer.  When
;;; applicable, tags from in given files are expanded.
;;;
;;;   To add new files types into the speedbar, there are some simple
;;; variables to edit.  Check the doc strings on `speedbar-file-regexp', 
;;; and `speedbar-fetch-etags-parse-list'.  This will let you get tags
;;; for files of different types not currently supported.  If you add new
;;; expressions, please email them to me for future inclusion.
;;;
;;;    If the updates are going to slow for you, modify the variable
;;; `speedbar-show-directories' to nil to turn off that feature (which
;;; is kind of slow)  Alternatly, change `speedbar-update-speed' to a
;;; longer idle time before updates.
;;;
;;;    To get speedbar-configure-faces to work, you will need to
;;; download my eieio package from my ftp site:
;;; ftp://ftp.ultranet.com/pub/zappo/eieio-*.tar.gz
;;;

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
  )

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

(defvar speedbar-sort-tags nil
  "*Sort tags before displaying on the screen")

(defvar speedbar-show-directories t
  "*Display directories to expand.  This will slow down the update process
on large directories.")

(defvar speedbar-update-speed 5
  "*Time in seconds of idle time needed before speedbar will update
it's buffer to match what you've been doing in your other frame.")

(defvar speedbar-width 20
  "*Initial size of the speedbar window")

(defvar speedbar-file-regexp "\\.\\(c\\|el\\)$"
  "*Regular expresson matching files we are allowed to display")

(defvar speedbar-fetch-etags-parse-list
  '(("\\.\\([cChH]\\|c++\\|cpp\\)$" . 
     "\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\s-*(?")
    ("\\.el\\|\\.emacs" .
     "defun\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*")
    )
  "*Alist matching extension vs an expression which will extract the
symbol name we wish to display as match 1.")

(defvar speedbar-fetch-etags-command "etags"
  "*Command used to create an etags file.")
(defvar speedbar-fetch-etags-arguments '("-D" "-I" "-o" "-")
  "*List of arguments to use with `speedbar-fetch-etags-command' to create
an etags output buffer.")


(if (not window-system)
    nil
  (require 'faces)

  (make-face 'speedbar-button-face)
  (cond ((face-differs-from-default-p 'speedbar-button-face))
	((x-display-color-p) (set-face-foreground 'speedbar-button-face 
						  "light green"))
	(t (copy-face 'bold 'speedbar-button-face)))

  (make-face 'speedbar-file-face)
  (cond ((face-differs-from-default-p 'speedbar-file-face))
	((x-display-color-p) (set-face-foreground 'speedbar-file-face
						  "cyan"))
	(t (copy-face 'bold 'speedbar-file-face)))

  (make-face 'speedbar-directory-face)
  (cond ((face-differs-from-default-p 'speedbar-directory-face))
	((x-display-color-p) (set-face-foreground 'speedbar-directory-face
						  "light blue"))
	(t (copy-face 'bold 'speedbar-directory-face)))

  (make-face 'speedbar-tag-face)
  (cond ((face-differs-from-default-p 'speedbar-tag-face))
	((x-display-color-p) (set-face-foreground 'speedbar-tag-face
						  "yellow"))
	(t (copy-face 'italic 'speedbar-tag-face)))

  (make-face 'speedbar-selected-face)
  (cond ((face-differs-from-default-p 'speedbar-selected-face))
	((x-display-color-p)
	 (set-face-foreground 'speedbar-selected-face "red")
	 (set-face-underline-p 'speedbar-selected-face t))
	(t (copy-face 'bold 'speedbar-selected-face)))

  (make-face 'speedbar-highlight-face)
  (cond ((face-differs-from-default-p 'speedbar-highlight-face))
	((x-display-color-p)
	 (set-face-background 'speedbar-highlight-face "sea green"))
	(t (copy-face 'highlight 'speedbar-highlight-face)))

  )

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
	(cancel-timer speedbar-timer)
	(setq speedbar-frame nil))
    ;; Set this as our currently attached frame
    (setq speedbar-attached-frame (selected-frame))
    ;; Get the buffer to play with
    (speedbar-mode)
    ;; Get the frame to work in
    (if (and speedbar-frame (frame-live-p speedbar-frame))
	(raise-frame speedbar-frame)
      (let ((x-pointer-shape x-pointer-top-left-arrow)
	    (x-sensitive-text-pointer-shape x-pointer-hand2))
	(setq speedbar-frame (make-frame (list '(minibuffer . nil)
					       (cons 'width speedbar-width)
					       (cons 'height (frame-height))
					       '(scroll-bar-width . 7)
					       '(auto-raise . t)
					       '(auto-lower . t)
					       '(modeline . nil)
					       '(border-width . 0)
					       '(unsplittable t)
					       )))))
    ;; Put the buffer into the frame
    (save-window-excursion
      (select-frame speedbar-frame)
      (switch-to-buffer speedbar-buffer)
      (setq default-minibuffer-frame speedbar-attached-frame))
    (setq speedbar-timer (run-with-idle-timer speedbar-update-speed 
					      t 'speedbar-timer-fn))
    ))

(defun speedbar-close-frame ()
  "Turn off speedbar mode"
  (interactive)
  (speedbar-frame-mode -1))

(defun speedbar-mode ()
  "Create and return a SPEEDBAR buffer."
  (setq speedbar-buffer (set-buffer (get-buffer-create "SPEEDBAR")))
  (setq major-mode 'speedbar-mode)
  (setq mode-name "SB")
  (use-local-map speedbar-key-map)
  (set-syntax-table speedbar-syntax-table)
  (setq mode-line-format
	'(" *SPEEDBAR* " (line-number-mode "[L%l]")))
  (speedbar-update-contents)
  )

(defun speedbar-update-contents()
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
	    ;(put-text-property start end 'mouse-face 'speedbar-highlight-face)
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
  (if (not (and speedbar-frame (frame-live-p speedbar-frame)))
      (cancel-timer speedbar-timer)
    (if (frame-visible-p speedbar-frame)
	(save-excursion
	  (select-frame speedbar-attached-frame)
	  ;; Update all the contents if directories change!
	  (if (or (string= default-directory (save-excursion
					       (set-buffer speedbar-buffer)
					       default-directory))
		  (not (buffer-file-name))
		  (not (string-match speedbar-file-regexp (buffer-file-name))))
	      nil
	    (message "Updating speedbar to: %s..." default-directory)
	    (speedbar-update-contents)
	    (message "Updating speedbar to: %s...done" default-directory))))
    ;; Ok, un-underline old file, underline current file
    (speedbar-update-current-file)))

(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer")

(defun speedbar-update-current-file ()
  "Find out what the current file is, and update our visuals to indicate
what it is."
  (let ((newcf (save-excursion
		 (select-frame speedbar-attached-frame)
		 (file-name-nondirectory (buffer-file-name))))
	(lastb (current-buffer))
	(lastf (selected-frame)))	
    (if (not (string= newcf speedbar-last-selected-file))
	(progn
	  (select-frame speedbar-frame)
	  (set-buffer speedbar-buffer)
	  (toggle-read-only -1)
	  (goto-char (point-min))
	  (if (and 
	       speedbar-last-selected-file
	       (re-search-forward (regexp-quote speedbar-last-selected-file) 
				  nil t))
	      (put-text-property (match-beginning 0)
				 (match-end 0)
				 'face
				 'speedbar-file-face))
	  (goto-char (point-min))
	  (if (re-search-forward (regexp-quote newcf) nil t)
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
	     (put-text-property (1- (point)) (point) 'mouse-face 
				'speedbar-highlight-face)
	     (put-text-property (1- (point)) (point) 'face 
				'speedbar-button-face)
	     (end-of-line)
	     (forward-char 1)
	     (speedbar-insert-taglist 
	      (speedbar-fetch-dynamic-etags (cdr todo)))
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
	     (toggle-read-only -1)))
	  ((eq (car todo) 'file)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (cdr todo)))
	     (speedbar-update-current-file)
	     ))
	  ((eq (car todo) 'directory)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (cdr todo)))
	     (speedbar-update-contents)
	     ))
	  ((eq (car todo) 'tag)
	   (let ((cdd default-directory))
	     (select-frame speedbar-attached-frame)
	     (find-file (concat cdd (car (cdr todo))))
	     (goto-char (cdr (cdr todo)))
	     ;; Is there a command that recenters well?
	     (recenter 1)
	     (speedbar-update-current-file)
	     ))
	  (t (message "%S" todo)))
    ))

(defun speedbar-insert-taglist (taglist)
  "Starting at point, insert data from the taglist into the current
buffer.  Put faces on everything so that when we click on them later
we can identify them."
  (while taglist
    (insert " >> ")
    (let ((start (point))
	  (end (progn (insert (car (car taglist))) (point))))
      (put-text-property start end 'mouse-face 'speedbar-highlight-face)
      (put-text-property start end 'face 'speedbar-tag-face)
      (put-text-property start end 'position (cdr (car taglist)))
      )
    (insert "\n")
    (setq taglist (cdr taglist))))

(defun speedbar-fetch-dynamic-etags (file)
  "For the complete file definition FILE, run etags as a subprocess,
fetch it's output, and create a list of symbols extracted, and their
position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (erase-buffer)
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
	    (let (tnl)
	      (while (not (save-excursion (end-of-line) (eobp)))
		(save-excursion
		  (setq tnl (speedbar-extract-one-symbol expr)))
		(if tnl (setq newlist (cons tnl newlist)))
		(forward-line 1)))))
      (kill-buffer "*etags tmp*"))
    (if speedbar-sort-tags
	(sort newlist '(lambda (a b) (string< (car a) (car b))))
      (reverse newlist))))

(defun speedbar-extract-one-symbol (expr)
  "At point in current buffer, return nil, or one alist of the form
of a dotted pair: ( symbol . position ) from etags output.  Parse the
output using the regular expression EXPR"
  (if (save-excursion
	(re-search-forward expr (save-excursion (end-of-line) (point)) t))
      (let ((sym (buffer-substring-no-properties (match-beginning 1)
						 (match-end 1)))
	    (pos (let ((j (re-search-forward "\\([0-9]+\\),\\([0-9]+\\)"
					     (save-excursion
					       (end-of-line)
					       (point))
					     t)))
		   (if j
		       (1+ (string-to-int (buffer-substring-no-properties
					   (match-beginning 2) 
					   (match-end 2))))
		     0))))
	(if (/= pos 0)
	    (cons sym pos)
	  nil))
    nil))

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