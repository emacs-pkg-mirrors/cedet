;;; speedbar - quick access to files and tags
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: speedbar.el,v 1.8 1996/10/30 03:00:38 zappo Exp $
;;; Version: 0.3
;;; Keywords: file, tags, tools
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
;;;
;;;   The speedbar provides a frame in which files, and locations in
;;; files are displayed.  These items can be clicked on with mouse-2
;;; in order to make the last active frame display that file location.
;;;
;;;   If you want to choose it from a menu or something, do this:
;;;
;;;   (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;;;   (define-key-after (lookup-key global-map [menu-bar tools])
;;;      [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])
;;;
;;;   To activate speedbar without the menu, type: M-x speedbar-frame-mode RET
;;;
;;;   Once a speedbar frame is active, it takes advantage of idle time
;;; to keep it's contents updated.  The contents is usually a list of
;;; files in the directory of the currently active buffer.  When
;;; applicable, tags in the active file can be expanded.
;;;
;;;   Speedbar uses multiple methods for creating tags to jump to.
;;; When the variable `speedbar-use-imenu-package' is set, then
;;; speedbar will first try to use imenu to get tags.  If the mode of
;;; the buffer doesn't support imenu, then etags is used.  Using Imenu
;;; has the advantage that tags are cached, so opening and closing
;;; tags lists is faster.  Speedbar-imenu will also load the file into
;;; a non-selected buffer so clicking the file later will be faster.
;;;
;;;   To add new files types into the speedbar, modify
;;; `speedbar-file-regexp' to include the extension of the file type
;;; you wish to include.  If speedbar complains that the file type is
;;; not supported, that means there is no built in support from imenu,
;;; and the etags part wasn't set up right.
;;;
;;;   To add new file types to imenu, see the documentation in the
;;; file imenu.el that comes with emacs.  To add new file types which
;;; etags supports, you need to modify the variable
;;; `speedbar-fetch-etags-parse-list'.  This variable is an
;;; association list with each element of the form: (extension-regex
;;; . parse-one-line) The extension-regex would be something like
;;; "\\.c$" for a .c file, and the parse-one-line would be either a
;;; regular expression where match tag 1 is the element you wish
;;; displayed as a tag.  If you need to do something more complex,
;;; then you can also write a function which parses one line, and put
;;; its symbol there instead.
;;;
;;;    If the updates are going to slow for you, modify the variable
;;; `speedbar-update-speed' to a longer idle time before updates.
;;;
;;;    If you navigate directories, you will probably notice that you
;;; will navigate to a directory which is eventually replaced after
;;; you go back to editing a file (unless you pull up a new file.)
;;; The delay time before this happens is in
;;; `speedbar-navigating-speed', and defaults to 20 seconds.
;;;
;;;    XEmacs users may want to change the default timeouts for
;;; `speedbar-update-speed' to something longer as XEmacs doesn't have
;;; idle timers, the speedbar timer keeps going off arbitrarilly while
;;; you're typing.  It's quite pesky.
;;;
;;;    To get speedbar-configure-faces to work, you will need to
;;; download my eieio package from my ftp site.
;;;
;;;    EIEIO is NOT required when using speedbar.  It is ONLY needed
;;; if you want to use a fancy dialog face editor for speedbar.

;;; Speedbar updates can be found at:
;;; ftp://ftp.ultranet.com/pub/zappo/speedbar.*.el
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
;;; 0.3   x-pointer-shape fixed for emacs 19.35, so I put that check in.
;;;       Added invisible codes to the beginning of each line.
;;;       Added list aproach to node expansion for easier addition of new
;;;         types of things to expand by
;;;       Added multi-level path name support
;;;       Added multi-level tag name support.
;;;       Only mouse-2 is now used for node expansion
;;;       Added keys e + - to edit expand, and contract node lines
;;;       Added longer legal file regexp for all those modes which support
;;;         imenu. (pascal, fortran90, ada, pearl)
;;;       Fixed centering algorithm
;;;       Tried to choose background independent colors.  Made more robust.
;;;       Rearranged code into a more logical order
;;; 0.3.1 Fixed doc & broken keybindings
;;;       Added mode hooks.
;;;       Improved face color selection
;;;       `nil' passed to `speedbar-frame-mode' now toggles the frame as
;;;         advertised in the doc string
;;;       
;;; TODO:
;;; 1) Rember contents of directories when leaving them so it's faster
;;;    when returning.
;;; 2) List of directories to never visit.  (User might be browsing
;;;    there temporarilly such as info files, documentation and the
;;;    like)
;;; 3) Implement SHIFT-mouse2 to rescan buffers with imenu.
;;; 4) Better XEmacs support of menus and button-bar
;;; 5) More functions to create buttons and options
;;; 6) filtering algoritms to reduce the number of tags/files
;;;    displayed.
;;; 7) Build `speedbar-file-regexp' on the fly.
;;; 8) More intelligent current file highlighting.

(defvar speedbar-xemacsp (string-match "XEmacs" emacs-version))

(defvar speedbar-initial-expansion-list
  '(speedbar-directory-buttons speedbar-default-directory-list)
  "*List of functions to call to fill in the speedbar buffer whenever
a top level update is issued.  These functions will allways get the
default directory to use passed in as the first parameter, and a 0 as
the second parameter.  They must assume that the cursor is at the
postion where they start inserting buttons.")

(defvar speedbar-show-unknown-files nil
  "*Non-nil shows files with a ? in the expansion tag for files we can't
expand.  `nil' means don't show the file in the list.")

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

(defvar speedbar-scrollbar-width 10
  "*Initial sizeo of the speedbar scrollbar.  The thinner, the more
display room you will have.")

(defvar speedbar-raise-lower t
  "*Non-nil means speedbar will auto raise and lower itself.  When this
is set, you can have only a tiny strip visible under your main emacs,
and it will raise and lower itself when you put the pointer in it.")

(defvar speedbar-use-imenu-package (not speedbar-xemacsp)
  "*Optionally use the imenu package instead of etags for parsing.  This
is experimental for performace testing.")

(defvar speedbar-before-delete-hook nil
  "*Hooks called before deletiing the speedbar frame.")

(defvar speedbar-mode-hook nil
  "*Hooks called after creating a speedbar buffer")

(defvar speedbar-timer-hook nil
  "*Hooks called after running the speedbar timer function")

(defvar speedbar-file-unshown-regexp
  (let ((nstr "") (noext completion-ignored-extensions))
    (while noext
      (setq nstr (concat nstr (regexp-quote (car noext)) "$"
			 (if (cdr noext) "\\|" ""))
	    noext (cdr noext)))
    (concat nstr "\\|#[^#]+#$\\|\\.\\.?$"))
  "*Regular expression matching files we don't want to display in a
speedbar buffer")

(defvar speedbar-file-regexp 
  (if speedbar-use-imenu-package
      "\\(\\.\\([CchH]\\|c\\(++\\|pp\\)\\|f90\\|ada\\|pl?\\|el\\|t\\(ex\\(i\\(nfo\\)?\\)?\\|cl\\)\\|emacs\\)$\\)\\|[Mm]akefile\\(\\.in\\)?"
    "\\.\\([CchH]\\|c\\(++\\|pp\\)\\|p\\|el\\|tex\\(i\\(nfo\\)?\\)?\\|emacs\\)$")
  "*Regular expresson matching files we know how to expand.")

(defvar speedbar-syntax-table nil
  "Syntax-table used on the speedbar")

(if speedbar-syntax-table
    nil
  (setq speedbar-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " speedbar-syntax-table)
  (modify-syntax-entry ?\" " " speedbar-syntax-table)
  (modify-syntax-entry ?( " " speedbar-syntax-table)
  (modify-syntax-entry ?) " " speedbar-syntax-table)
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

  (define-key speedbar-key-map "e" 'speedbar-edit-line)
  (define-key speedbar-key-map "+" 'speedbar-expand-line)
  (define-key speedbar-key-map "-" 'speedbar-contract-line)

  (if (string-match "XEmacs" emacs-version)
      (progn
	;; bind mouse bindings so we can manipulate the items on each line
	(define-key speedbar-key-map 'button2 'speedbar-click)

	;; Xemacs users.  You probably want your own toolbar for
	;; the speedbar frame or mode or whatever.  Make some buttons
	;; and mail me how to do it!
	;; Also, how do you disable all those menu items?  Email me that too
	;; as it would be most helpful.
	)
    ;; bind mouse bindings so we can manipulate the items on each line
    (define-key speedbar-key-map [mouse-2] 'speedbar-click)
    (define-key speedbar-key-map [down-mouse-2] 'speedbar-quick-mouse)

    ;; this was meant to do a rescan or something
    ;;(define-key speedbar-key-map [shift-mouse-2] 'speedbar-hard-click)

    ;; disable all menus - we don't have a lot of space to play with
    ;; in such a skinny frame.
    (define-key speedbar-key-map [menu-bar buffer] 'undefined)
    (define-key speedbar-key-map [menu-bar files] 'undefined)
    (define-key speedbar-key-map [menu-bar tools] 'undefined)
    (define-key speedbar-key-map [menu-bar edit] 'undefined)
    (define-key speedbar-key-map [menu-bar search] 'undefined)
    (define-key speedbar-key-map [menu-bar help-menu] 'undefined)

    ;; This lets the user scroll as if we had a scrollbar... well maybe not
    (define-key speedbar-key-map [mode-line mouse-2] 'speedbar-mouse-hscroll)

    ;; Create a menu for speedbar
    (setq speedbar-menu-map (make-sparse-keymap))
    (define-key speedbar-key-map [menu-bar speedbar] 
      (cons "Speedbar" speedbar-menu-map))
    (define-key speedbar-menu-map [close] 
      (cons "Close" 'speedbar-close-frame))
    (define-key speedbar-menu-map [clonfigure] 
      (cons "Configure Faces" 'speedbar-configure-faces))
    (define-key speedbar-menu-map [configopt] 
      (cons "Configure Options" 'speedbar-configure-options))
    (define-key speedbar-menu-map [Update] 
      (cons "Update" 'speedbar-update-contents))
    ))

(put 'speedbar-configure-faces 'menu-enable '(featurep 'dialog))
(put 'speedbar-configure-options 'menu-enable '(featurep 'dialog))

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

(defvar speedbar-shown-directories nil
  "Used to maintain list of directories simultaneously open in the current
speedbar.")


;;;
;;; Mode definitions/ user commands
;;;
;;;###autoload
(defun speedbar-frame-mode (&optional arg)
  "Enable or disable use of a speedbar.  Positive number means turn
on, negative turns speedbar off, and nil means toggle.  Once the
speedbar frame is activated, a buffer in `speedbar-mode' will be
displayed.  Currently, only one speedbar is supported at a time."
  (interactive "P")
  (if (not window-system)
      (error "Speedbar is not useful outside of a windowing environement"))
  ;; toggle frame on and off.
  (if (not arg) (if speedbar-frame (setq arg -1) (setq arg 1)))
  ;; turn the frame off on neg number
  (if (and (numberp arg) (< arg 0))
      (progn
	(run-hooks 'speedbar-before-delete-hook)
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
      (let ((params (list 
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
		     '(unsplittable . t) )))
	(setq speedbar-frame
	      (if (< emacs-minor-version 35)
		  (make-frame params)
		(let ((x-pointer-shape x-pointer-top-left-arrow)
		      (x-sensitive-text-pointer-shape x-pointer-hand2))
		  (make-frame params)))))
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
  "Create and return a SPEEDBAR buffer.  The speedbar buffer allows
the user to manage a list of directories and paths at different
depths.  The first line represents the default path of the speedbar
frame.  Each directory segment is a button which jumps speedbar's
default directory to that path.  Buttons are activated by clicking
mouse-2.

Each line starting with <+> represents a directory.  Click on the <+>
to insert the directory listing into the current tree.  Click on the
<-> to retract that list.  Click on the directory name to go to that
directory as the default.

Each line starting with [+] is a file.  If the variable
`speedbar-show-unknown-files' is t, the lines starting with [?] are
files which don't have imenu support, but are not expressly ignored.
Files are completely ignored if they match `speedbar-file-unshown-regexp'
which is generated from `completion-ignored-extensions'.

Click on the [+] to display a list of tags from that file.  Click on
the [-] to retract the list.  Click on the file name to edit the file
in the attached frame.

If you open tags, you might find a node starting with {+}, which is a
category of tags.  Click the {+} to expand the category.  Jumpable
tags start with >.  Click the name of the tag to go to that position
in the selected file.

Keybindings: \\<speedbar-key-map>
\\[speedbar-click]  Activate the button under the mouse.
\\[speedbar-edit-line]        Edit the file/directory on this line.  Same as clicking 
           on the name on the selected line.)
\\[speedbar-expand-line]        Expand the current line.  Same as clicking on the + on a line.
\\[speedbar-contract-line]        Contract the current line.  Same as clicking on the - on a line."
  (setq speedbar-buffer (set-buffer (get-buffer-create "SPEEDBAR")))
  (kill-all-local-variables)
  (setq major-mode 'speedbar-mode)
  (setq mode-name "SB")
  (use-local-map speedbar-key-map)
  (set-syntax-table speedbar-syntax-table)
  (setq mode-line-format
	'("<< SPEEDBAR " (line-number-mode " %3l ") " >>"))
  (setq font-lock-keywords nil) ;; no font-locking please
  (setq truncate-lines t)
  (if (not speedbar-xemacsp) (setq auto-show-mode nil))	;no auto-show for FSF
  (run-hooks 'speedbar-mode-hook)
  (speedbar-update-contents)
  )

(defun speedbar-mouse-hscroll (e)
  "Read a mouse event from the mode line, and horizontally scroll if the
mouse is being clicked on the far left, or far right of the modeline."
  (interactive "e")
  (let* ((xp (car (nth 2 (car (cdr e)))))
	 (cpw (/ (frame-pixel-width)
		 (frame-width)))
	 (oc (1+ (/ xp cpw)))
	 )
    (cond ((< oc 3)
	   (scroll-left 2))
	  ((> oc (- (window-width) 3))
	   (scroll-right 2))
	  (t (message "Click on the edge of the modeline to scroll left/right")))
    ;;(message "X: Pixel %d Char Pixels %d On char %d" xp cpw oc)
    ))


;;;
;;; Utility functions
;;;
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

(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate forms.  Turn read-only back
on when done."
  (list 'let '((speedbar-with-writable-buff (current-buffer)))
	'(toggle-read-only -1)
	(cons 'progn forms)
	'(save-excursion (set-buffer speedbar-with-writable-buff)
			 (toggle-read-only 1))))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun speedbar-make-button (start end face mouse function &optional token)
  "Create a button from START to END, with FACE as the display face
and MOUSE and the mouse face.  When this button is clicked on FUNCTION
will be run with the token parameter of TOKEN (any lisp object)"
  (put-text-property start end 'face face)
  (put-text-property start end 'mouse-face mouse)
  (put-text-property start end 'invisible nil)
  (if function (put-text-property start end 'speedbar-function function))
  (if token (put-text-property start end 'speedbar-token token))
  )

(defun speedbar-file-lists (directory)
  "Create file lists for DIRECTORY.  The car is the list of
directories, the cdr is list of files not matching ignored headers."
  (let ((default-directory directory)
	(dir (directory-files directory nil))
	(dirs nil)
	(files nil))
    (while dir
      (if (not (string-match speedbar-file-unshown-regexp (car dir)))
	  (if (file-directory-p (car dir))
	      (setq dirs (cons (car dir) dirs))
	    (setq files (cons (car dir) files))))
      (setq dir (cdr dir)))
    (cons (nreverse dirs) (list (nreverse files))))
  )

(defun speedbar-directory-buttons (directory index)
  "Inserts a single button group at point for DIRECTORY.  Each directory
path part is a different button.  If part of the path matches the user
directory ~, then it is replaced with a ~"
  (let* ((tilde (expand-file-name "~"))
	 (dd (expand-file-name directory))
	 (junk (string-match (regexp-quote tilde) dd))
	 (displayme (if junk
			(concat "~" (substring dd (match-end 0)))
		      dd))
	 (p (point)))
    (if (string-match "^~/?$" displayme) (setq displayme (concat tilde "/")))
    (insert displayme)
    (save-excursion
      (goto-char p)
      (while (re-search-forward "\\([^/]+\\)/" nil t)
	(speedbar-make-button (match-beginning 1) (match-end 1)
			      'speedbar-directory-face
			      'speedbar-highlight-face
			      'speedbar-directory-buttons-follow
			      (if (= (match-beginning 1) p)
				  (expand-file-name "~/")  ;the tilde
				(buffer-substring-no-properties
				 p (match-end 0))))))
    (if (string-match "^/[^/]+/$" displayme)
	(progn
	  (insert "  ")
	  (let ((p (point)))
	    (insert "<root>")
	    (speedbar-make-button p (point)
				  'speedbar-directory-face
				  'speedbar-highlight-face
				  'speedbar-directory-buttons-follow
				  "/"))))
    (insert-char ?\n 1 nil)))

(defun speedbar-make-tag-line (exp-button-type
			       exp-button-char exp-button-function
			       exp-button-data
			       tag-button tag-button-function tag-button-data
			       tag-button-face depth)
  "Creates a tag line with BUTTON-TYPE for the small button that
expands or contracts a node (if applicable), and BUTTON-CHAR the
character in it (+, -, ?, etc).  BUTTON-FUNCTION is the function to
call if it's clicked on.  Button types are 'bracket, 'angle, 'curly, or nil.

Next, TAG-BUTTON is the text of the tag.  TAG-FUNCTION is the function
to call if clicked on, and TAG-DATA is the data to attach to the text
field (such a tag positioning, etc).  TAG-FACE is a face used for this
type of tag.

Lastly, DEPTH shows the depth of expansion.

This function assumes that the cursor is in the speecbar window at the
position to insert a new item, and that the new item will end with a CR"
  (let ((start (point))
	(end (progn
	       (insert (int-to-string depth) ":")
	       (point))))
    (put-text-property start end 'invisible t)
    )
  (insert-char ?  depth nil)
  (put-text-property (- (point) depth) (point) 'invisible nil)
  (let* ((exp-button (cond ((eq exp-button-type 'bracket) "[%c]")
			   ((eq exp-button-type 'angle) "<%c>")
			   ((eq exp-button-type 'curly) "{%c}")
			   (t ">")))
	 (buttxt (format exp-button exp-button-char))
	 (start (point))
	 (end (progn (insert buttxt) (point)))
	 (bf (if exp-button-type 'speedbar-button-face nil))
	 (mf (if exp-button-function 'speedbar-highlight-face nil))
	 )
    (speedbar-make-button start end bf mf exp-button-function exp-button-data)
    )
  (insert-char ?  1 nil)
  (put-text-property (1- (point)) (point) 'invisible nil)
  (let ((start (point))
	(end (progn (insert tag-button) (point))))
    (insert-char ?\n 1 nil)
    (put-text-property (1- (point)) (point) 'invisible nil)
    (speedbar-make-button start end tag-button-face 
			  (if tag-button-function 'speedbar-highlight-face nil)
			  tag-button-function tag-button-data))
)

(defun speedbar-change-expand-button-char (char)
  "Change the expanson button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward ":\\s-*.\\([-+?]\\)" (save-excursion (end-of-line) 
								(point)) t)
	(speedbar-with-writable
	  (goto-char (match-beginning 1))
	  (delete-char 1)
	  (insert-char char 1 t)))))


;;;
;;; Build button lists
;;;
(defun speedbar-insert-files-at-point (files level)
  "Insert list of FILES starting at point, and indenting all files to LEVEL
depth.  Tag exapndable items with a +, otherwise a ?.  Don't highlight ? as
we don't know how to manage them.  The input parameter FILES is a cons
cell of the form ( 'dir-list . 'file-list )"
  ;; Start inserting all the directories
  (let ((dirs (car files)))
    (while dirs
      (speedbar-make-tag-line 'angle ?+ 'speedbar-dired (car dirs)
			      (car dirs) 'speedbar-dir-follow nil
			      'speedbar-directory-face level)
      (setq dirs (cdr dirs))))
  (let ((lst (car (cdr files))))
    (while lst
      (let* ((known (string-match speedbar-file-regexp (car lst)))
	     (expchar (if known ?+ ??))
	     (fn (if known 'speedbar-tag-file nil)))
	(if (or speedbar-show-unknown-files (/= expchar ??))
	    (speedbar-make-tag-line 'bracket expchar fn (car lst)
				    (car lst) 'speedbar-find-file nil
				    'speedbar-file-face level)))
      (setq lst (cdr lst)))))

(defun speedbar-default-directory-list (directory index)
  "Inserts files for DIRECTORY with level INDEX at point"
  (speedbar-insert-files-at-point
   (speedbar-file-lists directory) index)
  )

(defun speedbar-insert-generic-list (level lst expand-fun find-fun)
  "At LEVEL, inserts a generic multi-level alist LIST.  Associations with
lists get {+} tags (to expand into more nodes) and those with positions
just get a > as the indicator.  {+} buttons will have the function
EXPAND-FUN and the token is the CDR list.  The token name will have the
function FIND-FUN and not token."
  ;; Remove imenu rescan button
  (if (string= (car (car lst)) "*Rescan*")
      (setq lst (cdr lst)))
  ;; insert the parts
  (while lst
    (cond ((null (car-safe lst)) nil)	;this would be a separator
	  ((numberp (cdr-safe (car-safe lst)))
	   (speedbar-make-tag-line nil nil nil nil ;no expand button data
				   (car (car lst)) ;button name
				   find-fun ;function
				   (cdr (car lst)) ;token is position
				   'speedbar-tag-face 
				   (1+ level)))
	  ((listp (cdr-safe (car-safe lst)))
	   (speedbar-make-tag-line 'curly ?+ expand-fun (cdr (car lst))
				   (car (car lst)) ;button name
				   nil nil 'speedbar-tag-face 
				   (1+ level)))
	  (t (message "Ooops!")))
    (setq lst (cdr lst))))

;;;
;;; Timed functions
;;;
(defun speedbar-update-contents ()
  "Update the contents of the speedbar buffer."
  (interactive)
  (setq speedbar-last-selected-file nil)
  (setq speedbar-shown-directories (list (expand-file-name default-directory)))
  (let ((cbd default-directory)
	(funclst speedbar-initial-expansion-list))
    (save-excursion
      (set-buffer speedbar-buffer)
      (speedbar-with-writable
	(setq default-directory cbd)
	(delete-region (point-min) (point-max))
	(while funclst
	  (funcall (car funclst) cbd 0)
	  (setq funclst (cdr funclst)))))))

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
		(if (or (member (expand-file-name default-directory)
				speedbar-shown-directories)
			(eq af speedbar-frame)
			(not (buffer-file-name))
			)
		    nil
		  (message "Updating speedbar to: %s..." default-directory)
		  (speedbar-update-contents)
		  (message "Updating speedbar to: %s...done" default-directory)))))
      ;; Reset the timer
      (speedbar-set-timer speedbar-update-speed)
      ;; Ok, un-underline old file, underline current file
      (speedbar-update-current-file)))
  (run-hooks 'speedbar-timer-hook)
  )

(defun speedbar-update-current-file ()
  "Find out what the current file is, and update our visuals to indicate
what it is.  This is specific to file names."
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
	  (speedbar-with-writable
	    (goto-char (point-min))
	    (if (and 
		 speedbar-last-selected-file
		 (re-search-forward 
		  (concat " \\(" (regexp-quote speedbar-last-selected-file) "\\)\n")
		  nil t))
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face
				   'speedbar-file-face))
	    (goto-char (point-min))
	    (if (re-search-forward 
		 (concat " \\(" (regexp-quote newcf) "\\)\n") nil t)
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face 
				   'speedbar-selected-face))
	    (setq speedbar-last-selected-file newcf))
	  (forward-line -1)
	  (speedbar-position-cursor-on-line)
	  (set-buffer lastb)
	  (select-frame lastf)))))

;;;
;;; Clicking Activity
;;;
(defun speedbar-quick-mouse (e)
  "Since mouse events are strange, this will keep the mouse nicely
positioned."
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (forward-char 3)
  )

(defun speedbar-position-cursor-on-line ()
  "Position the cursor on a line."
  (beginning-of-line)
  (re-search-forward "[]>}]" (save-excursion (end-of-line) (point)) t))

(defun speedbar-line-path (depth)
  "Retrieve the pathname associated with the current line.  This may
require traversing backwards and combinding the default directory with
these items."
  (save-excursion
    (let ((path nil))
      (setq depth (1- depth))
      (while (/= depth -1)
	(if (not (re-search-backward (format "^%d:" depth) nil t))
	    (error "Error building path of tag")
	  (cond ((looking-at "[0-9]+:\\s-*<->\\s-+\\([^\n]+\\)$")
		 (setq path (concat (buffer-substring-no-properties
				     (match-beginning 1) (match-end 1))
				    "/"
				    path)))
		((looking-at "[0-9]+:\\s-*[-]\\s-+\\([^\n]+\\)$")
		 ;; This is the start of our path.
		 (setq path (buffer-substring-no-properties
			     (match-beginning 1) (match-end 1))))))
	(setq depth (1- depth)))
      (concat default-directory path))))

(defun speedbar-edit-line ()
  "Edit whatever tag or file is on the current speedbar line."
  (interactive)
  (beginning-of-line)
  (re-search-forward "[]>}] [a-zA-Z0-9]" (save-excursion (end-of-line) (point)))
  (speedbar-do-function-pointer))

(defun speedbar-expand-line ()
  "Expand the line under the cursor."
  (interactive)
  (beginning-of-line)
  (re-search-forward ":\\s-*.\\+. " (save-excursion (end-of-line) (point)))
  (forward-char -2)
  (speedbar-do-function-pointer))

(defun speedbar-contract-line ()
  "Expand the line under the cursor."
  (interactive)
  (beginning-of-line)
  (re-search-forward ":\\s-*.-. " (save-excursion (end-of-line) (point)))
  (forward-char -2)
  (speedbar-do-function-pointer))

(defun speedbar-click (e)
  "When the user clicks mouse 1 on our speedbar, we must decide what
we want to do!  The entire speedbar has functions attached to
buttons.  All we have to do is extract from the buffer the information
we need.  See `speedbar-mode' for the type of behaviour we want to achieve"
  (interactive "e")
  (mouse-set-point e)
  (speedbar-do-function-pointer))

(defun speedbar-do-function-pointer ()
  "Look under the cursor and examine the text properties.  From this extract
the file/tag name, token, indentation level and call a function if apropriate"
  (let* ((fn (get-text-property (point) 'speedbar-function))
	 (tok (get-text-property (point) 'speedbar-token))
	 ;; The 1-,+ is safe because scaning starts AFTER the point
	 ;; specified.  This lets the search include the character the
	 ;; cursor is on.
	 (tp (previous-single-property-change 
	      (if (get-text-property (1+ (point)) 'speedbar-function)
		  (1+ (point)) (point)) 'speedbar-function))
	 (np (next-single-property-change 
	      (if (and (> (point) 1) (get-text-property (1- (point)) 'speedbar-function))
		  (1- (point)) (point)) 'speedbar-function))
	 (txt (buffer-substring-no-properties (or tp (point-min))
					      (or np (point-max))))
	 (dent (save-excursion (beginning-of-line) 
			       (string-to-number 
				(if (looking-at "[0-9]+")
				    (buffer-substring-no-properties
				    (match-beginning 0) (match-end 0))
				  "0")))))
    ;;(message "%S:%S:%S:%s" fn tok txt dent)
    (and fn (funcall fn txt tok dent)))
  (speedbar-position-cursor-on-line))

(defun speedbar-find-file (text token indent)
  "Speedbar click handler for filenames.  Clicking the filename loads
that file into the attached buffer."
  (let ((cdd (speedbar-line-path indent)))
    (select-frame speedbar-attached-frame)
    (find-file (concat cdd text))
    (speedbar-update-current-file)
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)))

(defun speedbar-dir-follow (text token indent)
  "Speedbar click handler for directory names.  Clicking a directory will
cause the speedbar to list files in the selected subdirectory."
  (setq default-directory 
	(concat (expand-file-name (concat (speedbar-line-path indent) text))
		"/"))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed)
  (setq speedbar-last-selected-file nil)
  (speedbar-update-current-file))


(defun speedbar-dired (text token indent)
  "Speedbar click handler for filenames.  Clicking the filename loads
that file into the attached buffer."
  (cond ((string-match "+" text)	;we have to expand this file
	 (setq speedbar-shown-directories 
	       (cons (expand-file-name 
		      (concat (speedbar-line-path indent) token "/"))
		     speedbar-shown-directories))
	 (speedbar-change-expand-button-char ?-)
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   (speedbar-with-writable
	     (speedbar-default-directory-list 
	      (concat (speedbar-line-path indent) token "/")
	      (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (let ((oldl speedbar-shown-directories)
	       (newl nil)
	       (td (expand-file-name 
		    (concat (speedbar-line-path indent) token))))
	   (while oldl
	     (if (not (string-match (concat "^" (regexp-quote td)) (car oldl)))
		 (setq newl (cons (car oldl) newl)))
	     (setq oldl (cdr oldl)))
	   (setq speedbar-shown-directories newl))
	 (speedbar-change-expand-button-char ?+)
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   (speedbar-with-writable
	     (if (save-excursion (re-search-forward (format "^%d:" indent) nil t))
		 (delete-region (point) (match-beginning 0))
	       (delete-region (point) (point-max)))))
	 )
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly)
  (setq speedbar-last-selected-file nil)
  (save-excursion (speedbar-update-current-file)))

(defun speedbar-directory-buttons-follow (text token ident)
  "Speedbar click handler for default directory buttons."
  (setq default-directory token)
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed))

(defun speedbar-tag-file (text token indent)
  "The cursor is on a selected line.  Expand the tags in the specified
file.  The parameter TXT and TOK are required, where TXT is the button
clicked, and TOK is the file to expand."
  (cond ((string-match "+" text)	;we have to expand this file
	 (let* ((fn (expand-file-name (concat (speedbar-line-path indent)
					      token)))
		(lst (if speedbar-use-imenu-package
			(let ((tim (speedbar-fetch-dynamic-imenu fn)))
			  (if (eq tim t)
			      (speedbar-fetch-dynamic-etags fn)
			    tim))
		      (speedbar-fetch-dynamic-etags fn))))
	   ;; if no list, then remove expando button
	   (if (not lst)
	       (speedbar-change-expand-button-char ??)
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (speedbar-insert-generic-list indent
					       lst 'speedbar-tag-expand
					       'speedbar-tag-find))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (if (save-excursion (re-search-forward (format "^%d:" indent) nil t))
		 (delete-region (point) (match-beginning 0))
	       (delete-region (point) (point-max))))))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

(defun speedbar-tag-find (text token indent)
  "For the tag in a file, goto that position"
  (let ((file (speedbar-line-path indent)))
    (select-frame speedbar-attached-frame)
    (find-file file)
    (save-excursion (speedbar-update-current-file))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)
    (goto-char token)))

(defun speedbar-tag-expand (text token indent)
  "For the tag in a file which is really a list of tags of a certain type,
expand or contract that list."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (speedbar-insert-generic-list indent
					   token 'speedbar-tag-expand
					   'speedbar-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (if (save-excursion (re-search-forward (format "^%d:" indent) nil t))
		 (delete-region (point) (match-beginning 0))))))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

;;;
;;; Centering Utility
;;;
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
    ;; too big
    (let (depth start end exp p)
      (save-excursion
	(beginning-of-line)
	(setq depth (if (looking-at "[0-9]+")
			(string-to-int (buffer-substring-no-properties
					(match-beginning 0) (match-end 0)))
		      0))
	(setq exp (format "^%d:\\s-*[[{<]\\([?+-]\\)[]>}]" depth)))
      (save-excursion
	(end-of-line)
	(if (re-search-backward exp nil t)
	    (setq start (point))
	  (error "Center error"))
	(save-excursion			;Not sure about this part.
	  (end-of-line)
	  (setq p (point))
	  (while (and (not (re-search-forward exp nil t))
		      (>= depth 0))
	    (setq depth (1- depth))
	    (setq exp (format "^%d:\\s-*[[{<]\\([?+-]\\)[]>}]" depth)))
	  (if (/= (point) p)
	      (setq end (point))
	    (setq end (point-max)))))
      ;; Now work out the details of centering
      (let ((nl (count-lines start end))
	    (cp (point)))
	(if (> nl (window-height (selected-window)))
	    ;; We can't fit it all, so just center on cursor
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
	(goto-char cp)))))


;;;
;;; Tag Management -- Imenu
;;;
(defun speedbar-fetch-dynamic-imenu (file)
  "Use the imenu package to load in file, and extract all the items
tags we wish to display in the speedbar package."
  (eval-when-compile (require 'imenu))
  (save-excursion
    (set-buffer (find-file-noselect file))
    (condition-case nil
	(imenu--make-index-alist t)
      (error t))))


;;;
;;; Tag Management -- etags  (Not useful for FSF emacs)
;;;
(defvar speedbar-fetch-etags-parse-list
  '(("\\.\\([cChH]\\|c++\\|cpp\\|cc\\)$" . speedbar-parse-c-or-c++tag)
    ("\\.el\\|\\.emacs" .
     "defun\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
    ("\\.tex$" . speedbar-parse-tex-string)
    ("\\.p" .
     "\\(\\(FUNCTION\\|function\\|PROCEDURE\\|procedure\\)\\s-+\\([a-zA-Z0-9_.:]+\\)\\)\\s-*(?^?")

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
    (reverse newlist)))

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


;;;
;;; configuration scripts (optional)
;;;
(defun speedbar-configure-options ()
  "Configure variable options for the speedbar program using dlg-config"
  (interactive)
  (require 'dlg-config)
  (save-excursion
    (select-frame speedbar-attached-frame)
    (dlg-init)
    (let ((oframe (create-widget "Speedbar Options" widget-frame 
				 widget-toplevel-shell
				 :x 2 :y -3
				 :frame-label "Speedbar Options"))
	)
      (create-widget "show-unknown" widget-toggle-button oframe
		     :x 1 :y 1 :label-value "Show files that are not supported by imenu"
		     :state (data-object-symbol "speedbar-show-unknown-files"
						:value speedbar-show-unknown-files
						:symbol 'speedbar-show-unknown-files))
      
      (create-widget "raiselower" widget-toggle-button oframe
		     :x 1 :y -1 :label-value "Use frame auto raise/lower property"
		     :state (data-object-symbol "speedbar-raise-lower"
						:value speedbar-raise-lower
						:symbol 'speedbar-raise-lower))
      
      (create-widget "update-speed" widget-label oframe
		     :x 1 :y -2 :label-value "Update Delay    :")
      (create-widget "update-speed-txt" widget-text-field oframe
		     :width 5 :height 1 :x -2 :y t 
		     :value (data-object-symbol-string-to-int 
			     "update-speed"
			     :symbol 'speedbar-update-speed
			     :value  (int-to-string speedbar-update-speed)))
      (create-widget "update-speed-unit" widget-label oframe
		     :x -3 :y t :label-value "Seconds")
      
      (create-widget "navigating-speed" widget-label oframe
		     :x 1 :y -1 :label-value "Navigating Delay:")
      (create-widget "navigating-speed-txt" widget-text-field oframe
		     :width 5 :height 1 :x -2 :y t 
		     :value (data-object-symbol-string-to-int 
			     "navigating-speed"
			     :symbol 'speedbar-navigating-speed
			     :value  (int-to-string speedbar-navigating-speed)))
      (create-widget "navigating-speed-unit" widget-label oframe
		     :x -3 :y t :label-value "Seconds")

      (create-widget "width" widget-label oframe
		     :x 1 :y -2 :label-value "Display Width   :")
      (create-widget "width-txt" widget-text-field oframe
		     :width 5 :height 1 :x -2 :y t 
		     :value (data-object-symbol-string-to-int 
			     "width"
			     :symbol 'speedbar-width
			     :value  (int-to-string speedbar-width)))
      (create-widget "width-unit" widget-label oframe
		     :x -3 :y t :label-value "Characters")
      
      (create-widget "scrollbar-width" widget-label oframe
		     :x 1 :y -1 :label-value "Scrollbar Width :")
      (create-widget "scrollbar-width-txt" widget-text-field oframe
		     :width 5 :height 1 :x -2 :y t 
		     :value (data-object-symbol-string-to-int 
			     "width"
			     :symbol 'speedbar-width
			     :value  (int-to-string speedbar-scrollbar-width)))
      (create-widget "scrollbar-width-unit" widget-label oframe
		     :x -3 :y t :label-value "Pixels")
      
      
      )
    (dlg-end)
    (dialog-refresh)
    ))

(defun speedbar-configure-faces ()
  "Configure faces for the speedbar program using dlg-config."
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

;;;
;;; Color loading section  This is message *Blech!*
;;;
(defun speedbar-load-color (sym l-fg l-bg d-fg d-bg &optional bold italic underline)
  "Create a color for SYM with a L-FG and L-BG color, or D-FG and
D-BG. Optionally make BOLD, ITALIC, or UNDERLINED if applicable.  If
the background attribute of the current frame is determined to be
light (white, for example) then L-FG and L-BG is used.  If not, then
D-FG and D-BG is used.  This will allocate the colors in the best
possible mannor.  This will allow me to store multiple defaults and
dynamically determine which colors to use."
  (let* ((params (frame-parameters))
	 (disp-res (if (fboundp 'x-get-resource)
		       (if speedbar-xemacsp
			   (x-get-resource ".displayType" "DisplayType" 'string)
			 (x-get-resource ".displayType" "DisplayType"))
		     nil))
	 (display-type
	  (cond (disp-res (intern (downcase disp-res)))
		((and (fboundp 'x-display-color-p) (x-display-color-p)) 'color)
		(t 'mono)))
	 (bg-res (if (fboundp 'x-get-resource)
		     (if speedbar-xemacsp
			 (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
		       (x-get-resource ".backgroundMode" "BackgroundMode"))
		   nil))
	 (bgmode
	  (cond (bg-res (intern (downcase bg-res)))
		((and params 
		      (fboundp 'x-color-values)
		      (< (apply '+ (x-color-values
				    (cdr (assq 'background-color params))))
			 (/ (apply '+ (x-color-values "white")) 3)))
		 'dark)
		(t 'light)))		;our default
	 (set-p (function (lambda (face-name resource)
			    (if speedbar-xemacsp
				(x-get-resource 
				 (concat face-name ".attribute" resource)
				 (concat "Face.Attribute" resource)
				 'string)
			      (x-get-resource 
			       (concat face-name ".attribute" resource)
			       (concat "Face.Attribute" resource)))
			    )))
	 (nbg (cond ((eq bgmode 'dark) d-bg) 
		    (t l-bg)))
	 (nfg (cond ((eq bgmode 'dark) d-fg)
		    (t l-fg))))

    (if (not (eq display-type 'color))
	;; we need a face of some sort, so just make due with default
	(progn
	  (copy-face 'default sym)
	  (if bold (condition-case nil
		       (make-face-bold sym)
		     (error (message "Cannot make face %s bold!" 
				     (symbol-name sym)))))
	  (if italic (condition-case nil
			 (make-face-italic sym)
		       (error (message "Cannot make face %s italic!"
				       (symbol-name sym)))))
	  (set-face-underline-p sym underline)
	  )
      ;; make a colorized version of a face.  Be sure to check Xdefaults
      ;; for possible overrides first!
      (let ((newface (make-face sym)))
	;; For each attribute, check if it might already be set by Xdefaults
	(if (and nfg (not (funcall set-p (symbol-name sym) "Foreground")))
	    (set-face-foreground sym nfg))
	(if (and nbg (not (funcall set-p (symbol-name sym) "Background")))
	    (set-face-background sym nbg))
	
	(if bold (condition-case nil
		     (make-face-bold sym)
		   (error (message "Cannot make face %s bold!"
				       (symbol-name sym)))))
	(if italic (condition-case nil
		       (make-face-italic sym)
		     (error (message "Cannot make face %s italic!"
				     (symbol-name sym)))))
	(set-face-underline-p sym underline)
	))))

(if (x-display-color-p)
    (progn
      (speedbar-load-color 'speedbar-button-face "green4" nil "green3" nil nil nil nil)
      (speedbar-load-color 'speedbar-file-face "cyan4" nil "cyan" nil nil nil nil)
      (speedbar-load-color 'speedbar-directory-face "blue4" nil "light blue" nil nil nil nil)
      (speedbar-load-color 'speedbar-tag-face "brown" nil "yellow" nil nil nil nil)
      (speedbar-load-color 'speedbar-selected-face "red" nil "red" nil nil nil t)
      (speedbar-load-color 'speedbar-highlight-face nil "green" nil "sea green" nil nil nil)
      ) ; color
  (make-face 'speedbar-button-face)
  ;;(make-face 'speedbar-file-face)
  (copy-face 'bold 'speedbar-file-face)
  (make-face 'speedbar-directory-face)
  (make-face 'speedbar-tag-face)
  ;;(make-face 'speedbar-selected-face)
  (copy-face 'underline 'speedbar-selected-face)
  ;;(make-face 'speedbar-highlight-face)
  (copy-face 'highlight 'speedbar-highlight-face)

  ) ;; monochrome

;;; end of lisp
(provide 'speedbar)