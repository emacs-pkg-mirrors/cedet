;;; speedbar - quick access to files and tags -*-byte-compile-warnings:nil;-*-
;;;
;;; Copyright (C) 1996, 1997 Eric M. Ludlam
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: speedbar.el,v 1.24 1997/02/07 04:28:26 zappo Exp $
;;; Version: 0.4
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
;;;   To add new files types into the speedbar, use the function
;;; `speedbar-add-supported-extension' If speedbar complains that the
;;; file type is not supported, that means there is no built in
;;; support from imenu, and the etags part wasn't set up correctly.
;;;
;;;   To add new file types to imenu, see the documentation in the
;;; file imenu.el that comes with emacs.  To add new file types which
;;; etags supports, you need to modify the variable
;;; `speedbar-fetch-etags-parse-list'.
;;;
;;;    If the updates are going too slow for you, modify the variable
;;; `speedbar-update-speed' to a longer idle time before updates.
;;;
;;;    If you navigate directories, you will probably notice that you
;;; will navigate to a directory which is eventually replaced after
;;; you go back to editing a file (unless you pull up a new file.)
;;; The delay time before this happens is in
;;; `speedbar-navigating-speed', and defaults to 10 seconds.
;;;
;;;    XEmacs users may want to change the default timeouts for
;;; `speedbar-update-speed' to something longer as XEmacs doesn't have
;;; idle timers, the speedbar timer keeps going off arbitrarilly while
;;; you're typing.  It's quite pesky.
;;;
;;;    Users of emacs previous to to v 19.31 (when idle timers
;;; where introduced) will not have speedbar updating automatically.
;;; Use "r" to refresh the display after changing directories.
;;; Remember, do not interrupt the stealthy updates or you display may
;;; not be completely refreshed.
;;;
;;;    See optional file `speedbcfg.el' for interactive buffers
;;; allowing simple configuration of colors and features of speedbar.
;;;
;;;    AUC-TEX users: The imenu tags for AUC-TEX mode don't work very
;;; well.  Use the imenu keywords from tex-mode.el for better results.

;;; Speedbar updates can be found at:
;;; ftp://ftp.ultranet.com/pub/zappo/speedbar*.el
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
;;;       Added pascal support to etags from Dave Penkler <dave_penkler@grenoble.hp.com>
;;;       Fixed centering algorithm
;;;       Tried to choose background independent colors.  Made more robust.
;;;       Rearranged code into a more logical order
;;; 0.3.1 Fixed doc & broken keybindings
;;;       Added mode hooks.
;;;       Improved color selection to be background mode smart
;;;       `nil' passed to `speedbar-frame-mode' now toggles the frame as
;;;         advertised in the doc string
;;; 0.4a  Added modified patch from Dan Schmidt <dfan@lglass.com> allowing a
;;;         directory cache to be maintained speeding up revisiting of files.
;;;       Default raise-lower behavior is now off by default.
;;;       Added some menu items for edit expand and contract.
;;;       Pre 19.31 emacsen can run without idle timers.
;;;       Added some patch information from Farzin Guilak <farzin@protocol.com>
;;;         adding xemacs specifics, and some etags upgrades.
;;;       Added ability to set a faces symbol-value to a string
;;;         representing the desired foreground color.  (idea from
;;;         Farzin Guilak, but implemented differently)
;;;       Fixed problem with 1 character buttons.
;;;       Added support for new Imenu marker technique.
;;;       Added `speedbar-load-hooks' for things to run only once on
;;;         load such as updating one of the many lists.
;;;       Added `speedbar-supported-extension-expressions' which is a
;;;         list of extensions that speedbar will tag.  This variable
;;;         should only be updated with `speedbar-add-supported-extension'
;;;       Moved configure dialog support to a separate file so
;;;         speedbar is not dependant on eieio to run
;;;       Fixed list-contraction problem when the item was at the end
;;;         of a sublist.
;;;       Fixed XEmacs multi-frame timer selecting bug problem.
;;;       Added `speedbar-ignored-modes' which is a list of major modes
;;;         speedbar will not follow when it is displayed in the selected frame
;;; 0.4   When the file being edited is not in the list, and is a file
;;;         that should be in the list, the speedbar cache is replaced.     
;;;       Temp buffers are now shown in the attached frame not the
;;;         speedbar frame
;;;       New variables `speedbar-vc-*' and `speedbar-stealthy-function-list'
;;;         added.  `speedbar-update-current-file' is now a member of
;;;         the stealthy list.  New function `speedbar-check-vc' will
;;;         examine each file and mark it if it is checked out.  To
;;;         add new version control types, override the function
;;;         `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p'.
;;;         The stealth list is interruptible so that long operations
;;;         do not interrupt someones editing flow.  Other long
;;;         speedbar updates will be added to the stealthy list in the
;;;         future should interesting ones be needed.
;;;       Added many new functions including:
;;;         `speedbar-item-byte-compile' `speedbar-item-load'
;;;         `speedbar-item-copy' `speedbar-item-rename' `speedbar-item-delete'
;;;         and `speedbar-item-info'
;;;       If the user kills the speedbar buffer in some way, the frame will
;;;         be removed.
;;; 0.4.1 Bug fixes
;;;       <mark.jeffries@nomura.co.uk> added `speedbar-do-updates',
;;;         XEmacs fixes for menus, and tag sorting, and quit key.
;;;       Modeline now updates itself based on window-width.
;;;       Frame is cached when closed to make pulling it up again faster.
;;;       Speedbars window is now marked as dedicated.
;;;       Added bindings: <grossjoh@charly.informatik.uni-dortmund.de>
;;;       Long directories are now span multiple lines autmoatically
;;;       Added `speedbar-directory-button-trim-method' to specify how to
;;;         sorten the directory button to fit on the screen.
;;;
;;; TODO:
;;; 1) Implement SHIFT-mouse2 to rescan buffers with imenu.
;;; 2) More functions to create buttons and options
;;; 3) filtering algoritms to reduce the number of tags/files displayed.
;;; 4) Timeout directories we haven't visited in a while.

(require 'assoc)

(defvar speedbar-xemacsp (string-match "XEmacs" emacs-version))

(defvar speedbar-initial-expansion-list
  '(speedbar-directory-buttons speedbar-default-directory-list)
  "*List of functions to call to fill in the speedbar buffer.
Whenever a top level update is issued all functions in this list are
run.  These functions will allways get the default directory to use
passed in as the first parameter, and a 0 as the second parameter.
The 0 indicates the uppermost indentation level.  They must assume
that the cursor is at the postion where they start inserting
buttons.")

(defvar speedbar-stealthy-function-list
  '(speedbar-update-current-file speedbar-check-vc)
  "*List of functions to periodically call stealthely.
Each function must return `nil' if interrupted, or `t' if completed.
Stealthy functions which have a single operation should always return
t.  Functions which take a long time should maintain a state (where
they are in their speedbar related calculations) and permit
interruption.  See `speedbar-check-vc' as a good example.")

(defvar speedbar-show-unknown-files nil
  "*Non-nil shows files we can't expand with a ? in the expand button.
`nil' means don't show the file in the list.")

;; Xemacs timers aren't based on idleness.  Therefore tune it down a little
;; or suffer mightilly!
(defvar speedbar-update-speed (if speedbar-xemacsp 5 1)
  "*Idle time in seconds needed before speedbar will update itself.
Updates occur to allow speedbar to display directory information
relevant to the buffer you are currently editing.")
(defvar speedbar-navigating-speed 10
  "*Idle time to wait after navigation commands in speedbar are executed.
Navigation commands included expanding/contracting nodes, and moving
between different directories.")

(defvar speedbar-frame-parameters (list 
				   ;; Xemacs fails to delete speedbar
				   ;; if minibuffer is off.
				   (cons 'minibuffer 
					 (if speedbar-xemacsp t nil))
				   '(width . 20)
				   '(scroll-bar-width . 10)
				   '(border-width . 0)
				   '(unsplittable . t) )
  "*Parameters to use when creating the speedbar frame.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame speedbar
is attached to.  To add more frame defaults, `cons' new alist members
onto this variable through the `speedbar-load-hooks'")

(defvar speedbar-use-imenu-package (stringp (locate-library "imenu"))
  "*t to use imenu for file parsing.  nil to use etags.
XEmacs doesn't support imenu, therefore the default is to use etags
instead.  Etags support is not as robust as imenu support.")

(defvar speedbar-sort-tags nil
  "*If Non-nil, sort tags in the speedbar display.  (Etags only)
See imenu.el source for how imenu does sorting.")

(defvar speedbar-directory-button-trim-method 'span
  "*Indicates how the directory button will be displayed.  Possible values
are:
 'span - span large directories over multiple lines.
 'trim - trim large directories to only show the last few.
 nil   - no trimming.")

(defvar speedbar-before-delete-hook nil
  "*Hooks called before deletiing the speedbar frame.")

(defvar speedbar-mode-hook nil
  "*Hooks called after creating a speedbar buffer.")

(defvar speedbar-timer-hook nil
  "*Hooks called after running the speedbar timer function.")

(defvar speedbar-verbosity-level 1
  "*Verbosity level of the speedbar.  0 means say nothing.
1 means medium level verbosity.  2 and higher are higher levels of
verbosity.")

(defvar speedbar-vc-indicator " *"
  "*Text used to mark files which are currently checked out.
Currently only RCS is supported.  Other version control systems can be
added by examining the function `speedbar-this-file-in-vc' and 
`speedbar-vc-check-dir-p'")

(defvar speedbar-vc-do-check t
  "*Non-nil check all files in speedar to see if they have been checked out.
Any file checked out is marked with `speedbar-vc-indicator'")

(defvar speedbar-vc-to-do-point nil
  "Local variable used to maintain the list of files for stealthy
examination of version control")

(defvar speedbar-ignored-modes
  '(Info-mode)
  "*List of major modes which speedbar will not switch directories for.")

(defvar speedbar-file-unshown-regexp
  (let ((nstr "") (noext completion-ignored-extensions))
    (while noext
      (setq nstr (concat nstr (regexp-quote (car noext)) "$"
			 (if (cdr noext) "\\|" ""))
	    noext (cdr noext)))
    (concat nstr "\\|#[^#]+#$\\|\\.\\.?$"))
  "*Regexp matching files we don't want displayed in a speedbar buffer.
It is generated from the variable `completion-ignored-extensions'")

(defvar speedbar-supported-extension-expressions
  (append '(".[CcHh]\\(++\\|pp\\|c\\|h\\)?" ".tex\\(i\\(nfo\\)?\\)?"
	    ".el" ".emacs" ".p")
	  (if speedbar-use-imenu-package
	      '(".java" ".f90" ".ada" ".pl" ".tcl" "Makefile\\(\\.in\\)?")))
  "*List of regular expressions which will match files supported by tagging.
Do not prefix the `.' char with a double \\ to quote it, as the period
will be stripped by a simplified optimizer when compiled into a
singular expression.  This variable will be turned into
`speedbar-file-regexp' for use with speedbar.  You should use the
function `speedbar-add-supported-extension' to add a new extension at
runtime, or use the configuration dialog to set it in your .emacs
file.")

(defun speedbar-extension-list-to-regex (extlist)
  "Takes EXTLIST, a list of extensions and transforms it into regexp.
All the preceeding . are stripped for an optimized expression starting
with . followed by extensions, followed by full-filenames."
  (let ((regex1 nil) (regex2 nil))
    (while extlist
      (if (= (string-to-char (car extlist)) ?.)
	  (setq regex1 (concat regex1 (if regex1 "\\|" "")
			       (substring (car extlist) 1)))
	(setq regex2 (concat regex2 (if regex2 "\\|" "") (car extlist))))
      (setq extlist (cdr extlist)))
    ;; concat all the sub-exressions together.
    (concat "\\(\\(\\.\\(" regex1 "\\)\\)"
	(if regex2 (concat "\\|\\(" regex2 "\\)") "")
	"\\)$")))
	  
(defvar speedbar-file-regexp 
  (speedbar-extension-list-to-regex speedbar-supported-extension-expressions)
  "Regular expresson matching files we know how to expand.
Created from `speedbar-supported-extension-expression' with the
function `speedbar-extension-list-to-regex'")

(defun speedbar-add-supported-extension (ext)
  "Adds EXTENSION as a new supported extention for speedbar tagging.
This should start with a `.' if it is not a complete file name, and
the dot should NOT be quoted in with \\.  Other regular expression
matchers are allowed however.  EXTENSION may be a single string or a
list of strings."
  (if (not (listp ext)) (setq ext (list ext)))
  (while ext
    (if (member (car ext) speedbar-supported-extension-expressions)
      nil
    (setq speedbar-supported-extension-expressions
	  (cons (car ext) speedbar-supported-extension-expressions)
	  speedbar-file-regexp
	  (speedbar-extension-list-to-regex
	   speedbar-supported-extension-expressions)))))

(defvar speedbar-do-update t
  "*Indicate wether the speedbar should do automatic updates.  When
this is `nil' then speedbar will not follow the attached frame's path.")

(defvar speedbar-syntax-table nil
  "Syntax-table used on the speedbar.")

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

(autoload 'speedbar-configure-options "speedbcfg" "Configure speedbar variables" t)
(autoload 'speedbar-configure-faces "speedbcfg" "Configure speedbar faces" t)

(if speedbar-key-map
    nil
  (setq speedbar-key-map (make-keymap))
  (suppress-keymap speedbar-key-map t)

  (define-key speedbar-key-map "e" 'speedbar-edit-line)
  (define-key speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key speedbar-key-map "+" 'speedbar-expand-line)
  (define-key speedbar-key-map "-" 'speedbar-contract-line)
  (define-key speedbar-key-map "r" 'speedbar-refresh)
  (define-key speedbar-key-map "t" 'speedbar-toggle-updates)
  (define-key speedbar-key-map "n" 'speedbar-next)
  (define-key speedbar-key-map "p" 'speedbar-prev)
  (define-key speedbar-key-map " " 'speedbar-scroll-up)
  (define-key speedbar-key-map "\C-?" 'speedbar-scroll-down)
  
  (define-key speedbar-key-map "q" 'speedbar-frame-mode)

  ;; After much use, I suddenly desired in my heart to perform dired
  ;; style operations since the directory was RIGHT THERE!
  (define-key speedbar-key-map "I" 'speedbar-item-info)
  (define-key speedbar-key-map "B" 'speedbar-item-byte-compile)
  (define-key speedbar-key-map "L" 'speedbar-item-load)
  (define-key speedbar-key-map "C" 'speedbar-item-copy)
  (define-key speedbar-key-map "D" 'speedbar-item-delete)
  (define-key speedbar-key-map "R" 'speedbar-item-rename)

  (if (string-match "XEmacs" emacs-version)
      (progn
	;; bind mouse bindings so we can manipulate the items on each line
	(define-key speedbar-key-map 'button2 'speedbar-click)
	(define-key speedbar-key-map '(meta button2) 'speedbar-mouse-item-info)

	;; Setup XEmacs Menubar
	(defvar speedbar-menu
	  '("Speed Bar"
	    ["Refresh" speedbar-refresh t]
	    ["Allow Auto Updates"
	     speedbar-toggle-updates
	     :style toggle
	     :selected speedbar-do-update]
	    "-----"
	    ["Sort etags in Speedbar"
	     (speedbar-toggle-etags "sort")
	     :style toggle
	     :selected speedbar-sort-tags]
	    ["Show unknown files"
	     (speedbar-toggle-etags "show")
	     :style toggle
	     :selected speedbar-show-unknown-files]
	    "-----"
	    ["Use C++ Tagging"
	     (speedbar-toggle-etags "-C")
	     :style toggle
	     :selected (member "-C" speedbar-fetch-etags-arguments)]
	    ["Tag preprocessor defs"
	     (speedbar-toggle-etags "-D")
	     :style toggle
	     :selected (not (member "-D" speedbar-fetch-etags-arguments))]
	    ["Use indentation"
	     (speedbar-toggle-etags "-S")
	     :style toggle
	     :selected (not (member "-S" speedbar-fetch-etags-arguments))]))
	
	(add-submenu '("Tools") speedbar-menu nil)
	;;(add-submenu nil (append '("Speedbar") (copy-tree speedbar-menu)))
	)
    ;; bind mouse bindings so we can manipulate the items on each line
    (define-key speedbar-key-map [mouse-2] 'speedbar-click)
    ;; This adds a small unecessary visual effect
    ;;(define-key speedbar-key-map [down-mouse-2] 'speedbar-quick-mouse)
    (define-key speedbar-key-map [M-mouse-2] 'speedbar-mouse-item-info)

    ;; this was meant to do a rescan or something
    ;;(define-key speedbar-key-map [shift-mouse-2] 'speedbar-hard-click)

    ;; disable all menus - we don't have a lot of space to play with
    ;; in such a skinny frame.  This will cleverly find and nuke some
    ;; user-defined menus as well if they are there.  Too bad it
    ;; rely's on the structure of a keymap to work.
    (let ((k (lookup-key global-map [menu-bar]))
	  (l nil))
      (while k
	(if (and (listp (car k)) (listp (cdr (car k))))
	    (define-key speedbar-key-map (vector 'menu-bar (car (car k)))
	      'undefined))
	(setq k (cdr k))))

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
    (define-key speedbar-menu-map [delete] 
      (cons "Delete Item" 'speedbar-item-delete))
    (define-key speedbar-menu-map [rename] 
      (cons "Rename Item" 'speedbar-item-rename))
    (define-key speedbar-menu-map [copy] 
      (cons "Copy Item" 'speedbar-item-copy))
    (define-key speedbar-menu-map [compile] 
      (cons "Byte Compile File" 'speedbar-item-byte-compile))
    (define-key speedbar-menu-map [load] 
      (cons "Load Lisp File" 'speedbar-item-load))
    (define-key speedbar-menu-map [iinfo] 
      (cons "Item Information" 'speedbar-item-info))
    (define-key speedbar-menu-map [contract] 
      (cons "Contract Item" 'speedbar-contract-line))
    (define-key speedbar-menu-map [expand] 
      (cons "Expand Item" 'speedbar-expand-line))
    (define-key speedbar-menu-map [edit] 
      (cons "Edit Item On Line" 'speedbar-edit-line))
    (define-key speedbar-menu-map [toggle-auto-update] 
      (cons "Toggle Auto Update" 'speedbar-toggle-updates))
    (define-key speedbar-menu-map [update] 
      (cons "Update" 'speedbar-refresh))
    ))

;; enable these if one of these packages is loaded
(put 'speedbar-configure-faces 'menu-enable '(or (featurep 'dialog)
						 (featurep 'ecfg-menu)))
(put 'speedbar-configure-options 'menu-enable '(or (featurep 'dialog)
						   (featurep 'ecfg-menu)))
(put 'speedbar-contract-line 'menu-enable
     '(save-excursion (beginning-of-line) (looking-at "[0-9]+: *.-. ")))
(put 'speedbar-expand-line 'menu-enable
     '(save-excursion (beginning-of-line) (looking-at "[0-9]+: *.\\+. ")))

(put 'speedbar-item-byte-compile 'menu-enable
     '(save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\*\\)?$")))
(put 'speedbar-item-load 'menu-enable
     '(save-excursion (beginning-of-line)
		      (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\*\\)?$")))
(put 'speedbar-item-copy 'menu-enable
     '(save-excursion (beginning-of-line) (looking-at "[0-9]+: *\\[")))
(put 'speedbar-item-rename 'menu-enable
     '(save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]")))
(put 'speedbar-item-delete 'menu-enable
     '(save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]")))

(defvar speedbar-buffer nil
  "The buffer displaying the speedbar.")
(defvar speedbar-frame nil
  "The frame displaying speedbar.")
(defvar speedbar-cached-frame nil
  "The frame that was last created, then removed from the display.")
(defvar speedbar-timer nil
  "The speedbar timer used for updating the buffer.")
(defvar speedbar-attached-frame nil
  "The frame which started speedbar mode.
This is the frame from which all data displayed in the speedbar is
gathered, and in which files and such are displayed.")

(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer.")

(defvar speedbar-shown-directories nil
  "Maintain list of directories simultaneously open in the current speedbar.")

(defvar speedbar-directory-contents-alist nil
  "An association list of directories and their contents.
Each sublist was returned by `speedbar-file-lists'.  This list is
maintained to speed up the refresh rate when switching between
directories.")


;;;
;;; Mode definitions/ user commands
;;;
;;;###autoload
(defun speedbar-frame-mode (&optional arg)
  "Enable or disable speedbar.  Positive # means turn on, negative turns off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time."
  (interactive "P")
  (if (not window-system)
      (error "Speedbar is not useful outside of a windowing environment"))
  ;; toggle frame on and off.
  (if (not arg) (if speedbar-frame (setq arg -1) (setq arg 1)))
  ;; turn the frame off on neg number
  (if (and (numberp arg) (< arg 0))
      (progn
	(run-hooks 'speedbar-before-delete-hook)
	(if (and speedbar-frame (frame-live-p speedbar-frame))
	    (if speedbar-xemacsp
		(delete-frame speedbar-frame)
	      (setq speedbar-cached-frame speedbar-frame)
	      (modify-frame-parameters speedbar-frame '((visibility . nil)))))
	(setq speedbar-frame nil)
	(speedbar-set-timer nil)
	(if (bufferp speedbar-buffer)
	    (kill-buffer speedbar-buffer)))
    ;; Set this as our currently attached frame
    (setq speedbar-attached-frame (selected-frame))
    ;; Get the buffer to play with
    (speedbar-mode)
    ;; Get the frame to work in
    (if (frame-live-p speedbar-cached-frame)
	(progn
	  (setq speedbar-frame speedbar-cached-frame)
	  (modify-frame-parameters speedbar-frame '((visibility . t)))
	  (select-frame speedbar-frame)
	  (switch-to-buffer speedbar-buffer)
	  (raise-frame speedbar-frame)
	  (speedbar-set-timer speedbar-update-speed)
	  )
      (if (frame-live-p speedbar-frame)
	  (raise-frame speedbar-frame)
	(let ((params (cons (cons 'height (frame-height))
			    speedbar-frame-parameters)))
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
	  (set-window-dedicated-p (selected-window) t)
	  ;; Turn off toolbar and menubar under XEmacs
	  (if speedbar-xemacsp
	      (progn
		(set-specifier default-toolbar-visible-p (cons (selected-frame) nil))
		(set-specifier menubar-visible-p (cons (selected-frame) nil))
		(make-local-variable 'current-menubar)
		(setq current-menubar speedbar-menu)
		(add-submenu nil speedbar-menu nil)
		))
	  (setq default-minibuffer-frame speedbar-attached-frame))
	(speedbar-set-timer speedbar-update-speed)
	))))

(defun speedbar-close-frame ()
  "Turn off a currently active speedbar."
  (interactive)
  (speedbar-frame-mode -1))

(defun speedbar-mode ()
  "Create and return a SPEEDBAR buffer.

The speedbar buffer allows the user to manage a list of directories
and paths at different depths.  The first line represents the default
path of the speedbar frame.  Each directory segment is a button which
jumps speedbar's default directory to that path.  Buttons are
activated by clicking mouse-2.

Each line starting with <+> represents a directory.  Click on the <+>
to insert the directory listing into the current tree.  Click on the
<-> to retract that list.  Click on the directory name to go to that
directory as the default.

Each line starting with [+] is a file.  If the variable
`speedbar-show-unknown-files' is t, the lines starting with [?] are
files which don't have imenu support, but are not expressly ignored.
Files are completely ignored if they match `speedbar-file-unshown-regexp'
which is generated from `completion-ignored-extensions'.

Files with a `*' character after their name are files checked out of a
version control system.  (currently only RCS is supported.)  New
version control systems can be added by examining the documentation
for `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p'

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
\\[speedbar-contract-line]        Contract the current line.  Same as clicking on the - on a line.

\\[speedbar-mouse-item-info]  Get info about item on current line.
\\[speedbar-item-info]     Get info about item on current line.

\\[speedbar-item-load]   Load the lisp file under cursor (optional load .elc)
\\[speedbar-item-byte-compile]   Byte compile file under cursor
\\[speedbar-item-copy]   Copy the item under cursor somewhere
\\[speedbar-item-rename]   Rename the item under cursor
\\[speedbar-item-delete]   Delete the item under cursor
"
  ;; NOT interactive
  (setq speedbar-buffer (set-buffer (get-buffer-create "SPEEDBAR")))
  (kill-all-local-variables)
  (setq major-mode 'speedbar-mode)
  (setq mode-name "Speedbar")
  (use-local-map speedbar-key-map)
  (set-syntax-table speedbar-syntax-table)
  (setq font-lock-keywords nil) ;; no font-locking please
  (setq truncate-lines t)
  (make-local-variable 'temp-buffer-show-function)
  (setq temp-buffer-show-function 'speedbar-temp-buffer-show-function)
  (setq kill-buffer-hook '(lambda () (let ((skilling (boundp 'skilling)))
				       (if skilling
					   nil
					 (if (eq (current-buffer)
						 speedbar-buffer)
					     (speedbar-frame-mode -1))))))
  (speedbar-set-mode-line-format)
  (if (not speedbar-xemacsp) (setq auto-show-mode nil))	;no auto-show for FSF
  (run-hooks 'speedbar-mode-hook)
  (speedbar-update-contents)
  )

(defun speedbar-set-mode-line-format ()
  "Sets the format of the mode line based on the current speedbar
environment.  This gives visual indications of what is up.  It EXPECTS
the speedbar frame and window to be the currently active frame and window."
  (if (frame-live-p speedbar-frame)
      (save-excursion
	(set-buffer speedbar-buffer)
	(let* ((w (window-width 
		   (get-buffer-window speedbar-buffer speedbar-frame)))
	       (p1 "<<")
	       (p5 ">>")
	       (p3 (if speedbar-do-update "SPEEDBAR" "SLOWBAR"))
	       (blank (- w (length p1) (length p3) (length p5)
			 (if line-number-mode 4 0)))
	       (p2 (make-string (/ blank 2) ? ))
	       (p4 (make-string (+ (/ blank 2) (% blank 2)) ? ))
	       (tf
		(if line-number-mode
		    (list (concat p1 p2 p3) '(line-number-mode " %3l")
			  (concat p4 p5))
		  (list (concat p1 p2 p3 p4 p5)))))
	  (if (not (equal mode-line-format tf))
	      (progn
		(setq mode-line-format tf)
		(force-mode-line-update)))))))
  
(defun speedbar-temp-buffer-show-function (buffer)
  "Placed in the variable `temp-buffer-show-function' in speedbar-mode.
If a user requests help using C-h <Key> the temp buffer will be
redirected into a window on the attached frame."
  (if speedbar-attached-frame (select-frame speedbar-attached-frame))
  (pop-to-buffer buffer nil)
  (other-window -1)
  (run-hooks 'temp-buffer-show-hook))

(defun speedbar-mouse-hscroll (e)
  "Read a mouse event from the mode line, and horizontally scroll.
If the mouse is being clicked on the far left, or far right of the
modeline.  This is only useful for non-XEmacs"
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

(defun speedbar-next (arg)
  "Move to the next line in a speedbar buffer"
  (interactive "p")
  (forward-line (or arg 1))
  (speedbar-position-cursor-on-line))

(defun speedbar-prev (arg)
  "Move to the prev line in a speedbar buffer"
  (interactive "p")
  (speedbar-next (if arg (- arg) -1)))

(defun speedbar-scroll-up (&optional arg)
  "Page down one screenfull of the speedbar"
  (interactive "P")
  (scroll-up arg)
  (speedbar-position-cursor-on-line))

(defun speedbar-scroll-down (&optional arg)
  (interactive "P")
  (scroll-down arg)
  (speedbar-position-cursor-on-line))

;;;
;;; Speedbar file activity
;;;
(defun speedbar-refresh ()
  "Refresh the current speedbar display, disposing of any cahced data."
  (interactive)
  (adelete 'speedbar-directory-contents-alist default-directory)
  (if (<= 1 speedbar-verbosity-level) (message "Refreshing speedbar..."))
  (speedbar-update-contents)
  (speedbar-stealthy-updates)
  ;; Reset the timer in case it got really hosed for some reason...
  (speedbar-set-timer speedbar-update-speed)
  (if (<= 1 speedbar-verbosity-level) (message "Refreshing speedbar...done")))

(defun speedbar-item-load ()
  "Byte compile the item under the cursor or mouse if it is a lisp file."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (and (file-exists-p f) (string-match "\\.el$" f))
	(if (and (file-exists-p (concat f "c"))
		 (y-or-n-p (format "Load %sc? " f)))
	    ;; If the compiled version exists, load that instead...
	    (load-file (concat f "c"))
	  (load-file f))
      (error "Not a loadable file..."))))

(defun speedbar-item-byte-compile ()
  "Byte compile the item under the cursor or mouse if it is a lisp file."
  (interactive)
  (let ((f (speedbar-line-file))
	(sf (selected-frame)))
    (if (and (file-exists-p f) (string-match "\\.el$" f))
	(progn
	  (select-frame speedbar-attached-frame)
	  (byte-compile-file f nil)
	  (select-frame sf)))
    ))

(defun speedbar-mouse-item-info (event)
  "Provide information about what the user clicked on."
  (interactive "e")
  (mouse-set-point event)
  (speedbar-item-info))

(defun speedbar-item-info ()
  "Display info in the minibuffer about the button the mouse is over."
  (interactive)
  (let* ((item (speedbar-line-file))
	 (attr (if item (file-attributes item) nil)))
    (if item (message "%s %d %s" (nth 8 attr) (nth 7 attr) item)
      (save-excursion
	(beginning-of-line)
	(looking-at "\\([0-9]+\\):")
	(setq item (speedbar-line-path (string-to-int (match-string 1))))
	(if (re-search-forward "> \\([^ ]+\\)$"
			       (save-excursion(end-of-line)(point)) t)
	    (progn
	      (setq attr (get-text-property (match-beginning 1)
					    'speedbar-token))
	      (message "Tag %s in %s at position %s"
		       (match-string 1) item (if attr attr 0)))
	  (message "No special info for this line.")))
	  )))

(defun speedbar-item-copy ()
  "Copy the item under the cursor.
Files can be copied to new names or places."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f)	(error "Not a file."))
    (if (file-directory-p f)
	(error "Cannot copy directory.")
      (let* ((rt (read-file-name (format "Copy %s to: "
					 (file-name-nondirectory f))))
	     (refresh (member (expand-file-name (file-name-directory rt))
			      speedbar-shown-directories)))
	;; Create the right file name part
	(if (file-directory-p rt)
	    (setq rt 
		  (concat (expand-file-name rt)
			  (if (string-match "/$" rt) "" "/")
			  (file-name-nondirectory f))))
	(if (or (not (file-exists-p rt))
		(y-or-n-p (format "Overwrite %s with %s? " rt f)))
	    (progn
	      (copy-file f rt t t)
	      ;; refresh display if the new place is currently displayed.
	      (if refresh (speedbar-refresh))))))))


(defun speedbar-item-rename ()
  "Rename the item under the cursor or mouse.
Files can be renamed to new names or moved to new directories."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if f
	(let* ((rt (read-file-name (format "Rename %s to: "
					   (file-name-nondirectory f))))
	       (refresh (member (expand-file-name (file-name-directory rt))
				speedbar-shown-directories)))
	  ;; Create the right file name part
	  (if (file-directory-p rt)
	      (setq rt 
		    (concat (expand-file-name rt)
			    (if (string-match "/$" rt) "" "/")
			    (file-name-nondirectory f))))
	  (if (or (not (file-exists-p rt))
		  (y-or-n-p (format "Overwrite %s with %s? " rt f)))
	      (progn
		(rename-file f rt t)
		;; refresh display if the new place is currently displayed.
		(if refresh (speedbar-refresh)))))
      (error "Not a file."))))

(defun speedbar-item-delete ()
  "Delete the item under the cursor.  Files are removed from disk."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f) (error "Not a file."))
    (if (y-or-n-p (format "Delete %s? " f))
	(progn
	  (if (file-directory-p f)
	      (delete-directory f)
	    (delete-file f))
	  (message "Okie dokie..")
	  (speedbar-refresh)))
    ))


;;;
;;; Utility functions
;;;
(defun speedbar-set-timer (timeout)
  "Unset an old timer (if there is one) and activate a new timer with TIMEOUT.
TIMEOUT is the number of seconds until the speedbar timer is called
again."
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
   ;; Post 19.31 Emacs
   ((fboundp 'run-with-idle-timer)
    (if speedbar-timer 
	(progn (cancel-timer speedbar-timer)
	       (setq speedbar-timer nil)))
    (if timeout
	(setq speedbar-timer 
	      (run-with-idle-timer timeout nil 'speedbar-timer-fn))))
   ;; Older or other Emacsen with no timers.  Set up so that it's
   ;; obvious this emacs can't handle the updates
   (t 
    (setq speedbar-do-update nil)))
   ;; change this if it changed for some reason
  (speedbar-set-mode-line-format))

(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate forms.
Turn read-only back on when done."
  (list 'let '((speedbar-with-writable-buff (current-buffer)))
	'(toggle-read-only -1)
	(cons 'progn forms)
	'(save-excursion (set-buffer speedbar-with-writable-buff)
			 (toggle-read-only 1))))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun speedbar-make-button (start end face mouse function &optional token)
  "Create a button from START to END, with FACE as the display face.
MOUSE is the mouse face.  When this button is clicked on FUNCTION
will be run with the TOKEN parameter (any lisp object)"
  (put-text-property start end 'face face)
  (put-text-property start end 'mouse-face mouse)
  (put-text-property start end 'invisible nil)
  (if function (put-text-property start end 'speedbar-function function))
  (if token (put-text-property start end 'speedbar-token token))
  )

(defun speedbar-file-lists (directory)
  "Create file lists for DIRECTORY.
The car is the list of directories, the cdr is list of files not
matching ignored headers.  Cache any directory files found in
`speedbar-directory-contents-alist' and use that cache before scanning
the filesystem"
  (or (cdr-safe (assoc directory speedbar-directory-contents-alist))
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
	(let ((nl (cons (nreverse dirs) (list (nreverse files)))))
	  (aput 'speedbar-directory-contents-alist directory nl)
	  nl))
      ))

(defun speedbar-directory-buttons (directory index)
  "Inserts a single button group at point for DIRECTORY.
Each directory path part is a different button.  If part of the path
matches the user directory ~, then it is replaced with a ~"
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
				 p (match-end 0)))))
      ;; Nuke the beginning of the directory if it's too long...
      (cond ((eq speedbar-directory-button-trim-method 'span)
	     (beginning-of-line)
	     (let ((ww (window-width (get-buffer-window speedbar-buffer
							speedbar-frame))))
	       (move-to-column ww nil)
	       (while (>= (current-column) ww)
		 (re-search-backward "/" nil t)
		 (insert "/...\n..")
		 (move-to-column ww nil))))
	    ((eq speedbar-directory-button-trim-method 'trim)
	     (end-of-line)
	     (let ((ww (window-width (get-buffer-window speedbar-buffer
							speedbar-frame)))
		   (tl (current-column)))
	       (move-to-column (- tl ww))
	       (if (re-search-forward "/" nil t)
		   (progn
		     (delete-region (point-min) (point))
		     (insert "$")
		     )))))
      )
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
  "Creates a tag line with BUTTON-TYPE for the small expansion button.
This is the button that expands or contracts a node (if applicable),
and BUTTON-CHAR the character in it (+, -, ?, etc).  BUTTON-FUNCTION
is the function to call if it's clicked on.  Button types are
'bracket, 'angle, 'curly, or nil.

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
  "Insert list of FILES starting at point, and indenting all files to LEVEL.
Tag exapndable items with a +, otherwise a ?.  Don't highlight ? as we
don't know how to manage them.  The input parameter FILES is a cons
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
  (speedbar-reset-scanners)
  )

(defun speedbar-insert-generic-list (level lst expand-fun find-fun)
  "At LEVEL, inserts a generic multi-level alist LIST.
Associations with lists get {+} tags (to expand into more nodes) and
those with positions just get a > as the indicator.  {+} buttons will
have the function EXPAND-FUN and the token is the CDR list.  The token
name will have the function FIND-FUN and not token."
  ;; Remove imenu rescan button
  (if (string= (car (car lst)) "*Rescan*")
      (setq lst (cdr lst)))
  ;; insert the parts
  (while lst
    (cond ((null (car-safe lst)) nil)	;this would be a separator
	  ((or (numberp (cdr-safe (car-safe lst)))
	       (markerp (cdr-safe (car-safe lst))))
	   (speedbar-make-tag-line nil nil nil nil ;no expand button data
				   (car (car lst)) ;button name
				   find-fun        ;function
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
  "Run whenever emacs is idle to update the speedbar item."
  (if (not (and (frame-live-p speedbar-frame)
		(frame-live-p speedbar-attached-frame)))
      (speedbar-set-timer nil)
    (condition-case nil
	;; Save all the match data so that we don't mess up executing fns
	(save-match-data
	  (if (and (frame-visible-p speedbar-frame) speedbar-do-update)
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
			  (member major-mode speedbar-ignored-modes)
			  (eq af speedbar-frame)
			  (not (buffer-file-name))
			  )
		      nil
		    (if (<= 1 speedbar-verbosity-level) 
			(message "Updating speedbar to: %s..." default-directory))
		    (speedbar-update-contents)
		    (if (<= 1 speedbar-verbosity-level)
			(message "Updating speedbar to: %s...done" default-directory)))
		  (select-frame af))
		;; Now run stealthy updates of time-consuming items
		(speedbar-stealthy-updates))))
      ;; errors that might occur
      (error (message "Speedbar error!")))
    ;; Reset the timer
    (speedbar-set-timer speedbar-update-speed))
  (run-hooks 'speedbar-timer-hook)
  )


;;;
;;; Stealthy activities
;;;
(defun speedbar-stealthy-updates ()
  "For a given speedbar, run all items in the stealthy function list.
Each item returns t if it completes successfully, or nil if
interrupted by the user."
  (let ((l speedbar-stealthy-function-list))
    (unwind-protect
	(while (and l (funcall (car l)))
	  (sit-for 0)
	  (setq l (cdr l)))
      ;(message "Exit with %S" (car l))
      )))

(defun speedbar-reset-scanners ()
  "Resets any variables used by functions in the stealthy list as state.
If new functions are added, their state needs to be updated here."
  (setq speedbar-vc-to-do-point t)
  )

(defun speedbar-update-current-file ()
  "Find the current file is, and update our visuals to indicate its name.
This is specific to file names.  If the file name doesn't show up, but
it should be in the list, then the directory cache needs to be
updated."
  (let* ((lastf (selected-frame))
	 (newcfd (save-excursion
		  (select-frame speedbar-attached-frame)
		  (let ((rf (if (buffer-file-name)
				(buffer-file-name)
			      nil)))
		    (select-frame lastf)
		    rf)))
	 (newcf (if newcfd (file-name-nondirectory newcfd)))
	 (lastb (current-buffer)))
    (if (and newcf 
	     ;; check here, that way we won't refresh to newcf until
	     ;; its been written, thus saving ourselves some time
	     (file-exists-p newcf)
	     (not (string= newcf speedbar-last-selected-file)))
	(progn
	  (select-frame speedbar-frame)
	  (set-buffer speedbar-buffer)
	  (speedbar-with-writable
	    (goto-char (point-min))
	    (if (and 
		 speedbar-last-selected-file
		 (re-search-forward 
		  (concat " \\(" (regexp-quote speedbar-last-selected-file) 
			  "\\)\\(" (regexp-quote speedbar-vc-indicator)
			  "\\)?\n")
		  nil t))
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face
				   'speedbar-file-face))
	    (goto-char (point-min))
	    (if (re-search-forward 
		 (concat " \\(" (regexp-quote newcf) "\\)\\("
			 (regexp-quote speedbar-vc-indicator)
			 "\\)?\n") nil t)
		;; put the property on it
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face 
				   'speedbar-selected-face)
	      ;; Oops, it's not in the list.  Should it be?
	      (if (and (string-match speedbar-file-regexp newcf)
		       (string= (file-name-directory newcfd)
				(expand-file-name default-directory)))
		  ;; yes, it is (we will ignore unknowns for now...)
		  (progn
		    (speedbar-refresh)
		    (if (re-search-forward 
			 (concat " \\(" (regexp-quote newcf) "\\)\n") nil t)
			;; put the property on it
			(put-text-property (match-beginning 1)
					   (match-end 1)
					   'face 
					   'speedbar-selected-face)))
		;; if it's not in there now, whatever...
		))
	    (setq speedbar-last-selected-file newcf))
	  (forward-line -1)
	  (speedbar-position-cursor-on-line)
	  (set-buffer lastb)
	  (select-frame lastf))))
  ;; return that we are done with this activity.
  t)

(defun speedbar-check-vc ()
  "Scan all files in a directory, and for each see if it's checked out.
See `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p' for how
to add nore types of version control systems."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (set-buffer speedbar-buffer)
    (if (and speedbar-vc-do-check (eq speedbar-vc-to-do-point t)
	     (speedbar-vc-check-dir-p default-directory)
	     (not (and (featurep 'ange-ftp)
		       (string-match (car ange-ftp-name-format)
				     (expand-file-name default-directory)))))
	(setq speedbar-vc-to-do-point 0))
    (if (numberp speedbar-vc-to-do-point)
	(progn
	  (goto-char speedbar-vc-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*\\[[+-]\\] " nil t))
	    (setq speedbar-vc-to-do-point (point))
	    (if (speedbar-check-vc-this-line)
		(speedbar-with-writable
		  (insert speedbar-vc-indicator))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-vc-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-vc-this-line ()
  "Return t if the file on this line is check of of a version control system.
The one caller-requirement is that the last regex matching opperation
has the current depth stored in (match-string 1), and that the cursor
is right in front of the file name."
  (let* ((d (string-to-int (match-string 1)))
	 (f (speedbar-line-path d))
	 (fn (buffer-substring-no-properties
	      (point) (progn (end-of-line) (point))))
	 (fulln (concat f fn))
	 )
    (if (<= 2 speedbar-verbosity-level) 
	(message "Speedbar vc check...%s" fulln))
    (and (file-writable-p fulln)
	 (speedbar-this-file-in-vc f fn))))

(defun speedbar-vc-check-dir-p (path)
  "Return t if we should bother checking PATH for vc files.
This can be overloaded to add new types of version control systems."
  (or
   (file-exists-p (concat path "RCS/"))
   ;; If SCCS is added in `speedbar-this-file-in-vc'
   ;; (file-exists-p (concat path "SCCS/"))
   ;; (file-exists-p (getenv "SCCSPATHTHINGIDONTREMEMBER"))
   ))

(defun speedbar-this-file-in-vc (path name)
  "Check to see if the file in PATH with NAME is in a version control system.
You can add new VC systems by overriding this function.  You can
optimize this function by overriding it and only doing those checks
that will occur on your system."
  (or
   (file-exists-p (concat path "RCS/" fn ",v"))
   ;; Is this right?  I don't recall
   ;;(file-exists-p (concat path "SCCS/," fn))
   ;;(file-exists-p (concat (getenv "SCCSPATHTHING") "/SCCS/," fn))
   ))

;;;
;;; Clicking Activity
;;;
(defun speedbar-quick-mouse (e)
  "Since mouse events are strange, this will keep the mouse nicely positioned."
  (interactive "e")
  (mouse-set-point e)
  (speedbar-position-cursor-on-line)
  )

(defun speedbar-position-cursor-on-line ()
  "Position the cursor on a line."
  (beginning-of-line)
  (if (looking-at "[0-9]+:\\s-*..?.? ")
      (goto-char (1- (match-end 0)))))

(defun speedbar-line-file (&optional p)
  "Retrieve the file or whatever from the line at P point.
The return value is a string representing the file.  If it is a
directory, then it is the directory name."
  (save-excursion
    (beginning-of-line)
    (if (looking-at (concat
		     "\\([0-9]+\\): *[[<][-+][]>] \\([^ \n]+\\)\\("
		     (regexp-quote speedbar-vc-indicator)
		     "\\)?"))
	(let* ((depth (string-to-int (match-string 1)))
	       (path (speedbar-line-path depth))
	       (f (match-string 2)))
	  (concat path f))
      nil)))

(defun speedbar-line-path (depth)
  "Retrieve the pathname associated with the current line.
This may require traversing backwards and combinding the default
directory with these items."
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
      (if (and path
	       (string-match (concat (regexp-quote speedbar-vc-indicator) "$")
			     path))
	  (setq path (substring path 0 (match-beginning 0))))
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
  "Contract the line under the cursor."
  (interactive)
  (beginning-of-line)
  (re-search-forward ":\\s-*.-. " (save-excursion (end-of-line) (point)))
  (forward-char -2)
  (speedbar-do-function-pointer))

(defun speedbar-click (e)
  "Activate any speedbar buttons where the mouse is clicked.
This must be bound to a mouse event.  A button is any location of text
with a mouse face that has a text property called `speedbar-function'."
  (interactive "e")
  (mouse-set-point e)
  (speedbar-do-function-pointer)
  (speedbar-quick-mouse e))

(defun speedbar-do-function-pointer ()
  "Look under the cursor and examine the text properties.
From this extract the file/tag name, token, indentation level and call
a function if apropriate"
  (let* ((fn (get-text-property (point) 'speedbar-function))
	 (tok (get-text-property (point) 'speedbar-token))
	 ;; The 1-,+ is safe because scaning starts AFTER the point
	 ;; specified.  This lets the search include the character the
	 ;; cursor is on.
	 (tp (previous-single-property-change 
	      (1+ (point)) 'speedbar-function))
	 (np (next-single-property-change 
	      (point) 'speedbar-function))
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
  "Speedbar click handler for filenames.
Clicking the filename loads that file into the attached buffer."
  (let ((cdd (speedbar-line-path indent)))
    (select-frame speedbar-attached-frame)
    (find-file (concat cdd text))
    (speedbar-stealthy-updates)
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)))

(defun speedbar-dir-follow (text token indent)
  "Speedbar click handler for directory names.
Clicking a directory will cause the speedbar to list files in the
selected subdirectory."
  (setq default-directory 
	(concat (expand-file-name (concat (speedbar-line-path indent) text))
		"/"))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed)
  (setq speedbar-last-selected-file nil)
  (speedbar-stealthy-updates))

(defun speedbar-delete-subblock (indent)
  "Delete text from point to indentation level INDENT or greater.
Handles end-of-sublist smartly."
  (speedbar-with-writable
    (save-excursion
      (end-of-line) (forward-char 1)
      (while (and (not (save-excursion
			 (re-search-forward (format "^%d:" indent)
					    nil t)))
		  (>= indent 0))
	(setq indent (1- indent)))
      (delete-region (point) (if (>= indent 0)
				 (match-beginning 0)
			       (point-max))))))

(defun speedbar-dired (text token indent)
  "Speedbar click handler for directory expand button.
Clicking this button expands or contracts a directory."
  (cond ((string-match "+" text)	;we have to expand this dir
	 (setq speedbar-shown-directories 
	       (cons (expand-file-name 
		      (concat (speedbar-line-path indent) token "/"))
		     speedbar-shown-directories))
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-reset-scanners)
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   (speedbar-with-writable
	     (speedbar-default-directory-list 
	      (concat (speedbar-line-path indent) token "/")
	      (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-reset-scanners)
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
	 (speedbar-delete-subblock indent)
	 )
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly)
  (setq speedbar-last-selected-file nil)
  (save-excursion (speedbar-stealthy-updates)))

(defun speedbar-directory-buttons-follow (text token ident)
  "Speedbar click handler for default directory buttons."
  (setq default-directory token)
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed))

(defun speedbar-tag-file (text token indent)
  "The cursor is on a selected line.  Expand the tags in the specified file.
The parameter TXT and TOK are required, where TXT is the button clicked, and
TOK is the file to expand."
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
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

(defun speedbar-tag-find (text token indent)
  "For the tag in a file, goto that position"
  (let ((file (speedbar-line-path indent)))
    (select-frame speedbar-attached-frame)
    (find-file file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)
    (goto-char token)))

(defun speedbar-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature."
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
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

;;;
;;; Centering Utility
;;;
(defun speedbar-center-buffer-smartly ()
  "Recenters a speedbar buffer so the current indentation level is all visible.
This assumes that the cursor is on a file, or tag of a file which the user is
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
(if (eval-when-compile (string-match "XEmacs" emacs-version))

    nil

(defun speedbar-fetch-dynamic-imenu (file)
  "Use the imenu package to load in file, and extract all the items
tags we wish to display in the speedbar package."
  ;; Load this AND compile it in
  (eval-and-compile (require 'imenu))
  (save-excursion
    (set-buffer (find-file-noselect file))
    (condition-case nil
	(imenu--make-index-alist t)
      (error t))))

)


;;;
;;; Tag Management -- etags  (Only useful for Xemacs)
;;;
(defvar speedbar-fetch-etags-parse-list
  '(("\\.\\([cChH]\\|c++\\|cpp\\|cc\\|hh\\)$" . speedbar-parse-c-or-c++tag)
    ("\\.el\\|\\.emacs" . "defun\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
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
an etags output buffer.  Use `speedbar-toggle-etags' to modify this
list conveniently.

This variable is ignored if `speedbar-use-imenu-package' is `t'")

(defun speedbar-toggle-etags (flag)
  "Toggle FLAG in `speedbar-fetch-etags-arguments' to be a member of
etags command line arguments.  If flag is \"sort\", then toggle the
value of `speedbar-sort-tags'.  If it's value is \"show\" then toggle
the value of `speedbar-show-unknown-files'.

  This function is a convenience function for XEmacs menu created by 
Farzin Guilak <farzin@protocol.com>"
  (interactive)
  (cond
   ((equal flag "sort")
    (setq speedbar-sort-tags (not speedbar-sort-tags)))
   ((equal flag "show")
    (setq speedbar-show-unknown-files (not speedbar-show-unknown-files)))
   ((or (equal flag "-C")
	(equal flag "-S")
	(equal flag "-D"))
    (if (member flag speedbar-fetch-etags-arguments)
	(setq speedbar-fetch-etags-arguments
	      (delete flag speedbar-fetch-etags-arguments))
      (add-to-list 'speedbar-fetch-etags-arguments flag)))
   (t nil)))

(defun speedbar-fetch-dynamic-etags (file)
  "For the complete file definition FILE, run etags as a subprocess,
fetch it's output, and create a list of symbols extracted, and their
position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (if (get-buffer "*etags tmp*")
	      (kill-buffer "*etags tmp*"))	;kill to clean it up
	  (if (<= 1 speedbar-verbosity-level) (message "Fetching etags..."))
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (apply 'call-process speedbar-fetch-etags-command nil 
		 (current-buffer) nil 
		 (append speedbar-fetch-etags-arguments (list file)))
	  (goto-char (point-min))
	  (if (<= 1 speedbar-verbosity-level) (message "Fetching etags..."))
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
	(sort newlist (lambda (a b) (string< (car s1) (car s2))))
      (reverse newlist))))

;; This bit donated by Farzin Guilak <farzin@protocol.com> but I'm not
;; sure it's needed with the different sorting method.
;;
;(defun speedbar-clean-etags()
;  "Removes spaces before the ^? character, and removes `#define', 
;return types, etc. preceding tags.  This ensures that the sort operation
;works on the tags, not the return types."
;  (save-excursion
;    (goto-char (point-min))
;    (while  
;	(re-search-forward "(?[ \t](?\C-?" nil t)
;      (replace-match "\C-?" nil nil))
;    (goto-char (point-min))
;    (while
;	(re-search-forward "\\(.*[ \t]+\\)\\([^ \t\n]+.*\C-?\\)" nil t)
;      (delete-region (match-beginning 1) (match-end 1)))))

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
;;; Color loading section  This is messy *Blech!*
;;;
(defun speedbar-load-color (sym l-fg l-bg d-fg d-bg &optional bold italic underline)
  "Create a color for SYM with a L-FG and L-BG color, or D-FG and D-BG.
Optionally make BOLD, ITALIC, or UNDERLINED if applicable.  If the background
attribute of the current frame is determined to be light (white, for example)
then L-FG and L-BG is used.  If not, then D-FG and D-BG is used.  This will
allocate the colors in the best possible mannor.  This will allow me to store
multiple defaults and dynamically determine which colors to use."
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
		((let* ((bgc (or (cdr (assq 'background-color params))
				 (if speedbar-xemacsp
				     (x-get-resource ".background"
						     "Background" 'string)
				   (x-get-resource ".background"
						   "Background"))
				 ;; if no other options, default is white
				 "white"))
			(bgcr (if speedbar-xemacsp
				  (color-instance-rgb-components
				   (make-color-instance bgc))
				(x-color-values bgc)))
			(wcr (if speedbar-xemacsp
				 (color-instance-rgb-components
				  (make-color-instance "white"))
			       (x-color-values "white"))))
		   (< (apply '+ bgcr) (/ (apply '+ wcr) 3)))
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
	    (set-face-foreground newface nfg))
	(if (and nbg (not (funcall set-p (symbol-name sym) "Background")))
	    (set-face-background newface nbg))
	
	(if bold (condition-case nil
		     (make-face-bold newface)
		   (error (message "Cannot make face %s bold!"
				       (symbol-name sym)))))
	(if italic (condition-case nil
		       (make-face-italic newface)
		     (error (message "Cannot make face %s italic!"
				     (symbol-name newface)))))
	(set-face-underline-p newface underline)
	))))

(defun speedbar-enable-update ()
  "Enable automatic updating in speedbar"
  (interactive)
  (setq speedbar-do-update t)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer speedbar-update-speed))

(defun speedbar-disable-update ()
  "Disable automatic updating and stop consuming resources."
  (interactive)
  (setq speedbar-do-update nil)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer nil))

(defun speedbar-toggle-updates ()
  "Toggles whether updates are done automatically"
  (interactive)
  (if speedbar-do-update
      (speedbar-disable-update)
    (speedbar-enable-update)))

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

;; run load-time hooks
(run-hooks 'speedbar-load-hooks)

;;; end of lisp
(provide 'speedbar)
