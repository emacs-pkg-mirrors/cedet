;;; semantic-util-modes.el --- Semantic minor modes

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-decorate-mode.el,v 1.1 2004/06/13 02:50:41 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.:

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
;; A minor mode for use in decorating tags.
;;
;; There are two types of decorations that can be performed on a tag.
;; You can either highlight the full tag, or you can add a secondary
;; overlay on some part of the tag body.
;;
;; For secondary overlays in particular, managing them so that they
;; do not get corrupted is challenging.  This major mode and
;; corresponding macros will make handling those types of decorations
;; easier.
;;
;; Most of this mode was copied from `semantic-show-tag-boundaries-mode'
;;

;;; Acronyms in code:
;;
;; In the following code:
;;  dso - Decorate with Secondary Overlay
;;  dpo - Decorate with Primary Overlay

;;; Code:
(require 'semantic)
(require 'semantic-util-modes)

;;; DECORATION MODE
;;
;; Generic mode for handling basic highlighting and secondary
;; overlays.


;;;###autoload
(defun global-semantic-decoration-mode (&optional arg)
  "Toggle global use of option `semantic-decoration-mode'.
Decoration mode turns on all active decorations as specified
by `semantic-decoration-primary-alist' and
`semantic-decoration-secondary-alist'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-decoration-mode
        (semantic-toggle-minor-mode-globally
         'semantic-decoration-mode arg)))

;;;###autoload
(defcustom global-semantic-decoration-mode nil
  "*If non-nil, enable global use of `semantic-decoration-mode'.
When this mode is activated, decorations specified by
`semantic-decoration-primary-alist' and
`semantic-decoration-secondary-alist' will be enabled."
  :group 'semantic
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-decoration-mode (if val 1 -1))))

(defcustom semantic-decoration-hook nil
  "*Hook run at the end of function `semantic-decoration-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-decoration-mode nil
  "Non-nil if `semantic-decoration-mode' is enabled.
Use the command `semantic-decoration-mode' to change this
variable.")
(make-variable-buffer-local 'semantic-decoration-mode)

(defun semantic-decoration-setup ()
  "Setup the `semantic-decoration-mode' minor mode.
The minor mode can be turned on only if the semantic feature is available
and the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (if semantic-decoration-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-decoration-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; Add hooks
	(semantic-make-local-hook 'semantic-after-partial-cache-change-hook)
	(add-hook 'semantic-after-partial-cache-change-hook
		  'semantic-decorate-reparse-hook nil t)
	(semantic-make-local-hook 'semantic-after-toplevel-cache-change-hook)
	(add-hook 'semantic-after-toplevel-cache-change-hook
		  'semantic-dso-after-full-reparse-hook nil t)
	(semantic-decorate-reparse-hook (semantic-fetch-tags))
	)
    ;; Cleanup tag boundaries highlighting
    (semantic-dso-clear (semantic-fetch-tags))
    ;; Cleanup any leftover crap too.
    (semantic-dso-flush-rogue-overlays)
    ;; Remove hooks
    (remove-hook 'semantic-after-partial-cache-change-hook
		 'semantic-decorate-reparse-hook t)
    (remove-hook 'semantic-after-toplevel-cache-change-hook
		 'semantic-dso-after-full-reparse-hook t)
    )
  semantic-decoration-mode)
  
;;;###autoload
(defun semantic-decoration-mode (&optional arg)
  "Minor mode for decorating tags.
Decorations are specified in `semantic-decoration-primary-alist' and
`semantic-decorate-secondary-alist'.
You can create new decoration types with:
`define-semantic-decoration-with-secondary-overaly', or with
`define-semantic-decoration-with-primary-overlay'.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
;;
;;\\{semantic-decoration-map}"
  (interactive
   (list (or current-prefix-arg
             (if semantic-decoration-mode 0 1))))
  (setq semantic-decoration-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-decoration-mode)))
  (semantic-decoration-setup)
  (run-hooks 'semantic-decoration-hook)
  (if (interactive-p)
      (message "decoration-mode minor mode %sabled"
               (if semantic-decoration-mode "en" "dis")))
  (semantic-mode-line-update)
  semantic-decoration-mode)


(defun semantic-decorate-tag-secondary-overlay-p (ol)
  "Non nil of OL is a secondary overlay created for `semantic-decoration-mode'."
  (semantic-overlay-get ol 'semantic-dso))

(defun semantic-dso-flush-rogue-overlays ()
  "Flush ALL secondary overlays associated with decoration mode."
  (let ((ol (semantic-overlays-in (point-min) (point-max))))
    (while ol
      (if (semantic-decorate-tag-secondary-overlay-p (car ol))
	  (semantic-overlay-delete (car ol)))
      (setq ol (cdr ol)))))

(defun semantic-dso-clear (tag-list)
  "Clear boundaries off from TAG-LIST."
  (while tag-list
    (semantic-tag-delete-secondary-overlay (car tag-list) 'semantic-dso)

    ;; recurse over children
    (semantic-dso-clear
     (semantic-tag-components-with-overlays (car tag-list)))
    
    (setq tag-list (cdr tag-list)))
  )

(defun semantic-dso-after-full-reparse-hook (tag-list)
  "Called after a complete reparse of the current buffer.
Eventually calls `semantic-decorate-reparse-hook'.
Argument TAG-LIST is the list of tags recently parsed."
  ;; Flush everything
  (semantic-dso-flush-rogue-overlays)
  ;; Add it back on
  (semantic-decorate-reparse-hook tag-list))

(defun semantic-decorate-reparse-hook (tag-list)
  "Called when the new tags TAG-LIST are created in a buffer.
Adds decorations to these fresh tags, and makes sure old decorations
in the area are completely flushed."
  (while tag-list
    (let ((oldoverlays (semantic-tag-get-secondary-overlay
			(car tag-list) 'semantic-dso)))
      (when oldoverlays
	;; It would be nice if this never happened, but it still does
	;; once in a while.  Print a message to help flush these situations
	(message "Secondary overlay still on %s"
		 (semantic-format-tag-name (car tag-list)))
	(while oldoverlays
	  (semantic-tag-delete-secondary-overlay (car tag-list)
						 (car oldoverlays))
	  (setq oldoverlays (cdr oldoverlays))))
      )
    ;; Start with primary items
    (let ((primary semantic-decoration-primary-alist))
      (while primary
	(if (funcall (car (car primary)) (car tag-list))
	    (funcall (car (cdr (car primary))) (car tag-list)))
	(setq primary (cdr primary))))
    ;; Now do secondary overlay style decorations
    (let ((secondary semantic-decoration-secondary-alist))
      (while secondary
	(if (funcall (car (car secondary)) (car tag-list))
	    (funcall (car (cdr (car secondary))) (car tag-list)))
	(setq secondary (cdr secondary))))
    ;; Recurse on the children of all tags
    (semantic-decorate-reparse-hook
     (semantic-tag-components-with-overlays (car tag-list)))
    (setq tag-list (cdr tag-list))))


(semantic-add-minor-mode 'semantic-decoration-mode
                         ""
                         nil)

;;; Primary Decorations
;;
;; Functions and utiles for primary decorations

(defvar semantic-decoration-primary-alist
  nil
  "*List of active decoration styles applied to the primary tag overlay.
It is of the form
\( (NAME-p NAME-highlight) ... )
See `define-semantic-decoration-on-primary-overlay' which will
automatically add items to this list.")

;;; Primary Decoration Creation Macros
;;
;; Macro based construction of primary decoration management.
(defmacro define-semantic-decoration-on-primary-overlay (name doc &rest forms)
  "Defines a primary decoration mode.
This creates a new overload method called `NAME-p'. You must implement
`NAME-p-default'.  This method returns non-nil if first argument TAG
should be decorated by concept NAME.
This also creates a new function called `NAME-highlight' which is created
using FORMS.  FORMS should consist only of functions like
`semantic-set-tag-face', or `semantic-set-tag-intangible' found in
the source file semantic-decorate.el."
  (let ((-p 	    (intern (concat (symbol-name name) "-p")))
	(-highlight (intern (concat (symbol-name name) "-highlight")))
	)
    `(eval-and-compile
       ;; Create an override method to specify if a given tag belongs
       ;; to this type of decoration
       (define-overload ,-p (tag)
	 ,(concat "Determine if TAG should be decorated by "
		  (symbol-name name) "\n" doc))
       ;; Create an override method that will perform the highlight
       ;; operation if the -p method returns non-nil.
       (defun ,-highlight (tag) 
	 ,(concat "Propertly list applied tags matching " (symbol-name name) "-p\n"
		  doc)
	 ,@forms)
       ;; Add this to the list of primary decoration modes.
       (add-to-list 'semantic-decoration-primary-alist
		    ,(list 'quote (list -p -highlight)))
       ))
  )

;;; Private Method Highlight
;;
(defface semantic-decoration-private-members-face
  '((((class color) (background dark))
     (:background "#100000"))
    (((class color) (background light))
     (:background "#8fffff")))
  "*Face used to show privatly scoped tags in.
The face is used in  `semantic-decoration-mode.'."
  :group 'semantic-faces)


(define-semantic-decoration-on-primary-overlay
  semantic-decoration-private-members
  "Highlight class members that are designated as PRIVATE access."
  (semantic-set-tag-face tag 'semantic-decoration-private-members-face))

(defun semantic-decoration-private-members-p-default (tag)
  "Non-nil if TAG is private."
  (and (member (semantic-tag-class tag) '(function variable))
       (eq (semantic-tag-protection tag) 'private)))

(defface semantic-decoration-protected-members-face
  '((((class color) (background dark))
     (:background "#000010"))
    (((class color) (background light))
     (:background "#fffff8")))
  "*Face used to show protected scoped tags in.
The face is used in  `semantic-highlight-by-attribute-mode'."
  :group 'semantic-faces)

(define-semantic-decoration-on-primary-overlay
  semantic-decoration-protected-members
  "Highlight class members that are designated as PROTECTED access."
  (semantic-set-tag-face tag 'semantic-decoration-protected-members-face))

(defun semantic-decoration-protected-members-p-default (tag)
  "Non-nil if TAG is private."
  (and (member (semantic-tag-class tag) '(function variable))
       (eq (semantic-tag-protection tag) 'protected)))


;;; Secondary Decorations
;;
(defvar semantic-decoration-secondary-alist
  nil
  "*List of active decoration styles which use secondary overlays.
It is of the form
\( (NAME-p NAME-highlight) ... )
See `define-semantic-decoration-with-secondary-overlay' which will
automatically add items to this list.")

;;; Utils - secondary
;;
;; Functions that created modes should use.

(defun semantic-decoration-create-secondary-overlay (tag begin end face)
  "Create a secondary overlay on TAG on the region between BEGIN and END.
Set the 'face property of the overlay to FACE."
  (let ((o (semantic-tag-create-secondary-overlay tag)))
    ;; We do not use the unlink property because we do not want to
    ;; save the highlighting informatin in the DB.
    (semantic-overlay-put o 'face face)
    (semantic-overlay-put o 'semantic-dso t)
    (semantic-overlay-move o begin end)
    ))

;;; Decoration mode creation macros
;;
;; These macros will assist in the creation of new decoration modes.
(defmacro define-semantic-decoration-with-secondary-overlay (name doc)
  "Create a new decoration style that uses a secondary overlay.
This creates a new overload method called `NAME-p'.  You must implement
`NAME-p-default'.  It also creates a new overload method called
`NAME-highlight'.  You must implement `NAME-highlight-default'.
`NAME-highlight-default' must use
`semantic-decorate-create-secondary-overlay' when applying decoration
to the overlay so that decoration mode can manage it.
DOC is a documentation string describing the decoration style NAME.
It is appended to the autogenerated doc for -p and -highlight methods."
  (let ((-p 	    (intern (concat (symbol-name name) "-p")))
	(-highlight (intern (concat (symbol-name name) "-highlight")))
	)
    `(eval-and-compile
       ;; Create an override method to specify if a given tag belongs
       ;; to this type of decoration
       (define-overload ,-p (tag)
	 ,(concat "Determine if TAG should be decorated by "
		  (symbol-name name) "\n" doc))
       ;; Create an override method that will perform the highlight
       ;; operation if the -p method returns non-nil.
       (define-overload ,-highlight (tag)
	 ,(concat "Highlight TAG for " (symbol-name name) "\n"
		  doc))
       ;; Add this to the list of primary decoration modes.
       (add-to-list 'semantic-decoration-secondary-alist
		    ,(list 'quote (list -p -highlight)))
       ))
  )


;;; Tag Boundaries
;;
(define-semantic-decoration-with-secondary-overlay
  semantic-tag-boundary
  "Place an overline in front of each long tag.
Does not provide overlines for prototypes.")


(defface semantic-tag-boundary-face
  '((((class color) (background dark))
     (:overline "cyan"))
    (((class color) (background light))
     (:overline "blue")))
  "*Face used to show unmatched syntax in.
The face is used in  `semantic-decoration-mode'."
  :group 'semantic-faces)

(defun semantic-tag-boundary-p-default (tag)
  "Non nil if TAG is a type, or a non-prototype function."
  (let ((c (semantic-tag-class tag)))

    (and
     (or
      ;; All types get a line?
      (eq c 'type)
      ;; Functions which aren't prototypes get a line.
      (and (eq (semantic-tag-class tag) 'function)
	   (not (semantic-tag-get-attribute tag :prototype-flag)))
      )
     ;; Nothing smaller than a few lines
     (> (- (semantic-tag-end tag) (semantic-tag-start tag)) 150)
     ;; Random truth
     t
     )))

(defun semantic-tag-boundary-highlight-default (tag)
  "Highlight the first line of TAG as a boundary."
  (semantic-decoration-create-secondary-overlay
   tag 
   (semantic-tag-start tag)
   (save-excursion
     (set-buffer (semantic-tag-buffer tag))
     (goto-char (semantic-tag-start tag))
     (end-of-line)
     (forward-char 1)
     (point))
   'semantic-tag-boundary-face))


(provide 'semantic-decorate-mode)

;;; semantic-decorate-mode.el ends here
