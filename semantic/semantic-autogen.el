;;; semantic-autogen.el --- Auto load statement generator

;;; Copyright (C) 2002, 2003 Eric M. Ludlam

;; X-CVS: $Id: semantic-autogen.el,v 1.8 2003/02/15 09:38:04 ponced Exp $

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
;; Automatically generate autoloads for Semantic.
;;
;; Future: Automatically generate parser initialization

;;; History:
;; 

;;; Code:
;;

;; Load this in first
(require 'autoload)

;;; Compatibility
(defun semantic-autogen-noninteractive ()
  "Return non-nil if running non-interactively."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

(if (fboundp 'keywordp)
    (defalias 'semantic-autogen-keywordp 'keywordp)
  (defun semantic-autogen-keywordp (object)
    "Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray."
    (and (symbolp object)
         (char-equal ?: (aref 0 (symbol-name object)))))
  )

(when (semantic-autogen-noninteractive)
  ;; If the user is doing this non-interactively, we need to set up
  ;; these conveniences.
  (add-to-list 'load-path nil)
  (setq find-file-hooks nil
        find-file-suppress-same-file-warnings t)
  )

(defadvice make-autoload (before semantic-make-autoload activate)
  "Extend `make-autoload' with support for particular Semantic forms.
When a such form, like defclass, defmethod, etc., is recognized, it is
replaced with side effect by an equivalent known form before calling
the true `make-autoload' function."
  (if (consp (ad-get-arg 0))
      (let* ((form (ad-get-arg 0))
             (car (car-safe form))
             name args doc
             )
        (cond
         ((eq car 'define-overload)
          (setcar form 'defun)
          )
         ((eq car 'defmethod)
          (setq name (nth 1 form)
                args (nthcdr 2 form))
          (if (semantic-autogen-keywordp (car args))
              (setq args (cdr args)))
          (setq doc  (nth 1 args)
                args (car args))
          (setcar form 'defun)
          (setcdr form (list name args (if (stringp doc) doc)))
          )
         ((eq car 'defclass)
          (setq name (nth 1 form)
                args (nth 2 form)
                doc  (nth 4 form))
          (setcar form 'defun)
          (setcdr form (list name args (if (stringp doc) doc)))
          ))
        )))

(defconst semantic-autogen-header
  "Auto-generated semantic autoloads"
  "Header of the auto-generated autoloads file.")

(defvar semantic-autogen-file "semantic-al.el"
  "Name of the auto-generated autoloads file.")

(defvar semantic-autogen-subdirs '("wisent" "bovine")
  "Sub-directories to scan for autoloads.")

(defun semantic-autogen-update-header ()
  "Update header of the auto-generated autoloads file.
Run as `write-contents-hooks'."
  (when (string-equal generated-autoload-file (buffer-file-name))
    (let ((tag (format ";;; %s ---" (file-name-nondirectory
                                     (buffer-file-name)))))
      (message "Updating header...")
      (goto-char (point-min))
      (cond
       ;; Replace existing header line
       ((re-search-forward (concat "^" (regexp-quote tag)) nil t)
        (beginning-of-line)
        (kill-line 1)
        )
       ;; Insert header before first ^L encountered (XEmacs)
       ((re-search-forward "^" nil t)
        (beginning-of-line)
        ))
      (insert tag " " semantic-autogen-header)
      (newline)
      (message "Updating header...done")
      nil ;; Say not already written.
      )))

(defun semantic-hack-autoloads ()
  "Update semantic autoloads from sources.
Autoloads file name is defined in variable `semantic-autogen-file'."
  (interactive)
  (let* ((default-directory (file-name-directory (locate-library "semantic")))
         (generated-autoload-file (expand-file-name semantic-autogen-file))
         (subdirs (mapcar 'expand-file-name semantic-autogen-subdirs))
         (write-contents-hooks '(semantic-autogen-update-header))
         (command-line-args-left (cons default-directory subdirs))
         )
    ;; If file don't exist, and is not automatically created...
    (unless (or (file-exists-p generated-autoload-file)
                (fboundp 'autoload-ensure-default-file))
      ;; Create a file buffer.
      (find-file generated-autoload-file)
      ;; Use Unix EOLs, so that the file is portable to all platforms.
      (setq buffer-file-coding-system 'raw-text-unix)
      ;; Insert the header so that the buffer is not empty.
      (semantic-autogen-update-header))
    (batch-update-autoloads)))

(provide 'semantic-autogen)

;;; semantic-autogen.el ends here
