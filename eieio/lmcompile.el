;;; lmcompile.el --- highlight compile error lines

;;
;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Keywords: lisp
;;
;; Copyright (C) 2003 Eric M. Ludlam
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;;  This package uses the compile package, and the linemark package to
;; highlight all lines showing errors.

;;; Commentary:
;; 

(require 'linemark)

;;; Code:
(defclass lmcompile-linemark-group (linemark-group)
  (
   )
  "Linemark Group for compile error highlights.")

(defclass lmcompile-linemark-entry (linemark-entry)
  ((errormarker :initarg :errormarker
		:type marker
		:documentation
		"Marker pointing to the source of the match."))
  "Linemark Group for one compile error highlight.
Tracks additional information about the error.")

(defmethod linemark-new-entry ((g linemark-group) &rest args)
  "Create a new entry for G using init ARGS."
  (let ((f (plist-get args :filename))
	(l (plist-get args :line)))
    (apply 'lmcompile-linemark-entry (format "%s %d" f l)
	   args)))

(defmethod linemark-display ((e lmcompile-linemark-entry) active-p)
  "Set object E to be active or inactive."
  ;; A bug in linemark prevents individual entry colors.
  ;; Fix the color here.
  (when active-p
    (condition-case nil
	(save-excursion
	  (set-buffer (marker-buffer (oref e errormarker)))
	  (goto-char (oref e errormarker))
	  
	  (let ((face (cond ((re-search-forward "error" (point-at-eol) t)
			     'linemark-stop-face)
			    ((re-search-forward "warning" (point-at-eol) t)
			     'linemark-caution-face)
			    (t
			     'linemark-funny-face))))
	    (oset e :face face)
	    ))
      (error nil))
    )
  ;; Do the rest of our work
  (call-next-method)

  ;; Add a tool tip
  (when (and active-p
	     (slot-boundp e 'overlay)
	     (oref e overlay))

    (let ((em (oref e errormarker))
	  (txt nil))
      (condition-case nil
	  (save-excursion
	    (set-buffer (marker-buffer em))
	    (goto-char em)
	    (setq txt (buffer-substring-no-properties
		       (point-at-bol) (point-at-eol)))
	    )
	(error nil))

      (when txt
	(linemark-overlay-put (oref e overlay)
			      'help-echo
			      txt))
      ))
  )

(defun lmcompile-create-group (name)
  "Create a group object for tracking linemark entries.
Do not permit multiple groups with the same NAME."
  (let ((newgroup (lmcompile-linemark-group name))
	(foundgroup nil)
	(lmg linemark-groups))
    (while (and (not foundgroup) lmg)
      (if (string= name (object-name-string (car lmg)))
	  (setq foundgroup (car lmg)))
      (setq lmg (cdr lmg)))
    (if foundgroup
	(setq newgroup foundgroup)
      (setq linemark-groups (cons newgroup linemark-groups))
      newgroup)))

(defvar lmcompile-error-group (lmcompile-create-group "compiler errors")
  "The LMCOMPILE error group object.")

(defun lmcompile-clear ()
  "Flush all compile error entries."
  (interactive)
  (mapcar (lambda (e) (linemark-delete e))
	  (oref lmcompile-error-group marks)))

(defun lmcompile-do-highlight ()
  "Do compilation mode highlighting.
Works on grep, compile, or other type mode."
  (interactive)

  ;; Flush out the old
  (lmcompile-clear)

  ;; Set the buffer apropriately
  (setq compilation-last-buffer (compilation-find-buffer))

  ;; Get the list of errors to be activated.
  (compile-reinitialize-errors nil)
  
  (let ((marks
	 (save-excursion
	   (set-buffer compilation-last-buffer)
	   compilation-error-list))
	)
    (while marks
      (let ((errmark (nth 0 (car marks)))
	    (file (nth 1 (car marks)))
	    (line (nth 2 (car marks)))
	    (face nil)
	    (case-fold-search t)
	    (entry nil)
	    )
	(setq file (concat (car (cdr file))
			   (car file)))

	;; We've got the goods, lets add in an entry.
	;; If we can't find the file, skip it.  It'll be
	;; found eventually.
	(when (file-exists-p file)

	  (setq entry
		(linemark-add-entry
		 lmcompile-error-group
		 :filename file
		 :line line
		 :errormarker errmark
		 ))

	  ))
      (setq marks (cdr marks)))))

(provide 'lmcompile)

;;; lmcompile.el ends here
