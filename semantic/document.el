;;; document.el --- Use the bovinator to aid in generating documentation.

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: doc
;; X-RCS: $Id: document.el,v 1.1 2000/04/30 23:01:11 zappo Exp $

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
;; Document provides the ability to create a documentation framework
;; for functions and variables.  It uses `semantic' to find the
;; current function, and to provide additional context information for
;; generating the documentation.
;;
;;   Document then provides some rules for creating English Text based
;; on the name of a given function, it's return type, or variable
;; type.  It also has output rules for texinfo, or comments.

;;; Code:
(defvar document-current-output-file nil
  "A marker for the current output file used for documentation.")


;;; User Functions
;;
(defun document (&optional resetfile)
  "Document the function or variable the cursor is in.
Optional argument RESETFILE is provided w/ universal argument.
When non-nil, query for a new documentation file."
  (interactive (if current-prefix-arg
		   (save-excursion
		     (list (document-locate-file
			    (current-buffer) t)))))
  ;; First, garner some information from Semantic.
  (semantic-bovinate-toplevel nil t t)
  (let ((cdi (semantic-find-nonterminal-by-position (point) (current-buffer)))
	(cdib (current-buffer)))
    ;; Make sure we have a file.
    (document-locate-file (current-buffer))
    ;; Now go and create the documentation
    (if (not document-current-output-file)
	(error "No file found for your documentation"))
    (set-buffer (marker-buffer document-current-output-file))
    (goto-char document-current-output-file)
    (cond ((eq major-mode 'texinfo-mode)
	   (document-insert-texinfo cdi cdib))
	  (t
	   (document-insert-mode-comment cdi cdib)))
    (setq document-current-output-file (point-marker))
    ))

;;; Documentation insertion functions
;;
(defun document-insert-texinfo (nonterm buffer)
  "Insert texinfo documentation about NONTERM from BUFFER."
  (insert "\n")
  (let ((tt (semantic-token-token nonterm)))
    (insert "@"
	    (cond ((eq tt 'variable)
		   "defvar")
		  ((eq tt 'function)
		   "defun")
		  ((eq tt 'type)
		   "deftype")
		  (t (error "Don't know how to document that")))
	    " "
	    (semantic-token-name nonterm))
    (if (eq tt 'function)
	(let ((args (semantic-token-function-args nonterm)))
	  (while args
	    (insert " ")
	    (if (stringp (car args))
		(insert (car args))
	      (insert (semantic-token-name (car args))))
	    (setq args (cdr args)))))
    (insert "\n")
    (insert (document-massage-to-texinfo
	     nonterm
	     buffer
	     (document-generate-documentation nonterm buffer)))
    (insert "\n@end "
	    (cond ((eq tt 'variable)
		   "defvar")
		  ((eq tt 'function)
		   "defun")
		  ((eq tt 'type)
		   "deftype"))
	    "\n")))

(defun document-insert-mode-comment (nonterm buffer)
  "Insert mode-comment documentation about NONTERM from BUFFER."
  (error "Not yet implemented"))

;;; Documentatation generation functions
;;
(defun document-generate-documentation (nonterm buffer)
  "Return a plain string documenting NONTERM from BUFFER."
  (let ((doc ;; Second, does this thing have docs in the source buffer which
	 ;; an override method might be able to find?
	 (semantic-find-documentation buffer nonterm)
	 ))
    (if (not doc)
	(document-generate-new-documentation nonterm buffer)
      ;; Ok, now lets see what sort of formatting there might be,
      ;; and see about removing some of it.. (Tables of arguments,
      ;; and that sort of thing.)
      nil
      ;; Return the string.
      doc)))

(defun document-generate-new-documentation (nonterm buffer)
  "Look at elements of NONTERM in BUFFER to make documentation.
This will create a new documentation string from scratch."
  "No code for new docs yet."
  )

;;; Texinfo mangling.
;;
(defun document-massage-to-texinfo (nonterm buffer string)
  "Massage NONTERM's documentation from BUFFER as STRING.
This is to take advantage of TeXinfo's markup symbols."
  (if (save-excursion (set-buffer buffer)
		      (eq major-mode 'emacs-lisp-mode))
      ;; Elisp has a few advantages.  Hack it in.
      (setq string (document-texify-elisp-docstring string)))
  ;; Else, other languages are simpler.  Also, might as well
  ;; run the elisp version through also.
  (let ((case-fold-search nil))
    (while (string-match
	    "\\(^\\|[^{]\\)\\<\\([A-Z0-9_]+\\)\\>\\($\\|[^}]\\)"
	    string)
      (setq string (concat (substring string 0 (match-beginning 2))
			   "@var{"
			   (match-string 2 string)
			   "}"
			   (substring string (match-end 2)))))
    )
  string)

;; This FN was taken from EIEIO and modified.  Maybe convert later.
(defun document-texify-elisp-docstring (string)
  "Take STRING, (a normal doc string), and convert it into a texinfo string.
For instances where CLASS is the class being referenced, do not Xref
that class.

 `function' => @dfn{function}
 `variable' => @code{variable}
 `class'    => @code{class} @xref{class}
 `unknown'  => @code{unknonwn}
 'quoteme   => @code{quoteme}
 non-nil    => non-@code{nil}
 t          => @code{t}
 :tag       => @code{:tag}
 [ stuff ]  => @code{[ stuff ]}
 Key        => @kbd{Key}     (key is C\\-h, M\\-h, SPC, RET, TAB and the like)"
  (while (string-match "`\\([-a-zA-Z0-9]+\\)'" string)
    (let* ((vs (substring string (match-beginning 1) (match-end 1)))
	   (v (intern-soft vs)))
      (setq string
	    (concat
	     (replace-match (concat
			     (if (fboundp v)
				 "@dfn{" "@code{")
			     vs "}")
		    nil t string)))))
  (while (string-match "\\( \\|^\\)\\(nil\\|t\\|'[-a-zA-Z0-9]+\\|:[-a-zA-Z0-9]+\\)\\([ ,]\\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(non-\\)\\(nil\\)\\)\\([ ,]\\|$\\)" string)
    (setq string (replace-match "\\3@code{\\4}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\[[^]]+\\]\\)\\( \\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(\\(C-\\|M-\\|S-\\)+\\([^ \t\n]\\|RET\\|SPC\\|TAB\\)\\)\\|\\(RET\\|SPC\\|TAB\\)\\)\\( \\|$\\)" string)
    (setq string (replace-match "@kbd{\\2}" t nil string 2)))
  string)

;;; Buffer finding and managing
;;
(defun document-find-file (file)
  "Load up the document file FILE.
Make it current, and return a marker for the location of newly inserted
documentation."
  (set-buffer (find-file-noselect file))
  ;; Theoretically, we should add some smarts here for positioning
  ;; the cursor.  For now, do some simple stuff.
  (if (eq (point) (point-min))
      (progn
	(switch-to-buffer (current-buffer))
	(error "Position cursor in %s, and try inserting documentation again"))
    (point-marker)))

(defun document-locate-file (buffer &optional override)
  "Return a file in which documentation belonging to BUFFER should be placed.
Optional argument OVERRIDE indicates to override the last used location."
  (if (and document-current-output-file (not override))
      document-current-output-file
    ;; Else, perform some default behaviors
    (let ((files (if (and (fboundp 'ede-documentation-files) ede-minor-mode)
		     (save-excursion
		       (set-buffer buffer)
		       (ede-documentation-files))
		   ))
	  (choice nil))
      (while files
	(setq choice (cons (list (car files)) choice)
	      files (cdr files)))
      (setq choice
	    (if choice
		(completing-read "Documentation File: "
				 choice
				 nil t (car choice))
	      (read-file-name "Documentation File: "
			      default-directory)))
      (setq document-current-output-file (document-find-file choice)))))
      

(provide 'document)

;;; document.el ends here
