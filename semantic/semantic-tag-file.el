;;; semantic-tag-file.el --- Routines that find files based on tags.

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-tag-file.el,v 1.8 2004/04/29 10:14:01 ponced Exp $

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
;; A tag, by itself, can have representations in several files.
;; These routines will find those files.

(require 'semantic-tag)

;;; Code:

;;; Location a TAG came from.
;;
;;;###autoload
(define-overload semantic-go-to-tag (&optional tag parent)
  "Go to the location of TAG.
TAG may be a stripped element, in which case PARENT specifies a
parent tag that has position information.
Different behaviors are provided depending on the type of tag.
For example, dependencies (includes) will seek out the file that is
depended on (see `semantic-dependency-tag-file'."
  (or tag (setq tag (car (semantic-find-tag-by-overlay nil))))
  (unless (and (eq (semantic-tag-class tag) 'include)
               (let ((f (semantic-dependency-tag-file tag)))
                 (if f (find-file f))))
    (:override
     (if (semantic-tag-buffer tag)
         ;; If the tag has no buffer, it may be deoverlayed.
         ;; Assume the tool doing the finding knows that we came
         ;; in from a database, and use the current buffer.
         (set-buffer (semantic-tag-buffer tag)))
     (if (semantic-tag-with-position-p tag)
         ;; If it's a number, go there
         (goto-char (semantic-tag-start tag))
       ;; Otherwise, it's a trimmed vector, such as a parameter,
       ;; or a structure part.
       (if (not parent)
           nil
         (if (semantic-tag-with-position-p parent)
             (progn
               (if (semantic-tag-buffer parent)
                   ;; If this parent tag has no buffer, then it
                   ;; may be deoverlayed.
                   (set-buffer (semantic-tag-buffer parent)))
               (goto-char (semantic-tag-start parent))
               ;; Here we make an assumption that the text returned by
               ;; the parser and concocted by us actually exists
               ;; in the buffer.
               (re-search-forward (semantic-tag-name tag) nil t))))))))

(make-obsolete-overload 'semantic-find-nonterminal
                        'semantic-go-to-tag)

;;; Dependencies
;;
;; A tag which is of type 'include specifies a dependency.
;; Dependencies usually represent a file of some sort.
;; Find the file described by a dependency.
;;; Code:
;;;###autoload
(defvar semantic-dependency-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific to
the file being included.
If `semantic-find-dependency' is overridden for a given language, this
path is most likely ignored.

TODO: use ffap.el to locate such items.")
(make-variable-buffer-local `semantic-dependency-include-path)

;;;###autoload
(define-overload semantic-dependency-tag-file (&optional tag)
  "Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths."
  (or tag (setq tag (car (semantic-find-tag-by-overlay nil))))
  (unless (semantic-tag-of-class-p tag 'include)
    (signal 'wrong-type-argument (list tag 'include)))
  ;; First, see if this file exists in the current EDE project
  (if (and (fboundp 'ede-expand-filename) ede-minor-mode
	   (ede-expand-filename (ede-toplevel)
				(semantic-tag-name tag)))
      (ede-expand-filename (ede-toplevel)
			   (semantic-tag-name tag))
    (:override
     (save-excursion
       ;; TODO: Allow TAG to travel with originating file info.
       (when (semantic-tag-buffer tag)
         (set-buffer (semantic-tag-buffer tag)))
       (let ((name (semantic-tag-name tag)))
         (cond ((file-exists-p name)
                (expand-file-name name))
               ((and (symbolp semantic-dependency-include-path)
                     (fboundp semantic-dependency-include-path))
                (funcall semantic-dependency-include-path name))
               (t
                (let ((p semantic-dependency-include-path)
                      (found nil))
                  (while (and p (not found))
                    (if (file-exists-p (concat (car p) "/" name))
                        (setq found (concat (car p) "/" name)))
                    (setq p (cdr p)))
                  found))))))))

(make-obsolete-overload 'semantic-find-dependency
                        'semantic-dependency-tag-file)

;;; PROTOTYPE FILE
;;
;; In C, a function in the .c file often has a representation in a
;; corresponding .h file.  This routine attempts to find the
;; prototype file a given source file would be associated with.
;; This can be used by prototype manager programs.
;;;###autoload
(define-overload semantic-prototype-file (buffer)
  "Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in."
  (:override
   ;; Perform some default behaviors
   (if (and (fboundp 'ede-header-file) ede-minor-mode)
       (save-excursion
         (set-buffer buffer)
         (ede-header-file))
     ;; No EDE options for a quick answer.  Search.
     (save-excursion
       (set-buffer buffer)
       (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
           (match-string 1))))))

(semantic-alias-obsolete 'semantic-find-nonterminal
                         'semantic-go-to-tag)

(semantic-alias-obsolete 'semantic-find-dependency
                         'semantic-dependency-tag-file)


(provide 'semantic-tag-files)

;;; semantic-tag-find.el ends here
