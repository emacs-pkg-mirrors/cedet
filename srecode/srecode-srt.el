;;; srecode-srt.el --- argument handlers for SRT files

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-srt.el,v 1.1 2008/01/21 14:47:54 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Filters for SRT files, the Semantic Recoder template files.

;;; Code:

(defvar srecode-read-variable-name-history nil
  "History for `srecode-read-variable-name'.")

;;;###autoload
(defun srecode-read-variable-name (prompt &optional initial hist default)
  "Read in the name of a declaired variable in the current SRT file.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit."
  (let* ((newdict (srecode-create-dictionary))
	 (currfcn (semantic-current-tag))
	 )
    (srecode-resolve-argument-list
     (mapcar 'read
	     (semantic-tag-get-attribute currfcn :arguments))
     newdict)

    (with-slots (namehash) newdict
      (completing-read prompt namehash nil nil initial
		       (or hist 'srecode-read-variable-name-history)
		       default))
    ))

;;;###autoload
(defun srecode-read-template-name (prompt &optional initial hist default)
  "Read in the name of a declaired variable in the current SRT file.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit."
  ;; @todo - Find the list of templates available in this
  ;; context, and complete on them.
  (read-string prompt initial hist default)
  )

;;;###autoload
(defun srecode-semantic-handle-:srt (dict)
  "Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end"
  (let* ((vars (semantic-find-tags-by-class 'variable (current-buffer)))
	 (es (semantic-find-first-tag-by-name "escape_start" (current-buffer)))
	 (ee (semantic-find-first-tag-by-name "escape_end" (current-buffer))))
    (srecode-dictionary-set-value dict "ESCAPE_START"
				  (if es
				      (read (semantic-tag-variable-default es))
				    "{{"))
    (srecode-dictionary-set-value dict "ESCAPE_END"
				  (if ee
				      (read (semantic-tag-variable-default ee))
				    "}}"))
    ))

(provide 'srecode-srt)
;;; srecode-srt.el ends here
