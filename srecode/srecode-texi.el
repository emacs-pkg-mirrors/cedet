;;; srecode-texi.el --- Srecode texinfo support.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-texi.el,v 1.1 2008/01/22 22:53:28 zappo Exp $

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
;; Texinfo semantic recoder support.
;;
;; Contains some handlers, and a few simple texinfo srecoder applications.

(require 'semantic)

;;; Code:
(defun srecode-texi-add-menu ()
  "Add an item into the current menu.  Add @node statements as well."
  (interactive)
  
  )

;;;###autoload
(defun srecode-semantic-handle-:texi (dict)
  "Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level"
  (let ((tags (reverse (semantic-find-tag-by-overlay)))
	(level nil))
    (while (and tags (not (semantic-tag-of-class-p (car tags) 'section)))
      (setq tags (cdr tags)))
    (when tags
      (save-excursion
	(goto-char (semantic-tag-start (car tags)))
	(when (looking-at "@\\(\\w+\\)")
	  (setq level (match-string 1))
	  )))
    (srecode-dictionary-set-value dict "LEVEL" (or level "chapter"))
    (let ((nl (assoc level '( ( nil . "chapter" )
			      ("chapter" . "section")
			      ("section" . "subsection")
			      ("subsection" . "subsubsection")
			      ("subsubsection" . "subsubsection")
			      ))))
      (srecode-dictionary-set-value dict "NEXTLEVEL" (cdr nl)))))

(provide 'srecode-texi)
;;; srecode-texi.el ends here
