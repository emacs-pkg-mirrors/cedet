;;; semanticdb-mk.el --- Command line database builder

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-mk.el,v 1.1 2002/08/11 01:47:36 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
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
;; 
;;; Commentary:
;;
;; For use by semanticdb.sh for building tag files.
;;

;;; Code
;;
(defun semanticdb-mk-find-eieio ()
  "Find EIEIO in the usual places."
  (cond 
   ((locate-library "eieio")
    nil)
   ((getenv "EIEIO_LOADPATH")
    (add-to-list 'load-path (getenv "EIEIO_LOADPATH")))
   (t;; For now, lets hope it was installed in their .emacs file.
    (let ((de (find-file-noselect "~/.emacs"))
	  (path nil))
      (save-excursion
	(set-buffer de)
	(goto-char (point-min))
	(while (not path)
	  (re-search-forward "eieio")
	  (if (re-search-backward "\"" nil t)
	      (progn
		(mark-sexp 1)
		(setq path
		      (buffer-substring-no-properties (point) (mark))))))
	(add-to-list 'load-path (read path)))))))

(if (not noninteractive)
    (error "You should not load semanticdb-mk interactivly."))

;; Force semantic onto the load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Add eieio to the path.
(semanticdb-mk-find-eieio)

;; Initialize semantic
(require 'semantic-load)

;; Turn on semanticdb
(global-semanticdb-minor-mode 1)

;; Process loaded buffers from the command line.
(let ((args command-line-args))
  ;; Move past this load file being loaded.
  (while (not
	  (progn
	    ;;(message "Compare %s to %s" (car args) load-file-name)
	    (string= (car args) load-file-name)))
    (setq args (cdr args)))
  (setq args (cdr args))
  ;; Grab the rest of the file names.
  ;; For each file, load it in, let semantic evaluate it for tags.
  (while args
    (message "Loading %s" (car args))
    (find-file (car args))
    (setq args (cdr args)))
  )

;; Save the databases.
(semanticdb-save-all-db)

;; Done
