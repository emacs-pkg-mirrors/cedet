;;; semantic-autogen.el --- Auto load statement generator

;;; Copyright (C) 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-autogen.el,v 1.1 2002/08/08 17:46:04 zappo Exp $

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

;;; Code
;;

;;;###autoload
(defun semantic-hack-autoloads ()
  "Create semantic autoloads from sources."
  (interactive)
  (let* ((dir (file-name-directory (locate-library "semantic")))
	  (generated-autoload-file (concat dir "semantic-al.el"))
	  )
    (find-file (concat dir "semantic-al.el"))
    (erase-buffer)

    (insert ";;; semantic-al.el --- Auto-generated file filled with autoloads.\n\n")
    (update-autoloads-from-directories dir)
    )
  (newline 2))

(provide 'semantic-autogen)

;;; semantic-alg.el ends here
