;;; ede-proj-info.el --- EDE Generic Project texinfo support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-info.el,v 1.2 1999/11/09 20:35:31 zappo Exp $

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Handle texinfo in and EDE Project file.

;;; Code:
(defclass ede-proj-target-makefile-info (ede-proj-target-makefile)
  ((mainmenu :initarg :mainmenu
	     :initform ""
	     :type string
	     :custom string
	     :documentation "The main menu resides in this file.
All other sources should be included independently."))
  "Target for a single info file.")

(defmethod ede-want-file-p ((obj ede-proj-target-makefile-info) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.texi?$" file))

(defmethod ede-proj-makefile-garbage-patterns
  ((this ede-proj-target-makefile-info))
  "Return a list of patterns that are considred garbage to THIS.
These are removed with make clean."
  '("*.info")
  )

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile-info))
  "Insert rules to build THIS set of texinfo documentation files."
  (call-next-method)
  (let ((mm (oref this mainmenu)))
    (if (or (string= mm "") (not mm))
	(setq mm (car (oref this source))))
    (insert "\n" (ede-name this) ": $(" (ede-pmake-varname this) "_INFOS)\n"
	    "\tmakeinfo " mm "\n")))

(provide 'ede-proj-info)

;;; ede-proj-info.el ends here
