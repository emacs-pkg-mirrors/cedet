;;; ede-proj-scheme.el --- EDE Generic Project scheme (guile) support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make, scheme
;; RCS: $Id: ede-proj-scheme.el,v 1.2 1999/11/09 20:36:45 zappo Exp $

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
;; Handle scheme (Guile) in and EDE Project file.
;; This is a specialized do nothing class.

;;; Code:
(defclass ede-proj-target-scheme (ede-proj-target)
  ((interpreter :initarg :interpreter
		:initform "guile"
		:type string
		:custom string
		:documentation "The preferred interpreter for this code.")
   )
  "This target consists of scheme files.")

(defmethod ede-want-file-p ((obj ede-proj-target-scheme) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.scm$" file))

(provide 'ede-proj-scheme)

;;; ede-proj-scheme.el ends here
