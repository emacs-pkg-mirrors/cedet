;;; ede-proj-shared.el --- EDE Generic Project shared library support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-shared.el,v 1.4 2000/07/12 14:09:48 zappo Exp $

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
;; Handle shared object libraries in and EDE Project file.
;; Tries to deal with libtool and non-libtool situations.

(eval-and-compile (require 'ede-proj-prog))

;;; Code:
(defclass ede-proj-target-makefile-shared-object
  (ede-proj-target-makefile-program)
  ((ldflags :custom (repeat (string :tag "Libtool flag"))
	    :documentation
	    "Additional flags to add when linking this target with libtool.
Use ldlibs to add addition libraries.")
   (libtool :initarg :libtool
	    :initform nil
	    :custom boolean
	    :documentation
	    "Non-nil if libtool should be used to generate the library.")
   )
  "This target generates a shared library using libtool.")

; This variable is used in automake for listing active libraries.
;(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-shared-object))
;  "Return the variable name for THIS's sources."
;  (concat (oref this name) "_LTLIBRARIES"))

(provide 'ede-proj-shared)

;;; ede-proj-shared.el ends here
