;;; ede-proj-archive.el --- EDE Generic Project archive support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-archive.el,v 1.3 1999/11/09 20:34:58 zappo Exp $

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
;; Handle object code archives in and EDE Project file.

(require 'ede-proj-obj)

;;; Code:
(defclass ede-proj-target-makefile-archive
  (ede-proj-target-makefile-objectcode)
  ()
  "This target generates an object code archive.")

(defmethod ede-proj-makefile-insert-rules
  ((this ede-proj-target-makefile-archive))
  "Create the make rule needed to create an archive for THIS."
  (call-next-method)
  (insert "# Sorry, rule for making archive " (ede-name this)
	  "has not yet been implemented.\n\n")
  )


(provide 'ede-proj-archive)

;;; ede-proj-archive.el ends here
