;;; ede-proj-nusc.el --- EDE Generic Project Emacs Lisp support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-misc.el,v 1.2 1999/11/09 20:36:09 zappo Exp $

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
;; Handle miscelaneous compilable projects in and EDE Project file.
;; This misc target lets the user link in custom makefiles to an EDE
;; project.

;;; Code:
(defclass ede-proj-target-makefile-miscelaneous (ede-proj-target-makefile)
  ((submakefile :initarg :submakefile
		:initform "makefile.misc"
		:type string
		:custom string
		:documentation
		"Miscelaneous sources which have a specialized makefile.
The sub-makefile is used to build this target.")
   )
   "Miscelaneous target type.
A user-written makefile is used to build this target.
All listed sources are included in the distribution.")

(defmethod ede-want-file-p ((obj ede-proj-target-makefile-miscelaneous) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.texi?$" file))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-miscelaneous))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_MISC"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-miscelaneous))
  "Return a list of files which THIS target depends on."
  (list (oref this submakefile)))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile-miscelaneous))
  "Create the make rule needed to create an archive for THIS."
  (call-next-method)
  (insert (ede-name this) ": " (oref this submakefile) "\n"
	  "\t$(MAKE) -f " (oref this submakefile) "\n\n"))

(provide 'ede-proj-misc)

;;; ede-proj-misc.el ends here
