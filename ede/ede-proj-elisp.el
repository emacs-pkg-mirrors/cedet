;;; ede-proj-elisp.el --- EDE Generic Project Emacs Lisp support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-elisp.el,v 1.2 1999/11/10 14:18:30 zappo Exp $

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
;; Handle Emacs Lisp in and EDE Project file.

;;; Code:
(defclass ede-proj-target-elisp (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (load-path :initarg :load-path
	      :initform nil
	      :type list
	      :custom (repeat string)
	      :documentation "Additional load-path arguments.
When compiling from the command line, these are added to the makefile.
When compiling from within emacs, these are ignored.")
   (requirements :initarg :requirements
		 :initform nil
		 :type list
		 :custom (repeat string)
		 :documentation
		 "Additional packages that should be loaded before building.
When using eieio, tools generally need to be loaded before you can compile
them safely.")
   )
  "This target consists of a group of lisp files.
A lisp target may be one general program with many separate lisp files in it.")

(defmethod ede-want-file-p ((obj ede-proj-target-elisp) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.el$" file))

(defmethod project-compile-target ((obj ede-proj-target-elisp))
  "Compile all sources in a Lisp target OBJ."
  (mapcar (lambda (src)
	    (let ((elc (concat (file-name-sans-extension src) ".elc")))
	      (if (or (not (file-exists-p elc))
		      (file-newer-than-file-p src elc))
		  (byte-compile-file src))))
	  (oref obj source)))

;;; Makefile generation functions
;;
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-elisp))
  "Return the variable name for THIS's sources."
  (cond ((ede-proj-automake-p)
	 "lisp_LISP")
	(t (concat (ede-pmake-varname this) "_LISP"))))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-elisp))
  "Insert variables needed by target THIS."
  (call-next-method this)
  (insert "EMACS=" (car command-line-args) "\n")
  (if (oref this load-path)
      (insert "LOADPATH=" (mapconcat (lambda (a) a) (oref this load-path) " ")
	      "\n")))

(defmethod ede-proj-makefile-garbage-patterns ((this ede-proj-target-elisp))
  "Return a list of patterns that are considred garbage to THIS.
These are removed with make clean."
  '("*.elc")
  )

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-elisp))
  "Insert rules to build THIS set of Emacs Lisp files."
  (call-next-method)
  (insert (ede-name this) ":\n"
	  "\t@echo \"(add-to-list 'load-path \\\"$(PWD)\\\")\" > "
	  (ede-name this) "-compile-script\n")
  (if (oref this load-path)
      (progn
	(insert "\t@for loadpath in ${LOADPATH}; do \\\n")
	(insert "\t  echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> "
		(ede-name this) "-compile-script; \\\n")
	(insert "\t  done\n")))
;  (let ((lp (oref this load-path)))
;    (while lp
;      (insert "\t@echo \"(add-to-list 'load-path \\\"" (car lp) "\\\")\" >> "
;	      (ede-name this) "-comp\n")
;      (setq lp (cdr lp))))
  (let ((ar (oref this requirements)))
    (while ar
      (insert "\t@echo \"(require '" (car ar) ")\" >> " (ede-name this)
	      "-compile-script\n")
      (setq ar (cdr ar))))
  (insert "\t$(EMACS) -batch -l " (ede-name this) "-compile-script "
	  "-f batch-byte-compile  $(" (ede-proj-makefile-sourcevar this)
	  ")\n"))

(provide 'ede-proj-elisp)

;;; ede-proj-elisp.el ends here
