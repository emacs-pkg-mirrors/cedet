;;; ede-pmake.el --- EDE Generic Project Makefile code generator.

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-pmake.el,v 1.1 1999/02/26 02:50:38 zappo Exp $

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
;; Code generator for Makefiles.

(require 'ede-proj)

;;; Code:
(defmethod ede-proj-makefile-create ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS.
MFILENAME is the makefile to generate."
  (let ((mt nil) tmp
	(isdist (string= mfilename (ede-proj-dist-makefile this))))
    ;; Collect the targets that belong in a makefile.
    (with-slots (targets) this
      (while targets
	(if (and (obj-of-class-p (car targets) 'ede-proj-target-makefile)
		 (string= (oref (car targets) makefile) mfilename))
	    (setq mt (cons (car targets) mt)))
	(setq targets (cdr targets))))
    (save-excursion
      (set-buffer (find-file-noselect mfilename))
      (erase-buffer)
      (insert
       "# Automatically Generated " (file-name-nondirectory mfilename)
       " by EDE.\n"
       "# For use with: "
       (with-slots (makefile-type) this
	 (cond ((eq makefile-type 'Makefile) "make")
	       ((eq makefile-type 'Makefile.in) "autoconf")
	       ((eq makefile-type 'Makefile.am) "automake")
	       (t (error ":makefile-type in project invalid"))))
       "\n#\n"
       "# DO NOT MODIFY THIS FILE UNLESS YOU DO NOT PLAN ON USING EDE FOR\n"
       "# FUTURE DEVELOPMENT.  EDE is the Emacs Development Environment.\n"
       "# \n")
      (cond
       ((eq (oref this makefile-type) 'Makefile)
	(insert "DISTDIR=" (oref this name) "-" (oref this version) "\n\n")
	(ede-proj-makefile-insert-variables this)
	;; Distribution variables
	(if isdist
	    (setq tmp (oref this targets))
	  (setq tmp mt))
	(while tmp
	  (ede-proj-makefile-insert-variables (car tmp))
	  (setq tmp (cdr tmp)))
	(ede-proj-makefile-insert-rules this)
	(setq tmp mt)
	(insert "\n\nall:")
	(while tmp (insert " " (oref (car tmp) name))
	       (setq tmp (cdr tmp)))
	(insert "\n\n")
	(setq tmp mt)
	(while tmp
	  (ede-proj-makefile-insert-rules (car tmp))
	  (setq tmp (cdr tmp)))
	(if isdist
	    (progn
	      ;; Build DIST, TAG, and other rules here.
	      (insert "\ndist:\n")
	      (insert "\trm -rf $(DISTDIR)\n")
	      (insert "\tmkdir $(DISTDIR)\n")
	      (setq tmp (oref this targets))
	      (insert "\tcp")
	      (while tmp (insert " $(" (ede-proj-makefile-sourcevar (car tmp))
				 ")")
		     (setq tmp (cdr tmp)))
	      (insert " $(ede_FILES) $(DISTDIR)\n")
	      (insert "\ttar -cvzf $(DISTDIR).tar.gz $(DISTDIR)\n\n")
	      (insert "\n\n# End of Makefile\n"))))
       ((eq (oref this makefile-type) 'Makefile.in)
	)
       ((eq (oref this makefile-type 'Makefile.am))
	)
       (t (error "Unknown makefile type when generating Makefile")))
      (goto-char (point-min))
      (save-buffer))))

;;; SOURCE VARIABLE NAME CONSTRUCTION
;;
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target))
  "Return the variable name for THIS's sources."
  (concat (oref this name) "_AUX"))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-objectcode))
  "Return the variable name for THIS's sources."
  (concat (oref this name) "_SOURCE"))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-info))
  "Return the variable name for THIS's sources."
  (concat (oref this name) "_INFOS"))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-lisp))
  "Return the variable name for THIS's sources."
  (cond ((ede-proj-automake-p)
	 "lisp_LISP")
	(t (concat (oref this name) "_LISP"))))

;;; GENERIC VARIABLES
;;
(defmethod ede-proj-makefile-insert-variables ((this ede-proj-project))
  "Insert variables needed by target THIS."
  (mapcar (lambda (c) (insert (car c) "=" (cdr c) "\n"))
	  (oref this variables))
  (insert "\nede_FILES=" (file-name-nondirectory (oref this file)) " "
	  (ede-proj-dist-makefile this) "\n"))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target))
  "Insert variables needed by target THIS."
  (insert (ede-proj-makefile-sourcevar this) "="
	  (mapconcat (lambda (a) a) (oref this source) " ")
	  "\n"))

(defmethod ede-proj-makefile-insert-variables
  ((this ede-proj-target-makefile-objectcode))
  "Insert variables needed by target THIS."
  (call-next-method)
  (insert (ede-name this) "_OBJ="
	  (mapconcat (lambda (a)
		       (concat (file-name-sans-extension a) ".o"))
		     (oref this source) " ")
	  " "
	  (mapconcat (lambda (a)
		       (concat (file-name-sans-extension a) ".o"))
		     (oref this auxsource) " ")
	  "\n"))

;;; RULES
;;
(defmethod ede-proj-makefile-insert-rules ((this ede-proj-project))
  "Insert inference rules needed by THIS target."
  (mapcar 'ede-proj-makefile-insert-rules (oref this inference-rules)))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target))
  "Insert inference rules needed by THIS target."
  (mapcar 'ede-proj-makefile-insert-rules (oref this rules)))

(defmethod ede-proj-makefile-insert-rules ((this ede-makefile-rule))
  "Insert rules needed for THIS rule object."
  (insert (oref this target) ": " (oref this dependencies) "\n\t"
	  (mapconcat (lambda (c) c) (oref this rules) "\n\t")
	  "\n\n"))

(defmethod ede-proj-makefile-insert-rules
  ((this ede-proj-target-makefile-objectcode))
  "Insert rules needed by THIS target."
  (call-next-method)
  (insert (ede-name this) ": $(" (ede-name this) "_OBJ)\n"
	  ;; Compile line
	  "\t$(CC)"
	  ;; Extra link flags
	  " $(LDFLAGS) -o $@ $(" (ede-name this) "_OBJ)"
	  ;; Separate this out later.
	  (if (ede-proj-target-makefile-program-p this)
	      ;; Some libaries
	      (concat " "
		      (mapconcat (lambda (c) (concat "-l" c))
				 (oref this ldlibs) " "))
	    "")
	  "\n\n"))

(provide 'ede-pmake)

;;; ede-pmake.el ends here
