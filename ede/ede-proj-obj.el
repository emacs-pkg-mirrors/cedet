;;; ede-proj-obj.el --- EDE Generic Project Object code generation support

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-obj.el,v 1.3 1999/12/01 01:55:09 zappo Exp $

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
;; Handles a supperclass of target types which create object code in
;; and EDE Project file.

;;; Code:
(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  (;; Give this a new default
   (configuration-variables :initform ("debug" . (("CFLAGS" . "-g")
						  ("LDFLAGS" . "-g"))))
   (headers :initarg :headers
	    :initform nil
	    :type list
	    :custom (repeat (string :tag "Header"))
	    :documentation "Header files included in the distribution.
EDE generated Makefiles make all sources dependent on all header files.
In automake mode, these headers are ignored, but automake generates
dependencies automatically.")
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defmethod ede-want-file-p ((obj ede-proj-target-makefile-objectcode) file)
  "Return t if OBJ wants to own FILE."
  ;; Only C targets for now.  I'll figure out more later.
  (string-match "\\.\\(c\\|C\\|cc\\|cpp\\|CPP\\|h\\|hh\\|hpp\\)$"
		file))

(defmethod project-add-file ((this ede-proj-target-makefile-objectcode) file)
  "Add to target THIS the current buffer represented as FILE."
  (setq file (file-name-nondirectory file))
  ;; Header files go into auxiliary sources.  Add more for more languages.
  (if (not (string-match "\\.\\(h\\|hh\\)$" file))
      ;; No match, add to regular sources.
      (call-next-method)
    (if (not (member file (oref this headers)))
	(oset this headers (append (oref this headers) (list file))))
    (ede-proj-save (ede-current-project))))

(defmethod project-remove-file ((target ede-proj-target-makefile-objectcode)
				file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target headers (delete (file-name-nondirectory file)
			       (oref target headers)))
  ;; This will do sources, and save the project for us.
  (call-next-method))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-objectcode))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_SOURCE"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-objectcode))
  "Return a list of source files to convert to dependencies.
Argument THIS is the target to get sources from."
  (append (oref this source) (oref this auxsource)))

(defmethod ede-proj-makefile-insert-variables
  ((this ede-proj-target-makefile-objectcode))
  "Insert variables needed by target THIS."
  (call-next-method this (oref this headers))
  (let ((obj-ext
	 (if (and (obj-of-class-p this 'ede-proj-target-makefile-shared-object)
		  (oref this libtool))
	     ".lo" ".o")))
    (insert (ede-pmake-varname this) "_OBJ="
	    (mapconcat (lambda (a)
			 (concat (file-name-sans-extension a) obj-ext))
		       (oref this source) " ")
	    " "
	    (mapconcat (lambda (a)
			 (concat (file-name-sans-extension a) obj-ext))
		       (oref this auxsource) " ")
	    "\n")
    ))

(defmethod ede-proj-makefile-garbage-patterns
  ((this ede-proj-target-makefile-objectcode))
  "Return a list of patterns that are considred garbage to THIS.
These are removed with make clean."
  ;; This is constant.  Oh well.
  '("*.o" ".deps/*.P"))

(defmethod ede-proj-makefile-insert-rules
  ((this ede-proj-target-makefile-objectcode))
  "Insert rules needed by THIS target."
  (call-next-method)
  (insert (ede-name this) ": $(" (ede-pmake-varname this) "_OBJ)\n"
	  ;; Compile line
	  (if have-libtool
	      "\t$(LTLINK) "
	    "\t$(LINK) ")
	  ;; Shared flag if needed
	  (if (and
	       (obj-of-class-p this 'ede-proj-target-makefile-shared-object)
	       (not have-libtool))
	      "-shared "
	    "")
	  ;; Additional linker flags
	  (if (and (obj-of-class-p this 'ede-proj-target-makefile-program)
		   (oref this ldflags))
	      (concat (mapconcat (lambda (c) c) (oref this ldflags) " ") " ")
	    "")
	  ;; The objects to link
	  "$(" (ede-pmake-varname this) "_OBJ)"
	  ;; Separate this out later.
	  (if (obj-of-class-p this 'ede-proj-target-makefile-program)
	      ;; Some libaries
	      (concat " "
		      (mapconcat (lambda (c)
				   (if (= (aref c 0) ?$)
				       c
				     (concat "-l" c)))
				 (oref this ldlibs) " "))
	    "")
	  "\n\n"))

;;; Speedbar options:
;;
(defmethod eieio-speedbar-child-make-tag-lines
  ((this ede-proj-target-makefile-objectcode))
  "Expand an object code node in speedbar.
This is special for additional headers."
  (call-next-method)
  (with-slots (headers) this
    (mapcar (lambda (car)
	      (speedbar-make-tag-line 'bracket ?+
				      'ede-tag-file
				      (concat (oref this :path) car)
				      car
				      'ede-file-find
				      (concat (oref this :path) car)
				      'speedbar-file-face depth))
	    headers)))

(provide 'ede-proj-obj)

;;; ede-proj-obj.el ends here
