;;; eieio-comp.el -- eieio routines to help with byte compilation

;;;
;; Copyright (C) 1995,1996, 1998, 1999, 2000 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-comp.el,v 1.5 2000/07/16 21:18:19 zappo Exp $
;; Keywords: oop, lisp, tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;  
;; Byte compiler functions for defmethod.  This will affect the new GNU
;; byte compiler for Emacs 19 and better.  This function will be called by
;; the byte compiler whenever a `defmethod' is encountered in a file.
;; It will output a function call to `eieio-defmethod' with the byte
;; compiled function as a parameter.

;; Some compatibility stuff
;;; Code:
(if (not (fboundp 'byte-compile-compiled-obj-to-list))
    (defun byte-compile-compiled-obj-to-list (moose) nil))

(if (not (boundp 'byte-compile-outbuffer))
    (defvar byte-compile-outbuffer nil))

;; This teaches the byte compiler how to do this sort of thing.
(put 'defmethod 'byte-hunk-handler 'byte-compile-file-form-defmethod)

(defun byte-compile-file-form-defmethod (form)
  "Mumble about the method we are compiling.
This function is mostly ripped from `byte-compile-file-form-defun', but
it's been modified to handle the special syntax of the defmethod
command.  There should probably be one for defgeneric as well, but
that is called but rarely.  Argument FORM is the body of the method."
  (setq form (cdr form))
  (let* ((meth (car form))
	 (key (progn (setq form (cdr form))
		     (cond ((eq ':BEFORE (car form))
			    (setq form (cdr form))
			    ":BEFORE ")
			   ((eq ':AFTER (car form))
			    (setq form (cdr form))
			    ":AFTER ")
			   (t ""))))
	 (params (car form))
	 (lamparams (byte-compile-defmethod-param-convert params))
	 (class (car (cdr (car params))))
	 (my-outbuffer (if (eval-when-compile
			     (string-match "XEmacs" emacs-version))
			   byte-compile-outbuffer outbuffer))
	 )
    (let ((name (format "%s::%s" (or class "#<generic>") meth)))
      (if byte-compile-verbose
	  ;; #### filename used free
	  (message "Compiling %s... (%s)" (or filename "") name))
      (setq byte-compile-current-form name) ; for warnings
      )
    ;; Flush any pending output
    (byte-compile-flush-pending)
    ;; Byte compile the body.  For the byte compiled forms, add the
    ;; rest arguments, which will get ignored by the engine which will
    ;; add them later (I hope)
    (let* ((new-one (byte-compile-lambda
		     (append (list 'lambda lamparams)
			     (cdr form))))
	   (code (byte-compile-byte-code-maker new-one)))
      (princ "\n(eieio-defmethod '" my-outbuffer)
      (princ meth my-outbuffer)
      (princ " '(" my-outbuffer)
      (princ key my-outbuffer)
      (prin1 params my-outbuffer)
      (princ " " my-outbuffer)
      (eieio-byte-compile-princ-code code my-outbuffer)
      (princ "))" my-outbuffer)
      nil
      )
    (unless (fboundp meth)
      ;; Now add this function to the list of known functions by defining
      ;; it in Emacs proper.  This is the wrong way to do it.
      ;; Technically, I should be using `byte-compile-function-environment'
      ;; and updating stuff there, but it has some strange behaviors I
      ;; don't like or understand, and various newsgroups don't seem to
      ;; unserstand it either.
      (eieio-defgeneric meth (cdr form))
      ;; Remove it from the undefined list if it is there.
      (let ((elt (assq meth byte-compile-unresolved-functions)))
	(if elt
	    (setq byte-compile-unresolved-functions
		  (delq elt byte-compile-unresolved-functions))))
      )))


(defun eieio-byte-compile-princ-code (code outbuffer)
  "Xemacs and GNU Emacs do their things differently.
Lets do it right on both platforms
Argument CODE is the code to output.
Argument OUTBUFFER is the buffer to dump the created code to."
  (if (eval-when-compile (not (string-match "XEmacs" emacs-version)))
      ;; FSF emacs
      (prin1 code outbuffer)
    ;; XEmacs
    (if (atom code)
	(princ "#[" outbuffer)
      (princ "'(" outbuffer))
    (let ((codelist (if (byte-code-function-p code)
			(byte-compile-compiled-obj-to-list code)
		      (append code nil))))
      (while codelist
	(eieio-prin1 (car codelist) outbuffer)
	(princ " " outbuffer)
	(setq codelist (cdr codelist))
	))
    (if (atom code)
	(princ "]" outbuffer)
      (princ ")" outbuffer))))

(defun eieio-prin1 (code outbuffer)
  "For XEmacs only, princ one item.
Recurse into lists in search of `byte-code' which needs expanding...
Argument CODE is the code to output.
Argument OUTBUFFER is the buffer to dump the created code to."
  (cond ((byte-code-function-p code)
	 (let ((codelist (byte-compile-compiled-obj-to-list code)))
	   (princ "#[" outbuffer)
	   (while codelist
	     (eieio-prin1 (car codelist) outbuffer)
	     (princ " " outbuffer)
	     (setq codelist (cdr codelist))
	     )
	   (princ "]" outbuffer)))
	((vectorp code)
	 (let ((i 0) (ln (length code)))
	   (princ "[" outbuffer)
	   (while (< i ln)
	     (eieio-prin1 (aref code i) outbuffer)
	     (princ " " outbuffer)
	     (setq i (1+ i)))
	   (princ "]" outbuffer)))
	(t (prin1 code outbuffer))))
    

(defun byte-compile-defmethod-param-convert (paramlist)
  "Convert method params into the params used by the defmethod thingy.
Argument PARAMLIST is the paramter list to convert."
  (let ((argfix nil))
    (while paramlist
      (setq argfix (cons (if (listp (car paramlist))
			     (car (car paramlist))
			   (car paramlist))
			 argfix))
      (setq paramlist (cdr paramlist)))
    (nreverse argfix)))

(provide 'eieio-comp)

;;; eieio-comp.el ends here
