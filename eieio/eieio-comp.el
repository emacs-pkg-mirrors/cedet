;;; eieio-comp.el -- eieio routines to help with byte compilation

;;;
;;; Copyright (C) 1995,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: eieio-comp.el,v 1.1 1996/11/09 23:23:04 zappo Exp $
;;; Keywords: OO                                           
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;           
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Updates can be found at:
;;;    ftp://ftp.ultranet.com/pub/zappo

;;;
;;; Commentary:
;;;  
;;; Byte compiler functions for defmethod.  This will affect the new GNU
;;; byte compiler for emacs 19 and better.  This function will be called by
;;; the byte compiler whenever a `defmethod' is encountered in a file.
;;; It will output a function call to `defmethod-engine' with the byte
;;; compiled function as a parameter.

;; Some compatibility stuff
(if (not (fboundp 'byte-compile-compiled-obj-to-list))
    (defun byte-compile-compiled-obj-to-list (moose) nil))

(if (not (boundp 'byte-compile-outbuffer))
    (defvar byte-compile-outbuffer nil))

;; This teaches the byte compiler how to do this sort of thing.
(put 'defmethod 'byte-hunk-handler 'byte-compile-file-form-defmethod)

(defun byte-compile-file-form-defmethod (form)
  "Mumble about the thing we are compiling:  This function is mostly ripped
from byte-compile-file-form-defun, but it's been modified to handle the special
syntax of the defmethod command.  There should probably be one for defgeneric
as well, but that is called but rarely."
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
      (princ "\n(defmethod-engine '" my-outbuffer)
      (princ meth my-outbuffer)
      (princ " '(" my-outbuffer)
      (princ key my-outbuffer)
      (prin1 params my-outbuffer)
      (princ " " my-outbuffer)
      (eieio-byte-compile-princ-code code my-outbuffer)
      (princ "))" my-outbuffer)
      nil
      )))

(defun eieio-byte-compile-princ-code (code outbuffer)
  "Xemacs and GNU emacs do their things differently. Lets do it right
on both platforms"
  (if (eval-when-compile (not (string-match "XEmacs" emacs-version)))
      ;; FSF emacs
      (prin1 code outbuffer)
    ;; XEmacsb
    (if (atom code)
	(princ "#[" outbuffer)
      (princ "'(" outbuffer))
    (let ((codelist (if (byte-code-function-p code)
			(byte-compile-compiled-obj-to-list code)
		      (append code nil))))
      (while codelist
	(prin1 (car codelist) outbuffer)
	(princ " " outbuffer)
	(setq codelist (cdr codelist))
	))
    (if (atom code)
	(princ "]" outbuffer)
      (princ ")" outbuffer))))

(defun byte-compile-defmethod-param-convert (paramlist)
  "Convert method params into the params used by the defmethod thingy."
  (let ((argfix nil))
    (while paramlist
      (setq argfix (cons (if (listp (car paramlist)) 
			     (car (car paramlist))
			   (car paramlist))
			 argfix))
      (setq paramlist (cdr paramlist)))
    (nreverse argfix)))

;;; end of list
(provide 'eieio-comp)