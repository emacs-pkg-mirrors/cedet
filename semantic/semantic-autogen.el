;;; semantic-autogen.el --- Auto load statement generator

;;; Copyright (C) 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-autogen.el,v 1.4 2002/08/10 13:58:02 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; Automatically generate autoloads for Semantic.
;;
;; Future: Automatically generate parser initialization

;;; Code
;;

(if noninteractive
    (progn
      ;; if the user is doing this non-interactivly, we need to set up
      ;; these conveniences.
      (add-to-list 'load-path nil)
      (setq find-file-hooks nil
	    find-file-suppress-same-file-warnings t)
      ))

;; Load this in first
(require 'autoload)

(put 'define-overload 'doc-string-elt 3)

(defun semantic-hack-autoloads ()
  "Create semantic autoloads from sources."
  (interactive)
  (let* ((dir (file-name-directory (locate-library "semantic")))
	  (generated-autoload-file (concat dir "semantic-al.el"))
	  )
    (find-file (concat dir "semantic-al.el"))
    (erase-buffer)

    (insert ";;; semantic-al.el --- Auto-generated file filled with autoloads.")
    (update-autoloads-from-directories dir)
    )
  (newline 2))

;; Bum me out.  Taken from autoload.el and augmented.  Just look at these
;; hard coded lists.  BLECH!
(defun make-autoload (form file)
  "Turn FORM into an autoload or defvar for source file FILE.
Returns nil if FORM is not a special autoload form (i.e. a function definition
or macro definition or a defcustom)."
  (let ((car (car-safe form)) expand)
    (cond
     ;; For complex cases, try again on the macro-expansion.
     ((and (memq car '(easy-mmode-define-global-mode
		       easy-mmode-define-minor-mode define-minor-mode))
	   (setq expand (let ((load-file-name file)) (macroexpand form)))
	   (eq (car expand) 'progn)
	   (memq :autoload-end expand))
      (let ((end (memq :autoload-end expand)))
	;; Cut-off anything after the :autoload-end marker.
	(setcdr end nil)
	(cons 'progn
	      (mapcar (lambda (form) (make-autoload form file))
		      (cdr expand)))))

     ;; For special function-like operators, use the `autoload' function.
     ((memq car '(defun define-skeleton defmacro define-derived-mode
		   define-generic-mode easy-mmode-define-minor-mode
		   easy-mmode-define-global-mode
		   define-minor-mode defun* defmacro*
		   ;; Semantic Special:  Some functions
		   define-overload
		   ))
      (let* ((macrop (memq car '(defmacro defmacro*)))
	     (name (nth 1 form))
	     (body (nthcdr (get car 'doc-string-elt) form))
	     (doc (if (stringp (car body)) 
		      (prog1 (car body)
			(setq body (cdr body))))))
	;; `define-generic-mode' quotes the name, so take care of that
	(list 'autoload (if (listp name) name (list 'quote name)) file doc
	      (or (and (memq car '(define-skeleton define-derived-mode
				    define-generic-mode
				    easy-mmode-define-global-mode
				    easy-mmode-define-minor-mode
				    define-minor-mode)) t)
		  (eq (car-safe (car body)) 'interactive))
	      (if macrop (list 'quote 'macro) nil))))

     ;; So we can export a class, export it as a function.
     ;; That way when we "initialize" allocate one of this
     ;; class, it loads the file.
     ((eq car 'defclass)
      (let ((varname (car-safe (cdr-safe form)))
	    )
	(list 'autoload varname file nil nil nil)
	))

     ;; Convert defcustom to a simpler (and less space-consuming) defvar,
     ;; but add some extra stuff if it uses :require.
     ((eq car 'defcustom)
      (let ((varname (car-safe (cdr-safe form)))
	    (init (car-safe (cdr-safe (cdr-safe form))))
	    (doc (car-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    (rest (cdr-safe (cdr-safe (cdr-safe (cdr-safe form))))))
	(if (not (plist-get rest :require))
	    `(defvar ,varname ,init ,doc)
	  `(progn
	     (defvar ,varname ,init ,doc)
	     (custom-add-to-group ,(plist-get rest :group)
				  ',varname 'custom-variable)
	     (custom-add-load ',varname
			      ,(plist-get rest :require))))))

     ;; nil here indicates that this is not a special autoload form.
     (t nil))))

(provide 'semantic-autogen)

;;; semantic-alg.el ends here
