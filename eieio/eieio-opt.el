;;; eieio-opt.el -- eieio optional functions (debug, printing, speedbar)

;;; Copyright (C) 1996, 1998, 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-opt.el,v 1.9 1999/12/01 15:25:12 zappo Exp $
;; Keywords: OO, lisp
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
;;   This contains support functions to eieio.  These functions contain
;; some small class browser and class printing functions.
;;

(require 'eieio)

;;; Code:
(defun eieio-browse (&optional root-class)
  "Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'."
  (interactive (if current-prefix-arg
		   (list (read (completing-read "Class: "
						(eieio-build-class-alist)
						nil t)))
		 nil))
  (if (not root-class) (setq root-class 'eieio-default-superclass))
  (if (not (class-p root-class)) (signal 'wrong-type-argument (list 'class-p root-class)))
  (display-buffer (get-buffer-create "*EIEIO OBJECT BROWSE*") t)
  (save-excursion
    (set-buffer (get-buffer "*EIEIO OBJECT BROWSE*"))
    (erase-buffer)
    (goto-char 0)
    (eieio-browse-tree root-class "" "")
    ))

(defun eieio-browse-tree (this-root prefix ch-prefix)
  "Recursively, draws the children of the given class on the screen.
Argument THIS-ROOT is the local root of the tree.
Argument PREFIX is the character prefix to use.
Argument CH-PREFIX is another character prefix to display."
  (if (not (class-p (eval this-root))) (signal 'wrong-type-argument (list 'class-p this-root)))
  (let ((myname (symbol-name this-root))
	(chl (aref (class-v this-root) class-children))
	(fprefix (concat ch-prefix "  +--"))
	(mprefix (concat ch-prefix "  |  "))
	(lprefix (concat ch-prefix "     ")))
    (insert prefix myname "\n")
    (while (cdr chl)
      (eieio-browse-tree (car chl) fprefix mprefix)
      (setq chl (cdr chl)))
    (if chl
	(eieio-browse-tree (car chl) fprefix lprefix))
    ))

(defun eieio-describe-class (class)
  "Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect."
  (interactive (list (eieio-read-class "Class: ")))
  (with-output-to-temp-buffer "*Help*"
    (prin1 class)
    (terpri)
    (princ "Documentation:")
    (terpri)
    (princ (documentation-property class 'variable-documentation))
    (terpri)
    (terpri)
    (let ((methods (eieio-all-generic-functions class))
	  (doc nil))
      (if (not methods) nil
	(princ "Specialized Methods:")
	(terpri)
	(while methods
	  (setq doc (eieio-method-documentation (car methods) class))
	  (prin1 (car methods))
	  (if (not doc)
	      (princ "Undocumented")
	    (if (car doc)
		(progn
		  (terpri)
		  (princ ":BEFORE method:")
		  (terpri)
		  (princ (car doc))))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (terpri)
		  (princ ":PRIMARY method:")
		  (terpri)
		  (princ (car doc))))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (terpri)
		  (princ ":AFTER method:")
		  (terpri)
		  (princ (car doc))))
	    (terpri)
	    (terpri))
	  (setq methods (cdr methods)))))
    ))
    
(defalias 'describe-class 'eieio-describe-class)

(defun eieio-build-class-alist (&optional class buildlist)
  "Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
Optional argument BUILDLIST is more list to attach."
  (let* ((cc (or class eieio-default-superclass))
	 (sublst (aref (class-v cc) class-children)))
    (setq buildlist (cons (cons (symbol-name cc) 1) buildlist))
    (while sublst
      (setq buildlist (eieio-build-class-alist (car sublst) buildlist))
      (setq sublst (cdr sublst)))
    buildlist))

(defun eieio-read-class (prompt)
  "Return a class chosen by the user using PROMPT."
  (intern (completing-read prompt (eieio-build-class-alist) nil t)))

;;; Collect all the generic functions created so far, and do cool stuff.
;;
(defun eieio-all-generic-functions (&optional class)
  "Return a list of all generic functions.
Optional CLASS argument returns only those functions that contain methods for CLASS."
  (let ((l nil) tree (cn (if class (symbol-name class) nil)))
    (mapatoms
     (lambda (symbol)
       (setq tree (get symbol 'eieio-method-obarray))
       (if tree
	   (progn
	     ;; A symbol might be interned for that class in one of
	     ;; these three slots in the method-obarray.
	     (if (or (not class)
		     (intern-soft cn (aref tree 0))
		     (intern-soft cn (aref tree 1))
		     (intern-soft cn (aref tree 2)))
		 (setq l (cons symbol l)))))))
    l))

(defun eieio-method-documentation (generic class)
  "Return a list of the specific documentation of GENERIC for CLASS.
If there is not an explicit method for CLASS in GENERIC, or if that
function has no documentation, then return nil."
  (let ((tree (get generic 'eieio-method-obarray))
	(cn (symbol-name class))
	before primary after)
    (if (not tree)
	nil
      ;; A symbol might be interned for that class in one of
      ;; these three slots in the method-obarray.
      (setq before (intern-soft cn (aref tree 0))
	    primary (intern-soft cn (aref tree 1))
	    after (intern-soft cn (aref tree 2)))
      (if (not (or before primary after))
	  nil
	(list (if before (documentation before) nil)
	      (if primary (documentation primary) nil)
	      (if after (documentation after)))))))

;;; How about showing the hierarchy in speedbar?  Cool!
;;
(eval-when-compile
  (condition-case nil
      (require 'speedbar)
    (error nil)))

(defvar eieio-class-speedbar-key-map nil
  "Keymap used when working with a project in speedbar.")

(defun eieio-class-speedbar-make-map ()
  "Make a keymap for eieio under speedbar."
  (setq eieio-class-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing stuff
  (define-key eieio-class-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key eieio-class-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key eieio-class-speedbar-key-map "-" 'speedbar-contract-line)
  )

(if eieio-class-speedbar-key-map
    nil
  (if (not (featurep 'speedbar))
      (add-hook 'speedbar-load-hook (lambda ()
				      (eieio-class-speedbar-make-map)
				      (speedbar-add-expansion-list
				       '("EIEIO"
					 eieio-class-speedbar-menu
					 eieio-class-speedbar-key-map
					 eieio-class-speedbar))))
    (eieio-class-speedbar-make-map)
    (speedbar-add-expansion-list '("EIEIO"
				   eieio-class-speedbar-menu
				   eieio-class-speedbar-key-map
				   eieio-class-speedbar))))

(defvar eieio-class-speedbar-menu
  ()
  "Menu part in easymenu format used in speedbar while in `eieio' mode.")

(defun eieio-class-speedbar (dir-or-object depth)
  "Create buttons in speedbar that represents the current project.
DIR-OR-OBJECT is the object to expand, or nil, and DEPTH is the current
expansion depth."
  ;; This function is only called once, to start the whole deal.
  ;; Ceate, and expand the default object.
  (eieio-class-button eieio-default-superclass 0)
  (forward-line -1)
  (speedbar-expand-line))

(defun eieio-class-button (class depth)
  "Draw a speedbar button at the current point for CLASS at DEPTH."
  (if (not (class-p class))
      (signal 'wrong-type-argument (list 'class-p class)))
  (speedbar-make-tag-line 'angle ?+
			  'eieio-sb-expand
			  class
			  (symbol-name class)
			  'eieio-describe-class-sb
			  class
			  'speedbar-directory-face
			  depth))

(defun eieio-sb-expand (text class indent)
  "For button TEXT, expand CLASS at the current location.
Argument INDENT is the depth of indentation."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((subclasses (aref (class-v class) class-children)))
	       (while subclasses
		 (eieio-class-button (car subclasses) (1+ indent))
		 (setq subclasses (cdr subclasses)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun eieio-describe-class-sb (text token indent)
  "Describe the class TEXT in TOKEN.
INDENT is the current indentation level."
  (select-frame speedbar-attached-frame)
  (eieio-describe-class token))

(provide 'eieio-opt)

;;; eieio-opt.el ends here
