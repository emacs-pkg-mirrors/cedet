;;; eieio-opt.el -- eieio optional functions (debug, printing, speedbar)

;;; Copyright (C) 1996, 1998 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-opt.el,v 1.4 1998/12/16 13:54:58 zappo Exp $
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

(defun eieio-thing-to-string (thing)
  "Convert THING into a string.
If THING is an object, use `object-print' instead, if THING is a class,
then use `class-name' instead, if THING is a list of stuff, try those."
  (if (object-p thing) (object-print thing)
    (if (class-p thing) (class-name thing)
      (if (and thing (listp thing))
	  (let ((op "("))
	    (while thing
	      (setq op (concat op " " (eieio-thing-to-string (car thing))))
	      (setq thing (cdr thing)))
	    (concat op ")"))
	(format "%S" thing))))
  )

(defun eieio-describe-class (class)
  "Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect."
  (interactive (list
		(completing-read "Class: " (eieio-build-class-alist) nil t)))
  (switch-to-buffer (get-buffer-create "*EIEIO OBJECT DESCRIBE*"))
  (erase-buffer)
  (let* ((cv (cond ((stringp class) (if (string= class "")
					(class-v eieio-default-superclass)
				      (class-v (read class))))
		   ((symbolp class) (class-v class))
		   ((object-p class) (class-v (object-class-fast class)))
		   (t (error "Can't find class info from parameter"))))
	 (this (if (object-p class) class this))
	 (scoped-class (if (object-p class) (object-class-fast class) scoped-class))
	 (priva (aref cv class-private-a))
	 (publa (aref cv class-public-a))
	 (privd (aref cv class-private-d))
	 (publd (aref cv class-public-d)))
    (insert "Description of")
    (if (object-p class)
	(insert " object `" (aref class 2) "'"))
    (insert " class `" (symbol-name (aref cv 1)) "'\n")
    (if (not priva)
	nil
      (insert "\nPRIVATE\n")
      (put-text-property (point)
			 (progn (insert "Field:\t\t\tdefault value"
					(if (object-p class)
					    "\t\tCurrent Value" ""))
				(point))
			 'face 'underline)
      (insert "\n")
      (while priva
	(let ((dvs (eieio-thing-to-string (car privd))))
	  (insert (symbol-name (car priva)) "\t"
		  (if (< (length (symbol-name (car priva))) 8) "\t" "")
		  (if (< (length (symbol-name (car priva))) 16) "\t" "")
		  dvs
		  (if (object-p class)
		      (concat
		       "\t"
		       (if (< (length dvs) 8) "\t" "")
		       (if (< (length dvs) 16) "\t" "")
		       (eieio-thing-to-string (oref-engine class (car priva))))
		    "")
		  "\n"))
	(setq priva (cdr priva)
	      privd (cdr privd))))
    (insert "\nPUBLIC\n")
    (put-text-property (point)
		       (progn (insert "Field:\t\t\tdefault value"
				      (if (object-p class)
					  "\t\tCurrent Value" ""))
			      (point))
		       'face 'underline)
    (insert "\n")
    (while publa
      (let ((dvs (eieio-thing-to-string (car publd))))
	(insert (symbol-name (car publa)) "\t"
		(if (< (length (symbol-name (car publa))) 8) "\t" "")
		(if (< (length (symbol-name (car publa))) 16) "\t" "")
		dvs
		(if (object-p class)
		    (concat
		     "\t"
		     (if (< (length dvs) 8) "\t" "")
		     (if (< (length dvs) 16) "\t" "")
		     (eieio-thing-to-string (oref-engine class (car publa))))
		  "")
		"\n"))
      (setq publa (cdr publa)
	    publd (cdr publd)))))

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

;;; How about showing the hierarchy in speedbar?  Cool!
;;
(eval-when-compile
  (condition-case nil
      (require 'speedbar)
    (error nil)))

(defvar eieio-speedbar-key-map nil
  "Keymap used when working with a project in speedbar.")

(defun eieio-speedbar-make-map ()
  "Make a keymap for eieio under speedbar."
  (setq eieio-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing stuff
  (define-key eieio-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key eieio-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key eieio-speedbar-key-map "-" 'speedbar-contract-line)
  )

(if eieio-speedbar-key-map
    nil
  (if (not (featurep 'speedbar))
      (add-hook 'speedbar-load-hook (lambda ()
				      (eieio-speedbar-make-map)
				      (speedbar-add-expansion-list
				       '("EIEIO"
					 eieio-speedbar-menu
					 eieio-speedbar-key-map
					 eieio-speedbar))))
    (eieio-speedbar-make-map)
    (speedbar-add-expansion-list '("EIEIO"
				   eieio-speedbar-menu
				   eieio-speedbar-key-map
				   eieio-speedbar))))

(defvar eieio-speedbar-menu
  ()
  "Menu part in easymenu format used in speedbar while in `eieio' mode.")

(defun eieio-speedbar (dir-or-object depth)
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
