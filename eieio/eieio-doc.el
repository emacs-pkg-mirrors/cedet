;;; eieiodoc - create texinfo documentation for an eieio class
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: eieio-doc.el,v 1.1 1996/11/17 23:58:48 zappo Exp $
;;; Keywords: OO, lisp, docs
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
;;;  Outputs into the current buffer documentation in texinfo format
;;;  for a class, all it's children, and all it's slots.

(defvar eieiodoc-currently-in-node nil
  "String representing the node we go BACK to")

(defvar eieiodoc-current-section-level nil
  "String represending what type of section header to use")

(defun eieiodoc-class (root-class indexstring &optional skiplist)
  "Create documentation starting with ROOT-CLASS.  The first job is to
create an indented menu of all the classes starting with `root-class'
and including all it's children.  Once this is done, @nodes are
created for all the subclasses.  Each node is then documented with a
description of the class, a brief inheritance tree (with xrefs) and a
list of all slots in a big table.  Where each slot is inherited from
is also documented.  In addition, each class is documented in the
index referenced by INDEXSTRING, a two letter code described in the
texinfo manual.

The optional third argument SKIPLIST is a list of object not to put
into any menus, nodes or lists."
  (interactive
   (list (intern-soft
	  (completing-read "Class: " (eieio-build-class-alist) nil t))
	 (read-string "Index name (2 chars): ")))
  (save-excursion
    (setq eieiodoc-currently-in-node
	  (if (re-search-backward "@node \\([^,]+\\)" nil t)
	      (buffer-substring (match-beginning 1) (match-end 1))
	    "Top")
	  eieiodoc-current-section-level
	  (if (re-search-forward "@\\(chapter\\|\\(sub\\)*section\\)"
				 (+ (point) 500) t)
	      (progn
		(goto-char (match-beginning 0))
		(cond ((looking-at "@chapter") "section")
		      ((looking-at "@section") "subsection")
		      ((looking-at "@\\(sub\\)+section") "subsubsection")
		      (t "subsubsection")))
	    "subsubsection")))
  (save-excursion
    (eieiodoc-main-menu root-class skiplist)
    (insert "\n")
    (eieiodoc-recurse root-class 'eieiodoc-one-node nil skiplist)))
  
(defun eieiodoc-main-menu (class skiplist)
  "Create a menu of all classes under CLASS indented the correct
amount.  SKIPLIST is a list of objects to skip"
  (end-of-line)
  (insert "\n@menu\n")
  (eieiodoc-recurse class (lambda (class level)
			(insert "* " (make-string level ? )
				(symbol-name class) " ::\n"))
		nil skiplist)
  (insert "@end menu\n"))

(defun eieiodoc-one-node (class level)
  "Create a node for CLASS, and for all subclasses of CLASS in order"
  (message "Building node for %s" class)
  (insert "\n@node " (symbol-name class) ", "
	  (if eieiodoc-next-class (symbol-name eieiodoc-next-class) " ") ", "
	  (if eieiodoc-prev-class (symbol-name eieiodoc-prev-class) " ") ", "
	  eieiodoc-currently-in-node "\n"
	  "@comment  node-name,  next,  previous,  up\n"
	  "@" eieiodoc-current-section-level " " (symbol-name class) "\n"
	  ;; indexstring is grabbed from parent calling function
	  "@" indexstring "index " (symbol-name class) "\n\n")
  ;; Now lets create a nifty little inheritance tree
  (let ((cl class)
	(revlist nil)
	(depth 0))
    (while cl
      (setq revlist (cons cl revlist)
	    cl (class-parent cl)))
    (insert "@table @asis\n@item Inheritance Tree:\n")
    (while revlist
      ;; root-class is dragged in from the top-level function
      (insert "@table @code\n@item "
	      (if (and (child-of-class-p (car revlist) root-class)
		       (not (eq class (car revlist))))
		  (concat "@w{@xref{" (symbol-name (car revlist)) "}}")
		(symbol-name (car revlist)))
	      "\n")
      (setq revlist (cdr revlist)
	    depth (1+ depth)))
    (let ((clist (reverse (aref (class-v rclass) class-children))))
      (if (not clist)
	  (insert "No children")
	(insert "@table @asis\n@item Children:\n")
	(while clist
	  (insert "@w{@xref{" (symbol-name (car clist)) "}}")
	  (if (cdr clist) (insert ", "))
	  (setq clist (cdr clist)))
	(insert "\n@end table\n")
	))
    (while (> depth 0)
      (insert "\n@end table\n")
      (setq depth (1- depth)))
    (insert "@end table\n\n  "))
  ;; Now lets build some documentation by extracting information from
  ;; the class description vector
  (let* ((cv (class-v class))
	 (docs (aref cv class-public-doc))
	 (names (aref cv class-public-a))
	 (deflt (aref cv class-public-d))
	 (pdocs (aref cv class-private-doc))
	 (pnames (aref cv class-private-a))
	 (pdeflt (aref cv class-private-d))
	 (set-one nil)
	 (anchor nil)
	 )
    ;; doc of the class itself
    (insert (eieiodoc-texify-docstring (aref cv 2) class) "\n\n@table @asis\n")
    (if names 
	(progn
	  (setq anchor (point))
	  (insert "@item Public Slots:\n\n@table @code\n")
	  (while names
	    (if (eieiodoc-one-attribute class (car names) (car docs))
		(setq set-one t))
	    (setq names (cdr names)
		  docs (cdr docs)
		  deflt (cdr deflt)))
	  (insert "@end table\n\n")
	  (if (not set-one) (delete-region (point) anchor))
	  ))
    (if pnames 
	(progn
	  (setq set-one nil
		anchor (point))
	  (insert "@item Private Slots:\n\n@table @code\n")
	  (while pnames
	    (if (eieiodoc-one-attribute class (car pnames) (car pdocs))
		(setq set-one t))
	    (setq pnames (cdr pnames)
		  pdocs (cdr pdocs)
		  pdeflt (cdr pdeflt)))
	  (insert "@end table\n\n")
	  (if (not set-one) (delete-region (point) anchor))
	  ))
    ;; This ends the table which only has public/private slots
    (insert "@end table\n")
    ))

(defun eieiodoc-one-attribute (class attribute doc)
  "Create documentation for a single attribute.  Assume this attribute
is inside a table, so it is initiated with the @item indicator.  If
this attribute is not inserted (because it is contained in the parent)
then return nil, else return t"
  (let ((pv (eieiodoc-parent-diff class attribute))
	(ia (eieio-attribute-to-initarg class attribute))
	(set-me nil))
    (if (or (eq pv t) (not ia))
	nil  ;; same in parent or no init arg
      (setq set-me t)
      (insert "@item " (symbol-name ia) "\nDefault Value: "
	      "@code{"(format "%S" (car deflt)) "}\n\n")
      (if (eq pv 'default)
	  ;; default differs only, xref the parent
	  (insert "@xref{" (symbol-name (class-parent class)) "}\n")
	(insert (if doc (eieiodoc-texify-docstring doc class) "Not Documented")
		"\n@refill\n\n")))
    set-me))
;;;
;;; Utilities
;;;
(defvar eieiodoc-prev-class nil
  "This has a value while eieiodoc-recurse is running, and can be
referenced from the recursed function.")

(defvar eieiodoc-next-class nil
  "This has a value while eieiodoc-recurse is running, and can be
referenced from the recursed function.")

(defun eieiodoc-recurse (rclass func &optional level skiplist)
  "Recurse down all children of RCLASS, calling FUNC on each one.
LEVEL indicates the current depth below the first call we are.  The
function FUNC will be called with RCLASS and LEVEL.  This will then
recursivly call itself once for each child class of RCLASS.  The
optional fourth argument SKIPLIST is a list of objects to ignore while
recursing."

  (if (not level) (setq level 0))

  ;; we reverse the children so they appear in the same order as it
  ;; does in the code that creates them.
  (let* ((children (reverse (aref (class-v rclass) class-children)))
	 (ocnc eieiodoc-next-class)
	 (eieiodoc-next-class (or (car children) ocnc))
	 (eieiodoc-prev-class eieiodoc-prev-class))

    (if (not (member rclass skiplist))
	(progn
	  (apply func (list rclass level))

	  (setq eieiodoc-prev-class rclass)))

    (while children
      (setq eieiodoc-next-class (or (car (cdr children)) ocnc))
      (setq eieiodoc-prev-class (eieiodoc-recurse (car children) func (1+ level)))
      (setq children (cdr children)))
    ;; return the previous class so that the prev/next node gets it right
    eieiodoc-prev-class))

(defun eieiodoc-parent-diff (class slot)
  "Return nil if the parent of CLASS does not have slot SLOT.  Return
t if it does, and return 'default if the default has changed."
  (let (df (err t) (scoped-class (class-parent class)))
    (condition-case nil
	(setq df (oref-default-engine (class-parent class) slot)
	      err nil)
      (invalid-slot-name (setq df nil))
      (error (setq df nil)))
    (if err 
	nil
      (if (equal df (oref-default-engine class slot))
	  t
	'default))))

(defun eieiodoc-texify-docstring (string class)
  "Take STRING, which is formatted to a normal doc string, and convert
it into a texinfo style string.  For instances where CLASS is the
class being referenced, do not Xref that class.

 `function' => @dfn{function}
 `variable' => @code{variable}
 `class'    => @code{class} @xref{class} <and prepend @##index class\n
 `unknown'  => @code{unknonwn}
 'quoteme   => @code{quoteme}
 non-nil    => non-@code{nil}
 t          => @code{t}
 :tag       => @code{:tag}
 [ stuff ]  => @code{[ stuff ]}
"
  (while (string-match "`\\([-a-zA-Z0-9]+\\)'" string)
    (let* ((vs (substring string (match-beginning 1) (match-end 1)))
	   (v (intern-soft vs)))
      (setq string
	    (concat
	     (replace-match (concat 
			     (if (and (not (class-p v))(fboundp v))
				 "@dfn{" "@code{")
			     vs "}"
			     (if (and (class-p v) (not (eq v class)))
				 (concat " @xref{" vs "}")))
			    nil t string)))))
  (while (string-match "\\( \\|^\\)\\(nil\\|t\\|'[-a-zA-Z0-9]+\\|:[-a-zA-Z0-9]+\\)\\([ ,]\\|$\\)" string)
    (setq string (replace-match "@code{\\2}" nil nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(non-\\)\\(nil\\)\\)\\([ ,]\\|$\\)" string)
    (setq string (replace-match "\\2@code{\\3}" nil nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\[[^]]+\\]\\)\\( \\|$\\)" string)
    (setq string (replace-match "@code{\\2}" nil nil string 2)))
  string)

;;; end of lisp
(provide 'eieio-doc)