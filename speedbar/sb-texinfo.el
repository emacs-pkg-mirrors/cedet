;;; sb-texinfo.el --- provide hierarchical speedbar menu's for texinfo files

;; Copyright (c) 2000 Richard Y. Kim

;; Author: Richard Y. Kim, <ryk@ap.com>
;; Maintainer: Richard Y. Kim, <ryk@ap.com>
;; Created: Fri Jun 16 17:23:11 2000
;; Version: $Id: sb-texinfo.el,v 1.1 2000/06/30 13:01:03 zappo Exp $
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; The speedbar mode provides support for texinfo files by creating
;; tags using imenu.  This presentation do not reflect the natural
;; hierarchy of the texinfo document.
;;
;; This small add-on to speedbar provides an alternate way to
;; view the document which shows the natural hierarchy of the document.
;;
;; Installtation procedure:
;;   Not yet worked out.

;;; Change Log:
;;;
;;; 1.2 - Use newly created variable speedbar-texinfo-section-regexp rather
;;;       than texinfo-section-types-regexp, because the latter does not
;;;       have "top" in it.
;;;
;;;       Also added a hook to texinfo-mode-hook which sets
;;;       speedbar-tag-hierarchy-method to nil.
;;;
;;; 1.1 - first draft sent to Eric. on June 16, 2000.

;;; Code:

(require 'speedbar)

(add-to-list 'speedbar-dynamic-tags-function-list
	     '(speedbar-fetch-dynamic-texinfo . speedbar-insert-texinfo-list))

(defun speedbar-texinfo-mode-hook ()
  (setq speedbar-tag-hierarchy-method nil))

(add-hook 'texinfo-mode-hook 'speedbar-texinfo-mode-hook)

(defconst speedbar-texinfo-section-regexp
  "^@\\(top \\|chapter \\|sect\\|subs\\|subh\\|unnum\\|major\\|chapheading \\|heading \\|appendix\\)")

;; This returns t if the major mode of the current buffer is not 'texinfo-mode.
;; If it is 'texinfo-mode, then this returns a list where each element is
;; (LEVEL NAME . MARKER).
;; LEVEL is 0, 1, 2, or 3 corresponding to chapter section,
;; subsection, and subsubsection respectively.
;; NAME is the name of the node.
;; MARKER is emacs marker that points to the beginning of the node.
;; The elements in the the list returned are in ascending order of the MARKER.
;; This function along with it's parter, speedbar-insert-texinfo-list, are
;; designed to be added to the speedbar-dynamic-tags-function-list list.
(defun speedbar-fetch-dynamic-texinfo ( filename )
  (if (not (eq major-mode 'texinfo-mode))
      t
    (condition-case nil
	(save-excursion
	  (let ((heading-to-level
		 '(("top" . 0)
		   ("chapter" . 0) ("section" . 1)
		   ("subsection" . 2) ("subsubsection" . 3)
		   ("unnumbered" . 0) ("unnumberedsec" . 1)
		   ("unnumberedsubsec" . 2) ("unnumberedsubsubsec" . 3)
		   ("chapheading" . 0) ("heading" . 1)
		   ("subheading" . 2) ("subsubheading" . 3)
		   ("appendix" . 0) ("appendixsec" . 1)
		   ("appendixsubsec" . 2) ("appendixsubsubsec" . 3)
		   ("centerchap" . 0) ("majorheading" . 0)))
		pos-beg title level section alist)
	    ;; Create a list of all nodes
	    (goto-char (point-min))
	    (while (re-search-forward speedbar-texinfo-section-regexp nil t)
	      (goto-char (1+ (match-beginning 0))) ;right after @
	      (setq pos-beg (point-marker))
	      (setq section (symbol-name (read (current-buffer))))
	      (setq level (cdr (assoc section heading-to-level)))
	      (setq title (buffer-substring
			   (progn (skip-chars-forward " \t")
				  (point))
			   (progn (skip-chars-forward "^,\n")
				  (skip-chars-backward " \t")
				  (point))))
	      (setq alist (cons (cons level (cons title pos-beg)) alist)))
	    (nreverse alist)))
      (error t))))

;; See speedbar-fetch-dynamic-texinfo for description of LST.
;; LEVEL should be 0 unless this is called recursively.
;; This function converts LST from a flat list strcuture into
;; a hierarchical list suitable to be passed on to speedbar-insert-texinfo-list.
;; This function creates the hierarchical list recursively.
;; On first pass all the nodes of each chapter are grouped into a list.
;; Then each of these lists are recursively converted so that all nodes
;; of each section are grouped together.
(defun speedbar-format-texinfo-list ( lst level )
  (let (new-list elem section-list)
    (while lst
      (setq section-list (list (cdr (car lst))))
      (setq lst  (cdr lst))
      (while (and lst (> (car (car lst)) level))
	(setq section-list (cons (car lst) section-list))
	(setq lst  (cdr lst)))
      (setq new-list (cons (nreverse section-list) new-list)))
    (setq new-list (nreverse new-list))
    ;; Each member of new-list is a list of all chapters (on first pass), i.e.,
    ;; ( ((chap1 marker) (1 sec1 marker) ...)  ((chap2 marker) ...) ... )

    ;; Start recursion.
    (setq level (1+ level))
    (mapcar '(lambda (x)
	       (if (eq (length x) 1)
		   ;;(cons (car (car x)) x)
		   (car x)
		 (let ((head (car x)))
		   (cons (car head)
			 (cons head
			       (speedbar-format-texinfo-list (cdr x) level))))))
	    new-list)))

(defun speedbar-insert-texinfo-list (indent lst)
  ;; Set speedbar-tag-hierarchy-method to nil so that 
  ;; speedbar-create-tag-hierarchy won't mess up the list.
  (speedbar-insert-generic-list indent (speedbar-format-texinfo-list lst indent)
				'speedbar-tag-expand
				'speedbar-tag-find))

;;; sb-texinfo.el ends here

