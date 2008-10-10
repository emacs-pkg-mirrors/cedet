;;; semantic-ectag-parse.el --- exuberent CTags into Semantic tags

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-parse.el,v 1.1 2008/10/10 22:23:30 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Converting CTags output tables into Semantic Tag Tables.
;;
;; This works by scanning the output of a CTags output buffer,
;; and instead creating semantic tags for each line.
;;
;; Tags that appear as members, or otherwise appear to belong to
;; other tags in the list will be parented appropriately.

(require 'semantic-ectag-util)
(require 'semantic-ectag-lang)
;;; Code:

;; These variables need to be bound to values on a per-mode basis.
(defvar semantic-ectag-lang nil
  "The language name used by Exuberent CTags for the current buffer.")
(defvar semantic-ectag-lang-kind nil
  "The kinds of tags fetched by Exuberent CTags for the current language.")

;;;###autoload
(defun semantic-ectag-parse-buffer ()
  "Execute Exuberent CTags on this buffer.
Convert the output tags into Semantic tags."
  (interactive)
  (when (not semantic-ectag-lang)
    (error "Exuberent CTag support for Semantic not configured for %s"
	   major-mode))
  (let* ((start (current-time))
	 (buff (semantic-ectag-run
		"--excmd=number" ;; add line numbers
		"--fields=aKStsim" ;; Add extra info
		(format "--%s-kinds=%s"
			semantic-ectag-lang
			semantic-ectag-lang-kind)
		"-f" "-" ;; Send to standard out.
		(buffer-file-name) ;; The file to parse.
		))
	 (mode major-mode)
	 (end (current-time))
	 (parsed-output (save-excursion
			  (set-buffer buff)
			  (with-mode-local mode
			    ;; This enables all the semantic parsing while
			    ;; parsing the ctags file.
			    (semantic-ectag-parse-tags))))
	 )
    (when (interactive-p)
      (message "Parsed %d tags in %d seconds."
	       (length parsed-output)
	       (semantic-elapsed-time start end))
      (let ((ab (data-debug-new-buffer (concat "*"
					       (buffer-name)
					       " ADEBUG*")))
	    )
	(data-debug-insert-tag-list parsed-output "* ")))
    ))

(defun semantic-ectag-parse-tags ()
  "Parse the Exuberent CTags output in the current buffer."
  (goto-char (point-min))
  (let ((tags nil))
    (while (not (eobp))
      (let* ((line (buffer-substring (point) (point-at-eol)))
	     (elements (split-string line "\t"))
	     (ect-class (nth 3 elements))
	     (class (intern ect-class))
	     (prototype nil)
	     (type nil)
	     (class-sym (cond
			 ((member class '(function variable))
			  class)
			 ((eq class 'prototype)
			  (setq prototype t)
			  'function)
			 ((member class '(namespace class struct union enum typedef))
			  (setq type (symbol-name class))
			  'type)
			 ((eq class 'member)
			  'variable)
			 (t
			  (error "Unknown ctag output kind %s" class))))
	     (attr (semantic-ectag-split-fields (nthcdr 4 elements) class))
	     (line (string-to-number (nth 2 elements)))
	     (tag (apply 'semantic-tag (nth 0 elements)
			 class-sym
			 :filename (nth 1 elements)
			 :line line
			 :prototype prototype
			 :type (if (eq class-sym 'type) type nil) ;; Nil alows override later
			 attr
			 ))
	     )
	(push tag tags)
      
	(end-of-line)
	(condition-case nil (forward-char 1) (error nil))))
    (nreverse tags)))

(defun semantic-ectag-split-fields (fields class)
  "Convert FIELDS into a list of Semantic tag attributes.
CLASS is the class of the tag we are parsing these fields for."
  (let ((attr nil))
    (while fields
      (string-match "\\w+:" (car fields))
      (let* ((me (match-end 0))
	     (field (substring (car fields) 0 (1- me)))
	     (str (substring (car fields) me))
	     )
	(cond ((string= field "type")
	       (push str attr)
	       (push :type attr))
	      ((string= field "line")
	       (push (string-to-number str) attr)
	       (push :line attr))
	      ;; Class and Namespace seem to provide a name similar
	      ;; to our :parent tag, so both should do that.
	      ;; There is something extra here though..  It should
	      ;; be possible to use this info to do a reparenting operation.
	      ((string= field "class")
	       (push str attr)
	       (push :parent attr))
	      ((string= field "namespace")
	       (push str attr)
	       (push :parent attr))
	      ;((string= field "inheritance")
	       ;(push str attr)
	       ;(push :parent attr)
	       ;)
	      ((string= field "access")
	       (push str attr)
	       (push :protection attr))
	      ((string= field "signature")
	       (let ((sigattr (semantic-ectag-split-signature-summary str)))
		 (push sigattr attr)
		 (push :arguments attr)))
	      (t
	       (message "Unknown ectag field %s" field))))
      (setq fields (cdr fields)))
    attr))

(define-overloadable-function semantic-ectag-split-signature-summary (summary)
  "Split SUMMARY into Semantic tag compatible attributes.
SUMMARY is part of the output from Exuberent CTags that shows the
text from a file where the tag was found.")

(defun semantic-ectag-split-signature-summary-default (summary)
  "Default behavior for splitting a Exuberent CTags SUMMARY.
Assume comma separated list."
  (let ((s (split-string summary "[(),]" t))
	(args nil))
    (dolist (S s)
      (let ((argsplit (split-string S " ")))
	(if (= (length argsplit) 1)
	    (push (car argsplit) args)
	  (push (semantic-tag-new-variable
		 (car (last argsplit))
		 ;; The variable type is the earlier bits
		 (car (last argsplit 1)))
		args))))
    (nreverse args)))

(provide 'semantic-ectag-parse)
;;; semantic-ectag-parse.el ends here
