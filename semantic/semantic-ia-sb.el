;;; semantic-ia-sb.el --- Speedbar analysis display interactor

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ia-sb.el,v 1.1 2002/03/14 03:31:22 zappo Exp $

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
;; Speedbar node for displaying derived context information.
;;

(require 'semantic-analyze)
(require 'speedbar)

;;; Code:
(defvar semantic-ia-sb-key-map nil
  "Keymap used when in semantic analysis display mode.")

(if semantic-ia-sb-key-map
    nil
  (setq semantic-ia-sb-key-map (speedbar-make-specialized-keymap))

  ;; Basic featuers.
  (define-key semantic-ia-sb-key-map "\C-m" 'speedbar-edit-line)
  (define-key semantic-ia-sb-key-map "+" 'speedbar-expand-line)
  (define-key semantic-ia-sb-key-map "-" 'speedbar-contract-line)
  (define-key semantic-ia-sb-key-map "=" 'speedbar-contract-line)
  )

(defvar semantic-ia-sb-easymenu-definition
  ()
  "Extra menu items Analysis mode.")

;; Make sure our special speedbar major mode is loaded
(speedbar-add-expansion-list '("Analyze"
			       semantic-ia-sb-easymenu-definition
			       semantic-ia-sb-key-map
			       semantic-ia-speedbar))

(defun semantic-speedbar-analysis ()
  "Pull up speedbar in semantic analysis mode."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into RPM mode on speedbar.
  (speedbar-change-initial-expansion-list "Analyze")
  )

(defvar semantic-ia-sb-last-analysis nil
  "The last analysis object is stored here to prevent too much analysis.")

(defun semantic-ia-speedbar (directory zero)
  "Create buttons in speedbar which define the current analysis at POINT.
DIRECTORY is the current directory, which is ignored, and ZERO is 0."
  (let ((analysis nil)
	(buffer nil)
	(completions nil)
	(fnargs nil)
	(cf (selected-frame)))
    ;; Try and get some sort of analysis
    (condition-case nil
	(progn
	  (speedbar-select-attached-frame)
	  (setq buffer (current-buffer))
	  (save-excursion
	    (if (eq (car semantic-ai-sb-last-analysis) (point))
		(setq analysis (cdr semantic-ia-sb-last-analysis))
	      (setq analysis (semantic-analyze-current-context (point))))
	    (setq semantic-ia-sb-last-analysis (cons (point-marker) analysis))
	    (when analysis
	      (setq completions (semantic-analyze-possible-completions analysis))
	      (setq fnargs (semantic-get-local-arguments (point)))
	      )))
      (error nil))
    (select-frame cf)
    (set-buffer speedbar-buffer)
    ;; If we have something, do something spiff with it.
    (erase-buffer)
    (insert "Buffer:\n")
    ;; Note to self: Turn this into an expandable file name.
    (speedbar-insert-button (buffer-name buffer)
			    'speedbar-file-face
			    nil nil nil nil)
    (when analysis
      ;; Now insert information about the context
      ;;     (insert "Context:\n")
      ;;     (speedbar-insert-button (object-name-string analysis)
      ;; 			    'speedbar-tag-face
      ;; 			    nil nil nil nil)
      (when fnargs
	(insert "Arguments:\n")
	(semantic-ia-sb-string-list fnargs 'speedbar-tag-face nil))
      (let ((localvars (oref analysis localvariables)))
	(when localvars
	  (insert "Local Variables:\n")
	  (semantic-ia-sb-string-list localvars 'speedbar-tag-face nil)))
      (let ((prefix (oref analysis prefix)))
	(when prefix
	  (insert "Prefix:\n")
	  (semantic-ia-sb-string-list prefix 'speedbar-tag-face nil))
	)
      (when completions
	(insert "Completions:\n")
	(semantic-ia-sb-string-list completions 'speedbar-tag-face nil))
      )))

(defun semantic-ia-sb-string-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let ((string (cond ((stringp (car list))
			 (car list))
			((semantic-token-p (car list))
			 (semantic-uml-concise-prototype-nonterminal (car list)))
			(t "foo"))))
      (speedbar-make-tag-line 'statictag ?  nil nil
			      string function (car list) face
			      0)
      (setq list (cdr list)))))
		 





(provide 'semantic-ia-sb)

;;; semantic-ia-sb.el ends here
