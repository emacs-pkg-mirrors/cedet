;;; semantic-ia-sb.el --- Speedbar analysis display interactor

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ia-sb.el,v 1.3 2002/03/16 15:29:09 zappo Exp $

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
  '( "---"
     [ "Expand" speedbar-expand-line nil ]
     [ "Contract" speedbar-contract-line nil ]
     [ "Jump to Tag" speedbar-edit-line t ]
     [ "Complete" speedbar-edit-line t ]
     )
  "Extra menu items Analysis mode.")

;; Make sure our special speedbar major mode is loaded
(speedbar-add-expansion-list '("Analyze"
			       semantic-ia-sb-easymenu-definition
			       semantic-ia-sb-key-map
			       semantic-ia-speedbar))

(speedbar-add-mode-functions-list
 (list "Analyze"
       ;;'(speedbar-item-info . eieio-speedbar-item-info)
       '(speedbar-line-path . semantic-ia-sb-line-path)))

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
	    (if (eq (car semantic-ia-sb-last-analysis) (point))
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
    (speedbar-insert-separator "Buffer")
    ;; Note to self: Turn this into an expandable file name.
    (speedbar-make-tag-line 'bracket ?  nil nil
			    (buffer-name buffer)
			    nil nil 'speedbar-file-face 0)
    (when analysis
      ;; Now insert information about the context
      ;;     (insert "Context:\n")
      ;;     (speedbar-insert-button (object-name-string analysis)
      ;; 			    'speedbar-tag-face
      ;; 			    nil nil nil nil)
      (when fnargs
	(speedbar-insert-separator "Arguments")
	(semantic-ia-sb-string-list fnargs
				    'speedbar-tag-face
				    'semantic-sb-token-jump))
      (let ((localvars (oref analysis localvariables)))
	(when localvars
	  (speedbar-insert-separator "Local Variables")
	  (semantic-ia-sb-string-list localvars
				      'speedbar-tag-face
				      ;; This is from semantic-sb
				      'semantic-sb-token-jump)))
      (let ((prefix (oref analysis prefix)))
	(when prefix
	  (speedbar-insert-separator "Prefix")
	  (semantic-ia-sb-string-list prefix
				      'speedbar-tag-face
				      'semantic-sb-token-jump))
	)
      (when completions
	(speedbar-insert-separator "Completions")
	(semantic-ia-sb-completion-list completions
					'speedbar-tag-face
					'semantic-ia-sb-complete))
      )))
		 
(defun semantic-ia-sb-string-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let* ((usefn nil)
	   (string (cond ((stringp (car list))
			  (car list))
			 ((semantic-token-p (car list))
			  (setq usefn (semantic-token-with-position-p (car list)))
			  (semantic-uml-concise-prototype-nonterminal (car list)))
			 (t "foo"))))
      (if (semantic-token-p (car list))
	  (speedbar-make-tag-line 'angle ?i
				  'semantic-ia-sb-token-info (car list)
				  string (if usefn function) (car list) face
				  0)
	(speedbar-make-tag-line 'statictag ??
				nil nil
				string (if usefn function) (car list) face
				0))
      (setq list (cdr list)))))
		 
(defun semantic-ia-sb-completion-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let* ((documentable nil)
	   (string (cond ((stringp (car list))
			  (car list))
			 ((semantic-token-p (car list))
			  (setq documentable t)
			  (semantic-uml-concise-prototype-nonterminal (car list)))
			(t "foo"))))
      (if documentable
	  (speedbar-make-tag-line 'angle ?i
				  'semantic-ia-sb-token-info
				  (car list)
				  string function (car list) face
				  0)
	(speedbar-make-tag-line 'statictag ?  nil nil
				string function (car list) face
				0))
      (setq list (cdr list)))))

(defun semantic-ia-sb-token-info (text token indent)
  "Display as much information as we can about token..
Show the information in a shrunk split-buffer and expand
out as many details as possible.
TEXT, TOKEN, and INDENT are speedbar function arguments."
  (unwind-protect
      (progn
	(speedbar-select-attached-frame)
	(with-output-to-temp-buffer "*Tag Information*"
	  ;; Output something about this token:
	  (save-excursion
	    (set-buffer "*Tag Information*")
	    (goto-char (point-max))
	    (insert
	     (semantic-prototype-nonterminal token nil t)
	     "\n")
	    (let ((type (semantic-token-type token))
		  (typetok nil))
	      (if type
		  (progn
		    (cond ((semantic-token-p type)
			   (setq type (semantic-token-name type)))
			  ((listp type)
			   (setq type (car type))))
		    (save-excursion
		      (set-buffer
		       (marker-buffer
			(car semantic-ia-sb-last-analysis)))
		      (setq typetok (semanticdb-find-nonterminal-by-name
				     type nil t nil nil t)))
		    (if typetok
			(insert
			 (semantic-prototype-nonterminal
			  (cdr (car typetok)) nil t))
		      (save-excursion
			(set-buffer
			 (marker-buffer
			  (car semantic-ia-sb-last-analysis)))
			(semantic-flex-keyword-p type)
			(setq typetok
			      (semantic-flex-keyword-get type 'summary)))
		      (if typetok
			  (insert typetok))
		      
		      )))
	      )
	    ))
	;; Make it small
	(shrink-window-if-larger-than-buffer
	 (get-buffer-window "*Tag Information*")))
    (select-frame speedbar-frame)))

(defun semantic-ia-sb-line-path (&optional depth)
  "Return the file name associated with DEPTH."
  (save-match-data
    (let* ((tok (speedbar-line-token))
	   (buff (if (semantic-token-buffer tok)
		     (semantic-token-buffer tok)
		   ;; Local variables are deoverlayed.  We should assume
		   ;; that they are in the buffer deriving the context.
		   (marker-buffer (car semantic-ia-sb-last-analysis)))))
      (buffer-file-name buff))))

(defun semantic-ia-sb-complete (text token indent)
  "At point in the attached buffer, complete the symbol clicked on.
TEXT TOKEN and INDENT are the details."
  ;; Find the specified bounds from the current analysis.
  (let* ((a (cdr semantic-ia-sb-last-analysis))
	 (pnt (car semantic-ia-sb-last-analysis))
	 (bounds (oref a bounds))
	 (movepoint nil)
	 )
    (save-excursion
      (set-buffer (marker-buffer pnt))
      (if (and (<= (point) (cdr bounds)) (>= (point) (car bounds)))
	  (setq movepoint t))
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds))
      (insert (semantic-token-name token))
      (if movepoint (setq movepoint (point)))
      ;; I'd like to use this to add fancy () or what not at the end
      ;; but we need the parent file whih requires an upgrade to the
      ;; analysis tool.
      ;;(senator-insert-foreign-token token ??))
      )
    (if movepoint
	(let ((cf (selected-frame)))
	  (speedbar-select-attached-frame)
	  (goto-char movepoint)
	  (select-frame cf)))))

(provide 'semantic-ia-sb)

;;; semantic-ia-sb.el ends here
