;;; semantic-ex.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: parse
;; X-RCS: $Id: semantic-bnf.el,v 1.7 2000/04/20 23:48:48 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Convert BNF definitions similar to bison into bovine tables.
;;
;; Major mode for BNF-for-emacs editing.
;;
;; See the semantic info file for details.

;;; History:
;; 

(require 'semantic)

;;; Code:
(defvar semantic-bovine-bnf-table
  ;; BNF's BNF
  ;;
  ;; rule : result punctuation ":" rule-list
  ;;      ;
  ;;
  ;; result : symbol
  ;;        ;
  ;;
  ;; rule-list : match-list lambda rule-or-list punctuation ";"
  ;;           ;
  ;;
  ;; rule-or-list : punctuation "|" match-list lambda rule-or-list
  ;;              | EMPTY
  ;;              ;
  ;;
  ;; match-list : symbol match-list
  ;;            | string match-list
  ;;            | symbol
  ;;            | string
  ;;            ;
  '((bovine-toplevel
     (symbol punctuation ":" rule-list punctuation ";"
	     (lambda (vals start end)
	       (list (nth 0 vals) 'rule nil (nth 2 vals) start end)
	       )))
    (rule-list
     (match-list lambda-fn rule-or-list
		 (lambda (vals start end)
		   (append (cons (cons (car (nth 1 vals)) (nth 0 vals))
				 (nth 2 vals))
			   (list start end))
		   )))
    (rule-or-list
     (punctuation "|" match-list lambda-fn rule-or-list
		  (lambda (vals start end)
		    (append (cons (cons (car (nth 2 vals)) (nth 1 vals))
				  (nth 3 vals))
			    (list start end))
		    ))
     ((lambda (vals start end) (list nil))
      ))
    (match-list
     (symbol match-list
	     (lambda (vals start end)
	       (append (cons (nth 0 vals) (nth 1 vals)) (list start end))))
     (string match-list
	     (lambda (vals start end)
	       (append (cons (nth 0 vals) (nth 1 vals)) (list start end))))
     (string)
     (symbol)
     )
    (lambda-fn
      (semantic-list
       (lambda (vals start end)
	 (list (buffer-substring-no-properties start end)
	       start end)))
      ((lambda (vals start end) (list "" start end))))
    )
  "Bovine table used to convert a BNF language file into a bovine table.")

(defun semantic-bnf-EXPAND (lst)
  "Insert a token expand function based on LST."
  (let ((argv (1- (string-to-int (substring (symbol-name (car (cdr lst)))
					    1)))))
    (insert "\n")
    (indent-for-tab-command)
    (insert "(semantic-bovinate-from-nonterminal "
	    "(car (nth " (int-to-string argv) " vals)) "
	    "(cdr (nth " (int-to-string argv) " vals)) "
	    "'" (symbol-name (car (cdr (cdr lst))))
	    ")\n")
    (indent-for-tab-command)))

(defun semantic-bnf-lambda-substitute (lst &optional inplace)
  "Insert LST substituting based on rules for the BNF converter.
LST is the list in which we are substituting.
Optional INPLACE indicates that the list is being expanded from elsewhere."
  (if (eq (car lst) 'quote)
      (progn
	(setq lst (cdr lst))
	(if (and (= (length lst) 1) (listp (car lst)))
	    (progn
	      (insert " (append")
	      (semantic-bnf-lambda-substitute (car lst) nil)
	      (insert ")")
	      (setq lst nil inplace nil))
	  (insert "(list")
	  (setq inplace t))
	)
    (if inplace (insert " (")))
  (if (eq (car lst) 'EXPAND)
      (semantic-bnf-EXPAND lst)
    (let ((inlist nil))
      (while lst
	(cond ((eq (car lst) nil)
	       (if (and (not inlist) (not inplace))
		   (progn (insert " (list")
			  (setq inlist t)))
	       (insert " nil"))
	      ((listp (car lst))
	       (let ((fn (and (symbolp (car (car lst))) (fboundp (car (car lst))))))
		 (if (and (not inlist) (not inplace))
		     (progn (insert " (list")
			    (setq inlist t)))
		 (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
		     (insert " (append"))
		 (semantic-bnf-lambda-substitute (car lst) (and fn (not (eq fn 'quote))))
		 (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
		     (insert  ")"))
		 ))
	      ((symbolp (car lst))
	       (let ((n (symbol-name (car lst))) ;the name
		     (x nil))		;expand flag
		 (if (eq (aref n 0) ?,)
		     (setq n (substring n 1)
			   x t))
		 (if (string= n "")
		     ;; We expand only the next item in place (a list?)
		     (progn
		       (setq lst (cdr lst))
		       ;; A regular inline-list...
		       (semantic-bnf-lambda-substitute (car lst) t))
		   (if (eq (aref n 0) ?$)
		       (let ((val (1- (string-to-int (substring n 1)))))
			 (if (and (not x) (not inlist) (not inplace))
			     (insert " (list")
			   (if (and x inlist (not inplace))
			       (progn (insert ")")
				      (setq inlist nil))))
			 (insert " (nth " (int-to-string val) " vals)")
			 (if (and (not x) (not inplace)) (setq inlist t)))
		     (if (and (not inlist) (not inplace))
			 (progn (insert " (list")
				(setq inlist t)))
		     (insert " " (if inplace "" "'") n)))))
	      (t
	       (if (and (not inlist) (not inplace))
		   (progn (insert " (list")
			  (setq inlist t)))
	       (insert (format " %S" (car lst)))))
	(setq lst (cdr lst)))
      (if inlist (insert ")"))))
  (if inplace (insert ")")))

(defun semantic-bnf-lambda-convert (semliststr vals)
  "Convert SEMLISTSTR into Lisp code based on VALS.
VALS are the matches in the BNF notation file."
  (if (string= "" semliststr)
      nil
    (let ((slsr (read semliststr)))
      ;; We converted the lambda string into a list.  Now write it
      ;; out as the bovine lambda expression, and do macro-like
      ;; conversion upon it.
      (insert "\n")
      (indent-for-tab-command)
      (insert ",(lambda (vals start end)\n")
      (indent-for-tab-command)
      (cond ((eq (car slsr) 'EXPAND)
	     (semantic-bnf-EXPAND slsr))
	    ((and (listp (car slsr))
		  (eq (car (car slsr)) 'EVAL))
	     ;; The user wants to evaluate the following args.
	     ;; Use a simpler expander
	     )
	    (t
	     (insert "(append ")
	     (semantic-bnf-lambda-substitute slsr)
	     ;; Finish it off
	     (insert "\n")
	     (indent-for-tab-command)
	     (insert "(list start end))")))
      (insert ")"))))

(defun semantic-bnf-to-bovine (file)
  "Insert the BNF file FILE into the current buffer as a bovine table."
  (interactive "FBNF file: ")
  (let* ((tokstream (save-excursion
		      (set-buffer (find-file-noselect file))
		      (semantic-clear-toplevel-cache)
		      (save-excursion
			(goto-char (point-min))
			(semantic-bovinate-toplevel 0 t))))
	 (tl (float (length tokstream))))
    (insert "`(")
    (indent-for-tab-command)
    (working-status-forms "Building bovine table" "done"
      (while tokstream
	;; Each element is a top level match, of the form:
	;; ( RESULT MATCH1 MATCH2 ... )
	;; where a match is of the form:
	;; ( LAMBDA-STRING TOKEN1 TOKEN2 ... )
	(let* ((rule (car tokstream))
	       (matches (car (cdr (cdr (cdr rule))))))
	  (insert "(" (car rule) "\n")
	  (indent-for-tab-command)
	  (while matches
	    (let* ((mla (car matches))
		   (lamb (car mla))
		   (ml (cdr mla)))
	      (insert "(")
	      (if (and (= (length ml) 1) (string= (car ml) "EMPTY"))
		  nil
		(while ml
		  (insert " " (car ml))
		  (setq ml (cdr ml))))
	      (semantic-bnf-lambda-convert lamb (car (cdr mla)))
	      (insert ")\n")
	      (indent-for-tab-command))
	    (setq matches (cdr matches)))
	  (insert ") ; end " (car rule) "\n")
	  (indent-for-tab-command))
	(setq tokstream (cdr tokstream))
	(working-status (* 100.0 (- 1.0 (/ (float (length tokstream)) tl)))))
      (working-status t))
    (insert ")\n")
    ))

(defun semantic-bnf-find-destination ()
  "Find the destination file for this BNF file."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 "^# TABLE: \\([-a-zA-Z0-9_-]+\\.el\\):\\([-a-zA-Z0-9_]+\\)$"
	 nil t)
	(save-excursion
	  (let ((f (match-string 1))
		(v (match-string 2)))
	    (set-buffer (find-file-noselect f))
	    (goto-char (point-min))
	    (if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
					   (regexp-quote v)) nil t)
		(progn
		  (goto-char (match-beginning 0))
		  (point-marker)))))
      nil)))

(defun semantic-bnf-find-mode ()
  "Find the mode this BNF is used in."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^# MODE: \\([-a-z]+\\)$" nil t)
	(save-excursion
	  (let ((m (match-string 1)))
	    (read m)))
      nil)))

(defun semantic-bnf-generate-and-load ()
  "Take the current BNF, auto-generate it into a table, and load it."
  (interactive)
  (if (not (eq major-mode 'semantic-bnf-mode))
      (error "Not valid outside the scope of a BNF file"))
  (let ((bb (current-buffer))
	(dest (semantic-bnf-find-destination))
	(mode (semantic-bnf-find-mode)))
    (if (not dest)
	(error "You must specify a destination table in your BNF file"))
    (save-excursion
      (set-buffer (marker-buffer dest))
      (goto-char dest)
      (re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
      (if (looking-at "\\s-*`(") (kill-sexp 1))
      (delete-blank-lines)
      (semantic-bnf-to-bovine (buffer-file-name bb))
      (eval-defun nil))
    (if mode
	(save-excursion
	  (let ((bufs (buffer-list)))
	    (while bufs
	      (set-buffer (car bufs))
	      (if (eq major-mode mode)
		  (funcall mode))
	      (setq bufs (cdr bufs))))))))

(defun semantic-test-bnf ()
  "Convert the current buffer (in BNF mode) into a list bovine table."
  (interactive)
  (let ((bb (current-buffer)))
    (switch-to-buffer "*BNF CONVERT*")
    (emacs-lisp-mode)
    (erase-buffer)
    (semantic-bnf-to-bovine (buffer-file-name bb))))

;;; Semantic BNF mode
;;
;; Major mode for editing BNF files.  More importantly, define a syntax
;; table so that the semantic do-whatsis will work correctly.
(defvar semantic-bnf-syntax-table nil
  "Syntax used in a BNF buffer.")

(if semantic-bnf-syntax-table
    nil
  (setq semantic-bnf-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?: "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?| "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\; "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\" "\"" semantic-bnf-syntax-table)
  (modify-syntax-entry ?- "_" semantic-bnf-syntax-table)
  (modify-syntax-entry ?( "(" semantic-bnf-syntax-table)
  (modify-syntax-entry ?) ")(" semantic-bnf-syntax-table)
  (modify-syntax-entry ?# "<" semantic-bnf-syntax-table)
  (modify-syntax-entry ?\n ">" semantic-bnf-syntax-table)
  'foo
  )

(defvar semantic-bnf-mode-hook nil
  "Hook run when starting BNF mode.")

(defvar semantic-bnf-mode-keywords
  '(("^\\(\\w+\\)\\s-*:" 1 font-lock-function-name-face)
    ("\\<\\(EMPTY\\|symbol\\|punctuation\\|string\\|semantic-list\
\\|\\(open\\|close\\)-paren\\|comment\\)\\>"
     1 font-lock-keyword-face)
    ("\\$[0-9]+" 0 font-lock-variable-name-face)
    )
  "Font Lock keywords used to highlight BNF buffer.")

(defvar semantic-bnf-map nil
  "Keymap used in `semantic-bnf-mode'.")

(if semantic-bnf-map
    nil
  (setq semantic-bnf-map (make-sparse-keymap))
  (define-key semantic-bnf-map "\t" 'semantic-bnf-indent)
  (define-key semantic-bnf-map "|" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map ";" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "#" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "\C-c\C-c" 'semantic-bnf-generate-and-load)
  )

(speedbar-add-supported-extension ".bnf")

(defun semantic-bnf-mode ()
  "Initialize a buffer for editing BNF code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-bnf-mode
	mode-name "BNF")
  (set-syntax-table semantic-bnf-syntax-table)
  (use-local-map semantic-bnf-map)
  (make-local-variable 'semantic-toplevel-bovine-table)
  (setq semantic-toplevel-bovine-table semantic-bovine-bnf-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'semantic-bnf-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((semantic-bnf-mode-keywords)
			     nil ; do not do string/comment highlighting
			     nil ; keywords are case insensitive.
			     ;; This puts _ & - as a word constituant,
			     ;; simplifying our keywords significantly
			     ((?_ . "w") (?- . "w"))))
  (run-hooks 'semantic-bnf-mode-hook))

(defun semantic-bnf-electric-punctuation ()
  "Insert and reindent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (semantic-bnf-indent))

(defun semantic-bnf-indent ()
  "Indent the current line according to BNF rules."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((indent (current-indentation)))
      (if (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
	  (delete-horizontal-space)
	(save-excursion
	  (forward-line -1)
	  (if (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
	      (setq indent (- (match-end 0) (point) 1))
	    (if (looking-at "\\s-*;")
		(setq indent 0)
	      (if (looking-at "\\s-*[|#]")
		  (setq indent (current-indentation))
		(setq indent (- (current-indentation) 2))))))
	(if (not (looking-at "\\s-*[|;#]"))
	    (setq indent (+ 2 indent)))
	(if (= (current-indentation) indent)
	    nil
	  (delete-horizontal-space)
	  (indent-to indent)))))
  (if (bolp) (if (looking-at "\\s-+") (end-of-line))))

(add-to-list 'auto-mode-alist '("\\.bnf$" . semantic-bnf-mode))
	     

(provide 'semantic-bnf)

;;; semantic-bnf.el ends here
