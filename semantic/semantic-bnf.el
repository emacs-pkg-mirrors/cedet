;;; semantic-bnf.el --- Semantic details for some languages

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2
;; Keywords: parse
;; X-RCS: $Id: semantic-bnf.el,v 1.27 2001/01/24 21:11:48 zappo Exp $

;; Semantic-bnf is free software; you can redistribute it and/or modify
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
;; Convert BNF definitions similar to bison into bovine tables.
;;
;; Major mode for BNF-for-emacs editing.
;;
;; See the semantic info file for details.

;;; History:
;; 

(require 'semantic)

;;; Code:
(defvar semantic-setup-code-delimiters '("^\\s-*;; Code generated from" .
					 "^\\s-*;; End code generated from")
  "Delimiter comments in a setup function where code is added from a bnf file.")

(defvar semantic-bovine-bnf-table
  ;; BNF's BNF
  ;;
  ;; decl : punctuation "%" semantic-list punctuation "%"
  ;;      ;
  ;;
  ;; Here, the first symbol is a special token meaning something to
  ;; the generator.
  ;; percenttoken : punctuation "%" symbol symbol
  ;;              ;
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
  `((bovine-toplevel
     (symbol punctuation ":" rule-list punctuation ";"
	     ,(semantic-lambda
	       (list (nth 0 vals) 'rule nil (nth 2 vals))))
     (punctuation "%" percent-thing
		  ,(semantic-lambda
		    (nth 1 vals))))
    (percent-thing
     (semantic-list punctuation "%"
		    ;; When loading lisp rules, use READ to convert
		    ;; into a list we can pretty print later.
		    ,(semantic-lambda
		      (let ((r (buffer-substring
				(car (car vals))
				(cdr (car vals)))))
			(list (symbol-name (car (read r))) 'setting r))))
;     (symbol "token" symbol symbol
;	     ,(semantic-lambda
;	       (list (nth 1 vals) 'token (nth 2 vals))))
     (symbol "start" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'start)))
     (symbol "token" symbol string
	     ,(semantic-lambda
	       (list (nth 1 vals) 'keyword "symbol" (nth 2 vals))))
     (symbol "token" symbol symbol string
	     ,(semantic-lambda
	       (list (nth 1 vals) 'token
		     (nth 2 vals)  (nth 3 vals))))
     (symbol "put" symbol symbol symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'put
		     (nth 2 vals)
		     (nth 3 vals))))
     (symbol "put" symbol symbol string
	     ,(semantic-lambda
	       (list (nth 1 vals) 'put
		     (nth 2 vals)
		     (nth 3 vals))))
     (symbol "put" symbol symbol semantic-list
	     ,(semantic-lambda
	       (list (nth 1 vals) 'put
		     (nth 2 vals)
		     (semantic-flex-text (cons 1 (nth 3 vals))))))
     (symbol "outputfile" symbol punctuation "." symbol "\\bel\\b"
	     ,(semantic-lambda
	       (list (concat (nth 1 vals) ".el") 'outputfile)))
     (symbol "parsetable" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'parsetable)))
     (symbol "keywordtable" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'keywordtable)))
     (symbol "languagemode" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'languagemode)))
     (symbol "setupfunction" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'setupfunction)))
     (symbol "quotemode" symbol
	     ,(semantic-lambda
	       (list (nth 1 vals) 'quotemode)))
     )
    (rule-list
     (match-list lambda-fn rule-or-list
		 ,(semantic-lambda
		   (cons (cons (car (nth 1 vals)) (nth 0 vals))
			 (nth 2 vals)))))
    (rule-or-list
     (punctuation "|" match-list lambda-fn rule-or-list
		  ,(semantic-lambda
		    (cons (cons (car (nth 2 vals)) (nth 1 vals))
			  (nth 3 vals))))
     (,(semantic-lambda nil)))
    (match-list
     (symbol match-list
	     ,(semantic-lambda
	       (cons (nth 0 vals) (nth 1 vals))))
     (string match-list
	     ,(semantic-lambda
	       (cons (nth 0 vals) (nth 1 vals))))
     (string)
     (symbol)
     )
    (lambda-fn
     (semantic-list
      ,(semantic-lambda
	(list (buffer-substring-no-properties start end))))
     (,(semantic-lambda (list "" ))))
    )
"Bovine table used to convert a BNF language file into a bovine table.")


;;; Conversion routines
;;
(defun semantic-bnf-EXPAND (lst)
  "Insert a token expand function based on LST."
  (let ((argv (1- (string-to-int (substring (symbol-name (car (cdr lst)))
					    1)))))
    (insert "\n ")
    (insert "(semantic-bovinate-from-nonterminal "
	    "(car (nth " (int-to-string argv) " vals)) "
	    "(cdr (nth " (int-to-string argv) " vals)) "
	    "'" (symbol-name (car (cdr (cdr lst))))
	    ")\n ")))

(defun semantic-bnf-EXPANDFULL (lst)
  "Insert a token full expand function based on LST."
  (let ((argv (1- (string-to-int (substring (symbol-name (car (cdr lst)))
					    1)))))
    (insert "\n ")
    (insert "(semantic-bovinate-from-nonterminal-full "
	    "(car (nth " (int-to-string argv) " vals)) "
	    "(cdr (nth " (int-to-string argv) " vals)) "
	    "'" (symbol-name (car (cdr (cdr lst))))
	    ")\n ")))

(defun semantic-bnf-ASSOC (lst quotemode)
  "Handle an ASSOC list based on LST.
QUOTEMODE is the current mode of quotation."
  (setq lst (cdr lst))
  (insert "\n ")
  (insert "(semantic-bovinate-make-assoc-list ")
  (while lst
    ;; The key
    (insert "'" (symbol-name (car lst)))
    (setq lst (cdr lst))
    ;; the value
    (if (listp (car lst))
	(semantic-bnf-lambda-substitute (car lst) quotemode t)
      (insert (format "%S" (car lst))))
    ;; next
    (setq lst (cdr lst)))
  (insert ")\n"))

(defun semantic-bnf-lambda-substitute (lst quotemode &optional inplace)
  "Insert LST substituting based on rules for the BNF converter.
LST is the list in which we are substituting.
Argument QUOTEMODE is non-nil if we are in backquote mode.
Optional INPLACE indicates that the list is being expanded from elsewhere."
  (if (eq (car lst) 'quote)
      (progn
	(setq lst (cdr lst))
	(if (and (= (length lst) 1) (listp (car lst)))
	    (progn
	      (insert " (append")
	      (semantic-bnf-lambda-substitute (car lst) quotemode nil)
	      (insert ")")
	      (setq lst nil inplace nil))
	  (if (and (= (length lst) 1) (symbolp (car lst)))
	      (progn
		(insert "'" (symbol-name (car lst)))
		(setq lst nil inplace nil))
	    (insert "(list")
	    (setq inplace t))
	  )))
  (cond ((eq (car lst) 'EXPAND)
	 (semantic-bnf-EXPAND lst))
	((eq (car lst) 'EXPANDFULL)
	 (semantic-bnf-EXPANDFULL lst))
	((eq (car lst) 'ASSOC)
	 (semantic-bnf-ASSOC lst quotemode))
	(t
	 (if inplace (insert " ("))
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
;		      (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
;			  (insert " (append"))
		      (semantic-bnf-lambda-substitute (car lst) quotemode t);(and fn (not (eq fn 'quote))))
;		      (if (and inplace (not fn) (not (eq (car (car lst)) 'EXPAND)))
;			  (insert  ")"))
		      ))
		   ((symbolp (car lst))
		    (let ((n (symbol-name (car lst))) ;the name
			  (q quotemode)	;implied quote flag
			  (x nil))	;expand flag
		      (if (eq (aref n 0) ?,)
			  (if quotemode
			      ;; backquote mode needs the @
			      (if (eq (aref n 1) ?@)
				  (setq n (substring n 2)
					q nil
					x t)
				;; non backquote mode behaves normally.
				(setq n (substring n 1)
				      q nil))
			    (setq n (substring n 1)
				  x t)))
		      (if (string= n "")
			  ;; We expand only the next item in place (a list?)
			  (progn
			    (setq lst (cdr lst))
			    ;; A regular inline-list...
			    (semantic-bnf-lambda-substitute (car lst) quotemode t))
			(if (and (eq (aref n 0) ?$)
				 ;; Don't expand $ tokens in implied quote
				 ;; mode.  This acts like quoting in other
				 ;; symbols.
				 (not q))
			    (let ((val (1- (string-to-int (substring n 1)))))
			      (if (and (not x) (not inlist) (not inplace))
				  (insert " (list")
				(if (and x inlist (not inplace))
				    (progn (insert ")")
					   (setq inlist nil))))
			      (insert " (nth " (int-to-string val) " vals)")
			      (if (and (not x) (not inplace)) (setq inlist t)))
			  (if (and (not inlist) (not inplace) )
			      (progn (insert " (list")
				     (setq inlist t)))
			  (insert " "
				  (if (or inplace (eq (car lst) t)) "" "'")
				  n; " "
				  )))))
		   (t
		    (if (and (not inlist) (not inplace))
			(progn (insert " (list")
			       (setq inlist t)))
		    (insert (format " %S" (car lst)))))
	     (setq lst (cdr lst)))
	   (if inlist (insert ")")))
	   (if inplace (insert ")"))))
  )

(defun semantic-bnf-lambda-convert (semliststr vals quotemode)
  "Convert SEMLISTSTR into Lisp code based on VALS.
VALS are the matches in the BNF notation file.
QUOTEMODE is the mode in which quoted symbols are slurred."
  (if (string= "" semliststr)
      nil
    (let ((slsr (read semliststr)))
      ;; We converted the lambda string into a list.  Now write it
      ;; out as the bovine lambda expression, and do macro-like
      ;; conversion upon it.
      (insert "\n ")
      (cond ((eq (car slsr) 'EXPAND)
	     (insert ",(lambda (vals start end)\n ")
	     (semantic-bnf-EXPAND slsr)
	     )
	    ((and (listp (car slsr))
		  (eq (car (car slsr)) 'EVAL))
	     ;; The user wants to evaluate the following args.
	     ;; Use a simpler expander
	     )
	    (t
	     (insert " ,(semantic-lambda\n ")
	     (semantic-bnf-lambda-substitute slsr quotemode)
	     ))
      (insert ")"))))

(defun semantic-bnf-to-bovine (tokstream &optional start)
  "Insert the BNF TOKSTREAM into the current buffer as a bovine table.
Optional argument START is the token to start with."
  (interactive "FBNF file: ")
  (let ((tl (float (length tokstream)))
	(tokens (semantic-find-nonterminal-by-token 'token tokstream))
	(quotemode (if (semantic-find-nonterminal-by-token 'quotemode tokstream)
		       t nil)))
    (insert "`(")
    (working-status-forms "Building bovine table" "done"
      (while tokstream
	;; Each element is a top level match, of the form:
	;; ( RESULT MATCH1 MATCH2 ... )
	;; where a match is of the form:
	;; ( LAMBDA-STRING TOKEN1 TOKEN2 ... )
	(let* ((rule (car tokstream))
	       (matches (car (cdr (cdr (cdr rule))))))
	  (when (eq (car (cdr rule)) 'rule)
	    (insert "(")
	    (if (and start (string= start (car rule)))
		(insert "bovine-toplevel")
	      (insert (car rule)))
	    (insert "\n ")
	    (while matches
	      (let* ((mla (car matches))
		     (lamb (car mla))
		     (ml (cdr mla)))
		(insert "(")
		(if (and (= (length ml) 1) (string= (car ml) "EMPTY"))
		    nil
		  (while ml
		    (let ((a (assoc (car ml) tokens)))
		      (if a
			  (insert " " (nth 2 a) " "
				  (format
				   "%S"
				   (concat "\\b"
					   (regexp-quote (read (nth 3 a)))
					   "\\b")))
			(insert " " (car ml))))
		    (setq ml (cdr ml))))
		(semantic-bnf-lambda-convert lamb (car (cdr mla)) quotemode)
		(insert ")\n "))
	      (setq matches (cdr matches)))
	    (insert ") ; end " (car rule) "\n ")))
	(setq tokstream (cdr tokstream))
	(working-status (* 100.0 (- 1.0 (/ (float (length tokstream)) tl)))))
      (working-status t))
    (insert ")\n ")
    ))

;;; Output File hacks
;;
(defun semantic-bnf-find-table-destination-old ()
  "Find the destination file for this BNF file via comments."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 "^#\\s-*TABLE:\\s-*\\([-a-zA-Z0-9_-]+\\.el\\):\\([-a-zA-Z0-9_]+\\)$"
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

(defun semantic-bnf-find-table-destination (tokstream)
  "Find the destination file for this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
parse table variable."
  (save-excursion
    (let ((file (semantic-find-nonterminal-by-token 'outputfile tokstream))
	  (var (semantic-find-nonterminal-by-token 'parsetable tokstream)))
      (if (or (not file) (not var))
	  (semantic-bnf-find-table-destination-old)
	;; Fix file/var to strings
	(setq file (semantic-token-name (car file))
	      var (semantic-token-name (car var)))
	;; Look these items up.
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
				       (regexp-quote var)) nil t)
	    (progn
	      (goto-char (match-beginning 0))
	      (point-marker))
	  (error "You must add a declaration for %s in %s"
		 var file))))))

(defun semantic-bnf-find-keyword-destination (tokstream)
  "Find the destination file for keywords in this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
keyword table variable."
  (save-excursion
    (let ((file (semantic-find-nonterminal-by-token 'outputfile tokstream))
	  (var (semantic-find-nonterminal-by-token 'keywordtable tokstream)))
      (if (or (not file) (not var))
	  nil
	;; Fix file/var to strings
	(setq file (semantic-token-name (car file))
	      var (semantic-token-name (car var)))
	;; Look these items up.
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(if (re-search-forward (concat "def\\(var\\|const\\)\\s-+"
				       (regexp-quote var)) nil t)
	    (progn
	      (goto-char (match-beginning 0))
	      (point-marker))
	  (error "You must add a declaration for %s in %s"
		 var file))))))

(defun semantic-bnf-find-languagemode-old ()
  "Find the mode this BNF is used in."
  (error "Upgrade")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\-*MODE:\\s-*\\([-a-z]+\\)$" nil t)
	(save-excursion
	  (let ((m (match-string 1)))
	    (read m)))
      nil)))

(defun semantic-bnf-find-languagemode (tokstream)
  "Find the lanugage mode for this BNF file.
Argument TOKSTREAM is the list of tokens in which to find the file and
parse table variable."
  (let ((mode (semantic-find-nonterminal-by-token 'languagemode tokstream)))
    (if mode
	(intern (semantic-token-name (car mode)))
      (semantic-bnf-find-languagemode-old))))

(defun semantic-bnf-find-setup-code (tokstream sourcefile)
  "Find the setup code based on TOKSTREAM.
Return a marker where the code is to be inserted.
SOURCEFILE is the file name from whence tokstream came."
  (let ((setfn (semantic-find-nonterminal-by-token 'setupfunction tokstream)))
    (if (not setfn)
	nil
      ;; The setup function
      (goto-char (point-min))
      (if (not (re-search-forward (concat "(defun\\s-+"
					  (semantic-token-name (car setfn))
					  "\\s-+\\(()\\|nil\\)")
				  nil t))
	  (error "Setup function %s not found in %s"
		 (semantic-token-name (car setfn)) (buffer-file-name))
	;; Scan for setup text, and remove old stuff, insert new.
	(let ((b (match-beginning 0))
	      (e (save-excursion (end-of-defun) (point))))
	  (if (re-search-forward (car semantic-setup-code-delimiters)
				 nil t)
	      ;; Search and destroy
	      (let ((mb (progn (goto-char (match-end 0))
			       (end-of-line)
			       (point)))
		    (me (progn (re-search-forward
				(cdr semantic-setup-code-delimiters) e t)
			       (beginning-of-line)
			       (point))))
		(delete-region (1+ mb) (1- me))
		(goto-char (1+ mb))
		t)
	    ;; Add a new on in at the end
	    (goto-char e)
	    (down-list -1)		; hop into the end
	    ;; Insert delimiters, move cursor
	    (let ((m (string-match ";"
				   (car semantic-setup-code-delimiters))))
	      (insert (substring (car semantic-setup-code-delimiters) m))
	      (insert " " sourcefile "\n  ")
	      (save-excursion;; save in the middle
		(insert "\n " (substring (cdr semantic-setup-code-delimiters)
					m))
		(insert " " sourcefile "\n "))
	      t)
	    ))))))

(defvar semantic-bnf-indent-table t
  "Non nil means to indent the large table during creation.")

(defun semantic-bnf-generate-and-load-no-indent ()
  "Call `semantic-bnf-genrate-and-load' without indenting the table."
  (interactive)
  (let ((semantic-bnf-indent-table nil))
    (semantic-bnf-generate-and-load)))
  
(defun semantic-bnf-generate-and-load ()
  "Take the current BNF, auto-generate it into a table, and load it."
  (interactive)
  (if (not (eq major-mode 'semantic-bnf-mode))
      (error "Not valid outside the scope of a BNF file"))
  ;; Do the work
  (semantic-clear-toplevel-cache)
  (let* ((fname (file-name-nondirectory (buffer-file-name)))
	 (tok (semantic-bovinate-toplevel t))
	 (bb (current-buffer))
	 (dest (semantic-bnf-find-table-destination tok))
	 (keydest (semantic-bnf-find-keyword-destination tok))
	 (mode (semantic-bnf-find-languagemode tok))
	 (start (semantic-find-nonterminal-by-token 'start tok))
	 )
    (if (not dest)
	(error "You must specify a destination table in your BNF file"))
    (save-excursion
      (set-buffer (marker-buffer dest))
      ;; Keyword table
      (when keydest
	(goto-char keydest)
	(re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
	(if (looking-at "\\s-*\\(nil\\|(semantic-flex-make-keyword-table\\)")
	    (delete-region (point) (save-excursion (forward-sexp 1) (point))))
	(delete-blank-lines)
	(let ((key (semantic-find-nonterminal-by-token 'keyword tok))
	      (put (semantic-find-nonterminal-by-token 'put tok))
	      (start (point)))
	  (if (not key)
	      (insert "nil\n ")
	    (insert "(semantic-flex-make-keyword-table \n `(")
	    ;; Get all the keys
	    (while key
	      (insert " (" (nth 3 (car key)) " . " (car (car key)) ")\n ")
	      (setq key (cdr key)))
	    (insert ")\n  '(\n ")
	    ;; Now get all properties
	    (while put
	      (setq key (semantic-find-nonterminal-by-token 'keyword tok))
	      (let ((a (assoc (nth 0 (car put)) key)))
		(if (not a) (error "Token %s not found" (nth 0 (car put))))
		(insert "  ("
			(nth 3 a) " "
			(nth 2 (car put)) " "
			(nth 3 (car put)) ")\n "))
	      (setq put (cdr put)))
	    (insert "))\n "))
	  (save-excursion
	  (indent-region start (point) nil)))
	(eval-defun nil))
      ;; Insert setup code in the startup function or hook
      (when (semantic-bnf-find-setup-code tok fname)
	;; Point should now be in the region to add stuff
	;; Add in the bovine table to be used
	(indent-region
	 (point)
	 (let ((var (semantic-find-nonterminal-by-token 'parsetable tok))
	       (key (semantic-find-nonterminal-by-token 'keywordtable tok)))
	   (when var
	     ;; The bovine table
	     (insert "(setq semantic-toplevel-bovine-table "
		     (semantic-token-name (car var)) ")\n "))
	   ;; Keytable setup
	   (when key
	     (insert "(setq semantic-flex-keywords-obarray "
		     (semantic-token-name (car key)) ")\n "))
	   ;; Add in user specified settings
	   (let ((settings (semantic-find-nonterminal-by-token 'setting tok)))
	     (while settings
	       (insert (nth 2 (car settings)))
	       (insert "\n ")
	       (setq settings (cdr settings))))
	   (point))
	 nil)
	(eval-defun nil))
      ;; The table
      (goto-char dest)
      (re-search-forward "def\\(var\\|const\\)\\s-+\\(\\w\\|\\s_\\)+\\s-*\n")
      (if (looking-at "\\s-*\\(`?(\\|nil\\)")
	  (delete-region (point) (save-excursion (forward-sexp 1) (point))))
      (delete-blank-lines)
      (semantic-bnf-to-bovine tok (if start (semantic-token-name (car start))))
      (if semantic-bnf-indent-table
	  (save-excursion
	    (message "Indenting table....")
	    (indent-region (progn (re-search-backward "(defvar")
				  (goto-char (match-beginning 0))
				  (point))
			   (progn (forward-sexp 1) (point))
			   nil)))
      (eval-defun nil))
    (message "Done.")
    (if mode
	(save-excursion
	  (let ((bufs (buffer-list)))
	    (while bufs
	      (set-buffer (car bufs))
	      (if (eq major-mode mode)
		  (funcall mode))
	      (setq bufs (cdr bufs))))))))

(defun semantic-bnf-generate-one-rule ()
  "Generate code for one rule in a temporary buffer."
  (interactive)
  (semantic-bovinate-toplevel t)
  (let ((r (semantic-current-nonterminal)))
    (if (or (not r) (not (eq (semantic-token-token r) 'rule)))
	(error "No rule to expand nearby"))
    (pop-to-buffer "*Rule Expansion*" t)
    (save-excursion
      (set-buffer "*Rule Expansion*")
      (erase-buffer)
      (insert "Expanding rule [" (semantic-token-name r) "]\n\n")
      (semantic-bnf-to-bovine (list r)))))

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
  (modify-syntax-entry ?% "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\; "." semantic-bnf-syntax-table)
  (modify-syntax-entry ?\" "\"" semantic-bnf-syntax-table)
  (modify-syntax-entry ?- "_" semantic-bnf-syntax-table)
  (modify-syntax-entry ?# "<" semantic-bnf-syntax-table)
  (modify-syntax-entry ?\n ">" semantic-bnf-syntax-table)
  'foo
  )

(defvar semantic-bnf-mode-hook nil
  "Hook run when starting BNF mode.")

(defvar semantic-bnf-mode-keywords
  '((";\\s-*[^#\n ].*$" 0 font-lock-comment-face)
    ("^\\(\\w+\\)\\s-*:" 1 font-lock-function-name-face)
    ("\\<\\(EMPTY\\|symbol\\|punctuation\\|string\\|semantic-list\
\\|\\(open\\|close\\)-paren\\|comment\\)\\>"
     1 font-lock-keyword-face)
    ("\\$[0-9]+" 0 font-lock-variable-name-face)
    ("%" 0 font-lock-reference-face)
    ("%\\(\\w+\\)" 1 font-lock-type-face)
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
  (define-key semantic-bnf-map "%" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "(" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map ")" 'semantic-bnf-electric-punctuation)
  (define-key semantic-bnf-map "\C-c\C-c" 'semantic-bnf-generate-and-load-no-indent)
  (define-key semantic-bnf-map "\C-cc" 'semantic-bnf-generate-and-load)
  (define-key semantic-bnf-map "\C-cr" 'semantic-bnf-generate-one-rule)
  )

(speedbar-add-supported-extension ".bnf")

(defalias 'bnf-mode 'semantic-bnf-mode)
(defun semantic-bnf-mode ()
  "Initialize a buffer for editing BNF code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-bnf-mode
	mode-name "BNF")
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "# *")
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
  (setq semantic-symbol->name-assoc-list
	'( (keyword . "Keywords")
	   (token . "Tokens")
	   (rule  . "Rules")
	   )
	imenu-create-index-function 'semantic-create-imenu-index)
  (run-hooks 'semantic-bnf-mode-hook))

(defun semantic-bnf-electric-punctuation ()
  "Insert and reindent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (semantic-bnf-indent))

(defun semantic-bnf-in-settings-p (&optional point)
  "Non-nil if POINT is in a settings block."
  (condition-case nil
      (save-excursion
	(if point (goto-char point))
	(up-list -1)
	(while (not (eq (preceding-char) ?%))
	  (up-list -1))
	t)
    (error nil)))

(defun semantic-bnf-in-lambda-continuation-p (&optional point)
  "Non-nil if POINT is in a settings block."
  (condition-case nil
      (save-excursion
	(if point (goto-char point) (setq point (point)))
	(beginning-of-line)
	(condition-case nil
	    (while t
	      (up-list -1))
	  (error nil))
	(end-of-line)
	(<= (point) point))
    (error nil)))

(defun semantic-bnf-previous-colon-indentation ()
  "Calculation the indentation of the last colon oporator.
Returns the previous colon's column."
  (save-excursion
    (let ((p (point))
	  (ci (progn
		(if (re-search-backward "^\\s-*\\(\\w\\|\\s_\\)+\\s-*:" nil t)
		    (progn
		      (beginning-of-line)
		      (- (match-end 0) 1 (point)))
		  0)))
	  (cp (point))
	  (sc nil))
      (goto-char p)
      (while (and (re-search-backward "^\\s-*;\\s-*$" nil t)
		  (semantic-bnf-in-lambda-continuation-p))
	(setq sc t))
      (if sc
	  (if (< (point) cp)
	      ci
	    0)
	ci))))

(defun semantic-bnf-do-lisp-indent (&optional point)
  "Run the stander Emacs Lisp indenter on a line of code.
Optional argument POINT is the position on the line to indent."
  (condition-case nil
      (save-excursion
	(if point (goto-char point) (setq point (point)))
	(up-list -1)
	(condition-case nil
	    (while t
	      (up-list -1))
	  (error nil))
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region (point) point)
	  (goto-char point)
	  (with-syntax-table emacs-lisp-mode-syntax-table
	    (lisp-indent-line))))
    (error nil)))

(defun semantic-bnf-indent ()
  "Indent the current line according to BNF rules."
  (interactive)
  (if (semantic-bnf-in-settings-p)
      (semantic-bnf-do-lisp-indent)
    (if (semantic-bnf-in-lambda-continuation-p)
	(semantic-bnf-do-lisp-indent)
      (save-excursion
	(beginning-of-line)
	(let ((indent (semantic-bnf-previous-colon-indentation)))
	  (cond
	   ((or (looking-at "\\s-*\\(\\w\\|\\s_\\)+\\s-*:")
		(looking-at "\\s-*%"))
	    (delete-horizontal-space))
	   (t
	    (save-excursion
	      (if (and (not (looking-at "\\s-*[|;#]"))
		       (/= indent 0))
		  (setq indent (+ 2 indent))))
	    (if (= (current-indentation) indent)
		nil
	      (delete-horizontal-space)
	      (indent-to indent))))))))
  (if (bolp) (if (looking-at "\\s-+") (end-of-line))))

(add-to-list 'auto-mode-alist '("\\.bnf$" . semantic-bnf-mode))

(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'semantic-bnf-mode))

(provide 'semantic-bnf)

;;; semantic-bnf.el ends here
