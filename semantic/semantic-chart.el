;;; semantic-util.el --- Utilities for use with semantic token streams

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: chart
;; X-RCS: $Id: semantic-chart.el,v 1.5 2001/11/17 15:44:56 zappo Exp $

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
;; A set of simple functions for charting details about a file based on
;; the output of the semantic parser.
;;

(require 'semantic)
(require 'chart)

;;; Code:

;;;###autoload
(defun semantic-chart-nonterminals-by-token (&optional buffer-or-stream)
  "Create a bar chart representing the number of nonterminals for a token.
Each bar represents how many toplevel nonterminal in BUFFER-OR-STREAM
exist with a given token type.  See `semantic-symbol->name-assoc-list'
for tokens which will be charted."
  (interactive)
  (let* ((stream (cond ((not buffer-or-stream)
			(semantic-bovinate-toplevel t))
		       ((bufferp buffer-or-stream)
			(save-excursion
			  (set-buffer buffer-or-stream)
			  (semantic-bovinate-toplevel t)))
		       (t buffer-or-stream)))
	 (names (mapcar 'cdr semantic-symbol->name-assoc-list))
	 (nums (mapcar
		(lambda (symname)
		  (length
		   (semantic-find-nonterminal-by-token
		    (car symname)
		    stream
		    t nil)))
		  semantic-symbol->name-assoc-list)))
    (chart-bar-quickie 'vertical
		       "Semantic Toplevel Token Volume"
		       names "Token Type"
		       nums "Volume")
    ))

;;;###autoload
(defun semantic-chart-database-size (&optional buffer-or-stream)
  "Create a bar chart representing the size of each file in semanticdb.
Each bar represents how many toplevel nonterminals in BUFFER-OR-STREAM
exist in each database entry."
  (interactive)
  (if (or (not (fboundp 'semanticdb-minor-mode-p))
	  (not (semanticdb-minor-mode-p)))
      (error "Semanticdb is not enabled"))
  (let* ((stream (cond ((not buffer-or-stream)
			(semantic-bovinate-toplevel t))
		       ((bufferp buffer-or-stream)
			(save-excursion
			  (set-buffer buffer-or-stream)
			  (semantic-bovinate-toplevel t)))
		       (t buffer-or-stream)))
	 (db semanticdb-current-database)
	 (names (mapcar 'car (object-assoc-list 'file (oref db tables))))
	 (numnuts (mapcar (lambda (a)
			    (prog1
				(cons (length (car a))
				      (car names))
			      (setq names (cdr names))))
			  (object-assoc-list 'tokens (oref db tables))))
	 (nums nil)
	 (fh (/ (- (frame-height) 7) 4)))
    (setq numnuts (sort numnuts (lambda (a b) (> (car a) (car b)))))
    (setq names (mapcar 'cdr numnuts)
	  nums (mapcar 'car numnuts))
    (if (> (length names) fh)
	(progn
	  (setcdr (nthcdr fh names) nil)
	  (setcdr (nthcdr fh nums) nil)))
    (chart-bar-quickie 'horizontal
		       "Semantic DB Toplevel Token Volume"
		       names "File"
		       nums "Volume")
    ))

(defun semantic-chart-token-complexity (tok)
  "Calculate the `complexity' of token TOK."
  (count-lines
   (semantic-token-end tok)
   (semantic-token-start tok)))

;;;###autoload
(defun semantic-chart-nonterminal-complexity-token
  (&optional symbol buffer-or-stream)
  "Create a bar chart representing the complexity of some tokens.
Complexity is calculated for tokens with a token of SYMBOL.  Each bar
represents the complexity of some nonterminal in BUFFER-OR-STREAM.
Only the most complex items are charted."
  (interactive)
  (let* ((sym (if (not symbol) 'function))
	 (stream
	  (semantic-find-nonterminal-by-token
	   sym
	   (cond ((not buffer-or-stream)
		  (semantic-bovinate-toplevel t))
		 ((bufferp buffer-or-stream)
		  (save-excursion
		    (set-buffer buffer-or-stream)
		    (semantic-bovinate-toplevel t)))
		 (t buffer-or-stream))
	   'positiononly nil))
	 (name (cond ((semantic-token-with-position-p (car stream))
		      (buffer-name (semantic-token-buffer (car stream))))
		     (t "")))
	 (cplx (mapcar (lambda (tok)
			 (cons tok (semantic-chart-token-complexity tok)))
		       stream))
	 (namelabel (cdr (assoc 'function semantic-symbol->name-assoc-list)))
	 (names nil)
	 (nums nil))
    (setq cplx (sort cplx (lambda (a b) (> (cdr a) (cdr b)))))
    (while (and cplx (<= (length names) (/ (- (frame-height) 7) 4)))
      (setq names (cons (semantic-token-name (car (car cplx)))
			names)
	    nums (cons (cdr (car cplx)) nums)
	    cplx (cdr cplx)))
;; ;;     (setq names (mapcar (lambda (str)
;; ;; 			  (substring str (- (length str) 10)))
;; ;; 			names))
    (chart-bar-quickie 'horizontal
		       (format "Function Complexity in %s" name)
		       names namelabel
		       nums "Complexity (Lines of code)")
    ))

(provide 'semantic-chart)

;;; semantic-chart.el ends here
