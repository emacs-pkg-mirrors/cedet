;;; semantic-ctxt.el --- Context calculations for Semantic tools.

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ctxt.el,v 1.1 2001/01/31 16:45:49 zappo Exp $

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
;; Semantic, as a tool, provides a nice list of searchable tokens.
;; That information can provide some very accurate answers if the current
;; context of a position is known.
;;
;; This library provides the hooks needed for a language to specify how
;; the current context is calculated.
;;


;;; Code:
(defun semantic-up-context (&optional point)
  "Move point up one context from POINT.
Return non-nil if there are no more context levels.
Overloaded functions take no parameters."
  (if point (goto-char (point)))
  (let ((s (semantic-fetch-overload 'up-context)))
    (if s (funcall s)
      (semantic-up-context-default)
      )))

(defun semantic-up-context-default ()
  "Move the point up and out one context level.
Works with languages that use parenthetical grouping."
  ;; By default, assume that the language uses some form of parenthetical
  ;; do dads for their context.
  (condition-case nil
      (progn
	(up-list -1)
	nil)
    (error t)))

(defun semantic-beginning-of-context (&optional point)
  "Move POINT to the beginning of the current context.
Return non-nil if there is no upper context."
  (if point (goto-char (point)))
  (let ((s (semantic-fetch-overload 'beginning-of-context)))
    (if s (funcall s)
      (semantic-beginning-of-context-default)
      )))

(defun semantic-beginning-of-context-default ()
  "Move point to the beginning of the current context via parenthisis.
Return non-nil if there is no upper context."
  (if (semantic-up-context)
      t
    (forward-char 1)
    nil))

(defun semantic-end-of-context (&optional point)
  "Move POINT to the end of the current context.
Return non-nil if there is no upper context."
  (if point (goto-char (point)))
  (let ((s (semantic-fetch-overload 'end-of-context)))
    (if s (funcall s)
      (semantic-end-of-context-default)
      )))

(defun semantic-end-of-context-default ()
  "Move point to the end of the current context via parenthisis.
Return non-nil if there is no upper context."
  (if (semantic-up-context)
      t
    ;; Go over the list, and back over the end parenthisis.
    (forward-sexp 1)
    (forward-char -1)
    nil))

(defun semantic-get-local-variables (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic token format."
  (save-excursion
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'get-local-variables)))
      (if s (funcall s)
	(semantic-get-local-variables-default)
	))))

(defun semantic-get-local-variables-default ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `bovine-inner-scope'
to collect tokens, such as local variables or prototypes."
  (working-status-forms "Local" "done"
    (let ((semantic-bovination-working-type nil))
      (semantic-bovinate-region-until-error
       (point) (save-excursion (semantic-end-of-context) (point))
       'bovine-inner-scope))))

(defun semantic-get-all-local-variables (&optional point)
  "Get all local variables for this context, and parent contexts.
Local variables are returned in Semantic token format."
  (save-excursion
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'get-all-local-variables)))
      (if s (funcall s)
	(semantic-get-all-local-variables-default)
	))))

(defun semantic-get-all-local-variables-default ()
  "Get all local variables for this context, and parent contexts.
Local variables are returned in Semantic token format.
Uses `semantic-beginning-of-context', `semantic-end-of-context',
`semantic-up-context', and `semantic-get-local-variables' to collect
this information."
  (let ((varlist nil)
	(sublst nil))
    (save-excursion
      (while (not (semantic-beginning-of-context))
	;; Get the local variables
	(setq sublist (semantic-get-local-variables))
	(if sublist
	    (setq varlist (cons sublist varlist)))
	;; Move out of this context to the next.
	(semantic-up-context)))
    ;; arguments to some local function
    (setq sublist (semantic-get-local-arguments))
    (if sublist (setq varlist (cons sublist varlist)))
    ;; fix er up.
    (nreverse varlist)))

(defun semantic-get-local-arguments (&optional point)
  "Get arguments (variables) from the current context at POINT.
Parameters are available if the point is in a function or method.
This function returns a list of tokens.  If the local token returns
just a list of strings, then this function will convert them to tokens."
  (if point (goto-char (point)))
  (let* ((s (semantic-fetch-overload 'get-local-arguments))
	 (params (if s (funcall s)
		   (semantic-get-local-arguments-default)))
	 (rparams nil))
    ;; convert unsafe params to the right thing.
    (while params
      (setq rparams
	    (cons (cond ((semantic-token-p (car params))
			 (car params))
			((stringp (car params))
			 (list (car params) 'variable))
			(t (error "Unknown parameter element")))
		  rparams)
	    params (cdr params)))
    (nreverse rparams)))

(defun semantic-get-local-arguments-default ()
  "Get arguments (variables) from the current context.
Parameters are available if the point is in a function or method."
  (let ((tok (semantic-current-nonterminal)))
    (if (and tok (eq (semantic-token-token tok) 'function))
	(semantic-token-function-args tok))))

(provide 'semantic-ctxt)

;;; semantic-ctxt.el ends here
