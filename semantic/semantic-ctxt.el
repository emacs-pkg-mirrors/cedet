;;; semantic-ctxt.el --- Context calculations for Semantic tools.

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ctxt.el,v 1.2 2001/02/03 03:13:44 zappo Exp $

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
;;
(defvar semantic-command-separation-character
 ";"
  "String which indicates the end of a command.
Used for identifying the end of a single command.")

(defvar semantic-function-argument-separation-character
 ","
  "String which indicates the end of a command.
Used for identifying the end of a single command.")

;;; Local variable parsing.
;;
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

;;; Local context parsing
;;
;; Context parsing assumes a series of language independent commonalities.
;; These terms are used to describe those contexts:
;;
;; command      - One command in the language.
;; symbol       - The symbol the cursor is on.
;;                This would include a series of type/field when applicable.
;; assignment   - The variable currently being assigned to
;; function     - The function call the cursor is on/in
;; argument     - The index to the argument the cursor is on.
;;
;;
(defun semantic-end-of-command ()
  "Move to the end of the current command."
    (let ((s (semantic-fetch-overload 'end-of-command)))
      (if s (funcall s)
	(semantic-end-of-command-default)
	)))

(defun semantic-end-of-command-default ()
  "Move to the beginning of the current command.
Depends on `semantic-command-separation-character' to find the
beginning and end of a command."
  (let ((nt (semantic-current-nonterminal)))
    (if (re-search-forward (regexp-quote semantic-command-separation-character)
			   (if nt (semantic-token-end nt))
			   t)
	(forward-char -1))))

(defun semantic-beginning-of-command ()
  "Move to the beginning of the current command."
    (let ((s (semantic-fetch-overload 'beginning-of-command)))
      (if s (funcall s)
	(semantic-beginning-of-command-default)
	)))

(defun semantic-beginning-of-command-default ()
  "Move to the beginning of the current command.
Depends on `semantic-command-separation-character' to find the
beginning and end of a command."
  (let ((nt (semantic-current-nonterminal)))
    (if (or
	 (and nt
	      (re-search-backward (regexp-quote semantic-command-separation-character)
				  (semantic-token-start nt)
				  t))
	 (re-search-backward (regexp-quote semantic-command-separation-character)
			     nil
			     t))
	(progn
	  ;; Here is a speedy way to skip over junk between the end of
	  ;; the last command, and the beginning of the next.
	  (forward-word 1)
	  (forward-word -1)))))

(defun semantic-ctxt-current-symbol (&optional point)
  "Return the current symbol the cursor is on at POINT in a list.
This will include a list of type/field names when applicable."
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'ctxt-current-symbol)))
      (if s (funcall s)
	(semantic-ctxt-current-symbol-default)
	)))

(defun semantic-ctxt-current-symbol-default ()
  "Return the current symbol the cursor is on at POINT in a list.
This will include a list of type/field names when applicable.
Depends on `semantic-type-relation-separator-character'."
  (let ((fieldsep (mapconcat (lambda (a) (regexp-quote a))
			     semantic-type-relation-separator-character
			     "\\|"))
	(symlist nil)
	end begin)
    (save-excursion
      (if (looking-at "\\w\\|\\s_")
	  (forward-sexp 1))
      (setq end (point))
      (forward-char -1)
      (condition-case nil
	  (while (looking-at "\\w\\|\\s_")
	    ;; We have a symbol.. Do symbol things
	    (forward-sexp -1)
	    (setq symlist (cons (buffer-substring-no-properties (point) end)
				symlist))
	    ;; Skip the next syntactic expression backwards, then go forwards.
	    (forward-sexp -1)
	    (forward-sexp 1)
	    (when (looking-at fieldsep)
	      (setq end (point))
	      (forward-char -1))
	    )
	(error nil)))
    symlist))

(defun semantic-ctxt-current-assignment (&optional point)
  "Return the current assignment near the cursor at POINT.
Return a list as per `semantic-ctxt-current-symbol'.
Return nil if there is nothing relevant."
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'ctxt-current-assignment)))
      (if s (funcall s)
	(semantic-ctxt-current-assignment-default)
	)))

(defun semantic-ctxt-current-assignment-default ()
  "Return the current assignment near the cursor at POINT.
By default, assume that \"=\" indicates an assignment."
  (condition-case nil
      (let* ((begin (save-excursion (semantic-beginning-of-command) (point)))
	     (upc (save-excursion (semantic-up-context) (point)))
	     (nearest (if (< begin upc) upc begin)))
	(save-excursion
	  ;; TODO: Skip a regexp backwards with whitespace from the
	  ;; syntax table.
	  (skip-chars-backward " \t\n")
	  ;; Lets wander backwards till we find an assignment.
	  (while (and (not (= (preceding-char) ?=))
		      (> (point) nearest))
	    (forward-sexp -1)
	    (skip-chars-backward " \t\n")
	    )
	  ;; We are at an equals sign.  Go backwards a sexp, and
	  ;; we'll have the variable
	  (forward-sexp -1)
	  (semantic-ctxt-current-symbol)))
    (error nil)))

(defun semantic-ctxt-current-function (&optional point)
  "Return the current symbol the cursor is on at POINT.
The function returned is the one accepting the arguments that
the cursor is currently in."
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'ctxt-current-function)))
      (if s (funcall s)
	(semantic-ctxt-current-function-default)
	)))

(defun semantic-ctxt-current-function-default ()
  "Return the current symbol the cursor is on at POINT in a list."
  (save-excursion
    (semantic-up-context)
    (when (looking-at "(")
      (semantic-ctxt-current-symbol)))
  )

(defun semantic-ctxt-current-argument (&optional point)
  "Return the current symbol the cursor is on at POINT."
    (if point (goto-char (point)))
    (let ((s (semantic-fetch-overload 'ctxt-current-argument)))
      (if s (funcall s)
	(semantic-ctxt-current-argument-default)
	)))

(defun semantic-ctxt-current-argument-default ()
  "Return the current symbol the cursor is on at POINT in a list.
Depends on `semantic-function-argument-separation-character'."
  (when (semantic-ctxt-current-function)
    (save-excursion
      ;; Only get the current arg index if we are in function args.
      (let ((p (point))
	    (idx 1))
	(semantic-up-context)
	(while (re-search-forward
		(regexp-quote semantic-function-argument-separation-character)
		p t)
	  (setq idx (1+ idx)))
	idx))))

(provide 'semantic-ctxt)

;;; semantic-ctxt.el ends here
