;;; semantic-analyze.el --- Analyze semantic tokens against local context

;;; Copyright (C) 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-analyze.el,v 1.1 2001/10/04 14:52:44 zappo Exp $

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
;; Semantic-ctxt provides ways of analyzing, and manipulating the
;; semantic context of a language in code.
;;
;; This library provides routines for finding intelligent answers to
;; tough problems, such as if an argument to a function has the correct
;; return type, or all possible tokens that fit in a given local context.
;;

(require 'semantic-ctxt)
(eval-when-compile (require 'semanticdb))

;;; Code:
(defun semantic-analyze-find-nonterminal (name &optional tokentype)
  "Attempt to find a nonterminal with NAME.
Optional argument TOKENTYPE specifies tye type of token to
return, such as 'function or 'variable.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
	   (semanticdb-minor-mode-p))
      ;; Search the database
      (let ((dbans (semanticdb-find-nonterminal-by-name
		    name nil nil t nil t)))
	;; Lame, grabbing the first file match
	(cdr (car dbans)))
    ;; Search just this file
    (semantic-find-nonterminal-by-name
     name (current-buffer) nil t)))

(defun semantic-analyze-token-type-to-name (token)
  "Get the name of TOKEN's type.
The TYPE field in a token can be nil (return nil)
or a string, or a non-positional token."
  (let ((tt (semantic-token-type token)))
    (cond ((semantic-token-p tt)
	   (semantic-token-name tt))
	  ((stringp tt)
	   tt)
	  ((listp tt)
	   (car tt))
	  (t nil))))

(defun semantic-analyze-find-nonterminal-sequence (sequence &optional lasttokentype)
  "Attempt to find all nonterminals in SEQUENCE.
Optional argument LASTTOKENTYPE is the tokentype of the last
entry in SEQUENCE, such as 'variable, or 'function."
  (let ((s sequence)			;copy of the sequence
	(tmp nil)			;tmp find variable
	(nexttype nil)			;a token for the type next in sequence
	(tok nil)			;token return list
	(localvar (semantic-get-local-variables))
	)
    ;; For the first entry, it better be a variable, but it might
    ;; be in the local context too.
    (setq tmp (or (semantic-analyze-find-nonterminal (car s) 'variable)
		  (semantic-find-nonterminal-by-name
		   (car s) (semantic-get-local-arguments))
		  (semantic-find-nonterminal-by-name
		   (car s) localvar)))

    (if (and (listp tmp) (semantic-token-p (car tmp)))
	;; We should be smarter... :(
	(setq tmp (car tmp)))
    (if (not (semantic-token-p tmp))
	(error "Cannot find definition for \"%s\"" (car s)))
    (setq s (cdr s))
    (setq tok (cons tmp tok))

    ;; For the middle entries
    (while s
      ;; Using the token found in TMP, lets find the token
      ;; representing the full typeographic information of it's
      ;; type, and use that to determine the search context for
      ;; (car s)
      (let ((tmptype (semantic-analyze-token-type-to-name tmp))
	    (tmptypetok nil)
	    (slots nil))
	
	(if tmptype
	    (setq tmptypetok
		  (or (semantic-analyze-find-nonterminal tmptype)
		      ;; In some languages, types can be
		      (semantic-find-nonterminal-by-name
		       (car s) localvar)))
	  (error "Semantic token %s has no type info"
		 (semantic-token-name tmp)))

	(if (and (listp tmptypetok) (semantic-token-p (car tmptypetok)))
	    ;; We should be smarter... :(
	    (setq tmptypetok (car tmptypetok)))

	;; Now that we have the type associated with TMP, we can find
	;; fields in that structure.  Make sure we really have a type
	;; first though.
	(if (not (and tmptypetok
		      (eq (semantic-token-token tmptypetok) 'type)))
	    (error "Symbol %s type %s is not a known aggregate type"
		   (semantic-token-name tmp)
		   tmptype))

	;; get the slots
	(setq slots (semantic-token-type-parts tmptypetok))

	;; find (car s) in the list o slots
	(setq tmp (semantic-find-nonterminal-by-name (car s)
						     slots nil nil))
	
	(if (and (listp tmp) (semantic-token-p (car tmp)))
	    ;; We should be smarter... :(
	    (setq tmp (car tmp)))

	(if (not (semantic-token-p tmp))
	    (if (cdr s)
		;; In the middle, we need to keep seeking our types out.
		(error "Cannot find definition for \"%s\"" (car s))
	      ;; Else, it's ok to end with a non-token
	      (setq tmp (car s))))

	(setq tok (cons tmp tok))
	)
      (setq s (cdr s)))

    ;; Return the mess
    (nreverse tok)))

(defun semantic-analyze-current-context (&optional position)
  "Analyze the current context at POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns a list structured in one of the following ways:
  (assignment TOKEN PREFIX)
    - Positioned where a value in an assignment belongs.
      TOKEN is a semantic token representing the item being assigned
            into.  (A variable)
      PREFIX is nil, a list returned by
       	    `semantic-analyze-find-nonterminal-sequence' or a string
       	    representing text already at POSITION which can be used to
       	    narrow down possible matches to that context.

  (functionarg TOKEN1 INDEX TOKEN2 PREFIX)
    - Positioned where an argument to a function belongs.
      TOKEN1 is a list of semantic tokens for the function definition
             foo.fun() results in a definition for the variable/class FOO
             followed by the def in FOO's type for FUN.
      INDEX is the index of the argument (ie 2nd argument)
      TOKEN2 is a list of semantic tokens represeing the argument
             definition.
      PREFIX See above.

  (nocontext PREFIX)
    - No particular context.
      PREFIX See above."
  (interactive)
  (let ((context-return nil)
	(prefix (semantic-ctxt-current-symbol))
	(function (semantic-ctxt-current-function)))

    (condition-case nil
	;; If we are on lame stuff, it won't be found!
	(setq prefix (semantic-analyze-find-nonterminal-sequence prefix))
      (error nil))

    (if function
	;; If we have a function, then we can get the argument
	(let ((arg (semantic-ctxt-current-argument))
	      (fntok (semantic-analyze-find-nonterminal-sequence function))
	      (argtok nil))
	  ;; Find the token belonging to our function
	  (setq argtok (nth (1- arg)
			    (semantic-token-function-args fntok)))

	  (setq context-return
		(list 'functionarg
		      fntok
		      arg
		      argtok
		      prefix))
	  )
      ;; No function, try assignment
      (let ((assign (semantic-ctxt-current-assignment)))
	(if assign
	    ;; We have an assignment
	    (let ((asstok (semantic-analyze-find-nonterminal-sequence
			   assign)))
	      (setq context-return
		    (list 'assignment
			  ;; Just the last entry from the list.
			  ;; thus foo.bar.a returns the type info for
			  ;; a only.
			  asstok
			  prefix)))
	  ;; Nothing in particular
	  (setq context-return
		(list 'nocontext prefix)))))

    ;; Check for interactivity
    (if (interactive-p)
	(semantic-analyze-pop-to-context context-return))
    ;; Return our context.
    context-return))

(defcustom semantic-analyze-summary-function 'semantic-prototype-nonterminal
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-token->text-functions'."
  :group 'semantic
  :type semantic-token->text-custom-list)

(defun semantic-analyze-princ-sequence (sequence &optional prefix)
  "Send the token SEQUENCE to standard out.
Use PREFIX as a label."
  (while sequence
    (princ prefix)
    (if (semantic-token-p (car sequence))
	(princ (funcall semantic-analyze-summary-function
			(car sequence)))
      (if (stringp (car sequence))
	  (princ (semantic-colorize-text (car sequence) 'variable))
	(format "%S" (car sequence))))
    (princ "\n")
    (setq sequence (cdr sequence))
    (setq prefix (make-string (length prefix) ? ))
    ))

(defun semantic-analyze-pop-to-context (context)
  "Display CONTEXT in a temporary buffer.
CONTEXT's content is described in `semantic-analyze-current-context'."
  (with-output-to-temp-buffer "*Semantic Context Analysis*"
    (let ((prefix nil))
      (princ "Context Type: ")
      (princ (car context))
      (princ "\n")
      (cond ((eq (car context) 'functionarg)
	     (semantic-analyze-princ-sequence (nth 1 context)
					      "Function: ")
	     (princ "Argument Index: ")
	     (princ (nth 2 context))
	     (princ "\n")
	     (semantic-analyze-princ-sequence (nth 3 context)
					      "Argument: ")
	     (setq prefix (nth 4 context))
	     )
	    ((eq (car context) 'assignment)
	     (semantic-analyze-princ-sequence (nth 1 context)
					      "Assignee: ")
	     (setq prefix (nth 2 context))
	     )
	    (t (setq prefix (car (cdr context)))))
      ;; Deal with the prefix
      (semantic-analyze-princ-sequence prefix "Prefix: ")
      ))
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Semantic Context Analysis*"))
  )

(provide 'semantic-analyze)

;;; semantic-analyze.el ends here
