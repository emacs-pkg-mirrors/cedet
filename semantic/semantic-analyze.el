;;; semantic-analyze.el --- Analyze semantic tokens against local context

;;; Copyright (C) 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-analyze.el,v 1.2 2001/10/05 19:40:50 zappo Exp $

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

(defvar semantic-analyze-current-local-variables nil
  "The current set of local variables.
Bound in the outer scope of a function using local variables
to optimize out the need to calculate them multiple times.")

;;; Context Analysis
;;
(defun semantic-analyze-find-nonterminals-by-prefix (prefix)
  "Attempt to find a nonterminal with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (let ((expr (concat "^" (regexp-quote prefix))))
    (if (and (fboundp 'semanticdb-minor-mode-p)
	     (semanticdb-minor-mode-p))
	;; Search the database
	(let ((dbans (semanticdb-find-nonterminal-by-name-regexp
		      expr  nil nil t nil t)))
	  ;; Concatenate all matches together.
	  (apply #'append (mapcar #'cdr dbans))
	  )
      ;; Search just this file because there is no DB available.
      (semantic-find-nonterminal-by-name-regexp
       expr (current-buffer) nil t))))
  
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

(defun semantic-analyze-find-nonterminal-sequence (sequence &optional typereturn)
  "Attempt to find all nonterminals in SEQUENCE.
Optional argument TYPERETURN is a symbol ini which the types of all found
will be stored.  If nil, that data is thrown away."
  (let ((s sequence)			;copy of the sequence
	(tmp nil)			;tmp find variable
	(nexttype nil)			;a token for the type next in sequence
	(tok nil)			;token return list
	(toktype nil)			;token types return list
	(localvar
	 (or semantic-analyze-current-local-variables
	     (semantic-get-local-variables)))
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
	(setq toktype (cons tmptypetok toktype))
	)
      (setq s (cdr s)))

    (if typereturn (set typereturn (nreverse toktype)))
    ;; Return the mess
    (nreverse tok)))

(defun semantic-analyze-current-context (position)
  "Analyze the current context at POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns a list structured in one of the following ways:
  (assignment TOKEN PREFIX PREFIXTYPES)
    - Positioned where a value in an assignment belongs.
      TOKEN is a semantic token representing the item being assigned
            into.  (A variable)
      PREFIX is nil, a list returned by
       	    `semantic-analyze-find-nonterminal-sequence' or a string
       	    representing text already at POSITION which can be used to
       	    narrow down possible matches to that context.
      PREFIXTYPES is a list of semantic tokens representing the types
            of elements in PREFIX.

  (functionarg TOKEN1 INDEX TOKEN2 PREFIX PREFIXTYPES)
    - Positioned where an argument to a function belongs.
      TOKEN1 is a list of semantic tokens for the function definition
             foo.fun() results in a definition for the variable/class FOO
             followed by the def in FOO's type for FUN.
      INDEX is the index of the argument (ie 2nd argument)
      TOKEN2 is a list of semantic tokens represeing the argument
             definition.
      PREFIX See above.
      PREFIXTYPES See above.

  (nocontext PREFIX PREFIXTYPES)
    - No particular context.
      PREFIX See above.
      PREFIXTYPES See above."
  (interactive "d")
  (save-excursion
    (goto-char position)
    (let ((context-return nil)
	  (prefix (semantic-ctxt-current-symbol))
	  (prefixtypes nil)
	  (function (semantic-ctxt-current-function)))

      (condition-case nil
	  ;; If we are on lame stuff, it won't be found!
	  (setq prefix (semantic-analyze-find-nonterminal-sequence
			prefix 'prefixtypes))
	(error nil))

      (if function
	  ;; If we have a function, then we can get the argument
	  (let* ((arg (semantic-ctxt-current-argument))
		 (fntok (semantic-analyze-find-nonterminal-sequence function))
		 (fntokend (car (reverse fntok)))
		 (argtok (nth (1- arg)
			      (semantic-token-function-args fntokend))))

	    (setq context-return
		  (list 'functionarg
			fntok
			arg
			(list argtok)
			prefix
			prefixtypes))
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
			    prefix
			    prefixtypes)))
	    ;; Nothing in particular
	    (setq context-return
		  (list 'nocontext prefix prefixtypes)))))

      ;; Check for interactivity
      (if (interactive-p)
	  (semantic-analyze-pop-to-context context-return))
      ;; Return our context.
      context-return)))


;;; Context Analysis Results
;;
;; Use the current context to provide useful token streams
(defun semantic-analyze-possible-completions (point)
  "Return a list of semantic tokens which are possible completions.
Analysis is done at POINT."
  (interactive "d")
  (let ((a (semantic-analyze-current-context point))
	(semantic-analyze-current-local-variables
	 (semantic-get-local-variables point))
	(fnargs (semantic-get-local-arguments point))
	(desired-type nil)
	(prefix nil)
	(prefixtypes nil)
	(completetext nil)
	(completetexttype nil)
	(c nil))
    ;; Calculate the completions
    (cond ((eq (car a) 'functionarg)
	   ;; The desired type of function arg is based on the
	   ;; type of the argument in the 3rd pos
	   (setq desired-type (car (nth 3 a)))
	   ;; Prefix data
	   (setq prefix (nth 4 a))
	   (setq prefixtypes (nth 5 a))
	   )
	  ((eq (car a) 'assignment)
	   ;; The desired type is the type of the final entry in the
	   ;; assignee slot in the 1st position
	   (setq desired-type (car (reverse (nth 1 a))))
	   ;; Prefix data
	   (setq prefix (nth 2 a))
	   (setq prefixtypes (nth 3 a))
	   )
	  (t
	   ;; There is no desired type.
	   (setq desired-type nil)
	   ;; Prefix data
	   (setq prefix (car (cdr a)))
	   (setq prefixtypes (car (cdr a)))
	   ))

    (when desired-type
      ;; Convert the desired type if needed.
      (if (not (eq (semantic-token-token desired-type) 'type))
	  (setq desired-type (semantic-token-type desired-type)))
      ;; Protect against plain strings
      (cond ((stringp desired-type)
	     (setq desired-type (list desired-type 'type)))
	    ((and (stringp (car desired-type))
		  (not (semantic-token-p desired-type)))
	     (setq desired-type (list (car desired-type) 'type)))
	    (t (setq desired-type nil))
	    ))

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-token-p completetext)
	(setq completetext (semantic-token-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (if (or (not completetexttype)
	    (not (and (semantic-token-p completetexttype)
		      (eq (semantic-token-token completetexttype) 'type))))
	;; What should I do here?  I think this is an error condition.
	(setq completetexttype nil))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if completetexttype
	;; We have a type of the text preceeding our completetext.
	;; Lets set that as our stream.
	(let ((stream (semantic-token-type-parts completetexttype))
	      )
		
	  (while stream

	    ;; Find all the bits.
	    (setq c (append (semantic-find-nonterminal-by-name-regexp
			     (concat "^" completetext)
			     stream nil nil)
			    c))

	    ;; Once this stream is analyzed, find the parent of the current
	    ;; type, and repeat the process.
	    (setq completetexttype
		  (semantic-analyze-find-nonterminal
		   (semantic-token-type-parent completetexttype))
		  stream (semantic-token-type-parts completetexttype))))

      (let ((expr (concat "^" completetext)))
	;; No type based on the completetext.  This is a free-range
	;; var or function.  We need to expand our search beyond this
	;; scope into semanticdb, etc.
	(setq c (append
		 ;; Argument list
		 (semantic-find-nonterminal-by-name-regexp
		  expr fnargs
		  nil nil)
		 ;; Local variables
		 (semantic-find-nonterminal-by-name-regexp
		  expr semantic-analyze-current-local-variables
		  nil nil)
		 ;; The world
		 (semantic-analyze-find-nonterminals-by-prefix
		  completetext))
	      )
	))

    ;; Ok, we now have a completion list based on the text we found
    ;; we want to complete on.  Now filter that stream against the
    ;; type we want to search for.
    (if desired-type
	(setq c (semantic-find-nonterminal-by-type
		 (semantic-token-name desired-type)
		 c nil nil)))

    ;; All done!

    ;; If interactive, display them.
    (when (interactive-p)
      (with-output-to-temp-buffer "*Possible Completions*"
	(semantic-analyze-princ-sequence c ""))
      (shrink-window-if-larger-than-buffer
       (get-buffer-window "*Possible Completions*"))
      )
    c))


;;; Friendly output of a context analysis.
;;
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
    (let ((prefix nil)
	  (prefixtypes nil))
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
	     (setq prefixtypes (nth 5 context))
	     )
	    ((eq (car context) 'assignment)
	     (semantic-analyze-princ-sequence (nth 1 context)
					      "Assignee: ")
	     (setq prefix (nth 2 context))
	     (setq prefixtypes (nth 3 context))
	     )
	    (t (setq prefix (car (cdr context)))
	       (setq prefixtypes (car (cdr (cdr context))))))
      ;; Deal with the prefix
      (semantic-analyze-princ-sequence prefix "Prefix: ")
      (semantic-analyze-princ-sequence prefixtypes "PrefixTypes: ")
      ))
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Semantic Context Analysis*"))
  )

(provide 'semantic-analyze)

;;; semantic-analyze.el ends here
