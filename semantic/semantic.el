;;; semantic.el --- Semantic buffer evaluator.

;;; Copyright (C) 1999 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: syntax
;; X-RCS: $Id: semantic.el,v 1.1 1999/05/03 18:06:03 zappo Exp $

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
;; API for determining semantic content of a buffer.  The mode using
;; semantic must be a deterministic programming language.
;;
;; Bovine Table Tips & Tricks:
;; ---------------------------
;;
;; Many of the tricks needed to create rules in bison or yacc can be
;; used here.  The exceptions to this rule are that there is no need to
;; declare bison tokens, and you cannot put "code" in the middle of a
;; match rule.  In addition, you should avoid empty matching rules as
;; I haven't quite gotten those to be reliable yet.
;;
;; The top-level bovine table is an association list of all the rules
;; needed to parse your language, or language segment.  It is easiest
;; to create one master rule file, and call the semantic bovinator on
;; subsections passing down the synthetic rule you want to match.
;;
;; Thus, every entry in the bovine table is of the form:
;; ( SYNTHETIC-SYMBOL MATCH-LIST )
;; 
;; The synthetic symbol is equivalent to the bison RESULT, and the
;; MATCH-LIST is equivalent to the bison COMPONENTS.  Thus, the bison
;; rule:
;;        expseq: expseq1
;;              | expseq2
;;              ;
;; becomes:
;;        ( expseq ( expseq1 ) ( expseq2 ) )
;; which defines RESULT expseq which can be either COMPONENT expseq1
;; or expseq2.  These two table entries also use synthetic results,
;; and also use the DEFAULT RESULT LAMBDA (see below for details on
;; the RESULT LAMBDA).
;;
;; You can also have recursive rules, as in bison.  For example the
;; bison rule:
;;        expseq1: exp
;;               | expseq1 ',' exp
;;               ;
;; becomes:
;;        (expseq1 (exp)
;;                 (expseq1 punctuation "," exp
;;                          (lambda (val start end)
;;                                  ( -generator code- ))))
;;
;; This time, the second rule uses it's own RESULT LAMBDA.
;;
;; Lastly, you can also have STRING LITERALS in your rules, though
;; these are different from Bison.  As can be seen above, a literal is
;; a constant flex symbol, such as `punctuation', followed by a string
;; which is a *regular expression* which must match, or this rule will
;; fail.
;;
;; In BISON, a given rule can have inline ACTIONS.  In the semantic
;; bovinator, there can be only one ACTION which I will refer to here
;; as the RESULT LAMBDA.  There are two default RESULT LAMBDAs which
;; can be used which cover the default case.  The RESULT LAMBDA must
;; return a valid synthetic token.  A synthetic token is always of the
;; form ( TOKEN VALUE1 VALUE2 ... START END).  Token should be the same
;; symbol as the synthetic token generated, though it does not have to
;; be.  The values can be anything you want, including other tokens.
;; START and END indicate where in the buffer this token is, and is
;; easily derived from the START and END parameter passed down.
;;
;; A RESULT LAMBDA must take three parameters, VALS, START and END.
;; VALS is the list of literals derived during the bovination of the
;; match list, including punctuation, parens, and explicit
;; matches.  other elements
;;
;; Here are some example match lists and their code:
;;
;; (expression (lambda (vals start end)
;;                     (append (car vals) (list start end))))
;;
;; In this RESULT LAMBDA, VALS will be of length one, and it's first
;; element will contain the synthetic expression result.  It is
;; likely to use a rule like this when there is a top level synthetic
;; symbol whose contents are several other single synthetic rules.
;; Because of this, we want to result that value with our START and END
;; appended.
;;
;; NOTE: synthetic values passed in as VALS always have their
;;       START/END parts stripped!
;;
;; This example lambda is also one of the DEFAULT lambdas for the case
;; of a single synthetic result.  Thus, the above rule could also be
;; written as (expression).
;;
;; A more complex example uses more flex elements.  Lets match this:
;;
;;    (defun myfunction (arguments) "docstring" ...)
;;
;; If we assume a flex depth of 1, we can write it this way:
;;
;; (open-paren "(" symbol "defun" symbol semantic-list string
;;             (lambda (vals start end)
;;                     (list 'function (nth 2 vals) nil (nth 3 vals)
;;                           (nth 4 vals) start end)))
;;
;; The above will create a function token, whose format is
;; predefined.  (See the symbol `semantic-toplevel-bovine-table' for
;; details on some default symbols that should be provided.)
;;
;; From this we can see that VALS will have the value:
;; ( "(" "defun" "myfunction" "(arguments)" "docstring")
;;
;; If we also want to return a list of arguments in our function
;; token, we can replace `semantic-list' with the following recursive
;; synthetic rule.
;;
;; ( arg-list (semantic-list
;;             (lambda (vals start end)
;;                (semantic-bovinate-from-synthetic start end 'argsyms))))
;; ( argsyms
;;   (open-paren argsyms (lambda (vals start end)
;;			   (append (car (cdr vals)) (list start end))))
;;   (symbol argsyms (lambda (vals start end)
;;		       (append (cons (car vals) (car (cdr vals)))
;;			       (list start end))))
;;   (symbol close-paren (lambda (vals start end)
;;			   (list (car vals) start end))))
;;
;; This recursive rule can find a parenthetic list with any number of
;; symbols in it.
;;
;; Here we also see a new function, `semantic-bovinate-from-synthetic'.
;; This function takes START END and a synthetic result symbol to
;; match.  This will return a complete token, including START and
;; END.  This function should ONLY BE USED IN A RESULT LAMBDA.  It
;; uses knowledge of that scope to reduce the number of parameters
;; that need to be passed in.  This is useful for decomposing complex
;; syntactic elements, such as semantic-list.
;;
;; DESIGN ISSUES:
;;
;;  At the moment, the only thing I really dislike is the RESULT
;;  LAMBDA format.  While having some good defaults is nice, the use
;;  of append and list in the lambda seems unnecessarily complex.
;;
;;  Also of issue, I am still not sure I like the idea of stripping
;;  BEGIN/END off of synthetic tokens passed down in VALS.  While they
;;  are often unnecessary, I can imagine that they could prove useful.
;;  Only time will tell.

;;; History:
;; 

(eval-and-compile
  (condition-case nil
      (require 'working)
    (error
     (progn
       (defmacro working-status-forms (message donestr &rest forms)
	 "Contain a block of code during which a working status is shown."
	 (list 'let (list (list 'msg message) (list 'dstr donestr)
			  '(ref1 0))
	       (cons 'progn forms)))
  
       (defun working-status (&optional percent &rest args)
	 "Called within the macro `working-status-forms', show the status."
	 (message "%s%s" (apply 'format msg args)
		  (if (eq percent t) (concat "... " dstr)
		    (format "... %3d%%" percent ))))
  
       (put 'working-status-forms 'lisp-indent-function 2)))))

;;; Code:
(defvar semantic-toplevel-bovine-table nil
  "Variable that defines how to bovinate top level items in a buffer.
Set this in your major mode to return function and variable semantic
types.

The format of a BOVINE-TABLE is:

 ( ( SYNTHETIC-SYMBOL1 MATCH-LIST1 )
   ( SYNTHETIC-SYMBOL2 MATCH-LIST2 )
   ...
   ( SYNTHETIC-SYMBOLn MATCH-LISTn )
 
Where each SYNTHETIC-SYMBOL is an artificial symbol which can appear
in any child sate.  As a starting place, one of the SYNTHETIC-SYMBOLS
must be `bovine-toplevel'.

A MATCH-LIST is a list of possible matches of the form:

 ( STATE-LIST1
   STATE-LIST2
   ...
   STATE-LISTN )

where STATE-LIST is of the form:
  ( TYPE1 [ \"VALUE1\" ] TYPE2 [ \"VALUE2\" ] ... LAMBDA )

where TYPE is one of the returned types of the token stream.
VALUE is a value, or range of values to match against.  For
example, a SYMBOL might need to match \"foo\".  Some TYPES will not
have matching criteria.

LAMBDA is a lambda expression which is evaled with the text of the
type when it is found.  It is passed the list of all buffer text
elements found since the last lambda expression.  It should return a
semantic element (see below.)

For consistency between languages, always use the following symbol
forms.  It is fine to create new symbols, or to exclude some if they
do not exist, however by using these symbols, you can maximize the
number of language-independent programs available for use.

GENERIC ENTRIES:

 Bovine table entry return elements are up to the table author.  It is
recommended, however, that the following format be used.

 (type-symbol \"NAME\" [\"TYPE\"] ... \"DOCSTRING\" START END)

Where type-symbol is the type of return token found, and NAME is it's
name.  If there is any typing informatin needed to describe this
entry, make that come next.  Next, any information you want follows
the optional type.  The last data entry can be the DOCSTRING.  A
docstring does not have to exist in the form used by Emacs Lisp.  It
could be the text of a comment appearing just before a function call,
or in line with a variable.  Lastly, make sure the last two elements
are START and END.

TOP-LEVEL ENTRIES:

 (variable \"NAME\" \"TYPE\" CONST DEFAULT-VALUE \"DOCSTRING\" START END)
   The definition of a variable, or constant.  CONST is a boolean
   indicating that the variable is constant.  DEFAULT-VALUE can be
   something apropriate such a a string, or list of parsed elements.
   DOCSTRING is optional.
   Some languages do not have the TYPE field available for arg lists.
   In this case nil is appropriate.

 (function \"NAME\" \"TYPE\" ( ARG-LIST ) \"DOCSTRING\" START END)
   A function/procedure definition.  DOCSTRING is optional.
   ARG-LIST is a list of variable definitions.

 (include \"FILE\" \"DOCSTRING\" START END)
   In C, an #include statement.  In elisp, a require statement.
   Indicates additional locations of sources or definitions.

OTHER ENTRIES:")
(make-variable-buffer-local 'semantic-toplevel-bovine-table)

;;; Utility API functions
;;
;; These functions use the flex and bovination engines to perform some
;; simple tasks useful to other programs.
;;
(defun semantic-bovinate-toplevel (&optional depth trashcomments)
  "Bovinate the entire current buffer to a list depth of DEPTH.
DEPTH is optional, and defaults to 0.
Optional argument TRASHCOMMENTS indicates that comments should be
stripped from the main list of synthesized tokens."
  (let ((ss (semantic-flex (point-min) (point-max) (or depth 0)))
	(res nil))
    (working-status-forms "Scanning" "done"
	(while ss
	  (if (not (and trashcomments (eq (car (car ss)) 'comment)))
	      (let ((synthsym
		     (semantic-bovinate-synthetic
		      ss semantic-toplevel-bovine-table)))
		(if (not synthsym)
		    (error "Parse error @ %d" (car (cdr (car ss)))))
		(if (car (cdr synthsym))
		    (setq res (cons (car (cdr synthsym)) res)))
		;; Designated to ignore.
		(setq ss (car synthsym)))
	    (setq ss (cdr ss)))
	  (working-status
	   (if ss
	       (* 100.0 (/ (float (car (cdr (car ss)))) (point-max)))
	     100)))
	(working-dynamic-status t))
    (nreverse res)))


;;; Semantic Bovination
;;
;; Take a semantic token stream, and convert it using the bovinator.
;; The bovinator takes a state table, and converts the token stream
;; into a new semantic stream defined by the bovination table.
;;

(defun semantic-bovinate-synthetic (stream table &optional synthetic)
  "Bovinate STREAM based on the TABLE of synthetic symbols.
Optional argument SYNTHETIC is the synthetic symbol to start with.
Use `bovine-toplevel' if it is not provided."
  (if (not synthetic) (setq synthetic 'bovine-toplevel))
  (let ((ml (assoc synthetic table)))
    (semantic-bovinate-stream stream (cdr ml) table)))

(defun semantic-bovinate-symbol-synthetic-p (sym table)
  "Return non-nil if SYM is in TABLE, indicating it is SYNTHETIC."
  (if (assoc sym table) t nil))

(defun semantic-bovinate-stream (stream matchlist table)
  "Bovinate STREAM using MATCHLIST resolving synthetics with TABLE.
This is the core routine for converting a stream into a table.
See the variable `semantic-toplevel-bovine-table' for details on the
format of MATCHLIST.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (let ((s   nil)			;Temp Stream Tracker
	(lse nil)			;Local Semantic Element
	(lte nil)			;Local matchlist element
	(tev nil)			;Matchlist entry values from buffer
	(val nil)			;Value found in buffer.
	(cvl nil)			;collected values list.
	(out nil)			;Output
	(s-stack nil)			;rollback stream stack
	(start nil)			;the beginning and end.
	(end nil)
	)
    ;; prime the rollback stack
    (setq s-stack (cons stream s-stack)
	  start (car (cdr (car stream))))
    (while matchlist
      (setq s (car s-stack)		;init s from the stack.
	    cvl nil			;re-init the collected value list.
	    lte (car matchlist))	;Get the local matchlist entry.
      (while (and lte (not (listp (car lte))))
	;; COMMENT STRIPPING
	;; If I were to strip comments automatically, do it here.
	;; I suspect that comments may be important to keep for some
	;;   applications.
	(if (semantic-bovinate-symbol-synthetic-p (car lte) table)
	    ;; We have a synthetic symbol.  Recurse inline.
	    (let ((synthout (semantic-bovinate-synthetic s table (car lte))))
	      (setq s (car synthout)
		    val (car (cdr synthout)))
	      (if val
		  (let ((len (length val))
			(strip (nreverse (cdr (cdr (reverse val))))))
		    (setq end (nth (1- len) val) ;reset end to the end of exp
			  cvl (cons strip cvl) ;prepend value of exp
			  lte (cdr lte))) ;update the local table entry
		;; No value means that we need to terminate this match.
		(setq lte nil cvl nil)))	;No match, exit
	  (setq lse (car s)		;Get the local stream element
		s (cdr s))		;update stream.
	  (if (eq (car lte) (car lse))	;syntactic match
	      (progn
		(setq val (semantic-flex-text lse)
		      lte (cdr lte))
		(if (stringp (car lte))
		    (progn
		      (setq tev (car lte)
			    lte (cdr lte))
		      (if (string-match tev val)
			  (setq cvl (cons val cvl)) ;append this value
			(setq lte nil cvl nil)))	;clear the entry (exit)
		  (setq cvl (cons val cvl))) ;append unchecked value.
		(setq end (cdr (cdr lse))))
	    (setq lte nil cvl nil))))	;No match, exit
      (if (not cvl)			;lte=nil;  there was no match.
	  (setq matchlist (cdr matchlist)) ;Move to next matchlist entry
	(setq out (if (car lte)
		      (apply (car lte)	;call matchlist fn on values
			     (nreverse cvl) start (list end))
		    (cond ((and (= (length cvl) 1)
				(listp (car cvl)))
			   (append (car cvl) (list start end)))
			  (t
			   (append (nreverse cvl) (list start end))))
		    )
	      matchlist nil)		;generate exit condition
	;; Nothin?
	))
    (list s out)))

(defun semantic-bovinate-from-synthetic (start end synth &optional depth)
  "Bovinate from within a synthetic lambda from START to END.
Depends on the existing environment created by `semantic-bovinate-stream'.
Argument SYNTH is the synthetic symbol to start with.
Optional argument DEPTH is the depth of lists to dive into.
Whan used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (let* ((stream (semantic-flex start end (or depth 1)))
	 (ans (semantic-bovinate-synthetic stream table synth)))
    (car (cdr ans))))

;;; Semantic Flexing
;;
;; This is a simple scanner which uses the syntax table to generate
;; a stream of simple tokens.
;;
;; A flex element is of the form:
;;  (SYMBOL START . END)
;; Where symbol is the type of thing it is.  START and END mark that
;; objects boundary.

(defun semantic-flex-buffer (&optional depth)
  "Sematically flex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-flex (point-min) (point-max) depth))

(defun semantic-flex (start end &optional depth)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element being a list, such
as (symbol start-expression .  end-expresssion)."
  ;(message "Flexing muscles...")
  (let ((ts nil)
	(sym nil)
	(pos (point))
	(ep nil)
	(curdepth 0)
	(cs (concat "\\(\\s<\\|" (regexp-quote comment-start) "\\)")))
    (goto-char start)
    (while (< (point) end)
      (cond (;; comment end is also EOL for some languages.
	     (looking-at "\\(\\s-\\|\\s>\\)+"))
	    ((looking-at "\\(\\sw\\|\\s_\\)+")
	     (setq ts (cons (cons 'symbol
				  (cons (match-beginning 0) (match-end 0)))
			    ts)))
	    ((looking-at "\\s\\+")
	     (setq ts (cons (cons 'charquote
				  (cons (match-beginning 0) (match-end 0)))
			    ts)))
	    ((looking-at "\\s(+")
	     (if (or (not depth) (< curdepth depth))
		 (progn
		   (setq curdepth (1+ curdepth))
		   (setq ts (cons (cons 'open-paren
					(cons (match-beginning 0) (match-end 0)))
				  ts)))
	       (setq ts (cons (cons 'semantic-list
				    (cons (match-beginning 0)
					  (save-excursion
					    (forward-list 1)
					    (setq ep (point)))))
			      ts))))
	    ((looking-at "\\s)+")
	     (setq ts (cons (cons 'close-paren
				  (cons (match-beginning 0) (match-end 0)))
			    ts))
	     (setq curdepth (1- curdepth)))
	    ((looking-at "\\s\"")
	     ;; Zing to the end of this string.
	     (setq ts (cons (cons 'string
				  (cons (match-beginning 0)
					(save-excursion
					  (forward-sexp 1)
					  (setq ep (point)))))
			    ts)))
	    ((looking-at cs)
	     ;; Zing to the end of this comment.
	     (if (eq (car (car ts)) 'comment)
		 (setcdr (cdr (car ts)) (save-excursion
					  (forward-comment 1)
					  (setq ep (point))))
	       (setq ts (cons (cons 'comment
				    (cons (match-beginning 0)
					  (save-excursion
					    (forward-comment 1)
					    (setq ep (point)))))
			      ts))))
	    ((looking-at "\\(\\s.\\|\\s$\\|\\s'\\)")
	     (setq ts (cons (cons 'punctuation
				  (cons (match-beginning 0) (match-end 0)))
			    ts)))
	    (t (error "What is that?")))
      (goto-char (or ep (match-end 0)))
      (setq ep nil))
    (goto-char pos)
    ;(message "Flexing muscles...done")
    (nreverse ts)))

(defun semantic-flex-text (semobj)
  "Fetch the text associated with the semantic object SEMOBJ."
  (buffer-substring-no-properties (car (cdr semobj)) (cdr (cdr semobj))))

(defun semantic-flex-list (semlist depth)
  "Flex the body of SEMLIST to DEPTH."
  (semantic-flex (car (cdr semlist)) (cdr (cdr semlist)) depth))

(provide 'semantic)

;;; semantic.el ends here

