;;; semantic-lex.el --- Lexical Analyzer builder

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-lex.el,v 1.1 2002/07/03 18:44:43 zappo Exp $

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
;; In semantic 1.x, the lexical analyzer was an all purpose routine.
;; To boost efficiency, the analyzer is now a series of routines that
;; are constructed at build time into a single routine.  This will
;; eliminate uneeded if statements to speed the lexer.

(defvar semantic-lex-analyzer nil
  "The lexical analyzer used for a given buffer.")

;;; Code:
(defvar semantic-lex-tokens
  '(
    (bol)
    (charquote)
    (close-paren)
    (comment)
    (newline)
    (open-paren)
    (punctuation)
    (semantic-list)
    (string)
    (symbol)
    (whitespace)
    )
  "An alist of of semantic token types.
As of December 2001 (semantic 1.4beta13), this variable is not used in
any code.  The only use is to refer to the doc-string from elsewhere.

The key to this alist is the symbol representing token type that
\\[semantic-flex] returns.  These are

  - bol:           Empty string matching a beginning of line.
                   This token is produced with
                   `semantic-lex-beginning-of-line'.

  - charquote:     String sequences that match `\\s\\+' regexp.
                   This token is produced with `semantic-lex-charquote'.

  - close-paren:   Characters that match `\\s)' regexp.
                   These are typically `)', `}', `]', etc.
                   This token is produced with
                   `semantic-lex-close-paren'.

  - comment:       A comment chunk.  These token types are not
                   produced by default.
                   This token is produced with `semantic-lex-comments'.
                   Comments are ignored with `semantic-lex-ignore-comments'.
                   Comments are treated as whitespace with
                   `semantic-lex-comments-as-whitespace'.

  - newline        Characters matching `\\s-*\\(\n\\|\\s>\\)' regexp.
                   This token is produced with `semantic-lex-newline'.

  - open-paren:    Characters that match `\\s(' regexp.
                   These are typically `(', `{', `[', etc.
                   If `semantic-lex-paren-or-list' is used,
                   then `open-paren' is not usually generated unless
                   the `depth' argument to \\[semantic-lex] is
                   greater than 0.
                   This token is always produced if the analyzer
                   `semantic-lex-open-paren' is used.

  - punctuation:   Characters matching `{\\(\\s.\\|\\s$\\|\\s'\\)'
                   regexp.
                   This token is produced with `semantic-lex-punctuation'.
                   Always specify this analyzer after the comment
                   analyzer.

  - semantic-list: String delimited by matching parenthesis, braces,
                   etc.  that the lexer skipped over, because the
                   `depth' parameter to \\[semantic-flex] was not high
                   enough.
                   This token is produced with `semantic-lex-paren-or-list'.

  - string:        Quoted strings, i.e., string sequences that start
                   and end with characters matching `\\s\"'
                   regexp.  The lexer relies on @code{forward-sexp} to
                   find the matching end.
                   This token is produced with `semantic-lex-string'.

  - symbol:        String sequences that match `\\(\\sw\\|\\s_\\)+'
                   regexp.
                   This token is produced with
                   `semantic-lex-symbol-or-keyword'.  Always add this analyzer
                   after `semantic-lex-number', or other analyzers that
                   match its regular expression.

  - whitespace:    Characters that match `\\s-+' regexp.
                   This token is produced with `semantic-lex-whitespace'.")

(defvar semantic-lex-syntax-modifications nil
  "Changes to the syntax table for this buffer.
These changes are active only while the buffer is being flexed.
This is a list where each element has the form:
  (CHAR CLASS)
CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what syntax class CHAR has.")
(make-variable-buffer-local 'semantic-lex-syntax-modifications)

(defvar semantic-lex-comment-regex nil
  "Regular expression for identifying comment start during lexical analysis.
This may be automatically set when semantic initializes in a mode, but
may need to be overriden for some special languages.")
(make-variable-buffer-local 'semantic-lex-comment-regex)

(defvar semantic-lex-number-expression
  ;; This expression was written by David Ponce for Java, and copied
  ;; here for C and any other similar language.
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Regular expression for matching a number.
If this value is nil, no number extraction is done during lex.
This expression tries to match C and Java like numbers.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")
(make-variable-buffer-local 'semantic-lex-number-expression)

(defvar semantic-lex-unterminated-syntax-end-function
  (lambda (syntax syntax-start lex-end) lex-end)
  "Function called when unterminated syntax is encountered.
This should be set to one function.  That function should take three
parameters.  The SYNTAX, or type of syntax which is unterminated.
SYNTAX-START where the broken syntax begins.
LEX-END is where the lexical analysis was asked to end.
This function can be used for languages that can intelligently fix up
broken syntax, or the exit lexical analysis via `throw' or `signal'
when finding unterminated syntax.")

(defun semantic-lex-init ()
  "Initialize any lexical state for this buffer."
  (when (not semantic-lex-comment-regex)
    (setq semantic-lex-comment-regex
	  (if comment-start-skip
	      (concat "\\(\\s<\\|" comment-start-skip "\\)")
	    (concat "\\(\\s<\\)"))
	  ))
  )

;; Do the cutover for compatibility
(defalias 'semantic-flex 'semantic-lex)

(defun semantic-lex (start end &optional depth length)
  "Lexically analyze text in the current buffer between START and END.
Optional argument DEPTH indicates at what level to scan over entire
lists.  The last argument, LENGTH specifies that `semantic-lex'
should only return LENGTH tokens.  The return value is a token stream.
Each element is a list, such of the form
  (symbol start-expression . end-expression)
where SYMBOL denotes the token type.
See `semantic-lex-tokens' variable for details on token types.  END
does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'."
  (funcall semantic-lex-analyzer start end depth length nil)
  )

(defmacro semantic-lex-one-token (analyzers)
  "Calculate one token from the current buffer at point.
Uses locally bound variables from `define-lex'.
Argument ANALYZERS is the list of analyzers being used."
  (let ((code nil) (an analyzers))
    (while an
      (setq code (cons (symbol-value (car an)) code)
	    an (cdr an)))
    (cons 'cond (nreverse code))))

(defmacro define-lex (name doc &rest analyzers)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
ANALYZERS are small code snippets of analyzers to use when
building the new NAMED analyzer.  Only use analyzers which
are written to be used in `define-lex'.
Each analyzer should be an analyzer created with `define-lex-analyzer'."
  `(defun ,name  (start end &optional depth length)
     ,(concat doc "\nSee `semantic-lex' for more information.")
     (let ((starting-position (point))
	   (token-stream nil)
	   (end-point start)
	   (current-depth 0)
	   (newsyntax (copy-syntax-table (syntax-table)))
	   (mods semantic-lex-syntax-modifications)
	   )
       ;; Update the syntax table
       (while mods
	 (modify-syntax-entry (car (car mods))
			      (car (cdr (car mods)))
			      newsyntax)
	 (setq mods (cdr mods)))
       (with-syntax-table newsyntax
	 (goto-char start)
	 (while (and (< (point) end)
		     (or (not length) (<= (length token-stream) length)))
	   (semantic-lex-one-token ,analyzers)
	   (goto-char end-point)))
       ;; Return to where we started
       (goto-char starting-position)
       ;; Return the token stream
       (nreverse token-stream)))
  )

;;; Analyzer creation macros
;;
;;; Semantic lex token creation
;;
(defmacro semantic-lex-token (symbol start end)
  "Create a lexical token.
SYMBOL is a symbol representing the class of syntax found.
START and END define the bounds of the token in the current buffer.
This macro should only be called within the bounds of
`define-lex-analyzer'.  It changes the values of the lexical
analyzer variables `token-stream' and `end-point'.
If you need to move `end-point' somewhere else, just modify this
variable after calling `semantic-lex-token'."
  `(setq end-point ,end
	 token-stream (cons (cons ,symbol (cons ,start end-point))
			    token-stream)))

(defmacro define-lex-analyzer (name doc condition &rest forms)
  "Create a single lexical analyzer NAME with DOC.
When an analyzer is called, the current buffer and point are
positioned in a buffer at the location to be analyzed.
CONDITION is an expression which returns t if FORMS should be run.
Within the bounds of CONDITION and FORMS, the use of backquote
can be used to evaluate expressions at compile time.
While forms are running, the following variables will be locally bound:
 macro.  The macro will have access to
some local lexical analysis variables, including:
  `start', `end', `depth', `length' - As passed to `semantic-lex'.
  `current-depth' - The current depth of `semantic-list' that has
                  been decended.
  `end-point' - End Point after match.  Analyzers should set this to
                   a buffer location if their match string does not
                   represent the end of the matched text.
  `token-stream' - The token list being collected.  Add new lexical
                   tokens to this list.
Proper action in FORMS is to move the value of `end-point' to after
the location of the analyzed entry, and to add any discovered tokens
at the beginning of `token-stream'.   This can be done by using
`semantic-lex-token'."
  `(defvar ,name nil ,doc)
  `(setq ,name '(,condition ,@forms))
  )


(defmacro define-lex-regex-analyzer (name doc regexp &rest forms)
  "Create a lexical analyzer with NAME and DOC  that match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers."
  `(defvar ,name nil ,doc)
  `(setq ,name  '((looking-at ,regexp) ,@forms)))

(defmacro define-lex-simple-regex-analyzer (name doc regexp toksym
						 &optional index
						 &rest forms)
  "Create a lexical analyzer with NAME and DOC  that match REGEXP.
TOKSYM is the symbol to use when creating a semantic lexical token.
INDEX is the index into the match that defines the bounds of the token.
Index should be a plain integer, and not specified in the macro as an
expression.
FORMS are evaluated upon a successful match BEFORE the new token is
created.  It is valid to ignore FORMS.
See `define-lex-analyzer' for more about analyzers."
  `(defvar ,name nil ,doc)
  `(setq ,name
	'((looking-at ,regexp)
	  ,@forms
	  (semantic-lex-token ,toksym
			      (match-beginning ,(or index 0))
			      (match-end ,(or index 0)))))
  )

;;; Analyzers
;;
(define-lex-analyzer semantic-lex-default-action
  "The default action when no other lexical actions match text.
This action will just throw an error."
  t
  (error "Unmatched Text during Lexical Analysis")
  )

(define-lex-analyzer semantic-lex-beginning-of-line
  "Detect and create a beginning of line token (BOL)."
  (and (bolp)
       ;; Just insert a (bol N . N) token in the token stream,
       ;; without moving the point.  N is the point at the
       ;; beginning of line.
       (semantic-lex-token 'bol (point) (point))
       nil) ;; CONTINUE
  ;; We identify and add the BOL token onto the stream, but since
  ;; end-point doesn't move, we always fail CONDITION, and have no
  ;; FORMS body.
  nil)

(define-lex-simple-regex-analyzer semantic-lex-newline
  "Detect and create newline tokens."
  "\\s-*\\(\n\\|\\s>\\)"  newline 1)

(define-lex-regex-analyzer semantic-lex-newline-as-whitespace
  "Detect and create newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters) AND when you want whitespace tokens."
  "\\s-*\\(\n\\|\\s>\\)"  
  ;; Language wants whitespaces, link them together.
  (if (eq (car (car token-stream)) 'whitespace)
      (setcdr (cdr (car token-stream)) (match-end 0))
    (semantic-lex-token 'whitespace
			(match-beginning 0)
			(match-end 0))))

(define-lex-regex-analyzer semantic-lex-ignore-newline
  "Detect and create newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters)."
  "\\s-*\\(\n\\|\\s>\\)"  
  (setq end-point (match-end 0)))

(define-lex-regex-analyzer semantic-lex-whitespace
  "Detect and create whitespace tokens."
  ;; catch whitespace when needed
  "\\s-+"
  ;; Language wants whitespaces, link them together.
  (if (eq (car (car token-stream)) 'whitespace)
      (setcdr (cdr (car token-stream)) (match-end 0))
    (semantic-lex-token 'whitespace
			(match-beginning 0)
			(match-end 0))))

(define-lex-regex-analyzer semantic-lex-ignore-whitespace
  "Detect and skip over whitespace tokens."
  ;; catch whitespace when needed
  "\\s-+"
  ;; Skip over the detected whitespace.
  (setq end-point (match-end 0))
  )

(define-lex-simple-regex-analyzer semantic-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'number)

(define-lex-regex-analyzer semantic-lex-symbol-or-keyword
  "Detect and create symbol and keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-token
   (or (semantic-flex-keyword-p (match-string 0)) 'symbol)
   (match-beginning 0) (match-end 0)))

(define-lex-simple-regex-analyzer semantic-lex-charquote
  "Detect and create charquote tokens."
  ;; Character quoting characters (ie, \n as newline)
  "\\s\\+" 'charquote)

(define-lex-simple-regex-analyzer semantic-lex-punctuation
  "Detect and create punctuation tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)" 'punctuation)

(define-lex-regex-analyzer semantic-lex-paren-or-list
  "Detect open parenthisis.  
Return either a paren token or a semantic list token depending on
`current-depth'."
  "\\s("
  (if (or (not depth) (< current-depth depth))
      (progn
	(setq current-depth (1+ current-depth))
	(semantic-lex-token 'open-paren (match-beginning 0) (match-end 0)))
    (semantic-lex-token 'semantic-list
			(match-beginning 0)
			(save-excursion
			  (condition-case nil
			      (forward-list 1)
			    ;; This case makes lex robust
			    ;; to broken lists.
			    (error
			     (goto-char
			      (funcall
			       semantic-lex-unterminated-syntax-end-function
			       'semantic-list
			       start end))))
			  (setq end-point (point))))))

(define-lex-simple-regex-analyzer semantic-lex-open-paren
  "Detect and create an open parenthisis token."
  "\\s(" 'open-paren 0  (setq current-depth (1+ current-depth)))

(define-lex-simple-regex-analyzer semantic-lex-close-paren
  "Detect and create a close paren token."
  "\\s)" 'close-paren 0 (setq current-depth (1- current-depth)))

(define-lex-regex-analyzer semantic-lex-string
  "Detect and create a string token."
  "\\s\""
  ;; Zing to the end of this string.
  (semantic-lex-token 'string (point)
		      (save-excursion
			(condition-case nil
			    (forward-sexp 1)
			  ;; This case makes lex
			  ;; robust to broken strings.
			  (error
			   (goto-char
			    (funcall
			     semantic-lex-unterminated-syntax-end-function
			     'string
			     start end))))
			(point))))

(define-lex-regex-analyzer semantic-lex-comments
  "Detect and create a comment token."
  semantic-lex-comment-regex
  (save-excursion
    (forward-comment 1)
    ;; Generate newline token if enabled
    (if (bolp) (backward-char 1))
    (setq end-point (point))
    ;; Language wants comments or want them as whitespaces,
    ;; link them together.
    (if (eq (car (car token-stream)) 'comment)
	(setcdr (cdr (car token-stream)) end-point)
      (semantic-lex-token 'comment
			  (match-beginning 0)
			  end-point))))

(define-lex-regex-analyzer semantic-lex-comments-as-whitespace
  "Detect comments and create a whitespace token."
  semantic-lex-comment-regex
  (save-excursion
    (forward-comment 1)
    ;; Generate newline token if enabled
    (if (bolp) (backward-char 1))
    (setq end-point (point))
    ;; Language wants comments or want them as whitespaces,
    ;; link them together.
    (if (eq (car (car token-stream)) 'whitespace)
	(setcdr (cdr (car token-stream)) end-point)
      (semantic-lex-token 'whitespace
			  (match-beginning 0)
			  end-point))))

(define-lex-regex-analyzer semantic-lex-ignore-comments
  "Detect and create a comment token."
  semantic-lex-comment-regex
  (let ((comment-start-point (point)))
    (forward-comment 1)
    (if (eq (point) comment-start-point)
	;; In this case our start-skip string failed
	;; to work properly.  Lets try and move over
	;; whatever white space we matched to begin
	;; with.
	(skip-syntax-forward "-.'"
			     (save-excursion
			       (end-of-line)
			       (point)))
      ;; We may need to back up so newlines or whitespace is generated.
      (if (bolp)
	  (backward-char 1)))
    (if (eq (point) comment-start-point)
	(error "Strange comment syntax prevents lexical analysis"))
    (setq end-point (point))))

;;; Test Lexer
;;
(define-lex semantic-simple-lexer
  "A simple lexical analyzer that handles simple buffers.
This lexer ignores comments and whitespace, and will return
syntax as specified by the syntax table."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-number
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(provide 'semantic-lex)

;;; semantic-lex.el ends here
