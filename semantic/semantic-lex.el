;;; semantic-lex.el --- Lexical Analyzer builder

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-lex.el,v 1.10 2002/09/05 13:28:18 ponced Exp $

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

(require 'semantic-fw)
;;; Code:

;;; Compatibility
;;
(eval-and-compile (if (not (fboundp 'with-syntax-table))

;; Copied from Emacs 21 for compatibility with released Emacses.
(defmacro with-syntax-table (table &rest body)
  "With syntax table of current buffer set to a copy of TABLE, evaluate BODY.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table (copy-syntax-table ,table))
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

))

;;; Semantic 2.x lexical analysis
;;
(defun semantic-lex-map-symbols (fun table &optional property)
  "Call function FUN on every symbol in TABLE.
If optional PROPERTY is non-nil, call FUN only on every symbol which
as a PROPERTY value.  FUN receives a symbol as argument."
  (if (arrayp table)
      (mapatoms
       #'(lambda (symbol)
           (if (or (null property) (get symbol property))
               (funcall fun symbol))))
    table))

;;; Keyword table handling.
;;
(defvar semantic-flex-keywords-obarray nil
  "Buffer local keyword obarray for the lexical analyzer.
These keywords are matched explicitly, and converted into special symbols.")
(make-variable-buffer-local 'semantic-flex-keywords-obarray)

(defmacro semantic-lex-keyword-invalid (name)
  "Signal that NAME is an invalid keyword name."
  `(signal 'wrong-type-argument '(semantic-lex-keyword-p ,name)))

(defsubst semantic-lex-keyword-symbol (name)
  "Return keyword symbol with NAME or nil if not found.
Return nil otherwise."
  (and (arrayp semantic-flex-keywords-obarray)
       (stringp name)
       (intern-soft name semantic-flex-keywords-obarray)))

(defsubst semantic-lex-keyword-p (name)
  "Return non-nil if a keyword with NAME exists in the keyword table.
Return nil otherwise."
  (and (setq name (semantic-lex-keyword-symbol name))
       (symbol-value name)))

(defsubst semantic-lex-keyword-set (name value)
  "Set value of keyword with NAME to VALUE and return VALUE."
  (set (intern name semantic-flex-keywords-obarray) value))

(defsubst semantic-lex-keyword-value (name)
  "Return value of keyword with NAME.
Signal an error if a keyword with NAME does not exist."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (symbol-value keyword)
      (semantic-lex-keyword-invalid name))))

(defsubst semantic-lex-keyword-put (name property value)
  "For keyword with NAME, set its PROPERTY to VALUE."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (put keyword property value)
      (semantic-lex-keyword-invalid name))))

(defsubst semantic-lex-keyword-get (name property)
  "For keyword with NAME, return its PROPERTY value."
  (let ((keyword (semantic-lex-keyword-symbol name)))
    (if keyword
        (get keyword property)
      (semantic-lex-keyword-invalid name))))

(defun semantic-lex-make-keyword-table (specs &optional propspecs)
  "Convert keyword SPECS into an obarray and return it.
SPECS must be a list of (NAME . TOKSYM) elements, where:

  NAME is the name of the keyword symbol to define.
  TOKSYM is the lexical token symbol of that keyword.

If optional argument PROPSPECS is non nil, then interpret it, and
apply those properties.
PROPSPECS must be a list of (NAME PROPERTY VALUE) elements."
  ;; Create the symbol hash table
  (let ((semantic-flex-keywords-obarray (make-vector 13 0))
        spec)
    ;; fill it with stuff
    (while specs
      (setq spec  (car specs)
            specs (cdr specs))
      (semantic-lex-keyword-set (car spec) (cdr spec)))
    ;; Apply all properties
    (while propspecs
      (setq spec (car propspecs)
            propspecs (cdr propspecs))
      (semantic-lex-keyword-put (car spec) (nth 1 spec) (nth 2 spec)))
    semantic-flex-keywords-obarray))

(defsubst semantic-lex-map-keywords (fun &optional property)
  "Call function FUN on every semantic keyword.
If optional PROPERTY is non-nil, call FUN only on every keyword which
as a PROPERTY value.  FUN receives a semantic keyword as argument."
  (semantic-lex-map-symbols
   fun semantic-flex-keywords-obarray property))

(defun semantic-lex-keywords (&optional property)
  "Return a list of semantic keywords.
If optional PROPERTY is non-nil, return only keywords which have a
PROPERTY set."
  (let (keywords)
    (semantic-lex-map-keywords
     #'(lambda (symbol) (cons symbol keywords)) property)
    keywords))

;;; Token table handling.
;;
(defvar semantic-lex-tokens-obarray nil
  "Buffer local token obarray for the lexical analyzer.")
(make-variable-buffer-local 'semantic-lex-tokens-obarray)

(defmacro semantic-lex-token-invalid (class)
  "Signal that CLASS is an invalid token class name."
  `(signal 'wrong-type-argument '(semantic-lex-token-class-p ,class)))

(defsubst semantic-lex-token-symbol (class)
  "Return token symbol with CLASS or nil if not found.
Return nil otherwise."
  (and (arrayp semantic-lex-tokens-obarray)
       (stringp class)
       (intern-soft class semantic-lex-tokens-obarray)))

(defsubst semantic-lex-token-class-p (class)
  "Return non-nil if a token with CLASS name exists.
Return nil otherwise."
  (and (setq class (semantic-lex-token-symbol class))
       (symbol-value class)))

(defsubst semantic-lex-token-set (class value)
  "Set value of token with CLASS name to VALUE and return VALUE."
  (set (intern class semantic-lex-tokens-obarray) value))

(defsubst semantic-lex-token-value (class &optional noerror)
  "Return value of token with CLASS name.
If optional argument NOERROR is non-nil return nil if a token with
CLASS name does not exist.  Otherwise signal an error."
  (let ((token (semantic-lex-token-symbol class)))
    (if token
        (symbol-value token)
      (unless noerror
        (semantic-lex-token-invalid class)))))

(defsubst semantic-lex-token-put (class property value &optional add)
  "For token with CLASS name, set its PROPERTY to VALUE.
If optional argument ADD is non-nil, create a new token with CLASS
name if it does not already exist.  Otherwise signal an error."
  (let ((token (semantic-lex-token-symbol class)))
    (unless token
      (or add (semantic-lex-token-invalid class))
      (semantic-lex-token-set class nil)
      (setq token (semantic-lex-token-symbol class)))
    (put token property value)))

(defsubst semantic-lex-token-get (class property &optional noerror)
  "For token with CLASS name, return its PROPERTY value.
If optional argument NOERROR is non-nil return nil if a token with
CLASS name does not exist.  Otherwise signal an error."
  (let ((token (semantic-lex-token-symbol class)))
    (if token
        (get token property)
      (unless noerror
        (semantic-lex-token-invalid class)))))

(defun semantic-lex-make-token-table (specs &optional propspecs)
  "Convert token SPECS into an obarray and return it.
SPECS must be a list of (CLASS . TOKENS) elements, where:

  CLASS is the name of the token class symbol to define.
  TOKENS is an list of (TOKSYM . MATCHER) elements, where:

    TOKSYM is any lexical token symbol.
    MATCHER is a string or regexp a text must match to be a such
    lexical token.

If optional argument PROPSPECS is non nil, then interpret it, and
apply those properties.
PROPSPECS must be a list of (CLASS PROPERTY VALUE)."
  ;; Create the symbol hash table
  (let* ((semantic-lex-tokens-obarray (make-vector 13 0))
         spec class tokens token alist default)
    ;; fill it with stuff
    (while specs
      (setq spec   (car specs)
            specs  (cdr specs)
            class  (car spec)
            tokens (cdr spec))
      (while tokens
        (setq token  (car tokens)
              tokens (cdr tokens))
        (if (cdr token)
            (setq alist (cons token alist))
          (setq token (car token))
          (if default
              (message
               "*** `%s' default matching spec %S redefined as %S"
               class default token))
          (setq default token)))
      ;; Ensure the default matching spec is the first one.
      (semantic-lex-token-set class (cons default (nreverse alist))))
    ;; Apply all properties
    (while propspecs
      (setq spec (car propspecs)
            propspecs (cdr propspecs))
      (semantic-lex-token-put (car spec) (nth 1 spec) (nth 2 spec)))
    semantic-lex-tokens-obarray))

(defsubst semantic-lex-map-tokens (fun &optional property)
  "Call function FUN on every lexical token class.
If optional PROPERTY is non-nil, call FUN only on every token which
as a PROPERTY value.  FUN receives a semantic token class symbol as argument."
  (semantic-lex-map-symbols
   fun semantic-lex-tokens-obarray property))

(defun semantic-lex-tokens (&optional property)
  "Return a list of lexical token class symbols.
If optional PROPERTY is non-nil, return only tokens which have a
PROPERTY set."
  (let (tokens)
    (semantic-lex-map-tokens
     #'(lambda (symbol) (cons symbol tokens)) property)
    tokens))

;;;###autoload
(defvar semantic-lex-analyzer 'semantic-flex
  "The lexical analyzer used for a given buffer.
See `semantic-lex' for documentation.
For compatibility with Semantic 1.x it defaults to `semantic-flex'.")
(make-variable-buffer-local 'semantic-lex-analyzer)

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

(defvar semantic-lex-syntax-table nil
  "Syntax table used by lexical analysis.
See also `semantic-lex-syntax-modifications'.")
(make-variable-buffer-local 'semantic-lex-syntax-table)

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

(defvar semantic-lex-depth 0
  "Default lexing depth.
This specifies how many lists to create tokens in.")
(make-variable-buffer-local 'semantic-lex-depth)

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

(defmacro semantic-lex-one-token (analyzers)
  "Calculate one token from the current buffer at point.
Uses locally bound variables from `define-lex'.
Argument ANALYZERS is the list of analyzers being used."
  (cons 'cond (mapcar #'symbol-value analyzers)))

;;;###autoload
(defmacro define-lex (name doc &rest analyzers)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
ANALYZERS are small code snippets of analyzers to use when
building the new NAMED analyzer.  Only use analyzers which
are written to be used in `define-lex'.
Each analyzer should be an analyzer created with `define-lex-analyzer'."
  `(defun ,name  (start end &optional depth length)
     ,(concat doc "\nSee `semantic-lex' for more information.")
     (let* ((starting-position (point))
            (token-stream nil)
            (end-point start)
            (current-depth 0)
            ;; Use the default depth when not specified.
            (depth (or depth semantic-lex-depth)))
       ;; Maybe REMOVE THIS LATER.
       ;; Trying to find incremental parser bug.
       (if (> end (point-max))
	   (error "Lex: End = %d, but point-max = %d" end (point-max)))
       (with-syntax-table semantic-lex-syntax-table
         (goto-char start)
         (while (and (< (point) end)
                     (or (not length) (<= (length token-stream) length)))
           (semantic-lex-one-token ,analyzers)
           (goto-char end-point)))
       ;; Return to where we started.
       ;; Do not wrap in protective stuff so that if there is an error
       ;; thrown, the user knows where.
       (goto-char starting-position)
       ;; Return the token stream
       (nreverse token-stream))))

;;; Lexical token API
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

(defsubst semantic-lex-token-class (token)
  "Fetch the class of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (car token))

(defsubst semantic-lex-token-bounds (token)
  "Fetch the start and end locations of the lexical token TOKEN.
Return a pair (START . END)."
  (cdr token))

(defsubst semantic-lex-token-start (token)
  "Fetch the start position of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (car (semantic-lex-token-bounds token)))

(defsubst semantic-lex-token-end (token)
  "Fetch the end position of the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (cdr (semantic-lex-token-bounds token)))

(defsubst semantic-lex-token-text (token)
  "Fetch the text associated with the lexical token TOKEN.
See also the function `semantic-lex-token'."
  (buffer-substring-no-properties
   (semantic-lex-token-start token)
   (semantic-lex-token-end   token)))

;;;###autoload
(defun semantic-lex-init ()
  "Initialize any lexical state for this buffer."
  (when (not semantic-lex-comment-regex)
    (setq semantic-lex-comment-regex
	  (if comment-start-skip
	      (concat "\\(\\s<\\|" comment-start-skip "\\)")
	    (concat "\\(\\s<\\)"))
	  ))
  ;; Setup the lexer syntax-table
  (setq semantic-lex-syntax-table (copy-syntax-table (syntax-table)))
  (let* ((mods semantic-lex-syntax-modifications)
	 mod)
	 (while mods
	   (setq mod  (car mods)
		 mods (cdr mods))
	   (modify-syntax-entry
	    (car mod) (nth 1 mod) semantic-lex-syntax-table))))

;;;###autoload
(define-overload semantic-lex (start end &optional depth length)
  "Lexically analyze text in the current buffer between START and END.
Optional argument DEPTH indicates at what level to scan over entire
lists.  The last argument, LENGTH specifies that `semantic-lex'
should only return LENGTH tokens.  The return value is a token stream.
Each element is a list, such of the form
  (symbol start-expression .  end-expression)
where SYMBOL denotes the token type.
See `semantic-lex-tokens' variable for details on token types.  END
does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'."
  (funcall semantic-lex-analyzer start end depth length))

(defsubst semantic-lex-buffer (&optional depth)
  "Lex the current buffer.
Optional argument DEPTH is the depth to scan into lists."
  (semantic-lex (point-min) (point-max) depth))

(defsubst semantic-lex-list (semlist depth)
  "Lex the body of SEMLIST to DEPTH."
  (semantic-lex (semantic-lex-token-start semlist)
                (semantic-lex-token-end   semlist)
                depth))

;;; Analyzer creation macros
;;

;;;###autoload
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
  `(eval-and-compile
     (defvar ,name nil ,doc)
     ;; Do this part separately so that re-evaluation rebuilds this code.
     (setq ,name '(,condition ,@forms))
     ))

;;;###autoload
(defmacro define-lex-regex-analyzer (name doc regexp &rest forms)
  "Create a lexical analyzer with NAME and DOC  that match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers."
  `(eval-and-compile
     (defvar ,name nil ,doc)
     (setq ,name  '((looking-at ,regexp) ,@forms))))

;;;###autoload
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
  `(eval-and-compile
     (defvar ,name nil ,doc)
     (setq ,name
	   '((looking-at ,regexp)
	     ,@forms
	     (semantic-lex-token ,toksym
				 (match-beginning ,(or index 0))
				 (match-end ,(or index 0)))))
     ))

;;;###autoload
(defmacro define-lex-block-analyzer (name doc spec1 &rest specs)
  "Create a lexical analyzer NAME for paired delimiters blocks.
It detects a paired delimiters block or the corresponding open or
close delimiter depending on the value of the variable
`current-depth'.  DOC is the documentation string of the lexical
analyzer.  SPEC1 and SPECS specify the token symbols and open, close
delimiters used.  Each SPEC has the form:

\(BLOCK-SYM (OPEN-DELIM OPEN-SYM) (CLOSE-DELIM CLOSE-SYM))
 
where BLOCK-SYM is the symbol returned in a block token.  OPEN-DELIM
and CLOSE-DELIM are respectively the open and close delimiters
identifying a block.  OPEN-SYM and CLOSE-SYM are respectively the
symbols returned in open and close tokens."
  (let ((specs (cons spec1 specs))
        spec open olist clist)
    (while specs
      (setq spec  (car specs)
            specs (cdr specs)
            open  (nth 1 spec)
            ;; build alist ((OPEN-DELIM OPEN-SYM BLOCK-SYM) ...)
            olist (cons (list (car open) (cadr open) (car spec)) olist)
            ;; build alist ((CLOSE-DELIM CLOSE-SYM) ...)
            clist (cons (nth 2 spec) clist)))
    `(eval-and-compile
       (defvar ,name nil ,doc)
       (setq ,name
             '((and
                (looking-at "\\(\\s(\\|\\s)\\)")
                (let ((text (match-string 0)) match)
                  (cond
                   ((setq match (assoc text ',olist))
                    (if (or (not depth) (< current-depth depth))
                        (progn
                          (setq current-depth (1+ current-depth))
                          (semantic-lex-token
                           (nth 1 match)
                           (match-beginning 0) (match-end 0)))
                      (semantic-lex-token
                       (nth 2 match)
                       (match-beginning 0)
                       (save-excursion
                         (condition-case nil
                             (forward-list 1)
                           ;; This case makes lex robust to broken lists.
                           (error
                            (goto-char
                             (funcall
                              semantic-lex-unterminated-syntax-end-function
                              (nth 2 match) start end))))
                         (setq end-point (point))))))
                   ((setq match (assoc text ',clist))
                    (setq current-depth (1- current-depth))
                    (semantic-lex-token
                     (nth 1 match)
                     (match-beginning 0) (match-end 0)))))))
             ))))

;;; Analyzers
;;
(define-lex-analyzer semantic-lex-default-action
  "The default action when no other lexical actions match text.
This action will just throw an error."
  t
  (error "Unmatched Text during Lexical Analysis"))

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
  "\\s-*\\(\n\\|\\s>\\)"  'newline 1)

(define-lex-regex-analyzer semantic-lex-newline-as-whitespace
  "Detect and create newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters) AND when you want whitespace tokens."
  "\\s-*\\(\n\\|\\s>\\)"  
  ;; Language wants whitespaces, link them together.
  (if (eq (semantic-lex-token-class (car token-stream)) 'whitespace)
      (setcdr (semantic-lex-token-bounds (car token-stream))
              (match-end 0))
    (semantic-lex-token
     'whitespace (match-beginning 0) (match-end 0))))

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
  (if (eq (semantic-lex-token-class (car token-stream)) 'whitespace)
      (setcdr (semantic-lex-token-bounds (car token-stream))
              (match-end 0))
    (semantic-lex-token
     'whitespace (match-beginning 0) (match-end 0))))

(define-lex-regex-analyzer semantic-lex-ignore-whitespace
  "Detect and skip over whitespace tokens."
  ;; catch whitespace when needed
  "\\s-+"
  ;; Skip over the detected whitespace.
  (setq end-point (match-end 0)))

(define-lex-simple-regex-analyzer semantic-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'number)

(define-lex-regex-analyzer semantic-lex-symbol-or-keyword
  "Detect and create symbol and keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-token
   (or (semantic-lex-keyword-p (match-string 0)) 'symbol)
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
	(semantic-lex-token
	 'open-paren (match-beginning 0) (match-end 0)))
    (semantic-lex-token
     'semantic-list (match-beginning 0)
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
  (semantic-lex-token
   'string (point)
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
    (if (eq (semantic-lex-token-class (car token-stream)) 'comment)
	(setcdr (semantic-lex-token-bounds (car token-stream))
		end-point)
      (semantic-lex-token
       'comment (match-beginning 0) end-point))))

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
    (if (eq (semantic-lex-token-class (car token-stream)) 'whitespace)
	(setcdr (semantic-lex-token-bounds (car token-stream))
		end-point)
      (semantic-lex-token
       'whitespace (match-beginning 0) end-point))))

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

;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()
     
     (def-edebug-spec define-lex
       (&define name stringp (&rest symbolp))
       )
     (def-edebug-spec define-lex-analyzer
       (&define name stringp form def-body)
       )
     (def-edebug-spec define-lex-regex-analyzer
       (&define name stringp form def-body)
       )
     (def-edebug-spec define-lex-simple-regex-analyzer
       (&define name stringp form symbolp [ &optional form ] def-body)
       )
     (def-edebug-spec define-lex-block-analyzer
       (&define name stringp form (&rest form))
       )
     
     ))

;;; Compatibility with Semantic 1.x lexical analysis
;;

(defalias 'semantic-flex-start 'semantic-lex-token-start)
(defalias 'semantic-flex-end 'semantic-lex-token-end)
(defalias 'semantic-flex-text 'semantic-lex-token-text)
(defalias 'semantic-flex-make-keyword-table 'semantic-lex-make-keyword-table)
(defalias 'semantic-flex-keyword-p 'semantic-lex-keyword-p)
(defalias 'semantic-flex-keyword-put 'semantic-lex-keyword-put)
(defalias 'semantic-flex-keyword-get 'semantic-lex-keyword-get)
(defalias 'semantic-flex-map-keywords 'semantic-lex-map-keywords)
(defalias 'semantic-flex-keywords 'semantic-lex-keywords)
(defalias 'semantic-flex-buffer 'semantic-lex-buffer)
(defalias 'semantic-flex-list   'semantic-lex-list)

;; This simple scanner uses the syntax table to generate a stream of
;; simple tokens of the form:
;;
;;  (SYMBOL START . END)
;;
;; Where symbol is the type of thing it is.  START and END mark that
;; objects boundary.

(defvar semantic-flex-tokens semantic-lex-tokens
  "An alist of of semantic token types.
See variable `semantic-lex-tokens'.")

(defvar semantic-flex-unterminated-syntax-end-function
  (lambda (syntax syntax-start flex-end) flex-end)
  "Function called when unterminated syntax is encountered.
This should be set to one function.  That function should take three
parameters.  The SYNTAX, or type of syntax which is unterminated.
SYNTAX-START where the broken syntax begins.
FLEX-END is where the lexical analysis was asked to end.
This function can be used for languages that can intelligently fix up
broken syntax, or the exit lexical analysis via `throw' or `signal'
when finding unterminated syntax.")

(defvar semantic-flex-extensions nil
  "Buffer local extensions to the lexical analyzer.
This should contain an alist with a key of a regex and a data element of
a function.  The function should both move point, and return a lexical
token of the form:
  ( TYPE START .  END)
nil is also a valid return value.
TYPE can be any type of symbol, as long as it doesn't occur as a
nonterminal in the language definition.")
(make-variable-buffer-local 'semantic-flex-extensions)

(defvar semantic-flex-syntax-modifications nil
  "Changes to the syntax table for this buffer.
These changes are active only while the buffer is being flexed.
This is a list where each element has the form:
  (CHAR CLASS)
CHAR is the char passed to `modify-syntax-entry',
and CLASS is the string also passed to `modify-syntax-entry' to define
what syntax class CHAR has.")
(make-variable-buffer-local 'semantic-flex-syntax-modifications)

(defvar semantic-ignore-comments t
  "Default comment handling.
t means to strip comments when flexing.  Nil means to keep comments
as part of the token stream.")
(make-variable-buffer-local 'semantic-ignore-comments)

(defvar semantic-flex-enable-newlines nil
  "When flexing, report 'newlines as syntactic elements.
Useful for languages where the newline is a special case terminator.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-newlines)

(defvar semantic-flex-enable-whitespace nil
  "When flexing, report 'whitespace as syntactic elements.
Useful for languages where the syntax is whitespace dependent.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-whitespace)

(defvar semantic-flex-enable-bol nil
  "When flexing, report beginning of lines as syntactic elements.
Useful for languages like python which are indentation sensitive.
Only set this on a per mode basis, not globally.")
(make-variable-buffer-local 'semantic-flex-enable-bol)

(defvar semantic-number-expression semantic-lex-number-expression
  "See variable `semantic-lex-number-expression'.")
(make-variable-buffer-local 'semantic-number-expression)

(defvar semantic-flex-depth 0
  "Default flexing depth.
This specifies how many lists to create tokens in.")
(make-variable-buffer-local 'semantic-flex-depth)

(defun semantic-flex (start end &optional depth length)
  "Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element is a list, such of
the form (symbol start-expression .  end-expression) where SYMBOL
denotes the token type.
See `semantic-flex-tokens' variable for details on token types.
END does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.
The last argument, LENGTH specifies that `semantic-flex' should only
return LENGTH tokens."
  ;;(message "Flexing muscles...")
  (if (not semantic-flex-keywords-obarray)
      (setq semantic-flex-keywords-obarray [ nil ]))
  (let ((ts nil)
        (pos (point))
        (ep nil)
        (curdepth 0)
        (cs (if comment-start-skip
                (concat "\\(\\s<\\|" comment-start-skip "\\)")
              (concat "\\(\\s<\\)")))
        (newsyntax (copy-syntax-table (syntax-table)))
        (mods semantic-flex-syntax-modifications)
        ;; Use the default depth if it is not specified.
        (depth (or depth semantic-flex-depth)))
    ;; Update the syntax table
    (while mods
      (modify-syntax-entry (car (car mods)) (car (cdr (car mods))) newsyntax)
      (setq mods (cdr mods)))
    (with-syntax-table newsyntax
      (goto-char start)
      (while (and (< (point) end) (or (not length) (<= (length ts) length)))
        (cond
         ;; catch beginning of lines when needed.
         ;; Must be done before catching any other tokens!
         ((and semantic-flex-enable-bol
               (bolp)
               ;; Just insert a (bol N . N) token in the token stream,
               ;; without moving the point.  N is the point at the
               ;; beginning of line.
               (setq ts (cons (cons 'bol (cons (point) (point))) ts))
               nil)) ;; CONTINUE
         ;; special extensions, includes whitespace, nl, etc.
         ((and semantic-flex-extensions
               (let ((fe semantic-flex-extensions)
                     (r nil))
                 (while fe
                   (if (looking-at (car (car fe)))
                       (setq ts (cons (funcall (cdr (car fe))) ts)
                             r t
                             fe nil
                             ep (point)))
                   (setq fe (cdr fe)))
                 (if (and r (not (car ts))) (setq ts (cdr ts)))
                 r)))
         ;; catch newlines when needed
         ((looking-at "\\s-*\\(\n\\|\\s>\\)")
          (if semantic-flex-enable-newlines
              (setq ep (match-end 1)
                    ts (cons (cons 'newline
                                   (cons (match-beginning 1) ep))
                             ts))))
         ;; catch whitespace when needed
         ((looking-at "\\s-+")
          (if semantic-flex-enable-whitespace
              ;; Language wants whitespaces, link them together.
              (if (eq (car (car ts)) 'whitespace)
                  (setcdr (cdr (car ts)) (match-end 0))
                (setq ts (cons (cons 'whitespace
                                     (cons (match-beginning 0)
                                           (match-end 0)))
                               ts)))))
         ;; numbers
         ((and semantic-number-expression
               (looking-at semantic-number-expression))
          (setq ts (cons (cons 'number
                               (cons (match-beginning 0)
                                     (match-end 0)))
                         ts)))
         ;; symbols
         ((looking-at "\\(\\sw\\|\\s_\\)+")
          (setq ts (cons (cons
                          ;; Get info on if this is a keyword or not
                          (or (semantic-flex-keyword-p (match-string 0))
                              'symbol)
                          (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Character quoting characters (ie, \n as newline)
         ((looking-at "\\s\\+")
          (setq ts (cons (cons 'charquote
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; Open parens, or semantic-lists.
         ((looking-at "\\s(")
          (if (or (not depth) (< curdepth depth))
              (progn
                (setq curdepth (1+ curdepth))
                (setq ts (cons (cons 'open-paren
                                     (cons (match-beginning 0) (match-end 0)))
                               ts)))
            (setq ts (cons
                      (cons 'semantic-list
                            (cons (match-beginning 0)
                                  (save-excursion
                                    (condition-case nil
                                        (forward-list 1)
                                      ;; This case makes flex robust
                                      ;; to broken lists.
                                      (error
                                       (goto-char
                                        (funcall
                                         semantic-flex-unterminated-syntax-end-function
                                         'semantic-list
                                         start end))))
                                    (setq ep (point)))))
                      ts))))
         ;; Close parens
         ((looking-at "\\s)")
          (setq ts (cons (cons 'close-paren
                               (cons (match-beginning 0) (match-end 0)))
                         ts))
          (setq curdepth (1- curdepth)))
         ;; String initiators
         ((looking-at "\\s\"")
          ;; Zing to the end of this string.
          (setq ts (cons (cons 'string
                               (cons (match-beginning 0)
                                     (save-excursion
                                       (condition-case nil
                                           (forward-sexp 1)
                                         ;; This case makes flex
                                         ;; robust to broken strings.
                                         (error
                                          (goto-char
                                           (funcall
                                            semantic-flex-unterminated-syntax-end-function
                                            'string
                                            start end))))
                                       (setq ep (point)))))
                         ts)))
         ;; comments
         ((looking-at cs)
          (if (and semantic-ignore-comments
                   (not semantic-flex-enable-whitespace))
              ;; If the language doesn't deal with comments nor
              ;; whitespaces, ignore them here.
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
                  ;;(forward-comment 1)
                  ;; Generate newline token if enabled
                  (if (and semantic-flex-enable-newlines
                           (bolp))
                      (backward-char 1)))
                (if (eq (point) comment-start-point)
                    (error "Strange comment syntax prevents lexical analysis"))
                (setq ep (point)))
            (let ((tk (if semantic-ignore-comments 'whitespace 'comment)))
              (save-excursion
                (forward-comment 1)
                ;; Generate newline token if enabled
                (if (and semantic-flex-enable-newlines
                         (bolp))
                    (backward-char 1))
                (setq ep (point)))
              ;; Language wants comments or want them as whitespaces,
              ;; link them together.
              (if (eq (car (car ts)) tk)
                  (setcdr (cdr (car ts)) ep)
                (setq ts (cons (cons tk (cons (match-beginning 0) ep))
                               ts))))))
         ;; punctuation
         ((looking-at "\\(\\s.\\|\\s$\\|\\s'\\)")
          (setq ts (cons (cons 'punctuation
                               (cons (match-beginning 0) (match-end 0)))
                         ts)))
         ;; unknown token
         (t
          (error "What is that?")))
        (goto-char (or ep (match-end 0)))
        (setq ep nil)))
    ;; maybe catch the last beginning of line when needed
    (and semantic-flex-enable-bol
         (= (point) end)
         (bolp)
         (setq ts (cons (cons 'bol (cons (point) (point))) ts)))
    (goto-char pos)
    ;;(message "Flexing muscles...done")
    (nreverse ts)))

(provide 'semantic-lex)

;;; semantic-lex.el ends here
