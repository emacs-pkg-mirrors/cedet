;;; wisent-bovine.el --- Wisent - Semantic gateway

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-bovine.el,v 1.27 2003/03/14 08:18:01 ponced Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Here are functions necessary to use the Wisent LALR parser from
;; Semantic environment.

;;; History:
;; 

;;; Code:

(require 'semantic)
(require 'wisent)

;;; Lexical analysis
;;

(defvar wisent-lex-istream nil
  "Input stream of `semantic-lex' syntactic tokens.")

(defvar wisent-lex-lookahead nil
  "Extra lookahead token.
When non-nil it is directly returned by `wisent-lex-function'.")

(defsubst wisent-lex-token-rules (symbol)
  "Return matching rules of token class SYMBOL."
  (semantic-lex-type-value (symbol-name symbol) 'noerror))

(defsubst wisent-lex-token-get (symbol property)
  "For token class SYMBOL, return its PROPERTY value or nil."
  (semantic-lex-type-get (symbol-name symbol) property 'noerror))

(defun wisent-lex-make-token-table (specs &optional propspecs)
  "Convert token SPECS into an obarray and return it.
If optional argument PROPSPECS is non nil, then interpret it, and
apply those properties (see `semantic-lex-make-type-table' for
details)."
  (let ((semantic-lex-types-obarray
         (semantic-lex-make-type-table specs propspecs)))
    ;; Set up some useful default properties
    (semantic-lex-type-put "punctuation" 'char-literal t 'add)
    (semantic-lex-type-put "open-paren"  'char-literal t 'add)
    (semantic-lex-type-put "close-paren" 'char-literal t 'add)
    semantic-lex-types-obarray))

(defsubst wisent-lex-match (text default rules &optional usequal)
  "Return lexical symbol matching TEXT or DEFAULT if not found.
RULES is an alist of (TOKEN . MATCHER).  If optional argument USEQUAL
is non-nil use direct string comparison between TEXT and MATCHERs
instead of regexp match."
  (if usequal
      (or (car (rassoc text rules)) default)
    (let* (lexem regex)
      (while (and (not lexem) rules)
        (if (or (null (setq regex (cdar rules)))
                (string-match regex text))
            (setq lexem (caar rules))
          (setq rules (cdr rules))))
      (or lexem default))))

;;; Semantic 2.x lexical analysis
;;

;; Lexer creation macros
;;
(defmacro wisent-lex-eoi ()
  "Return an End-Of-Input lexical token.
The EOI token is like this: ($EOI "" POINT-MAX . POINT-MAX)."
  `(cons ',wisent-eoi-term
         (cons ""
               (cons (point-max) (point-max)))))

(defmacro define-wisent-lexer (name doc &rest body)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
When a token is available in `wisent-lex-istream', eval BODY forms
sequentially.  BODY must return a lexical token for the LALR parser.

Each token in input was produced by `semantic-lex', it is a list:

  (TOKSYM START . END)

TOKSYM is a terminal symbol used in the grammar.
START and END mark boundary in the current buffer of that token's
value.

Returned tokens must have the form:

  (TOKSYM VALUE START . END)

where VALUE is the buffer substring between START and END positions."
  `(defun
     ,name () ,doc
     (cond
      (wisent-lex-lookahead
       (prog1 wisent-lex-lookahead
         (setq wisent-lex-lookahead nil)))
      (wisent-lex-istream
       ,@body)
      ((wisent-lex-eoi)))))

;;; General purpose lexers
;;

(define-wisent-lexer wisent-flex
  "Return the next available lexical token in Wisent's form.
Eat syntactic tokens produced by `semantic-lex', available in
variable `wisent-lex-istream', and return Wisent's lexical tokens.
See documentation of `semantic-lex-tokens' for details on the
syntactic tokens returned by `semantic-lex'.

In most cases one syntactic token is mapped to one lexical token.  But
in certain cases several successive syntactic tokens can be mapped to
one lexical tokens.  A common case is given by arithmetic operators
which can be made of multiple punctuations.

Also the mapping between syntactic tokens and lexical ones uses regexp
match by default, but can use string comparison too.

The rules specifying how to do the mapping are defined in two symbol
tables:

  - The keyword table in variable `semantic-lex-keywords-obarray';

  - The token table in variable `semantic-lex-tokens-obarray'.

Keywords are directly mapped to equivalent Wisent's lexical tokens
like this (SL- prefix means `semantic-lex', WL- `wisent-lex'):

  (SL-KEYWORD start . end)  ->  (WL-KEYWORD \"name\" start . end)

Mapping of other tokens obeys to rules in the token table.  Here is an
example on how to define the mapping of 'punctuation syntactic tokens.

1. Add (`intern') the symbol 'punctuation into the token table.

2. Set its value to the mapping rules to use.  Mapping rules are an
   alist of (WL-TOKEN . MATCHER) elements.  WL-TOKEN is the category
   of the Wisent's lexical token (for example 'OPERATOR).  MATCHER is
   the regular expression used to filter input data (for example
   \"[+-]\").  The first element of the mapping rule alist defines a
   default matching rule. It must be nil or have the form (WL-TOKEN).
   When there is no mapping rule that matches the syntactic token
   value, the default WL-TOKEN or nil is returned.

   Thus, if the syntactic token symbol 'punctuation has the mapping
   rules '(nil (OPERATOR . \"[+-]\")), the following token:

   (punctuation 1 . 2)

   will be mapped to the lexical token

   (OPERATOR \"+\" 1 . 2)

   if the buffer contained \"+\" between positions 1 and 2.

   To define multiple matchers for the same WL-TOKEN just give
   several (WL-TOKEN . MATCHER) values.  MATCHERs will be tried in
   sequence until one matches.

3. Optionally customize how `wisent-flex' will interpret mapping
   rules, using symbol properties.

   The following properties are recognized:

   'string
     If non-nil MATCHERs are interpreted as strings instead of
     regexps, and matching uses direct string comparison.  This could
     speed up things in certain cases.

   'multiple
     non-nil indicates to lookup at multiple successive syntactic
     tokens and try to match the longest one.

   'char-literal
     non-nil indicates to return the first character of the syntactic
     token value as the lexical token category.  It is the default for
     punctuation, open-paren and close-paren syntactic tokens.  Use
     this property when grammar contains references to character
     literals.

   'handler
     If non-nil must specify a function with no argument that will be
     called first to map the syntactic token.  It must return a
     lexical token or nil, and update the input stream in variable
     `wisent-lex-istream' accordingly.

   The following example maps multiple punctuations to operators and
   use string comparison:

   (let ((entry (intern 'punctuation token-table)))
     (set entry '(nil ;; No default mapping
                  (LSHIFT . \"<<\") (RSHIFT . \">>\")
                  (LT     .  \"<\") (GT     .  \">\")))
     (put entry 'string   t)
     (put entry 'multiple t))"
  (let* ((is   wisent-lex-istream)
         (flex (car is))
         (stok (semantic-lex-token-class flex))
         (text (semantic-lex-token-text flex))
         default rules usequal wlex term beg end ends n is2)
      
    (if (setq term (semantic-lex-keyword-p text))
       
        ;; Keyword
        ;; -------
        (setq wlex (cons term
                         (cons text
                               (semantic-lex-token-bounds flex)))
              ;; Eat input stream
              wisent-lex-istream (cdr is))
                
        
      ;; Token
      ;; -----
      (if (null (setq rules (wisent-lex-token-rules stok)))
          ;; Eat input stream
          (setq wisent-lex-istream (cdr is))
          
        ;; Map syntactic token following RULES
        (setq default (car rules)
              rules   (cdr rules))
        (cond
           
         ;; If specified try a function first to map token.
         ;; It must return a lexical token or nil and update the
         ;; input stream (`wisent-lex-istream') accordingly.
         ((and (setq n (wisent-lex-token-get stok 'handler))
               (setq wlex (funcall n))))
           
         ;; Several/One mapping
         ((wisent-lex-token-get stok 'multiple)
          (setq beg  (semantic-lex-token-start flex)
                end  (semantic-lex-token-end   flex)
                ends (list end)
                n    1
                is2  (cdr is)
                flex (car is2))
          ;; Collect successive `semantic-lex' tokens
          (while (and (eq (semantic-lex-token-class flex) stok)
                      (= end (semantic-lex-token-start flex)))
            (setq end  (semantic-lex-token-end flex)
                  ends (cons end ends)
                  n    (1+ n)
                  is2  (cdr is2)
                  flex (car is2)))
          ;; Search the longest match
          (setq usequal (wisent-lex-token-get stok 'string))
          (while (and (not wlex) ends)
            (setq end  (car ends)
                  text (buffer-substring-no-properties beg end)
                  term (wisent-lex-match text default rules usequal))
            (if term
                (setq wlex (cons term (cons text (cons beg end)))
                      ;; Eat input stream
                      wisent-lex-istream (nthcdr n is))
              (setq n    (1- n)
                    ends (cdr ends)))))
           
         ;; One/one token mapping
         ((setq usequal (wisent-lex-token-get stok 'string)
                term (wisent-lex-match text default rules usequal))
          (setq wlex (cons term
                           (cons text
                                 (semantic-lex-token-bounds flex)))
                ;; Eat input stream
                wisent-lex-istream (cdr is))))))
      
    ;; Return value found or default one
    (or wlex
        (cons (if (wisent-lex-token-get stok 'char-literal)
                  (aref text 0)
                stok)
              (cons text (semantic-lex-token-bounds flex))))))

(define-wisent-lexer wisent-lex
  "Return the next available lexical token in Wisent's form.
The variable `wisent-lex-istream' contains the list of lexical tokens
produced by `semantic-lex'.  Pop the next token available and convert
it to a form suitable for the Wisent's parser."
  (let* ((tk (car wisent-lex-istream)))
    ;; Eat input stream
    (setq wisent-lex-istream (cdr wisent-lex-istream))
    (cons (semantic-lex-token-class tk)
          (cons (semantic-lex-token-text tk)
                (semantic-lex-token-bounds tk)))))

;;; Syntax analysis
;;
(defvar wisent-error-function #'ignore
  "Function used to report parse error.")
(make-variable-buffer-local 'wisent-error-function)

(defvar wisent-lexer-function #'wisent-lex
  "Function used to obtain the next lexical token in input.
Should be a lexical analyzer created with `define-wisent-lexer'.")
(make-variable-buffer-local 'wisent-lexer-function)

;; Tag production
;;
(defsubst wisent-token (&rest return-val)
  "Return a raw Semantic token including RETURN-VAL.
Should be used in Semantic actions to build the bovine cache."
  (nconc return-val
         (if (or $region
                 (setq $region (nthcdr 2 wisent-input)))
             (list (car $region) (cdr $region))
           (list (point-max) (point-max)))))

(defmacro wisent-cooked-token (&rest return-val)
  "Return a cooked Semantic token including RETURN-VAL.
Should be used in Semantic actions to build the bovine cache."
  `(let* ((cooked (semantic-raw-to-cooked-token
                   (wisent-token ,@return-val)))
          (l cooked))
     (while l
       (semantic-tag-put (car l) 'reparse-symbol $nterm)
       (setq l (cdr l)))
     cooked))

(defsubst wisent-raw-tag (semantic-tag)
  "Return raw form of given Semantic tag SEMANTIC-TAG.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (nconc semantic-tag
         (if (or $region
                 (setq $region (nthcdr 2 wisent-input)))
             (list (car $region) (cdr $region))
           (list (point-max) (point-max)))))

(defsubst wisent-cook-tag (raw-tag)
  "From raw form of Semantic tag RAW-TAG, return a list of cooked tags.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (let* ((cooked (semantic-raw-to-cooked-token raw-tag))
         (l cooked))
    (while l
      (semantic-tag-put (car l) 'reparse-symbol $nterm)
      (setq l (cdr l)))
    cooked))

;; Unmatched syntax collector
;;
(defun wisent-collect-unmatched-syntax (nomatch)
  "Add lexical token NOMATCH to the cache of unmatched tokens.
See also the variable `semantic-unmatched-syntax-cache'.

NOMATCH is in Wisent's form: (SYMBOL VALUE START . END)
and will be collected in `semantic-lex' form: (SYMBOL START . END)."
  (let ((region (cddr nomatch)))
    (and (number-or-marker-p (car region))
         (number-or-marker-p (cdr region))
         (setq semantic-unmatched-syntax-cache
               (cons (cons (car nomatch) region)
                     semantic-unmatched-syntax-cache)))))

;; Parser plug-ins
;;
;; The following functions permit to plug the Wisent LALR parser in
;; Semantic toolkit.  They use the standard API provided by Semantic
;; to plug parsers in.
;;
;; Two plug-ins are available, BUT ONLY ONE MUST BE USED AT A TIME:
;;
;; - `wisent-parse-stream' designed to override the standard function
;;   `semantic-parse-stream'.
;;
;; - `wisent-parse-region' designed to override the standard function
;;   `semantic-parse-region'.
;;
;; Maybe the latter is faster because it eliminates a lot of function
;; call.
;;
(defun wisent-parse-stream (stream goal)
  "Parse STREAM using the Wisent LALR parser.
GOAL is a nonterminal symbol to start parsing at.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found.
The LALR parser automaton must be available in buffer local variable
`semantic-toplevel-bovine-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-stream'."
  (let (wisent-lex-istream wisent-lex-lookahead la-elt cache)
    
    ;; IMPLEMENTATION NOTES:
    ;; `wisent-parse' returns a lookahead token when it stopped
    ;; parsing before encountering the end of input.  To re-enter the
    ;; parser it is necessary to push back in the lexical input stream
    ;; the last lookahead token issued.  Because the format of
    ;; lookahead tokens and tokens in STREAM can be different the
    ;; lookahead token is put in the variable `wisent-lex-lookahead'
    ;; before calling `wisent-parse'.  Wisent's lexers always pop the
    ;; next lexical token from that variable when non nil, then from
    ;; the lexical input stream.
    ;;
    ;; The first element of STREAM is used to keep lookahead tokens
    ;; across successive calls to `wisent-parse-stream'.  In fact
    ;; what is kept is a stack of lookaheads encountered so far.  It
    ;; is cleared when `wisent-parse' returns a valid semantic token,
    ;; or twice the same lookahead token!  The latter indicates that
    ;; there is a syntax error on that token.  If so, tokens currently
    ;; in the lookahead stack have not been used, and are moved into
    ;; `semantic-unmatched-syntax-cache'.  When the parser will be
    ;; re-entered, a new lexical token will be read from STREAM.
    ;;
    ;; The first element of STREAM that contains the lookahead stack
    ;; has this format (compatible with the format of `semantic-lex'
    ;; tokens):
    ;;
    ;; (LOOKAHEAD-STACK START . END)
    ;;
    ;; where LOOKAHEAD-STACK is a list of lookahead tokens.  And
    ;; START/END are the bounds of the lookahead at top of stack.
    
    ;; Retrieve lookahead token from stack
    (setq la-elt (car stream))
    (if (consp (car la-elt))
        ;; The first elt of STREAM contains a lookahead stack
        (setq wisent-lex-lookahead (caar la-elt)
              stream (cdr stream))
      (setq la-elt nil))
    ;; Parse
    (setq wisent-lex-istream stream
          cache (condition-case nil
                    (wisent-parse semantic-toplevel-bovine-table
                                  wisent-lexer-function
                                  wisent-error-function
                                  goal)
                  (error nil)))
    ;; Manage returned lookahead token
    (if wisent-lookahead
        (if (eq (caar la-elt) wisent-lookahead)
            ;; It is already at top of lookahead stack
            (progn
              (setq cache nil
                    la-elt (car la-elt))
              (while la-elt
                ;; Collect unmatched tokens from the stack
                (run-hook-with-args
                 'wisent-discarding-token-functions (car la-elt))
                (setq la-elt (cdr la-elt))))
          ;; New lookahead token
          (if (or (consp cache) ;; Clear the stack if parse succeeded
                  (null la-elt))
              (setq la-elt (cons nil nil)))
          ;; Push it into the stack
          (setcar la-elt (cons wisent-lookahead (car la-elt)))
          ;; Update START/END
          (setcdr la-elt (cddr wisent-lookahead))
          ;; Push (LOOKAHEAD-STACK START . END) in STREAM
          (setq wisent-lex-istream (cons la-elt wisent-lex-istream))))
    ;; Return (STREAM SEMANTIC-STREAM)
    (list wisent-lex-istream
          (if (consp cache) cache '(nil))
          )))

(defun wisent-parse-region (start end &optional goal depth returnonerror)
  "Parse the area between START and END using the Wisent LALR parser.
Return the list of semantic tokens found.
Optional arguments GOAL is a nonterminal symbol to start parsing at,
DEPTH is the lexical depth to scan, and RETURNONERROR is a flag to
stop parsing on syntax error, when non-nil.
The LALR parser automaton must be available in buffer local variable
`semantic-toplevel-bovine-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-region'."
  (if (or (< start (point-min)) (> end (point-max)) (< end start))
      (error "Invalid bounds [%s %s] passed to `wisent-parse-region'"
             start end))
  (let* ((case-fold-search semantic-case-fold)
         (wisent-lex-istream (semantic-lex start end depth))
         ptree token cooked lstack wisent-lex-lookahead)
    ;; Loop while there are lexical tokens available
    (while wisent-lex-istream
      ;; Parse
      (setq  wisent-lex-lookahead (car lstack)
            token (condition-case nil
                      (wisent-parse semantic-toplevel-bovine-table
                                    wisent-lexer-function
                                    wisent-error-function
                                    goal)
                    (error nil)))
      ;; Manage returned lookahead token
      (if wisent-lookahead
          (if (eq (car lstack) wisent-lookahead)
              ;; It is already at top of lookahead stack
              (progn
                (setq token nil)
                (while lstack
                  ;; Collect unmatched tokens from lookahead stack
                  (run-hook-with-args
                   'wisent-discarding-token-functions (car lstack))
                  (setq lstack (cdr lstack))))
            ;; Push new lookahead token into the stack
            (setq lstack (cons wisent-lookahead lstack))))
      ;; Manage the parser result
      (cond
       ;; Parse succeeded, cook result
       ((consp token)
        (setq lstack nil ;; Clear the lookahead stack
              cooked (semantic-raw-to-cooked-token token)
              ptree (append cooked ptree))
        (while cooked
          (setq token  (car cooked)
                cooked (cdr cooked))
          (or (semantic-tag-get token 'reparse-symbol)
              (semantic-tag-put token 'reparse-symbol goal)))
        )
       ;; Return on error if requested
       (returnonerror
        (setq wisent-lex-istream nil)
        ))
      ;; Work in progress...
      (if wisent-lex-istream
	  (if (eq semantic-bovination-working-type 'percent)
	      (working-status
               (/ (* 100 (semantic-lex-token-start
                          (car wisent-lex-istream)))
                  (point-max)))
	    (working-dynamic-status))))
    ;; Return parse tree
    (nreverse ptree)))

;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()
     
     (def-edebug-spec define-wisent-lexer
       (&define name stringp def-body)
       )
     
     ))

(provide 'wisent-bovine)

;;; wisent-bovine.el ends here
