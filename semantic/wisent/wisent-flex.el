;;; wisent-flex.el --- Wisent - Semantic lexer

;; Copyright (C) 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 23 Feb 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-flex.el,v 1.5 2002/07/02 09:40:35 ponced Exp $

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
;; A lexical analyser for use by Wisent's LALR parser in Semantic
;; environment.

;;; History:
;; 

;;; Code:

(require 'semantic)

(defvar wisent-flex-istream nil
  "Input stream of `semantic-flex' syntactic tokens.")

(defvar wisent-flex-tokens-obarray nil
  "Buffer local token obarray for `wisent-flex'.")
(make-variable-buffer-local 'wisent-flex-tokens-obarray)

(defsubst wisent-flex-token-rules (token)
  "Return matching rules of TOKEN."
  (symbol-value
   (intern-soft (symbol-name token) wisent-flex-tokens-obarray)))

(defsubst wisent-flex-token-get (token property)
  "For token TOKEN, get the value of PROPERTY."
  (get (intern-soft (symbol-name token) wisent-flex-tokens-obarray)
       property))

(defun wisent-flex-add-token (token obarray)
  "Check and add TOKEN to OBARRAY."
  (let* ((stok  (intern (car token) obarray))
         (rules (cdr token))
         rule entry entries default)
    (while rules
      (setq rule  (car rules)
            rules (cdr rules))
      (if (cdr rule)
          (setq entries (cons rule entries))
        (setq rule (car rule))
        (if default
            (message "*** `%s' default rule %S redefined as %S"
                     stok default rule))
        (setq default rule)))
    ;; Ensure that the default rule is the first one.
    (set stok (cons default (nreverse entries)))))

(defsubst wisent-flex-put-default (name property value obarray)
  "Set NAME's PROPERTY to VALUE.
Define NAME in OBARRAY if it does not already exist."
  (let ((symbol (intern-soft name obarray)))
    (or symbol (set (setq symbol (intern name obarray)) nil))
    (put symbol property value)))

(defun wisent-flex-make-token-table (tokens &optional propertyalist)
  "Convert a list of TOKENS into an obarray and return it.
If optional argument PROPERTYALIST is non nil, then interpret it, and
apply those properties"
  ;; Create the symbol hash table
  (let* ((obarray (make-vector 13 0))
         property)
    ;; fill it with stuff
    (while tokens
      (wisent-flex-add-token (car tokens) obarray)
      (setq tokens (cdr tokens)))
    ;; Set up some useful default properties
    (wisent-flex-put-default "punctuation" 'char-literal t obarray)
    (wisent-flex-put-default "open-paren"  'char-literal t obarray)
    (wisent-flex-put-default "close-paren" 'char-literal t obarray)
    ;; Apply all properties
    (while propertyalist
      (setq property      (car propertyalist)
            propertyalist (cdr propertyalist))
      (put (or (intern-soft (car property) obarray)
               (signal 'wrong-type-argument
                       (list (car property) 'token)))
           (nth 1 property)
           (nth 2 property)))
    obarray))

(defsubst wisent-flex-match (text default rules &optional usequal)
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

(defmacro wisent-flex-eoi ()
  "Return an End-Of-Input lexical token.
The EOI token is like this: ($EOI "" POINT-MAX . POINT-MAX)."
  `(cons ',wisent-eoi-term (cons "" (cons (point-max) (point-max)))))

(defun wisent-flex ()
"Return the next available lexical token.

Eat syntactic tokens produced by `semantic-flex', available in
variable `wisent-flex-istream', and return Wisent's lexical tokens.

See documentation of `semantic-flex-tokens' for details on the
syntactic tokens returned by `semantic-flex'.

In most cases one syntactic token is mapped to one lexical token.  But
in certain cases several successive syntactic tokens can be mapped to
one lexical tokens.  A common case is given by arithmetic operators
which can be made of multiple punctuations.

Also the mapping between syntactic tokens and lexical ones uses regexp
match by default, but can use string comparison too.

The rules specifying how to do the mapping are defined in two symbol
tables:

  - The keyword table in variable `semantic-flex-keywords-obarray';

  - The token table in variable `wisent-flex-tokens-obarray'.

Keywords are directly mapped to equivalent Wisent's lexical tokens
like this (SF- prefix means `semantic-flex', WF- `wisent-flex'):

  (SF-KEYWORD start . end)  ->  (WF-KEYWORD \"name\" start . end)

Mapping of other tokens obeys to rules in the token table.  Here is an
example on how to define the mapping of 'punctuation syntactic tokens.

1. Add (`intern') the symbol 'punctuation into the token table.

2. Set its value to the mapping rules to use.  Mapping rules are an
   alist of (WF-TOKEN . MATCHER) elements.  WF-TOKEN is the category
   of the Wisent's lexical token (for example 'OPERATOR).  MATCHER is
   the regular expression used to filter input data (for example
   \"[+-]\").  The first element of the mapping rule alist defines a
   default matching rule. It must be nil or have the form (WF-TOKEN).
   When there is no mapping rule that matches the syntactic token
   value, the default WF-TOKEN or nil is returned.

   Thus, if the syntactic token symbol 'punctuation has the mapping
   rules '(nil (OPERATOR . \"[+-]\")), the following token:

   (punctuation 1 . 2)

   will be mapped to the lexical token

   (OPERATOR \"+\" 1 . 2)

   if the buffer contained \"+\" between positions 1 and 2.

   To define multiple matchers for the same WF-TOKEN just give
   several (WF-TOKEN . MATCHER) values.  MATCHERs will be tried in
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
     `wisent-flex-istream' accordingly.

   The following example maps multiple punctuations to operators and
   use string comparison:

   (let ((entry (intern 'punctuation token-table)))
     (set entry '(nil ;; No default mapping
                  (LSHIFT . \"<<\") (RSHIFT . \">>\")
                  (LT     .  \"<\") (GT     .  \">\")))
     (put entry 'string   t)
     (put entry 'multiple t))"
  (if (null wisent-flex-istream)
      
      ;; End of input
      ;; ------------
      (wisent-flex-eoi)
    
    (let* ((is   wisent-flex-istream)
           (flex (car is))
           (stok (car flex))
           (text (semantic-flex-text flex))
           default rules usequal wlex term beg end ends n is2)
      
      (if (setq term (semantic-flex-keyword-p text))
       
          ;; Keyword
          ;; -------
          (setq wlex (cons term (cons text (cdr flex)))
                ;; Eat input stream
                wisent-flex-istream (cdr is))
                
        
        ;; Token
        ;; -----
        (if (null (setq rules (wisent-flex-token-rules stok)))
            ;; Eat input stream
            (setq wisent-flex-istream (cdr is))
          
          ;; Map syntactic token following RULES
          (setq default (car rules)
                rules   (cdr rules))
          (cond
           
           ;; If specified try a function first to map token.
           ;; It must return a lexical token or nil and update the
           ;; input stream (`wisent-flex-istream') accordingly.
           ((and (setq n (wisent-flex-token-get stok 'handler))
                 (setq wlex (funcall n))))
           
           ;; Several/One mapping
           ((wisent-flex-token-get stok 'multiple)
            (setq beg  (semantic-flex-start flex)
                  end  (semantic-flex-end   flex)
                  ends (list end)
                  n    1
                  is2  (cdr is)
                  flex (car is2))
            ;; Collect successive `semantic-flex' tokens
            (while (and (eq (car flex) stok)
                        (= end (semantic-flex-start flex)))
              (setq end  (semantic-flex-end flex)
                    ends (cons end ends)
                    n    (1+ n)
                    is2  (cdr is2)
                    flex (car is2)))
            ;; Search the longest match
            (setq usequal (wisent-flex-token-get stok 'string))
            (while (and (not wlex) ends)
              (setq end  (car ends)
                    text (buffer-substring-no-properties beg end)
                    term (wisent-flex-match text default rules usequal))
              (if term
                  (setq wlex (cons term (cons text (cons beg end)))
                        ;; Eat input stream
                        wisent-flex-istream (nthcdr n is))
                (setq n    (1- n)
                      ends (cdr ends)))))
           
           ;; One/one token mapping
           ((setq usequal (wisent-flex-token-get stok 'string)
                  term (wisent-flex-match text default rules usequal))
            (setq wlex (cons term (cons text (cdr flex)))
                  ;; Eat input stream
                  wisent-flex-istream (cdr is))))))
      
      ;; Return value found or default one
      (or wlex
          (cons (if (wisent-flex-token-get stok 'char-literal)
                    (aref text 0)
                  stok)
                (cons text (cdr flex)))))))

(provide 'wisent-flex)

;;; wisent-flex.el ends here
