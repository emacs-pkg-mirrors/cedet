;;; wisent-bovine.el --- Wisent - Semantic gateway

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-bovine.el,v 1.1 2001/08/30 14:01:05 ponced Exp $

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

;;;;
;;;; These two functions should be moved to semantic.el
;;;;

(defsubst semantic-flex-token-value (table category &optional key)
  "Return %token values from the token table TABLE.
CATEGORY is a symbol identifying a token category.  If the symbol KEY
is specified the function returns the particular value of this token.
Otherwise the function returns the alist of (KEY . VALUE) for this
category.  See also the function `semantic-bnf-token-table'."
  (let ((cat-alist (cdr (assq category table))))
    (if key
        (cdr (assq key cat-alist))
      cat-alist)))

(defsubst semantic-flex-token-key (table category value)
  "Search for a %token symbol in the token table TABLE.
CATEGORY is a symbol identifying a token category.  VALUE is the value
of the token to search for.  If not found return nil.  See also the
function `semantic-bnf-token-table'."
  (car (rassoc value (cdr (assq category table)))))

;;;;
;;;; Token production
;;;;

(defsubst wisent-token (&rest return-val)
  "Return a Semantic token including RETURN-VAL.
To be used in Wisent LALR(1) grammar actions to build the
`semantic-toplevel-bovine-cache'."
  (list
   (nconc return-val
          (list nil (vector (car $region) (cdr $region))))))

;;;;
;;;; Bovination
;;;;

(defvar wisent-flex-istream nil
  "Input stream of `semantic-flex' lexical tokens.")
(make-variable-buffer-local 'wisent-flex-istream)

(defvar wisent-flex-function nil
  "Function used to get the next lexical token in input.
This function does not have argument and should pop `semantic-flex'
tokens from `wisent-flex-istream'.")
(make-variable-buffer-local 'wisent-flex-function)

(defvar wisent-error-function #'ignore
  "Function used to report parse error.")
(make-variable-buffer-local 'wisent-error-function)

(defun wisent-lexer-wrapper ()
  "Return the next lexical input available.
Used as a wrapper to call `wisent-flex-function' and to provide
working goodies."
  (prog1
      (funcall wisent-flex-function)
    (if wisent-flex-istream
        (if (eq semantic-bovination-working-type 'percent)
            (working-status
             (floor
              (* 100.0 (/ (float (semantic-flex-start
                                  (car wisent-flex-istream)))
                          (float (point-max))))))
          (working-dynamic-status)))))

(defun wisent-bovinate-nonterminal (stream table lexer error
                                           &optional nonterminal)
  "Bovinate STREAM based on LALR TABLE.
Use LEXER to get next input and ERROR as error reporting function.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (let* ((gc-cons-threshold 10000000)
         (wisent-flex-istream stream)
         (cache (wisent-parse table lexer error nonterminal)))
    (semantic-overlay-list cache)
    (list wisent-flex-istream cache)))

(defun wisent-bovinate-nonterminals (stream nonterm
                                            &optional returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
Optional argument RETURNONERROR indicates that the parser should exit
with the current results on a parse error."
  (let ((case-fold-search semantic-case-fold)
        result nontermsym sstream)
    (while stream
      (setq nontermsym (wisent-bovinate-nonterminal
                        stream
                        semantic-toplevel-bovine-table
                        #'wisent-lexer-wrapper
                        wisent-error-function
                        nonterm)
            stream     (car nontermsym)
            sstream    (nth 1 nontermsym))
      (if sstream
          (setq result (append sstream result))
        (if returnonerror
            (setq stream nil)
          ;;(error "Parse error")
	  )))
    result))

(defun wisent-bovinate-toplevel (&optional checkcache)
  "Semantic alternate Java LALR(1) parser.
The optional argument CHECKCACHE is ignored."
  ;; Reparse the whole system
  (let* ((semantic-flex-depth nil)
         (stream (semantic-flex (point-min) (point-max)))
         (cache nil))
    ;; Init a dump
    ;;(if semantic-dump-parse
    ;;    (semantic-dump-buffer-init))
    ;; Parse!
    (working-status-forms (format "%s [LALR]" (buffer-name)) "done"
      (setq cache (wisent-bovinate-nonterminals stream nil))
      (working-status t))
    ;;(semantic-set-toplevel-bovine-cache cache)
    ;;semantic-toplevel-bovine-cache))
    cache))

(provide 'wisent-bovine)

;;; wisent-bovine.el ends here
