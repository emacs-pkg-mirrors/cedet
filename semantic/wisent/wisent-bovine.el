;;; wisent-bovine.el --- Wisent - Semantic gateway

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-bovine.el,v 1.18 2002/07/20 08:06:16 ponced Exp $

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

(provide 'wisent-bovine)
(require 'semantic)
(require 'wisent)
(require 'wisent-flex)

;;; Token production
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
       (semantic-token-put (car l) 'reparse-symbol $nterm)
       (setq l (cdr l)))
     cooked))

;;; Bovination
;;
(defvar wisent-error-function #'ignore
  "Function used to report parse error.")
(make-variable-buffer-local 'wisent-error-function)

(defvar wisent-lexer-function #'wisent-flex
  "Function used to obtain the next lexical token in input.
Should be a lexical analyzer created with `define-wisent-lexer'.")
(make-variable-buffer-local 'wisent-lexer-function)

(defvar wisent-lexer-lookahead nil
  "Extra lookahead token.
When non-nil it is directly returned by `wisent-lexer-function'.")

(defun wisent-collect-unmatched-syntax (input)
  "Add INPUT lexical token to the cache of unmatched tokens.
See also the variable `semantic-unmatched-syntax-cache'."
  (let ((region (cddr input)))
    (and (number-or-marker-p (car region))
         (number-or-marker-p (cdr region))
         (setq semantic-unmatched-syntax-cache
               (cons (cons (car input) region)
                     semantic-unmatched-syntax-cache)))))

(defun wisent-bovinate-nonterminal (stream table
                                           &optional nonterminal)
  "Bovinate STREAM based on LALR TABLE.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (let (wisent-flex-istream wisent-lexer-lookahead lookahead cache)
    (if (vectorp (caar stream))
        (setq lookahead (aref (caar stream) 0)
              wisent-lexer-lookahead lookahead
              stream (cdr stream)))
    (setq wisent-flex-istream stream
          cache (condition-case nil
                    (wisent-parse table
                                  wisent-lexer-function
                                  wisent-error-function
                                  nonterminal)
                  (error nil)))
    (if wisent-lookahead
        (if (eq lookahead wisent-lookahead)
            (progn
              (setq cache nil)
              ;; collect unmatched token here
              (run-hook-with-args
               'wisent-discarding-token-functions lookahead)
              )
          ;; push back the lookahead token
          (setq wisent-flex-istream
                (cons (cons (vector wisent-lookahead)
                            (cddr wisent-lookahead))
                      wisent-flex-istream))))
    (list wisent-flex-istream
          (if (consp cache) cache '(nil))
          )))

;;; wisent-bovine.el ends here
