;;; wisent-bovine.el --- Wisent - Semantic gateway

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-bovine.el,v 1.12 2001/12/19 10:53:32 ponced Exp $

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

;;; These two functions should be moved to semantic.el
;;
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

;;; Token production
;;
(defsubst wisent-token (&rest return-val)
  "Return a Semantic token including RETURN-VAL.
To be used in Wisent LALR(1) grammar actions to build the
`semantic-toplevel-bovine-cache'."
  ;; To avoid calling:
  ;;   (semantic-token-put return-val 'reparse-symbol $nterm)
  ;; the token property list with the 'reparse-symbol property is
  ;; setup "on the fly" ;)
  (list (nconc return-val
               (list
                (list (cons 'reparse-symbol $nterm))
                (vector (car $region) (cdr $region))))))

;;; Unmatched syntax
;;
(defun wisent-collect-unmatched-syntax (input)
  "Add INPUT lexical token to the cache of unmatched tokens.
Run as `wisent-skip-token-hook' hook function.
See also the variable `semantic-unmatched-syntax-cache'."
  (let ((region (cddr input)))
    (and (number-or-marker-p (car region))
         (number-or-marker-p (cdr region))
         (setq semantic-unmatched-syntax-cache
               (cons (cons (car input) region)
                     semantic-unmatched-syntax-cache)))))

;;; Bovination
;;
(defvar wisent-flex-istream nil
  "Input stream of `semantic-flex' lexical tokens.
The actual value of this variable is local to
`wisent-bovinate-nonterminal'.")

(defvar wisent-flex-depth nil
  "How `semantic-flex' will setup the lexer input stream.
See also `semantic-flex-depth'.")
(make-variable-buffer-local 'wisent-flex-depth)

(defvar wisent-lexer-function nil
  "Function used to get the next lexical token in input.
This function does not have argument and must pop tokens from
`wisent-flex-istream'.")
(make-variable-buffer-local 'wisent-lexer-function)

(defvar wisent-error-function #'ignore
  "Function used to report parse error.")
(make-variable-buffer-local 'wisent-error-function)

(defvar wisent-lexer-lookahead nil)

(defun wisent-lexer-wrapper ()
  "Return the next lexical input available.
Used as a wrapper to call `wisent-lexer-function' and to provide
working goodies."
  (if wisent-flex-istream
      (if (eq semantic-bovination-working-type 'percent)
          (working-status
           (/ (* 100 (semantic-flex-start (car wisent-flex-istream)))
              (point-max)))
        (working-dynamic-status)))
  (or (prog1
          wisent-lexer-lookahead
        (setq wisent-lexer-lookahead nil))
      (funcall wisent-lexer-function)))

(defun wisent-bovinate-nonterminal (stream table lexer error
                                           &optional nonterminal)
  "Bovinate STREAM based on LALR TABLE.
Use LEXER to get next input and ERROR as error reporting function.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (let* ((wisent-flex-istream stream)
         (cache (wisent-parse table lexer error nonterminal)))
    (list wisent-flex-istream
          ;; Ensure to get only valid Semantic tokens from the LALR
          ;; parser!
          (delq nil
                (mapcar #'(lambda (tok)
                            (if (semantic-token-p tok)
                                tok))
                        cache)))))

(defun wisent-bovinate-nonterminals (stream nonterm
                                            &optional returnonerror)
  "Bovinate the entire stream STREAM starting with NONTERM.
Optional argument RETURNONERROR indicates that the parser should exit
with the current results on a parse error."
  (let ((case-fold-search semantic-case-fold)
        result nontermsym sstream lookahead wisent-lexer-lookahead)
    ;; Collect unmatched syntax lexical tokens
    (add-hook 'wisent-skip-token-hook
              'wisent-collect-unmatched-syntax)
    (while stream
      (setq lookahead wisent-lexer-lookahead
            nontermsym (wisent-bovinate-nonterminal
                        stream
                        semantic-toplevel-bovine-table
                        #'wisent-lexer-wrapper
                        wisent-error-function
                        nonterm)
            stream     (car nontermsym)
            sstream    (nth 1 nontermsym))
      (if (and wisent-lookahead (eq lookahead wisent-lookahead))
          (run-hook-with-args 'wisent-skip-token-hook lookahead)
        (setq wisent-lexer-lookahead wisent-lookahead))
      (if sstream
          (setq result (nconc sstream result))
        (if returnonerror
            (setq stream nil)
          ;;(error "Parse error")
	  )))
    ;; End of collect of unmatched syntax lexical tokens
    (remove-hook 'wisent-skip-token-hook
                 'wisent-collect-unmatched-syntax)
    result))

(defun wisent-rebovinate-token (token)
  "Use TOKEN for extents, and reparse it, splicing it back into the cache."
  ;; Pre Hooks
  (run-hook-with-args 'semantic-pre-clean-token-hooks token)

  (let* ((semantic-flex-depth wisent-flex-depth)
         (stream (semantic-flex (semantic-token-start token)
                                (semantic-token-end token)))
	 ;; For embeded tokens (type parts, for example) we need a
	 ;; different symbol.  Come up with a plan to solve this.
	 (nonterminal (semantic-token-get token 'reparse-symbol))
	 (new (and nonterminal
                   (nth 1 (condition-case nil
                              (wisent-bovinate-nonterminal
                               stream
                               semantic-toplevel-bovine-table
                               #'wisent-lexer-wrapper
                               wisent-error-function
                               nonterminal)
                            (error nil))))))
    (if (or (null (car new))            ; Clever reparse failed
            (> (length new) 1))         ; or returned multiple tokens
        ;; Don't do much, we have to do a full recheck.
        (setq semantic-toplevel-bovine-cache-check t)

      ;; Update the cache with the new token
      (semantic-overlay-list new)       ; Setup overlays
      (setq new (car new))              ; Get the new token
      (let ((oo (semantic-token-overlay token))
            (o (semantic-token-overlay new)))
        ;; Copy all properties of the old overlay here.  I think I can
        ;; use plists in emacs, but not in XEmacs.  Ack!
        (semantic-overlay-put o 'face (semantic-overlay-get oo 'face))
        (semantic-overlay-put o 'old-face (semantic-overlay-get oo 'old-face))
        (semantic-overlay-put o 'intangible (semantic-overlay-get oo 'intangible))
        (semantic-overlay-put o 'invisible (semantic-overlay-get oo 'invisible))
        ;; Free the old overlay(s)
        (semantic-deoverlay-token token)
        ;; Recover properties
        (let ((p (semantic-token-properties token)))
          (while p
            (semantic-token-put new (car (car p)) (cdr (car p)))
            (setq p (cdr p))))
        (semantic-token-put new 'reparse-symbol nonterminal)
        (semantic-token-put new 'dirty nil)
        ;; Splice into the main list.
        (setcdr token (cdr new))
        (setcar token (car new))
        ;; This important bit is because the CONS cell representing
        ;; TOKEN is what we need here, even though the whole thing is
        ;; the same.
        (semantic-overlay-put o 'semantic token)
        ;; Hooks
        (run-hook-with-args 'semantic-clean-token-hooks token)
        ))))

(defun wisent-bovinate-toplevel (&optional checkcache)
  "Bovinate the entire current buffer with the LALR parser.
If the optional argument CHECKCACHE is non-nil, then make sure the
cached token list is up to date.  If a partial reparse is possible, do
that, otherwise, do a full reparse."
  (cond
   ;; Check if before bovination hooks allow to parse
   ((not (run-hook-with-args-until-failure
	  'semantic-before-toplevel-bovination-hook))
    ;; If any hook returns nil, we must return the cache as the buffer
    ;; is supposedly unsafe for parsing.
    semantic-toplevel-bovine-cache)
   
   ;; Partial reparse
   ((semantic-bovine-toplevel-partial-reparse-needed-p checkcache)
    (garbage-collect)
    (let* ((gc-cons-threshold 10000000)
           (changes (semantic-remove-dirty-children)))
      ;; We have a cache, and some dirty tokens
      (let ((semantic-bovination-working-type 'dynamic))
        (working-status-forms (format "%s [LALR]" (buffer-name)) "done"
          (while (and semantic-dirty-tokens
                      (not (semantic-bovine-toplevel-full-reparse-needed-p
                            checkcache)))
            (wisent-rebovinate-token (car semantic-dirty-tokens))
            (setq semantic-dirty-tokens (cdr semantic-dirty-tokens)))
          (working-dynamic-status t))
        (setq semantic-dirty-tokens nil))
      
      (if (semantic-bovine-toplevel-full-reparse-needed-p checkcache)
          ;; If the partial reparse fails, jump to a full reparse.
          (wisent-bovinate-toplevel checkcache)
        ;; Clear the cache of unmatched syntax tokens
        (semantic-clear-unmatched-syntax-cache)
        ;; After partial reparse completed, let hooks know the updated
        ;; tokens
        (run-hook-with-args 'semantic-after-partial-cache-change-hook
                            changes)
        semantic-toplevel-bovine-cache)))
   
   ;; Full parse
   ((semantic-bovine-toplevel-full-reparse-needed-p checkcache)
    (garbage-collect)
    ;; Reparse the whole system
    (let* ((gc-cons-threshold 10000000)
           (semantic-flex-depth wisent-flex-depth)
           ;; Capture the lexical tokens here so that if an error is
           ;; thrown, the cache is still safe.
           (lex (semantic-flex (point-min) (point-max)))
           res)
      ;; Init a dump
      ;;(if semantic-dump-parse
      ;;    (semantic-dump-buffer-init))
      ;; Clear the caches
      (semantic-clear-toplevel-cache)
      ;; Parse!
      (working-status-forms (format "%s [LALR]" (buffer-name)) "done"
        (setq res (wisent-bovinate-nonterminals lex nil))
        (working-status t))
      (setq res (nreverse res))
      ;; Set up the new overlays, and then reset the cache.
      (semantic-overlay-list res)
      (semantic-set-toplevel-bovine-cache res)
      semantic-toplevel-bovine-cache))

   ;; No parse needed
   (t
    ;; We have a cache with stuff in it, so return it
    semantic-toplevel-bovine-cache)
   
   ))

(defadvice semantic-bovinate-toplevel (around wisent-bovine activate)
  "Bypass `semantic-bovinate-toplevel-override' handling.
So `wisent-bovinate-toplevel' can handle partial reparse too!"
  (if (eq semantic-bovinate-toplevel-override 'wisent-bovinate-toplevel)
      (setq ad-return-value (wisent-bovinate-toplevel (ad-get-arg 0)))
    ad-do-it))

;;; Bovine table functions
;;
;; These are functions that can be called from within a bovine table.
;; Most of these have code auto-generated from other construct in the
;; BNF.
(defsubst wisent-bovinate-from-nonterminal-full (start end nonterm
                                                       &optional depth)
  "Bovinate from within a nonterminal from START to END.
Iterates until all the space between START and END is exhausted.
Argument NONTERM is the nonterminal symbol to start with or nil for
the default goal.  Optional argument DEPTH is the depth of lists to
dive into.  It defaults to 1."
  (nreverse
   (wisent-bovinate-nonterminals (semantic-flex start end (or depth 1))
                                 nonterm)))

(defun wisent-bovinate-region-until-error (start end nonterm
                                                 &optional depth)
  "Bovinate between START and END starting with NONTERM.
Optinal DEPTH specifies how many levels of parenthesis to enter.  This
command will parse until an error is encountered, and return the list
of everything found until that moment.
This is meant for finding variable definitions at the beginning of
code blocks in methods.  If NONTERM can also support commands, use
`wisent-bovinate-from-nonterminal-full'."
  (nreverse
   (wisent-bovinate-nonterminals (semantic-flex start end depth)
                                 nonterm
                                 ;; This says stop on an error.
                                 t)))

(provide 'wisent-bovine)

;;; wisent-bovine.el ends here
