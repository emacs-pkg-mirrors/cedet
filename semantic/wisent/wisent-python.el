;;; wisent-python.el --- LALR grammar for Python
;;
;; Copyright (C) 2002 Richard Kim
;;
;; Author: Richard Kim <ryk@dspwiz.com>
;; Maintainer: Richard Kim <ryk@dspwiz.com>
;; Created: June 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-python.el,v 1.44 2004/06/28 12:53:35 ponced Exp $
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file contains the python parser created from the grammar
;; specified in wisent-python.wy file.  It also has some support code.
;;
;;; Code:

(require 'semantic-wisent)
(require 'wisent-python-wy)

;;;****************************************************************************
;;;@ Support Code
;;;****************************************************************************
;;
;; Some of these need to come before `wisent-python-default-setup' so that
;; symbols are defined before their first use.

;; Indentation stack to keep track of INDENT tokens generated without
;; matching DEDENT tokens. Generation of each INDENT token results in
;; a new integer being added to the beginning of this list where the
;; integer represents the indentation of the current line. Each time a
;; DEDENT token is generated, the latest entry is popped off
;; this list.
(defvar wisent-python-lexer-indent-stack '(0))

;; Variable set to t only by `semantic-lex-python-charquote' so that
;; `semantic-lex-python-beginning-of-line' will not generate any
;; INDENT or DEDENT tokens for continued lines.
(defvar wisent-python-explicit-line-continuation nil)

;; Python strings are delimited by either single quotes or double
;; quotes, e.g., "I'm a string" and 'I too am s string'.
;; In addition a string can have either a 'r' and/or 'u' prefix.
;; The 'r' prefix means raw, i.e., normal backslash substitutions are
;; to be suppressed.  For example, r"01\n34" is a string with six
;; characters 0, 1, \, n, 3 and 4.  The 'u' prefix means the following
;; string is a unicode.
(defconst wisent-python-string-re "[rR]?[uU]?['\"]"
  "Regexp matching beginning of a python string.")

;;;****************************************************************************
;;;@ Lexer
;;;****************************************************************************

;; Pop all items from the "indent stack" if we are at buffer end.
(defun semantic-lex-python-pop-indent-stack ()
  (if (eq (point) (cdr semantic-lex-analysis-bounds))
      (while (> (car wisent-python-lexer-indent-stack) 0)
        (semantic-lex-push-token
         (semantic-lex-token 'DEDENT (point) (point)))
        (pop wisent-python-lexer-indent-stack))))

(define-lex-analyzer semantic-lex-python-beginning-of-line
  "Handle beginning-of-line case, i.e., possibly generate INDENT or
DEDENT tokens by comparing current indentation level with the previous
indentation values stored in `wisent-python-lexer-indent-stack'
stack."
  (and (and (bolp) (not wisent-python-explicit-line-continuation))
       (let ((last-indent (or (car wisent-python-lexer-indent-stack) 0))
             (last-pos (point))
             curr-indent)
         (skip-chars-forward " \t")
         (setq curr-indent (current-column))
         (cond
          ;; Blank or comment line => no indentation change
          ((looking-at "\\(#\\|$\\)")
           (forward-line 1)
           (setq semantic-lex-end-point (point))
           (semantic-lex-python-pop-indent-stack)
           ;; Since position changed, returning t here won't result in
           ;; infinite loop.
           t)
          ;; No change in indentation.
          ((= curr-indent last-indent)
           (setq semantic-lex-end-point (point))
           ;; If pos did not change, then we must return nil so that
           ;; other lexical analyzers can be run.
           nil)
          ;; Indentation increased
          ((> curr-indent last-indent)
           (if (or (not semantic-lex-maximum-depth)
                   (< semantic-lex-current-depth semantic-lex-maximum-depth))
               (progn
                 ;; Return an INDENT lexical token
                 (setq semantic-lex-current-depth (1+ semantic-lex-current-depth))
                 (push curr-indent wisent-python-lexer-indent-stack)
                 (semantic-lex-push-token
                  (semantic-lex-token 'INDENT last-pos (point)))
                 t)
             ;; Add an INDENT_BLOCK token
             (semantic-lex-push-token
              (semantic-lex-token
               'INDENT_BLOCK
               (progn (beginning-of-line) (point))
               (save-excursion
                 (semantic-lex-unterminated-syntax-protection
                  'INDENT_BLOCK
                  (let ((starting-indentation (current-indentation)))
                    (while (>= (current-indentation) starting-indentation)
                      (forward-list 1)
                      (beginning-of-line)))
                  (point)))))
             t)
           )
          ;; Indentation decreased
          (t
           ;; Pop items from indentation stack
           (while (< curr-indent last-indent)
             (setq semantic-lex-current-depth (1- semantic-lex-current-depth))
             (semantic-lex-push-token
              (semantic-lex-token 'DEDENT last-pos (point)))
             (pop wisent-python-lexer-indent-stack)
             (setq last-indent (or (car wisent-python-lexer-indent-stack) 0)))
           ;; If pos did not change, then we must return nil so that
           ;; other lexical analyzers can be run.
           (not (eq last-pos (point))))
          )))
  nil ;; all the work was done in the previous form
  )

(define-lex-analyzer semantic-lex-python-reset-continued-line
  "Reset `wisent-python-explicit-line-continuation' back to nil."
  (setq wisent-python-explicit-line-continuation nil)
  ()
  )

(define-lex-analyzer semantic-lex-python-newline
  "Handle NEWLINE syntactic tokens.
If the following line is an implicit continuation of current line,
then throw away any immediately following INDENT and DEDENT tokens."
  (looking-at "\\(\n\\|\\s>\\)") ;; newline or end of buffer
  (goto-char (match-end 0))
  (semantic-lex-push-token
   (semantic-lex-token 'NEWLINE (1- (point)) (point)))
  (semantic-lex-python-pop-indent-stack))

(define-lex-analyzer semantic-lex-python-string
  "Handle python strings."
  (looking-at wisent-python-string-re)
  (let ((opos (point))
        (e (semantic-lex-unterminated-syntax-protection
            'STRING_LITERAL
            ;; skip over "r" and/or "u" characters if any
            (goto-char (1- (match-end 0)))
            (cond
             ((looking-at "\"\"\"")
              (forward-char 3)
              (search-forward "\"\"\""))
             (t
              (forward-sexp 1)))
            (point))))
    (semantic-lex-push-token
     (semantic-lex-token 'STRING_LITERAL opos e))))

(define-lex-analyzer semantic-lex-python-charquote
  "Handle BACKSLASH syntactic tokens."
  (looking-at "\\s\\")
  (forward-char 1)
  (when (looking-at "$")
    (forward-char 1)
    (skip-chars-forward " \t")
    (setq wisent-python-explicit-line-continuation nil))
  (setq semantic-lex-end-point (point)))

;; This is same as wisent-java-lex-symbol except for using 'NAME token
;; rather than 'IDENTIFIER. -ryk1/05/03.
(define-lex-regex-analyzer semantic-lex-python-symbol
  "Detect and create identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
   (or (semantic-lex-keyword-p (match-string 0))
       'NAME)
   (match-beginning 0)
   (match-end 0))))

;; Same as wisent-java-lex-number. -ryk1/05/03.
(define-lex-simple-regex-analyzer semantic-lex-python-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUMBER_LITERAL)

;; Same as wisent-java-lex-blocks. -ryk1/05/03.
(define-lex-block-analyzer semantic-lex-python-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACK_BLOCK ("[" LBRACK) ("]" RBRACK)))

(define-lex semantic-python-lexer
  "Lexical Analyzer for Python code."
  ;; semantic-lex-python-beginning-of-line needs to be the first so
  ;; that we don't miss any DEDENT tokens at the beginning of lines.
  semantic-lex-python-beginning-of-line
  semantic-lex-python-reset-continued-line
  ;; semantic-lex-python-string needs to come before symbols because
  ;; of the "r" and/or "u" prefix.
  semantic-lex-python-string
  semantic-lex-ignore-whitespace
  semantic-lex-python-newline
  semantic-lex-python-number    ;; rather than semantic-lex-number
  semantic-lex-python-symbol    ;; rather than semantic-lex-symbol-or-keyword
  semantic-lex-python-charquote
  semantic-lex-python-blocks    ;; rather than semantic-lex-paren-or-list/semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation-type ;; rather than semantic-lex-punctuation
  semantic-lex-default-action
  )

(defun python-next-line ()
  "Move the cursor to the next logical line to check for INDENT or DEDENT tokens.
Usually this is simply the next physical line unless strings, lists, explicit
line continuation, blank lines, or comment lines are encountered.
This function skips over such items so that the cursor is at the beginning of
the next logical line."
  (let (beg)
    (while (not (eolp))
      (setq beg (point))
      (cond
       ;; skip over triple-quote string
       ((looking-at "\"\"\"")
        (forward-char 3)
        ;; TODO: this probably should be protected with
        ;; semantic-lex-unterminated-syntax-protection
        ;; in case the closing triple quote is not found. -ryk2/17/03.
        (search-forward "\"\"\""))
       ;; skip over lists, strings, etc
       ((looking-at "\\(\\s(\\|\\s\"\\|\\s<\\)")
        (forward-sexp 1))
       ;; backslash is the explicit line continuation character
       ((looking-at "\\s\\")
        (forward-line 1))
       ;; skip over white space, word, symbol, punctuation, and paired
       ;; delimiter (backquote) characters.
       (t (skip-syntax-forward "-w_.$")))
      (if (= (point) beg)
          (error "You have found a bug in python-next-line")))
    ;; the point now should be at the end of a line
    (forward-line 1)
    (while (and (looking-at "\\s-*\\(\\s<\\|$\\)")
                (not (eobp))) ;; skip blank and comment lines
      (forward-line 1))))

(defun python-scan-lists ( &optional target-column )
  "Without actually changing the position, return the buffer position of
the next line whose indentation is the same as the current line or less
than current line."
  (or target-column (setq target-column (current-indentation)))
  (save-excursion
    (python-next-line)
    (while (> (current-indentation) target-column)
      (python-next-line))
    ;; Move the cursor to the original indentation level or first non-white
    ;; character which ever comes first.
    (skip-chars-forward " \t" (+ (point) target-column))
    (point)))

(defadvice scan-lists (around handle-python-mode activate compile)
  "Use python mode specific function, python-scan-lists, if the
current major mode is python-mode.
Otherwise simply call the original function."
  (if (and (eq major-mode 'python-mode)
           (not (looking-at "\\s(")))
      (setq ad-return-value (python-scan-lists))
    ad-do-it))

;;;****************************************************************************
;;;@ Parser
;;;****************************************************************************

;; TODO: Is this really needed? -ryk2/9/03.
(define-mode-local-override semantic-parse-region python-mode
  (start end &optional nonterminal depth returnonerror)
  "Over-ride in order to initialize some variables."
  (let ((wisent-python-lexer-indent-stack '(0))
        (wisent-python-explicit-line-continuation nil))
    (semantic-parse-region-default
     start end nonterminal depth returnonerror)))

;; Commented this out after learning that there is no need to convert
;; tokens to names.  See "(semantic)Style Guide". -ryk2/7/03.
'(define-mode-local-override semantic-parse-region python-mode
  (start end &optional nonterminal depth returnonerror)
  "Over-ride so that 'paren_classes' non-terminal tokens can be intercepted
then converted to simple names to comply with the semantic token style guide."
  (let ((tokens (semantic-parse-region-default
                 start end nonterminal depth returnonerror)))
    (if (eq nonterminal 'paren_classes)
        (mapcar #'semantic-token-name tokens)
      tokens)))

(define-mode-local-override semantic-get-local-variables python-mode ()
  "Get the local variables based on point's context.
To be implemented for python!  For now just return nil."
  nil)

;;;###autoload
(defun wisent-python-default-setup ()
  "Setup buffer for parse."
  (wisent-python-wy--install-parser)
  (setq
   ;; Character used to separation a parent/child relationship
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   wisent-python-lexer-indent-stack '(0)
   semantic-lex-analyzer #'semantic-python-lexer
   ))

;;;###autoload
(add-hook 'python-mode-hook #'wisent-python-default-setup)

;;; Test
;;
(defun wisent-python-lex-buffer ()
  "Run `semantic-python-lexer' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'semantic-python-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max) 0)))
    (with-current-buffer
        (get-buffer-create "*semantic-python-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'wisent-python)

;;; wisent-python.el ends here
