;;; pprint.el --- A flexible Elisp pretty-printer

;; Copyright (C) 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 06 Mar 2002
;; Keywords: lisp
;; X-RCS: $Id: pprint.el,v 1.1 2002/03/10 09:06:50 ponced Exp $

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
;; pprint library provides a simple and flexible pretty-printer for
;; Elisp code.
;;
;; The core function `pprint-to-string' uses `prin1' to write into a
;; temporary buffer a "flat" representation of a given expression.
;; Then it walks over that linear form and inserts line breaks and
;; indentations to produce a pretty-printed representation of the
;; expression.
;;
;; The implementation uses a lot of inline functions to improve
;; performance.  And, when byte-compiled, pprint is reasonably fast.
;;
;; pprint can handle specific expressions using adapted "printers".  A
;; "printer" is a function that will be called to process a form that
;; match a regular expression.  A number of "standard printers" are
;; predefined to pretty print common Elisp statements like `defun',
;; `defvar', `lambda', `let', `progn', `cond', `if', etc..
;;
;; More "printers" should be provided in future versions ;-)
;;

;;; History:
;;

;;; Code:

;;;;
;;;; Printers management
;;;;

(defvar pprint-standard-printers nil
  "The standard printers.")

(defvar pprint-printers nil
  "The current printers.
This is an alist which maps printers (functions) to
matchers (regexps).")

(defsubst pprint-clear-printers ()
  "Clear the current defined printers."
  (setq pprint-printers nil))

(defun pprint-push-printer (printer matcher)
  "Push a new PRINTER on top of defined printers.
MATCHER is a regexp matching expressions passed to PRINTER."
  (add-to-list 'pprint-printers (cons printer matcher)))
(put 'pprint-push-printer 'lisp-indent-function 1)

(defmacro pprint-with-printers (table &rest body)
  "Set up a copy of the TABLE of printers and evaluate BODY.
The current table of printers is saved, BODY is evaluated, and
the saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "old-table")))
    `(let ((,old-table pprint-printers))
       (unwind-protect
           (progn
             (setq pprint-printers (copy-sequence ,table))
             ,@body)
         (setq pprint-printers ,old-table)))))
(put 'pprint-with-printers 'lisp-indent-function 1)

;;;;
;;;; Core functions
;;;;

(defvar pprint-width)
(defvar pprint-no-break)

(defmacro pprint-no-break-p (&rest motions)
  "Return non-nil if after MOTIONS current column is <= width."
  (if motions
      `(<= (save-excursion ,@ motions (current-column)) pprint-width)
    `(<= (current-column) pprint-width)))

(defsubst pprint-maybe-newline-and-indent ()
  "Insert a newline, then indent.
Does nothing if point is before a close parenthesis character or
already at the beginning of a line."
  (or (looking-at "\\s)")
      (save-excursion (skip-syntax-backward "-") (bolp))
      (newline-and-indent)))

(defsubst pprint-search-printer (table)
  "Search in TABLE for a printer to process expression at point.
Return the first one that match expression at point or nil if not
found."
  (while (and table (not (looking-at (cdar table))))
    (setq table (cdr table)))
  (caar table))
  
(defsubst pprint-dispatch-printer ()
  "Dispatch a printer to print current expression.
Return non-nil if a printer was found and dispatched."
  (let ((printer
         (or (pprint-search-printer pprint-printers)
             (pprint-search-printer pprint-standard-printers))))
    (when printer
      (funcall printer)
      t)))

(defsubst pprint-sexp-width (room)
  "Return the width needed to pretty print current expression.
ROOM specifies the available width."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (progn (forward-sexp) (point)))
      (let* ((old-sexp (buffer-string))
             (pprint-width room))
        (goto-char (point-min))
        (pprint-sexp)
        (let ((width 0))
          (goto-char (point-min))
          (while (not (eobp))
            (end-of-line)
            (setq width (max width (current-column)))
            (beginning-of-line)
            (kill-line t))
          (insert old-sexp)
          width)))))

(defsubst pprint-list ()
  "Built-in list printer."
  (if (pprint-no-break-p (forward-sexp))
      (progn
        (forward-sexp)
        (down-list -1))
    (if (and (prog1 (looking-at "(\\sw+")
               (down-list 1)
               (pprint-sexp t)) ;; never break after an open paren
             (not (looking-at "\\s)")))
        (let* ((room (- pprint-width (current-column)))
               (width 0))
          (save-excursion
            (while (and (not (looking-at "\\s)")) (<= width room))
              (setq width (max width (pprint-sexp-width room)))
              (forward-sexp)))
          (if (<= width room)
              (pprint-sexp t))
          (pprint-maybe-newline-and-indent)))))

(defsubst pprint-close-list ()
  "Built-in printer to process close parenthesis characters."
  (up-list 1)
  (or (looking-at "\\s)")
      (eobp)
      (= 1 (save-excursion
             (save-restriction
               (widen)
               (narrow-to-region 
                (save-excursion (forward-list -1) (point))
                (point))
               (goto-char (point-min))
               (1+ (vertical-motion (point-max))))))
      (pprint-maybe-newline-and-indent)))

(defsubst pprint-sequence ()
  "Built-in printer of a sequence of expressions.
Insert a line break before each expression."
  (while (not (looking-at "\\s)"))
    (pprint-maybe-newline-and-indent)
    (pprint-sexp)))

(defconst pprint-min-width 20
  "Minimum width required to prettify an expression.
If current width is greater than this value, the pretty printer does
nothing.")

(defun pprint-sexp (&optional pprint-no-break)
  "Pretty print S-expression at point.
If optional argument PPRINT-NO-BREAK is non-nil the pretty-printed
representation will not start on a new line."
  (when (>= pprint-width pprint-min-width)
    (let ((old-mark (copy-marker (mark-marker))))
      (set-marker (mark-marker) (save-excursion (forward-sexp) (point)))
      (while (< (point) (marker-position (mark-marker)))
        (condition-case nil
            (or pprint-no-break
                (pprint-no-break-p (forward-sexp))
                (pprint-maybe-newline-and-indent))
          (error nil))
        (setq pprint-no-break nil)
        (skip-syntax-forward "-'")
        (cond
         ((pprint-dispatch-printer))
         ((looking-at "\\s(")
          (pprint-list))
         ((looking-at "\\s)")
          (pprint-close-list))
         (t
          (forward-sexp))))
      (set-marker (mark-marker) (marker-position old-mark))
      (set-marker old-mark nil))))

;;;;
;;;; Standard printers
;;;;

(defun pprint-nil ()
  "Print nil symbol as ()."
  (delete-region (point) (save-excursion (forward-sexp) (point)))
  (insert "()"))

(defun pprint-lambda ()
  "Standard printer for `lambda' like forms."
  (down-list 1)
  (forward-sexp)
  ;; Print empty args as () instead of nil
  (pprint-with-printers nil
    (pprint-push-printer 'pprint-nil "\\<nil\\>")
    (pprint-sexp t))
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-defun ()
  "Standard printer for `defun' like forms."
  (pprint-maybe-newline-and-indent)
  (down-list 1)
  (forward-sexp)
  (forward-sexp)
  ;; Print empty args as () instead of nil
  (pprint-with-printers nil
    (pprint-push-printer 'pprint-nil "\\<nil\\>")
    (pprint-sexp t))
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-defvar ()
  "Standard printer for `defvar' like forms."
  (pprint-maybe-newline-and-indent)
  (down-list 1)
  (forward-sexp))

(defun pprint-let ()
  "Standard printer for `let' like forms."
  (down-list 1)
  (forward-sexp)
  (pprint-sexp t)
  (pprint-maybe-newline-and-indent)
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-if ()
  "Standard printer for `if' like forms."
  (down-list 1)
  (forward-sexp)
  (pprint-sexp t)
  (pprint-maybe-newline-and-indent)
  (pprint-sexp)
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-while ()
  "Standard printer for `while' like forms."
  (down-list 1)
  (forward-sexp)
  (pprint-sexp t)
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-progn ()
  "Standard printer for `progn' like forms."
  (down-list 1)
  (forward-sexp)
  (pprint-sequence)
  (pprint-close-list))

(defun pprint-setq ()
  "Standard printer for `setq' like forms."
  (down-list 1)
  (forward-sexp)
  (forward-sexp) ;; 1rst VAR
  (pprint-sexp t) ;; 1rst VAL
  (while (not (looking-at "\\s)"))
    (pprint-maybe-newline-and-indent)
    (forward-sexp) ;; VAR
    (pprint-sexp t)) ;; VAL
  (pprint-close-list))

(defun pprint-setup-standard-printers ()
  "Setup standard printers."
  (pprint-clear-printers)
  ;; Printers are searched in sequence from last to first pushed one.
  ;; So it could be important to push the most generic printers first!
  (pprint-push-printer 'pprint-defun
    (format "(%s\\>"
            (regexp-opt
             '(
               "defun" "defmacro" "defsubst"
               ) t)))
  (pprint-push-printer 'pprint-lambda
    (format "(%s\\>"
            (regexp-opt
             '(
               "lambda"
               ) t)))
  (pprint-push-printer 'pprint-defvar
    (format "(%s\\>"
            (regexp-opt
             '(
               "defvar" "defconst"
               ) t)))
  (pprint-push-printer 'pprint-let
    (format "(%s\\>"
            (regexp-opt
             '(
               "let" "let*"
               ) t)))
  (pprint-push-printer 'pprint-if
    (format "(%s\\>"
            (regexp-opt
             '(
               "if"
               ) t)))
  (pprint-push-printer 'pprint-while
    (format "(%s\\>"
            (regexp-opt
             '(
               "while" "with" "when" "unless"
               "condition-case" "catch"
               ) t)))
  (pprint-push-printer 'pprint-progn
    (format "(%s\\>"
            (regexp-opt
             '(
               "prog1" "progn" "cond"
               "save-excursion" "save-restriction"
               ) t)))
  (pprint-push-printer 'pprint-setq
    (format "(%s\\>"
            (regexp-opt
             '(
               "setq"
               ) t)))
  (setq pprint-standard-printers pprint-printers))

(pprint-setup-standard-printers)

;;;;
;;;; User's functions & commands
;;;;

;;;###autoload
(defun pprint-to-string (object &optional width)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.
The pretty printer try as much as possible to limit the length of
lines to given WIDTH.  WIDTH value defaults to `fill-column'."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ( ;;(print-escape-newlines t)
          (print-quoted t))
      (prin1 object (current-buffer)))
    (goto-char (point-min))
    (let* ((pprint-width (or width fill-column))
           (zmacs-regions nil)) ;; XEmacs
      (pprint-sexp))
    (buffer-string)))

;;;###autoload
(defun pprint (object &optional stream width)
"Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
The pretty printer try as much as possible to limit the length of
lines to given WIDTH.  WIDTH value defaults to `fill-column'."
  (princ (pprint-to-string object width)
         (or stream standard-output)))

;;;###autoload
(defun pprint-function (function-name)
  "See a pretty-printed representation of FUNCTION-NAME."
  (interactive "aPretty print function: ")
  (let ((code (symbol-function function-name)))
    (if (byte-code-function-p code)
        (error "can't pretty-print a byte compiled function"))
    (with-current-buffer
        (get-buffer-create (format "*pprint-function %s*"
                                   function-name))
      (erase-buffer)
      (emacs-lisp-mode)
      (pprint code (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'pprint)

;;; pprint.el ends here
