;;; semantic-java.el --- Semantic functions for Java

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; X-RCS: $Id: semantic-java.el,v 1.4 2003/02/19 15:20:59 ponced Exp $

;; This file is not part of GNU Emacs.

;; semantic-java is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

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
;; Common function for Java parsers.

;;; History:
;; 

;;; Code:
(require 'semantic)
(require 'semantic-ctxt)

;;; Lexical analysis
;;

(defconst semantic-java-number-regexp
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
  "Lexer regexp to match Java number terminals.
Following is the specification of Java number literals.

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

;;; Environment
;;

;; Prototype handler
;;
(defun semantic-java-prototype-function (token &optional parent color)
  "Return a function (method) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (let ((name (semantic-token-name token))
        (type (semantic-token-type token))
        (args (semantic-token-function-args token))
        (argp "")
        arg argt)
    (while args
      (setq arg  (car args)
            args (cdr args))
      (if (semantic-token-p arg)
          (setq argt (if color
                         (semantic-colorize-text
                          (semantic-token-type arg) 'type)
                       (semantic-token-type arg))
                argp (concat argp argt (if args "," "")))))
    (if color
        (progn
          (if type
              (setq type (semantic-colorize-text type 'type)))
          (setq name (semantic-colorize-text name 'function))))
    (concat (or type "") (if type " " "") name "(" argp ")")))

(defun semantic-java-prototype-variable (token &optional parent color)
  "Return a variable (field) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (concat (if color
              (semantic-colorize-text
               (semantic-token-type token) 'type)
            (semantic-token-type token))
          " "
          (if color
              (semantic-colorize-text
               (semantic-token-name token) 'variable)
            (semantic-token-name token))))

(defun semantic-java-prototype-type (token &optional parent color)
  "Return a type (class/interface) prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-java-prototype-nonterminal'."
  (concat (semantic-token-type token)
          " "
          (if color
              (semantic-colorize-text
               (semantic-token-name token) 'type)
            (semantic-token-name token))))

(defun semantic-java-prototype-nonterminal (token &optional parent color)
  "Return a prototype for TOKEN.
Override `semantic-prototype-nonterminal'.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in."
  (let ((fprot (intern-soft
                (format "semantic-java-prototype-%s"
                        (semantic-token-token token)))))
    (if (fboundp fprot)
        (funcall fprot token parent color)
      (semantic-prototype-nonterminal-default token parent color))))

;; Documentation handler
;;
(defsubst semantic-java-skip-spaces-backward ()
  "Move point backward, skipping Java whitespaces."
  (skip-chars-backward " \n\r\t"))

(defsubst semantic-java-skip-spaces-forward ()
  "Move point forward, skipping Java whitespaces."
  (skip-chars-forward " \n\r\t"))

(defun semantic-java-find-documentation (&optional token nosnarf)
  "Find documentation from TOKEN and return it as a clean string.
Java have documentation set in a comment preceeding TOKEN's
definition.  Optional argument NOSNARF means to only return the flex
token for it.  If NOSNARF is 'flex, then only return the flex token.
Override `semantic-find-documentation'."
  (if (or token (setq token (semantic-current-nonterminal)))
      (save-excursion
        (set-buffer (semantic-token-buffer token))
        ;; Move the point at token start
        (goto-char (semantic-token-start token))
        (semantic-java-skip-spaces-forward)
        ;; If the point already at "/**" (this occurs after a doc fix)
        (if (looking-at "/\\*\\*")
            nil
          ;; Skip previous spaces
          (semantic-java-skip-spaces-backward)
          ;; Ensure point is after "*/" (javadoc block comment end)
          (condition-case nil
              (backward-char 2)
            (error nil))
          (when (looking-at "\\*/")
            ;; Move the point backward across the comment
            (forward-char 2)            ; return just after "*/"
            (forward-comment -1)        ; to skip the entire block
            ))
        ;; Verify the point is at "/**" (javadoc block comment start)
        (if (looking-at "/\\*\\*")
            (let ((p (point))
                  (c (semantic-find-doc-snarf-comment 'flex)))
              (when c
                ;; Verify that the token just following the doc
                ;; comment is the current one!
                (goto-char (semantic-flex-end c))
                (semantic-java-skip-spaces-forward)
                (when (eq token (semantic-current-nonterminal))
                  (goto-char p)
                  (semantic-find-doc-snarf-comment nosnarf))))))))

;; Local context handler
;;
(defun semantic-java-get-local-variables ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `field_declaration'
to collect tokens, such as local variables or prototypes.
This function is a Java specific `get-local-variables' override."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-bovinate-region-until-error
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration)
                      vars))))
    vars))

;;; Javadoc facilities
;;

;; Javadoc elements
;;
(defvar semantic-java-doc-line-tags nil
  "Valid javadoc line tags.
Ordered following Sun's Tag Convention at
<http://java.sun.com/products/jdk/javadoc/writingdoccomments/index.html>")

(defvar semantic-java-doc-with-name-tags nil
  "Javadoc tags which have a name.")

(defvar semantic-java-doc-with-ref-tags nil
  "Javadoc tags which have a reference.")

;; Optional javadoc tags by token category
;;
(defvar semantic-java-doc-extra-type-tags nil
  "Optional tags used in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-extra-function-tags nil
  "Optional tags used in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-extra-variable-tags nil
  "Optional tags used in field documentation.
Ordered following Sun's Tag Convention.")

;; All javadoc tags by token category
;;
(defvar semantic-java-doc-type-tags nil
  "Tags allowed in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-function-tags nil
  "Tags allowed in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-java-doc-variable-tags nil
  "Tags allowed in field documentation.
Ordered following Sun's Tag Convention.")

;; Access to Javadoc elements
;;
(defmacro semantic-java-doc-tag (name)
  "Return doc tag from NAME.
That is @NAME."
  `(concat "@" ,name))

(defsubst semantic-java-doc-tag-name (tag)
  "Return name of the doc TAG symbol.
That is TAG `symbol-name' without the leading '@'."
  (substring (symbol-name tag) 1))

(defun semantic-java-doc-keyword-before-p (k1 k2)
  "Return non-nil if javadoc keyword K1 is before K2."
  (let* ((t1   (semantic-java-doc-tag k1))
         (t2   (semantic-java-doc-tag k2))
         (seq1 (and (semantic-flex-keyword-p t1)
                    (plist-get (semantic-flex-keyword-get t1 'javadoc)
                               'seq)))
         (seq2 (and (semantic-flex-keyword-p t2)
                    (plist-get (semantic-flex-keyword-get t2 'javadoc)
                               'seq))))
    (if (and (numberp seq1) (numberp seq2))
        (<= seq1 seq2)
      ;; Unknown tags (probably custom ones) are always after official
      ;; ones and are not themselves ordered.
      (or (numberp seq1)
          (and (not seq1) (not seq2))))))

(defun semantic-java-doc-keywords-map (fun &optional property)
  "Run function FUN for each javadoc keyword.
Return the list of FUN results.  If optional PROPERTY is non nil only
call FUN for javadoc keyword which have a value for PROPERTY.  FUN
receives two arguments: the javadoc keyword and its associated
'javadoc property list. It can return any value.  Nil values are
removed from the result list."
  (delq nil
        (mapcar
         #'(lambda (k)
             (let* ((tag   (semantic-java-doc-tag k))
                    (plist (semantic-flex-keyword-get tag 'javadoc)))
               (if (or (not property) (plist-get plist property))
                   (funcall fun k plist))))
         semantic-java-doc-line-tags)))


;;; Mode setup
;;

(defun semantic-java-doc-setup ()
  "Lazy initialization of javadoc elements."
  (or semantic-java-doc-line-tags
      (setq semantic-java-doc-line-tags
            (sort (mapcar #'semantic-java-doc-tag-name
                          (semantic-flex-keywords 'javadoc))
                  #'semantic-java-doc-keyword-before-p)))

  (or semantic-java-doc-with-name-tags
      (setq semantic-java-doc-with-name-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-name)))

  (or semantic-java-doc-with-ref-tags
      (setq semantic-java-doc-with-ref-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-ref)))

  (or semantic-java-doc-extra-type-tags
      (setq semantic-java-doc-extra-type-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-extra-function-tags
      (setq semantic-java-doc-extra-function-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-extra-variable-tags
      (setq semantic-java-doc-extra-variable-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-java-doc-type-tags
      (setq semantic-java-doc-type-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k)))))

  (or semantic-java-doc-function-tags
      (setq semantic-java-doc-function-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k)))))

  (or semantic-java-doc-variable-tags
      (setq semantic-java-doc-variable-tags
            (semantic-java-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k)))))
  
  )

(provide 'semantic-java)

;;; semantic-java.el ends here
