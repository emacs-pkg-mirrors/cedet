;;; semantic-el.el --- Semantic details for Emacs Lisp

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-el.el,v 1.6 2003/01/29 03:43:44 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-ex is free software; you can redistribute it and/or modify
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
;; Use the Semantic Bovinator for Emacs Lisp

(require 'semantic)
(require 'backquote)
(require 'find-func)
(eval-when-compile
  (require 'semantic-imenu)
  )

;;; Code:
(defvar semantic-toplevel-elisp-bovine-table
  `((bovine-toplevel
     (semantic-list
      ,(lambda (vals start end)
	 (append (semantic-elisp-use-read (car vals))
		 (list start end)))))
    )
  "Top level bovination table for elisp.")

(defun semantic-elisp-desymbolify (arglist)
  "Convert symbols to strings for ARGLIST."
  (let ((out nil))
    (while arglist
      (setq out
	    (cons
	     (if (symbolp (car arglist))
		 (symbol-name (car arglist))
	       (if (and (listp (car arglist))
			(symbolp (car (car arglist))))
		   (symbol-name (car (car arglist)))
		 (format "%S" (car arglist))))
	     out)
	    arglist (cdr arglist)))
    (nreverse out)))

(defun semantic-elisp-clos-slot-property-string (slot property)
  "For SLOT, a string representing PROPERTY."
  (let ((p (member property slot)))
    (if (not p)
	nil
      (setq p (cdr p))
      (cond
       ((stringp (car p))
	(car p))
       ((or (symbolp (car p)) (listp (car p)))
	(format "%S" (car p)))
       (t nil)))))

(defun semantic-elisp-clos-args-to-semantic (partlist)
  "Convert a list of CLOS class slot PARTLIST to `variable' tokens."
  (let ((vars nil))
    (while partlist
      (let ((part (car partlist)))
	(setq vars
	      (cons
	       (list (symbol-name (car part))
		     'variable
		     (semantic-elisp-clos-slot-property-string
		      part :type)
		     (semantic-elisp-clos-slot-property-string
		      part :initform)
		     (semantic-bovinate-make-assoc-list
		      'protection
		      (semantic-elisp-clos-slot-property-string
		       part :protection)
		      'static
		      (equal (semantic-elisp-clos-slot-property-string
			      part :allocation)
			     ":class")
		      )
		     (semantic-elisp-clos-slot-property-string
		      part :documentation))
	       vars)))
      (setq partlist (cdr partlist)))
    (nreverse vars)))

(defun semantic-elisp-form-to-doc-string (form)
  "After reading a form FORM, covert it to a doc string.
For Emacs Lisp, sometimes that string is non-existant.
Recently discovered, sometimes it is a form which is evaluated
at compile time, permitting compound strings."
  (cond ((stringp form) form)
	((and (listp form) (eq (car form) 'concat)
	      (stringp (nth 1 form)))
	 (nth 1 form))
	(t nil)))

(defun semantic-elisp-use-read (sl)
  "Use `read' on the semantic list SL.
Return a bovination list to use."
  (let* ((rt (read (buffer-substring (car sl) (cdr sl)))) ; read text
	 (ts (car rt)) ; type symbol
	 (tss (nth 1 rt))
	 (ss (if (not (listp tss)) tss
	       (if (eq (car tss) 'quote)
		   (nth 1 tss)
		 (car tss))))
	 (sn (format "%S" ss))
	 )
    (cond
     ((listp ts)
      ;; If the first elt is a list, then it is some arbitrary code.
      (list "anonymous" 'code))
     ((eq ts 'eval-and-compile)
      ;; Eval and compile can be wrapped around definitions, such as in
      ;; eieio.el, so splice it's parts back into the main list.
      (condition-case foo
	  (semantic-parse-region (car sl) (cdr sl) nil 1)
	(error (message "MUNGE: %S" foo)
	       nil))
      )
     ((or (eq ts 'defvar)
	  (eq ts 'defconst)
	  (eq ts 'defcustom)
	  (eq ts 'defface)
	  (eq ts 'defimage))
      (let ((doc (semantic-elisp-form-to-doc-string (nth 3 rt))))
        ;; Variables and constants
        (list sn 'variable nil (nth 2 rt)
              (semantic-bovinate-make-assoc-list
               'const (if (eq ts 'defconst) t nil)
               'user-visible (and doc
                                  (> (length doc) 0)
                                  (= (aref doc 0) ?*))
               )
              doc)
        ))
     ((or (eq ts 'defun)
	  (eq ts 'defun*)
	  (eq ts 'defsubst)
	  (eq ts 'defmacro)
	  (eq ts 'define-overload)
	  )
      ;; functions and macros
      (list sn 'function nil (semantic-elisp-desymbolify (nth 2 rt))
	    (semantic-bovinate-make-assoc-list
	     'user-visible (equal (car-safe (nth 4 rt)) 'interactive)
	     )
	    (nth 3 rt))
      )
     ((eq ts 'autoload)
      (list (format "%S" (car (cdr (car (cdr rt)))))
	    'function
	    nil nil
	    (semantic-bovinate-make-assoc-list
	     'use-visible (and (nth 4 rt)
			       (not (eq (nth 4 rt) 'nil)))
	     'prototype t)
	    (nth 3 rt))
      )
     ((or (eq ts 'defmethod)
	  (eq ts 'defgeneric))
      ;; methods
      (let* ((a2 (nth 2 rt))
	     (a3 (nth 3 rt))
	     (args (if (listp a2) a2 a3))
	     (doc (nth (if (listp a2) 3 4) rt)))
	(list sn 'function nil
	      (if (listp (car args))
		  (cons (symbol-name (car (car args)))
			(semantic-elisp-desymbolify (cdr args)))
		(semantic-elisp-desymbolify (cdr args)))
	      (semantic-bovinate-make-assoc-list
	       'parent (symbol-name
			(if (listp (car args)) (car (cdr (car args))))))
	      doc)
	))
     ((eq ts 'defadvice)
      ;; Advice
      (list sn 'function nil (semantic-elisp-desymbolify (nth 2 rt))
	    nil (nth 3 rt)))
     ((eq ts 'defclass)
      ;; classes
      (let ((docpart (nthcdr 4 rt)))
	(list sn 'type "class"
	      (semantic-elisp-clos-args-to-semantic (nth 3 rt))
	      (semantic-elisp-desymbolify (nth 2 rt))
	      (semantic-bovinate-make-assoc-list
	       'typemodifiers
	       (semantic-elisp-desymbolify
		(if (not (stringp docpart))
		    docpart))
	       )
	      (if (stringp (car docpart))
		  (car docpart)
		(car (cdr (member :documentation docpart))))))
      )
     ((eq ts 'defstruct)
      ;; structs
      (list sn 'type "struct" (semantic-elisp-desymbolify (nthcdr 2 rt))
	    nil ;(semantic-elisp-desymbolify (nth 2 rt))
	    nil (nth 4 rt))
      )
     ;; Now about a few Semantic specials?
     ((eq ts 'define-lex)
      (list sn 'function nil nil
	    (semantic-bovinate-make-assoc-list
	     'lexical-analyzer t)
	    nil))
     ((eq ts 'define-mode-overload-implementation)
      (let ((args (nth 3 rt))
	    )
	(list sn 'function nil
	      (when (listp args)
		(semantic-elisp-desymbolify args))
	      (semantic-bovinate-make-assoc-list
	       'override-function t
	       'parent (format "%S" (nth 2 rt))
	       )
	      nil)))
     ((eq ts 'defvar-mode-local)
      (list (format "%S" (nth 2 rt)) 'variable nil
	    (nth 3 rt) ; default value
	    (semantic-bovinate-make-assoc-list
	     'override-variable t
	     'parent sn
	     )
	    (nth 4 rt)))
     ;; Now for other stuff
     ((eq ts 'require)
      (list sn 'include nil nil))
     ((eq ts 'provide)
      (list sn 'package (nth 3 rt) nil))
     (t
      ;; Other stuff
      (list (symbol-name ts) 'code)
      ))))

(defun semantic-expand-elisp-nonterminal (nonterm)
  "Expand Emacs Lisp nonterminals.
Finds compound nonterminals embedded in sub-lists.
Argument NONTERM is the nonterminal to test for expansion."
  (if (semantic-token-p (car nonterm))
      (progn
        ;; Nuke the overlay from the end.
        ;; For some reason, it takes token in reverse order.
        (setq nonterm (nreverse nonterm))
        ;; Don't forget to delete the overlay from its buffer!
        ;; Otherwise `semantic-find-nonterminal-by-overlay' will
        ;; return this invalid NONTERM as a token.
        (if (semantic-overlay-p (car nonterm))
            (semantic-overlay-delete (car nonterm)))
      (cdr (cdr nonterm)))
    nil))

(define-lex semantic-emacs-lisp-lexer
  "A simple lexical analyzer for Emacs Lisp.
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

(define-mode-overload-implementation semantic-find-dependency
  emacs-lisp-mode (token)
  "Find the file BUFFER depends on described by TOKEN."
  (let ((f (file-name-sans-extension
	    (locate-library (semantic-token-name token)))))
    (concat f ".el")))

(define-mode-overload-implementation semantic-find-documentation
  emacs-lisp-mode (token &optional nosnarf)
  "Return the documentation string for TOKEN.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-token-docstring token)))
    (if (and d (> (length d) 0) (= (aref d 0) ?*))
	(substring d 1)
      d)))

(define-mode-overload-implementation semantic-insert-foreign-token
  emacs-lisp-mode (token tokenfile)
  "Insert TOKEN from TOKENFILE at point.
Attempts a simple prototype for calling or using TOKEN."
  (cond ((eq (semantic-token-token token) 'function)
	 (insert "(" (semantic-token-name token) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-token-name token)))))

(define-mode-overload-implementation semantic-nonterminal-protection
  emacs-lisp-mode (token &optional parent)
  "Return the protection of TOKEN in PARENT.
Override function for `semantic-nonterminal-protection'."
  (let ((prot (semantic-token-extra-spec token 'protection)))
    (cond
     ((not prot) 'public)
     ((string= prot ":public") 'public)
     ((string= prot "public") 'public)
     ((string= prot ":private") 'private)
     ((string= prot "private") 'private)
     ((string= prot ":protected") 'protected)
     ((string= prot "protected") 'protected))))

(define-mode-overload-implementation semantic-elisp-nonterminal-static
  emacs-lisp-mode (token &optional parent)
  "Return non-nil if TOKEN is static in PARENT class.
Overrides `semantic-nonterminal-static'."
  ;; This can only be true (theoretically) in a class where it is assigned.
  (semantic-token-extra-spec token 'static))

;;; Context parsing
;;
;; Emacs lisp is very different from C,C++ which most context parsing
;; functions are written.  Support them here.
(define-mode-overload-implementation semantic-up-context emacs-lisp-mode
  (&optional point bounds-type)
  "Move up one context in an Emacs Lisp function.
A Context in many languages is a block with it's own local variables.
In Emacs, we will move up lists and stop when one starts with one of
the following context specifiers:
  `let', `let*', `defun', `with-slots'
Returns non-nil it is not possible to go up a context."
  (let ((last-up (semantic-up-context-default)))
  (while
      (and (not (looking-at
		 "(\\(let\\*?\\|def\\(un\\|method\\|generic\\|\
define-mode-overload\\)\
\\|with-slots\\)"))
	   (not last-up))
    (setq last-up (semantic-up-context-default)))
  last-up))


(define-mode-overload-implementation semantic-get-local-variables emacs-lisp-mode
  (&optional point)
  "Do not collect local variables.
Local variables are useful for dynamic completion and other things that
are staticly typed.  Since lisp is not staticly typed, this is not
a useful use of CPU cycles."
  nil)

(define-mode-overload-implementation semantic-end-of-command emacs-lisp-mode
  ()
  "Move cursor to the end of the current command.
In emacs lisp this is easilly defined by parenthisis bounding."
  (condition-case nil
      (up-list 1)
    (error nil)))

(define-mode-overload-implementation semantic-beginning-of-command emacs-lisp-mode
  ()
  "Move cursor to the beginning of the current command.
In emacs lisp this is easilly defined by parenthisis bounding."
  (condition-case nil
      (progn
        (up-list -1)
        (forward-char 1))
    (error nil)))

(define-mode-overload-implementation semantic-ctxt-current-symbol emacs-lisp-mode
  (&optional point)
  "List the symbol under point."
  (save-excursion
    (if point (goto-char point))
    (require 'thingatpt)
    (thing-at-point 'symbol)))

(define-mode-overload-implementation semantic-ctxt-current-assignment emacs-lisp-mode
  (&optional point)
  "What is the variable being assigned into at POINT?
Don't implement this."
  nil)

(define-mode-overload-implementation semantic-ctxt-current-function emacs-lisp-mode
  (&optional point)
  "Return a string which is the current function being called."
  (save-excursion
    (if point (goto-char point))
    (semantic-beginning-of-command)
    (function-at-point)
    ))

(define-mode-overload-implementation semantic-ctxt-current-argument emacs-lisp-mode
  (&optional point)
  "Return the index into the argument the cursor is in, or nil."
  (save-excursion
    (if point (goto-char point))
    (if (looking-at "\\<\\w")
	(forward-char 1))
    (let ((count 0))
      (while (condition-case nil
		 (progn
		   (forward-sexp -1)
		   t)
	       (error nil))
	(setq count (1+ count)))
      (cond ((= count 0)
	     0)
	    (t (1- count))))
    ))


(defvar-mode-local emacs-lisp-mode semantic-lex-analyzer
  'semantic-emacs-lisp-lexer)

(defvar-mode-local emacs-lisp-mode semantic-toplevel-bovine-table
  semantic-toplevel-elisp-bovine-table)

(defvar-mode-local emacs-lisp-mode semantic-expand-nonterminal
  'semantic-expand-elisp-nonterminal)

(defvar-mode-local emacs-lisp-mode semantic-function-argument-separator
  " ")

(defvar-mode-local emacs-lisp-mode semantic-function-argument-separation-character
  " ")

(defvar-mode-local emacs-lisp-mode semantic-symbol->name-assoc-list
  '(
    (type     . "Types")
    (variable . "Variables")
    (function . "Defuns")
    (include  . "Requires")
    (package  . "Provides")
    ))

(defvar-mode-local emacs-lisp-mode imenu-create-index-function
  'semantic-create-imenu-index)

;;;###autoload
(defun semantic-default-elisp-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  )

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

;;;###autoload
(eval-after-load "semanticdb"
  '(require 'semanticdb-el)
  )

(provide 'semantic-el)

;;; semantic-el.el ends here
