;;; semantic-el.el --- Semantic details for Emacs Lisp

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-el.el,v 1.29 2004/06/24 00:22:19 zappo Exp $

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
(require 'semantic-bovine)
(require 'backquote)
(require 'find-func)
(eval-when-compile
  (require 'semantic-imenu)
  )

;;; Code:
(defvar semantic--elisp-parse-table
  `((bovine-toplevel
     (semantic-list
      ,(lambda (vals start end)
         (let ((tag (semantic-elisp-use-read (car vals))))
           ;; At this point, if `semantic-elisp-use-read' returned an
           ;; already expanded tag (from definitions parsed inside an
           ;; eval and compile wrapper), just pass it!
           (if (semantic--tag-expanded-p tag)
               tag
             (append tag (list start end)))))))
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
  "Convert a list of CLOS class slot PARTLIST to `variable' tags."
  (let (vars part v)
    (while partlist
      (setq part (car partlist)
            partlist (cdr partlist)
            v (semantic-tag-new-variable
               (symbol-name (car part))
               (semantic-elisp-clos-slot-property-string part :type)
               (semantic-elisp-clos-slot-property-string part :initform)
               ;; Attributes
               :protection (semantic-elisp-clos-slot-property-string
                            part :protection)
               :static-flag (equal (semantic-elisp-clos-slot-property-string
                                    part :allocation)
                                   ":class")
               :documentation (semantic-elisp-clos-slot-property-string
                               part :documentation))
            vars (cons v vars)))
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

(defvar semantic-elisp-store-documentation-in-tag nil
  "*When non-nil, store documentation strings in the created tags.")

(defun semantic-elisp-do-doc (str)
  "Return STR as a documentation string IF they are enabled."
  (when semantic-elisp-store-documentation-in-tag
    (semantic-elisp-form-to-doc-string str)))

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
      (semantic-tag-new-code "anonymous" nil))
     ((or (eq ts 'eval-and-compile)
	  (eq ts 'eval-when-compile))
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
	(semantic-tag-new-variable
	 sn nil (nth 2 rt)
	 :user-visible-flag (and doc
			    (> (length doc) 0)
			    (= (aref doc 0) ?*))
	 :constant-flag (if (eq ts 'defconst) t nil)
	 :documentation (semantic-elisp-do-doc doc)
	 )
	))
     ((or (eq ts 'defun)
	  (eq ts 'defun*)
	  (eq ts 'defsubst)
	  (eq ts 'defmacro)
	  (eq ts 'define-overload)
	  )
      ;; functions and macros
      (semantic-tag-new-function
       sn nil (semantic-elisp-desymbolify (nth 2 rt))
       :user-visible-flag (equal (car-safe (nth 4 rt)) 'interactive)
       :documentation (semantic-elisp-do-doc (nth 3 rt))
       :overloadable (eq ts 'define-overload)
       )
      )
     ((eq ts 'autoload)
      (semantic-tag-new-function
       (format "%S" (car (cdr (car (cdr rt)))))
       nil nil
       :user-visible-flag (and (nth 4 rt)
			       (not (eq (nth 4 rt) 'nil)))
       :prototype-flag t
       :documentation (semantic-elisp-do-doc (nth 3 rt)))
      )
     ((or (eq ts 'defmethod)
	  (eq ts 'defgeneric))
      ;; methods
      (let* ((a2 (nth 2 rt))
	     (a3 (nth 3 rt))
	     (args (if (listp a2) a2 a3))
	     (doc (nth (if (listp a2) 3 4) rt)))
	(semantic-tag-new-function
	 sn nil
	 (if (listp (car args))
	     (cons (symbol-name (car (car args)))
		   (semantic-elisp-desymbolify (cdr args)))
	   (semantic-elisp-desymbolify (cdr args)))
	 :parent (symbol-name
		  (if (listp (car args)) (car (cdr (car args)))))
	 :documentation (semantic-elisp-do-doc doc)
	 )))
     ((eq ts 'defadvice)
      ;; Advice
      (semantic-tag-new-function
       sn nil (semantic-elisp-desymbolify (nth 2 rt))
       )
       ;; (nth 3 rt) doc string
      )
     ((eq ts 'defclass)
      ;; classes
      (let ((docpart (nthcdr 4 rt)))
	(semantic-tag-new-type
	 sn "class"
	 (semantic-elisp-clos-args-to-semantic (nth 3 rt))
	 (semantic-elisp-desymbolify (nth 2 rt))
	 :typemodifiers (semantic-elisp-desymbolify
			 (if (not (stringp docpart))
			     docpart))
	 :documentation
	 (semantic-elisp-do-doc
	  (if (stringp (car docpart))
	      (car docpart)
	    (car (cdr (member :documentation docpart)))))
	 )
	))
     ((eq ts 'defstruct)
      ;; structs
      (semantic-tag-new-type
       sn "struct" (semantic-elisp-desymbolify (nthcdr 2 rt))
       nil ;(semantic-elisp-desymbolify (nth 2 rt))
       )
      ;; (nth 4 rt) doc string
      )
     ;; Now about a few Semantic specials?
     ((eq ts 'define-lex)
      (semantic-tag-new-function
       sn nil nil
       :lexical-analyzer-flag t
       :documentation (semantic-elisp-do-doc (nth 2 rt))
       )
      )
     ((memq ts '( define-mode-overload-implementation
                  define-mode-local-override ))
      (let ((args (nth 3 rt))
	    )
	(semantic-tag-new-function
	 sn nil
	 (when (listp args) (semantic-elisp-desymbolify args))
	 :override-function-flag t
	 :parent (format "%S" (nth 2 rt))
	 :documentation (semantic-elisp-do-doc (nth 4 rt))
	 )
	))
     ((eq ts 'defvar-mode-local)
      (semantic-tag-new-variable
       (format "%S" (nth 2 rt)) nil
       (nth 3 rt) ; default value
       :override-variable-flag t
       :parent sn
       :documentation (semantic-elisp-do-doc (nth 4 rt))
       )
      )
     ;; Now for other stuff
     ((eq ts 'require)
      (semantic-tag-new-include
       sn nil :directory (nth 2 rt)))
     ((eq ts 'provide)
      (semantic-tag-new-package
       sn (nth 3 rt)))
     (t
      ;; Other stuff
      (semantic-tag-new-code (symbol-name ts) nil)
      ))))

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

(define-mode-local-override semantic-find-dependency
  emacs-lisp-mode (tag)
  "Find the file BUFFER depends on described by TAG."
  (let ((f (file-name-sans-extension
	    (locate-library (semantic-tag-name tag)))))
    (concat f ".el")))

(defun semantic-emacs-lisp-overridable-doc (tag)
  "Return the documentation string generated for overloadable functions.
Fetch the item for TAG.  Only returns info about what symbols can be
used to perform the override."
  (if (and (eq (semantic-tag-class tag) 'function)
	   (semantic-tag-get-attribute tag :overloadable))
      ;; Calc the doc to use for the overloadable symbols.
      (overload-docstring-extension (intern (semantic-tag-name tag)))
    ""))

(defun semantic-emacs-lisp-obsoleted-doc (tag)
  "Indicate that TAG is a new name that has obsoleted  some old name.
Unfortunately, this requires that the tag in question has been loaded
into Emacs Lisp's memory."
  (let ((obsoletethis (intern-soft (semantic-tag-name tag)))
	(obsoletor nil))
    ;; This asks if our tag is available in the Emacs name space for querying.
    (when obsoletethis
      (mapatoms (lambda (a)
		  (let ((oi (get a 'byte-obsolete-info)))
		    (if (and oi (eq (car oi) obsoletethis))
			(setq obsoletor a)))))
      (if obsoletor
	  (format "\n@obsolete{%s,%s}" obsoletor (semantic-tag-name tag))
	""))))

(define-mode-local-override semantic-documentation-for-tag
  emacs-lisp-mode (tag &optional nosnarf)
  "Return the documentation string for TAG.
Optional argument NOSNARF is ignored."
  (let ((d (semantic-tag-docstring tag)))
    (when (and (not d) (semantic-tag-buffer tag))
      ;; Doc isn't in the tag itself.  Lets pull it out of the
      ;; sources.
      (let ((semantic-elisp-store-documentation-in-tag t))
        (setq tag (with-current-buffer (semantic-tag-buffer tag)
                    (goto-char (semantic-tag-start tag))
                    (semantic-elisp-use-read
                     ;; concoct a lexical token.
                     (cons (semantic-tag-start tag)
                           (semantic-tag-end tag))))
              d (semantic-tag-docstring tag))))
    (when d
      (concat
       (substitute-command-keys
        (if (and (> (length d) 0) (= (aref d 0) ?*))
            (substring d 1)
          d))
       (semantic-emacs-lisp-overridable-doc tag)
       (semantic-emacs-lisp-obsoleted-doc tag)))))

(define-mode-local-override semantic-insert-foreign-tag
  emacs-lisp-mode (tag tagfile)
  "Insert TAG from TAGFILE at point.
Attempts a simple prototype for calling or using TAG."
  (cond ((semantic-tag-of-class-p tag 'function)
	 (insert "(" (semantic-tag-name tag) " )")
	 (forward-char -1))
	(t
	 (insert (semantic-tag-name tag)))))

(define-mode-local-override semantic-tag-protection
  emacs-lisp-mode (tag &optional parent)
  "Return the protection of TAG in PARENT.
Override function for `semantic-tag-protection'."
  (let ((prot (semantic-tag-get-attribute tag :protection)))
    (cond
     ;; If a protection is not specified, AND there is a parent
     ;; data type, then it is public.
     ((and (not prot) parent) 'public)
     ((string= prot ":public") 'public)
     ((string= prot "public") 'public)
     ((string= prot ":private") 'private)
     ((string= prot "private") 'private)
     ((string= prot ":protected") 'protected)
     ((string= prot "protected") 'protected))))

(define-mode-local-override semantic-tag-static-p
  emacs-lisp-mode (tag &optional parent)
  "Return non-nil if TAG is static in PARENT class.
Overrides `semantic-nonterminal-static'."
  ;; This can only be true (theoretically) in a class where it is assigned.
  (semantic-tag-get-attribute tag :static-flag))

;;; Context parsing
;;
;; Emacs lisp is very different from C,C++ which most context parsing
;; functions are written.  Support them here.
(define-mode-local-override semantic-up-context emacs-lisp-mode
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


(define-mode-local-override semantic-get-local-variables emacs-lisp-mode
  (&optional point)
  "Return a list of local variables for POINT.
Scan backwards from point at each successive function.  For all occurances
of `let' or `let*', grab those variable names."
  (let* ((vars nil)
	 (fn nil))
    (save-excursion
      (while (setq fn (car (semantic-ctxt-current-function)))
	(when (member fn '("let" "let*"))
	  ;; Snarf variables
	  (up-list -1)
	  (forward-char 1)
	  (forward-word 1)
	  (skip-chars-forward "* \t\n")
	  (let ((varlst (read (buffer-substring (point)
						(save-excursion
						  (forward-sexp 1)
						  (point))))))
	    (while varlst
	      (let* ((oneelt (car varlst))
		     (name (if (symbolp oneelt)
			       oneelt
			     (car oneelt))))
		(setq vars (cons (semantic-tag-new-variable
				  (symbol-name name)
				  nil nil)
				 vars)))
	      (setq varlst (cdr varlst)))
	    ))
	(up-list -1)))
    (nreverse vars)))

(define-mode-local-override semantic-end-of-command emacs-lisp-mode
  ()
  "Move cursor to the end of the current command.
In emacs lisp this is easilly defined by parenthisis bounding."
  (condition-case nil
      (up-list 1)
    (error nil)))

(define-mode-local-override semantic-beginning-of-command emacs-lisp-mode
  ()
  "Move cursor to the beginning of the current command.
In emacs lisp this is easilly defined by parenthisis bounding."
  (condition-case nil
      (progn
        (up-list -1)
        (forward-char 1))
    (error nil)))

(define-mode-local-override semantic-ctxt-current-symbol emacs-lisp-mode
  (&optional point)
  "List the symbol under point."
  (save-excursion
    (if point (goto-char point))
    (require 'thingatpt)
    (let ((sym (thing-at-point 'symbol)))
      (if sym (list sym)))
    ))

(define-mode-local-override semantic-ctxt-current-function emacs-lisp-mode
  (&optional point)
  "Return a string which is the current function being called."
  (save-excursion
    (if point (goto-char point) (setq point (point)))
    ;; (semantic-beginning-of-command)
    (if (condition-case nil
	    (and (save-excursion
		   (up-list -2)
		   (looking-at "(("))
		 (save-excursion
		   (up-list -3)
		   (looking-at "(let")))
	  (error nil))
	;; This is really a let statement, not a function.
	nil
      (let ((fun (condition-case nil
		     (save-excursion
		       (up-list -1)
		       (forward-char 1)
		       (buffer-substring-no-properties
			(point) (progn (forward-sexp 1)
				       (point))))
		   (error nil))
		 ))
	(when fun
	  ;; Do not return FUN IFF the cursor is on FUN.
	  ;; Huh?  Thats because if cursor is on fun, it is
	  ;; the current symbol, and not the current function.
	  (if (save-excursion
		(condition-case nil
		    (progn (forward-sexp -1)
			   (and
			    (looking-at (regexp-quote fun))
			    (<= point (+ (point) (length fun))))
			   )
		  (error t)))
	      nil
	    ;; We are ok, so get it.
	    (list fun))
	  ))
      )))

(define-mode-local-override semantic-ctxt-current-assignment emacs-lisp-mode
  (&optional point)
  "What is the variable being assigned into at POINT?"
  (save-excursion
    (if point (goto-char point))
    (let ((fn (semantic-ctxt-current-function point))
	  (point (point)))
      ;; We should never get lists from here.
      (if fn (setq fn (car fn)))
      (cond 
       ;; SETQ 
       ((and fn (or (string= fn "setq") (string= fn "set")))
	(save-excursion
	  (condition-case nil
	      (let ((count 0)
		    (lastodd nil)
		    (start nil))
		(up-list -1)
		(down-list 1)
		(forward-sexp 1)
		;; Skip over sexp until we pass point.
		(while (< (point) point)
		  (setq count (1+ count))
		  (forward-comment 1)
		  (setq start (point))
		  (forward-sexp 1)
		  (if (= (% count 2) 1)
		      (setq lastodd
			    (buffer-substring-no-properties start (point))))
		  )
		(if lastodd (list lastodd))
		)
	    (error nil))))
       ;; This obscure thing finds let statements.
       ((condition-case nil
	    (and 
	     (save-excursion
	       (up-list -2)
	       (looking-at "(("))
	     (save-excursion
	       (up-list -3)
	       (looking-at "(let")))
	  (error nil))
	(save-excursion
	  (semantic-beginning-of-command)
	  ;; Use func finding code, since it is the same format.
	  (semantic-ctxt-current-symbol)))
       ;; 
       ;; DEFAULT- nothing
       (t nil))
      )))

(define-mode-local-override semantic-ctxt-current-argument emacs-lisp-mode
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

(define-mode-local-override semantic-ctxt-current-class-list emacs-lisp-mode
  (&optional point)
  "Return a list of tag classes allowed at POINT.
Emacs Lisp knows much more about the class of the tag needed to perform
completion than some langauges.  We distincly know if we are to be
a function name, variable name, or any type of symbol.  We could identify
fields and such to, but that is for some other day."
  (save-excursion
    (if point (goto-char point))
    (setq point (point))
    (condition-case nil
	(let ((count 0))
	  (up-list -1)
	  (forward-char 1)
	  (while (< (point) point)
	    (setq count (1+ count))
	    (forward-sexp 1))
	  (if (= count 1)
	      '(function)
	    '(variable))
	  )
      (error '(variable)))
    ))

(define-mode-local-override semantic-tag-include-filename emacs-lisp-mode
  (tag)
  "Return the name of the tag with .el appended.
If there is a detail, prepend that directory."
  (let ((name (semantic-tag-name tag))
	(detail (semantic-tag-get-attribute tag :directory)))
    (concat (expand-file-name name detail) ".el")))

(define-mode-local-override semantic-format-tag-abbreviate emacs-lisp-mode
  (tag &optional parent color)
  "Return an abbreviated string describing tag."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	str)
    (cond
     ((eq class 'function)
      (concat "(" name ")"))
     (t
      (semantic-format-tag-abbreviate-default tag parent color)))))
  
(define-mode-local-override semantic-format-tag-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a prototype string describing tag.
In Emacs Lisp, a prototype for something may start (autoload ...).
This is certainly not expected if this is used to display a summary.
Make up something else.  When we go to write something that needs
a real Emacs Lisp protype, we can fix it then."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	str)
    (cond
     ((eq class 'function)
      (let* ((args  (semantic-tag-function-arguments tag))
	     (argstr (semantic--format-tag-arguments args
						     #'identity
						     color)))
	(concat "(" name (if args " " "")
		argstr
		")")))
     (t
      (semantic-format-tag-prototype-default tag parent color)))))

(define-mode-local-override semantic-format-tag-concise-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a concise prototype string describing tag.
See `semantic-format-tag-prototype' for Emacs Lisp for more details."
  (semantic-format-tag-prototype tag parent color))
  
(define-mode-local-override semantic-format-tag-uml-prototype emacs-lisp-mode
  (tag &optional parent color)
  "Return a uml prototype string describing tag.
See `semantic-format-tag-prototype' for Emacs Lisp for more details."
  (semantic-format-tag-prototype tag parent color))
  
(defvar-mode-local emacs-lisp-mode semantic-lex-analyzer
  'semantic-emacs-lisp-lexer)

(defvar-mode-local emacs-lisp-mode semantic--parse-table
  semantic--elisp-parse-table)

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

(define-child-mode lisp-mode emacs-lisp-mode
  "Make `lisp-mode' inherits mode local behavior from `emacs-lisp-mode'.")

;;;###autoload
(defun semantic-default-elisp-setup ()
  "Setup hook function for Emacs Lisp files and Semantic."
  )

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)
;;;###autoload
(add-hook 'lisp-mode-hook 'semantic-default-elisp-setup)

;;;###autoload
(eval-after-load "semanticdb"
  '(require 'semanticdb-el)
  )

(provide 'semantic-el)

;;; semantic-el.el ends here
