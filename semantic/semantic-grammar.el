;;; semantic-grammar.el --- Major mode framework for Semantic grammars
;;
;; Copyright (C) 2002, 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: semantic-grammar.el,v 1.39 2003/08/29 07:55:58 ponced Exp $
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
;; Major mode framework for editing Semantic's input grammar files.

;;; History:
;;

;;; Code:
(require 'wisent-bovine)
(require 'semantic-grammar-wy)
(require 'sformat)

(eval-when-compile
  (require 'font-lock)
  (require 'semantic-edit))

;;;;
;;;; Set up lexer
;;;;

;;; Analyzers
;;
(define-lex-regex-analyzer semantic-grammar-lex-prologue
  "Detect and create a prologue token."
  "\\<%{"
  ;; Zing to the end of this brace block.
  (semantic-lex-push-token
   (semantic-lex-token
    'PROLOGUE (point)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'PROLOGUE
        (forward-char)
        (forward-sexp 1)
        (point))))))

(defsubst semantic-grammar-epilogue-start ()
  "Return the start position of the grammar epilogue."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*\\<%%\\>\\s-*$" nil t 2)
        (match-beginning 0)
      (1+ (point-max)))))

(define-lex-regex-analyzer semantic-grammar-lex-epilogue
  "Detect and create an epilogue or percent-percent token."
  "\\<%%\\>"
  (let ((start (match-beginning 0))
        (end   (match-end 0))
        (class 'PERCENT_PERCENT))
    (when (>= start (semantic-grammar-epilogue-start))
      (setq class 'EPILOGUE
            end   (point-max)))
    (semantic-lex-push-token
     (semantic-lex-token class start end))))

(define-lex-regex-analyzer semantic-grammar-lex-symbol
  "Detect and create an identifier or keyword token."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0))
        'SYMBOL)
    (match-beginning 0)
    (match-end 0))))

(define-lex-regex-analyzer semantic-grammar-lex-string
  "Detect and create a string token."
  "\\s\""
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'STRING (point)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'STRING
        (forward-sexp 1)
        (point))))))

(defconst semantic-grammar-lex-c-char-re "'\\s\\?.'"
  "Regexp matching C-like character literals.")

(define-lex-simple-regex-analyzer semantic-grammar-lex-char
  "Detect and create a C-like character token."
  semantic-grammar-lex-c-char-re 'CHARACTER)

(define-lex-block-analyzer semantic-grammar-lex-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE)))

(define-lex-analyzer semantic-grammar-lex-sexp
  "Detect and create an s-expression token."
  t
  (semantic-lex-push-token
   (semantic-lex-token
    'SEXP
    (match-beginning 0)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'SEXP
        (forward-sexp 1)
        (point))))))

(define-lex-regex-analyzer semantic-grammar-lex-prefixed-list
  "Detect and create a prefixed list token."
  "\\s'\\s-*("
  (semantic-lex-push-token
   (semantic-lex-token
    'PREFIXED_LIST
    (match-beginning 0)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'PREFIXED_LIST
        (forward-sexp 1)
        (point))))))

;;; Lexer
;;
(define-lex semantic-grammar-lexer
  "Lexical analyzer that handles Semantic grammar buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-newline
  semantic-lex-ignore-whitespace
  ;; Must detect prologue/epilogue before other symbols/keywords!
  semantic-grammar-lex-prologue
  semantic-grammar-lex-epilogue
  semantic-grammar-lex-symbol
  semantic-grammar-lex-char
  semantic-grammar-lex-string
  ;; Must detect comments after strings because `comment-start-skip'
  ;; regexp match semicolons inside strings!
  semantic-lex-ignore-comments
  ;; Must detect prefixed list before punctuation because prefix chars
  ;; are also punctuations!
  semantic-grammar-lex-prefixed-list
  ;; Must detect punctuations after comments because the semicolon can
  ;; be a punctuation or a comment start!
  semantic-lex-punctuation-type
  semantic-grammar-lex-blocks
  semantic-grammar-lex-sexp)

;;; Test the lexer
;;
(defun semantic-grammar-lex-buffer ()
  "Run `semantic-grammar-lex' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'semantic-grammar-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer (get-buffer-create "*semantic-grammar-lex*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;;
;;;; Semantic action expansion
;;;;

(defun semantic-grammar-ASSOC (&rest args)
  "Return expansion of built-in ASSOC expression.
ARGS are ASSOC's key value list."
  (let ((key t))
    `(semantic-tag-make-assoc-list
      ,@(mapcar #'(lambda (i)
                    (prog1
                        (if key
                            (list 'quote i)
                          i)
                      (setq key (not key))))
                args))))

(defsubst semantic-grammar-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst semantic-grammar-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

;;;;
;;;; API to access grammar tags
;;;;

(defvar-mode-local semantic-grammar-mode
  senator-add-log-tokens '(nonterminal put token keyword)
  "List of nonterminal tags used with add-log.")

(define-mode-overload-implementation semantic-tag-components
  semantic-grammar-mode (tag)
  "Return the children of tag TAG."
  (semantic-tag-get-attribute tag :children))

(defun semantic-grammar-first-tag-name (class)
  "Return the name of the first tag of class CLASS found.
Warn if other tags of class CLASS exist."
  (let* ((tags (semantic-find-tags-by-class
                class (current-buffer))))
    (if tags
        (prog1
            (semantic-tag-name (car tags))
          (if (cdr tags)
              (message "*** Ignore all but first declared %s"
                       class))))))

(defun semantic-grammar-tag-symbols (class)
  "Return the list of symbols defined in tags of class CLASS.
That is tag names plus names defined in tag attribute `:rest'."
  (let* ((tags (semantic-find-tags-by-class
                class (current-buffer))))
    (apply 'append
           (mapcar
            #'(lambda (tag)
                (mapcar
                 'intern
                 (cons (semantic-tag-name tag)
                       (semantic-tag-get-attribute tag :rest))))
            tags))))

(defsubst semantic-grammar-item-text (item)
  "Return the readable string form of ITEM."
  (if (string-match semantic-grammar-lex-c-char-re item)
      (concat "?" (substring item 1 -1))
    item))

(defsubst semantic-grammar-item-value (item)
  "Return symbol or character value of ITEM string."
  (if (string-match semantic-grammar-lex-c-char-re item)
      (read (concat "?" (substring item 1 -1)))
    (intern item)))

(defun semantic-grammar-prologue ()
  "Return grammar prologue code as a string value."
  (let ((tag (semantic-find-first-tag-by-name
              "prologue"
              (semantic-find-tags-by-class 'code (current-buffer)))))
    (if tag
        (save-excursion
          (concat
           (buffer-substring
            (progn
              (goto-char (semantic-tag-start tag))
              (skip-chars-forward "%{\r\n\t ")
              (point))
            (progn
              (goto-char (semantic-tag-end tag))
              (skip-chars-backward "\r\n\t %}")
              (point)))
           "\n"))
      "")))

(defun semantic-grammar-epilogue ()
  "Return grammar epilogue code as a string value."
  (let ((tag (semantic-find-first-tag-by-name
              "epilogue"
              (semantic-find-tags-by-class 'code (current-buffer)))))
    (if tag
        (save-excursion
          (concat
           (buffer-substring
            (progn
              (goto-char (semantic-tag-start tag))
              (skip-chars-forward "%\r\n\t ")
              (point))
            (progn
              (goto-char (semantic-tag-end tag))
              (skip-chars-backward "\r\n\t")
              ;; If a grammar footer is found, skip it.
              (re-search-backward "^;;;\\s-+\\S-+\\s-+ends here"
                                  (save-excursion
                                    (beginning-of-line)
                                    (point))
                                  t)
              (skip-chars-backward "\r\n\t")
              (point)))
           "\n"))
      "")))

(defsubst semantic-grammar-buffer-file (&optional buffer)
  "Return name of file sans directory BUFFER is visiting.
No argument or nil as argument means use the current buffer."
  (file-name-nondirectory (buffer-file-name buffer)))

(defun semantic-grammar-package ()
  "Return the %package value as a string.
If there is no %package statement in the grammar, return a default
package name derived from the grammar file name.  For example, the
default package name for the grammar file foo.wy is foo-wy, and for
foo.by it is foo-by."
  (or (semantic-grammar-first-tag-name 'package)
      (let ((file (semantic-grammar-buffer-file)))
        (if (string-match (format "\\([.]\\)%s\\'"
                                  (file-name-extension file))
                          file)
            (replace-match "-" nil nil file 1)))))

(defsubst semantic-grammar-languagemode ()
  "Return the %languagemode value as a list of symbols or nil."
  (semantic-grammar-tag-symbols 'languagemode))

(defsubst semantic-grammar-start ()
  "Return the %start value as a list of symbols or nil."
  (semantic-grammar-tag-symbols 'start))

(defsubst semantic-grammar-scopestart ()
  "Return the %scopestart value as a symbol or nil."
  (intern (or (semantic-grammar-first-tag-name 'scopestart) "nil")))

(defsubst semantic-grammar-quotemode ()
  "Return the %quotemode value as a symbol or nil."
  (intern (or (semantic-grammar-first-tag-name 'quotemode) "nil")))

(defsubst semantic-grammar-keywords ()
  "Return the language keywords.
That is an alist of (VALUE . TOKEN) where VALUE is the string value of
the keyword and TOKEN is the terminal symbol identifying the keyword."
  (mapcar
   #'(lambda (key)
       (cons (semantic-tag-get-attribute key :value)
             (intern (semantic-tag-name key))))
   (semantic-find-tags-by-class 'keyword (current-buffer))))

(defun semantic-grammar-keyword-properties (keywords)
  "Return the list of KEYWORDS properties."
  (let ((puts (semantic-find-tags-by-class
               'put (current-buffer)))
        put keys key plist assoc pkey pval props)
    (while puts
      (setq put   (car puts)
            puts  (cdr puts)
            keys  (mapcar
                   'intern
                   (cons (semantic-tag-name put)
                         (semantic-tag-get-attribute put :rest))))
      (while keys
        (setq key   (car keys)
              keys  (cdr keys)
              assoc (rassq key keywords))
        (if (null assoc)
            nil ;;(message "*** %%put to undefined keyword %s ignored" key)
          (setq key   (car assoc)
                plist (semantic-tag-get-attribute put :value))
          (while plist
            (setq pkey  (intern (caar plist))
                  pval  (read (cdar plist))
                  props (cons (list key pkey pval) props)
                  plist (cdr plist))))))
    props))

(defun semantic-grammar-tokens ()
  "Return defined lexical tokens.
That is an alist (TYPE . DEFS) where type is a %token <type> symbol
and DEFS is an alist of (TOKEN . VALUE).  TOKEN is the terminal symbol
identifying the token and VALUE is the string value of the token or
nil."
  (let (tags alist assoc tag type term names value)

    ;; Check for <type> in %left, %right & %nonassoc declarations
    (setq tags (semantic-find-tags-by-class
                'assoc (current-buffer)))
    (while tags
      (setq tag  (car tags)
            tags (cdr tags))
      (when (setq type (semantic-tag-type tag))
        (setq names (semantic-tag-get-attribute tag :value)
              assoc (assoc type alist))
        (or assoc (setq assoc (list type)
                        alist (cons assoc alist)))
        (while names
          (setq term  (car names)
                names (cdr names))
          (or (string-match semantic-grammar-lex-c-char-re term)
              (setcdr assoc (cons (list (intern term))
                                  (cdr assoc)))))))

    ;; Then process %token declarations so they can override any
    ;; previous specifications
    (setq tags (semantic-find-tags-by-class
                'token (current-buffer)))
    (while tags
      (setq tag  (car tags)
            tags (cdr tags))
      (setq names (cons (semantic-tag-name tag)
                        (semantic-tag-get-attribute tag :rest))
            type  (or (semantic-tag-type tag) "<no-type>")
            value (semantic-tag-get-attribute tag :value)
            assoc (assoc type alist))
      (or assoc (setq assoc (list type)
                      alist (cons assoc alist)))
      (while names
        (setq term  (intern (car names))
              names (cdr names))
        (setcdr assoc (cons (cons term value) (cdr assoc)))))
    alist))

(defun semantic-grammar-token-properties (tokens)
  "Return the list of properties of lexical tokens TOKENS."
  (let ((puts (semantic-find-tags-by-class
               'put (current-buffer)))
        put keys key plist assoc pkey pval props)
    (while puts
      (setq put   (car puts)
            puts  (cdr puts)
            keys  (cons (semantic-tag-name put)
                        (semantic-tag-get-attribute put :rest)))
      (while keys
        (setq key   (car keys)
              keys  (cdr keys)
              assoc (assoc key tokens))
        (if (null assoc)
            nil ;; (message "*** %%put to undefined token %s ignored" key)
          (setq key   (car assoc)
                plist (semantic-tag-get-attribute put :value))
          (while plist
            (setq pkey  (intern (caar plist))
                  pval  (read (cdar plist))
                  props (cons (list key pkey pval) props)
                  plist (cdr plist))))))
    props))

(defun semantic-grammar-use-macros ()
  "Return macro definitions from %use-macros statements.
Also load the specified macro libraries."
  (let (lib defs)
    (dolist (tag (semantic-find-tags-by-class 'macro (current-buffer)))
      (setq lib (intern (semantic-tag-type tag)))
      (condition-case nil
          ;;(load lib) ;; Be sure to use the latest macro library.
          (require lib)
        (error nil))
      (dolist (mac (semantic-tag-get-attribute tag :value))
        (push (cons (intern mac)
                    (intern (format "%s-%s" lib mac)))
              defs)))
    (nreverse defs)))

(defvar semantic-grammar-macros nil
  "List of associations (MACRO-NAME . EXPANDER).")
(make-variable-buffer-local 'semantic-grammar-macros)

(defun semantic-grammar-macros ()
  "Build and return the alist of defined macros."
  (append
   ;; Definitions found in tags.
   (semantic-grammar-use-macros)
   ;; Other pre-installed definitions.
   semantic-grammar-macros))

;;;;
;;;; Overloaded functions that build parser data.
;;;;

;;; Keyword table builder
;;
(defun semantic-grammar-keywordtable-builder-default ()
  "Return the default value of the keyword table."
  (let ((keywords (semantic-grammar-keywords)))
    `(semantic-lex-make-keyword-table
      ',keywords
      ',(semantic-grammar-keyword-properties keywords))))

(define-overload semantic-grammar-keywordtable-builder ()
  "Return the keyword table table value.")

;;; Token table builder
;;
(defun semantic-grammar-tokentable-builder-default ()
  "Return the default value of the table of lexical tokens."
  (let ((tokens (semantic-grammar-tokens)))
    `(semantic-lex-make-type-table
      ',tokens
      ',(semantic-grammar-token-properties tokens))))

(define-overload semantic-grammar-tokentable-builder ()
  "Return the value of the table of lexical tokens.")

;;; Parser table builder
;;
(defun semantic-grammar-parsetable-builder-default ()
  "Return the default value of the parse table."
  (error "`semantic-grammar-parsetable-builder' not defined"))

(define-overload semantic-grammar-parsetable-builder ()
  "Return the parser table value.")

;;; Parser setup code builder
;;
(defun semantic-grammar-setupcode-builder-default ()
  "Return the default value of the setup code form."
  (error "`semantic-grammar-setupcode-builder' not defined"))

(define-overload semantic-grammar-setupcode-builder ()
  "Return the parser setup code form.")

;;;;
;;;; Lisp code generation
;;;;
(defvar semantic--grammar-input-buffer  nil)
(defvar semantic--grammar-output-buffer nil)

(defsubst semantic-grammar-keywordtable ()
  "Return the variable name of the keyword table."
  (concat (file-name-sans-extension
           (semantic-grammar-buffer-file
            semantic--grammar-output-buffer))
          "--keyword-table"))

(defsubst semantic-grammar-tokentable ()
  "Return the variable name of the token table."
  (concat (file-name-sans-extension
           (semantic-grammar-buffer-file
            semantic--grammar-output-buffer))
          "--token-table"))

(defsubst semantic-grammar-parsetable ()
  "Return the variable name of the parse table."
  (concat (file-name-sans-extension
           (semantic-grammar-buffer-file
            semantic--grammar-output-buffer))
          "--parse-table"))

(defsubst semantic-grammar-setupfunction ()
  "Return the name of the parser setup function."
  (concat (file-name-sans-extension
           (semantic-grammar-buffer-file
            semantic--grammar-output-buffer))
          "--install-parser"))

(defmacro semantic-grammar-as-string (object)
  "Return OBJECT as a string value."
  `(if (stringp ,object)
       ,object
     (require 'pp)
     (pp-to-string ,object)))

(defun semantic-grammar-insert-defconst (name value docstring)
  "Insert declaration of constant NAME with VALUE and DOCSTRING."
  (let ((start (point)))
    (insert (format "(defconst %s\n%s%S)\n\n" name value docstring))
    (save-excursion
      (goto-char start)
      (indent-sexp))))

(defun semantic-grammar-insert-defun (name body docstring)
  "Insert declaration of function NAME with BODY and DOCSTRING."
  (let ((start (point)))
    (insert (format "(defun %s ()\n%S\n%s)\n\n" name docstring body))
    (save-excursion
      (goto-char start)
      (indent-sexp))))

(defconst semantic-grammar-header-template
  "\
;;; %F --- Generated parser support file

%C

;; Author: %U <%M>
;; Created: %D
;; Keywords: syntax
;; X-RCS: %V

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
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file %G.

;;; History:
;;

;;; Code:
"
  "Generated header template.")

(defconst semantic-grammar-footer-template
  "\

\(provide '%L)

;;; %F ends here
"
  "Generated footer template.")

(defun semantic-grammar-copyright-line ()
  "Return the grammar copyright line, or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^;;+[ \t]+Copyright (C) .*$"
                             ;; Search only in the four top lines
                             (save-excursion (forward-line 4) (point))
                             t)
      (match-string 0))))

(defun semantic-grammar-header ()
  "Return text of a generated standard header."
  (let ((file (semantic-grammar-buffer-file
               semantic--grammar-output-buffer))
        (gram (semantic-grammar-buffer-file))
        (date (format-time-string "%Y-%m-%d %T%z"))
        (vcid (concat "$" "Id" "$")) ;; Avoid expansion
        ;; Try to get the copyright from the input grammar, or
        ;; generate a new one if not found.
        (copy (or (semantic-grammar-copyright-line)
                  (concat (format-time-string ";; Copyright (C) %Y ")
                          user-full-name))))
    (Sformat '((?U user-full-name)
               (?M user-mail-address)
               (?F file)
               (?G gram)
               (?C copy)
               (?D date)
               (?V vcid))
             semantic-grammar-header-template)))

(defun semantic-grammar-footer ()
  "Return text of a generated standard footer."
  (let* ((file (semantic-grammar-buffer-file
                semantic--grammar-output-buffer))
         (libr (file-name-sans-extension file)))
    (Sformat '((?F file)
               (?L libr))
             semantic-grammar-footer-template)))

(defun semantic-grammar-token-data ()
  "Return the string value of the table of lexical tokens."
  (semantic-grammar-as-string
   (semantic-grammar-tokentable-builder)))

(defun semantic-grammar-keyword-data ()
  "Return the string value of the table of keywords."
  (semantic-grammar-as-string
   (semantic-grammar-keywordtable-builder)))

(defun semantic-grammar-parser-data ()
  "Return the parser table as a string value."
  (semantic-grammar-as-string
   (semantic-grammar-parsetable-builder)))

(defun semantic-grammar-setup-data ()
  "Return the parser setup code form as a string value."
  (semantic-grammar-as-string
   (semantic-grammar-setupcode-builder)))

(defcustom semantic-grammar-file-regexp "\\.[wb]y$"
  "Regexp which matches grammar source files."
  :group 'semantic
  :type 'regexp)

(defsubst semantic-grammar-noninteractive ()
  "Return non-nil if running without interactive terminal."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

(defun semantic-grammar-create-package ()
  "Create package Lisp code from grammar in current buffer."
  (interactive)
  (semantic-bovinate-toplevel t)
  (let* (
         ;; Values of the following local variables are obtained from
         ;; the grammar parsed tree in current buffer, that is before
         ;; switching to the output file.
         (package  (semantic-grammar-package))
         (output   (concat package ".el"))
         (semantic--grammar-input-buffer  (current-buffer))
         (semantic--grammar-output-buffer (find-file-noselect output))
         (header   (semantic-grammar-header))
         (prologue (semantic-grammar-prologue))
         (epilogue (semantic-grammar-epilogue))
         (footer   (semantic-grammar-footer))
         )
    (if (file-newer-than-file-p
         (buffer-file-name semantic--grammar-output-buffer)
         (buffer-file-name semantic--grammar-input-buffer))
        (message "Package `%s' is up to date." package)
      ;; Create the package
      (set-buffer semantic--grammar-output-buffer)
      (erase-buffer)
      (unless (eq major-mode 'emacs-lisp-mode)
        (emacs-lisp-mode))
      
;;;; Header + Prologue
      
      (insert header
              "\n;;; Prologue\n;;\n"
              prologue
              "\n;;; Declarations\n;;\n"
              )
      ;; Evaluate the prologue now, because it might provide definition
      ;; of grammar macro expanders.
      (eval-region (point-min) (point))
      
      (save-excursion
        
;;;; Declarations
        
        ;; `eval-defun' is not necessary to reset `defconst' values.
        (semantic-grammar-insert-defconst
         (semantic-grammar-keywordtable)
         (with-current-buffer semantic--grammar-input-buffer
           (semantic-grammar-keyword-data))
         "Table of language keywords.")
        
        (semantic-grammar-insert-defconst
         (semantic-grammar-tokentable)
         (with-current-buffer semantic--grammar-input-buffer
           (semantic-grammar-token-data))
         "Table of lexical tokens.")
        
        (semantic-grammar-insert-defconst
         (semantic-grammar-parsetable)
         (with-current-buffer semantic--grammar-input-buffer
           (semantic-grammar-parser-data))
         "Parser table.")
        
        (semantic-grammar-insert-defun
         (semantic-grammar-setupfunction)
         (with-current-buffer semantic--grammar-input-buffer
           (semantic-grammar-setup-data))
         "Setup the Semantic Parser.")
        
;;;; Epilogue & Footer
        
        (insert "\n;;; Epilogue\n;;\n"
                epilogue
                footer
                )
        
        )
      
      ;; If running in batch mode, there is nothing more to do.
      ;; Save the generated file and quit.
      (if (semantic-grammar-noninteractive)
          (let ((version-control t)
                (delete-old-versions t)
                (make-backup-files t)
                (vc-make-backup-files t))
            (save-buffer 16)
            (kill-buffer (current-buffer)))
        ;; If running interactively, eval declarations and epilogue
        ;; code, then pop to the buffer visiting the generated file.
        (eval-region (point) (point-max))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))
        ;; The generated code has been evaluated and updated into
        ;; memory.  Now find all buffers that match the major modes we
        ;; have created this language for, and force them to call our
        ;; setup function again, refreshing all semantic data, and
        ;; enabling them to work with the new code just created.
;;;; FIXME?
        ;; At this point, I don't know any user's defined setup code :-(
        ;; At least, what I can do for now, is to run the generated
        ;; parser-install function.
        (semantic-map-mode-buffers
         (semantic-grammar-setupfunction)
         (semantic-grammar-languagemode)))
      )
    ;; Return the name of the generated package file.
    output))

(defun semantic-grammar-batch-build-one-package (file)
  "Build a Lisp package from the grammar in FILE.
That is, generate Lisp code from FILE, and `byte-compile' it.
Return non-nil if there were no errors, nil if errors."
  (unless (auto-save-file-name-p file)
    ;; Create the package
    (let ((packagename
           (condition-case err
               (with-current-buffer (find-file-noselect file)
                 (semantic-grammar-create-package))
             (error
              (message "%s" (error-message-string err))
              nil))))
      (when packagename
        ;; Only byte compile if out of date
        (if (file-newer-than-file-p
             packagename (byte-compile-dest-file packagename))
            (let (;; Some complex grammar table expressions need a few
                  ;; more resources than the default.
                  (max-specpdl-size    (max 3000 max-specpdl-size))
                  (max-lisp-eval-depth (max 1000 max-lisp-eval-depth))
                  )
              ;; byte compile the resultant file
              (batch-byte-compile-file packagename))
          t)))))

;;;###autoload
(defun semantic-grammar-batch-build-packages ()
  "Build Lisp packages from grammar files on the command line.
That is, run `semantic-grammar-batch-build-one-package' for each file.
Each file is processed even if an error occurred previously.
Must be used from the command line, with `-batch'.
For example, to process grammar files in current directory, invoke:

  \"emacs -batch -f semantic-grammar-batch-build-packages .\".

See also the variable `semantic-grammar-file-regexp'."
  (or (semantic-grammar-noninteractive)
      (error "\
`semantic-grammar-batch-build-packages' must be used with -batch"
             ))
  (let ((status 0)
        ;; Remove vc from find-file-hooks.  It causes bad stuff to
        ;; happen in Emacs 20.
        (find-file-hooks (delete 'vc-find-file-hook find-file-hooks)))
    (dolist (arg command-line-args-left)
      (unless (and arg (file-exists-p arg))
        (error "Argument %s is not a valid file name" arg))
      (setq arg (expand-file-name arg))
      (if (file-directory-p arg)
          ;; Directory as argument
          (dolist (src (condition-case nil
                           (directory-files
                            arg nil semantic-grammar-file-regexp)
                         (error
                          (error "Unable to read directory files"))))
            (or (semantic-grammar-batch-build-one-package
                 (expand-file-name src arg))
                (setq status 1)))
        ;; Specific file argument
        (or (semantic-grammar-batch-build-one-package arg)
            (setq status 1))))
    (kill-emacs status)
    ))

;;;;
;;;; Macros highlighting
;;;;

(defvar semantic--grammar-macros-regexp-1 nil)
(make-variable-buffer-local 'semantic--grammar-macros-regexp-1)

(defun semantic--grammar-macros-regexp-1 ()
  "Return font-lock keyword regexp for pre-installed macro names."
  (and semantic-grammar-macros
       (not semantic--grammar-macros-regexp-1)
       (condition-case nil
           (setq semantic--grammar-macros-regexp-1
                 (concat "(\\s-*"
                         (regexp-opt
                          (mapcar #'(lambda (e) (symbol-name (car e)))
                                  semantic-grammar-macros)
                          t)
                         "\\>"))
         (error nil)))
  semantic--grammar-macros-regexp-1)

(defconst semantic--grammar-macdecl-re
  "\\<%use-macros\\>[ \t\r\n]+\\(\\sw\\|\\s_\\)+[ \t\r\n]+{"
  "Regexp that matches a macro declaration statement.")

(defvar semantic--grammar-macros-regexp-2 nil)
(make-variable-buffer-local 'semantic--grammar-macros-regexp-2)

(defun semantic--grammar-clear-macros-regexp-2 (&rest ignore)
  "Clear the cached regexp that match macros local in this grammar.
IGNORE arguments.
Added to `before-change-functions' hooks to be run before each text
change."
  (setq semantic--grammar-macros-regexp-2 nil))

(defun semantic--grammar-macros-regexp-2 ()
  "Return the regexp that match macros local in this grammar."
  (unless semantic--grammar-macros-regexp-2
    (let (macs)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward semantic--grammar-macdecl-re nil t)
          (condition-case nil
              (setq macs (nconc macs
                                (split-string
                                 (buffer-substring-no-properties
                                  (point)
                                  (progn
                                    (backward-char)
                                    (forward-list 1)
                                    (down-list -1)
                                    (point))))))
            (error nil)))
        (when macs
          (setq semantic--grammar-macros-regexp-2
                (concat "(\\s-*" (regexp-opt macs t) "\\>"))))))
  semantic--grammar-macros-regexp-2)

(defun semantic--grammar-macros-matcher (end)
  "Search for a grammar macro name to highlight.
END is the limit of the search."
  (let ((regexp (semantic--grammar-macros-regexp-1)))
    (or (and regexp (re-search-forward regexp end t))
        (and (setq regexp (semantic--grammar-macros-regexp-2))
             (re-search-forward regexp end t)))))

;;;;
;;;; Define major mode
;;;;

(defvar semantic-grammar-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\: "."     table) ;; COLON
    (modify-syntax-entry ?\> "."     table) ;; GT
    (modify-syntax-entry ?\< "."     table) ;; LT
    (modify-syntax-entry ?\| "."     table) ;; OR
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\% "w"     table) ;; Word
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    (modify-syntax-entry ?\. "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)
    (modify-syntax-entry ?\# "'"     table) ;; Prefix # (sharp)
    table)
  "Syntax table used in a Semantic grammar buffers.")

(defvar semantic-grammar-mode-hook nil
  "Hook run when starting Semantic grammar mode.")

(defvar semantic-grammar-mode-keywords-1
  `(("\\(\\<%%\\>\\|\\<%[{}]\\)"
     0 font-lock-reference-face)
    ("\\(%\\)\\(\\(\\sw\\|\\s_\\)+\\)"
     (1 font-lock-reference-face)
     (2 font-lock-keyword-face))
    ("\\<error\\>"
     0 (unless (semantic-grammar-in-lisp-p) 'bold))
    ("^\\(\\(\\sw\\|\\s_\\)+\\)[ \n\r\t]*:"
     1 font-lock-function-name-face)
    (semantic--grammar-macros-matcher
     1 ,(if (boundp 'font-lock-builtin-face)
            'font-lock-builtin-face
          'font-lock-preprocessor-face))
    ("\\$\\(\\sw\\|\\s_\\)*"
     0 font-lock-variable-name-face)
    ("<\\(\\(\\sw\\|\\s_\\)+\\)>"
     1 font-lock-type-face)
    (,semantic-grammar-lex-c-char-re
     0 ,(if (boundp 'font-lock-constant-face)
            'font-lock-constant-face
          'font-lock-string-face) t)
    ;; Must highlight :keyword here, because ':' is a punctuation in
    ;; grammar mode!
    ("[\r\n\t ]+:\\sw+\\>"
     0 font-lock-builtin-face)
    )
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords-2
  (append semantic-grammar-mode-keywords-1
          lisp-font-lock-keywords-1)
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords-3
  (append semantic-grammar-mode-keywords-1
          lisp-font-lock-keywords-2)
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-mode-keywords
  semantic-grammar-mode-keywords-1
  "Font Lock keywords used to highlight Semantic grammar buffers.")

(defvar semantic-grammar-map
  (let ((km (make-sparse-keymap)))

    (define-key km "|" 'semantic-grammar-electric-punctuation)
    (define-key km ";" 'semantic-grammar-electric-punctuation)
    (define-key km "%" 'semantic-grammar-electric-punctuation)
    (define-key km "(" 'semantic-grammar-electric-punctuation)
    (define-key km ")" 'semantic-grammar-electric-punctuation)
    (define-key km ":" 'semantic-grammar-electric-punctuation)

    (define-key km "\t"       'semantic-grammar-indent)
    (define-key km "\M-\t"    'semantic-grammar-complete)
    (define-key km "\C-c\C-c" 'semantic-grammar-create-package)
    (define-key km "\C-cm"    'semantic-grammar-find-macro-expander)
;;  (define-key km "\C-cc"    'semantic-grammar-generate-and-load)
;;  (define-key km "\C-cr"    'semantic-grammar-generate-one-rule)

    km)
  "Keymap used in `semantic-grammar-mode'.")

(defsubst semantic-grammar-in-lisp-p ()
  "Return non-nil if point is in Lisp code."
  (or (>= (point) (semantic-grammar-epilogue-start))
      (condition-case nil
          (save-excursion
            (up-list -1)
            t)
        (error nil))))

(defun semantic-grammar-edits-new-change-hook-fcn (overlay)
  "Function set into `semantic-edits-new-change-hook'.
Argument OVERLAY is the overlay created to mark the change.
When OVERLAY marks a change in the scope of a nonterminal tag extend
the change bounds to encompass the whole nonterminal tag."
  (let ((outer (car (semantic-find-tag-by-overlay-in-region
                     (semantic-edits-os overlay)
                     (semantic-edits-oe overlay)))))
    (if (semantic-tag-of-class-p outer 'nonterminal)
        (semantic-overlay-move overlay
                               (semantic-tag-start outer)
                               (semantic-tag-end outer)))))

(defun semantic-grammar-mode ()
  "Initialize a buffer for editing Semantic grammars.

\\{semantic-grammar-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-grammar-mode
        mode-name "Semantic Grammar Framework")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-start)
  (setq comment-start ";;")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table semantic-grammar-syntax-table)
  (use-local-map semantic-grammar-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'semantic-grammar-indent)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  (make-local-variable 'font-lock-multiline)
  (setq font-lock-multiline 'undecided)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((semantic-grammar-mode-keywords
           semantic-grammar-mode-keywords-1
           semantic-grammar-mode-keywords-2
           semantic-grammar-mode-keywords-3)
          nil ;; perform string/comment fontification
          nil ;; keywords are case sensitive.
          ;; This puts _ & - as a word constituant,
          ;; simplifying our keywords significantly
          ((?_ . "w") (?- . "w"))))
  ;; Setup Semantic to parse grammar
  (semantic-grammar-wy--install-parser)
  (setq semantic-lex-comment-regex ";;"
        semantic-lex-analyzer 'semantic-grammar-lexer
        semantic-type-relation-separator-character '(":")
        semantic-symbol->name-assoc-list
        '(
          (code         . "Setup Code")
          (keyword      . "Keyword")
          (token        . "Token")
          (nonterminal  . "Nonterminal")
          (rule         . "Rule")
          )
        semantic-format-face-alist
        '(
          (code         . default)
          (keyword      . font-lock-keyword-face)
          (token        . font-lock-type-face)
          (nonterminal  . font-lock-function-name-face)
          (rule         . default)
          ))
  (make-local-variable 'semantic-stickyfunc-sticky-classes)
  (setq semantic-stickyfunc-sticky-classes '(nonterminal))
  ;; Before each change, clear the cached regexp used to highlight
  ;; macros local in this grammar.
  (semantic-make-local-hook 'before-change-functions)
  (add-hook 'before-change-functions
            'semantic--grammar-clear-macros-regexp-2 nil t)
  ;; Handle safe re-parse of grammar rules.
  (semantic-make-local-hook 'semantic-edits-new-change-hooks)
  (add-hook 'semantic-edits-new-change-hooks
            'semantic-grammar-edits-new-change-hook-fcn
            nil t)
  (run-hooks 'semantic-grammar-mode-hook))

;;;;
;;;; Useful commands
;;;;

(defvar semantic-grammar-skip-quoted-syntax-table
  (let ((st (copy-syntax-table semantic-grammar-syntax-table)))
    (modify-syntax-entry ?\' "$" st)
    st)
  "Syntax table to skip a whole quoted expression in grammar code.
Consider quote as a \"paired delimiter\", so `forward-sexp' will skip
whole quoted expression.")

(defsubst semantic-grammar-backward-item ()
  "Move point to beginning of the previous grammar item."
  (forward-comment (- (point-max)))
  (if (zerop (skip-syntax-backward "."))
      (if (eq (char-before) ?\')
          (with-syntax-table
              ;; Can't be Lisp code here!  Temporarily consider quote
              ;; as a "paired delimiter", so `forward-sexp' can skip
              ;; the whole quoted expression.
              semantic-grammar-skip-quoted-syntax-table
            (forward-sexp -1))
        (forward-sexp -1))))

(defun semantic-grammar-anchored-indentation ()
  "Return indentation based on previous anchor character found."
  (let (indent)
    (save-excursion
      (while (not indent)
        (semantic-grammar-backward-item)
        (cond
         ((bobp)
          (setq indent 0))
         ((looking-at ":")
          (setq indent (current-column))
          (forward-char)
          (skip-syntax-forward "-")
          (if (eolp) (setq indent 2))
          )
         ((and (looking-at "[;%]")
               (not (looking-at "\\<%prec\\>")))
          (setq indent 0)
          ))))
    indent))

(defun semantic-grammar-do-grammar-indent ()
  "Indent a line of grammar.
When called the point is not in Lisp code."
  (let (indent n)
    (save-excursion
      (beginning-of-line)
      (skip-syntax-forward "-")
      (setq indent (current-column))
      (cond
       ((or (bobp)
            (looking-at "\\(\\w\\|\\s_\\)+\\s-*:")
            (and (looking-at "%")
                 (not (looking-at "%prec\\>"))))
        (setq n 0))
       ((looking-at ":")
        (setq n 2))
       ((and (looking-at ";;")
             (save-excursion (forward-comment (point-max))
                             (looking-at ":")))
        (setq n 1))
       (t
        (setq n (semantic-grammar-anchored-indentation))
        (unless (zerop n)
          (cond
           ((looking-at ";;")
            (setq n (1- n)))
           ((looking-at "[|;]")
            )
           (t
            (setq n (+ n 2)))))))
      (when (/= n indent)
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to n)))))

(defvar semantic-grammar-brackets-as-parens-syntax-table
  (let ((st (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}  " st)
    (modify-syntax-entry ?\} "){  " st)
    st)
  "Syntax table that consider brackets as parenthesis.
So `lisp-indent-line' will work inside bracket blocks.")

(defun semantic-grammar-do-lisp-indent ()
  "Maybe run the Emacs Lisp indenter on a line of code.
Return nil if not in a Lisp expression."
    (condition-case nil
        (save-excursion
          (beginning-of-line)
          (skip-chars-forward "\t ")
          (let ((first (point)))
            (or (>= first (semantic-grammar-epilogue-start))
                (up-list -1))
            (condition-case nil
                (while t
                  (up-list -1))
              (error nil))
            (beginning-of-line)
            (save-restriction
              (narrow-to-region (point) first)
              (goto-char (point-max))
              (with-syntax-table
                  ;; Temporarily consider brackets as parenthesis so
                  ;; `lisp-indent-line' can indent Lisp code inside
                  ;; brackets.
                  semantic-grammar-brackets-as-parens-syntax-table
                (lisp-indent-line))))
          t)
      (error nil)))

(defun semantic-grammar-indent ()
  "Indent the current line.
Use the Lisp or grammar indenter depending on point location."
  (interactive)
  (let ((orig (point))
        first)
    (or (semantic-grammar-do-lisp-indent)
        (semantic-grammar-do-grammar-indent))
    (setq first (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward "\t ")
                  (point)))
    (if (or (< orig first) (/= orig (point)))
        (goto-char first))))

(defun semantic-grammar-electric-punctuation ()
  "Insert and reindent for the symbol just typed in."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    (semantic-grammar-indent)))

(defun semantic-grammar-complete ()
  "Attempt to complete the current symbol."
  (interactive)
  (if (condition-case nil
          (progn (up-list -1) t)
        (error nil))
      ;; We are in lisp code.  Do lisp completion.
      (lisp-complete-symbol)
    ;; We are not in lisp code.  Do rule completion.
    (let* ((nonterms (semantic-find-tags-by-class 'nonterminal (current-buffer)))
           (sym (car (semantic-ctxt-current-symbol)))
           (ans (try-completion sym nonterms)))
      (cond ((eq ans t)
             ;; All done
             (message "Symbols is already complete"))
            ((and (stringp ans) (string= ans sym))
             ;; Max matchable.  Show completions.
             (let ((all (all-completions sym nonterms)))
               (with-output-to-temp-buffer "*Completions*"
                 (display-completion-list (all-completions sym nonterms)))
               ))
            ((stringp ans)
             ;; Expand the completions
             (forward-sexp -1)
             (delete-region (point) (progn (forward-sexp 1) (point)))
             (insert ans))
            (t (message "No Completions."))
            ))
    ))

;;; Macro facilities
;;

(defsubst semantic--grammar-macro-function-tag (name)
  "Search for a function tag for the grammar macro with name NAME.
Return the tag found or nil if not found."
  (car (semantic-find-tags-by-class
        'function
        (or (semantic-find-tags-by-name name (current-buffer))
            (and (featurep 'semanticdb)
                 semanticdb-current-database
                 (cdar (semanticdb-find-tags-by-name name nil t)))))))

(defsubst semantic--grammar-macro-lib-part (def)
  "Return the library part of the grammar macro defined by DEF."
  (let ((suf (format "-%s\\'" (regexp-quote (symbol-name (car def)))))
        (fun (symbol-name (cdr def))))
    (substring fun 0 (string-match suf fun))))

(defun semantic--grammar-macro-compl-elt (def &optional full)
  "Return a completion entry for the grammar macro defined by DEF.
If optional argument FULL is non-nil qualify the macro name with the
library found in DEF."
  (let ((mac (car def))
        (lib (semantic--grammar-macro-lib-part def)))
    (cons (if full
              (format "%s/%s" mac lib)
            (symbol-name mac))
          (list mac lib))))

(defun semantic--grammar-macro-compl-dict ()
  "Return a completion dictionnary of macro definitions."
  (let ((defs (semantic-grammar-macros))
        def dups dict)
    (while defs
      (setq def  (car defs)
            defs (cdr defs))
      (if (or (assoc (car def) defs) (assoc (car def) dups))
          (push def dups)
        (push (semantic--grammar-macro-compl-elt def) dict)))
    (while dups
      (setq def  (car dups)
            dups (cdr dups))
      (push (semantic--grammar-macro-compl-elt def t) dict))
    dict))

(defun semantic-grammar-find-macro-expander (macro-name library)
  "Visit the Emacs Lisp library where a grammar macro is implemented.
MACRO-NAME is a symbol that identifies a grammar macro.
LIBRARY is the name (sans extension) of the Emacs Lisp library where
to start searching the macro implementation.  Lookup in included
libraries, if necessary.
Find a function tag (in current tags table) whose name contains MACRO-NAME.
Select the buffer containing the tag's definition, and move point there."
  (interactive
   (let* ((dic (semantic--grammar-macro-compl-dict))
          (def (assoc (completing-read "Macro: " dic nil 1) dic)))
     (or (cdr def) '(nil nil))))
  (when (and macro-name library)
    (let* ((lib (format "%s.el" library))
           (buf (find-file-noselect (or (locate-library lib t) lib)))
           (tag (with-current-buffer buf
                  (semantic--grammar-macro-function-tag
                   (format "%s-%s" library macro-name)))))
      (if tag
          (progn
            (pop-to-buffer (semantic-tag-buffer tag))
            (goto-char (semantic-tag-start tag))
            (semantic-momentary-highlight-tag tag))
        (pop-to-buffer buf)
        (message "No expander found in library %s for macro %s"
                 library macro-name)))))

;;; Additional help
;;

(defvar semantic-grammar-syntax-help
  `(
    ;; Lexical Symbols
    ("symbol" . "Syntax: A symbol of alpha numeric and symbol characters")
    ("number" . "Syntax: Numeric characters.")
    ("punctuation" . "Syntax: Punctuation character.")
    ("semantic-list" . "Syntax: A list delimited by any valid list characters")
    ("open-paren" . "Syntax: Open Parenthesis character")
    ("close-paren" . "Syntax: Close Parenthesis character")
    ("string" . "Syntax: String character delimited text")
    ("comment" . "Syntax: Comment character delimited text")
    ;; Special Macros
    ("EMPTY" . "Syntax: Match empty text")
    ("ASSOC" . "Lambda Key: (ASSOC key1 value1 key2 value2 ...)")
    ("EXPAND" . "Lambda Key: (EXPAND <list id> <rule>)")
    ("EXPANDFULL" . "Lambda Key: (EXPANDFULL <list id> <rule>)")
    ;; Tag Generator Macros
    ("TAG" . "Generic Tag Generation: (TAG <name> <tag-class> [ :key value ]*)")
    ("VARIABLE-TAG" . "(VARIABLE-TAG <name> <lang-type> <default-value> [ :key value ]*)")
    ("FUNCTION-TAG" . "(FUNCTION-TAG <name> <lang-type> <arg-list> [ :key value ]*)")
    ("TYPE-TAG" . "(TYPE-TAG <name> <lang-type> <part-list> <parents> [ :key value ]*)")
    ("INCLUDE-TAG" . "(INCLUDE-TAG <name> <system-flag> [ :key value ]*)")
    ("PACKAGE-TAG" . "(PACKAGE-TAG <name> <detail> [ :key value ]*)")
    ("CODE-TAG" . "(CODE-TAG <name> <detail> [ :key value ]*)")
    ;; Special value macros
    ("$1" . "Match Value: Value from match list in slot 1")
    ("$2" . "Match Value: Value from match list in slot 2")
    ("$3" . "Match Value: Value from match list in slot 3")
    ("$4" . "Match Value: Value from match list in slot 4")
    ("$5" . "Match Value: Value from match list in slot 5")
    ("$6" . "Match Value: Value from match list in slot 6")
    ("$7" . "Match Value: Value from match list in slot 7")
    ("$8" . "Match Value: Value from match list in slot 8")
    ("$9" . "Match Value: Value from match list in slot 9")
    ;; Same, but with annoying , in front.
    (",$1" . "Match Value: Value from match list in slot 1")
    (",$2" . "Match Value: Value from match list in slot 2")
    (",$3" . "Match Value: Value from match list in slot 3")
    (",$4" . "Match Value: Value from match list in slot 4")
    (",$5" . "Match Value: Value from match list in slot 5")
    (",$6" . "Match Value: Value from match list in slot 6")
    (",$7" . "Match Value: Value from match list in slot 7")
    (",$8" . "Match Value: Value from match list in slot 8")
    (",$9" . "Match Value: Value from match list in slot 9")
    )
  "Association of syntax elements, and the corresponding help.")

(eval-when-compile
  (require 'eldoc)
  (require 'semantic-ctxt)
  (cond
   ((fboundp 'eldoc-function-argstring-from-docstring)
    (defalias 'semantic-grammar-function-argstring-from-docstring
      'eldoc-function-argstring-from-docstring)
    )
   ((fboundp 'help-split-fundoc)
    (defsubst semantic-grammar-function-argstring-from-docstring (fun)
      (help-split-fundoc (documentation fun t) fun))
    )
   (t
    (defalias 'semantic-grammar-function-argstring-from-docstring
      'ignore)
    )
   ))

(defun semantic-grammar-eldoc-get-macro-docstring (macro expander)
  "Return a one-line docstring for the given grammar MACRO.
EXPANDER is the name of the function that expands MACRO."
  (if (and (eq expander (aref eldoc-last-data 0))
           (eq 'function (aref eldoc-last-data 2)))
      (aref eldoc-last-data 1)
    (let ((doc (semantic-grammar-function-argstring-from-docstring expander)))
      (cond
       (doc
        (setq doc (car doc))
        (string-match "\\`[^ )]* ?" doc)
        (setq doc (concat "(" (substring doc (match-end 0)))))
       (t
        (setq doc (eldoc-function-argstring expander))))
      (when doc
        (setq doc (eldoc-docstring-format-sym-doc
                   macro (format "==> %s %s" expander doc)))
        (eldoc-last-data-store expander doc 'function))
      doc)))

(define-mode-overload-implementation eldoc-current-symbol-info
  semantic-grammar-mode ()
  "Display additional eldoc information about grammar syntax elements.
Syntax element is the current symbol at point.
If it is associated a help string in `semantic-grammar-syntax-help',
return that string.
If it is a macro name, return a description of the associated expander
function parameter list.
If it is a function name, return a description of this function
parameter list.
It it is a variable name, return a brief (one-line) documentation
string for the variable.
If a default description of the current context can be obtained,
return it.
Otherwise return nil."
  (let* ((elt (car (semantic-ctxt-current-symbol)))
         (val (and elt (cdr (assoc elt semantic-grammar-syntax-help)))))
    (when (and (not val) elt (semantic-grammar-in-lisp-p))
      ;; Ensure to load macro definitions before doing `intern-soft'.
      (setq val (semantic-grammar-macros)
            elt (intern-soft elt)
            val (and elt (cdr (assq elt val))))
      (cond
       ;; Grammar macro
       ((and val (fboundp val))
        (setq val (semantic-grammar-eldoc-get-macro-docstring elt val)))
       ;; Function
       ((and elt (fboundp elt))
        (setq val (eldoc-get-fnsym-args-string elt)))
       ;; Variable
       ((and elt (boundp elt))
        (setq val (eldoc-get-var-docstring elt)))
       (t nil)))
    (or val (senator-eldoc-print-current-symbol-info-default))))

(define-mode-overload-implementation semantic-abbreviate-nonterminal
  semantic-grammar-mode (tag &optional parent color)
  "Return a string abbreviation of TAG.
Optional PARENT is not used.
Optional COLOR is used to flag if color is added to the text."
  (let ((class (semantic-tag-class tag))
        (name (semantic-format-tag-name tag parent color)))
    (cond
     ((eq class 'nonterminal)
      (concat name ":"))
     ((eq class 'setting)
      "%settings%")
     ((memq class '(rule keyword))
      name)
     (t
      (concat "%" (symbol-name class) " " name)))))

(define-mode-overload-implementation semantic-summarize-nonterminal
  semantic-grammar-mode (tag &optional parent color)
  "Return a string summarizing TAG.
Optional PARENT is not used.
Optional argument COLOR determines if color is added to the text."
  (let ((class (semantic-tag-class tag))
        (name (semantic-format-tag-name tag parent color))
        (label nil)
        (desc nil))
    (cond
     ((eq class 'nonterminal)
      (setq label "Nonterminal: "
            desc (format
                  " with %d match lists."
                  (length (semantic-tag-components tag)))))
     ((eq class 'keyword)
      (setq label "Keyword: ")
      (let (summary)
        (semantic--find-tags-by-function
         #'(lambda (put)
             (unless summary
               (setq summary (cdr (assoc "summary"
                                         (semantic-tag-get-attribute
                                          put :value))))))
         ;; Get `put' tag with TAG name.
         (semantic-find-tags-by-name-regexp
          (regexp-quote (semantic-tag-name tag))
          (semantic-find-tags-by-class 'put (current-buffer))))
        (setq desc (concat " = "
                           (semantic-tag-get-attribute tag :value)
                           (if summary
                               (concat " - " (read summary))
                             "")))))
     ((eq class 'token)
      (setq label "Token: ")
      (let ((val   (semantic-tag-get-attribute tag :value))
            (names (semantic-tag-get-attribute tag :rest))
            (type  (semantic-tag-type tag)))
        (if names
            (setq name (mapconcat 'identity (cons name names) " ")))
        (setq desc (concat
                    (if type
                        (format " <%s>" type)
                      "")
                    (if val
                        (format "%s%S" val (if type " " ""))
                      "")))))
     ((eq class 'assoc)
      (setq label "Assoc: ")
      (let ((val   (semantic-tag-get-attribute tag :value))
            (type  (semantic-tag-type tag)))
        (setq desc (concat
                    (if type
                        (format " <%s>" type)
                      "")
                    (if val
                        (concat " " (mapconcat 'identity val " "))
                      "")))))
     (t
      (setq desc (semantic-format-tag-abbreviate tag parent color))))
    (if (and color label)
        (setq label (semantic--format-colorize-text label 'label)))
    (if (and color label desc)
        (setq desc (semantic--format-colorize-text desc 'comment)))
    (if label
        (concat label name desc)
      ;; Just a description is the abbreviated version
      desc)))

(provide 'semantic-grammar)

;;; semantic-grammar.el ends here
