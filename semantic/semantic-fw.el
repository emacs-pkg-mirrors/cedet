;;; semantic-fw.el --- Framework for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; X-CVS: $Id: semantic-fw.el,v 1.14 2003/03/11 12:53:44 ponced Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; Semantic has several core features shared across it's lex/parse/util
;; stages.  This used to clutter semantic.el some.  These routines are all
;; simple things that are not parser specific, but aid in making
;; semantic flexible and compatible amongst different Emacs platforms.

;;; No Requirements.

(load "semantic-al" nil t)

;;; Code:
;;

;;; Compatibility
;;
(if (featurep 'xemacs)
    (progn
      (defalias 'semantic-overlay-live-p 'extent-live-p)
      (defalias 'semantic-make-overlay 'make-extent)
      (defalias 'semantic-overlay-put 'set-extent-property)
      (defalias 'semantic-overlay-get 'extent-property)
      (defalias 'semantic-overlay-properties 'extent-properties)
      (defalias 'semantic-overlay-move 'set-extent-endpoints)
      (defalias 'semantic-overlay-delete 'delete-extent)
      (defalias 'semantic-overlays-at
        (lambda (pos) (extent-list nil pos pos)))
      (defalias 'semantic-overlays-in
	(lambda (beg end) (extent-list nil beg end)))
      (defalias 'semantic-overlay-buffer 'extent-buffer)
      (defalias 'semantic-overlay-start 'extent-start-position)
      (defalias 'semantic-overlay-end 'extent-end-position)
      (defalias 'semantic-overlay-next-change 'next-extent-change)
      (defalias 'semantic-overlay-previous-change 'previous-extent-change)
      (defalias 'semantic-overlay-lists
	(lambda () (list (extent-list))))
      (defalias 'semantic-overlay-p 'extentp)
      (defun semantic-read-event ()
        (let ((event (next-command-event)))
          (if (key-press-event-p event)
              (let ((c (event-to-character event)))
                (if (char-equal c (quit-char))
                    (keyboard-quit)
                  c)))
          event))
      )
  (defalias 'semantic-overlay-live-p 'overlay-buffer)
  (defalias 'semantic-make-overlay 'make-overlay)
  (defalias 'semantic-overlay-put 'overlay-put)
  (defalias 'semantic-overlay-get 'overlay-get)
  (defalias 'semantic-overlay-properties 'overlay-properties)
  (defalias 'semantic-overlay-move 'move-overlay)
  (defalias 'semantic-overlay-delete 'delete-overlay)
  (defalias 'semantic-overlays-at 'overlays-at)
  (defalias 'semantic-overlays-in 'overlays-in)
  (defalias 'semantic-overlay-buffer 'overlay-buffer)
  (defalias 'semantic-overlay-start 'overlay-start)
  (defalias 'semantic-overlay-end 'overlay-end)
  (defalias 'semantic-overlay-next-change 'next-overlay-change)
  (defalias 'semantic-overlay-previous-change 'previous-overlay-change)
  (defalias 'semantic-overlay-lists 'overlay-lists)
  (defalias 'semantic-overlay-p 'overlayp)
  (defalias 'semantic-read-event 'read-event)
  )

(if (and (not (featurep 'xemacs))
	 (>= emacs-major-version 21))
    (defalias 'semantic-make-local-hook 'identity)
  (defalias 'semantic-make-local-hook 'make-local-hook)
  )

(if (featurep 'xemacs)
    (defalias 'semantic-mode-line-update 'redraw-modeline)
  (defalias 'semantic-mode-line-update 'force-mode-line-update))

(defun semantic-delete-overlay-maybe (overlay)
  "Delete OVERLAY if it is a semantic token overlay."
  (if (semantic-overlay-get overlay 'semantic)
      (semantic-overlay-delete overlay)))

;;; Misc utilities
;;
(defun semantic-map-buffers (fun)
  "Run function FUN for each Semantic enabled buffer found.
FUN does not have arguments.  When FUN is entered `current-buffer' is
the current Semantic enabled buffer found."
  (let ((bl (buffer-list))
        b)
    (while bl
      (setq b  (car bl)
            bl (cdr bl))
      (if (and (buffer-live-p b)
               (buffer-file-name b))
          (with-current-buffer b
            (if (semantic-active-p)
                (funcall fun)))))))

(defun semantic-map-mode-buffers (fun modes)
  "Run function FUN for each file buffer with major mode in MODES.
MODES can be a symbol or a list of symbols.
FUN does not have arguments."
  (or (listp modes) (setq modes (list modes)))
  (let ((bl (buffer-list))
        b)
    (while bl
      (setq b  (car bl)
            bl (cdr bl))
      (if (buffer-file-name b)
          (with-current-buffer b
            (if (memq major-mode modes)
                (funcall fun)))))))


;;; Behavioral APIs
;;
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.  This section permits the setting of override functions
;; for tasks of that nature, and also provides reasonable defaults.

;;; Core Semantic bindings API
;;
(defvar semantic-symbol-table nil
  "Buffer local semantic obarray.
These symbols provide a hook for a `major-mode' to specify specific
behaviors.  Use the function `semantic-bind' to define new bindings.")
(make-variable-buffer-local 'semantic-symbol-table)

(defsubst semantic-current-bindings (&optional mode)
  "Return the current semantic symbol table.
That is `semantic-symbol-table' if locally set, or the symbol table of
current major mode or its parents.
If optional argument MODE is specified return the symbol table of that
mode or its parents."
  (let (table)
    (or mode (setq table semantic-symbol-table
                   mode  major-mode))
    (while (and mode (not table))
      (or (setq table (get mode 'semantic-symbol-table))
          (setq mode  (get mode 'derived-mode-parent))))
    table))

(defun semantic-new-bindings (&optional table)
  "Return a new semantic symbol table.
If optional argument TABLE is non-nil, it must be another symbol
table, and its content is copied into the new table."
  (let ((new-table (make-vector 13 0)))
    (if table
        (mapatoms
         #'(lambda (sym)
             (let ((new-sym (intern (symbol-name sym) new-table)))
               (set new-sym (symbol-value sym))
               (setplist new-sym (symbol-plist sym))))
         table))
    new-table))

(defun semantic-bind (bindings &optional plist mode)
  "Define BINDINGS in the specified environment.
BINDINGS is a list of (VARIABLE . VALUE).
Optional argument PLIST is a property list each VARIABLE symbol will
be set to.  The following properties have special meaning:

- `constant' if non-nil, prevent to rebind variables.
- `mode-var' if non-nil, define mode variables.
- `override' if non-nil, define override functions.

The `override' and `mode-var' properties are mutually exclusive.

If optional argument MODE is non-nil, it must be a major mode symbol.
BINDINGS will be defined globally for this major mode.  If MODE is
nil, BINDINGS will be defined locally in the current buffer, in
variable `semantic-symbol-table'.  The later should be done in MODE
hook."
  ;; Check plist consistency
  (and (plist-get plist 'mode-var)
       (plist-get plist 'override)
       (error "Bindings can't be both overrides and mode variables"))
  (let (table variable varname value binding)
    (if mode
        (progn
          (setq table
                ;; Install in given MODE symbol table
                (or (get mode 'semantic-symbol-table)
                    ;; Or in a new one inherited from MODE parents
                    (semantic-new-bindings
                     (semantic-current-bindings mode))))
          (put mode 'semantic-symbol-table table))
      ;; Fail if trying to bind mode variables in local context!
      (if (plist-get plist 'mode-var)
          (error "Mode required to bind mode variables"))
      ;; Install in buffer local symbol table
      (or semantic-symbol-table
          ;; Or in a new one inherited from `major-mode' or parents
          (setq semantic-symbol-table
                (semantic-new-bindings
                 (semantic-current-bindings))))
      (setq table semantic-symbol-table))
    (while bindings
      (setq binding  (car bindings)
            bindings (cdr bindings)
            varname  (symbol-name (car binding))
            value    (cdr binding))
      (if (setq variable (intern-soft varname table))
          ;; Binding already exists
          ;; Check rebind consistency
          (cond
           ((get variable 'constant)
            (error "Can't change the value of constant `%s'"
                   variable))
           ((and (get variable 'mode-var)
                 (plist-get plist 'override))
            (error "Can't rebind override `%s' as a mode variable"
                   variable))
           ((and (get variable 'override)
                 (plist-get plist 'mode-var))
            (error "Can't rebind mode variable `%s' as an override"
                   variable))
           (t
            ;; Merge plist and assign new value
            (setplist variable (append plist (symbol-plist variable)))
            (set variable value)))
        ;; New binding
        (setq variable (intern varname table))
        ;; Set new plist and assign initial value
        (setplist variable plist)
        (set variable value)))
    ;; Return the symbol table used
    table))

(defsubst semantic-symbol (sym &optional mode)
  "Return the semantic symbol SYM or nil if not found.
Fetch SYM from `semantic-symbol-table' if locally set, or from
the symbol table of current major mode or its parents.
If optional argument MODE is non-nil search in the symbol table of
that mode or its parents.
Unless MODE is non-nil, set the buffer local value of
`semantic-symbol-table' to the current symbol table found."
  (or (and sym (symbolp sym)) (error "Invalid symbol %S" sym))
  (or (symbolp mode) (error "Invalid major mode symbol %S" mode))
  (let ((table (semantic-current-bindings mode)))
    (and (arrayp table)
         (or mode (setq semantic-symbol-table table))
         (intern-soft (symbol-name sym) table))))
  
  
(defsubst semantic-symbol-value (sym &optional mode property)
  "Return the current semantic value of SYM, or nil if not found.
If optional argument PROPERTY is non-nil check that SYM has that
property set.
See also `semantic-symbol'."
  (and (setq sym (semantic-symbol sym mode))
       (or (not property) (get sym property))
       (symbol-value sym)))

(defsubst semantic-set-local-variable (sym val)
  "Set variable SYM to VAL locally in current buffer.
BUFFER defaults to the current buffer."
  (set (make-local-variable sym) val))

;;; Mode local variables API
;;
;; There are buffer local variables, and frame local variables.
;; These routines give the illusion of mode specific variables.
;;
;; Why?  Some tokens have values specific to a major mode, but their buffer
;; might not be loaded.  This lets them run as though they were in a buffer
;; of the apropriate type.
;;
(defun semantic-activate-mode-bindings (&optional mode)
  "Set buffer local variables with MODE local variables.
If MODE is not specified it defaults to current `major-mode'."
  (let ((table (semantic-current-bindings (or mode major-mode))))
    (when table
      (mapatoms
       #'(lambda (var)
           (if (get var 'mode-var)
               (semantic-set-local-variable
                (intern (symbol-name var)) (symbol-value var))))
       table))))

(defsubst mode-local-value (mode sym)
  "Return the value of the MODE local variable SYM."
  (or mode (error "Missing major mode symbol"))
  (semantic-symbol-value sym mode 'mode-var))

(defmacro setq-mode-local (mode &rest args)
  "Assign new values to variables local in MODE.
MODE must be a major mode symbol.
ARGS is a list (SYM VAL SYM VAL ...).
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Set each SYM to the value of its VAL, locally in buffers already in
MODE, or in buffers switched to that mode.
Return the value of the last VAL."
  (when args
    (let (i ll bl sl tmp sym val)
      (setq i 0)
      (while args
        (setq tmp  (make-symbol (format "tmp%d" i))
              i    (1+ i)
              sym  (car args)
              val  (cadr args)
              ll   (cons (list tmp val) ll)
              bl   (cons `(cons ',sym ,tmp) bl)
              sl   (cons `(semantic-set-local-variable ',sym ,tmp) sl)
              args (cddr args)))
      `(let* ,(nreverse ll)
         ;; Save mode bindings
         (semantic-bind (list ,@bl) '(mode-var t) ',mode)
         ;; Assign to local variables in all existing buffers in MODE
         (semantic-map-mode-buffers #'(lambda () ,@sl) ',mode)
         ;; Return the last value
         ,tmp)
      )))

(defmacro defvar-mode-local (mode sym val &optional docstring)
  "Define MODE local variable SYM with value VAL.
DOCSTRING is optional."
  `(progn
     (setq-mode-local ,mode ,sym ,val)
     (put (semantic-symbol ',mode ',sym)
          'variable-documentation ,docstring)
     ',sym))

(defmacro defconst-mode-local (mode sym val &optional docstring)
  "Define MODE local constant SYM with value VAL.
DOCSTRING is optional."
  (let ((tmp (make-symbol "tmp")))
  `(let (,tmp)
     (setq-mode-local ,mode ,sym ,val)
     (setq ,tmp (semantic-symbol ',mode ',sym))
     (put ,tmp 'constant t)
     (put ,tmp 'variable-documentation ,docstring)
     ',sym)))

;;; Override functions API
;;
(defsubst semantic-fetch-overload (overload)
  "Return the current OVERLOAD function, or nil if not found.
Fetch OVERLOAD from `semantic-override-table' if locally set, or from
the override table of current major mode or its parents.  Set the
buffer local value of `semantic-override-table' to the current
override table found."
  (semantic-symbol-value overload nil 'override))

(defsubst semantic-install-function-overrides (overrides &optional transient mode)
  "Install the function OVERRIDES in the specified environment.
OVERRIDES must be an alist ((OVERLOAD .  FUNCTION) ...) where OVERLOAD
is a symbol identifying an overloadable entry, and FUNCTION is the
function to override it with.
If optional argument TRANSIENT is non-nil, installed overrides can in
turn be overridden by next installation.
If optional argument MODE is non-nil, it must be a major mode symbol.
OVERRIDES will be installed globally for this major mode.  If MODE is
nil, OVERRIDES will be installed locally in the current buffer.  This
later installation should be done in MODE hook."
  (semantic-bind overrides
                 (list 'constant (not transient) 'override t)
                 mode))

;;;###autoload
(defmacro define-overload (name args docstring &rest body)
  "Define a new function, as with `defun' which can be overloaded.
NAME is the name of the function to create.  If it is of the form
`semantic-NAME'.  The function will strip `semantic-' from the front
as the name of the symbol created.
ARGS are the arguments to the function.
DOCSTRING is a documentation string to describe the function.  The
docstring will automatically had details about its overload symbol
appended to the end.
BODY is code that would be run as a default if this function is not
overloaded for a specific mode.  The default is to call the function
`NAME-default' with the appropriate ARGS.
If you want the main function of an overloadable function to do extra
work, write that yourself without `define-overload'.  See the info
manual for details."
  (let* ((fastargs (delq '&rest (delq '&optional (copy-sequence args))))
         (sym-name (symbol-name name))
         (overload (if (string-match "^semantic-" sym-name)
                       (intern (substring sym-name (match-end 0)))
                     name)))
    (or body (setq body `((,(intern (concat sym-name "-default"))
                           ,@fastargs))))
    `(eval-and-compile
       (defun ,name ,args
         ,(format "%s\n
This function can be overloaded using the symbol `%s'."
                  docstring overload)
         (let ((s (semantic-fetch-overload ',overload)))
           (if s
               (funcall s ,@fastargs)
             ;; Else, perform some default behaviors
             ,@body)))
       (put ',name 'semantic-overload ',overload))))

(defmacro define-mode-overload-implementation
  (name mode args docstring &rest body)
  "Define a new function overload, as with `defun' that has an implementation.
Function implementations are only useful for functions created with
`define-overload'.
NAME is the name of the function being overloaded.
MODE is the major mode this override is being defined for.
ARGS are the function arguments, which should match those of the same
named function created with `define-overload'.
DOCSTRING is the documentation string.
BODY is the implementation of this function."
  (let* ((sym-name (symbol-name name))
         (overload (if (string-match "^semantic-" sym-name)
                       (intern (substring sym-name (match-end 0)))
                     name))
	 (newname (intern (concat sym-name "-" (symbol-name mode)))))
    `(progn
       (eval-and-compile
	 (defun ,newname ,args
	   ,(format "%s\n
This function is an implementation for %s"
		    docstring overload)
	   ;; The body for this implementation
	   ,@body))
       (semantic-install-function-overrides '((,overload . ,newname)) t ',mode)
       )))

;;; Emacs Help hacks
;;
(defun semantic-function-overload-p (symbol)
  "The symbol that SYMBOL can be overriden with, or nil."
  (get symbol 'semantic-overload))

(defun semantic-augment-function-help (symbol)
  "Augment the *Help* buffer for SYMBOL.
SYMBOL is a function that can be overriden with semantic."
  (save-excursion
    (set-buffer "*Help*")
    (unwind-protect
	(progn
	  (toggle-read-only -1)
	  (goto-char (point-max))
	  (beginning-of-line)
	  (forward-line -1)
	  (insert "\nThis function can be overriden in semantic with
the symbol `" (symbol-name (semantic-function-overload-p symbol)) "'.\n")
	  ;; NOTE TO SELF:
	  ;; LIST ALL LOADED OVERRIDES FOR SYMBOL HERE
	  )
      (toggle-read-only 1))))

;; Help for Overload functions.  Need to advise help.
(defadvice describe-function (around semantic-help activate)
  "Display the full documentation of FUNCTION (a symbol).
Returns the documentation as a string, also."
  (prog1
      ad-do-it
    (if (semantic-function-overload-p (ad-get-arg 0))
	(semantic-augment-function-help (ad-get-arg 0)))))



;;; Editor goodies ;-)
;;
(put 'defvar-mode-local   'lisp-indent-function 'defun)
(put 'defconst-mode-local 'lisp-indent-function 'defun)

(defconst semantic-fw-font-lock-keywords
  (eval-when-compile
    (let* (
           ;; Variable declarations         
           (kv (regexp-opt
                '(
                  "defconst-mode-local"
                  "defvar-mode-local"
                  ) t))
           ;; Function declarations
           (kf (regexp-opt
                '(
                  "define-lex"
                  "define-lex-analyzer"
                  "define-lex-block-analyzer"
                  "define-lex-regex-analyzer"
                  "define-lex-simple-regex-analyzer"
                  "define-mode-overload-implementation"
                  "define-overload"
                  "define-wisent-lexer"
                  ) t))
           ;; Regexp depths
           (kv-depth (regexp-opt-depth kv))
           (kf-depth (regexp-opt-depth kf))
           )
      `((,(concat
           ;; Declarative things
           "(\\(" kv "\\|" kf "\\)"
           ;; Whitespaces & names
           "\\>[ \t]*\\(\\sw+\\)?[ \t]*\\(\\sw+\\)?"
           )
         (1 font-lock-keyword-face)
         (,(+ 1 kv-depth kf-depth 1)
          (cond ((match-beginning 2)
                 font-lock-type-face)
                ((match-beginning ,(+ 1 kv-depth 1))
                 font-lock-function-name-face)
                )
          nil t)
         (,(+ 1 kv-depth kf-depth 1 1)
          (cond ((match-beginning 2)
                 font-lock-variable-name-face)
                )
          nil t)))
      ))
  "Highlighted Semantic keywords.")

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          semantic-fw-font-lock-keywords)
  )

;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()
     
     (def-edebug-spec setq-mode-local
       (symbolp (&rest symbolp form))
       )
     (def-edebug-spec defvar-mode-local
       (&define symbolp name def-form [ &optional stringp ] )
       )
     (def-edebug-spec defconst-mode-local
       defvar-mode-local
       )
     (def-edebug-spec define-overload
       (&define name lambda-list stringp def-body)
       )
     (def-edebug-spec define-mode-overload-implementation
       (&define name symbolp lambda-list stringp def-body)
       )
     
     ))

(provide 'semantic-fw)

;;; semantic-fw.el ends here
