;;; semantic-fw.el --- Framework for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; X-CVS: $Id: semantic-fw.el,v 1.35 2004/03/08 14:03:57 ponced Exp $

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

;;; Code:
;;

;;; Compatibility
;;
(if (featurep 'xemacs)
    (progn
      (defalias 'semantic-overlay-live-p          'extent-live-p)
      (defalias 'semantic-make-overlay            'make-extent)
      (defalias 'semantic-overlay-put             'set-extent-property)
      (defalias 'semantic-overlay-get             'extent-property)
      (defalias 'semantic-overlay-properties      'extent-properties)
      (defalias 'semantic-overlay-move            'set-extent-endpoints)
      (defalias 'semantic-overlay-delete          'delete-extent)
      (defalias 'semantic-overlays-at
        (lambda (pos) (extent-list nil pos pos)))
      (defalias 'semantic-overlays-in
        (lambda (beg end) (extent-list nil beg end)))
      (defalias 'semantic-overlay-buffer          'extent-buffer)
      (defalias 'semantic-overlay-start           'extent-start-position)
      (defalias 'semantic-overlay-end             'extent-end-position)
      (defalias 'semantic-overlay-next-change     'next-extent-change)
      (defalias 'semantic-overlay-previous-change 'previous-extent-change)
      (defalias 'semantic-overlay-lists
        (lambda () (list (extent-list))))
      (defalias 'semantic-overlay-p               'extentp)
      (defun semantic-read-event ()
        (let ((event (next-command-event)))
          (if (key-press-event-p event)
              (let ((c (event-to-character event)))
                (if (char-equal c (quit-char))
                    (keyboard-quit)
                  c)))
          event))
      )
  (defalias 'semantic-overlay-live-p          'overlay-buffer)
  (defalias 'semantic-make-overlay            'make-overlay)
  (defalias 'semantic-overlay-put             'overlay-put)
  (defalias 'semantic-overlay-get             'overlay-get)
  (defalias 'semantic-overlay-properties      'overlay-properties)
  (defalias 'semantic-overlay-move            'move-overlay)
  (defalias 'semantic-overlay-delete          'delete-overlay)
  (defalias 'semantic-overlays-at             'overlays-at)
  (defalias 'semantic-overlays-in             'overlays-in)
  (defalias 'semantic-overlay-buffer          'overlay-buffer)
  (defalias 'semantic-overlay-start           'overlay-start)
  (defalias 'semantic-overlay-end             'overlay-end)
  (defalias 'semantic-overlay-next-change     'next-overlay-change)
  (defalias 'semantic-overlay-previous-change 'previous-overlay-change)
  (defalias 'semantic-overlay-lists           'overlay-lists)
  (defalias 'semantic-overlay-p               'overlayp)
  (defalias 'semantic-read-event              'read-event)
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

;;; Positional Data Cache
;;
(defvar semantic-cache-data-overlays nil
  "List of all overlays waiting to be flushed.")

(defun semantic-cache-data-to-buffer (buffer start end value name &optional lifespan)
  "In BUFFER over the region START END, remember VALUE.
NAME specifies a special name that can be searched for later to
recover the cached data with `semantic-get-cache-data'.
LIFESPAN indicates how long the data cache will be remembered.
The default LIFESPAN is 'end-of-command.
Possible Lifespans are:
  'end-of-command - Remove the cache at the end of the currently
                    executing command.
  'exit-cache-zone - Remove when point leaves the overlay at the
                    end of the currently executing command."
  ;; Check if LIFESPAN is valid before to create any overlay
  (or lifespan (setq lifespan 'end-of-command))
  (or (memq lifespan '(end-of-command exit-cache-zone))
      (error "semantic-cache-data-to-buffer: Unknown LIFESPAN: %s"
             lifespan))
  (let ((o (semantic-make-overlay start end buffer)))
    (semantic-overlay-put o 'cache-name   name)
    (semantic-overlay-put o 'cached-value value)
    (semantic-overlay-put o 'lifespan     lifespan)
    (setq semantic-cache-data-overlays
          (cons o semantic-cache-data-overlays))
    ;;(message "Adding to cache: %s" o)
    (add-hook 'post-command-hook 'semantic-cache-data-post-command-hook)
    ))

(defun semantic-cache-data-post-command-hook ()
  "Flush `semantic-cache-data-overlays' based 'lifespan property.
Remove self from `post-command-hook' if it is empty."
  (let ((newcache nil)
        (oldcache semantic-cache-data-overlays))
    (while oldcache
      (let* ((o    (car oldcache))
             (life (semantic-overlay-get o 'lifespan))
             )
        (if (or (eq life 'end-of-command)
                (and (eq life 'exit-cache-zone)
                     (not (member o (semantic-overlays-at (point))))))
            (progn
              ;;(message "Removing from cache: %s" o)
              (semantic-overlay-delete o)
              )
          (setq newcache (cons o newcache))))
      (setq oldcache (cdr oldcache)))
    (setq semantic-cache-data-overlays (nreverse newcache)))

  ;; Remove ourselves if we have removed all overlays.
  (unless semantic-cache-data-overlays
    (remove-hook 'post-command-hook
                 'semantic-cache-data-post-command-hook)))

(defun semantic-get-cache-data (name &optional point)
  "Get cached data with NAME from optional POINT."
  (save-excursion
    (if point (goto-char point))
    (let ((o (semantic-overlays-at (point)))
          (ans nil))
      (while (and (not ans) o)
        (if (equal (semantic-overlay-get (car o) 'cache-name) name)
            (setq ans (car o))
          (setq o (cdr o))))
      (when ans
        (semantic-overlay-get ans 'cached-value)))))

(defun semantic-test-data-cache ()
  "Test the data cache."
  (interactive)
  (let ((data '(a b c)))
    (semantic-cache-data-to-buffer (current-buffer) (point) (+ (point) 5)
				   data 'moose 'exit-cache-zone)
    (if (equal (semantic-get-cache-data 'moose) data)
	(message "Successfully retrieved cached data.")
      (message "Failed to retrieve cached data."))
    ))

;;; Obsoleting various functions & variables
;;
(defun semantic-alias-obsolete (oldfnalias newfn)
  "Make OLDFNALIAS an alias for NEWFN.
Mark OLDFNALIAS as obsolete, such that the byte compiler
will throw a warning when it encounters this symbol."
  (defalias oldfnalias newfn)
  (make-obsolete oldfnalias newfn))

(defun semantic-varalias-obsolete (oldvaralias newvar)
  "Make OLDVARALIAS an alias for variable NEWVAR.
Mark OLDVARALIAS as obsolete, such that the byte compiler
will throw a warning when it encounters this symbol."
  (condition-case err
      (defvaralias oldvaralias newvar)
    (error
     ;; Only throw this warning when byte compiling things.
     (when (and (boundp 'byte-compile-current-file)
		byte-compile-current-file)
       (message "+++ %s\n\
*** Compatibility with Semantic 2 might be broken:\n\
    can't make obsolete variable `%s'\n\
    alias of `%s'." (error-message-string err) oldvaralias newvar)
       )))
  (make-obsolete-variable oldvaralias newvar))

;;; Semantic autoloads
;;
;; Load semantic-loaddefs after compatibility code, to allow to use it
;; in autoloads without infinite recursive load problems.
(load "semantic-loaddefs" nil t)

;;; Help debugging
;;
(defmacro semantic-safe (format &rest body)
  "Turn into a FORMAT message any error caught during eval of BODY.
Return the value of last BODY form or nil if an error occurred.
FORMAT can have a %s escape which will be replaced with the actual
error message.
If `debug-on-error' is set, errors are not caught, so that you can
debug them.
Avoid using a large BODY since it is duplicated."
  ;;(declare (debug t) (indent 1))
  `(if debug-on-error
       (progn ,@body)
     (condition-case err
	 (progn ,@body)
       (error
        (message ,format (error-message-string err))
        nil))))
(put 'semantic-safe 'lisp-indent-function 1)

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
(defsubst semantic-set-parent-mode (mode parent)
  "Set parent of major mode MODE to PARENT mode.
To work properly, this function should be called after PARENT mode
local variables have been defined."
  (put mode 'semantic-mode-parent parent)
  ;; Refresh mode bindings to get mode local variables inherited from
  ;; PARENT. To work properly, the following should be called after
  ;; PARENT mode local variables have been defined.
  (semantic-map-mode-buffers mode 'semantic-activate-mode-bindings))

(defsubst semantic-get-parent-mode (mode)
  "Return the mode parent of the major mode MODE.
 Return nil if MODE has no parent."
  (or (get mode 'semantic-mode-parent)
      (get mode 'derived-mode-parent)))

(defmacro define-semantic-child-mode (mode parent &optional docstring)
  "Make major mode MODE inherits semantic behavior from PARENT mode.
DOCSTRING is optional and not used.
To work properly, this should be put after PARENT mode local variables
definition."
  `(semantic-set-parent-mode ',mode ',parent))

(defvar semantic-symbol-table nil
  "Buffer local semantic obarray.
These symbols provide a hook for a `major-mode' to specify specific
behaviors.  Use the function `semantic-bind' to define new bindings.")
(make-variable-buffer-local 'semantic-symbol-table)

(defvar semantic-bindings-active-mode nil
   "Major mode in which bindings are active.")

(defsubst semantic-current-bindings (&optional mode)
   "Return the current semantic symbol table.
That is `semantic-symbol-table' if locally set, or the symbol table of
current active mode or its parents.
By default the active mode is the current major mode.
If optional argument MODE is specified return the symbol table of that
mode or its parents."
   (let (table)
     (or mode
         (setq mode  semantic-bindings-active-mode)
         (setq table semantic-symbol-table
               mode  major-mode))
     (while (and mode (not table))
       (or (setq table (get mode 'semantic-symbol-table))
           (setq mode  (semantic-get-parent-mode mode))))
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
           ((equal (symbol-value variable) value)
            ;; Just ignore rebind with the same value.
            )
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
the symbol table of the current active mode or its parents.
By default the active mode is the current major mode.
When MODE is nil and there is no explicit active mode, set the buffer
local value of `semantic-symbol-table' to the current symbol table
found."
  (or (and sym (symbolp sym)) (error "Invalid symbol %S" sym))
  (or (symbolp mode) (error "Invalid major mode symbol %S" mode))
  (let ((table (semantic-current-bindings mode)))
    (and (arrayp table)
         (or mode
             semantic-bindings-active-mode
             (setq semantic-symbol-table table))
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
  "Activate variables defined locally in MODE and its parents.
That is, copy mode local bindings into corresponding buffer local
variables.
If MODE is not specified it defaults to current `major-mode'."
  (let (modes table)
    (or mode (setq mode major-mode))
    ;; Get MODE's parents & MODE in the right order.
    (while mode
      (setq modes (cons mode modes)
            mode  (semantic-get-parent-mode mode)))
    ;; Activate mode bindings following parent modes order.
    (while modes
      (when (setq table (get (car modes) 'semantic-symbol-table))
        (mapatoms
         #'(lambda (var)
             (if (get var 'mode-var)
                 (semantic-set-local-variable
                  (intern (symbol-name var)) (symbol-value var))))
         table))
      (setq modes (cdr modes)))))

(defun semantic-deactivate-mode-bindings (&optional mode)
  "Deactivate variables defined locally in MODE and its parents.
That is, kill buffer local variables set from the corresponding mode
local bindings.
If MODE is not specified it defaults to current `major-mode'."
  (let* ((mode (or mode major-mode))
         table)
    (while mode
      (when (setq table (get mode 'semantic-symbol-table))
        (mapatoms
         #'(lambda (var)
             (if (get var 'mode-var)
                 (kill-local-variable (intern (symbol-name var)))))
         table))
      (setq mode (semantic-get-parent-mode mode)))))

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
     (put (semantic-symbol ',sym ',mode)
          'variable-documentation ,docstring)
     ',sym))

(defmacro defconst-mode-local (mode sym val &optional docstring)
  "Define MODE local constant SYM with value VAL.
DOCSTRING is optional."
  (let ((tmp (make-symbol "tmp")))
  `(let (,tmp)
     (setq-mode-local ,mode ,sym ,val)
     (setq ,tmp (semantic-symbol ',sym ',mode))
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

(defun semantic-overload-symbol-from-function (name)
  "Return the symbol for overload used by NAME, the defined symbol."
  (let ((sym-name (symbol-name name)))
    (if (string-match "^semantic-" sym-name)
	(intern (substring sym-name (match-end 0)))
      name)))

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
         (overload (semantic-overload-symbol-from-function name)))
    (or body (setq body `((,(intern (concat sym-name "-default"))
                           ,@fastargs))))
    `(eval-and-compile
       (defun ,name ,args
         ,docstring
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


;;; Temporary Mode Local settings
;;
;; Use this to use a tag from a buffer that may not be of the
;; same major mode as the originator.
(defmacro semantic-with-mode-bindings (mode &rest body)
   "Evaluate BODY with the local bindings of MODE.
The current mode bindings are saved, BODY is evaluated, and the saved
bindings are restored, even in case of an abnormal exit.
Value is what BODY returns."
   (let ((old-mode  (make-symbol "mode")))
     `(let ((,old-mode semantic-bindings-active-mode))
        (unwind-protect
            (progn
              (semantic-deactivate-mode-bindings ,old-mode)
              (setq semantic-bindings-active-mode ',mode)
              (semantic-activate-mode-bindings ',mode)
              ,@body)
          (semantic-deactivate-mode-bindings ',mode)
          (setq semantic-bindings-active-mode ,old-mode)
          (semantic-activate-mode-bindings ,old-mode)))))
(put 'semantic-with-mode-bindings 'lisp-indent-function 1)

;;; Emacs Help hacks
;;
(defun semantic-function-overload-p (symbol)
  "The symbol that SYMBOL can be overriden with, or nil."
  (get symbol 'semantic-overload))

(defun semantic-overload-docstring-extension (name)
  "Return the documentation string used to augment an overloaded function.
The augmented string is NAME."
  (concat "\nThis function can be overriden in semantic using the
symbol `" (symbol-name name) "'."))

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
	  (insert
	   (semantic-overload-docstring-extension
	    (semantic-function-overload-p symbol))
	   "\n")
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


;;; User Interrupt handling
;;
(defvar semantic-current-input-throw-symbol nil
  "The current throw symbol for `semantic-exit-on-input'.")

(defmacro semantic-exit-on-input (symbol &rest forms)
  "Using SYMBOL as an argument to `throw', execute FORMS.
If FORMS includes a call to `semantic-thow-on-input', then
if a user presses any key during execution, this form macro
will exit with the value passed to `semantic-throw-on-input'.
If FORMS completes, then the return value is the same as `progn'."
  `(let ((semantic-current-input-throw-symbol ,symbol))
     (catch ,symbol
       ,@forms)))
(put 'semantic-exit-on-input 'lisp-indent-function 1)

(defmacro semantic-throw-on-input (from)
  "Exit with `throw' when in `semantic-exit-on-input' on user input.
FROM is an indication of where this function is called from as a value
to pass to `throw'.  It is recommended to use the name of the function
calling this one."
  `(when (and semantic-current-input-throw-symbol (input-pending-p))
     (throw semantic-current-input-throw-symbol ,from)))

(defun semantic-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (semantic-throw-on-input 'done-die)
  (message "Exit Code: %s"
	   (semantic-exit-on-input 'testing
	     (let ((inhibit-quit nil)
		   (message-log-max nil))
	       (while (sit-for 0)
		 (message "Looping ...")
		 (semantic-throw-on-input 'test-inner-loop))
	       'exit))))


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
                  "define-lex-keyword-type-analyzer"
                  "define-lex-sexp-type-analyzer"
                  "define-lex-regex-type-analyzer"
                  "define-lex-string-type-analyzer"
                  "define-lex-block-type-analyzer"
                  "define-mode-overload-implementation"
                  "define-semantic-child-mode"
                  "define-semantic-idle-service"
                  "define-overload"
                  "define-wisent-lexer"
                  "semantic-alias-obsolete"
                  "semantic-varalias-obsolete"
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
     (def-edebug-spec semantic-exit-on-input
       (symbolp def-body)
       )
     
     ))

(provide 'semantic-fw)

;;; semantic-fw.el ends here
