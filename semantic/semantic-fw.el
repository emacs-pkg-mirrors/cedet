;;; semantic-fw.el --- Framework for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-fw.el,v 1.6 2002/08/04 01:50:09 zappo Exp $

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
      (defalias 'semantic-overlay-live-p 'extent-live-p)
      (defalias 'semantic-make-overlay 'make-extent)
      (defalias 'semantic-overlay-put 'set-extent-property)
      (defalias 'semantic-overlay-get 'extent-property)
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


;;; Primitive Token access system:
;;
;; Raw tokens in semantic are lists.  Each token/list has a basic structure
;; for all tokens.  This includes the first two elements, and the last 3.
;; See `semantic-tfe-*' for details.
;;
;; TFE = Token From End

(defconst semantic-tfe-overlay 1
  "Amount to subtract from the length of the token to get the overlay.")
(defconst semantic-tfe-properties 2
  "Amount to subtract from the length of the token to get the property list.")
(defconst semantic-tfe-docstring 3
  "Amount to subtract from the length of the token to get the doc string.")
(defconst semantic-tfe-number 2
  "The number of required end elements.")

(defmacro semantic-token-token (token)
  "Retrieve from TOKEN the token identifier.
ie, the symbol 'variable, 'function, 'type, or other."
  `(nth 1 ,token))

(defsubst semantic-token-name (token)
  "Retrieve the name of TOKEN."
  (car token))

(defun semantic-token-docstring (token &optional buffer)
  "Retrieve the documentation of TOKEN.
Optional argument BUFFER indicates where to get the text from.
If not provided, then only the POSITION can be provided."
  (let ((p (nth (- (length token) semantic-tfe-docstring) token)))
    (if (and p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (semantic-flex-text (car (semantic-lex p (1+ p)))))
      p)))

(defmacro semantic-token-properties (token)
  "Retrieve the PROPERTIES part of TOKEN.
The returned item is an ALIST of (KEY . VALUE) pairs."
  `(nth (- (length ,token) semantic-tfe-properties) ,token))

(defmacro semantic-token-properties-cdr (token)
  "Retrieve the cons cell for the PROPERTIES part of TOKEN."
  `(nthcdr (- (length ,token) semantic-tfe-properties) ,token))

(defun semantic-token-put (token key value)
  "For TOKEN, put the property KEY on it with VALUE.
If VALUE is nil, then remove the property from TOKEN."
  (let* ((c (semantic-token-properties-cdr token))
	 (al (car c))
	 (a (assoc key (car c))))
    (if a
	(if value
	    (setcdr a value)
	  (adelete 'al key)
	  (setcar c al))
      (if value
	  (setcar c (cons (cons key value) (car c)))))
    ))

(defun semantic-token-put-no-side-effect (token key value)
  "For TOKEN, put the property KEY on it with VALUE without side effects.
If VALUE is nil, then remove the property from TOKEN.
All cons cells in the property list are replicated so that there
are no side effects if TOKEN is in shared lists."
  (let* ((c (semantic-token-properties-cdr token))
	 (al (copy-sequence (car c)))
	 (a (assoc key (car c))))
    ;; This removes side effects
    (setcar c a)
    (if a
	(if value
	    (setcdr a value)
	  (adelete 'al key)
	  (setcar c al))
      (if value
	  (setcar c (cons (cons key value) (car c)))))
    ))

(defsubst semantic-token-get (token key)
  "For TOKEN, get the value for property KEY."
  (cdr (assoc key (semantic-token-properties token))))

(defmacro semantic-token-overlay (token)
  "Retrieve the OVERLAY part of TOKEN.
The returned item may be an overlay or an unloaded buffer representation."
  `(nth (- (length ,token) semantic-tfe-overlay) ,token))

(defmacro semantic-token-overlay-cdr (token)
  "Retrieve the cons cell containing the OVERLAY part of TOKEN."
  `(nthcdr (- (length ,token) semantic-tfe-overlay) ,token))

(defmacro semantic-token-extent (token)
  "Retrieve the extent (START END) of TOKEN."
  `(let ((o (semantic-token-overlay ,token)))
     (if (semantic-overlay-p o)
	 (list (semantic-overlay-start o) (semantic-overlay-end o))
       (list (aref o 0) (aref o 1)))))

(defsubst semantic-token-start (token)
  "Retrieve the start location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-start o)
      (aref o 0))))

(defsubst semantic-token-end (token)
  "Retrieve the end location of TOKEN."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-end o)
      (aref o 1))))

(defsubst semantic-token-buffer (token)
  "Retrieve the buffer TOKEN resides in."
  (let ((o (semantic-token-overlay token)))
    (if (semantic-overlay-p o)
        (semantic-overlay-buffer o)
      ;; We have no buffer for this token (It's not in Emacs right now.)
      nil)))

(defsubst semantic-token-p (token)
  "Return non-nil if TOKEN is most likely a semantic token."
  (and (listp token)
       (stringp (car token))
       (car (cdr token))
       (symbolp (car (cdr token)))))

(defun semantic-token-with-position-p (token)
  "Return non-nil if TOKEN is a semantic token with positional information."
  (and (semantic-token-p token)
       (let ((o (semantic-token-overlay token)))
	 (or (semantic-overlay-p o)
	     (and (arrayp o)
		  (not (stringp o)))))))

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

(defun semantic-map-mode-buffers (fun mode)
  "Run function FUN for each MODE enabled buffer found.
FUN does not have arguments.  When FUN is entered `current-buffer' is
the current MODE controlled buffer found."
  (let ((bl (buffer-list))
        b)
    (while bl
      (setq b  (car bl)
            bl (cdr bl))
      (if (and (buffer-live-p b)
               (buffer-file-name b))
          (with-current-buffer b
            (if (eq major-mode mode)
                (funcall fun)))))))


;;; Behavioral APIs
;;
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.  This section permits the setting of override functions
;; for tasks of that nature, and also provides reasonable defaults.

(defvar semantic-override-table nil
  "Buffer local semantic function overrides obarray.
These overrides provide a hook for a `major-mode' to override specific
behaviors with respect to generated semantic toplevel nonterminals and
things that these non-terminals are useful for.  Use the function
`semantic-install-function-overrides' to install overrides.

Use `semantic-list-overrides' to get a list of override functions.")
(make-variable-buffer-local 'semantic-override-table)

(defsubst semantic-current-overrides (&optional mode)
  "Return the current function overrides table.
That is `semantic-override-table' if locally set, or the override
table of current major mode or its parents.
If optional argument MODE is specified return the override table of
that mode or its parents."
  (let (table)
    (or mode (setq table semantic-override-table
                   mode  major-mode))
    (while (and mode (not table))
      (or (setq table (get mode 'semantic-override-table))
          (setq mode  (get mode 'derived-mode-parent))))
    table))

(defun semantic-new-overrides (&optional table)
  "Return a new function overrides table.
If optional argument TABLE is non-nil, it must be an overrides table.
Its content is copied into the new table."
  (let ((new-table (make-vector 13 0)))
    (if table
        (mapatoms
         #'(lambda (sym)
             (let ((new-sym (intern (symbol-name sym) new-table)))
               (set new-sym (symbol-value sym))
               (put new-sym 'override (get sym 'override))))
         table))
    new-table))

(defun semantic-install-function-overrides (overrides &optional transient mode)
  "Install the function OVERRIDES in the specified environment.
OVERRIDES must be an alist ((OVERLOAD .  FUNCTION) ...) where OVERLOAD
is a symbol identifying an overloadable entry, and FUNCTION is the
function to override it with.
If optional argument TRANSIENT is non-nil, installed overrides can in
turn be overridden by next installation.
If optional argument MODE is non-nil, it must be a major mode symbol.
OVERRIDES will be installed globally for this major mode.  If MODE is
nil, OVERRIDES will be installed locally in the current buffer, in
variable `semantic-override-table'.  This later installation should be
done in MODE hook."
  (let (table overload overname function override)
    (if mode
        (progn
          (setq table
                ;; Install in given MODE override table
                (or (get mode 'semantic-override-table)
                    ;; Or in a new one inherited from MODE parents
                    (semantic-new-overrides
                     (semantic-current-overrides mode))))
          (put mode 'semantic-override-table table))
      ;; Install in buffer local override table
      (or semantic-override-table
          ;; Or in a new one inherited from `major-mode' or parents
          (setq semantic-override-table
                (semantic-new-overrides
                 (semantic-current-overrides))))
      (setq table semantic-override-table))
    (while overrides
      (setq override  (car overrides)
            overrides (cdr overrides)
            overname  (symbol-name (car override))
            function  (cdr override))
      ;; Keep trace of mode where function overrides something
      (and mode function (symbolp function)
           (put function 'semantic-override-mode mode))
      (if (setq overload (intern-soft overname table))
          (if (get overload 'override)
              (set overload function)
            (or (equal (symbol-value overload) function)
                (message
                 "Current `%s' function #'%s not overrode by #'%s"
                 overload (symbol-value overload) function)))
        (setq overload (intern overname table))
        (set overload function))
      (put overload 'override transient))))

(defun semantic-fetch-overload (overload)
  "Return the current OVERLOAD function, or nil if not found.
Fetch OVERLOAD from `semantic-override-table' if locally set, or from
the override table of current major mode or its parents.  Set the
buffer local value of `semantic-override-table' to the current
override table found."
  (let ((table (semantic-current-overrides)))
    (and (arrayp table)
         (setq semantic-override-table table)
         (setq overload (intern-soft (symbol-name overload) table))
         (symbol-value overload))))

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
    `(eval-and-compile
       (defun ,newname ,args
	 ,(format "%s\n
This function is an implementation for %s"
		  docstring overload)
	 ;; The body for this implementation
	 ,@body)
       (semantic-install-function-overrides '((,overload . ,newname)) nil ',mode)
       )))

;;; MODE VARIABLES
;;
;; There are buffer local variables, and frame local variables.
;; These routines give the illusion of mode specific variables.
;;
;; They work like this:
;; a symbol, like `c-mode' has a `semantic-variables' property which
;; is a list of variables set for this mode.
;; A variable with a mode value appears in that list, AND it has
;; a property matching the mode name.  This property contains the value
;; of that variable for that mode.
;;
;; Why?  Some tokens have values specific to a major mode, but their buffer
;; might not be loaded.  This lets them run as though they were in a buffer
;; of the apropriate type.
;;
(defun semantic-symbol-value-for-mode (variable mode)
  "Retrieve the value of VARIABLE (a symbol) for MODE."
  (if (memq mode (get mode 'semantic-variables))
      (get variable mode)
    (symbol-value variable)))

(defun semantic-symbol-value-mode-assign ()
  "For the current major mode, set values of variables into local variables.
To be called by semantic init function."
  (let ((vars (get major-mode 'semantic-variables)))
    (while vars
      (set (car vars) (get (car vars) major-mode))
      (setq vars (cdr vars)))))

(defun semantic-setq-major-mode (varname modename value)
  "Perform the work of `setq-major-mode'.
VARNAME is the variable name, MODENAME is the major mode, and VALUE is the
new value."
  ;; Force this value into MODENAME for later extraction
  (let ((plist (get modename 'semantic-variables)))
    (add-to-list 'plist varname)
    (put modename 'semantic-variables plist))
  ;; Make this assignment into the mode part.
  (put varname modename value)
  ;; Assign to all existing buffers.
  (semantic-map-mode-buffers (lambda () (set varname value))
			     modename)
  )

(defmacro setq-major-mode (varname modename value)
  "Assign into VARNAME for all modes of MODENAME a new VALUE.
VARNAME is a symbol (unquoted).
MODENAME must be a major mode variable, or the assignment is useless.
VALUE is the new value to be assigned.
The assignments is saved for new buffers created of class MODENAME,
and assigned into those modes.
All existing modes of a given type will be given VALUE also."
  `(semantic-setq-major-mode ',varname ',modename ,value))

(provide 'semantic-fw)

;;; semantic-fw.el ends here
