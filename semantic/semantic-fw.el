;;; semantic-fw.el --- Framework for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-fw.el,v 1.4 2002/07/30 19:56:40 ponced Exp $

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
table of MODE or its parents.  MODE defaults to current value of
`major-mode'."
  (or mode (setq mode major-mode))
  (let ((table semantic-override-table))
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
        (or (setq table (get mode 'semantic-override-table))
            (put mode 'semantic-override-table
                 (setq table (semantic-new-overrides))))
      (or semantic-override-table
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

(defun semantic-fetch-overload (overload &optional mode)
  "Return the current OVERLOAD function, or nil if not found.
Fetch OVERLOAD from `semantic-override-table' if locally set, or from
the override table of MODE or its parents.  MODE defaults to current
value of `major-mode'."
  (let ((table (semantic-current-overrides mode)))
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

(provide 'semantic-fw)

;;; semantic-fw.el ends here
