;;; senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000, 2001, 2002 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 10 Nov 2000
;; Keywords: syntax
;; X-RCS: $Id: senator.el,v 1.59.2.1 2003/04/04 08:28:31 ponced Exp $

;; This file is not part of Emacs

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
;; This library defines commands and a minor mode to navigate between
;; semantic language tokens in the current buffer.  It uses Eric Ludlam's
;; Semantic Bovinator tool to parse the buffer and find the language
;; tokens.

;; The commands `senator-next-token' and `senator-previous-token'
;; navigate respectively to the token after or before the point.  The
;; command `senator-jump' directly jumps to a particular semantic
;; symbol.

;; Also, for each built-in search command `search-forward',
;; `search-backward', `re-search-forward', `re-search-backward',
;; `word-search-forward' and `word-search-backward', there is an
;; equivalent `senator-<search-command>' defined which searches only
;; in semantic token names.

;; The command `senator-isearch-toggle-semantic-mode' toggles semantic
;; search in isearch mode.  When semantic search is enabled, isearch
;; is restricted to token names.

;; Finally, the library provides a `senator-minor-mode' to easily
;; enable or disable the SEmantic NAvigaTOR stuff for the current
;; buffer.

;; The best way to use navigation commands is to bind them to keyboard
;; shortcuts.  Senator minor mode uses the common prefix key "C-c ,".
;; The following default key bindings are provided when semantic minor
;; mode is enabled:
;;
;;    key             binding
;;    ---             -------
;;    C-c , n         `senator-next-token'
;;    C-c , p         `senator-previous-token'
;;    C-c , j         `senator-jump'
;;    C-c , i         `senator-isearch-toggle-semantic-mode'
;;    C-c , TAB       `senator-complete-symbol'
;;    C-c , SPC       `senator-completion-menu-popup'
;;    S-mouse-3       `senator-completion-menu-popup'
;;    C-c , C-y       `senator-yank-token'
;;    C-c , C-w       `senator-kill-token'
;;    C-c , M-w       `senator-copy-token'
;;
;; To install, put this file on your Emacs-Lisp load path and add
;;   (require 'senator)
;; into your ~/.emacs startup file.  To enable senator stuff in the
;; current buffer use
;;   (senator-minor-mode 1).

;; You can customize the `senator-step-at-token-ids' to navigate (and
;; search) only between tokens of a particular type.  (Such as
;; functions and variables.)

;; Customize `senator-step-at-start-end-token-ids' to stop at the
;; start and end of the specified token types.

;; To have a mode specific customization, do something like this in a hook:
;;
;; (add-hook 'mode-hook
;;           (lambda ()
;;             (setq senator-step-at-token-ids '(function variable))
;;             (setq senator-step-at-start-end-token-ids '(function))
;;             ))
;;
;; The above example specifies to navigate (and search) only between
;; functions and variables, and to step at start and end of functions
;; only.
;;
;; Any comments, suggestions, bug reports or upgrade requests are
;; welcome.  Please send them to David Ponce at <david@dponce.com>

;;; History:
;;

;;; Code:
(require 'semantic)
(require 'semantic-ctxt)
(require 'semantic-imenu)
(eval-when-compile
  (require 'semanticdb)
  )

;;; Customization
(defgroup senator nil
  "SEmantic NAvigaTOR."
  :group 'semantic)

;;;###autoload
(defcustom global-senator-minor-mode nil
  "*If non-nil enable global use of senator minor mode."
  :group 'senator
  :type 'boolean
  :require 'senator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-senator-minor-mode (if val 1 -1))))

(defcustom senator-minor-mode-hook nil
  "Hook run at the end of function `senator-minor-mode'."
  :group 'senator
  :type 'hook)

(defcustom senator-step-at-token-ids nil
  "*List of token identifiers where to step.
Token identifier is symbol 'variable, 'function, 'type, or other.  If
nil navigation steps at any token found.  This is a buffer local
variable.  It can be set in a mode hook to get a specific langage
navigation."
  :group 'senator
  :type '(repeat (symbol)))
(make-variable-buffer-local 'senator-step-at-token-ids)

(defcustom senator-step-at-start-end-token-ids '(function)
  "*List of token identifiers where to step at start and end.
Token identifier is symbol 'variable, 'function, 'type, or other.  If
nil navigation only step at beginning of tokens.  If t step at start
and end of any token where it is allowed to step.  Also, stepping at
start and end of a token prevent stepping inside its children.  This
is a buffer local variable.  It can be set in a mode hook to get a
specific langage navigation."
  :group 'senator
  :type '(choice :tag "Identifiers"
                 (repeat :menu-tag "Symbols" (symbol))
                 (const  :tag "All" t)))
(make-variable-buffer-local 'senator-step-at-start-end-token-ids)

(defcustom senator-highlight-found t
  "*If non-nil highlight tokens found.
This option requires semantic 1.3 and above.  This is a buffer
local variable.  It can be set in a mode hook to get a specific
langage behaviour."
  :group 'senator
  :type 'boolean)
(make-variable-buffer-local 'senator-highlight-found)

;;; Faces
(defface senator-momentary-highlight-face '((((class color) (background dark))
                                             (:background "gray30"))
                                            (((class color) (background light))
                                             (:background "gray70")))
  "Face used to momentarily highlight tokens."
  :group 'semantic-faces)

(defface senator-intangible-face '((((class color) (background light))
                                    (:foreground "gray25"))
                                   (((class color) (background dark))
                                    (:foreground "gray75")))
  "Face placed on intangible text."
  :group 'semantic-faces)

(defface senator-read-only-face '((((class color) (background dark))
                                   (:background "#664444"))
                                  (((class color) (background light))
                                   (:background "#CCBBBB")))
  "Face placed on read-only text."
  :group 'semantic-faces)

;;;;
;;;; Common functions
;;;;

(defsubst senator-parse ()
  "Parse the current buffer and return the tokens where to navigate."
  (semantic-bovinate-toplevel t))

(defsubst senator-current-token ()
  "Return the current token in the current buffer.
Raise an error is there is no token here."
  (or (semantic-current-nonterminal)
      (error "No semantic tokens here")))

(defun senator-momentary-highlight-token (token)
  "Momentary highlight TOKEN.
Does nothing if `senator-highlight-found' is nil."
  (and senator-highlight-found
       (semantic-momentary-highlight-token
        token
        'senator-momentary-highlight-face)))

(defun senator-step-at-start-end-p (token)
  "Return non-nil if must step at start and end of TOKEN."
  (and token
       (or (eq senator-step-at-start-end-token-ids t)
           (memq (semantic-token-token token)
                 senator-step-at-start-end-token-ids))))

(defun senator-skip-p (token)
  "Return non-nil if must skip TOKEN."
  (and token
       senator-step-at-token-ids
       (not (memq (semantic-token-token token)
                  senator-step-at-token-ids))))

(defun senator-middle-of-token-p (pos token)
  "Return non-nil if POS is between start and end of TOKEN."
  (and (> pos (semantic-token-start token))
       (< pos (semantic-token-end   token))))

(defun senator-step-at-parent (token)
  "Return TOKEN's outermost parent if must step at start/end of it.
Return nil otherwise."
  (if token
      (let (parent parents)
        (setq parents (semantic-find-nonterminal-by-overlay
                       (semantic-token-start token)))
        (while (and parents (not parent))
          (setq parent  (car parents)
                parents (cdr parents))
          (if (or (eq token parent)
                  (senator-skip-p parent)
                  (not (senator-step-at-start-end-p parent)))
              (setq parent nil)))
        parent)))

(defun senator-previous-token-or-parent (pos)
  "Return the token before POS or one of its parent where to step."
  (let ((token (semantic-find-nonterminal-by-overlay-prev pos)))
    (or (senator-step-at-parent token) token)))

(defun senator-full-token-name (token parent)
  "Compose a full name from TOKEN name and PARENT names.
That is append to TOKEN name PARENT names each one separated by
`semantic-type-relation-separator-character'.  The PARENT list is in
reverse order."
  (let ((sep  (car semantic-type-relation-separator-character))
        (name ""))
    (while parent
      (setq name (concat name sep
                         (semantic-token-name (car parent)))
            parent (cdr parent)))
    (concat (semantic-token-name token) name)))

(defvar senator-completion-cache nil
  "The latest full completion list is cached here.")
(make-variable-buffer-local 'senator-completion-cache)

(defun senator-completion-cache-flush-fcn (&optional ignore)
  "Function called as a hook to clear the completion list cache.
This is added to `semantic-before-toplevel-cache-flush-hook' and
`semantic-clean-token-hooks'.  IGNORE arguments."
  (setq senator-completion-cache nil))

(defun senator-completion-flatten-stream (stream parents &optional top-level)
  "Return a flat list of all tokens available in STREAM.
PARENTS is the list of parent tokens.  Each element of the list is a
pair (TOKEN . PARENTS) where PARENTS is the list of TOKEN parent
tokens or nil.  If TOP-LEVEL is non-nil the completion list will
contain only tokens at top level.  Otherwise all children tokens are
included too."
  (let (fs e token children)
    (while stream
      (setq token  (car stream)
            stream (cdr stream)
            e      (cons token parents)
            fs     (cons e fs))
      (and (not top-level)
           ;; Not include function arguments
           (not (eq (semantic-token-token token) 'function))
           (setq children (semantic-nonterminal-children token t))
           (setq fs (append fs (senator-completion-flatten-stream
                                children e)))))
    fs))

(defun senator-completion-function-args (token)
  "Return a string of argument names from function TOKEN."
  (mapconcat #'(lambda (arg)
                 (if (semantic-token-p arg)
                     (semantic-token-name arg)
                   (format "%s" arg)))
             (semantic-token-function-args token)
             semantic-function-argument-separation-character))

(defun senator-completion-refine-name (elt)
  "Refine the name part of ELT.
ELT has the form (NAME . (TOKEN . PARENTS)).  The NAME refinement is
done in the following incremental way:

- - If TOKEN is a function one append the argument name list to NAME.

- - If TOKEN is a type one append \"{}\" to NAME.

- - If TOKEN is an include one append \"#\" to NAME.

- - If TOKEN is a package one append \"=\" to NAME.

- - If TOKEN has PARENTS append the next parent name to NAME,
    separated by the first string found in
    `semantic-type-relation-separator-character'.

- - Otherwise NAME is set to \"token-name@token-start-position\"."
  (let* ((sep     (car semantic-type-relation-separator-character))
         (name    (car elt))
         (token   (car (cdr elt)))
         (parents (cdr (cdr elt)))
         (oname   (semantic-token-name token))
         (tokt    (semantic-token-token token)))
    (cond
     ((and (eq tokt 'function) (string-equal name oname))
      (setq name (format "%s(%s)" name
                         (senator-completion-function-args token))))
     ((and (eq tokt 'type) (string-equal name oname))
      (setq name (format "%s{}" name)))
     ((and (eq tokt 'include) (string-equal name oname))
      (setq name (format "%s#" name)))
     ((and (eq tokt 'package) (string-equal name oname))
      (setq name (format "%s=" name)))
     (parents
      (setq name (format "%s%s%s" name
                         (if (eq (semantic-token-token (car parents))
                                 'function)
                             ")" sep)
                         (semantic-token-name (car parents)))
            parents (cdr parents)))
     (t
      (setq name (format "%s@%d" oname
                         (semantic-token-start token)))))
    (setcar elt name)
    (setcdr elt (cons token parents))))

(defun senator-completion-uniquify-names (completion-stream)
  "Uniquify names in COMPLETION-STREAM.
That is refine the name part of each COMPLETION-STREAM element until
there is no duplicated names.  Each element of COMPLETION-STREAM has
the form (NAME . (TOKEN . PARENTS)).  See also the function
`senator-completion-refine-name'."
  (let ((completion-stream (sort completion-stream
                                 #'(lambda (e1 e2)
                                     (string-lessp (car e1)
                                                   (car e2)))))
        (dupp t)
        clst elt dup name)
    (while dupp
      (setq dupp nil
            clst completion-stream)
      (while clst
        (setq elt  (car clst)
              name (car elt)
              clst (cdr clst)
              dup  (and clst
                        (string-equal name (car (car clst)))
                        elt)
              dupp (or dupp dup))
        (while dup
          (senator-completion-refine-name dup)
          (setq elt (car clst)
                dup (and elt (string-equal name (car elt)) elt))
          (and dup (setq clst (cdr clst))))))
    ;; Return a usable completion alist where each element has the
    ;; form (NAME . TOKEN).
    (setq clst completion-stream)
    (while clst
      (setq elt  (car clst)
            clst (cdr clst))
      (setcdr elt (car (cdr elt))))
    completion-stream))

(defun senator-completion-stream (stream &optional top-level)
  "Return a useful completion list from tokens in STREAM.
That is an alist of all (COMPLETION-NAME . TOKEN) available.
COMPLETION-NAME is an unique token name (see also the function
`senator-completion-uniquify-names').  If TOP-LEVEL is non-nil the
completion list will contain only tokens at top level.  Otherwise all
sub tokens are included too."
  (let* ((fs (senator-completion-flatten-stream stream nil top-level))
         cs elt tok)
    ;; Transform each FS element from (TOKEN . PARENTS)
    ;; to (NAME . (TOKEN . PARENT)).
    (while fs
      (setq elt (car fs)
            tok (car elt)
            fs  (cdr fs)
            cs  (cons (cons (semantic-token-name tok) elt) cs)))
    ;; Return a completion list with unique COMPLETION-NAMEs.
    (senator-completion-uniquify-names cs)))

(defun senator-current-type-context ()
  "Return tokens in the type context at point or nil if not found."
  (let ((context (semantic-find-nonterminal-by-token
                  'type (semantic-find-nonterminal-by-overlay))))
    (if context
        (semantic-token-type-parts
         (nth (1- (length context)) context)))))

(defun senator-completion-list (&optional in-context)
  "Return a useful completion list from tokens in current buffer.
If IN-CONTEXT is non-nil return only the top level tokens in the type
context at point or the top level tokens in the current buffer if no
type context exists at point."
  (let (stream)
    (if in-context
        (setq stream (senator-current-type-context)))
    (or stream (setq stream (senator-parse)))
    ;; IN-CONTEXT completion doesn't use nor set the cache.
    (or (and (not in-context) senator-completion-cache)
        (let ((clst (senator-completion-stream stream in-context)))
          (or in-context
              (setq senator-completion-cache clst))
          clst))))

;;; Senator stream searching functions:
;;
(defun senator-find-nonterminal-by-name (name)
  "Find a token with NAME.
Uses `semanticdb' when available."
  (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
      ;; semanticdb version returns a list of (DB-TABLE . TOKEN)
      (semanticdb-find-nonterminal-by-name name nil t)
    ;; for semantic version just return TOKEN
    (semantic-find-nonterminal-by-name name (current-buffer) t)))

(defun senator-find-nonterminal-by-name-regexp (regexp)
  "Find all tokens with a name matching REGEXP.
Uses `semanticdb' when available."
  (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
      ;; semanticdb version returns a list of (DB-TABLE . TOKEN-LIST)
      (semanticdb-find-nonterminal-by-name-regexp regexp nil t)
    ;; for semantic version just return TOKEN-LIST
    (semantic-find-nonterminal-by-name-regexp regexp (current-buffer) t)))

;;;;
;;;; Search functions
;;;;

(defun senator-search-token-name (token)
  "Search the TOKEN name in TOKEN bounds.
Set point to the end of the name, and return point.  To get the
beginning of the name use (match-beginning 0)."
  (let ((name (semantic-token-name token)))
    (setq name (if (string-match "\\`\\([^[]+\\)[[]" name)
                   (match-string 1 name)
                 name))
    (goto-char (semantic-token-start token))
    (re-search-forward (concat
                        ;; the token name is at the beginning of a
                        ;; word or after a whitespace or a punctuation
                        "\\(\\<\\|\\s-+\\|\\s.\\)"
                        (regexp-quote name))
                       (semantic-token-end token))
    (goto-char (match-beginning 0))
    (search-forward name)))

(defun senator-search-forward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in Semantic token names after point.
See `search-forward' for the meaning of BOUND NOERROR and COUNT."
  (let* ((origin (point))
         (count  (or count 1))
         (step   (cond ((= count 0) 0)
                       ((> count 0) 1)
                       (t (setq count (- count))
                          -1)))
         found next sstart send token tstart tend)
    (or (= step 0)
        (while (and (not found)
                    (setq next (funcall searcher what bound t step)))
          (setq sstart (match-beginning 0)
                send   (match-end 0)
                token  (semantic-current-nonterminal))
        (if (= sstart send)
            (setq found t)
          (if token
              (setq tend   (senator-search-token-name token)
                    tstart (match-beginning 0)
                    found  (and (>= sstart tstart)
                                (<= send tend)
                                (= (setq count (1- count)) 0))))
          (goto-char next))))
    (cond ((null found)
           (setq next origin
                 send origin))
          ((= step -1)
           (setq next send
                 send sstart))
          (t
           (setq next sstart)))
    (goto-char next)
    ;; Setup the returned value and the `match-data' or maybe fail!
    (funcall searcher what send noerror step)))

(defun senator-search-backward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in Semantic token names before point.
See `search-backward' for the meaning of BOUND NOERROR and COUNT."
  (let* ((origin (point))
         (count  (or count 1))
         (step   (cond ((= count 0) 0)
                       ((> count 0) 1)
                       (t (setq count (- count))
                          -1)))
         found next sstart send token tstart tend)
    (or (= step 0)
        (while (and (not found)
                    (setq next (funcall searcher what bound t step)))
          (setq sstart (match-beginning 0)
                send   (match-end 0)
                token  (semantic-current-nonterminal))
        (if (= sstart send)
            (setq found t)
          (if token
              (setq tend   (senator-search-token-name token)
                    tstart (match-beginning 0)
                    found  (and (>= sstart tstart)
                                (<= send tend)
                                (= (setq count (1- count)) 0))))
          (goto-char next))))
    (cond ((null found)
           (setq next origin
                 send origin))
          ((= step 1)
           (setq next send
                 send sstart))
          (t
           (setq next sstart)))
    (goto-char next)
    ;; Setup the returned value and the `match-data' or maybe fail!
    (funcall searcher what send noerror step)))

;;;;
;;;; Navigation commands
;;;;

;;;###autoload
(defun senator-next-token ()
  "Navigate to the next Semantic token.
Return the token or nil if at end of buffer."
  (interactive)
  (let ((pos   (point))
        (token (semantic-current-nonterminal))
        where)
    (if (and token
             (not (senator-skip-p token))
             (senator-step-at-start-end-p token)
             (or (= pos (semantic-token-start token))
                 (senator-middle-of-token-p pos token)))
        nil
      (if (setq token (senator-step-at-parent token))
          nil
        (setq token (semantic-find-nonterminal-by-overlay-next pos))
        (while (and token (senator-skip-p token))
          (setq token (semantic-find-nonterminal-by-overlay-next
                       (semantic-token-start token))))))
    (if (not token)
        (progn
          (goto-char (point-max))
          (working-message "End of buffer"))
      (cond ((and (senator-step-at-start-end-p token)
                  (or (= pos (semantic-token-start token))
                      (senator-middle-of-token-p pos token)))
             (setq where "end")
             (goto-char (semantic-token-end token)))
            (t
             (setq where "start")
             (goto-char (semantic-token-start token))))
      (senator-momentary-highlight-token token)
      (working-message "%S: %s (%s)"
                       (semantic-token-token token)
                       (semantic-token-name  token)
                       where))
    token))

;;;###autoload
(defun senator-previous-token ()
  "Navigate to the previous Semantic token.
Return the token or nil if at beginning of buffer."
  (interactive)
  (let ((pos   (point))
        (token (semantic-current-nonterminal))
        where)
    (if (and token
             (not (senator-skip-p token))
             (senator-step-at-start-end-p token)
             (or (= pos (semantic-token-end token))
                 (senator-middle-of-token-p pos token)))
        nil
      (if (setq token (senator-step-at-parent token))
          nil
        (setq token (senator-previous-token-or-parent pos))
        (while (and token (senator-skip-p token))
          (setq token (senator-previous-token-or-parent
                       (semantic-token-start token))))))
    (if (not token)
        (progn
          (goto-char (point-min))
          (working-message "Beginning of buffer"))
      (cond ((or (not (senator-step-at-start-end-p token))
                 (= pos (semantic-token-end token))
                 (senator-middle-of-token-p pos token))
             (setq where "start")
             (goto-char (semantic-token-start token)))
            (t
             (setq where "end")
             (goto-char (semantic-token-end token))))
      (senator-momentary-highlight-token token)
      (working-message "%S: %s (%s)"
                       (semantic-token-token token)
                       (semantic-token-name  token)
                       where))
    token))

(defvar senator-jump-completion-list nil
  "`senator-jump' stores here its current completion list.
Then use `assoc' to retrieve the token associated to a symbol.")

(defun senator-jump-interactive (prompt &optional in-context no-default require-match)
  "Called interactively to provide completion on some tag name.

Use PROMPT.  If optional IN-CONTEXT is non-nil jump in the local
type's context \(see function `senator-current-type-context').  If
optional NO-DEFAULT is non-nil do not provide a default value.  If
optional REQUIRE-MATCH is non-nil an explicit match must be made.

The IN-CONTEXT and NO-DEFAULT switches are combined using the
following prefix arguments:

- - \\[universal-argument]       IN-CONTEXT.
- - \\[universal-argument] -     NO-DEFAULT.
- - \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (let* ((arg (prefix-numeric-value current-prefix-arg))
         (no-default
          (or no-default
              ;; The `completing-read' function provided by XEmacs
              ;; (21.1) don't allow a default value argument :-(
              (featurep 'xemacs)
              (= arg -1)                ; C-u -
              (= arg 16)))              ; C-u C-u
         (in-context
          (or in-context
              (= arg 4)                 ; C-u
              (= arg 16)))              ; C-u C-u
         (context
          (and (not no-default)
               (or (semantic-ctxt-current-symbol)
                   (semantic-ctxt-current-function))))
         (completing-read-args
          (list (if (and context (car context))
                    (concat prompt "(default: " (car context) ") ")
                  prompt)
                (setq senator-jump-completion-list
                      (senator-completion-list in-context))
                nil
                require-match
                ""
                'semantic-read-symbol-history)))
    (list
     (apply #'completing-read
            (if (and context (car context))
                (append completing-read-args context)
              completing-read-args))
     in-context no-default)))

(defun senator-jump-noselect (sym &optional next-p regexp-p)
  "Jump to the semantic symbol SYM.
If NEXT-P is non-nil, then move the the next tag in the search
assuming there was already one jump for the given symbol.
If REGEXP-P is non nil, then treat SYM as a regular expression.
Return the token jumped to.
Note: REGEXP-P doesn't work yet.  This needs to be added to get
the etags override to be fully functional."
  (let ((token (cdr (assoc sym senator-jump-completion-list))))
    (when token
      (set-buffer (semantic-token-buffer token))
      (goto-char (semantic-token-start token))
      token)))

;;;###autoload
(defun senator-jump (sym &optional in-context no-default)
  "Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- - \\[universal-argument]       IN-CONTEXT.
- - \\[universal-argument] -     NO-DEFAULT.
- - \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: " nil nil t))
  (push-mark)
  (let ((tok (senator-jump-noselect sym no-default)))
    (when tok
      (switch-to-buffer (semantic-token-buffer tok))
      (senator-momentary-highlight-token tok)
      (working-message "%S: %s "
                       (semantic-token-token tok)
                       (semantic-token-name  tok)))))

;;;###autoload
(defun senator-jump-regexp (symregex &optional in-context no-default)
  "Jump to the semantic symbol SYMREGEX.
SYMREGEX is treated as a regular expression.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value and move to the
next match of SYMREGEX.  NOTE: Doesn't actually work yet.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- - \\[universal-argument]       IN-CONTEXT.
- - \\[universal-argument] -     NO-DEFAULT.
- - \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: "))
  (let ((tok (senator-jump-noselect symregex no-default)))
    (when tok
      (switch-to-buffer (semantic-token-buffer tok))
      (senator-momentary-highlight-token tok)
      (working-message "%S: %s "
                       (semantic-token-token tok)
                       (semantic-token-name  tok)))))

(defvar senator-last-completion-stats nil
  "The last senator completion was here.
Of the form (BUFFER STARTPOS INDEX REGEX COMPLIST...)")

(defsubst senator-current-symbol-start ()
  "Return position of start of the current symbol under point or nil."
  (condition-case nil
      (save-excursion (forward-sexp -1) (point))
    (error nil)))

;;;###autoload
(defun senator-complete-symbol (&optional cycle-once)
  "Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches."
  (interactive)
  (let ((symstart (senator-current-symbol-start))
        regex complst)
    (if symstart
        ;; Get old stats if apropriate.
        (if (and senator-last-completion-stats
                 ;; Check if completing in the same buffer
                 (eq (car senator-last-completion-stats) (current-buffer))
                 ;; Check if completing from the same point
                 (= (nth 1 senator-last-completion-stats) symstart)
                 ;; Check if completing the same symbol
                 (save-excursion
                   (goto-char symstart)
                   (looking-at (nth 3 senator-last-completion-stats))))
             
            (setq complst (nthcdr 4 senator-last-completion-stats))

          (setq regex (regexp-quote (buffer-substring symstart (point)))
                complst (let ((found
                               (senator-find-nonterminal-by-name-regexp
                                (concat "^" regex))))
                          (if (and found (semantic-token-p (car found)))
                              found
                            (apply #'append (mapcar #'cdr found))))
                senator-last-completion-stats (append (list (current-buffer)
                                                            symstart
                                                            0
                                                            regex)
                                                      complst))))
    ;; Do the completion if apropriate.
    (if complst
        (let ((ret   t)
              (index (nth 2 senator-last-completion-stats))
              newtok)
          (if (= index (length complst))
              ;; Cycle to the first completion token.
              (setq index  0
                    ;; Stop completion if CYCLE-ONCE is non-nil.
                    ret (not cycle-once)))
          ;; Get the new completion token.
          (setq newtok (nth index complst))
          (when ret
            ;; Move index to the next completion token.
            (setq index (1+ index)
                  ;; Return the completion string (useful to hippie
                  ;; expand for example)
                  ret   (semantic-token-name newtok))
            ;; Replace the string.
            (delete-region symstart (point))
            (insert ret))
          ;; Update the completion index.
          (setcar (nthcdr 2 senator-last-completion-stats) index)
          ret))))

;;;;
;;;; Completion menu
;;;;

(defcustom senator-completion-menu-summary-function
  'semantic-concise-prototype-nonterminal
  "*Function to use when creating items in completion menu.
Some useful functions are in `semantic-token->text-functions'."
  :group 'senator
  :type semantic-token->text-custom-list)
(make-variable-buffer-local 'senator-completion-menu-summary-function)

(defcustom senator-completion-menu-insert-function
  'senator-completion-menu-insert-default
  "*Function to use to insert an item from completion menu.
It will receive a Semantic token as argument."
  :group 'senator
  :type '(radio (const senator-completion-menu-insert-default)
                (function)))
(make-variable-buffer-local 'senator-completion-menu-insert-function)

(defun senator-completion-menu-insert-default (token)
  "Insert a text representation of TOKEN at point."
  (insert (semantic-token-name token)))

(defun senator-completion-menu-do-complete (token-array)
  "Replace the current syntactic expression with a chosen completion.
Argument TOKEN-ARRAY is an array of one element containting the token
choosen from the completion menu."
  (let ((token (aref token-array 0))
        (symstart (senator-current-symbol-start))
        (finsert (if (fboundp senator-completion-menu-insert-function)
                     senator-completion-menu-insert-function
                   #'senator-completion-menu-insert-default)))
    (if symstart
        (progn
          (delete-region symstart (point))
          (funcall finsert token)))))

(defun senator-completion-menu-item (token)
  "Return a completion menu item from TOKEN.
That is a pair (MENU-ITEM-TEXT . TOKEN-ARRAY).  TOKEN-ARRAY is an
array of one element containting TOKEN.  Can return nil to discard a
menu item."
  (if (semantic-token-p token)
      (cons (funcall (if (fboundp senator-completion-menu-summary-function)
                         senator-completion-menu-summary-function
                       #'semantic-prototype-nonterminal) token)
            (vector token))
    (cons (file-name-sans-extension (oref (car token) file))
          (delq nil
                (mapcar #'senator-completion-menu-item
                        (cdr token))))))
          
(defun senator-completion-menu-window-offsets (&optional window)
  "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET)) in
WINDOW'S frame."
  (let* ((window  (or window (selected-window)))
         (e       (window-edges window))
         (left    (nth 0 e))
         (top     (nth 1 e))
         (right   (nth 2 e))
         (bottom  (nth 3 e))
         (x       (+ left (/ (- right left) 2)))
         (y       (+ top  (/ (- bottom top) 2)))
         (wpos    (coordinates-in-window-p (cons x y) window))
         (xoffset 0)
         (yoffset 0))
    (if (consp wpos)
        (let* ((f  (window-frame window))
               (cy (/ 1.0 (float (frame-char-height f)))))
          (setq xoffset (- x (car wpos))
                yoffset (float (- y (cdr wpos))))
          ;; If Emacs 21 add to:
          ;; - XOFFSET the WINDOW left margin width.
          ;; - YOFFSET the height of header lines above WINDOW.
          (if (> emacs-major-version 20)
              (progn
                (setq wpos    (cons (+ left xoffset) 0.0)
                      bottom  (float bottom))
                (while (< (cdr wpos) bottom)
                  (if (eq (coordinates-in-window-p wpos window)
                          'header-line)
                      (setq yoffset (+ yoffset cy)))
                  (setcdr wpos (+ (cdr wpos) cy)))
                (setq xoffset (floor (+ xoffset
                                        (or (car (window-margins window))
                                            0))))))
          (setq yoffset (floor yoffset))))
    (cons xoffset yoffset)))

(defun senator-completion-menu-point-as-event()
  "Returns the text cursor position as an event.
Also move the mouse pointer to the cursor position."
  (let* ((w (get-buffer-window (current-buffer)))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
         )
    (if (featurep 'xemacs)
        (let* ((at (progn (set-mouse-position w x (1- y))
                          (cdr (mouse-pixel-position))))
               (x  (car at))
               (y  (cdr at)))
          (make-event 'button-press
                      (list 'button 3
                            'modifiers nil
                            'x x
                            'y y)))
      ;; Emacs
      (let ((offsets (senator-completion-menu-window-offsets w)))
        ;; Convert window position (x,y) to the equivalent frame
        ;; position and move the mouse pointer to it.
        (set-mouse-position (window-frame w)
                            (+ x (car offsets))
                            (+ y (cdr offsets)))
        t))))

;;;###autoload
(defun senator-completion-menu-popup ()
  "Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu."
  (interactive)
  (let ((symstart (senator-current-symbol-start))
        symbol regexp complst)
    (if symstart
        (setq symbol  (buffer-substring-no-properties symstart (point))
              regexp  (concat "^" (regexp-quote symbol))
              complst (senator-find-nonterminal-by-name-regexp regexp)))
    (if (not complst)
        (error "No completions available"))
    ;; We have a completion list, build a menu
    (let ((index (delq nil
                       (mapcar #'senator-completion-menu-item
                               complst)))
          title item)
      (cond ;; Here index is a menu structure like:
       
       ;; -1- (("menu-item1" . [token1]) ...)
       ((vectorp (cdr (car index)))
        ;; There are more than one item, setup the popup title.
        (if (cdr index)
            (setq title (format "%S completion" symbol))
          ;; Only one item , no need to popup the menu.
          (setq item (car index))))
       
       ;; -2- (("menu-title1" ("menu-item1" . [token1]) ...) ...)
       (t
        ;; There are sub-menus.
        (if (cdr index)
            ;; Several sub-menus, setup the popup title.
            (setq title (format "%S completion" symbol))
          ;; Only one sub-menu, convert it to a main menu and add the
          ;; sub-menu title (filename) to the popup title.
          (setq title (format "%S completion (%s)"
                              symbol (car (car index)))
                index (cdr (car index)))
          ;; But...
          (or (cdr index)
              ;; ... If only one menu item, no need to popup the menu.
              (setq item (car index))))))
      
      (or item
          (setq item ;; Delegates menu handling to imenu :-)
                (imenu--mouse-menu
                 index
                 ;; popup at point
                 (senator-completion-menu-point-as-event)
                 title)))
      (if item
          (senator-completion-menu-do-complete (cdr item))))))

;;;;
;;;; Search commands
;;;;

;;;###autoload
(defun senator-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for string WHAT.
Set point to the end of the occurrence found, and return point.  See
`search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  COUNT is just ignored in the current implementation."
  (interactive "sSemantic search: ")
  (senator-search-forward-raw #'search-forward what bound noerror count))

;;;###autoload
(defun senator-re-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for regexp WHAT.
Set point to the end of the occurrence found, and return point.  See
`re-search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  COUNT is just ignored in the current implementation."
  (interactive "sSemantic regexp search: ")
  (senator-search-forward-raw #'re-search-forward what bound noerror count))

;;;###autoload
(defun senator-word-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for word WHAT.
Set point to the end of the occurrence found, and return point.  See
`word-search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  COUNT is just ignored in the current implementation."
  (interactive "sSemantic word search: ")
  (senator-search-forward-raw #'word-search-forward what bound noerror count))

;;;###autoload
(defun senator-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for string WHAT.
Set point to the beginning of the occurrence found, and return point.
See `search-backward' for details and the meaning of BOUND NOERROR and
COUNT.  COUNT is just ignored in the current implementation."
  (interactive "sSemantic backward search: ")
  (senator-search-backward-raw #'search-backward what bound noerror count))

;;;###autoload
(defun senator-re-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for regexp WHAT.
Set point to the beginning of the occurrence found, and return point.
See `re-search-backward' for details and the meaning of BOUND NOERROR
and COUNT.  COUNT is just ignored in the current implementation."
  (interactive "sSemantic backward regexp search: ")
  (senator-search-backward-raw #'re-search-backward what bound noerror count))

;;;###autoload
(defun senator-word-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for word WHAT.
Set point to the beginning of the occurrence found, and return point.
See `word-search-backward' for details and the meaning of BOUND
NOERROR and COUNT.  COUNT is just ignored in the current
implementation."
  (interactive "sSemantic backward word search: ")
  (senator-search-backward-raw #'word-search-backward what bound noerror count))

;;;;
;;;; Others useful search commands (minor mode menu)
;;;;

;;; Compatibility
(or (not (featurep 'xemacs))
    (fboundp 'isearch-update-ring)

    ;; Provide `isearch-update-ring' function.
    ;; (from XEmacs 21.1.9 isearch-mode.el)
    (defun isearch-update-ring (string &optional regexp)
      "Add STRING to the beginning of the search ring.
REGEXP says which ring to use."
      (if (> (length string) 0)
          ;; Update the ring data.
          (if regexp
              (if (not (setq regexp-search-ring-yank-pointer
                             (member string regexp-search-ring)))
                  (progn
                    (setq regexp-search-ring
                          (cons string regexp-search-ring)
                          regexp-search-ring-yank-pointer regexp-search-ring)
                    (if (> (length regexp-search-ring) regexp-search-ring-max)
                        (setcdr (nthcdr (1- regexp-search-ring-max) regexp-search-ring)
                                nil))))
            (if (not (setq search-ring-yank-pointer
                           ;; really need equal test instead of eq.
                           (member string search-ring)))
                (progn
                  (setq search-ring (cons string search-ring)
                        search-ring-yank-pointer search-ring)
                  (if (> (length search-ring) search-ring-max)
                      (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))))
    
    )

(defun senator-nonincremental-search-forward (string)
  "Search for STRING  nonincrementally."
  (interactive "sSemantic search for string: ")
  (if (equal string "")
      (senator-search-forward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-forward string)))

(defun senator-nonincremental-search-backward (string)
  "Search backward for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (if (equal string "")
      (senator-search-backward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-backward string)))

(defun senator-nonincremental-re-search-forward (string)
  "Search for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (if (equal string "")
      (senator-re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-forward string)))

(defun senator-nonincremental-re-search-backward (string)
  "Search backward for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (if (equal string "")
      (senator-re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-backward string)))

(defun senator-nonincremental-repeat-search-forward ()
  "Search forward for the previous search string."
  (interactive)
  (if (null search-ring)
      (error "No previous search"))
  (senator-search-forward (car search-ring)))

(defun senator-nonincremental-repeat-search-backward ()
  "Search backward for the previous search string."
  (interactive)
  (if (null search-ring)
      (error "No previous search"))
  (senator-search-backward (car search-ring)))

(defun senator-nonincremental-repeat-re-search-forward ()
  "Search forward for the previous regular expression."
  (interactive)
  (if (null regexp-search-ring)
      (error "No previous search"))
  (senator-re-search-forward (car regexp-search-ring)))

(defun senator-nonincremental-repeat-re-search-backward ()
  "Search backward for the previous regular expression."
  (interactive)
  (if (null regexp-search-ring)
      (error "No previous search"))
  (senator-re-search-backward (car regexp-search-ring)))

;;;;
;;;; Token Properties
;;;;

(defun senator-toggle-read-only (&optional token)
  "Toggle the read-only status of the current TOKEN."
  (interactive)
  (let* ((tok  (or token (senator-current-token)))
         (read (semantic-token-read-only-p tok)))
    (semantic-set-token-read-only tok read)
    (semantic-set-token-face
     tok
     (if read nil 'senator-read-only-face))))

(defun senator-toggle-intangible (&optional token)
  "Toggle the tangibility of the current TOKEN."
  (interactive)
  (let* ((tok (or token (senator-current-token)))
         (tang (semantic-token-intangible-p tok)))
    (semantic-set-token-intangible tok tang)
    (semantic-set-token-face
     tok
     (if tang nil 'senator-intangible-face))))

(defun senator-set-face (face &optional token)
  "Set the foreground FACE of the current TOKEN."
  (interactive (list (read-face-name
                      (if (featurep 'xemacs)
                          "Face: "
                        ;; GNU Emacs already append ": "
                        "Face"))))
  (let ((tok (or token (senator-current-token))))
    (semantic-set-token-face tok face)))

(defun senator-set-foreground (color &optional token)
  "Set the foreground COLOR of the current TOKEN."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Foreground color: ")))
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face)
        (error "Unknown color: %s" color))
    (senator-set-face face)))

(defun senator-set-background (color &optional token)
  "Set the background COLOR of the current TOKEN."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Background color: ")))
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
        (error "Unknown color: %s" color))
    (senator-set-face face)))

(defun senator-clear-token (&optional token)
  "Clear all properties from TOKEN."
  (interactive)
  (let ((tok (or token (senator-current-token))))
    (semantic-set-token-read-only  tok t)
    (semantic-set-token-intangible tok t)
    (semantic-set-token-face       tok nil)))

;;;;
;;;; Misc. menu stuff.
;;;;

(defun senator-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if (featurep 'xemacs)
      (let ((n (length item))
            (i 0)
            slot l)
        (while (< i n)
          (setq slot (aref item i))
          (if (and (keywordp slot)
                   (eq slot :help))
              (setq i (1+ i))
            (setq l (cons slot l)))
          (setq i (1+ i)))
        (apply #'vector (nreverse l)))
    item))

;;;;
;;;; The dynamic sub-menu of Semantic minor modes.
;;;;
(defvar senator-registered-mode-entries  nil)
(defvar senator-registered-mode-settings nil)
(defvar senator-modes-menu-cache nil)

(defun senator-register-command-menu (spec global)
  "Register the minor mode menu item specified by SPEC.
Return a menu item allowing to change the corresponding minor mode
setting.  If GLOBAL is non-nil SPEC defines a global setting else a
local setting.

SPEC must be a list of the form:

\(CALLBACK [ KEYWORD ARG ] ... )

Where KEYWORD is one of those recognized by `easy-menu-define' plus:

:save VARIABLE

VARIABLE is a variable that will be saved by Custom when using the
\"Modes/Save global settings\" menu item.  This keyword is ignored if
GLOBAL is nil.

By default the returned menu item is setup with:

:active t :style toggle :selected CALLBACK.

So when :selected is not specified the function assumes that CALLBACK
is a symbol which refer to a bound variable too."
  (and (consp spec)
       (symbolp (car spec))
       (fboundp (car spec))
       (let* ((callback (car spec))     ; callback function
              (props    (cdr spec))     ; properties
              (selected callback)       ; selected default to callback
              (active   t)              ; active by default
              (style    'toggle)        ; toggle style by default
              (save     nil)            ; what to save via custom
              item key val)
         (while props
           (setq key   (car  props)
                 val   (cadr props)
                 props (cddr props))
           (cond
            ((eq key :save)
             (setq save val))
            ((eq key :active)
             (setq active val))
            ((eq key :style)
             (setq style val))
            ((eq key :selected)
             (setq selected val))
            (t
             (setq item (cons key (cons val item))))))
         (if (and global save (symbolp save) (boundp save))
             (add-to-list 'senator-registered-mode-settings save))
         (setq item (cons :selected (cons selected item))
               item (cons :active   (cons active   item))
               item (cons :style    (cons style    item))
               item (cons callback  item)))))

(defun senator-register-mode-menu-entry (name local global)
  "Register a minor mode menu entry.
This will add menu items to the \"Modes\" menu allowing to change the
minor mode settings.  NAME is the name displayed in the menu.  LOCAL
and GLOBAL define command menu items to respectively change the minor
mode local and global settings.  nil means to omit the corresponding
menu item.  See the function `senator-register-command-menu' for the
command menu specification.  If NAME is already registered the
corresponding entry will be updated with the given LOCAL and GLOBAL
definitions.  If LOCAL and GLOBAL are both nil the NAME entry is
unregistered if present."
  ;; Clear the cached menu to rebuild it.
  (setq senator-modes-menu-cache nil)
  (let* ((entry (assoc name senator-registered-mode-entries))
         (local-item  (senator-register-command-menu local nil))
         (global-item (senator-register-command-menu global t)))
    (if (not (or local-item global-item))
        (setq senator-registered-mode-entries
              (delq entry senator-registered-mode-entries))
      (if entry
          (setcdr entry (cons local-item global-item))
        (setq entry (cons name (cons local-item global-item))
              senator-registered-mode-entries
              (nconc senator-registered-mode-entries (list entry))))
      entry)))

(defconst senator-base-local-label  "In this buffer")
(defconst senator-base-global-label "Globally")
(defvar   senator-uniquify-count    nil)

(defsubst senator-build-command-menu-item (label props)
  "Return a command menu item with an unique name based on LABEL.
PROPS is the list of properties of this menu item."
  (if props
      (senator-menu-item
       (apply #'vector
              (cons (concat label
                            (make-string senator-uniquify-count ?\ ))
                    props)))))

(defcustom senator-mode-menu-item-format "_ %s"
  "*Format of menu item labels in the \"Modes\" menu.
This format is used when Emacs is displaying through a window system.
The function `format' will receives an unique label string."
  :group 'senator
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (setq senator-modes-menu-cache nil)))

(defcustom senator-mode-menu-item-format-tty "%s (%s)"
  "*Format of menu item labels in the \"Modes\" menu.
This format is used when Emacs is using a text-only terminal.
The function `format' will receives the mode name and a label string."
  :group 'senator
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (setq senator-modes-menu-cache nil)))

(defun senator-build-mode-menu-items (entry)
  "Return menu items for the registered minor mode ENTRY.
Each entry will be displayed in the menu like this:

      Entry-Name
  [x] _ In this buffer
  [x] _ Globally"
  (let ((name   (car entry))
        (local  (cadr entry))
        (global (cddr entry)))
    (if window-system
        (setq local  (senator-build-command-menu-item
                      (format senator-mode-menu-item-format
                              senator-base-local-label)
                      local)
              global (senator-build-command-menu-item
                      (format senator-mode-menu-item-format
                              senator-base-global-label)
                      global)
              name   (vector name nil nil)
              senator-uniquify-count (1+ senator-uniquify-count))
      ;; Merge mode name in active menu items (the only ones
      ;; displayed) when using a text-only terminal.
      (setq local  (senator-build-command-menu-item
                    (format senator-mode-menu-item-format-tty
                            name senator-base-local-label)
                    local)
            global (senator-build-command-menu-item
                    (format senator-mode-menu-item-format-tty
                            name senator-base-global-label)
                    global)
            name    nil))
    (delq nil (list name local global))))

(defun senator-build-modes-menu (&rest ignore)
  "Build and return the \"Modes\" menu.
It is dynamically build from registered minor mode entries.  See also
the function `senator-register-mode-menu-entry'.
IGNORE any arguments.
This function is a menu :filter."
  (or senator-modes-menu-cache
      (setq senator-uniquify-count 0
            senator-modes-menu-cache
            (nconc
             (apply #'nconc (mapcar #'senator-build-mode-menu-items
                                    senator-registered-mode-entries))
             (list "--"
                   (senator-menu-item
                    [ "Save global settings"
                      senator-save-registered-mode-settings
                      :help "\
Save global settings of Semantic minor modes in your init file."
                      ]))))))

(defun senator-save-registered-mode-settings ()
  "Save current value of registered minor modes global setting.
The setting is saved by Custom.  See the function
`senator-register-mode-menu-entry' for details on how to register a
minor mode entry."
  (interactive)
  (let ((opts senator-registered-mode-settings)
        opt)
    (while opts
      (setq opt  (car opts)
            opts (cdr opts))
      (customize-save-variable opt (default-value opt)))))

;; Register the various minor modes settings used by Semantic.
(senator-register-mode-menu-entry
 "Senator"
 '(senator-minor-mode
   :help "Turn off Senator minor mode."
   )
 '(global-senator-minor-mode
   :help "Automatically turn on Senator on all Semantic buffers."
   :save global-senator-minor-mode
   )
 )

(senator-register-mode-menu-entry
 "Highlight Dirty Tokens"
 '(semantic-show-dirty-mode
   :help "Highlight tokens in the current buffer which need to be reparsed."
   )
 '(global-semantic-show-dirty-mode
   :help "Automatically highlight dirty tokens in all Semantic buffers."
   :save global-semantic-show-dirty-mode
   )
 )

(senator-register-mode-menu-entry
 "Highlight Unmatched Syntax"
 '(semantic-show-unmatched-syntax-mode
   :help "Highlight syntax which is not recognized valid syntax."
   )
 '(global-semantic-show-unmatched-syntax-mode
   :help "Automatically highlight unmatched syntax in all Semantic buffers."
   :save global-semantic-show-unmatched-syntax-mode
   )
 )

(senator-register-mode-menu-entry
 "Auto parse"
 '(semantic-auto-parse-mode
   :help "Automatically parse buffer following changes."
   )
 '(global-semantic-auto-parse-mode
   :help "Automatically parse all Semantic buffer following changes."
   :save global-semantic-auto-parse-mode
   )
 )

(senator-register-mode-menu-entry
 "Summaries"
 '(semantic-summary-mode
   :help "Show useful things about the Semantic token near point."
   )
 '(global-semantic-summary-mode
   :help "Automatically enable summary mode in all Semantic buffers."
   :save global-semantic-summary-mode
   )
 )


;;;;
;;;; Global minor mode to show token names in the mode line
;;;;

(condition-case nil
    (require 'which-func)
  (error nil))

(let ((select (cond
               ;; Emacs 21
               ((boundp 'which-function-mode)
                'which-function-mode)
               ;; Emacs < 21
               ((boundp 'which-func-mode-global)
                'which-func-mode-global)
               (t nil))))
  (if (and (fboundp 'which-func-mode) select)
      (senator-register-mode-menu-entry
       "Which Function"
       nil
       (list 'which-func-mode
             :select select
             :help "Enable `which-func-mode' and use it in Semantic buffers."
             :save select
             ))
    ))

(senator-register-mode-menu-entry
 "Semantic Database"
 nil
 '(semanticdb-toggle-global-mode
   :active (featurep 'semanticdb)
   :selected (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
   :suffix (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
               (if (semanticdb-write-directory-p
                    semanticdb-current-database)
                   "[persist]"
                 "[session]")
             "")
   :help "Cache tokens for killed buffers and between sessions."
   :save semanticdb-global-mode
   )
 )

;;;;
;;;; Senator minor mode
;;;;

(defvar senator-status nil
  "Minor mode status displayed in the mode line.")
(make-variable-buffer-local 'senator-status)

(defvar senator-isearch-semantic-mode nil
  "Non-nil if isearch does semantic search.
This is a buffer local variable.")
(make-variable-buffer-local 'senator-isearch-semantic-mode)

(defvar senator-prefix-key [(control ?c) ?,]
  "The common prefix key in senator minor mode.")

(defvar senator-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km "i" 'senator-isearch-toggle-semantic-mode)
    (define-key km "j" 'senator-jump)
    (define-key km "p" 'senator-previous-token)
    (define-key km "n" 'senator-next-token)
    (define-key km "\t" 'senator-complete-symbol)
    (define-key km " " 'senator-completion-menu-popup)
    (define-key km "\C-w" 'senator-kill-token)
    (define-key km "\M-w" 'senator-copy-token)
    (define-key km "\C-y" 'senator-yank-token)
    km)
  "Default key bindings in senator minor mode.")

(defvar senator-menu-bar
  (list
   "Senator"
   (list
    "Navigate"
    (senator-menu-item
     ["Next"
      senator-next-token
      :active t
      :help "Go to the next token found"
      ])
    (senator-menu-item
     ["Previous"
      senator-previous-token
      :active t
      :help "Go to the previous token found"
      ])
    (senator-menu-item
     ["Jump..."
      senator-jump
      :active t
      :help "Jump to a semantic symbol"
      ])
    )
   (list
    "Search"
    (senator-menu-item
     ["Search..."
      senator-nonincremental-search-forward
      :active t
      :help "Search forward for a string"
      ])
    (senator-menu-item
     ["Search Backwards..."
      senator-nonincremental-search-backward
      :active t
      :help "Search backwards for a string"
      ])
    (senator-menu-item
     ["Repeat Search"
      senator-nonincremental-repeat-search-forward
      :active search-ring
      :help "Repeat last search forward"
      ])
    (senator-menu-item
     ["Repeat Backwards"
      senator-nonincremental-repeat-search-backward
      :active search-ring
      :help "Repeat last search backwards"
      ])
    (senator-menu-item
     ["Search Regexp..."
      senator-nonincremental-re-search-forward
      :active t
      :help "Search forward for a regular expression"
      ])
    (senator-menu-item
     ["Search Regexp Backwards..."
      senator-nonincremental-re-search-backward
      :active t
      :help "Search backwards for a regular expression"
      ])
    (senator-menu-item
     ["Repeat Regexp"
      senator-nonincremental-repeat-re-search-forward
      :active regexp-search-ring
      :help "Repeat last regular expression search forward"
      ])
    (senator-menu-item
     ["Repeat Regexp Backwards"
      senator-nonincremental-repeat-re-search-backward
      :active regexp-search-ring
      :help "Repeat last regular expression search backwards"
      ])
    "--"
    (senator-menu-item
     ["Semantic isearch mode"
      senator-isearch-toggle-semantic-mode
      :active t
      :style toggle :selected senator-isearch-semantic-mode
      :help "Toggle semantic search in isearch mode"
      ])
    )
   (list
    "Token Properties"
;;;    (senator-menu-item
;;;     [ "Hide Token"
;;;       senator-make-invisible
;;;       :active t
;;;       :help "Make the current token invisible"
;;;       ])
;;;    (senator-menu-item
;;;     [ "Show Token"
;;;       senator-make-visible
;;;       :active t
;;;       :help "Make the current token invisible"
;;;       ])
    (senator-menu-item
     [ "Read Only"
       senator-toggle-read-only
       :active (semantic-current-nonterminal)
       :style toggle
       :selected (let ((tok (semantic-current-nonterminal)))
                   (and tok (semantic-token-read-only-p tok)))
       :help "Make the current token read-only"
       ])
    (senator-menu-item
     [ "Intangible"
       senator-toggle-intangible
       ;; XEmacs extent `intangible' property seems to not exists.
       :active (and (not (featurep 'xemacs))
                    (semantic-current-nonterminal))
       :style toggle
       :selected (and (not (featurep 'xemacs))
                      (let ((tok (semantic-current-nonterminal)))
                        (and tok (semantic-token-intangible-p tok))))
       :help "Make the current token intangible"
       ])
    (senator-menu-item
     [ "Set Token Face"
       senator-set-face
       :active (semantic-current-nonterminal)
       :help "Set the face on the current token"
       ])
    (senator-menu-item
     [ "Set Token Foreground"
       senator-set-foreground
       :active (semantic-current-nonterminal)
       :help "Set the foreground color on the current token"
       ])
    (senator-menu-item
     [ "Set Token Background"
       senator-set-background
       :active (semantic-current-nonterminal)
       :help "Set the background color on the current token"
       ])
    (senator-menu-item
     [ "Remove all properties"
       senator-clear-token
       :active (semantic-current-nonterminal)
       :help "Remove all special face properties on the current token "
       ] )
    )
   (list
    "Token Copy/Paste"
    (senator-menu-item
     [ "Copy Token"
       senator-copy-token
       :active (semantic-current-nonterminal)
       :help "Copy the current token to the token ring"
       ])
    (senator-menu-item
     [ "Kill Token"
       senator-kill-token
       :active (semantic-current-nonterminal)
       :help "Kill token text to the kill ring, and copy the token to the token ring"
       ])
    (senator-menu-item
     [ "Yank Token"
       senator-yank-token
       :active (not (ring-empty-p senator-token-ring))
       :help "Yank a token from the token ring, inserting a summary/prototype"
       ])
    (senator-menu-item
     [ "Copy Token to Register"
       senator-copy-token-to-register
       :active (semantic-current-nonterminal)
       :help "Copy the current token to a register"
       ])
    )
   "--"
   (list
    "Analyze"
    (senator-menu-item
     [ "Speebar Class Browser"
       semantic-cb-speedbar-mode
       :active t
       :help "Start speedbar in Class Broswer mode showing inheritance"
       ])
    (senator-menu-item
     [ "Speedbar Analyzer Mode"
       semantic-speedbar-analysis
       :active t
       :help "Start speedbar in Context Analysis/Completion mode."
       ])
    (senator-menu-item
     [ "Context Analysis Dump"
       semantic-analyze-current-context
       :active t
       :help "Show a dump of an analysis of the current local context"
       ])
    (senator-menu-item
     [ "Smart Completion Dump"
       semantic-analyze-possible-completions
       :active t
       :help "Show a dump of the semantic analyzer's guess at possible completions"
       ])
    )
   (list
    "Chart"
    (senator-menu-item
     [ "Chart Tokens by Class"
       semantic-chart-nonterminals-by-token
       :active t
       :help "Catagorize all tokens by type, and chart the volume for each type"
       ])
    (senator-menu-item
     [ "Chart Tokens by Complexity"
       semantic-chart-nonterminal-complexity-token
       :active t
       :help "Choose the most complex tokens, and chart them by complexity"
       ])
    (senator-menu-item
     [ "Chart File Complexity"
       semantic-chart-database-size
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :help "Choose the files with the most tokens, and chart them by volume"
       ])
    )
   (if (or (featurep 'xemacs) (> emacs-major-version 20))
       (list "Modes" :filter 'senator-build-modes-menu)
     ;; The :filter feature seems broken in GNU Emacs versions before
     ;; 21.1.  So dont delay the menu creation.  This also means that
     ;; new registered minor mode entries will not be added "on the
     ;; fly" to the menu :-(
     (cons "Modes" (senator-build-modes-menu)))
   "--"
   (list
    "Imenu Config"
    (list
     "Token Sorting Function"
     (senator-menu-item
      [ "Do not sort"
        (setq semantic-imenu-sort-bucket-function nil)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function nil)
        :help "Do not sort imenu items"
        ])
     (senator-menu-item
      [ "Increasing by name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tokens-by-name-increasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-increasing)
        :help "Sort tokens by name increasing"
        ])
     (senator-menu-item
      [ "Decreasing by name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tokens-by-name-decreasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-decreasing)
        :help "Sort tokens by name decreasing"
        ])
     (senator-menu-item
      [ "Increasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tokens-by-name-increasing-ci)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-increasing-ci)
        :help "Sort tokens by name increasing and case insensitive"
        ])
     (senator-menu-item
      [ "Decreasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tokens-by-name-decreasing-ci)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-decreasing-ci)
        :help "Sort tokens by name decreasing and case insensitive"
        ])
     )
    (senator-menu-item
     [ "Bin tokens by type"
       (setq semantic-imenu-bucketize-file
             (not semantic-imenu-bucketize-file))
       :active t
       :style toggle
       :selected semantic-imenu-bucketize-file
       :help "Organize tokens in bins by type of token"
       ])
    (senator-menu-item
     [ "Bins are submenus"
       (setq semantic-imenu-buckets-to-submenu
             (not semantic-imenu-buckets-to-submenu))
       :active t
       :style toggle
       :selected semantic-imenu-buckets-to-submenu
       :help "Organize tokens into submenus by type of token"
       ])
    (senator-menu-item
     [ "Bin tokens in children"
       (setq semantic-imenu-bucketize-type-parts
             (not semantic-imenu-bucketize-type-parts))
       :active t
       :style toggle
       :selected semantic-imenu-bucketize-type-parts
       :help "When listing tokens inside another token; bin by token type"
       ])
    (senator-menu-item
     [ "List other files"
       (setq semantic-imenu-index-directory (not semantic-imenu-index-directory))
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :style toggle
       :selected semantic-imenu-index-directory
       :help "List all files in the current database in the Imenu menu"
       ])
    (senator-menu-item
     [ "Auto-rebuild other buffers"
       (setq semantic-imenu-auto-rebuild-directory-indexes
             (not semantic-imenu-auto-rebuild-directory-indexes))
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :style toggle
       :selected semantic-imenu-auto-rebuild-directory-indexes
       :help "If listing other buffers, update all buffer menus after a parse"
       ])
    )
   (list
    "Options"
    (senator-menu-item
     ["Semantic..."
      (customize-group "semantic")
      :active t
      :help "Customize Semantic options"
      ])
    (senator-menu-item
     ["Senator..."
      (customize-group "senator")
      :active t
      :help "Customize SEmantic NAvigaTOR options"
      ])
    (senator-menu-item
     ["Semantic Imenu..."
      (customize-group "semantic-imenu")
      :active t
      :help "Customize Semantic Imenu options"
      ])
    (senator-menu-item
     ["Semantic Database..."
      (customize-group "semanticdb")
      :active t
      :help "Customize Semantic Database options"
      ])
    )
   )
  "Menu for senator minor mode.")

(defvar senator-minor-menu nil
  "Menu keymap build from `senator-menu-bar'.")

(defvar senator-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km senator-prefix-key senator-prefix-map)
    (define-key km [(shift mouse-3)] 'senator-completion-menu-popup)
    (easy-menu-define senator-minor-menu km "Senator Minor Mode Menu"
                      senator-menu-bar)
    km)
  "Keymap for senator minor mode.")

(defvar senator-minor-mode nil
  "Non-nil if Senator minor mode is enabled.
Use the command `senator-minor-mode' to change this variable.")
(make-variable-buffer-local 'senator-minor-mode)

(defconst senator-minor-mode-name "n"
  "Name shown in the mode line when senator minor mode is on.
Not displayed if the minor mode is globally enabled.")

(defconst senator-minor-mode-isearch-suffix "i"
  "String appended to the mode name when senator isearch mode is on.")

(defun senator-mode-line-update ()
  "Update the modeline to show the senator minor mode state.
If `senator-isearch-semantic-mode' is non-nil append
`senator-minor-mode-isearch-suffix' to the value of the variable
`senator-minor-mode-name'."
  (if (not (and senator-minor-mode senator-minor-mode-name))
      (setq senator-status "")
    (setq senator-status
          (format "%s%s" senator-minor-mode-name
                  (if senator-isearch-semantic-mode
                      (or senator-minor-mode-isearch-suffix "")
                    ""))))
  (semantic-mode-line-update))

(defun senator-minor-mode-setup ()
  "Actually setup the senator minor mode.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if senator-minor-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq senator-minor-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; XEmacs needs this
        (if (featurep 'xemacs)
            (easy-menu-add senator-minor-menu senator-mode-map))
        ;; Parse the current buffer if needed
        (condition-case nil
            (progn
              (senator-parse)
              ;; Add completion hooks
              (semantic-make-local-hook 'semantic-before-toplevel-cache-flush-hook)
              (add-hook 'semantic-before-toplevel-cache-flush-hook
                        'senator-completion-cache-flush-fcn nil t)
              (semantic-make-local-hook 'semantic-clean-token-hooks)
              (add-hook 'semantic-clean-token-hooks
                        'senator-completion-cache-flush-fcn nil t))
          (quit
           (message "senator-minor-mode: parsing of buffer canceled."))))
    ;; XEmacs needs this
    (if (featurep 'xemacs)
        (easy-menu-remove senator-minor-menu))
    ;; Remove completion hooks
    (remove-hook 'semantic-before-toplevel-cache-flush-hook
                 'senator-completion-cache-flush-fcn)
    (remove-hook 'semantic-clean-token-hooks
                 'senator-completion-cache-flush-fcn)
    ;; Disable semantic isearch
    (setq senator-isearch-semantic-mode nil))
  senator-minor-mode)
  
;;;###autoload
(defun senator-minor-mode (&optional arg)
  "Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if senator-minor-mode 0 1))))
  (setq senator-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not senator-minor-mode)))
  (senator-minor-mode-setup)
  (run-hooks 'senator-minor-mode-hook)
  (if (interactive-p)
      (message "Senator minor mode %sabled"
               (if senator-minor-mode "en" "dis")))
  (senator-mode-line-update)
  senator-minor-mode)

(semantic-add-minor-mode 'senator-minor-mode
                         'senator-status
                         senator-mode-map)

;; To show senator isearch mode in the mode line
(semantic-add-minor-mode 'senator-isearch-semantic-mode
                         'senator-status
                         nil)
;;; Emacs 21 goodies
(and (not (featurep 'xemacs))
     (> emacs-major-version 20)
     (progn

       ;; Add Senator to the the minor mode menu in the mode line
       (define-key mode-line-mode-menu [senator-minor-mode]
         `(menu-item "Senator" senator-minor-mode
                     :button  (:toggle . senator-minor-mode)
                     :visible (and (featurep 'semantic)
                                   (semantic-active-p))))
     
       ))

;;;###autoload
(defun global-senator-minor-mode (&optional arg)
  "Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-senator-minor-mode
        (semantic-toggle-minor-mode-globally
         'senator-minor-mode arg)))

;;;;
;;;; Useful advices
;;;;

(defun senator-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Use semantic tokens to navigate.
ARG is the number of tokens to navigate (not yet implemented)."
  (let* ((senator-highlight-found nil)
         ;; Step at beginning of next token with id specified in
         ;; `senator-step-at-token-ids'.
         (senator-step-at-start-end-token-ids t)
         (token (senator-previous-token)))
    (when token
      (if (= (point) (semantic-token-end token))
          (goto-char (semantic-token-start token)))
      (beginning-of-line))
    (working-message nil)))

(defun senator-end-of-defun (&optional arg)
  "Move forward to next end of defun.
Use semantic tokens to navigate.
ARG is the number of tokens to navigate (not yet implemented)."
  (let* ((senator-highlight-found nil)
         ;; Step at end of next token with id specified in
         ;; `senator-step-at-token-ids'.
         (senator-step-at-start-end-token-ids t)
         (token (senator-next-token)))
    (when token
      (if (= (point) (semantic-token-start token))
          (goto-char (semantic-token-end token)))
      (skip-chars-forward " \t")
      (if (looking-at "\\s<\\|\n")
          (forward-line 1)))
    (working-message nil)))

(defun senator-narrow-to-defun ()
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Use semantic tokens to navigate."
  (interactive)
  (save-excursion
    (widen)
    (senator-end-of-defun)
    (let ((end (point)))
      (senator-beginning-of-defun)
      (narrow-to-region (point) end))))

(defun senator-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
Use semantic tokens to navigate."
  (interactive)
  (let ((origin (point))
        (end    (progn (senator-end-of-defun) (point)))
        (start  (progn (senator-beginning-of-defun) (point))))
    (goto-char origin)
    (push-mark (point))
    (goto-char end) ;; end-of-defun
    (push-mark (point) nil t)
    (goto-char start) ;; beginning-of-defun
    (re-search-backward "^\n" (- (point) 1) t)))

(defadvice beginning-of-defun (around senator activate)
  "Move backward to the beginning of a defun.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-beginning-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-defun (around senator activate)
  "Move forward to next end of defun.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-end-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice narrow-to-defun (around senator activate)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-narrow-to-defun)
    ad-do-it))

(defadvice mark-defun (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-mark-defun)
    ad-do-it))

(defadvice c-mark-function (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-mark-defun)
    ad-do-it))

(defadvice add-log-current-defun (around senator activate)
  "Return name of function definition point is in, or nil."
  (if senator-minor-mode
      (let ((cd (semantic-current-nonterminal)))
      (if (member (semantic-token-token cd) '(function variable type))
          (setq ad-return-value (semantic-token-name cd))
        ad-do-it))
    ad-do-it))

;;;;
;;;; Token Cut & Paste
;;;;

;; To copy a token, means to put a token definition into the token
;; ring.  To kill a token, put the token into the token ring AND put
;; the body of the token into the kill-ring.
;;
;; To retrieve a killed token's text, use C-y (yank), but to retrieve
;; the token as a reference of some sort, use senator-yank-token.

(defvar senator-token-ring (make-ring 20)
  "Ring of tokens for use with cut and paste.")

(defun senator-insert-foreign-token-default (token tokenfile)
  "Insert TOKEN from a foreign buffer into the current buffer.
This is the default behavior for `senator-insert-foreign-token'.
Assumes the current buffer is a language file, and attempts to insert
a prototype/function call.
Argument TOKENFILE is the file from wence TOKEN came."
  ;; Long term goal:
  ;; Have a mechanism for a tempo-like template insert for the given
  ;; token.
  (insert (semantic-prototype-nonterminal token)))

(defun senator-insert-foreign-token (token tokenfile)
  "Insert TOKEN from a foreign buffer into the current buffer.
TOKEN will have originated from TOKENFILE.
This function is overridable with the symbol `insert-foreign-token'."
  (if (or (not token) (not (semantic-token-p token)))
      (signal 'wrong-type-argument (list token 'semantic-token-p)))
  (let ((s (semantic-fetch-overload 'insert-foreign-token)))
    (if s (funcall s token tokenfile)
      (senator-insert-foreign-token-default token tokenfile))
    (message (semantic-summarize-nonterminal token))))

(defun senator-copy-token ()
  "Take the current token, and place it in the token ring."
  (interactive)
  (senator-parse)
  (let ((ct (senator-current-token)))
    (ring-insert senator-token-ring (cons ct (buffer-file-name)))
    (message (semantic-summarize-nonterminal ct))
    ct))

(defun senator-kill-token ()
  "Take the current token, place it in the token ring, and kill it.
Killing the token removes the text for that token, and places it into
the kill ring.  Retrieve that text with \\[yank\\]."
  (interactive)
  (let ((ct (senator-copy-token))) ;; this handles the reparse for us.
    (kill-region (semantic-token-start ct)
                 (semantic-token-end ct))))

(defun senator-yank-token ()
  "Yank a token from the token ring.
The form the token takes is differnet depending on where it is being
yanked to."
  (interactive)
  (or (ring-empty-p senator-token-ring)
      (let ((tok (ring-ref senator-token-ring 0)))
        (senator-insert-foreign-token (car tok) (cdr tok)))))

(defun senator-copy-token-to-register (register &optional kill-flag)
  "Copy the current token into REGISTER.
Optional argument KILL-FLAG will delete the text of the token to the
kill ring."
  (interactive "cToken to register: \nP")
  (let ((ct (senator-current-token)))
    (set-register register (cons ct (buffer-file-name)))
    (if kill-flag
        (kill-region (semantic-token-start ct)
                     (semantic-token-end ct)))))

(defadvice insert-register (around senator activate)
  "Insert contents of register REGISTER as a token.
If senator is not active, use the original mechanism."
  (let ((val (get-register (ad-get-arg 0))))
    (if (and senator-minor-mode (interactive-p)
             (listp val) (semantic-token-p (car val)))
        (senator-insert-foreign-token (car val) (cdr val))
      ad-do-it)))

(defadvice jump-to-register (around senator activate)
  "Insert contents of register REGISTER as a token.
If senator is not active, use the original mechanism."
  (let ((val (get-register (ad-get-arg 0))))
    (if (and senator-minor-mode (interactive-p)
             (listp val) (semantic-token-p (car val)))
        (progn
          (find-file (cdr val))
          (goto-char (semantic-token-start (car val))))
      ad-do-it)))

;;;;
;;;; Summary mode
;;;;

;; Since eldoc kicks butt for Emacs Lisp mode, this advice will let
;; us do eldoc like things for other languages.
(eval-when-compile (require 'eldoc)
                   (require 'semantic-ctxt))

(defcustom senator-eldoc-use-color (or (featurep 'xemacs)
                                       (>= emacs-major-version 21))
  "*Use color for eldoc strings generated with semantic.
Colored text can be printed with the message command with some
versions of Emacs."
  :group 'senator
  :type 'boolean)

(defun senator-eldoc-print-current-symbol-info-default ()
  "Return a string message describing the current context."
  (let ((sym (semantic-ctxt-current-symbol))
        found)
    (if sym
        (progn
          (setq found (if (and (featurep 'semanticdb)
                               semanticdb-current-database)
                          (cdr
                           (car (semanticdb-find-nonterminal-by-name
                                 (car sym) nil t)))
                        (semantic-find-nonterminal-by-name
                         (car sym) (current-buffer) t)))
          (and (not found)
               (semantic-flex-keyword-p (car sym))
               (setq found (semantic-flex-keyword-get (car sym) 'summary)))
          ))
    (or found
        (progn
          (setq sym (semantic-ctxt-current-function))
          (if sym
              (progn
                (setq found (if (and (featurep 'semanticdb)
                                     semanticdb-current-database)
                                (cdr
                                 (car (semanticdb-find-nonterminal-by-name
                                       (car sym) nil t)))
                              (semantic-find-nonterminal-by-name
                               (car sym) (current-buffer) t)))
                (and (not found)
                     (semantic-flex-keyword-p (car sym))
                     (setq found (semantic-flex-keyword-get (car sym) 'summary)))
                ))
          ))
    found))

(defun senator-eldoc-print-current-symbol-info ()
  "Print information using `eldoc-message' while in function `eldoc-mode'.
You can override the info collecting part with `eldoc-current-symbol-info'."
  (let* ((s (semantic-fetch-overload 'eldoc-current-symbol-info))
         found)

    (if s (setq found (funcall s))
      (setq found (senator-eldoc-print-current-symbol-info-default)))

    (eldoc-message
     (cond ((stringp found)
            found)
           ((semantic-token-p found)
            (semantic-summarize-nonterminal found nil senator-eldoc-use-color))
           (t nil)
           ))))

(defadvice eldoc-print-current-symbol-info (around senator activate)
  "Enable ELDOC in non Emacs Lisp, but semantic-enabled modes."
  (if (eq major-mode 'emacs-lisp-mode)
      ad-do-it
    (if (semantic-active-p)
        (if (eldoc-display-message-p)
            (senator-eldoc-print-current-symbol-info))
      (eldoc-mode -1))))

;;; HIPPIE EXPAND
;;
;; Senator has a nice completion mechanism.  Use it to add a new
;; hippie expand try method.

(eval-when-compile (require 'hippie-exp))

(defvar senator-try-function-already-enabled nil
  "non-nil if `hippie-expand' semantic completion was already enabled.
This flag remember `senator-hippie-expand-hook' to not remove
`senator-try-expand-semantic' from `hippie-expand-try-functions-list'
if it was previously put here by any sort of user's customization.")

(defun senator-hippie-expand-hook ()
  "Enable or disable use of semantic completion with `hippie-expand'.
Depending on the value of the variable `senator-minor-mode'.  Run as
`senator-minor-mode-hook'."
  (make-local-variable 'hippie-expand-try-functions-list)
  (make-local-variable 'senator-try-function-already-enabled)
  (if senator-minor-mode
      (progn
        ;; Does nothing if semantic completion is already enabled (via
        ;; customization for example).
        (setq senator-try-function-already-enabled
              (memq 'senator-try-expand-semantic
                    hippie-expand-try-functions-list))
        (or senator-try-function-already-enabled
            (setq hippie-expand-try-functions-list
                  (cons 'senator-try-expand-semantic
                        hippie-expand-try-functions-list))))
    ;; Does nothing if semantic completion wasn't enabled here.
    (or senator-try-function-already-enabled
        (setq hippie-expand-try-functions-list
              (delq 'senator-try-expand-semantic
                    hippie-expand-try-functions-list)))))

(add-hook 'senator-minor-mode-hook 'senator-hippie-expand-hook)

;;;###autoload
(defun senator-try-expand-semantic (old)
  "Attempt inline completion at the cursor.
Use Semantic, or the semantic database to look up possible
completions.  The argument OLD has to be nil the first call of this
function.  It returns t if a unique, possibly partial, completion is
found, nil otherwise."
  (if (semantic-active-p)
      (let (symstart)
        ;; If the hippie says so, start over.
        (if (not old)
            (if (setq symstart (senator-current-symbol-start))
                (progn
                  (he-init-string symstart (point))
                  (setq senator-last-completion-stats nil))))
        ;; do completion with senator's mechanism.
        (if (or old symstart)
            (let ((ret (senator-complete-symbol t)))
              (cond (ret
                     ;; Found a new completion, update the end marker.
                     (set-marker he-string-end (point))
                     ;; Update the tried table so other hippie expand
                     ;; try functions can see whether an expansion has
                     ;; already been tried.
                     (setq he-tried-table (cons ret he-tried-table)))
                    ;; No more completion
                    (old
                     ;; Reset the initial completed string for other
                     ;; hippie-expand try functions.
                     (he-reset-string)))
              ret)))))

;;;;
;;;; Using semantic search in isearch mode
;;;;

;;; Compatibility
(cond
 (;; GNU Emacs 21.0 lazy highlighting
  (fboundp 'isearch-lazy-highlight-cleanup)

  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'isearch-lazy-highlight-cleanup t)
    (set 'isearch-lazy-highlight-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))

  ) ;; End of GNU Emacs 21 lazy highlighting

 (;; XEmacs 21.4 lazy highlighting
  (fboundp 'isearch-highlight-all-cleanup)
       
  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'isearch-highlight-all-cleanup)
    (set 'isearch-highlight-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))

  ) ;; End of XEmacs 21.4 lazy highlighting

 (;; GNU Emacs 20 lazy highlighting via ishl
  (fboundp 'ishl-cleanup)
       
  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'ishl-cleanup t)
    (set 'ishl-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))

  ) ;; End of GNU Emacs 20 lazy highlighting

 (t ;; No lazy highlighting

  ;; Ignore this function used by senator
  (defalias 'senator-lazy-highlight-update 'ignore)

  )
      
 ) ;; End of compatibility stuff

(defmacro senator-define-search-advice (searcher)
  "Advice the built-in SEARCHER function to do semantic search.
That is to call the Senator counterpart searcher when variables
`isearch-mode' and `senator-isearch-semantic-mode' are non-nil."
  (let ((senator-searcher (intern (format "senator-%s" searcher))))
    `(defadvice ,searcher (around senator activate)
       (if (and isearch-mode senator-isearch-semantic-mode
                ;; The following condition ensure to do a senator
                ;; semantic search on the `isearch-string' only!
                (string-equal (ad-get-arg 0) isearch-string))
           (unwind-protect
               (progn
                 ;; Temporarily set `senator-isearch-semantic-mode' to
                 ;; nil to avoid an infinite recursive call of the
                 ;; senator semantic search function!
                 (setq senator-isearch-semantic-mode nil)
                 (setq ad-return-value
                       (funcall ',senator-searcher
                                (ad-get-arg 0) ; string
                                (ad-get-arg 1) ; bound
                                (ad-get-arg 2) ; no-error
                                (ad-get-arg 3) ; count
                                )))
             (setq senator-isearch-semantic-mode t))
         ad-do-it))))

;; Advice the built-in search functions to do semantic search when
;; `isearch-mode' and `senator-isearch-semantic-mode' are on.
(senator-define-search-advice search-forward)
(senator-define-search-advice re-search-forward)
(senator-define-search-advice word-search-forward)
(senator-define-search-advice search-backward)
(senator-define-search-advice re-search-backward)
(senator-define-search-advice word-search-backward)

(defun senator-isearch-toggle-semantic-mode ()
  "Toggle semantic searching on or off in isearch mode.
\\[senator-isearch-toggle-semantic-mode] toggle semantic searching."
  (interactive)
  (when senator-minor-mode
    (setq senator-isearch-semantic-mode
          (not senator-isearch-semantic-mode))
    (senator-mode-line-update)
    (if isearch-mode
        ;; force lazy highlight update
        (senator-lazy-highlight-update)
      (working-message "Isearch semantic mode %s"
                       (if senator-isearch-semantic-mode
                           "enabled"
                         "disabled")))))

;; Needed by XEmacs isearch to not terminate isearch mode when
;; toggling semantic search.
(put 'senator-isearch-toggle-semantic-mode 'isearch-command t)

;; Keyboard shortcut to toggle semantic search in isearch mode.
(define-key isearch-mode-map
  [(control ?,)]
  'senator-isearch-toggle-semantic-mode)

(defun senator-isearch-mode-hook ()
  "Isearch mode hook to setup semantic searching."
  (or senator-minor-mode
      (setq senator-isearch-semantic-mode nil))
  (senator-mode-line-update))

(add-hook 'isearch-mode-hook 'senator-isearch-mode-hook)

(provide 'senator)

;;; senator.el ends here
