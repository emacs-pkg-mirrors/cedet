;;; senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000, 2001 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 10 Nov 2000
;; Keywords: syntax
;; X-RCS: $Id: senator.el,v 1.26 2001/03/05 08:02:27 ponced Exp $

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
;;    C-c , SPC       `senator-completion-menu-keyboard-popup'
;;    S-mouse-3       `senator-completion-menu-mouse-popup'
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

;; $Log: senator.el,v $
;; Revision 1.26  2001/03/05 08:02:27  ponced
;; Added new Eldoc suggestion mode feature.
;;
;; Revision 1.25  2001/02/23 16:06:56  ponced
;; Copied over all the menu items that used to be in semanic-mode.
;; Added the new token cut/paste items into a menu.
;; Added menu items to configure semantic-imenu and enable semantic-db.
;; Added Options sub menu to customize Semantic, Senator, Semantic Imenu
;; and Semantic Database.
;; Fixed some XEmacs compatibility issues.
;; Improved robustness of some functions.
;;
;; Revision 1.24  2001/02/22 21:43:17  ponced
;; Added token Copy, Cut & Paste and Register feature.
;;
;; Improvement to the senator-jump completion stuff:
;;
;; o The completion list is now cached (buffer local) and reused when
;;   possible.  The cache is refreshed when parsing or partial parsing
;;   occurs.
;;
;; o Greatly simplified the way completion name are handled.  Now only
;;   duplicated names are suffixed by parent names :-)
;;
;; Some minor `checkdoc' fixes.
;;
;; Revision 1.23  2001/02/21 11:27:59  ponced
;; `senator-minor-mode-setup' now uses the `semantic-active-p' function
;; to check if the current buffer is set up for parsing.
;;
;; `senator-full-token-name' now append the parent names in reverse order
;; to the token name.  Thus the `senator-jump' completion list is now
;; easy to use.  You can just type a partial token name and press ENTER
;; to jump to it.  If the name is not unique a completion buffer is
;; displayed allowing to choose the right one.
;;
;; `senator-beginning-of-defun' and `senator-end-of-defun' now
;; respectively step at start or end of tokens with identifier specified
;; in `senator-step-at-token-ids'.  Thus, adviced `beginning-of-defun'
;; and `end-of-defun' now work with any tokens where you can step at.
;;
;; `senator-step-at-start-end-token-ids' can be set to `t' to step at
;; start and end of any token where it is allowed to step.
;;
;; Revision 1.22  2001/02/09 11:55:01  ponced
;; New implementation of completion menu to allow customization of menu
;; item text and insert function.
;;
;; Revision 1.21  2001/02/05 14:46:00  ponced
;; Use new `semantic-type-relation-separator-character' format.
;; New completion popup menu.
;;
;; Revision 1.20  2001/02/01 23:27:43  ponced
;; `senator-last-name' removed.
;; `senator-jump-completion-list' new variable.
;; `senator-jump' simpler and more robust!  Save the current
;; completion table in `senator-jump-completion-list', then reuse it
;; to retrieve the token association.  Also, this solves jump
;; conflicts when the same token name is present in different token
;; children.
;; `senator-completion-stream' Store the full token itself in the
;; value part of each association.
;;
;; Revision 1.19  2001/02/01 19:38:53  zappo
;; `senator-complete-symbol' now checks that it complete the same symbol
;; to reuse `senator-last-completion-stat'.  This is needed when for
;; example completing "set..." and then completing "get..." at the same
;; point.
;;
;; Revision 1.18  2001/02/01 02:25:58  zappo
;; Removed compatibility code.
;; Added isearch code, which was in `senator-isearch'.  Made that shorter.
;; `senator-step-at-start-end-p' fixed up.
;; Use `semantic-nonterminal-children' instead of querying for type and
;; then getting the parts.
;; Use semantic's separator character.
;; Added senator-find-nonterminal- .. functions.  Uses semanticdb if loaded.
;; When completing, use `senator-find...'.
;; Also do a little more to track if we are doing the same completion.
;;
;; Revision 1.18  2001/01/31 00:54:06  zappo
;; senator-isearch was merged in.  Isearch fns are now much simpler.
;; Updated documentation.
;; Added xemacs `isearch-update-ring' compatibility function.
;; Use `semantic-nonterminal-children' instead of checking for 'type.
;; Fixed up `senator-complete-symbol'.
;;
;; Revision 1.16  2001/01/10 08:09:16  david_ponce
;; Fixed undeclared local variable 'slot' in `senator-menu-item'.
;;
;; Revision 1.15  2001/01/03 16:09:28  david_ponce
;; New version 2.1.
;;
;; Fixed a nasty typo in `senator-minor-mode'.  `run-hooks' were missing
;; a quotation before their arguments.  Thanks to "Charles Rich"
;; <rich@merl.com> who has reported this bug.
;;
;; Added new `senator-jump' command.  Thanks to "Eric M. Ludlam"
;; <zappo@ultranet.com> and "T. V. Raman" <tvraman@almaden.ibm.com> for
;; their help.
;;
;; Added new `senator-minor-mode-name' option to customize the name
;; displayed in the modeline when senator minor mode is on.
;;
;; When semantic isearch mode is on it now appends "/si" to the senator
;; minor mode name displayed in the modeline.
;;
;; Added new keyboard shortcut "\C-," to toggle semantic search in
;; isearch mode.
;;
;; Some code improvements.
;;
;; Revision 1.14  2000/12/12 11:02:16  david_ponce
;; `senator-mark-defun' now work on XEmacs too.
;;
;; Revision 1.13  2000/12/12 09:21:43  david_ponce
;; Fixed a "side effect" bug with latest `beginning-of-defun' and
;; `end-of-defun' advices.  They caused font-lock, which uses
;; beginning/end of defun to force a reparse.  Thanks to "Eric M. Ludlam"
;; <zappo@ultranet.com> for pointing this.
;;
;; Improved consistency with standard behaviour of `beginning-of-defun'
;; (go to the beginning of the line where the defun starts) and
;; `end-of-defun' (go to the beginning of the line following the end of
;; the defun).
;;
;; Added useful advices for `narrow-to-defun', `mark-defun' and
;; `c-mark-function'.
;;
;; Advices are enabled when the functions are called interactively and
;; `senator-minor-mode' is enabled.
;;
;; Revision 1.12  2000/12/11 14:05:06  david_ponce
;; Code cleanup and optimization.
;; `senator-next-token' and `senator-previous-token' now correctly work
;; when not binded to keyboard shortcuts.
;; `beginning-of-defun' and `end-of-defun' advices no more need to be
;; called interactively.  So `narrow-to-defun' can use these advices when
;; `senator-minor-mode' is enabled.
;;
;; Revision 1.11  2000/12/11 07:07:09  david_ponce
;; Applied Eric Ludlam's patch.  It adds a special face for senator to
;; use for momentary highlight (requires latest semantic-util.el).  If
;; the user cancels the parse (C-g) when `senator-minor-mode'
;; initializes, it doesn't kill the rest of the configuration.  (Useful
;; on a slow machine.)
;;
;; Revision 1.10  2000/12/08 16:18:32  david_ponce
;; A bunch of XEmacs compatibility code!
;;
;; Revision 1.9  2000/12/07 09:20:21  david_ponce
;; Applied Eric Ludlam's doc fix patch.
;;
;; Revision 1.8  2000/12/05 15:59:07  david_ponce
;; Improved consistency with built-in search commands.
;; New search commands like the ones in the Emacs Search menu.
;; Added a menu to senator minor mode.
;; The common prefix key in senator minor mode is now "C-c ,".
;; Updated header comments.
;; Some code cleanup.
;;
;; Revision 1.7  2000/12/05 11:13:19  david_ponce
;; New major version 2.0 with [i]search feature and a senator minor mode.
;; Added compatibility code between GNU Emacs 20.7 and 21.
;;
;; Revision 1.6  2000/11/28 12:44:47  david_ponce
;; More performance improvements.
;; New option `senator-highlight-found' to customize token highlighting.
;;
;; Revision 1.5  2000/11/27 13:24:17  david_ponce
;; Fixed a serious performance problem in `senator-next-token' and
;; `senator-previous-token'.
;;
;; Before searching for a next or previous token the point was just moved
;; to respectively the next or previous character. Thus, during
;; navigation, the buffer was explored character by character :-(.  Now
;; `senator-next-token' and `senator-previous-token' skip whole tokens
;; (unless they are 'type tokens which may include sub tokens).
;;
;; Revision 1.4  2000/11/14 17:23:21  david_ponce
;; Minor change to `senator-next-token' and `senator-previous-token' to
;; return the token at point.  Useful when calling these commands
;; non-interactively.
;;
;; Revision 1.3  2000/11/14 13:04:26  david_ponce
;; Improved navigation in semantic token where to step at start and end.
;;
;; - `senator-next-token' move the point to the end of token if it was at
;;   beginning or in the middle of the token.
;;
;; - `senator-previous-token' move the point to the beginning of token if
;;   it was at end or in the middle of the token.
;;
;; Revision 1.2  2000/11/10 17:11:15  david_ponce
;; Fixed a little bug in `senator-previous-token' navigation.
;;
;; Revision 1.1  2000/11/10 16:04:20  david_ponce
;; Initial revision.
;;

;;; Code:
(require 'semantic)
(require 'semantic-ctxt)

;;; Customization
(defgroup senator nil
  "SEmantic NAvigaTOR."
  :group 'semantic)

(defcustom senator-minor-mode-name "Senator"
  "*Name displayed in the modeline when senator minor mode is on."
  :group 'senator
  :type 'string)

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
  "Face used to momentarilly highlight tokens."
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

;;; Compatibility
(if (featurep 'xemacs)
    
    ;; Provide `isearch-update-ring' function (from 21.1.9 isearch-mode.el)
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

(defun senator-message (&rest args)
  "Call function `message' with ARGS without logging."
  (let (message-log-max)
    (apply 'message args)))

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

(defun senator-find-token-before (tokens pos comp &optional prev)
  "Visit TOKENS and return the last one found at or before POS.
COMP is the function used to compare a current visited token start
position to POS.  That is `>=' to return the token before POS or `>'
to return the token at or before POS.  PREV is the last token found or
nil."
  (let (token children)
    (while tokens
      (setq token (car tokens))
      (if (funcall comp (semantic-token-start token) pos)
          (throw 'found prev))
      (or (senator-skip-p token)
          (setq prev token))
      (if (setq children (semantic-nonterminal-children token t))
          (setq prev (senator-find-token-before children pos comp prev)))
      (setq tokens (cdr tokens)))
    prev))

(defun senator-find-previous-token (tokens pos)
  "Visit TOKENS and return the token before POS."
  (catch 'found (senator-find-token-before tokens pos #'>=)))

(defun senator-find-last-token (tokens pos)
  "Visit TOKENS and return the token at or before POS."
  (catch 'found (senator-find-token-before tokens pos #'>)))

(defun senator-find-next-token (tokens pos)
  "Visit TOKENS and return the token after POS."
  (let (token children found)
    (while (and tokens (not found))
      (setq token (car tokens))
      (if (and (not (senator-skip-p token))
               (or (and (senator-step-at-start-end-p token)
                        (> (semantic-token-end token) pos))
                   (> (semantic-token-start token) pos)))
          (setq found token)
        (if (setq children (semantic-nonterminal-children token t))
            (setq found (senator-find-next-token children pos))))
      (setq tokens (cdr tokens)))
    found))

(defun senator-middle-of-token-p (pos token)
  "Return non-nil if POS is between start and end of TOKEN."
  (and (> pos (semantic-token-start token))
       (< pos (semantic-token-end   token))))

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
           (setq children (semantic-nonterminal-children token t))
           (setq fs (append fs (senator-completion-flatten-stream
                                children e)))))
    fs))

(defun senator-completion-stream (stream &optional top-level)
  "Return a useful completion list from tokens in STREAM.
That is an alist of all (COMPLETION-NAME . TOKEN) available.
COMPLETION-NAME is the token name.  If it is duplicated, parent token
names, separated by `semantic-type-relation-separator-character', are
appended in reverse order to the token name.  This helps to
distinguish between children tokens with the same name.  The value part
of each association is the full token itself.  If TOP-LEVEL is non-nil
the completion list will contain only tokens at top level.  Otherwise
all sub tokens are included too."
  (let ((fs (senator-completion-flatten-stream stream nil top-level))
        token parents saw e clst)
    (while fs
      (setq token   (car (car fs))
            parents (cdr (car fs))
            name    (semantic-token-name token)
            e       (assoc name saw)
            fs      (cdr fs))
      (if e
          ;; This is a duplicated completion name.
          (progn
            ;; Append parent names to the token name.
            (setq clst (cons (cons (senator-full-token-name token parents)
                                   token)
                             clst))
            ;; Don't forget to append parent names to the completion
            ;; name of the first token found with this name!
            (when (cdr e)
              (setq parents (cdr (cdr e))
                    token   (cdr (car (cdr e))))
              (setcar (car (cdr e))
                      (senator-full-token-name token parents))
              ;; Don't update again this completion name.
              (setcdr e nil))
            )
        ;; This is a new completion name.
        (setq e    (cons name token)
              clst (cons e clst)
              ;; Record the token name, completion element and parents
              ;; in an alist.
              saw  (cons (cons name (cons e parents)) saw))))
    clst))

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
      ;; semanticdb version returns a list..
      (car (semanticdb-find-nonterminal-by-name name nil t))
    (semantic-find-nonterminal-by-name name (current-buffer) t)))

(defun senator-find-nonterminal-by-name-regexp (regexp)
  "Find all tokens with a name matching REGEXP.
Uses `semanticdb' when available."
  (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
      ;; semanticdb version returns a list..
      (car (semanticdb-find-nonterminal-by-name-regexp regexp nil t))
    (semantic-find-nonterminal-by-name-regexp regexp (current-buffer) t)))

;;;;
;;;; Search functions
;;;;

(defun senator-search-token-name (token)
  "Search the TOKEN name in TOKEN bounds.
Set point to the end of the name, and return point.  To get the
beginning of the name use (match-beginning 0)."
  (let ((name (semantic-token-name token)))
    (goto-char (semantic-token-start token))
    (re-search-forward (concat
                        ;; the token name is at the beginning of a
                        ;; word or after a whitespace or a punctuation
                        "\\(\\<\\|\\s-\\|\\s.\\)"
                        (regexp-quote
                         (if (string-match "\\`\\([^[]+\\)[[]" name)
                             (match-string 1 name)
                           name)))
                       (semantic-token-end token))))

(defun senator-search-forward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in semantic tokens after point.
See `search-forward' for the meaning of BOUND NOERROR and COUNT.
COUNT is just ignored in the current implementation."
  (let ((origin (point))
        (tokens (senator-parse))
        (senator-step-at-start-end-token-ids nil)
        token pos start limit)
    (save-excursion
      (setq token (or (senator-find-last-token tokens origin)
                      (senator-find-next-token tokens origin)))
      (while (and token (not pos))
        (setq limit (senator-search-token-name token))
        (if bound
            (setq limit (min bound limit)))
        (setq start (match-beginning 0))
        (if (and (> origin start) (< origin limit))
            (setq start origin))
        (if (and bound (> start limit))
            (setq token nil)
          (goto-char start)
          (setq pos (funcall searcher what limit t))
          (if (and pos (>= (match-beginning 0) origin))
              nil
            (setq pos nil)
            (setq token (senator-find-next-token tokens (point)))))))
    (if pos
        (goto-char start)
      (setq limit (point)))
    (funcall searcher what limit noerror)))

(defun senator-search-backward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in semantic tokens before point.
See `search-backward' for the meaning of BOUND NOERROR and COUNT.
COUNT is just ignored in the current implementation."
  (let ((origin (point))
        (tokens (senator-parse))
        (senator-step-at-start-end-token-ids nil)
        token pos start limit)
    (save-excursion
      (setq token (senator-find-previous-token tokens origin))
      (while (and token (not pos))
        (setq start (senator-search-token-name token))
        (setq limit (match-beginning 0))
        (if bound
            (setq limit (max bound limit)))
        (if (and (< origin start) (> origin limit))
            (setq start origin))
        (if (and bound (< start limit))
            (setq token nil)
          (goto-char start)
          (setq pos (funcall searcher what limit t))
          (if (and pos (<= (match-end 0) origin))
              nil
            (setq pos nil)
            (goto-char (semantic-token-start token))
            (setq token (senator-find-previous-token tokens (point)))))))
    (if pos
        (goto-char start)
      (setq limit (point)))
    (funcall searcher what limit noerror)))

;;;;
;;;; Navigation commands
;;;;

;;;###autoload
(defun senator-next-token ()
  "Navigate to the next semantic token.
Return the semantic token or nil if at end of buffer."
  (interactive)
  (let ((pos    (point))
        (tokens (senator-parse))
        found where)
    (setq found (senator-find-next-token tokens (point)))
    (if (not found)
        (progn
          (goto-char (point-max))
          (senator-message "End of buffer"))
      (cond ((and (senator-step-at-start-end-p found)
                  (or (= pos (semantic-token-start found))
                      (senator-middle-of-token-p pos found)))
             (setq where "end")
             (goto-char (semantic-token-end found)))
            (t
             (setq where "start")
             (goto-char (semantic-token-start found))))
      (senator-momentary-highlight-token found)
      (senator-message "%S: %s (%s)"
                       (semantic-token-token found)
                       (semantic-token-name  found)
                       where))
    found))

;;;###autoload
(defun senator-previous-token ()
  "Navigate to the previous semantic token.
Return the semantic token or nil if at beginning of buffer."
  (interactive)
  (let ((pos    (point))
        (tokens (senator-parse))
        found where)
    (setq found (senator-find-previous-token tokens (point)))
    (if (not found)
        (progn
          (goto-char (point-min))
          (senator-message "Beginning of buffer"))
      (cond ((or (not (senator-step-at-start-end-p found))
                 (= pos (semantic-token-end found))
                 (senator-middle-of-token-p pos found))
             (setq where "start")
             (goto-char (semantic-token-start found)))
            (t
             (setq where "end")
             (goto-char (semantic-token-end found))))
      (senator-momentary-highlight-token found)
      (senator-message "%S: %s (%s)"
                       (semantic-token-token found)
                       (semantic-token-name  found)
                       where))
    found))

(defvar senator-jump-completion-list nil
  "`senator-jump' stores here its current completion list.
Then use `assoc' to retrieve the token associated to a symbol.")

;;;###autoload
(defun senator-jump (sym)
  "Jump to the semantic symbol SYM.
If called interactively and a prefix argument is supplied jump in the
local type's context (see function `senator-current-type-context')."
  (interactive
   (list
    (completing-read "Jump to: "
                     (setq senator-jump-completion-list
                           (senator-completion-list current-prefix-arg))
                     nil
                     t
                     ""
                     'semantic-read-symbol-history)))
  (let ((token (cdr (assoc sym senator-jump-completion-list))))
    (when token
      (goto-char (semantic-token-start token))
      (senator-momentary-highlight-token token)
      (senator-message "%S: %s "
                       (semantic-token-token token)
                       (semantic-token-name  token)))))

(defvar senator-last-completion-stats nil
  "The last senator completion was here.
Of the form (BUFFER STARTPOS INDEX REGEX COMPLIST...)")

;;;###autoload
(defun senator-complete-symbol ()
  "Complete the current symbol under point."
  (interactive)
  (let* ((symstart (save-excursion (forward-sexp -1) (point)))
         regex complst newstr index)
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
            complst (senator-find-nonterminal-by-name-regexp regex)
            senator-last-completion-stats (append (list (current-buffer)
                                                        symstart
                                                        0
                                                        regex)
                                                  complst)))
    ;; Do the completion if apropriate.
    (when complst
      ;; get the new string
      (setq index (nth 2 senator-last-completion-stats)
            newstr (nth index complst))
      ;; Update the index
      (if (< index (1- (length complst)))
          (setcar (nthcdr 2 senator-last-completion-stats) (1+ index))
        (setcar (nthcdr 2 senator-last-completion-stats) 0))
      ;; Replace the string
      (delete-region symstart (point))
      (insert (semantic-token-name newstr)))))

;;;;
;;;; Completion menu
;;;;

(defcustom senator-completion-menu-summary-function
  'semantic-concise-prototype-nonterminal
  "*Function to use when creating items in completion menu.
Some useful functions are:
`semantic-concise-prototype-nonterminal'
`semantic-abbreviate-nonterminal'
`semantic-summarize-nonterminal'
`semantic-prototype-nonterminal'"
  :group 'senator
  :type 'function)
(make-variable-buffer-local 'senator-completion-menu-summary-function)

(defcustom senator-completion-menu-insert-function
  'senator-completion-menu-insert-default
  "*Function to use to insert an item from completion menu.
It will receive a Semantic token as argument."
  :group 'senator
  :type 'function)
(make-variable-buffer-local 'senator-completion-menu-insert-function)

(defun senator-completion-menu-insert-default (token)
  "Insert a text representation of TOKEN at point."
  (insert (semantic-token-name token)))

(defun senator-completion-menu-do-complete (token-overlay)
  "Replace the current syntactic expression with a chosen completion.
Argument TOKEN-OVERLAY is the overlay of the token chosen from the
completion menu."
  (let ((token (semantic-overlay-get token-overlay 'semantic))
        (symstart (save-excursion (forward-sexp -1) (point)))
        (finsert (if (fboundp senator-completion-menu-insert-function)
                     senator-completion-menu-insert-function
                   #'senator-completion-menu-insert-default)))
      (delete-region symstart (point))
      (funcall finsert token)))

(defun senator-completion-menu-item (token)
  "Return a completion menu item from TOKEN.
That is a pair (MENU-ITEM-TEXT . TOKEN-OVERLAY).  Can return nil to
discard a menu item."
  (cons (funcall (if (fboundp senator-completion-menu-summary-function)
                     senator-completion-menu-summary-function
                   #'semantic-prototype-nonterminal) token)
        (semantic-token-overlay token)))

(defun senator-completion-menu-popup (event)
  "Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu.  EVENT is the
parametrized event that invoked this command."
  (let ((symstart (condition-case nil
                      (save-excursion (forward-sexp -1) (point))
                    (error nil)))
        symbol regexp complst)
    (if symstart
        (setq symbol  (buffer-substring-no-properties symstart (point))
              regexp  (concat "^" (regexp-quote symbol))
              complst (senator-find-nonterminal-by-name-regexp regexp)))
    (if (not complst)
        (error "No completions available"))
    ;; We have a completion list, build a menu
    (let* ((index (delq nil
                        (mapcar #'senator-completion-menu-item
                                complst)))
           (item  (if (cdr index)
                      ;; Delegates menu handling to imenu :-)
                      (imenu--mouse-menu
                       index
                       event ;; popup at mouse position
                       (format "%S completion" symbol))
                    ;; Only one item match, return it
                    (car index))))
      (if item
          (senator-completion-menu-do-complete (cdr item))))))

;;;###autoload
(defun senator-completion-menu-mouse-popup (event)
  "Popup a completion menu for the symbol at point.
EVENT is the parametrized event that invoked this command."
  (interactive "e")
  (senator-completion-menu-popup event))

;;;###autoload
(defun senator-completion-menu-keyboard-popup ()
  "Popup a completion menu for the symbol at point."
  (interactive)
  (let ((event (if (featurep 'xemacs)
                   ;; In XEmacs must build an event to popup the menu
                   ;; at mouse position
                   (let* ((edge (window-pixel-edges))
                          (left (car edge))
                          (top  (car (cdr edge)))
                          (at   (cdr (mouse-pixel-position)))
                          (x    (+ (or (car at) 0) left))
                          (y    (+ (or (cdr at) 0) top)))
                     (make-event 'button-press
                                 (list 'channel (selected-frame)
                                       'button 3
                                       'x x
                                       'y y)))
                 ;; In Emacs just use t to popup the menu at mouse
                 ;; position
                 t)))
    (senator-completion-menu-popup event)))

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
                        ;; FSF Emacs already append ": "
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
;;;; Senator minor mode
;;;;

(defvar senator-mode nil
  "Name of the minor mode, if non-nil.")
(make-variable-buffer-local 'senator-mode)

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
    (define-key km " " 'senator-completion-menu-keyboard-popup)
    (define-key km "\C-w" 'senator-kill-token)
    (define-key km "\M-w" 'senator-copy-token)
    (define-key km "\C-y" 'senator-yank-token)
    km)
  "Default key bindings in senator minor mode.")

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
    "-"
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
       senator-copy-token
       :active (semantic-current-nonterminal)
       :help "Copy the current token to a register"
       ])
    )
   "-"
   (list
    "Imenu Config"
    (list
     "Token Sorting Function"
     (senator-menu-item
      [ "Do not sort"
        (setq semantic-imenu-sort-bucket-functin nil)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function nil)
        :help "Do not sort imenu items"
        ])
     (senator-menu-item
      [ "Increasing by name"
        (setq semantic-imenu-sort-bucket-functin
              'semantic-sort-tokens-by-name-increasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-increasing)
        :help "Sort tokens by name increasing"
        ])
     (senator-menu-item
      [ "Decreasing by name"
        (setq semantic-imenu-sort-bucket-functin
              'semantic-sort-tokens-by-name-decreasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-decreasing)
        :help "Sort tokens by name decreasing"
        ])
     (senator-menu-item
      [ "Increasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-functin
              'semantic-sort-tokens-by-name-increasing-ci)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tokens-by-name-increasing-ci)
        :help "Sort tokens by name increasing and case insensitive"
        ])
     (senator-menu-item
      [ "Decreasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-functin
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
   (senator-menu-item
    [ "Semantic Database"
      (global-semanticdb-minor-mode)
      :active t
      :style toggle
      :selected (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
      :help "Cache tokens for killed buffers and between sessions."
      ])
   "-"
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

(defvar senator-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km senator-prefix-key senator-prefix-map)
    (define-key km [(shift mouse-3)] 'senator-completion-menu-mouse-popup)
    (easy-menu-define senator-minor-menu km "Senator Minor Mode Menu"
                      senator-menu-bar)
    km)
  "Keymap for senator minor mode.")

(defun senator-show-status ()
  "Update the modeline to show the senator minor mode state.
If `senator-isearch-semantic-mode' is non-nil append \"/si\" to
the value of the variable `senator-minor-mode-name'."
  (setq senator-mode (format (if senator-isearch-semantic-mode
                                 " %s/si"
                               " %s")
                             senator-minor-mode-name))
  (force-mode-line-update))

(defun senator-minor-mode-setup ()
  "Actually setup the senator minor mode.
Turn off the minor mode if semantic feature is not available or
`semantic-toplevel-bovine-table' not provided for the current buffer.
If minor mode is enabled parse the current buffer if needed.  Return
non-nil if the minor mode is enabled."
  (if senator-minor-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          ;; Disable minor mode if semantic stuff not available
          (senator-minor-mode nil)
        ;; XEmacs needs this
        (if (featurep 'xemacs)
            (easy-menu-add senator-minor-menu senator-mode-map))
        ;; Parse the current buffer if needed
        (condition-case nil
            (progn
              (senator-parse)
              ;; Add completion hooks
              (add-hook 'semantic-before-toplevel-cache-flush-hook
                        'senator-completion-cache-flush-fcn nil t)
              (add-hook 'semantic-clean-token-hooks
                        'senator-completion-cache-flush-fcn nil t))
          (quit (message "senator-minor-mode: parsing of buffer canceled.")))
        (senator-show-status)
        )
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
  
(if (fboundp 'define-minor-mode)

;;; Note that `define-minor-mode' actually calls the mode-function if
;;; the associated variable is non-nil, which requires that all needed
;;; functions be already defined.  [This is arguably a bug in d-m-m]
;;;###autoload
    (define-minor-mode senator-minor-mode
      "Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode is turned on only if semantic feature is available and a
`semantic-toplevel-bovine-table' is provided for the current buffer.
Return non-nil if the minor mode is enabled.

\\{senator-mode-map}"
      nil senator-mode senator-mode-map
      :global nil
      :group 'senator
      (senator-minor-mode-setup))

;;; `define-minor-mode' is not defined

  (defvar senator-minor-mode nil
    "Non-nil if senator minor mode is on.")
  (make-variable-buffer-local 'senator-minor-mode)
  
  (defvar senator-minor-mode-hook  nil
    "Hook called when senator minor mode is toggled")
  
  (defvar senator-minor-mode-on-hook nil
    "Hook called when senator minor mode is turned on")
  
  (defvar senator-minor-mode-off-hook nil
    "Hook called when senator minor mode is turned off")
  
;;;###autoload
  (defun senator-minor-mode (&optional arg)
      "Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode is turned on only if semantic feature is available and a
`semantic-toplevel-bovine-table' is provided for the current buffer.
Return non-nil if the minor mode is enabled.

\\{senator-mode-map}"
    (interactive "P")
    (let ((old-mode senator-minor-mode))
      (setq senator-minor-mode
            (if arg
                (or (listp arg) ;; C-u alone
                    (> (prefix-numeric-value arg) 0))
              (not senator-minor-mode)))
      (and senator-minor-mode-hook
           (not (equal old-mode senator-minor-mode))
           (run-hooks 'senator-minor-mode-hook))
      (and senator-minor-mode-on-hook
           senator-minor-mode
           (run-hooks 'senator-minor-mode-on-hook))
      (and senator-minor-mode-off-hook
           (not senator-minor-mode)
           (run-hooks 'senator-minor-mode-off-hook)))
    (senator-minor-mode-setup)
    (senator-message "Senator minor mode %s"
                     (if senator-minor-mode
                         "enabled"
                       "disabled"))
    senator-minor-mode)

  (if (fboundp 'add-minor-mode)
      
      ;; XEmacs
      (add-minor-mode 'senator-minor-mode 'senator-mode senator-mode-map)

    ;; Emacs 20
    (or (assq 'senator-minor-mode minor-mode-alist)
        (setq minor-mode-alist
              (cons (list 'senator-minor-mode 'senator-mode) minor-mode-alist)))
    
    (or (assq 'senator-minor-mode minor-mode-map-alist)
        (setq minor-mode-map-alist
              (cons (cons 'senator-minor-mode senator-mode-map)
                    minor-mode-map-alist)))
    
    ))

;;;;
;;;; Useful advices
;;;;

(defun senator-beginning-of-defun ()
  "Move backward to the beginning of a defun.
Use semantic tokens to navigate."
  (let ((senator-highlight-found nil)
        ;; Step at beginning of next token with id specified in
        ;; `senator-step-at-token-ids'.
        (senator-step-at-start-end-token-ids nil))
    (if (senator-previous-token)
        (beginning-of-line))
    (senator-message nil)))

(defun senator-end-of-defun ()
  "Move forward to next end of defun.
Use semantic tokens to navigate."
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
    (senator-message nil)))

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
      (senator-beginning-of-defun)
    ad-do-it))

(defadvice end-of-defun (around senator activate)
  "Move forward to next end of defun.
If semantic tokens are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-end-of-defun)
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
  (let ((val (get-register register)))
    (if (and senator-minor-mode (interactive-p)
             (listp val) (semantic-token-p (car val)))
        (senator-insert-foreign-token (car val) (cdr val))
      ad-do-it)))

(defadvice jump-to-register (around senator activate)
  "Insert contents of register REGISTER as a token.
If senator is not active, use the original mechanism."
  (let ((val (get-register register)))
    (if (and senator-minor-mode (interactive-p)
             (listp val) (semantic-token-p (car val)))
        (progn
          (find-file (cdr val))
          (goto-char (semantic-token-start (car val))))
      ad-do-it)))

(defun senator-insert-foreign-token (token tokenfile)
  "Insert TOKEN from a foreign buffer into the current buffer.
TOKEN will have originated from TOKENFILE.
This function is overridable."
  (if (or (not token) (not (semantic-token-p token)))
      (signal 'wrong-type-argument (list token 'semantic-token-p)))
  (let ((s (semantic-fetch-overload 'insert-foreign-token)))
    (if s (funcall s token tokenfile)
      (senator-insert-foreign-token-default token tokenfile))
    (message (semantic-summarize-nonterminal token))))

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

;;;;
;;;; Suggestion mode
;;;;

;; Since eldoc kicks butt for Emacs Lisp mode, this advice will let
;; us do eldoc like things for other languages.
(eval-when-compile (require 'eldoc)
                   (require 'semantic-ctxt))

(eval-after-load "eldoc"

  '(defadvice eldoc-print-current-symbol-info (around senator activate)
     "Enable ELDOC in non Emacs Lisp, but semantic-enabled modes."
     (if (eq major-mode 'emacs-lisp-mode)
         ad-do-it
       (when (eldoc-display-message-p)
         (senator-eldoc-print-current-symbol-info))))
  
  )

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
            (semantic-summarize-nonterminal found))
           (t nil)
           ))))

(defun senator-eldoc-print-current-symbol-info-default ()
  "Return a string message describing the current context."
  (let ((sym (semantic-ctxt-current-symbol))
        found)
    (if sym
        (progn
          (setq found (if semanticdb-current-database
                          (car (semanticdb-find-nonterminal-by-name
                                (car sym) nil t))
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
                (setq found (if semanticdb-current-database
                                (car (semanticdb-find-nonterminal-by-name
                                      (car sym) nil t))
                              (semantic-find-nonterminal-by-name
                               (car sym) (current-buffer) t)))
                (and (not found)
                     (semantic-flex-keyword-p (car sym))
                     (setq found (semantic-flex-keyword-get (car sym) 'summary)))
                ))
          ))
    found))

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
    (senator-show-status)
    (if isearch-mode
        ;; force lazy highlight update
        (senator-lazy-highlight-update)
      (senator-message "Isearch semantic mode %s"
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
  (senator-show-status))

(add-hook 'isearch-mode-hook 'senator-isearch-mode-hook)

(provide 'senator)

;;; senator.el ends here
