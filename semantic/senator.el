;;; senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 10 Nov 2000
;; Version: 2.1
;; Keywords: tools, syntax
;; VC: $Id: senator.el,v 1.15 2001/01/03 16:09:28 david_ponce Exp $

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
(require 'senator-isearch)       ; Needed isearch advices

(defgroup senator nil
  "SEmantic NAvigaTOR."
  :group 'semantic)

(defcustom senator-minor-mode-name "Senator"
  "*Name displayed in the modeline when senator minor mode is on."
  :group 'senator
  :type 'string)

(defface senator-momentary-highlight-face  '((((class color) (background dark))
					      (:background "gray30"))
					     (((class color) (background light))
					      (:background "gray70")))
  "Face used to momentarilly highlight tokens."
  :group 'senator)

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
nil navigation only step at beginning of tokens.  This is a buffer
local variable.  It can be set in a mode hook to get a specific
langage navigation."
  :group 'senator
  :type '(repeat (symbol)))
(make-variable-buffer-local 'senator-step-at-start-end-token-ids)

(defcustom senator-highlight-found t
  "*If non-nil highlight tokens found.
This option requires semantic 1.3 and above.  This is a buffer
local variable.  It can be set in a mode hook to get a specific
langage behaviour."
  :group 'senator
  :type 'boolean)
(make-variable-buffer-local 'senator-highlight-found)

(defcustom senator-separator-char ?#
  "*Character separator used to compose token full name."
  :group 'senator
  :type 'character)
(make-variable-buffer-local 'senator-separator-char)

;;; Compatibility
(cond ((fboundp 'semantic-momentary-highlight-token)
       ;; semantic 1.3
       (defun senator-parse ()
         "Parse the current buffer and return the tokens where to navigate."
         (semantic-bovinate-toplevel t))
       )
      (t
       ;; semantic before 1.3
       (defun semantic-momentary-highlight-token (&rest ignore)
         "Highlight a token, removing highlighting when the user hits a key.
Not implemented in this version of the Semantic Bovinator.  IGNORE
arguments and always return nil."
         nil)
       (defun semantic-find-nonterminal-by-overlay (&rest ignore)
         "Find all nonterminals covering a position by using overlays.
Not implemented in this version of the Semantic Bovinator.  IGNORE
arguments and always return nil."
         nil)
       (defun senator-parse ()
         "Parse the current buffer and return the tokens where to navigate."
         (semantic-bovinate-toplevel nil nil t))
       ))

;;;;
;;;; Common functions
;;;;

(defun senator-momentary-highlight-token (token)
  "Momentary highlight TOKEN.
Does nothing if `senator-highlight-found' is nil or semantic version
is bellow 1.3."
  (and senator-highlight-found
       (condition-case nil
 	   (semantic-momentary-highlight-token
	    token 'senator-momentary-highlight-face)
	 (error (semantic-momentary-highlight-token token)))))

(defun senator-message (&rest args)
  "Call function `message' with ARGS without logging."
  (let (message-log-max)
    (apply 'message args)))

(defun senator-step-at-start-end-p (token)
  "Return non-nil if must step at start and end of TOKEN."
  (if token
      (let ((categ (semantic-token-token token)))
        (and (not (eq categ 'type))
             (memq categ senator-step-at-start-end-token-ids)))))

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
  (let (token)
    (while tokens
      (setq token (car tokens))
      (if (funcall comp (semantic-token-start token) pos)
          (throw 'found prev))
      (or (senator-skip-p token)
          (setq prev token))
      (if (eq (semantic-token-token token) 'type)
          (setq prev (senator-find-token-before
                      (semantic-token-type-parts token) pos comp prev)))
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
  (let (token found)
    (while (and tokens (not found))
      (setq token (car tokens))
      (if (and (not (senator-skip-p token))
               (or (and (senator-step-at-start-end-p token)
                        (> (semantic-token-end token) pos))
                   (> (semantic-token-start token) pos)))
          (setq found token)
        (if (eq (semantic-token-token token) 'type)
            (setq found (senator-find-next-token
                         (semantic-token-type-parts token) pos))))
      (setq tokens (cdr tokens)))
    found))

(defun senator-middle-of-token-p (pos token)
  "Return non-nil if POS is between start and end of TOKEN."
  (and (> pos (semantic-token-start token))
       (< pos (semantic-token-end   token))))

(defun senator-full-token-name (token parent)
  "Compose a full name from TOKEN name and names in its PARENT list.
A `senator-separator-char' separates each token name.  The parent list
is in reverse order."
  (let ((sep  (char-to-string senator-separator-char))
        (name ""))
    (while parent
      (setq name (concat (semantic-token-name (car parent))
                         sep
                         name)
            parent (cdr parent)))
    (concat name (semantic-token-name token))))

(defun senator-last-name (full-name)
  "Return the last name from FULL-NAME.
That is the name after the last `senator-separator-char' or FULL-NAME
itself if it does not contain any separator character."
  (and (string-match (format "[%c]?\\([^%c]+\\)\\'"
                             senator-separator-char
                             senator-separator-char)
                     full-name)
       (match-string 1 full-name)))

(defun senator-completion-stream (stream parent full-name-p &optional top-level)
  "Return a useful completion list from STREAM.
That is a flat list of all tokens available.  Prepend to each token
name the name of tokens in its PARENT list if FULL-NAME-P is non-nil.
This helps to distinguish between tokens in multiple top level type
declarations or in sub type declarations.  If TOP-LEVEL is non-nil the
completion list will contain only tokens at top level.  Otherwise all
sub type tokens are included too."
  (let (cs token)
    (while stream
      (setq token  (car stream))
      (setq stream (cdr stream))
      (setq cs (cons (cons (senator-full-token-name token parent)
                           (cdr token))
                     cs))
      (and (not top-level)
           (eq (semantic-token-token token) 'type)
           (setq cs (append cs (senator-completion-stream
                                (semantic-token-type-parts token)
                                (and full-name-p (cons token parent))
                                t)))))
    cs))

(defun senator-current-type-context ()
  "Return tokens in the type context at point or nil if not found."
  (let ((context (semantic-find-nonterminal-by-token
                  'type (semantic-find-nonterminal-by-overlay))))
    (if context
        (semantic-token-type-parts
         (nth (1- (length context)) context)))))

(defun senator-completion-list (&optional in-context)
  "Return a useful completion list from tokens in current buffer.
That is a flat list of all tokens available.  If IN-CONTEXT is not nil
return only the top level tokens in the type context at point or the
top level tokens in the current buffer if no type context exists at
point."
  (let (stream full-name-p)
    (if in-context
        (setq stream (senator-current-type-context)))
    (or stream (setq stream (senator-parse)))
    (setq full-name-p (and (not in-context)
                           (cdr (semantic-find-nonterminal-by-token
                                 'type stream))))
    (senator-completion-stream stream nil full-name-p in-context)))

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
                        "\\b"
                        (regexp-quote
                         (if (string-match "\\`\\([^[]+\\)[[]" name)
                             (match-string 1 name)
                           name)))
                       (semantic-token-end token))))

(defun senator-search-forward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in semantic tokens after point.
See `search-forward' for the meaning of BOUND NOERROR and COUNT.
BOUND and COUNT are just ignored in the current implementation."
  (let ((origin (point))
        (tokens (senator-parse))
        (senator-step-at-start-end-token-ids nil)
        token pos start limit)
    (save-excursion
      (setq token (or (senator-find-last-token tokens origin)
                      (senator-find-next-token tokens origin)))
      (while (and token (not pos))
        (setq limit (senator-search-token-name token))
        (setq start (match-beginning 0))
        (if (and (> origin start) (< origin limit))
            (setq start origin))
        (goto-char start)
        (setq pos (funcall searcher what limit t))
        (if (and pos (>= (match-beginning 0) origin))
            nil
          (setq pos nil)
          (setq token (senator-find-next-token tokens (point))))))
    (if pos
        (goto-char start)
      (setq limit (point)))
    (funcall searcher what limit noerror)))

(defun senator-search-backward-raw (searcher what &optional bound noerror count)
  "Use SEARCHER to search WHAT in semantic tokens before point.
See `search-backward' for the meaning of BOUND NOERROR and
COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (let ((origin (point))
        (tokens (senator-parse))
        (senator-step-at-start-end-token-ids nil)
        token pos start limit)
    (save-excursion
      (setq token (senator-find-previous-token tokens origin))
      (while (and token (not pos))
        (setq start (senator-search-token-name token))
        (setq limit (match-beginning 0))
        (if (and (< origin start) (> origin limit))
            (setq start origin))
        (goto-char start)
        (setq pos (funcall searcher what limit t))
        (if (and pos (<= (match-end 0) origin))
            nil
          (setq pos nil)
          (goto-char (semantic-token-start token))
          (setq token (senator-find-previous-token tokens (point))))))
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

;;;###autoload
(defun senator-jump (sym)
  "Jump to the semantic symbol SYM.
If called interactively and a prefix argument is supplied jump in the
local type's context (see function `senator-current-type-context')."
  (interactive
   (list
    (completing-read "Jump to: "
                     (senator-completion-list current-prefix-arg)
                     nil
                     t
                     ""
                     'semantic-read-symbol-history)))
  (when sym
    (let ((token
           (semantic-find-nonterminal-by-name (senator-last-name sym)
                                              (current-buffer))))
      (goto-char (semantic-token-start  token))
      (senator-momentary-highlight-token token)
      (senator-message "%S: %s "
                       (semantic-token-token token)
                       (semantic-token-name  token)))))

;;;;
;;;; Search commands
;;;;

;;;###autoload
(defun senator-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for string WHAT.
Set point to the end of the occurrence found, and return point.  See
`search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (interactive "sSemantic search: ")
  (senator-search-forward-raw #'search-forward what bound noerror count))

;;;###autoload
(defun senator-re-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for regexp WHAT.
Set point to the end of the occurrence found, and return point.  See
`re-search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (interactive "sSemantic regexp search: ")
  (senator-search-forward-raw #'re-search-forward what bound noerror count))

;;;###autoload
(defun senator-word-search-forward (what &optional bound noerror count)
  "Search semantic tokens forward from point for word WHAT.
Set point to the end of the occurrence found, and return point.  See
`word-search-forward' for details and the meaning of BOUND NOERROR and
COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (interactive "sSemantic word search: ")
  (senator-search-forward-raw #'word-search-forward what bound noerror count))

;;;###autoload
(defun senator-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for string WHAT.
Set point to the beginning of the occurrence found, and return point.
See `search-backward' for details and the meaning of BOUND NOERROR and
COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (interactive "sSemantic backward search: ")
  (senator-search-backward-raw #'search-backward what bound noerror count))

;;;###autoload
(defun senator-re-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for regexp WHAT.
Set point to the beginning of the occurrence found, and return point.
See `re-search-backward' for details and the meaning of BOUND NOERROR
and COUNT.  BOUND and COUNT are just ignored in the current
implementation."
  (interactive "sSemantic backward regexp search: ")
  (senator-search-backward-raw #'re-search-backward what bound noerror count))

;;;###autoload
(defun senator-word-search-backward (what &optional bound noerror count)
  "Search semantic tokens backward from point for word WHAT.
Set point to the beginning of the occurrence found, and return point.
See `word-search-backward' for details and the meaning of BOUND
NOERROR and COUNT.  BOUND and COUNT are just ignored in the current
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
    km)
  "Default key bindings in senator minor mode.")

(defun senator-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if (featurep 'xemacs)
      (let ((n (length item))
            (i 0)
            l)
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
   "-"
   (senator-menu-item
    ["Options..."
     (customize-group "senator")
     :active t
     :help "Customize SEmantic NAvigaTOR options"
     ])
   )
  "Menu for senator minor mode.")

(defvar senator-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km senator-prefix-key senator-prefix-map)
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
      (if (not (and (featurep 'semantic) semantic-toplevel-bovine-table))
          ;; Disable minor mode if semantic stuff not available
          (senator-minor-mode nil)
        ;; XEmacs needs this
        (if (featurep 'xemacs)
            (easy-menu-add senator-minor-menu senator-mode-map))
        ;; Parse the current buffer if needed
	(condition-case nil
	    (senator-parse)
	  (quit (message "senator-minor-mode: parsing of buffer canceled.")))
        (senator-show-status)
        )
    ;; XEmacs needs this
    (if (featurep 'xemacs)
        (easy-menu-remove senator-minor-menu))
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
        (senator-step-at-start-end-token-ids nil)
        (senator-step-at-token-ids '(function)))
    (if (senator-previous-token)
        (beginning-of-line))
    (senator-message nil)))

(defun senator-end-of-defun ()
  "Move forward to next end of defun.
Use semantic tokens to navigate."
  (let* ((senator-highlight-found nil)
         (senator-step-at-start-end-token-ids '(function))
         (senator-step-at-token-ids '(function))
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
;;;; Using semantic search in isearch mode
;;;;

(defun senator-isearch-search-handler ()
  "Return the actual search function used by `isearch-search'.
If `senator-isearch-semantic-mode' is nil it delegates to the
function `isearch-default-search-handler'.  Otherwise it returns one
of the functions `senator-search-forward', `senator-search-backward',
`senator-word-search-forward', `senator-word-search-backward',
`senator-re-search-forward' or `senator-re-search-backward' depending
on current values of the variables `isearch-forward', `isearch-regexp'
and `isearch-word'."
  (if senator-isearch-semantic-mode
      (cond (isearch-word
             (if isearch-forward
                 'senator-word-search-forward
               'senator-word-search-backward))
            (isearch-regexp
             (if isearch-forward
                 'senator-re-search-forward
               'senator-re-search-backward))
            (t
             (if isearch-forward
                 'senator-search-forward
               'senator-search-backward)))
    (isearch-default-search-handler)))

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
(define-key isearch-mode-map [(control ?,)] 'senator-isearch-toggle-semantic-mode)

(defun senator-isearch-mode-hook ()
  "Isearch mode hook to setup semantic searching."
  (setq isearch-search-handler-provider #'senator-isearch-search-handler)
  (or senator-minor-mode
      (setq senator-isearch-semantic-mode nil))
  (senator-show-status))

(add-hook 'isearch-mode-hook 'senator-isearch-mode-hook)

(provide 'senator)

;;; senator.el ends here
