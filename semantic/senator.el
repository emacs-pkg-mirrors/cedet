;;; senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 10 Nov 2000
;; Version: 1.0
;; Keywords: tools, syntax
;; VC: $Id: senator.el,v 1.5 2000/11/27 13:24:17 david_ponce Exp $

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
;; This library defines the commands `senator-next-token' and
;; `senator-previous-token' to navigate between language semantic
;; tokens in current buffer.  It uses Eric Ludlam's semantic bovinator
;; tool to parse the buffer and find the language tokens.
;;
;; To install, put this file on your Emacs-Lisp load path and add
;; (require 'senator) into your ~/.emacs startup file.

;; The best way to use these navigation commands is to bind them to
;; keyword shortcuts.  The following example respectively binds
;; `senator-next-token' and `senator-previous-token' to [A-next]
;; (Alt+PageDown) and [A-prior] (Alt+PageUp) keys for the given mode:
;;
;;  (define-key mode-map [A-next]  'senator-next-token)
;;  (define-key mode-map [A-prior] 'senator-previous-token)
;;
;; You can customize the `senator-step-at-token-ids' and
;; `senator-step-at-start-end-token-ids' options to navigate only
;; between particular tokens and to step at start and end of some of
;; them.  To have a mode specific customization, do something like
;; this in a hook:
;;
;; (add-hook 'mode-hook
;;           (lambda ()
;;             (setq senator-step-at-token-ids '(function variable))
;;             (setq senator-step-at-start-end-token-ids '(function))
;;             ))
;;
;; The above example specifies to navigate only between functions and
;; variables, and to step at start and end of functions only.
;;
;; Any comments, suggestions, bug reports or upgrade requests are
;; welcome.  Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: senator.el,v $
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

(defgroup senator nil
  "SEmantic NAvigaTOR."
  :group 'semantic)

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

;;; Compatibility
(cond ((fboundp 'semantic-momentary-highlight-token)
       ;; semantic 1.3
       (defun senator-parse ()
         "Parse the current buffer and return the tokens where to navigate."
         (semantic-bovinate-toplevel t))
       )
      (t
       ;; semantic before 1.3
       (defun semantic-momentary-highlight-token (token)
         "Highlight TOKEN, not implemented in this version of semantic."
         ;; Does nothing
         )
       (defun senator-parse ()
         "Parse the current buffer and return the tokens where to navigate."
         (semantic-bovinate-toplevel nil nil t))
       ))

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

(defun senator-find-previous-token-aux (tokens pos &optional prev)
  "Visit TOKENS and return the token just before POS.
Optional PREV is the previous visited token.  This is an helper
function for `senator-find-previous-token'."
  (let (token)
    (while tokens
      (setq token (car tokens))
      (if (>= (semantic-token-start token) pos)
          (throw 'found prev))
      (setq prev (if (eq (semantic-token-token token) 'type)
                     (senator-find-previous-token-aux
                      (semantic-token-type-parts token) pos token)
                   token))
      (setq tokens (cdr tokens)))
    prev))

(defun senator-find-previous-token (tokens pos)
  "Visit TOKENS and return the token just before POS."
  (catch 'found (senator-find-previous-token-aux tokens pos)))

(defun senator-find-next-token (tokens pos)
  "Visit TOKENS and return the token at or just after POS."
  (let (token found)
    (while (and tokens (not found))
      (setq token (car tokens))
      (if (or (and (senator-step-at-start-end-p token)
                   (> (semantic-token-end token) pos))
             (>= (semantic-token-start token) pos))
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

;;;###autoload
(defun senator-next-token ()
  "Navigate to the next semantic token.
Return the semantic token or nil if at end of buffer."
  (interactive)
  (let ((pos    (point))
        (tokens (senator-parse))
        found where)
    (if (memq real-last-command
              '(senator-previous-token senator-next-token))
        (forward-char))
    (setq found (senator-find-next-token tokens (point)))
    (while (senator-skip-p found)
      (if (not (eq (semantic-token-token found) 'type))
          (goto-char (semantic-token-end found))
        (forward-char))
      (setq found (senator-find-next-token tokens (point))))
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
      (semantic-momentary-highlight-token found)
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
    (if (eq real-last-command 'senator-previous-token)
        (backward-char))
    (setq found (senator-find-previous-token tokens (point)))
    (while (senator-skip-p found)
      (if (not (eq (semantic-token-token found) 'type))
          (goto-char (semantic-token-start found))
        (backward-char))
      (setq found (senator-find-previous-token tokens (point))))
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
      (semantic-momentary-highlight-token found)
      (senator-message "%S: %s (%s)"
                       (semantic-token-token found)
                       (semantic-token-name  found)
                       where))
    found))

(provide 'senator)

;;; senator.el ends here
