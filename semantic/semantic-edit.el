;;; semantic-edit.el --- Edit Management for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id $

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
;; In Semantic 1.x, changes were handled in a simplistic manner, where
;; tokens that changed were reparsed one at a time.  Any other form of
;; edit were managed through a full reparse.
;;
;; This code attempts to minimize the number of times a full reparse
;; needs to occur.  While overlays and tokens will continue to be
;; recycled in the simple case, new cases where tokens are inserted
;; into the original list will be added.
;;

(require 'semantic)

;;; Code:
(defvar semantic-after-partial-cache-change-hook nil
  "Hooks run after the buffer token list has been updated.
This list will change when the current token list has been partially
reparsed.

Hook functions must take one argument, which is the list of tokens
updated among the ones associated with this buffer.

For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-dirty-tokens nil
  "List of tokens in the current buffer which are dirty.
Dirty functions can then be reparsed, and spliced back into the main list.")
(make-variable-buffer-local 'semantic-dirty-tokens)


(defvar semantic-dirty-token-hooks nil
  "Hooks run after when a token is marked as dirty (edited by the user).
The functions must take TOKEN, START, and END as a parameters.
This hook will only be called once when a token is first made dirty,
subsequent edits will not cause this to run a second time unless that
token is first cleaned.  Any token marked as dirty will
also be called with `semantic-clean-token-hooks', unless a full
reparse is done instead.")

(defvar semantic-pre-clean-token-hooks nil
  "Hooks run before a token is reparsed.
The functions must take a TOKEN as a parameter.
Any token sent to this hook is about to be cleaned, or reparsed.
The overlay may change, but many features and properties will
persist unless a full reparse is later required.
See `semantic-dirty-token-hooks' and `semantic-clean-token-hooks'.")

(defvar semantic-clean-token-hooks nil
  "Hooks run after a token is marked as clean (reparsed after user edits.)
The functions must take a TOKEN as a parameter.
Any token sent to this hook will have first been called with
`semantic-dirty-token-hooks'.  This hook is not called for tokens
marked dirty if the buffer is completely reparsed.  In that case, use
`semantic-after-toplevel-cache-change-hook'.")

(defvar semantic-change-hooks nil
  "Hooks run when semantic detects a change in a buffer.
Each hook function must take three arguments, identical to the
common hook `after-change-functions'.")

(defvar semantic-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as needing a reparse.
For language specific hooks, make sure you define this as a local
hook.
Not used yet; part of the next generation reparse mechanism")

(defvar semantic-no-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as not needing a reparse.
If the hook returns non-nil, then declare that a reparse is needed.
For language specific hooks, make sure you define this as a local
hook.
Not used yet; part of the next generation reparse mechanism.")


;; State
(defun semantic-bovine-toplevel-partial-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a partial reparse.
This only returns non-nil if `semantic-bovine-toplevel-full-reparse-needed-p'
returns nil.
Optional argument CHECKCACHE indicates if the cache check should be made
when checking `semantic-bovine-toplevel-full-reparse-needed-p'."
  (and semantic-toplevel-bovine-cache
       semantic-dirty-tokens
       (not (semantic-bovine-toplevel-full-reparse-needed-p checkcache))))

;; Cleanup
(defun semantic-remove-dirty-children-internal (token dirties)
  "Remove TOKEN children from DIRTIES.
Return the new value of DIRTIES."
  (if dirties
      (let ((children (semantic-nonterminal-children token t))
            child)
        (while (and children dirties)
          (setq child (car children)
                children (cdr children)
                dirties  (semantic-remove-dirty-children-internal
                          child (delq child dirties))))))
  dirties)

(defun semantic-remove-dirty-children ()
  "Remove children of dirty tokens from the list of dirty tokens.
It is not necessary and even dangerous to reparse these tokens as they
will be recreated when reparsing their parents.  Return the new value
of the variable `semantic-dirty-tokens' changed by side effect."
  (let ((dirties semantic-dirty-tokens)
        token)
    (while dirties
      (setq token   (car dirties)
            dirties (cdr dirties)
            semantic-dirty-tokens
            (semantic-remove-dirty-children-internal
             token semantic-dirty-tokens))))
  semantic-dirty-tokens)

(defun semantic-change-function (start end length)
  "Provide a mechanism for semantic token management.
Argument START, END, and LENGTH specify the bounds of the change."
  (setq semantic-unmatched-syntax-cache-check t)
  (run-hook-with-args 'semantic-change-hooks start end length))


;;; Reparse a dirty area, and update the master cache.
;;
(defun semantic-rebovinate-token (token)
  "Use TOKEN for extents, and reparse it, splicing it back into the cache."
  ;; Pre Hooks
  (run-hook-with-args 'semantic-pre-clean-token-hooks token)

  (let* ((flexbits (semantic-lex (semantic-token-start token)
                                 (semantic-token-end token)))
	 ;; For embedded tokens (type parts, for example) we need a
	 ;; different symbol.  Come up with a plan to solve this.
	 (nonterminal (semantic-token-get token 'reparse-symbol))
	 (new (semantic-bovinate-nonterminal
               flexbits
               semantic-toplevel-bovine-table
               nonterminal))
	 (cooked nil)
	 )
    (setq new (car (cdr new)))
    (if (not new)
        ;; Clever reparse failed, queuing full reparse.
        (setq semantic-toplevel-bovine-cache-check t)
      (setq cooked (semantic-raw-to-cooked-token new))
      (if (not (eq new (car cooked)))
          (if (= (length cooked) 1)
              ;; Cooking did a 1 to 1 replacement.  Use it.
              (setq new (car cooked))
          ;; If cooking results in multiple things, do a full reparse.
            (setq semantic-toplevel-bovine-cache-check t))))
    ;; Don't do much if we have to do a full recheck.
    (if semantic-toplevel-bovine-cache-check
        nil
      (semantic-overlay-token new)
      (let ((oo (semantic-token-overlay token))
            (o (semantic-token-overlay new)))
        ;; Copy all properties of the old overlay here.
        ;; I think I can use plists in emacs, but not in XEmacs.  Ack!
        (semantic-overlay-put o 'face (semantic-overlay-get oo 'face))
        (semantic-overlay-put o 'old-face (semantic-overlay-get oo 'old-face))
        (semantic-overlay-put o 'intangible (semantic-overlay-get oo 'intangible))
        (semantic-overlay-put o 'invisible (semantic-overlay-get oo 'invisible))
        ;; Free the old overlay(s)
        (semantic-deoverlay-token token)
        ;; Recover properties
        (let ((p (semantic-token-properties token)))
          (while p
            (semantic-token-put new (car (car p)) (cdr (car p)))
            (setq p (cdr p))))
        (semantic-token-put new 'reparse-symbol nonterminal)
        (semantic-token-put new 'dirty nil)
        ;; Splice into the main list.
        (setcdr token (cdr new))
        (setcar token (car new))
        ;; This important bit is because the CONS cell representing
        ;; TOKEN is what we need here, even though the whole thing is
        ;; the same.
        (semantic-overlay-put o 'semantic token)
        ;; Hooks
        (run-hook-with-args 'semantic-clean-token-hooks token)
        )
      )))


;;; Nonterminal regions and splicing
;;
;; This functionality is needed to take some set of dirty code,
;; and splice in new tokens after a partial reparse.

(defun semantic-change-function-mark-dirty  (start end length)
  "Run whenever a buffer controlled by `semantic-mode' changes.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change."
  (when (and (not semantic-toplevel-bovine-cache-check)
	     (not semantic-edits-are-safe))
    (let ((tl (condition-case nil
		  (nreverse (semantic-find-nonterminal-by-overlay-in-region
		   (1- start) (1+ end)))
		(error nil))))
      (if tl
	  (catch 'alldone
	    ;; Loop over the token list
	    (while tl
	      (cond
	       ;; If we are completely enclosed in this overlay.
	       ((and (> start (semantic-token-start (car tl)))
		     (< end (semantic-token-end (car tl))))
		(if (semantic-token-get (car tl) 'dirty)
		    nil
		  (add-to-list 'semantic-dirty-tokens (car tl))
		  (semantic-token-put (car tl) 'dirty t)
		  (condition-case nil
		      (run-hook-with-args 'semantic-dirty-token-hooks
					  (car tl) start end)
		    (error (if debug-on-error (debug)))))
		  (throw 'alldone t))
	       ;; If we cover the beginning or end of this item, we must
	       ;; reparse this object.  If there are more items coming, then postpone
	       ;; this till later.
	       ((not (cdr tl))
		(setq semantic-toplevel-bovine-cache-check t)
		(run-hooks 'semantic-reparse-needed-change-hook))
	       (t nil))
	      ;; next
	      (setq tl (cdr tl))))
	;; There was no hit, perhaps we need to reparse this intermediate area.
	(setq semantic-toplevel-bovine-cache-check t)
	)
      )))
;;
;; Properties set on the tokens are:
;;  dirty          - This token is dirty
;;  dirty-after    - This token, and the white space after it is dirty
;;  dirty-before   - This token, and the white space before it is dirty
;;  dirty-children - This token has children that are dirty.
;;
;; EXPERIMENTAL
(defsubst semantic-find-nearby-dirty-tokens (beg end)
  "Make a special kind of token for dirty whitespace.
Argument BEG and END is the region to find nearby tokens.
EXPERIMENTAL"
  (let ((prev (semantic-find-nonterminal-by-overlay-prev beg))
	(next (semantic-find-nonterminal-by-overlay-next end)))
    (if prev (semantic-token-put prev 'dirty-after t))
    (if next (semantic-token-put next 'dirty-before t))
    (list prev next)))

(defun semantic-set-tokens-dirty-in-region (beg end)
  "Mark the region between BEG and END as dirty.
This is done by finding tokens overlapping the region, and marking
them dirty.  Regions not covered by a token are then marked as
dirty-after, meaning the space after that area is dirty.
This function will be called in an after change hook, and must
be very fast.
EXPERIMENTAL"
  (let ((tromp (semantic-find-nonterminal-by-overlay-in-region beg end))
	(ttmp nil)
	)
    (if (not tromp)
	;; No tokens hit, setup a dirty region on the screen.
	(setq tromp nil) ;(semantic-get-dirty-token beg end))
      ;; First, mark all fully dirty tokens.
      (setq ttmp tromp)
      (while ttmp
	(and (> beg (semantic-token-start (car tromp)))
	     (< end (semantic-token-end (car tromp))))

	)
	)))

(provide 'semantic-edit)

;;; semantic-edit.el ends here
