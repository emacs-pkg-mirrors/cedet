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
;; or old tokens removed  from the original list are handled.
;;

;;; NOTES FOR IMPROVEMENT
;;
;; Work done by the incremental parser could be improved by the
;; following:
;;
;; 1. Tokens created could have as a property an overlay marking a region
;;    of themselves that can be edited w/out affecting the definition of
;;    that token.
;;
;; 2. Tokens w/ positioned children could have a property of an
;;    overlay marking the region in themselves that contain the
;;    children.  This could be used to better improve splicing near
;;    the beginning and end of the child lists.
;; 

;;; BUGS IN INCREMENTAL PARSER
;;
;; 1. Changes in the whitespace between tokens could extend a
;;    following token.  These will be marked as merely unmatched
;;    syntax instead.

;;
(require 'semantic)

;;; Code:
(defvar semantic-edits-need-reparse nil
  "This variable is set non-nil when a partial reparse is needed.")
(make-variable-buffer-local 'semantic-edits-need-reparse)

(defvar semantic-after-partial-cache-change-hook nil
  "Hooks run after the buffer token list has been updated.
This list will change when the current token list has been partially
reparsed.

Hook functions must take one argument, which is the list of tokens
updated among the ones associated with this buffer.

For language specific hooks, make sure you define this as a local hook.")

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

(defvar semantic-edits-new-change-hooks nil
  "Hooks run when a new change is found.
Functions must take one argument representing an overlay on that change.")

(defvar semantic-edits-delete-change-hooks nil
  "Hooks run before a change overlay is deleted.
Deleted changes occur when multiple changes are merged.
Functions must take one argument representing an overlay being deleted.")

(defvar semantic-edits-move-change-hooks nil
  "Hooks run after a change overlay is moved.
Changes move when a new change overlaps an old change.  The old change
will be moved.
Functions must take one argument representing an overlay being moved.")

(defvar semantic-edits-reparse-change-hooks nil
  "Hooks run after a change results in a reparse.
Functions are called before the overlay is deleted, and after the
incremental reparse.")

(defvar semantic-edits-incremental-reparse-failed-hooks nil
  "Hooks run after the incremental parser fails.
When this happens, the buffer is marked as needing a full reprase.")

;; State
(defun semantic-bovine-toplevel-partial-reparse-needed-p (&optional checkcache)
  "Return non-nil if the current buffer needs a partial reparse.
This only returns non-nil if `semantic-bovine-toplevel-full-reparse-needed-p'
returns nil.
Optional argument CHECKCACHE indicates if the cache check should be made
when checking `semantic-bovine-toplevel-full-reparse-needed-p'."
  (and semantic-toplevel-bovine-cache
       (or semantic-dirty-tokens semantic-edits-need-reparse)
       (not (semantic-bovine-toplevel-full-reparse-needed-p checkcache))))

(defun semantic-changes-in-region (start end &optional buffer)
  "Find change overlays which exist in whole or in part between START and END.
Optional argument BUFFER is the buffer to search for changes in."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((ol (semantic-overlays-in start end))
	  (ret nil))
      (while ol
	(let ((tmp (semantic-overlay-get (car ol) 'semantic-change)))
	  (when tmp
	    (setq ret (cons (car ol) ret))))
	(setq ol (cdr ol)))
      (sort ret (lambda (a b) (< (semantic-overlay-start a)
				 (semantic-overlay-start b)))))))

(defun semantic-edits-change-function-handle-changes  (start end length)
  "Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change."
  ;; We move start/end by one so that we can merge changes that occur
  ;; just before, or just after.  This lets simple typing capture everything
  ;; into one overlay.
  (let ((changes-in-change (semantic-changes-in-region (1- start) (1+ end)))
	)
    (if (not changes-in-change)
	(let ((o (semantic-make-overlay start end)))
	  (semantic-overlay-put o 'semantic-change t)
	  (setq semantic-edits-need-reparse t)
	  ;; Run the hooks safely.  When hooks blow it, our dirty
	  ;; function will be removed from the list of active change
	  ;; functions.
	  (condition-case nil
	      (run-hook-with-args 'semantic-edits-new-change-hooks o)
	    (error nil)))
      (let ((newstart start) (newend end)
	    (tmp changes-in-change))
	;; Find greatest bounds of all changes
	(while tmp
	  (when (< (semantic-overlay-start (car tmp)) start)
	    (setq start (semantic-overlay-start (car tmp))))
	  (when (> (semantic-overlay-end (car tmp)) end)
	    (setq end (semantic-overlay-end (car tmp))))
	  (setq tmp (cdr tmp)))
	;; Move the first found overlay, recycling that overlay.
	(semantic-overlay-move (car changes-in-change) start end)
	(condition-case nil
	    (run-hook-with-args 'semantic-edits-move-change-hooks
				(car changes-in-change))
	  (error nil))
	(setq changes-in-change (cdr changes-in-change))
	;; Delete other changes.  They are now all bound here.
	(while changes-in-change
	  (condition-case nil
	      (run-hook-with-args 'semantic-edits-delete-change-hooks
				  (car changes-in-change))
	    (error nil))
	  (semantic-overlay-delete (car changes-in-change))
	  (setq changes-in-change (cdr changes-in-change))))
      )))

(defsubst semantic-edits-flush-change (change)
  "Flush the CHANGE overlay."
  (condition-case nil
      (run-hook-with-args 'semantic-edits-delete-change-hooks
			  change)
    (error nil))
  (semantic-overlay-delete change))

(defun semantic-edits-flush-changes ()
  "Flush the changes in the current buffer."
  (let ((changes (semantic-changes-in-region (point-min) (point-max))))
    (while changes
      (semantic-edits-flush-change (car changes))
      (setq changes (cdr changes))))
  )

(defun semantic-edits-bovinate-region (start end &optional reparse-symbol)
  "Bovinate the area between START and END, and return any tokens found.
If END needs to be extended due to a lexical token being too large,
it will be silently ignored.
Optional argument REPARSE-SYMBOL is the rule to start parsing at if it
is known.
This function returns tokens with overlays (already cooked.)"
  (let* ((lexbits (semantic-lex start end))
	 (tokens (semantic-bovinate-nonterminals
		  lexbits
		  reparse-symbol
		  nil
		  t)))
    tokens))

(defun semantic-edits-change-in-one-token-p (change hits)
  "Return non-nil of the overlay CHANGE exists solely in one leaf token.
HITS is the list of tokens that CHANGE is in.  It can have more than
one token in it if the leaf token is within a parent token."
  (and (< (semantic-token-start (car hits))
	  (semantic-overlay-start change))
       (> (semantic-token-end (car hits))
	  (semantic-overlay-end change))
       ;; Recurse on the rest.  If this change is inside all
       ;; of these tokens, then they are all leaves or parents
       ;; of the smallest token.
       (or (not (cdr hits))
	   (semantic-edits-change-in-one-token-p change (cdr hits))))
  )

;;; Change/Token Query functions
;;
;; A change (region of space) can effect tokens in different ways.
;; These functions perform queries on a buffer to determine different
;; ways that a change effects a buffer.
;;
;; NOTE: After debugging these, replace below to no longer look
;;       at point and mark (via comments I assume.)
(defsubst semantic-edits-os (change)
  "For testing: Start of CHANGE, or smaller of (point) and (mark)."
  (if change (semantic-overlay-start change)
    (if (< (point) (mark)) (point) (mark))))

(defsubst semantic-edits-oe (change)
  "For testing: End of CHANGE, or larger of (point) and (mark)."
  (if change (semantic-overlay-end change)
    (if (> (point) (mark)) (point) (mark))))

(defun semantic-edits-change-leaf-token (change)
  "A leaf token which completely encompasses CHANGE.
If change overlaps a token, but is not encompassed in it, return nil.
Use `semantic-edits-change-overlap-leaf-token'.
If CHANGE is completely encompassed in a token, but overlaps sub-tokens,
return nil."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tokens (nreverse
		  (semantic-find-nonterminal-by-overlay-in-region
		   start end))))
    ;; A leaf is always first in this list
    (if (and tokens
	     (< (semantic-token-start (car tokens)) start)
	     (> (semantic-token-end (car tokens)) end))
	;; Ok, we have a match.  If this token has children,
	;; we have to do more tests.
	(let ((chil (semantic-nonterminal-children (car tokens) t)))
	  (if (not chil)
	      ;; Simple leaf.
	      (car tokens)
	    ;; For this type, we say that we encompass it if the
	    ;; change occurs outside the range of the children.
	    (if (or
		 (> start (semantic-token-end (nth (1- (length chil)) chil)))
		 (< end (semantic-token-start (car chil))))
		;; We have modifications to the definition of this parent
		;; so we have to reparse the whole thing.
		(car tokens)
	      ;; We actually modified an area between some children.
	      ;; This means we should return nil, as that case is
	      ;; calculated by someone else.
	      nil)))
      nil)))

(defun semantic-edits-change-between-tokens (change)
  "Return a cache list of tokens surrounding CHANGE.
The returned list is the CONS cell in the master list pointing to
a token just before CHANGE.  The CDR will have the token just after CHANGE.
CHANGE cannot encompass or overlap a leaf token.
If CHANGE is fully encompassed in a token that has children, and
this change occurs between those children, this returns non-nil.
See `semantic-edits-change-leaf-token' for details on parents."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tokens (nreverse
		  (semantic-find-nonterminal-by-overlay-in-region
		   start end)))
	 (list-to-search nil))
    (if (not tokens)
	(setq list-to-search semantic-toplevel-bovine-cache)
      ;; A leaf is always first in this list
      (if (and (< (semantic-token-start (car tokens)) start)
	       (> (semantic-token-end (car tokens)) end))
	  ;; We are completely encompassed in a token.
	  (if (setq list-to-search
		    (semantic-nonterminal-children (car tokens) t))
	      ;; Ok, we are completely encompassed within the first token
	      ;; entry, AND that token has children.  This means that change
	      ;; occured outside of all children, but inside some token
	      ;; with children.
	      (if (or
		   (> start (semantic-token-end
			     (nth (1- (length list-to-search))
				  list-to-search)))
		   (< end (semantic-token-start (car list-to-search))))
		  ;; We have modifications to the definition of this parent
		  ;; and not between it's children.  Clear the search list.
		  (setq list-to-search nil)))
	;; Search list is nil.
	))
    ;; If we have a search list, lets go.  Otherwise nothing.
    (while (and list-to-search
		;; Assume always (car (cdr list-to-search)).
		;; A thrown error will be captured nicely, but
		;; that case shouldn't happen.

		;; We end when the start of the CDR is after the
		;; end of our asked change.
		(< (semantic-token-start (car (cdr list-to-search)))
		   end))
      (setq list-to-search (cdr list-to-search)))
    ;; Return it.  If it is nil, there is a logic bug, and we need
    ;; to avoid this bit of logic anyway.
    list-to-search
    ))

(defun semantic-edits-change-over-tokens (change)
  "Return a cache list of tokens surrounding a CHANGE encompassing tokens.
CHANGE must not only include all overlapped tokens (excepting possible
parent tokens) in their entirety.  In this case, the change may be deleting
or moving whole tokens.
The return value is a vector.
Cell 0 is a list is a list of all tokens completely encompassed in change.
Cell 1 is the cons cell into a master parser cache starting with
the cell which occurs BEFORE the first position of CHANGE.
Cell 2 is the parent of cell 1, or nil for the bovine cache.
This function returns nil if any token covered by change is not
completely encompassed.
See `semantic-edits-change-leaf-token' for details on parents."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tokens (nreverse
		  (semantic-find-nonterminal-by-overlay-in-region
		   start end)))
	 (parent nil)
	 (overlapped-tokens nil)
	 inner-start inner-end
	 (list-to-search nil))
    ;; By the time this is already called, we know that it is
    ;; not a leaf change, nor a between token change.  That leaves
    ;; an overlap, and this condition.

    ;; A leaf is always first in this list.
    ;; Is the leaf encompassed in this change?
    (if (and tokens
	     (>= (semantic-token-start (car tokens)) start)
	     (<= (semantic-token-end (car tokens)) end))
	(progn
	  ;; We encompass one whole change.
	  (setq overlapped-tokens (list (car tokens))
		inner-start (semantic-token-start (car tokens))
		inner-end (semantic-token-end (car tokens))
		tokens (cdr tokens))
	  ;; Keep looping while tokens are inside the change.
	  (while (and tokens
		      (>= (semantic-token-start (car tokens)) start)
		      (<= (semantic-token-end (car tokens)) end))

	    ;; Check if this new all-encompassing token is a parent
	    ;; of that which went before.  Only check end because
	    ;; we know that start is less than inner-start since
	    ;; tokens was sorted on that.
	    (if (> (semantic-token-end (car tokens)) inner-end)
		;; This is a parent.  Drop the children found
		;; so far.
		(setq overlapped-tokens (list (car tokens))
		      inner-start (semantic-token-start (car tokens))
		      inner-end (semantic-token-end (car tokens))
		      )
	      ;; It is not a parent encompassing token
	      (setq overlapped-tokens (cons (car tokens)
					    overlapped-tokens)
		    inner-start (semantic-token-start (car tokens))))
	    (setq tokens (cdr tokens)))
	  (if (not tokens)
	      ;; There are no tokens left, and all tokens originally
	      ;; found are encompassed by the change.  Setup our list
	      ;; from the cache
	      (setq list-to-search semantic-toplevel-bovine-cache);; We have a token ouside the list.  Check for
	    ;; We know we have a parent because it would
	    ;; completely cover the change.  A token can only
	    ;; do that if it is a parent after we get here.
	    (when (and tokens
		       (< (semantic-token-start (car tokens)) start)
		       (> (semantic-token-end (car tokens)) end))
	      ;; We have a parent.  Stuff in the search list.
	      (setq parent (car tokens)
		    list-to-search (semantic-nonterminal-children
				    parent t))
	      ;; If the first of TOKENS is a parent (see above)
	      ;; then clear out the list.  All other tokens in
	      ;; here must therefore be parents of the car.
	      (setq tokens nil)
	      ;; One last check,  If start is before the first
	      ;; token or after the last, we may have overlap into
	      ;; the characters that make up the definition of
	      ;; the token we are parsing.
	      (when (or (< start (semantic-token-start
				  (car list-to-search)))
			(> end (semantic-token-end
				(nth (1- (length list-to-search))
				     list-to-search))))
		;; We have a problem
		(setq list-to-search nil
		      parent nil))))

	  (when list-to-search

	    ;; Ok, return the vector only if all TOKENS are
	    ;; confirmed as the lineage of `overlapped-tokens'
	    ;; which must have a value by now.
		
	    ;; Loop over the search list to find the preceeding CDR.
	    ;; Fortunatly, (car overlapped-tokens) happens to be
	    ;; the first token positionally.
	    (let ((tokstart (semantic-token-start (car overlapped-tokens))))
	      (while (and list-to-search
			  ;; Assume always (car (cdr list-to-search)).
			  ;; A thrown error will be captured nicely, but
			  ;; that case shouldn't happen.
			      
			  ;; We end when the start of the CDR is after the
			  ;; end of our asked change.
			  (< (semantic-token-start (car (cdr list-to-search)))
			     tokstart)
			  (setq list-to-search (cdr list-to-search)))))
	    ;; Create the return vector
	    (vector overlapped-tokens
		    list-to-search
		    parent)
	    ))
      nil)))

;;; Incremental Parser
;;
;; Logic about how to group changes for effective reparsing and splicing.

(defsubst semantic-edits-incremental-fail ()
  "When the incremental parser fails, we mark that we need a full reparse."
  ;;(debug)
  (setq semantic-toplevel-bovine-force-reparse t
	semantic-edits-need-reparse nil)
  (message "Force full reparse (%s)." (buffer-name (current-buffer)))
  (run-hooks 'semantic-edits-incremental-reparse-failed-hooks)
  )

(defun semantic-edits-incremental-parser ()
  "Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of the
buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against the
semantic cache to see what needs to be changed."
  (let ((changes (semantic-changes-in-region (point-min) (point-max)))
	(changed-tokens nil))
    (if (not changes)
	;; If we were called, and there are no changes, then we don't
	;; know what to do.  Force a full reparse.
	(semantic-edits-incremental-fail)
      ;; Else, we have some changes.  Loop over them attempting to
      ;; patch things up.
      (while (and changes
		  ;; Stop if we hit a problem.
		  (not (semantic-bovine-toplevel-full-reparse-needed-p
			t)))
	(let ((tokens nil)		;tokens found at changes
	      (newf-tokens nil)		;newfound tokens in change
	      (parse-start nil)		;location to start parsing
	      (parse-end nil)		;location to end parsing
	      (parent-token nil)	;parent of the cache list.
	      (cache-list nil)		;list of children within which
					;we incrementally reparse.
	      (reparse-symbol nil)	;The ruled we start at for reparse.
	      (change-group nil)	;changes grouped in this reparse
	    )
	  ;; Calculate the reparse boundary.
	  (condition-case nil
	      (progn
		
		;; We want to take some set of changes, and group them
		;; together into a small change group. One change forces
		;; a reparse of a larger region (the size of some set of
		;; tokens it encompases.)  It may contain several tokens.
		;; That region may have other changes in it (several small
		;; changes in one function, for example.)
		;; Optimize for the simple cases here, but try to handle
		;; complex ones too.

		(while (and changes	; we still have changes
			    (or (not parse-start)
				;; Below, if the change we are looking
				;; at is not the first change for this
				;; iteration, and it starts before the
				;; end of current parse region, then it
				;; is encompased within the bounds of
				;; tokens modified by the previous iteration's
				;; change.
				(< (semantic-overlay-start (car changes))
				   parse-end)))
		  ;; Store this change in this change group.
		  (setq change-group (cons (car changes) change-group))

		  (cond
		   ;; Is this is a new parse group?
		   ((not parse-start)
		    (let (tmp)
		      (cond

;;;; Are we encompassed all in one token?
		       ((setq tmp (semantic-edits-change-leaf-token (car changes)))
			(setq tokens (list tmp)
			      parse-start (semantic-token-start tmp)
			      parse-end (semantic-token-end tmp)
			      ))

;;;; Did the change occur between some tokens?
		       ((setq cache-list
			      (semantic-edits-change-between-tokens
			       (car changes)))
			;; The CAR of cache-list is the token just before
			;; our change, but wasn't modified.  Hmmm.
			;; Bound our reparse between these two tokens
			(setq tokens nil
			      parent-token
			      (car (semantic-find-nonterminal-by-overlay
				    parse-start)))
			(cond ((> (semantic-token-start (car cache-list))
				  (semantic-overlay-end (car changes)))
			       ;; A change at the beginning of the
			       ;; buffer.
			       (setq parse-start
				     ;; Don't worry about parents since
				     ;; there there would be an exact match
				     ;; in the token list otherwise and
				     ;; the routine would fail.
				     (point-min)
				     parse-end
				     (semantic-token-start (car cache-list))))
			      ((not (car (cdr cache-list)))
			       ;; A change at the end of the buffer.
			       (setq parse-start (semantic-token-end
						  (car cache-list))
				     parse-end (point-max)))
			      (t
			       (setq parse-start
				     (semantic-token-end (car cache-list))
				     parse-end
				     (semantic-token-start (car (cdr cache-list)))
				     ))))

;;;; Did the change completely overlap some number of tokens?
		       ((setq tmp (semantic-edits-change-over-tokens
				   (car changes)))
			;; Extract the information
			(setq tokens (aref tmp 0)
			      cache-list (aref tmp 1)
			      parent-token (aref tmp 2))
			;; We can calculate parse begin/end by
			;; checking out what is in TOKENS.  The one
			;; near start is always first.  Make sure the
			;; reprase includes the `whitespace' around
			;; the snarfed tokens.  Since cache-list
			;; is positioned properly, use it to find that
			;; boundary.
			(if (eq (car tokens) (car cache-list))
			    ;; Beginning of the buffer!
			    (let ((end-marker (nth (length tokens)
						   cache-list)))
			      (setq parse-start (point-min))
			      (if end-marker
				  (setq parse-end
					(semantic-token-start end-marker))
				(setq parse-end (semantic-overlay-end
						 (car changes)))))
			  ;; Middle of the buffer.
			  (setq parse-start
				(semantic-token-end (car cache-list)))
			  ;; For the end, we need to scoot down some
			  ;; number of tokens.  We 1+ the length
			  ;; of tokens because we want to skip the first
			  ;; token (remove 1-) then want the token after
			  ;; the end of the list (1+)
			  (let ((end-marker (nth (1+ (length tokens)) cache-list)))
			    (if end-marker
				(setq parse-end (semantic-token-start end-marker))
			      ;; No marker.  It is the last token in our
			      ;; list of tokens.  Only possible if END
			      ;; already matches the end of that token.
			      (setq parse-end
				    (semantic-overlay-end (car changes)))))
			  ))

;;;; Unhandled case.
		       ;; Throw error, and force full reparse.
		       (t
			(message "Unhandled change group.")
			(error nil)))
		      ))
		   ;; Is this change inside the previous parse group?
		   ;; We already checked start.
		   ((< (semantic-overlay-end (car changes)) parse-end)
		    nil)
		   (t
		    ;; This change extends the current parse group.
		    ;; Find any new tokens, and see how to append
		    ;; them.
		    (message "Unhandled secondary change overlapping boundary.")
		    (error nil)
		    ))
		  ;; Prepare for the next iteration.
		  (setq changes (cdr changes))))
	      (error (semantic-edits-incremental-fail)))
	  ;; Ok, after all that, are we still ok?
	  (unless (semantic-bovine-toplevel-full-reparse-needed-p t)
	    ;; By the time we get here, all TOKENS are children of
	    ;; some parent.  They should all have the same start symbol
	    ;; since that is how the multi-token parser works.  Grab
	    ;; the reparse symbol from the first of the returned tokens.
	    (setq reparse-symbol (semantic-token-get
                                  (car tokens) 'reparse-symbol))
	    ;; Find a parent if not provided.
	    (when (and (not parent-token) tokens)
	      (setq parent (semantic-find-nonterminal-parent-by-overlay
			    (car tokens))))
	    ;; We can do the same trick for our parent and resulting
	    ;; cache list.
	    (unless cache-list
	      (setq cache-list
		    ;; We need to get all children in case we happen
		    ;; to have a mix of positioned and non-positioned
		    ;; children.
		    (semantic-nonterminal-children parent-token nil)))
	    ;; Use the boundary to calculate the new tokens found.
	    (setq newf-tokens (semantic-edits-bovinate-region
			       parse-start parse-end reparse-symbol))
	    ;; Make sure all these tokens are given overlays.
	    ;; They have already been cooked by the bovinator
	    ;; and just need the overlays.
	    (let ((tmp newf-tokens))
	      (while tmp
		(semantic-overlay-token (car tmp))
		(setq tmp (cdr tmp))))
	    ;; See how this change lays out.
	    (cond

;;;; Whitespace change
	     ((and (not tokens) (not newf-tokens))
	      ;; A change that occured outside of any existing tokens
	      ;; and there are no new tokens to replace it.
	      (message "White space changes.")
	      nil
	      )

;;;; New tokens in old whitespace area.
	     ((and (not tokens) newf-tokens)
	      ;; A change occured outside existing tokens which added
	      ;; a new token.  We need to splice these tokens back
	      ;; into the cache at the right place.
	      (semantic-edits-splice-insert newf-tokens parent-token cache-list)

	      (setq changed-tokens
		    (append newf-tokens changed-tokens))

	      (message "Inserted tokens: (%s)"
		       (semantic-name-nonterminal (car newf-tokens)))
	      )

;;;; Old tokens removed
	     ((and tokens (not newf-tokens))
	      ;; A change occured where pre-existing tokens were deleted!
	      ;; Remove the token from the cache.
	      (semantic-edits-splice-remove tokens parent-token cache-list)

	      (setq changed-tokens
		    (append tokens changed-tokens))

	      (message "Deleted tokens: (%s)"
		       (semantic-name-nonterminal (car tokens)))
	      )
;;;; One token was updated.
	     ((and (= (length tokens) 1) (= (length newf-tokens) 1))
	      ;; One old token was modified, and it is replaced by
	      ;; One newfound token.  Splice the new token into the
	      ;; position of the old token.
	      ;; Do the splice.
	      (semantic-edits-splice-replace (car tokens) (car newf-tokens))
	      ;; Add this token to our list of changed toksns
	      (setq changed-tokens (cons (car tokens) changed-tokens))
	      ;; Debug
	      (message "Rebovinated: %s" (semantic-name-nonterminal (car tokens)) nil t)
	      ;; Flush change regardless of above if statement.
	      )


	     (t
	      ;; Some unhandled case.
	      (semantic-edits-incremental-fail)))
	    (unless (semantic-bovine-toplevel-full-reparse-needed-p t)
	      ;; We got this far, and we didn't flag a full reparse.ff
	      ;; Clear out this change group.
	      (while change-group
		(semantic-edits-flush-change (car change-group))
		(setq change-group (cdr change-group))))))
	;; Don't increment change here because an earlier loop
	;; created change-groups.
	))
    ;; Mark that we are done with this glop
    (setq semantic-edits-need-reparse nil)
    ;; Return the list of tokens that changed.  The caller will
    ;; use this information to call hooks which can fix themselves.
    changed-tokens))

;;; Cache Splicing
;;
;; The incremental parser depends on the ability to parse up sections
;; of the file, and splice the results back into the cache.  There are
;; three types of splices.  A REPLACE, an ADD, and a REMOVE.  REPLACE
;; is one of the simpler cases, as the starting cons cell representing
;; the old token can be used to auto-splice in.  ADD and REMOVE
;; require scanning the cache to find the correct location so that the
;; list can be fiddled.
(defun semantic-edits-splice-remove (oldtokens parent cachelist)
  "Remove OLDTOKENS from PARENT's CACHELIST.
OLDTOKENS are tokens in the currenet buffer, preferably linked
together also in CACHELIST.
PARENT is the parent token containing OLDTOKENS.
CACHELIST should be the children from PARENT, but may be
pre-positioned to a convenient location."
  (let* ((first (car oldtokens))
	 (last (nth (1- (length oldtokens)) oldtokens))
	 (chil (if parent
		   (semantic-nonterminal-children parent t)
		 semantic-toplevel-bovine-cache))
	 (cachestart cachelist)
	 (cacheend nil)
	 (tmp oldtokens)
	 )
    ;; First in child list?
    (if (eq first (car chil))
	;; First tokens in the cache are being deleted.
	(progn
	  ;; Find the last token
	  (setq cacheend chil)
	  (while (and chil (not (eq last (car cacheend))))
	    (setq cacheend (cdr cacheend)))
	  ;; Splice the found end token into the cons cell
	  ;; owned by the current top child.
	  (setcar chil (car (cdr cacheend)))
	  (setcdr chil (cdr (cdr cacheend)))
	  )
      ;; Find in the cache the preceeding tokenn
      (while (and cachestart (not (eq first (car (cdr cachestart)))))
	(setq cachestart (cdr cachestart)))
      ;; Find the last token
      (setq cacheend cachestart)
      (while (and cacheend (not (eq last (car cacheend))))
	(setq cacheend (cdr cacheend)))
      ;; Splice the end position into the start position.
      (setcdr cachestart (cdr cacheend))
      )
    ;; Remove old overlays of these deleted tokens
    (while oldtokens
      (semantic-deoverlay-token (car oldtokens))
      (setq oldtokens (cdr oldtokens)))
    ))

(defun semantic-edits-splice-insert (newtokens parent cachelist)
  "Insert NEWTOKENS into PARENT using CACHELIST.
PARENT could be nil, in which case CACHLIST is the bovine cache
which must be updated.
CACHELIST must be searched to find where NEWTOKENS are to be inserted.
The positions of NEWTOKENS must be synchronized with those in
CACHELIST for this to work.  Some routines pre-position CACHLIST at a
convenient location, so use that."
  (let* ((start (semantic-token-start (car newtokens)))
	 (newtokenendcell (nthcdr (1- (length newtokens)) newtokens))
	 (end (semantic-token-end (car newtokenendcell)))
	 )
    (if (> (semantic-token-start (car cachelist)) start)
	;; We are at the beginning.
	(let* ((pc (if parent
		       (semantic-nonterminal-children parent)
		     semantic-toplevel-bovine-cache))
	       (nc (cons (car pc) (cdr pc)))  ; new cons cell.
	       )
	  ;; Splice the new cache cons cell onto the end of our list.
	  (setcdr newtokenendcell nc)
	  ;; Set our list into parent.
	  (setcar pc (car newtokens))
	  (setcdr pc (cdr newtokens)))
      ;; We are at the end, or in the middle.  Find our match first.
      (while (and (cdr cachelist)
		  (> end (semantic-token-start (car (cdr cachelist)))))
	(setq cachelist (cdr cachelist)))
      ;; Now splice into the list!
      (setcdr newtokenendcell (cdr cachelist))
      (setcdr cachelist newtokens))))

(defun semantic-edits-splice-replace (oldtoken newtoken)
  "Replace OLDTOKEN with NEWTOKEN in the current cache.
Do this by recycling OLDTOKEN's first CONS cell.  This effectivly
causes the new token to completely replace the old one.
Make sure that all information in the overlay is transferred.
It is presumed that OLDTOKEN and NEWTOKEN are both cooked.
When this routine returns, OLDTOKEN is raw, and the data will be
lost if not transferred into NEWTOKEN."
  (let* ((oo (semantic-token-overlay oldtoken))
	 (o (semantic-token-overlay newtoken))
	 (oo-props (semantic-overlay-properties oo)))
    (while oo-props
      (semantic-overlay-put o (car oo-props) (car (cdr oo-props)))
      (setq oo-props (cdr (cdr oo-props)))
      )
    ;; Free the old overlay(s)
    (semantic-deoverlay-token oldtoken)
    ;; Recover properties
    (let ((p (semantic-token-properties oldtoken)))
      (while p
	(semantic-token-put newtoken (car (car p)) (cdr (car p)))
	(setq p (cdr p))))
    ;; The below line was from the old rebovinate routine.  Now
    ;; I'm not sure it's needed.  Lets check.
    ;;(semantic-token-put newtoken 'reparse-symbol nonterminal)
    ;; Splice into the main list.
    (setcdr oldtoken (cdr newtoken))
    (setcar oldtoken (car newtoken))
    ;; This important bit is because the CONS cell representing
    ;; OLDTOKEN is now pointing to NEWTOKEN, but the NEWTOKEN
    ;; cell is about to be abandoned.  Here we update our overlay
    ;; to point at the updated state of the world.
    (semantic-overlay-put o 'semantic oldtoken)
    ))

;;; OLD CODE
;;
;; The following semantic 1.x code handles reparses when changes to a buffer
;; are restricted withing the bounds of a single token.  Keep this around
;; till the new experimental incremental parse manager is completed.

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


;;; Reparse one token.
;;
;; Use this when a set of changes modifies a single token.
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
	    (semantic-edits-incremental-fail))))
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

(defun semantic-cleanup-dirty-tokens ()
  "Reparse tokens which are marked as dirty."
  (let ((changes (semantic-remove-dirty-children)))
    ;; We have a cache, and some dirty tokens
    (let ((semantic-bovination-working-type 'dynamic))
      (working-status-forms
	  (semantic-bovination-working-message (buffer-name))
	  "done"
	(while (and semantic-dirty-tokens
		    (not (semantic-bovine-toplevel-full-reparse-needed-p t)))
	  (semantic-rebovinate-token (car semantic-dirty-tokens))
	  (setq semantic-dirty-tokens (cdr semantic-dirty-tokens))
	  (working-dynamic-status))
	(working-dynamic-status t))
      (setq semantic-dirty-tokens nil))
    changes))


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

;;; OLD AND BROKEN CODE
;;
;; This code was the beginning of a new incremental reparse manager, but I
;; never got it working right.  Keep it around till I know for sure it is
;; useless.

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
