;;; semanticdb-typecache.el --- Manage Datatypes

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-typecache.el,v 1.17 2008/02/07 22:51:38 zappo Exp $

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Manage a datatype cache.
;;
;; For typed languages like C++ collect all known types from various
;; headers, merge namespaces, and expunge duplicates.
;;
;; It is likely this feature will only be needed for C/C++.

(require 'semanticdb)

;;; Code:
(defclass semanticdb-typecache ()
  ((filestream :initform nil
	       :documentation
	       "Fully sorted/merged list of tags within this buffer.")
   (includestream :initform nil
		  :documentation
		  "Fully sorted/merged list of tags from this file's includes list.")
   (stream :initform nil
	   :documentation
	   "The searchable tag stream for this cache.
NOTE: Can I get rid of this?  Use a hashtable instead?")
   ;; @todo - add some sort of fast-hash.
   )
  "Structure for maintaining a typecache.")

(defmethod semantic-reset ((tc semanticdb-typecache))
  "Reset the object IDX."
  (oset tc filestream nil)
  (oset tc includestream nil)

  (oset tc stream nil)
  )

(defmethod semanticdb-typecache-notify-reset ((tc semanticdb-typecache))
  "Do a reset from a notify from a table we depend on."
  (oset tc includestream nil)
  )

(defmethod semanticdb-partial-synchronize ((tc semanticdb-typecache)
					   new-tags)
  "Reset the typecache based on a pratial reparse."
  (when (semantic-find-tags-by-class 'include new-tags)
    (oset tc includestream nil)
    )

  (when (semantic-find-tags-by-class 'type new-tags)
    ;; Reset our index
    (oset tc filestream nil)
    t ;; Return true, our core file tags have changed in a relavant way.
    )

  ;; NO CODE HERE
  )

(defun semanticdb-typecache-length(thing)
  "How long is THING?
Debugging function."
  (cond ((semanticdb-typecache-child-p thing)
	 (length (oref thing stream)))
	((semantic-tag-p thing)
	 (length (semantic-tag-type-members thing)))
	((and (listp thing) (semantic-tag-p (car thing)))
	 (length thing))
	((null thing)
	 0)
	(t -1)	))


;;;###autoload
(defmethod semanticdb-get-typecache ((table semanticdb-abstract-table))
  "Retrieve the typecache from the semanticdb TABLE.
If there is no table, create one, and fill it in."
  (semanticdb-refresh-table table)
  (let* ((idx (semanticdb-get-table-index table))	 
	 (cache (oref idx type-cache))
	 )
    
    ;; Make sure we have a cache object in the DB index.
    (when (not cache)
      ;; The object won't change as we fill it with stuff.
      (setq cache (semanticdb-typecache (semanticdb-full-filename table)))
      (oset idx type-cache cache))

    cache))

(defmethod semanticdb-have-typecache-p ((table semanticdb-abstract-table))
  "Return non-nil (the typecache) if TABLE has a pre-calculated typecache."
  (let* ((idx (semanticdb-get-table-index table)))
    (oref idx type-cache)))

;;; MERGING
;;
;; Managing long streams of tags representing data types.
;;
(defun semanticdb-typecache-apply-filename (file stream)
  "Apply the filename FILE to all tags in STREAM."
  (let ((new nil))
    (while stream
      (setq new (cons (semantic-tag-copy (car stream) nil file)
		      new))
      ;;(semantic--tag-put-property (car stream) :filename file)
      (setq stream (cdr stream)))
    (nreverse new)))


(defsubst semanticdb-typecache-safe-tag-members (tag)
  "Return a list of members for TAG that are safe to permute."
  (let ((mem (semantic-tag-type-members tag))
	(fname (semantic-tag-file-name tag)))
    (if fname
	(setq mem (semanticdb-typecache-apply-filename fname mem))
      (copy-sequence mem))))

;; @todo - This should go into semantic-sort.el
;;;###autoload
(defun semanticdb-typecache-merge-streams (cache1 cache2)
  "Merge into CACHE1 and CACHE2 together.  The Caches will be merged in place."
  ;; Assume we always have datatypes, as this typecache isn'nt really
  ;; useful without a typed language.
  (let ((S (semantic-sort-tags-by-name-then-type-increasing
	    ;; I used to use append, but it copied cache1 but not cache2.
	    ;; Since sort was permuting cache2, I already had to make sure
	    ;; the caches were permute-safe.  Might as well use nconc here.
	    (nconc cache1 cache2)))
	(ans nil)
	(next nil)
	(prev nil)
	(type nil))
    ;; With all the tags in order, we can loop over them, and when
    ;; two have the same name, we can either throw one away, or construct
    ;; a fresh new tag merging the items together.
    (while S
      (setq prev (car ans))
      (setq next (car S))
      (if (or
	   ;; CASE 1 - First item
	   (null prev)
	   ;; CASE 2 - New name
	   (not (string= (semantic-tag-name next)
			 (semantic-tag-name prev))))
	  (setq ans (cons next ans))
	;; ELSE - We have a NAME match.
	(setq type (semantic-tag-type next))
	(if (semantic-tag-of-type-p prev type) ; Are they the same datatype
	    ;; Same Class, we can do a merge.
	    (cond
	     ((and (semantic-tag-of-class-p next 'type)
		   (string= type "namespace"))
	      ;; Namespaces - merge the children together.
	      (setcar ans
		      (semantic-tag-new-type
		       (semantic-tag-name prev) ; - they are the same
		       "namespace"	; - we know this as fact
		       (semanticdb-typecache-merge-streams
			(semanticdb-typecache-safe-tag-members prev)
			(semanticdb-typecache-safe-tag-members next))
		       nil		; - no attributes
		       ))
	      ;; Make sure we mark this as a fake tag.
	      (semantic-tag-set-faux (car ans))
	      )
	     ((semantic-tag-prototype-p next)
	      ;; NEXT is a prototype... so keep previous.
	      nil			; - keep prev, do nothing
	      )
	     ((semantic-tag-prototype-p prev)
	      ;; PREV is a prototype, but not next.. so keep NEXT.
	      ;; setcar - set by side-effect on top of prev
	      (setcar ans next)
	      )
	     (t
	      ;; @todo - comment out this debug statement.
	      ;(message "Don't know how to merge %s.  Keeping first entry." (semantic-tag-name next))
	     ))
	  ;; Not same class... but same name
	  ;(message "Same name, different type: %s, %s!=%s"
	  ;	   (semantic-tag-name next)
	  ;	   (semantic-tag-type next)
	  ;        (semantic-tag-type prev))
	  (setq ans (cons next ans))
	  ))
      (setq S (cdr S)))
    (nreverse ans)))

;;; Refresh / Query API
;;
;; Queries that can be made for the typecache.
(defmethod semanticdb-typecache-file-tags ((table semanticdb-table))
  "Update the typecache for TABLE, and return the file-tags.
File-tags are those that belong to this file only, and excludes
all included files."
  (let* ((idx (semanticdb-get-table-index table))	 
	 (cache (semanticdb-get-typecache table))
	 )

    ;; Make sure our file-tags list is up to date.
    (when (not (oref cache filestream))
      (oset cache filestream (semanticdb-typecache-merge-streams (semantic-find-tags-by-class 'type table)
								 nil)))

    ;; Return our cache.
    (oref cache filestream)
    ))

(defmethod semanticdb-typecache-include-tags ((table semanticdb-table))
  "Update the typecache for TABLE, and return the merged types from the include tags.
Include-tags are the tags brought in via includes, all merged together into
a master list."
  (let* ((idx (semanticdb-get-table-index table))	 
	 (cache (semanticdb-get-typecache table))
	 )

    ;; Make sure our file-tags list is up to date.
    (when (not (oref cache includestream))
      (let ((incstream nil)
	    (incpath (semanticdb-find-translate-path table nil))
	    )
	;; Get the translated path, and extract all the type tags, then merge
	;; them all together.
	(dolist (i incpath)
	  ;; don't include ourselves in this crazy list.
	  (when (and i (not (eq i table)))
	    (setq incstream
		  (semanticdb-typecache-merge-streams
		   incstream
		   ;; Getting the cache from this table will also cause this
		   ;; file to update it's cache from it's decendants.
		   ;;
		   ;; In theory, caches are only built for most includes
		   ;; only once (in the loop before this one), so this ends
		   ;; up being super fast as we edit our file.
		   (copy-sequence
		    (semanticdb-typecache-file-tags i))))
	    ))
	  
	;; Save...
	(oset cache includestream incstream)))

    ;; Return our cache.
    (oref cache includestream)
    ))


;;; Search Routines
;;
;;;###autoload
(define-overload semanticdb-typecache-find (type &optional path find-file-match)
  "Search the typecache for TYPE in PATH.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
PATH can be nil for the current buffer, or a semanticdb table.")

(defun semanticdb-typecache-find-default (type &optional path find-file-match)
  "Default implementation of `semanticdb-typecache-find'.
TYPE is the datatype to find.
PATH is the search path.. which should be one table object.
If FIND-FILE-MATCH is non-nil, then force the file belonging to the
found tag to be loaded.  NOTE: Not Impl'd yet
Call directly to `semanticdb-typecache-find-method'."
  (semanticdb-typecache-find-method (or path semanticdb-current-table)
				    type find-file-match))
    
(defmethod semanticdb-typecache-find-method ((table semanticdb-abstract-table)
					     type find-file-match)
  "Search the typecache in TABLE for the datatype TYPE.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
If FIND-FILE-MATCH is non-nil, then force the file belonging to the
found tag to be loaded."
  ;; convert string to a list.
  (when (stringp type) (setq type (semantic-analyze-split-name type)))
  (when (stringp type) (setq type (list type)))

  ;; Search for the list in our typecache.
  (let* ((file (semanticdb-typecache-file-tags table))
	 (inc (semanticdb-typecache-include-tags table))
	 (stream nil)
	 (f-ans nil)
	 (i-ans nil)
	 (ans nil)
	 (notdone t)
	 (lastfile nil)
	 (thisfile nil)
	 )
    ;; 1) Find first symbol in the two master lists and then merge
    ;;    the found streams.

    ;; We stripped duplicates, so these will be super-fast!
    (setq f-ans (semantic-find-first-tag-by-name (car type) file))
    (setq i-ans (semantic-find-first-tag-by-name (car type) inc))
    (if (and f-ans i-ans)
	(progn
	  ;; This trick merges the two identified tags, making sure our lists are
	  ;; complete.  The second find then gets the new 'master' from the list of 2.
	  (setq ans (semanticdb-typecache-merge-streams (list f-ans) (list i-ans)))
	  (setq ans (semantic-find-first-tag-by-name (car type) ans)))

      ;; The answers are already sorted and merged, so if one misses,
      ;; no need to do any special work.
      (setq ans (or f-ans i-ans)))

    ;; 2) Loop over the remaining parts.
    (while (and type notdone)

      ;; For pass > 1, stream will be non-nil, so do a search, otherwise
      ;; ans is from outside the loop.
      (when stream
	(setq ans (semantic-find-first-tag-by-name (car type) stream)))

      ;; NOTE: The below test to make sure we get a type is only relevant
      ;;       for the SECOND pass or later.  The first pass can only ever
      ;;       find a type/namespace because everything else is excluded.

      ;; If this is not the last entry from the list, then it
      ;; must be a type or a namespace.  Lets double check.
      (when (and stream 
		 (null (cdr type))
		 (not (semantic-tag-of-class-p ans 'type)))

	(setq ans (semantic-find-tags-by-class 'type stream))
	;; Same basic query as above with non types stripped out.
	(setq ans (semantic-find-first-tag-by-name (car type) ans)))

      ;; Track most recent file.
      (setq thisfile (semantic-tag-file-name ans))
      (when thisfile (setq lastfile thisfile))

      ;; If we have a miss, exit, otherwise, update the stream to
      ;; the next set of members.
      (if (not ans)
	  (setq notdone nil)
	(setq stream (semantic-tag-type-members ans)))

      (setq type (cdr type)))

    (when (and find-file-match lastfile)
      ;; This won't liven up the tag since we have a copy, but
      ;; we ought to be able to get there and go to the right line.
      (find-file-noselect lastfile))
    
    ans))

;;; DEBUG
;;
;;;###autoload
(defun semanticdb-typecache-dump ()
  "Dump the typecache for the current buffer."
  (interactive)
  (let* ((tab semanticdb-current-table)
	 (idx (semanticdb-get-table-index tab))
	 (junk (oset idx type-cache nil)) ;; flush!
	 (start (current-time))
	 (tc (semanticdb-get-typecache tab))
	 (end (current-time))
	 (ab (semantic-adebug-new-buffer "*TypeCache ADEBUG*"))
	 )
    
    (message "Calculating Cache took %.2f seconds."
	     (semantic-elapsed-time start end))

    (semantic-adebug-insert-thing tc "]" "")

    ))


(provide 'semanticdb-typecache)
;;; semanticdb-typecache.el ends here
