;;; semanticdb-typecache.el --- Manage Datatypes

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-typecache.el,v 1.3 2007/08/29 19:50:47 zappo Exp $

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
  ((stream :initform nil
	   :documentation
	   "The searchable tag stream for this cache.")
   ;; @todo - add some sort of fast-hash.
   )
  "Structure for maintaining a typecache.")

;;;###autoload
(defmethod semanticdb-get-typecache ((table semanticdb-abstract-table))
  "Retrieve the typecache from the semanticdb TABLE.
If there is no table, create one."
  (let* ((idx (semanticdb-get-table-index table))
	 (cache (oref idx type-cache)))
    (if cache
	cache
      (semanticdb-typecache-update table))
    ))

(defmethod semanticdb-have-typecache-p ((table semanticdb-abstract-table))
  "Return non-nil (the typecache) if TABLE has a pre-calculated typecache."
  (let* ((idx (semanticdb-get-table-index table)))
    (oref idx type-cache)))

(defun semanticdb-typecache-merge-streams (cache1 cache2)
  "Merge into CACHE1 and CACHE2 together."
  (let ((S (sort (append cache1 cache2)
		 (lambda (a b)
		   (string< (semantic-tag-name a)
			    (semantic-tag-name b)))))
	(ans nil)
	(next nil)
	(prev nil)
	(type nil))
    ;; With all the tags in order, we can loop over them, and when
    ;; two have the same name, we can either throw one away, or construct
    ;; a fresh new tag merging the items together.
    (while S
      (setq prev next)
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
	(if (equal type (semantic-tag-type prev))
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
			(semantic-tag-type-members prev)
			(semantic-tag-type-members next))
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
	      (message "Don't know how to merge %s.  Keeping first entry." (semantic-tag-name next)))
	     )
	    ;; Not same class... but same type
	    (message "Same name, different type: %s, %s!=%s"
		     (semantic-tag-name next)
		     (semantic-tag-type next)
		     (semantic-tag-type prev))))
      (setq S (cdr S)))
    (nreverse ans)))

(defmethod semanticdb-typecache-merge ((dest semanticdb-typecache) add)
  "Add into the cache DEST all entries from ADD.
If ADD is a tag stream, add them.
If ADD is another typecache object, Merge it in."
  (let* ((stream1 (oref dest stream))
	 (stream2 (if (semanticdb-typecache-child-p add)
		      (oref add stream)
		    add)))
    (oset dest stream
	  (semanticdb-typecache-merge-streams stream1 stream2))))

(defun semanticdb-stream-to-typecache (file stream)
  "For FILE, convert a tag stream STREAM into a typecache.
Argument FILE is the name of the created typecache.
STREAM is the list of tags."
  (let ((tc (semanticdb-typecache file)))
    (semanticdb-typecache-merge
     tc (semantic-find-tags-by-class 'type stream))
     tc))

(defmethod semanticdb-typecache-update ((table semanticdb-abstract-table))
  "Update the typecache for TABLE."
;  (message "Typecache (Abst) Update for: %s"
;	   (object-name table))
  (let ((idx (semanticdb-get-table-index table))
	(cache (semanticdb-typecache "empty-cache")))
    ;; The object won't change as we fill it with stuff.
    (oset idx type-cache cache)
    ))

(defvar semanticdb-typecache-recursion-flag nil
  "Use this to avoid recursion in the typecache update.")

(defmethod semanticdb-typecache-update ((table semanticdb-table))
  "Update the typecache for TABLE.
A typecache keeps all type information that is findable through semanticdb.
Each database table has a cache, and the cache for the current file will
combine the caches of all files included within itself."
;  (message "Typecache (File) Update for: %s"
;	   (object-name table))
  (let* ((idx (semanticdb-get-table-index table))
	 (local-list (semantic-find-tags-by-class 'type table))
	 (local-inc (semantic-find-tags-by-class 'include table))
	 (cache (semanticdb-stream-to-typecache 
		 (file-name-nondirectory (semanticdb-full-filename table))
		 local-list))
	 (incpath (reverse (semanticdb-find-translate-path table nil)))
	 )
    ;; The object won't change as we fill it with stuff.
    (oset idx type-cache cache)

    (when (not semanticdb-typecache-recursion-flag)
      ;; To avoid recursion problems, we need to go through our path
      ;; iteratively and force a cache refresh on each.
      ;; Go through the path backwards since the leave includes are at the
      ;; end.
      (while incpath
	(when (not (eq (car incpath) table))
	  (semanticdb-get-typecache (car incpath)))
	(setq incpath (cdr incpath)))
      )

    (let ((semanticdb-typecache-recursion-flag t))
      ;; Now loop over our local include path, and perge those caches with
      ;; our own.
      (while local-inc
	(let ((inc-tab (semanticdb-find-table-for-include (car local-inc)))
	      )
	  (when inc-tab
	    (semanticdb-typecache-merge 
	     cache
	     ;; Getting the cache from this table will also cause this
	     ;; file to update it's cache from it's decendants.
	     ;;
	     ;; In theory, caches are only built for most includes
	     ;; only once (in the loop before this one), so this ends
	     ;; up being super fast as we edit our file.
	     (semanticdb-get-typecache inc-tab)))
	  )
	(setq local-inc (cdr local-inc))))
    cache))

;;; DEBUG
;;
;;;###autoload
(defun semanticdb-typecache-dump ()
  "Dump the typecache for the current buffer."
  (interactive)
  (semantic-fetch-tags)
  (let* ((tab semanticdb-current-table)
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
