;;; semanticdb-typecache.el --- $^$

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-typecache.el,v 1.1 2007/08/29 11:42:08 zappo Exp $

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

(defun semanticdb-typecache-merge (cache1 cache2)
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
	     ((string= type "namespace")
	      ;; Namespaces - merge the children together.
	      (setcar ans
		      (semantic-tag-new-type
		       (semantic-tag-name prev) ; - they are the same
		       "namespace" ; - we know this as fact
		       (semanticdb-typecache-merge ; This is the member list
			(semantic-tag-type-members prev)
			(semantic-tag-type-members next))
		       nil ; - no attributes
		       ))
	      ;; Make sure we mark this as a fake tag.
	      (semantic-tag-set-faux (car ans))
	      )
	     ((semantic-tag-prototype-p next)
	      ;; NEXT is a prototype... so keep previous.
	      nil ; - keep prev, do nothing
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

(defun semanticdb-stream-to-typecache (stream)
  "Convert a tag stream STREAM into a typecache."
  (semanticdb-typecache-merge
   (semantic-find-tags-by-class 'type stream)
   nil))

(defmethod semanticdb-typecache-update ((table semanticdb-abstract-table))
  "Update the typecache for TABLE.
A typecache keeps all type information that is findable through semanticdb.
Each database table has a cache, and the cache for the current file will
combine the caches of all files included within itself."
  (let* ((idx (semanticdb-get-table-index table))
	 (local-list (semantic-find-tags-by-class 'type table))
	 (local-inc (semantic-find-tags-by-class 'include table))
	 (cache (semanticdb-stream-to-typecache local-list))
	 )
    (while local-inc
      (let ((inc-tab (semanticdb-find-table-for-include (car local-inc)))
	    )
	(when inc-tab
	  (setq 
	   cache 
	   (semanticdb-typecache-merge 
	    cache
	    ;; Getting the cache from this table will also cause this
	    ;; file to update it's cache from it's decendants.
	    ;; Hopefully we aren't too recursive here.
	    ;;
	    ;; In theory, caches are only built for most includes only
	    ;; once, so this ends up being super fast as we edit our
	    ;; file.
	    (semanticdb-get-typecache inc-tab))))
	)
      (setq local-inc (cdr local-inc)))

    (oset idx type-cache cache)
    cache))

;;; DEBUG
;;
;;;###autoload
(defun semanticdb-typecache-dump ()
  "Dump the typecache for the current buffer."
  (interactive)
  (let* ((tab semanticdb-current-table)
	 (start (current-time))
	 (tc (semanticdb-get-typecache tab))
	 (end (current-time))
	 (ab (semantic-adebug-new-buffer "*TypeCache ADEBUG*"))
	 )
    
    (message "Calculating Cache took %.2f seconds."
	     (semantic-elapsed-time start end))

    (semantic-adebug-insert-tag-list tc "* ")

    ))


(provide 'semanticdb-typecache)
;;; semanticdb-typecache.el ends here
