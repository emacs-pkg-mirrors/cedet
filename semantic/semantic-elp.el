;;; semantic-elp.el --- Bind ELP to measure Semantic

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-elp.el,v 1.4 2008/02/08 20:48:45 zappo Exp $

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
;; Provide fast ways to profile various (often slow) Semantic processes.

(require 'elp)
(require 'semantic-tag-ls)
(require 'semantic-tag-file)
(require 'semanticdb)
(require 'semanticdb-find)
(require 'semanticdb-typecache)
(require 'semantic-scope)
(require 'semantic-analyze-fcn)
(require 'semantic-analyze)
(require 'semantic-analyze-complete)

;;; Code:
(defvar semantic-elp-emacs-core-list
  '(
    append
    copy-sequence
    expand-file-name
    file-exists-p
    file-name-directory
    file-name-nondirectory
    length
    nconc
    nreverse
    sort
    string<
    string=
    )
  "List of Emacs functions for profiling.")

(defvar semantic-elp-eieio-core-list
  '(
    eieio-oref
    eieio-oset
    obj-of-class-p
    )
  "List of EIEIO functions for profiling.")

(defvar semantic-elp-semantic-core-list
  '(
    semantic-ctxt-current-argument
    semantic-ctxt-current-assignment
    semantic-ctxt-current-class-list
    semantic-ctxt-current-function
    semantic-ctxt-current-symbol-and-bounds
    semantic-current-tag
    semantic-dependency-tag-file
    semantic-equivalent-tag-p
    semantic-fetch-tags
    semantic-fetch-tags-fast
    semantic-find-tag-by-overlay
    semantic-sort-tags-by-name-decreasing
    semantic-sort-tags-by-name-increasing
    semantic-sort-tags-by-name-then-type-increasing
    semantic-sort-tags-by-type-decreasing
    semantic-sort-tags-by-type-increasing
    semantic-tag-clone
    semantic-tag-components
    semantic-tag-copy
    semantic-tag-external-member-children
    semantic-tag-file-name
    semantic-tag-function-arguments
    semantic-tag-function-parent
    semantic-tag-get-attribute
    semantic-tag-in-buffer-p
    semantic-tag-include-filename
    semantic-tag-lessp-name-then-type
    semantic-tag-name
    semantic-tag-new-type
    semantic-tag-of-class-p
    semantic-tag-of-type-p
    semantic-tag-of-type-p
    semantic-tag-p
    semantic-tag-prototype-p
    semantic-tag-set-faux
    semantic-tag-type
    semantic-tag-type-members
    semantic-tag-type-superclasses
    semantic-tag-with-position-p
    )
  "List of core Semantic functions for profiling.")
(defvar semantic-elp-semantic-find-core-list
  '(
    semantic-find-tags-by-class
    semantic-find-tags-by-name
    semantic-find-tags-by-name-regexp
    semantic-find-tags-by-scope-protection
    semantic-find-tags-by-type
    semantic-find-tags-for-completion
    semantic-find-tags-included
    semantic-find-tags-of-compound-type
    )
  "List of semantic-find routines for profiling.")

(defvar semantic-elp-semanticdb-core-list
  '(
    semanticdb-cache-get
    semanticdb-current-database-list
    semanticdb-file-table
    semanticdb-full-filename
    semanticdb-get-buffer
    semanticdb-get-table-index
    semanticdb-refresh-references
    semanticdb-refresh-table
    )
  "List of core Semanticdb functions for profiling.")

(defvar semantic-elp-include-path-list
  '(
    semanticdb-find-incomplete-cache-entries-p
    semanticdb-find-load-unloaded
    semanticdb-find-table-for-include
    semanticdb-find-throttle-active-p
    semanticdb-find-translate-path-default
    semanticdb-find-translate-path-brutish-default
    semanticdb-find-translate-path-includes--internal
    semanticdb-find-translate-path-includes-default
    )
  "List of include path calculation functions for profiling.")

(defvar semantic-elp-semanticdb-find-list
  '(
    semanticdb-find-results-p
    semanticdb-find-tags-by-class
    semanticdb-find-tags-by-name
    semanticdb-find-tags-by-name-regexp
    semanticdb-find-tags-collector
    semanticdb-find-tags-external-children-of-type
    semanticdb-find-tags-for-completion
    semanticdb-strip-find-results
    )
  "List of semanticdb find functions to profile.
You may also need `semantic-elp-include-path-list'.")

(defun semantic-elp-core-enable ()
  "Do an ELP reset, and enable profiling of the core system."
  (elp-reset-all)
  (elp-instrument-list semantic-elp-emacs-core-list)
  (elp-instrument-list semantic-elp-eieio-core-list)
  (elp-instrument-list semantic-elp-semantic-core-list)
  (elp-instrument-list semantic-elp-semanticdb-core-list)
  (elp-instrument-list semantic-elp-semanticdb-find-list)
  (elp-instrument-list semantic-elp-include-path-list)
  )


(defun semantic-elp-include-path-enable ()
  "Enable profiling for `semanticdb-find-translate-path'."
  (semantic-elp-core-enable)
  (elp-set-master 'semanticdb-find-translate-path-default)
  )

(defvar semantic-elp-typecache-list
  '(
    semantic-analyze-split-name
    semanticdb-get-typecache
    semanticdb-typecache-merge-streams
    semanticdb-typecache-safe-tag-members
    semanticdb-typecache-apply-filename
    semanticdb-typecache-file-tags
    semanticdb-typecache-include-tags
    )
  "List of typecaching functions for profiling.")
(defun semantic-elp-typecache-enable ()
  "Enable profiling for `semanticdb-get-typecache'."
  (semantic-elp-core-enable)
  (elp-instrument-list semantic-elp-typecache-list)
  (elp-set-master 'semanticdb-get-typecache)
  )

(defvar semantic-elp-scope-list
  '(
    semantic-analyze-find-tag
    semantic-analyze-scope-nested-tags
    semantic-analyze-scoped-types
    semantic-analyze-scoped-types
    semantic-analyze-tag-prototype-p
    semantic-analyze-type-parts
    semantic-calculate-scope
    semantic-ctxt-scoped-types
    semantic-get-all-local-variables
    semantic-scope-find
    semanticdb-typecache-find
    semanticdb-typecache-merge-streams
    )
  "List of scope calculation functions for profiling.")

(defun semantic-elp-scope-enable ()
  "Enable profiling for `semanticdb-calculate-scope'."
  (semantic-elp-core-enable)
  (elp-instrument-list semantic-elp-typecache-list)
  (elp-instrument-list semantic-elp-scope-list)
  (elp-set-master 'semantic-calculate-scope)
  )

(defvar semantic-elp-analyze-list
  '(
    semantic-analyze-current-context
    semantic-analyze-dereference-metatype
    semantic-analyze-find-tag-sequence
    semantic-analyze-find-tag-sequence
    semantic-analyze-inherited-tags
    semantic-analyze-interesting-tag
    semantic-analyze-pop-to-context
    semantic-analyze-select-best-tag
    semantic-analyze-tag-type
    semantic-analyze-tag-type-to-name
    semantic-analyze-type-constraint
    semantic-analyze-type-parts
    semantic-cache-data-to-buffer
    )
  "List of analyzer calculation functions for profiling.")

(defun semantic-elp-analyze-enable ()
  "Enable profiling for `semanticdb-analyze-current-context'."
  (semantic-elp-scope-enable)
  (elp-instrument-list semantic-elp-analyze-list)
  (elp-set-master 'semantic-analyze-current-context)
  )

(defvar semantic-elp-complete-list
  '(
    semantic-analyze-possible-completions
    semantic-analyze-possible-completions-default
    semantic-analyze-tags-of-class-list
    semantic-analyze-type-constants
    semantic-unique-tag-table-by-name
    )
  "List of smart completion functions for profiling.")

(defun semantic-elp-complete-enable ()
  "Enable profiling for `semanticdb-analyze-current-context'."
  (semantic-elp-analyze-enable)
  (elp-instrument-list semantic-elp-complete-list)
  (elp-set-master 'semantic-analyze-possible-completions)
  )

;;; Storage Classes
;;
;;
(defclass semantic-elp-data ()
  ((raw :initarg :raw
	:type list
	:documentation
	"The raw ELP data.")
   (sort :initform time
	 :documentation
	 "Which column do we sort our data by during various dumps.")
   (sorted :initform nil
	   :documentation
	   "The sorted and filtered version of this data.")
   )
  "Class for managing ELP data.")

(defmethod semantic-elp-change-sort ((data semantic-elp-data) &optional newsort)
  "Change the sort in DATA object to NEWSORT."
  (cond ((eq newsort 'rotate)
	 (let* ((arot '((time . avg)
			(avg . calls)
			(calls . name)
			(name . time)))
		(next (cdr (assoc (oref data sort) arot)))
		)
	   (oset data sort next)))
	((null newsort)
	 nil)
	(t
	 (oset data sort newsort)))
  (let ((r (copy-sequence (oref data raw)))
	(s (oref data sort)))
    (cond ((eq s 'time)
	   (oset data sorted (sort r (lambda (a b)
				       (> (aref a 1) (aref b 1))
				       )))
	   )
	  ((eq s 'avg)
	   (oset data sorted (sort r (lambda (a b)
				       (> (aref a 2) (aref b 2))
				       )))
	   )
	  ((eq s 'calls)
	   (oset data sorted (sort r (lambda (a b)
				       (> (aref a 0) (aref b 0))
				       )))
	   )
	  ((eq s 'name)
	   (oset data sorted (sort r (lambda (a b)
				       (string< (aref a 3) (aref b 3))
				       )))
	   )
	  (t (message "Don't know how to resort with %s" s)
	     ))))

(defmethod semantic-elp-dump-table ((data semantic-elp-data)
				    prefix)
  "dump out the current DATA table using PREFIX before each line."
  (let* ((elpd (oref data sorted))
	 (spaces (make-string (- (length prefix) 2) ? ))
	 )
    (semantic-adebug-insert-simple-thing 
     "Calls\t Total Time\t Avg Time/Call\tName"
     spaces " " 'underline)
    (dolist (d elpd)
      (when (> (aref d 0) 0) ;; We had some calls
	
	(semantic-adebug-insert-simple-thing 
	 (format " % 4d\t% 2.7f\t% 2.7f\t%s"
		 (aref d 0) (aref d 1) (aref d 2) (aref d 3))
	 spaces " " nil)

	))
    )
  )

(defmethod semantic-adebug/eieio-insert-fields ((data semantic-elp-data)
						prefix)
  "Show the fields of ELP data in an adebug buffer.
Ignore the usual, and format a nice table."
  (semantic-adebug-insert-thing (object-name-string data)
				prefix
				"Name: ")
  (let* ((cl (object-class data))
	 (cv (class-v cl)))
    (semantic-adebug-insert-thing (class-constructor cl)
				  prefix
				  "Class: ")
    )
  
  (let ((s (oref data sort))
	)
    ;; Show how it's sorted:
    (let ((start (point))
	  (end nil)
	  )
      (insert prefix "Sort Method: " (symbol-name s))
      (setq end (point))
      ;; (semantic-adebug-insert-thing s prefix "Sort Method: ")
      (put-text-property start end 'adebug data)
      (put-text-property start end 'adebug-indent(length prefix))
      (put-text-property start end 'adebug-prefix prefix)
      (put-text-property start end 'adebug-function
			 'semantic-elp-change-sort-adebug)
      (put-text-property start end 'help-echo
			 "Change the Sort by selecting twice.")
      (insert "\n"))

    ;; How to sort the raw data
    (semantic-elp-change-sort data)
    )
  ;; Display
  (semantic-elp-dump-table data prefix)
  )

(defun semantic-elp-change-sort-adebug (point)
  "Change the sort function here.  Redisplay.
Argument POINT is where the text is."
  (let* ((data (get-text-property point 'adebug))
	 (prefix (get-text-property point 'adebug-prefix))
	 )
    ;; Change it
    (semantic-elp-change-sort data 'rotate)
    (end-of-line)
    (forward-word -1)
    (delete-region (point) (point-at-eol))
    (insert (symbol-name (oref data sort)))
    ;; Redraw it.
    (save-excursion
      (end-of-line)
      (forward-char 1)
      (semantic-elp-dump-table data prefix))
    ))

(defclass semantic-elp-object (eieio-persistent)
  ((file-header-line :initform ";; SEMANTIC ELP Profiling Save File")
   (pathtime :initarg :pathtime
	     :type semantic-elp-data
	     :documentation
	     "Times for calculating the include path.")
   (typecachetime :initarg :typecachetime
		  :type semantic-elp-data
		  :documentation
		  "Times for calculating the typecache.")
   (scopetime :initarg :scopetime
	      :type semantic-elp-data
	      :documentation
	      "Times for calculating the typecache")
   (ctxttime :initarg :ctxttime
	     :type semantic-elp-data
	     :documentation
	     "Times for calculating the context.")
   (completiontime :initarg :completiontime
		   :type semantic-elp-data
		   :documentation
		   "Times for calculating the completions.")
   )
  "Results from a profile run.")
  
;;; ELP hackery.
;;

(defvar semantic-elp-last-results nil
  "Save the last results from an ELP run for more post processing.")

(defun semantic-elp-results (name)
  "Fetch results from the last run, and display.
Copied out of elp.el and modified only slightly.
Argument NAME is the name to give the ELP data object."
  (let ((resvec
	 (mapcar
	  (function
	   (lambda (funsym)
	     (let* ((info (get funsym elp-timer-info-property))
		    (symname (format "%s" funsym))
		    (cc (aref info 0))
		    (tt (aref info 1)))
	       (if (not info)
		   (insert "No profiling information found for: "
			   symname)
		 ;;(setq longest (max longest (length symname)))
		 (vector cc tt (if (zerop cc)
				   0.0 ;avoid arithmetic div-by-zero errors
				 (/ (float tt) (float cc)))
			 symname)))))
	  elp-all-instrumented-list))
	)				; end let
    (setq semantic-elp-last-results (semantic-elp-data name :raw resvec))
    (elp-reset-all))
  )

;;; The big analyze and timer function!
;;
;;

(defvar semantic-elp-last-run nil
  "The results from the last elp run.")

;;;###autoload
(defun semantic-elp-analyze ()
  "Run the analyzer, using ELP to measure performance."
  (interactive)
  (let ((elp-recycle-buffers-p nil)
	path pathtime
	typecache typecachetime
	scope scopetime
	ctxt ctxttime
	completion completiontime)
    ;; Path translation
    (semantic-elp-include-path-enable)
    (setq path (semanticdb-find-translate-path nil nil))
    (semantic-elp-results "translate-path")
    (setq pathtime semantic-elp-last-results)
    ;; typecache
    (semantic-elp-typecache-enable)
    (let* ((tab semanticdb-current-table)
	   (idx (semanticdb-get-table-index tab))
	   (junk (oset idx type-cache nil)) ;; flush!
	   (tc (semanticdb-get-typecache tab))
	   )
      (semanticdb-typecache-file-tags tab)
      (semanticdb-typecache-include-tags tab)
      (setq typecache tc))
    (semantic-elp-results "typecache")
    (setq typecachetime semantic-elp-last-results)
    ;; Scope
    (semantic-elp-scope-enable)
    (setq scope (semantic-calculate-scope))
    (semantic-elp-results "scope")
    (setq scopetime semantic-elp-last-results)
    ;; Analyze!
    (semantic-elp-analyze-enable)
    (setq ctxt (semantic-analyze-current-context))
    (semantic-elp-results "analyze")
    (setq ctxttime semantic-elp-last-results)
    ;; Complete!
    (semantic-elp-complete-enable)
    (setq completion (semantic-analyze-possible-completions ctxt))
    (semantic-elp-results "complete")
    (setq completiontime semantic-elp-last-results)
    ;; build it
    (let ((elpobj (semantic-elp-object
		   "ELP"
		   :pathtime	   pathtime
		   :typecachetime  typecachetime
		   :scopetime	   scopetime
		   :ctxttime	   ctxttime
		   :completiontime completiontime
		   )))
      (semantic-adebug-show elpobj)
      (setq semantic-elp-last-run elpobj)
      (let ((saveas (read-file-name "Save Profile to: " (expand-file-name "~/")
				    "semantic.elp" nil "semantic.elp")))
	(oset elpobj :file saveas)
	(eieio-persistent-save elpobj)
	)
      )))

(defun semantic-elp-show-last-run ()
  "Show the last elp run."
  (interactive)
  (when (not semantic-elp-last-run)
    (error "No last run to show"))
  (semantic-adebug-show semantic-elp-last-run))

;;;###autoload
(defun semantic-elp-load-old-run (file)
  "Load an old run from FILE, and show it."
  (interactive "fLast Run File: ")
  (setq semantic-elp-last-run
	(eieio-persistent-read file))
  (semantic-adebug-show semantic-elp-last-run))

(provide 'semantic-elp)
;;; semantic-elp.el ends here
