;;; semantic-complete.el --- Routines for performing tag completion

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-complete.el,v 1.7 2003/05/10 02:57:50 zappo Exp $

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
;; Completion of tags by name using tables of semantic generated tags.
;;
;; While it would be a simple matter of flattening all tag known
;; tables to perform completion across them using `all-completions',
;; or `try-completion', that process would be slow.  In particular,
;; when a system database is included in the mix, the potential for a
;; ludicrous number of options becomes apparent.
;;
;; As such, dynamically searching across tables using a prefix,
;; regular expression, or other feature is needed to help find symbols
;; quickly without resorting to "show me every possible option now".
;;
;; In addition, some symbol names will appear in multiple locations.
;; If it is important to distiguish, then a way to provide a choice
;; over these locations is important as well.
;;
;; Beyond brute force offers for completion of plain strings,
;; using the smarts of semantic-analyze to provide reduced lists of
;; symbols, or fancy tabbing to zoom into files to show multiple hits
;; of the same name can be provided.
;;
;;; How it works:
;;
;; There are several parts of any completion engine.  They are:
;;
;; A.  Collection of possible hits
;; B.  Typing or selecting an option
;; C.  Displaying possible unique completions
;; D.  Using the result
;;
;; Here, we will treat each section separately (excluding D)
;; They can then be strung together in user-visible commands to
;; fullfill specific needs.
;;
;; COLLECTORS:
;;
;; A collector is an object which represents the means by which tokens
;; to complete on are collected.  It's first job is to find all the
;; tokens which are to be completed against.  It can also rename
;; some tokens if needed so long as `semantic-tag-clone' is used.
;;
;; Some collectors will gather all tags to complete against first
;; (for in buffer queries, or other small list situations).  It may
;; choose to do a broad search on each completion request.  Built in
;; functionality automatically focuses the cache in as the user types.
;;
;; A collector choosing to create and rename tags could choose a
;; plain name format, a postfix name such as method:class, or a
;; prefix name such as class.method.
;;
;; DISPLAYORS
;;
;; A displayor is in charge if showing the user interesting things
;; about available completions, and can optionally provide a focus.
;; The simplest display just lists all available names in a separate
;; window.  It may even choose to show short names when there are
;; many to choose from, or long names when there are fewer.
;;
;; A complex displayor could opt to help the user 'foucs' on some
;; range.  For example, if 4 tags all have the same name, subsequent
;; calls to the displayor may opt to show each tag one at a time in
;; the buffer.  When the user likes one, selection would cause the
;; 'focus' item to be selected.

(require 'eieio)
(require 'semantic-tag)
(require 'semantic-find)
(require 'semantic-analyze)
(require 'semantic-format)
(require 'semantic-ctxt)
;; Keep semanticdb optional.
(eval-when-compile (require 'semanticdb))

;;; Code:

;;; Compatibility
;;
(if (fboundp 'minibuffer-contents)
    (defalias 'semantic-minibuffer-contents 'minibuffer-contents)
  (defalias 'semantic-minibuffer-contents 'buffer-string))
(if (fboundp 'delete-minibuffer-contents)
    (defalias 'semantic-delete-minibuffer-contents 'delete-minibuffer-contents)
  (defalias 'semantic-delete-minibuffer-contents 'erase-buffer))

;;; Option Selection harnesses
;;
(defvar semantic-completion-collector-engine nil
  "The tag collector for the current completion operation.
Value should be an object of a subclass of
`semantic-completion-engine-abstract'.")

(defvar semantic-completion-display-engine nil
  "The tag display engine for the current completion operation.
Value should be a ... what?")

(defvar semantic-complete-key-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " 'semantic-complete-complete-space)
    (define-key km "\t" 'semantic-complete-complete-tab)
    (define-key km "\C-m" 'semantic-complete-done)
    (define-key km "\C-g" 'abort-recursive-edit)
    (define-key km "\M-n" 'next-history-element)
    (define-key km "\M-p" 'previous-history-element)
    (define-key km "\C-n" 'next-history-element)
    (define-key km "\C-p" 'previous-history-element)
    ;; Add history navigation
    km)
  "Keymap used while completing across a list of tags.")

(defvar semantic-completion-default-history nil
  "Default history variable for any unhistoried prompt.
Keeps STRINGS only in the history.")


;;;###autoload
(defun semantic-complete-read-tag-engine (collector displayor prompt
						    default-tag initial-input
						    history)
  "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is a function which can be used to to return
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argumeng DISPLAYOR is a function used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (let ((semantic-completion-collector-engine collector)
	(semantic-completion-display-engine displayor)
	(ans nil)
	(tag nil)
	(orig-default-tag default-tag)
	)
    (unless default-tag
      (setq default-tag (semantic-complete-choose-default)))
    (when default-tag
      (setq default-tag (cond ((stringp default-tag)
			       default-tag)
			      ((semantic-tag-p default-tag)
			       (semantic-tag-name default-tag))
			      ((and (listp default-tag)
				    (stringp (car default-tag)))
			       (car (nreverse default-tag)))
			      )))
    (when (stringp default-tag)
      ;; Add this to the prompt.
      ;;
      ;; I really want to add a lookup of the symbol in those
      ;; tags available to the collector and only add it if it
      ;; is available as a possibility, but I'm too lazy right
      ;; now.
      ;;
      (if (string-match ":" prompt)
	  (setq prompt (concat
			(substring prompt 0 (match-beginning 0))
			" (" default-tag ")"
			(substring prompt (match-beginning 0))))
	(setq prompt (concat prompt " ("
			     default-tag
			     "): "))))
    (setq ans
	  (read-from-minibuffer prompt
				initial-input
				semantic-complete-key-map
				nil
				(or history
				    'semantic-completion-default-history)
				default-tag))
    ;; First, see if the displayor has a focus entry for us.
    (setq tag (list (semantic-displayor-current-focus displayor)))
    (unless tag
      ;; Convert the answer, a string, back into a tag using
      ;; the collector
      (if (slot-boundp collector 'match-list)
	  (setq tag (oref collector match-list))
	;; No match!
	(if (and (string= ans "") default-tag)
	    ;; Choose the default!
	    (if (semantic-tag-p orig-default-tag)
		(setq tag orig-default-tag)
	      (semantic-collector-calculate-completions collector
							default-tag)
	      (setq tag (oref collector match-list))
	      ))))
    ;; If it isn't a tag, we didn't finish properly.
    ;; Throw an error
    (if (semantic-tag-with-position-p (car tag))
	(car tag)
      (error "Error finding tag based upon result")
      )))

(defun semantic-complete-complete-space ()
  "Complete the partial input in the minibuffer."
  (interactive)
  (semantic-complete-do-completion t))

(defun semantic-complete-complete-tab ()
  "Complete the partial input in the minibuffer as far as possible."
  (interactive)
  (semantic-complete-do-completion))

(defun semantic-complete-done ()
  "Accept the current input."
  (interactive)
  (when (string= (semantic-minibuffer-contents) "")
      ;; The user wants the defaults!
      (exit-minibuffer))
  (semantic-complete-do-completion)
  (if (and (slot-boundp semantic-completion-collector-engine
			'match-list)
	   (oref semantic-completion-collector-engine match-list))
      (exit-minibuffer)
    (semantic-completion-message " [No Match]")
    ))

(defun semantic-complete-do-completion (&optional partial)
  "Do a completion for the current minibuffer.
If PARTIAL, do partial completion stopping at spaces."
  (semantic-collector-calculate-completions
   semantic-completion-collector-engine
   (semantic-minibuffer-contents))
  (let* ((na (semantic-collector-next-action
	      semantic-completion-collector-engine)))
    (cond ((eq na 'display)
	   ;; We need to display the completions.
	   ;; Set the completions into the display engine
	   (semantic-displayor-set-completions
	    semantic-completion-display-engine
	    (semantic-collector-all-completions
	     semantic-completion-collector-engine)
	    (semantic-minibuffer-contents))
	   ;; Ask the displayor to display them.
	   (semantic-displayor-show-request
	    semantic-completion-display-engine)
	   )
	  ((eq na 'focus)
	   (semantic-displayor-focus-request
	    semantic-completion-display-engine)
	   )
	  ((eq na 'complete)
	   ;; Else, we are in completion mode
	   (let ((comp (semantic-collector-try-completion
			semantic-completion-collector-engine))
		 )
	     (cond ((string= (semantic-minibuffer-contents)
			     comp)
		    ;; Minibuffer isn't changing.  Display.
		    )
		   ((null comp)
		    (semantic-completion-message " [No Match]")
		    (ding))
		   ((stringp comp)
		    (beginning-of-line)
		    (delete-region (point) (point-max))
		    ;; We should pay attention to the parameter
		    ;; PARTIAL here, and look at the text we are
		    ;; putting in, stopping on whitespace and word separator
		    ;; characters.
		    (insert comp))
		   ((and (listp comp)
			 (semantic-tag-p (car comp)))
		    (when (not (string= (buffer-string)
					(semantic-tag-name (car comp))))
		      ;; A fully unique completion was available.
		      (semantic-delete-minibuffer-contents)
		      (insert (semantic-tag-name (car comp))))
		    ;; The match is complete
		    (if (= (length comp) 1)
			(semantic-completion-message " [Complete]")
		      (semantic-completion-message " [Complete, but not unique]"))
		    )
		   (t nil))))
	  (t nil))))

(defun semantic-completion-message (fmt &rest args)
  "Display the string FMT formatted with ARGS at the end of the minibuffer."
  (message (concat (buffer-string) (apply 'format fmt args))))

(defun semantic-complete-choose-default ()
  "Based on the current position, choose a nice default for completion.
Defaults are chosent from the lcoal context.
It might be nice to someday have it find the tag assocated with the
default, possibly choosing a function or variable."
  (let ((sym nil))
    (setq sym (semantic-ctxt-current-symbol))
    (unless sym
      (setq sym (semantic-ctxt-current-function)))
    (unless sym
      (setq sym (semantic-ctxt-current-assignment)))
    sym))


;;; Specific queries
;;
(defun semantic-complete-read-tag-buffer-deep (prompt &optional
						      default-tag initial-input history)
  "Ask for a tag by name from the current buffer.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (semantic-complete-read-tag-engine
   (semantic-collector-buffer-deep prompt :buffer (current-buffer))
   (semantic-displayor-traditional-with-focus-highlight "simple")
   ;;(semantic-displayor-tooltip "simple")
   prompt
   default-tag
   initial-input
   history)
  )


;;; Collection Engines
;;
;; Collection engines can scan tags from the current environment and
;; provide lists of possible completions.
;;
;; General features of the abstract collector:
;; * Cache completion lists between uses
;; * Cache itself per buffer.  Handle reparse hooks

(defvar semantic-collector-per-buffer-list nil
  "List of collectors active in this buffer.")
(make-variable-buffer-local 'semantic-collector-per-buffer-list)

(defvar semantic-collector-list nil
  "List of global collectors active this session.")

(defclass semantic-collector-abstract ()
  ((buffer :initarg :buffer
	   :type buffer
	   :documentation "Originating buffer for this collector.
Some collectors use a given buffer as a starting place while looking up
tags.")
   (cache :initform nil
	  :type list
	  :documentation "Cache of tags.
These tags are re-used during a completion session.
Sometimes these tags are cached between completion sessions.")
   (last-all-completions :initarg nil
			 :type list
			 :documentation "Last result of `all-completions'.
This result can be used for refined completions as `last-prefix' gets
closer to a specific result.")
   (last-prefix :type string
		:documentation "The last queried prefix.
This prefix can be used to cache intermediate completion offers.
making the action of homing in on a token faster.")
   (last-completion :type string
		    :documentation "The last calculated completion.
This completion is calculated and saved for future use.")
   (match-list :type list
	       :documentation "The list of matched tags.
When tokens are matched, they are added to this list.")
   )
  "Root class for completion engines."
  :abstract t)

(defmethod semantic-collector-last-prefix= ((obj semantic-collector-abstract)
					    last-prefix)
  "Return non-nil if OBJ's prefix matches PREFIX."
  (and (slot-boundp obj 'last-prefix)
       (string= (oref obj last-prefix) last-prefix)))

(defmethod semantic-collector-next-action
  ((obj semantic-collector-abstract))
  "What should we do next?  OBJ can predict a next good action."
  (if (string= (semantic-minibuffer-contents)
	       (oref obj last-completion))
      (semantic-displayor-next-action semantic-completion-display-engine)
    'complete))

(defmethod semantic-collector-calculate-completions
  ((obj semantic-collector-abstract) prefix)
  "Calculate completions for prefix as setup for other queries."
  (let* ((case-fold-search semantic-case-fold)
	 (same-prefix-p (semantic-collector-last-prefix= obj prefix))
	 (completionlist
	  (if (or same-prefix-p
		  (and (slot-boundp obj 'last-prefix)
		       (eq (compare-strings (oref obj last-prefix) 0 nil
					    prefix 0 (length prefix))
			   t)))
	      ;; New prefix is subset of old prefix
	      (oref obj last-all-completions)
	    (or (oref obj cache)
		(semantic-collector-calculate-cache obj))))
	 ;; Get the result
	 (answer (if same-prefix-p
		     completionlist
		   (semantic-find-tags-for-completion prefix
						      completionlist))
		 )
	 (completion nil))
    (when (not same-prefix-p)
      ;; Save results if it is interesting and beneficial
      (oset obj last-prefix prefix)
      (oset obj last-all-completions answer))
    ;; Now calculation the completion.
    (setq completion (try-completion prefix answer))
    (oset obj match-list nil)
    (oset obj last-completion
	  (cond
	   ;; Unique match in AC.  Last completion is a match.
	   ;; Also set the match-list.
	   ((eq completion t)
	    (oset obj match-list answer)
	    prefix)
	   ;; Non unique match, return the string that handles
	   ;; completion
	   (t (or completion prefix))
	   ))
    ))

(defmethod semantic-collector-all-completions
  ((obj semantic-collector-abstract))
  "For OBJ, retrieve all completions matching PREFIX.
The returned list consists of all the tags currently
matching PREFIX."
  (when (slot-boundp obj 'last-all-completions)
    (oref obj last-all-completions)))

(defmethod semantic-collector-try-completion
  ((obj semantic-collector-abstract))
  "For OBJ, attempt to match PREFIX.
See `try-completion' for details on how this works.
Return nil for no match.
Return a string for a partial match.
For a unique match of PREFIX, return the list of all tags
with that name."
  (if (slot-boundp obj 'last-completion)
      (oref obj last-completion)))

(defmethod semantic-collector-calculate-cache
  ((obj semantic-collector-abstract))
  "Calculate the completion cache for OBJ."
  nil
  )

(defmethod semantic-collector-flush ((this semantic-collector-abstract))
  "Flush THIS collector object, clearing any caches and prefix."
  (oset this cache nil)
  (slot-makeunbound this 'last-prefix)
  )

;;; PER BUFFER
;;
(defclass semantic-collector-buffer-abstract (semantic-collector-abstract)
  ()
  "Root class for per-buffer completion engines.
These collectors track themselves on a per-buffer basis "
  :abstract t)

(defmethod constructor :STATIC ((this semantic-collector-buffer-abstract)
				newname &rest fields)
  "Reuse previously created objects of this type in buffer."
  (let ((old nil)
	(bl semantic-collector-per-buffer-list))
    (while (and bl (null old))
      (if (eq (object-class (car bl)) this)
	  (setq old (car bl))))
    (unless old
      (let ((new (call-next-method)))
	(add-to-list 'semantic-collector-per-buffer-list new)
	(setq old new)))
    (slot-makeunbound old 'last-completion)
    (slot-makeunbound old 'last-prefix)
    (slot-makeunbound old 'match-list)
    old))

;; Buffer specific collectors should flush themselves
(defun semantic-collector-buffer-flush (newcache)
  "Flush all buffer collector objects.
NEWCACHE is the new tag table, but we ignore it."
  (condition-case nil
      (let ((l semantic-collector-per-buffer-list))
	(while l
	  (if (car l) (semantic-collector-flush (car l)))
	  (setq l (cdr l))))
    (error nil)))

(add-hook 'semantic-after-toplevel-cache-change-hook
	  'semantic-collector-buffer-flush)

;;; DEEP BUFFER SPECIFIC COMPLETION
;;
(defclass semantic-collector-buffer-deep
  (semantic-collector-buffer-abstract)
  ()
  "Completion engine for tags in the current buffer.
Provides deep searches through types.")

(defmethod semantic-collector-calculate-cache
  ((obj semantic-collector-buffer-abstract))
  "Calculate the completion cache for OBJ.
Uses `semantic-flatten-tags-table'"
  (oset obj cache (semantic-flatten-tags-table (oref obj buffer))))


;;; Tag List Display Engines
;;
(defclass semantic-displayor-abstract ()
  ((table :type list
	  :initform nil
	  :documentation "List of tags this displayor is showing.")
   (last-prefix :type string
		:documentation "Prefix associated with slot `table'")
   )
  "Manages the display of some number of tags."
  :abstract t)

(defmethod semantic-displayor-next-action ((obj semantic-displayor-abstract))
  "The next action to take on the minibuffer related to display."
  (if (and (slot-boundp obj 'last-prefix)
	   (string= (oref obj last-prefix) (semantic-minibuffer-contents))
	   (eq last-command this-command))
      'focus
    'display))

(defmethod semantic-displayor-set-completions ((obj semantic-displayor-abstract)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (oset obj table table)
  (oset obj last-prefix prefix))

(defmethod semantic-displayor-show-request ((obj semantic-displayor-abstract))
  "A request to show the current tags table."
  (ding))

(defmethod semantic-displayor-focus-request ((obj semantic-displayor-abstract))
  "A request to for the displayor to focus on some tag option."
  (scroll-other-window))

(defmethod semantic-displayor-current-focus ((obj semantic-displayor-abstract))
  "Return the tag currently in focus."
  (if (slot-boundp obj 'table)
      (car (oref obj table))))

;; Traditional displayor
(defcustom semantic-completion-displayor-format-tag-function
  #'semantic-format-tag-name
  "*A Tag format function to use when showing completions."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defclass semantic-displayor-traditional (semantic-displayor-abstract)
  ()
  "Traditional display mechanism for a list of possible completions.")

(defmethod semantic-displayor-show-request ((obj semantic-displayor-abstract))
  "A request to show the current tags table."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list
     (mapcar semantic-completion-displayor-format-tag-function
	     (oref obj table)))
    )
  )

;;; Abstract baseclass for any displayor which supports focus
(defclass semantic-displayor-focus-abstract (semantic-displayor-abstract)
  ((focus :type number
	  :documentation "A tag index from `table' which has focus.
Multiple calls to the display function can choose to focus on a
given tag, by highlighting its location.")  
   )
  "A displayor which has the ability to focus in on one tag.
Focusing is a way of differentiationg between multiple tags
which have the same name."
  :abstract t)

(defmethod semantic-displayor-set-completions ((obj semantic-displayor-focus-abstract)
					       table prefix)
  "Set the list of tags to be completed over to TABLE."
  (call-next-method)
  (slot-makeunbound obj 'focus))

(defmethod semantic-displayor-focus-next-tag ((obj semantic-displayor-focus-abstract))
  "Return the next tag OBJ should focus on."
  (when (and (slot-boundp obj 'table) (oref obj table))
    (with-slots (table) obj
      (if (not (slot-boundp obj 'focus))
	  (oset obj focus 0)
	(oset obj focus (1+ (oref obj focus)))
	)
      (if (<= (length table) (oref obj focus))
	  (oset obj focus 0))
      (nth (oref obj focus) table))))

(defmethod semantic-displayor-current-focus ((obj semantic-displayor-focus-abstract))
  "Return the tag currently in focus."
  (if (and (slot-boundp obj 'focus)
	   (slot-boundp obj 'table))
      (nth (oref obj focus) (oref obj table))
    (call-next-method)))

;;; Simple displayor which performs traditional display completion,
;; and also focuses with highlighting.
(defclass semantic-displayor-traditional-with-focus-highlight
  (semantic-displayor-traditional semantic-displayor-focus-abstract)
  ()
  "A traditional displayor which can focus on a tag by showing it.")

(defmethod semantic-displayor-focus-request
  ((obj semantic-displayor-traditional-with-focus-highlight))
  "Focus in on possible tag completions.
Focus is performed by cycling through the tags and highlighting
one in the source buffer."
  (let ((tag (semantic-displayor-focus-next-tag obj))
	)
    (if (semantic-tag-with-position-p tag)
	(when (semantic-tag-buffer tag)
	  (if (get-buffer-window (semantic-tag-buffer tag))
	      (unwind-protect
		  (progn
		    (select-window (get-buffer-window (semantic-tag-buffer tag)))
		    (goto-char (semantic-tag-start tag))
		    (semantic-momentary-highlight-token tag)
		    )
		(select-window (minibuffer-window))))))
    ))

;;; Tooltip completion lister
;; 
;; Written and contributed by Masatake YAMATO <jet@gyve.org>
;;
(defclass semantic-displayor-tooltip (semantic-displayor-abstract)
  ((max-tags     :type integer
		 :initarg :max-tags
		 :initform 10
		 :custom integer
		 :documentation 
		 "Max number of tags displayed on tooltip at once.
If `force-show' is 1,  this value is ignored with typing tab or space twice continuously.
if `force-show' is 0, this value is always ignored.")
   (force-show   :type integer
		 :initarg :force-show
	         :initform 1
		 :custom (choice (const 
				  :tag "Show when double typing"
				  1)
				 (const
				  :tag "Show always"
				  0)
				 (const 
				  :tag "Show if the number of tags is less than `max-tags'." 
				  -1))
	         :documentation
		 "Control the behavior of the number of tags is greater than `max-tags'.
-1 means tags are never shown. 
0 means the tags are always shown. 
1 means tags are shown if space or tab is typed twice continuously.")
   (typing-count :type integer
		 :initform 0
		 :documentation 
		 "Counter holding how many times the user types space or tab continuously before showing tags.")
   (shown        :type boolean
		 :initform nil
		 :documentation
		 "Flag representing whether tags is shown once or not.")
   )
  "Display mechanism using tooltip for a list of possible completions.")

(defmethod semantic-displayor-show-request ((obj semantic-displayor-tooltip))
  "A request to show the current tags table."
  (let* ((l (mapcar semantic-completion-displayor-format-tag-function
		    (oref obj table)))
	 (ll (length l))
	 (typing-count (oref obj typing-count))
	 (force-show (oref obj force-show))
	 msg)
    (if (or (oref obj shown)
	    (< ll (oref obj max-tags)) 
	    (and (<= 0 force-show)
		 (< (1- force-show) typing-count)))
	(progn 
	  (oset obj typing-count 0)
	  (oset obj shown t)
	  (if (eq 1 ll)
	      (setq msg "SOLE COMPLETION")
	    (setq msg (mapconcat 'identity l "\n"))
	    (if (eq 0 (length msg))
		(setq msg "NO MORE COMPLETIONS"))) 
	  (semantic-displayor-tooltip-show msg))
      (oset obj typing-count (1+ typing-count))
      (cond
       ((= force-show -1)
	(semantic-displayor-tooltip-show "TOO MANY"))
       ((= force-show 1)
	(semantic-displayor-tooltip-show 
	 "TOO MANY (Type TAB or SPACE again to show force)"))))))

(defun semantic-displayor-tooltip-show (text)
  (require 'tooltip)
  (require 'avoid)
  (let* ((P (mouse-avoidance-point-position))
	 (frame (car P))
	 (x (cadr P))
	 (y (cddr P))
	 (oP (mouse-position))
	 (oframe (car oP))
	 (ox     (cadr oP))
	 (oy     (cddr oP)))
    (set-mouse-position frame x y)
    (tooltip-show text)
    (set-mouse-position frame (1+ x) y)))

;; End code contributed by Masatake YAMATO <jet@gyve.org>


;;; Testing
;;
(defun semantic-complete-test ()
  "Test completion mechanisms."
  (interactive)
  (message "%S"
   (semantic-format-tag-prototype
    (semantic-complete-read-tag-buffer-deep "Symbol: ")
    )))

;; End
(provide 'semantic-complete)

;;; semantic-complete.el ends here
