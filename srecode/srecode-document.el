;;; srecode-document.el --- Documentation (comment) generation

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-document.el,v 1.3 2008/12/30 18:47:00 zappo Exp $

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
;; Routines for fabricating human readable text from function an
;; variable names as base-text for function comments.  Document is not
;; meant to generate end-text for any function.  It is merely meant to
;; provide some useful base words and text, and as a framework for
;; managing comments.
;;
;;; Origins:
;;
;; Document was first written w/ cparse, a custom regexp based c parser.
;;
;; Document was then ported to cedet/semantic using sformat (super
;; format) as the templating engine.
;;
;; Document has now been ported to srecode, using the semantic recoder
;; as the templating engine.

(require 'srecode-insert)
(require 'srecode-dictionary)
(require 'srecode-extract)
(require 'srecode-args)
(require 'semantic)
(require 'semantic-tag)
(require 'semantic-doc)
(require 'pulse)

;;; Code:
(defgroup document nil
  "File and tag browser frame."
  :group 'texinfo
  :group 'srecode
  )

;;;###autoload
(eval-after-load "srecode-mode"
  ;; Once SRecode mode is loaded, then lets add ourself to the keymap.
  '(progn
     (srecode-add-code-generator 'srecode-document-insert-comment
				 "Comments"
				 "C")
     )
)

;;;###autoload
(defun srecode-document-insert-comment ()
  "Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments."
  (interactive)
  (semantic-fetch-tags)
  (let ((ctxt (srecode-calculate-context)))
    (if ;; Active region stuff.
	(or srecode-handle-region-when-non-active-flag
	    (eq last-command 'mouse-drag-region)
	    (and transient-mark-mode mark-active))
	(srecode-document-insert-group-comments (point) (mark))
      ;; ELSE

      ;; A declaration comment.  Find what it documents.
      (when (equal ctxt '("declaration" "comment"))
	
	;; If we are on a one line tag/comment, go to that fcn.
	(if (save-excursion (back-to-indentation)
			    (semantic-current-tag))
	    (back-to-indentation)

	  ;; Else, do we have a fcn following us?
	  (let ((tag (semantic-find-tag-by-overlay-next)))
	    (when tag (semantic-go-to-tag tag))))
	)

      ;; Now analyze the tag we may be on.

      (when (semantic-current-tag)
	(cond
	 ;; A one-line variable
	 ((and (semantic-tag-of-class-p (semantic-current-tag) 'variable)
	       (srecode-document-one-line-tag-p (semantic-current-tag)))
	  (srecode-document-insert-variable-one-line-comment))
	 ;; A plain function
	 ((semantic-tag-of-class-p (semantic-current-tag) 'function)
	  (srecode-document-insert-function-comment))
	 ;; Don't know.
	 (t
	  (error "Not sure what to comment"))
	 )))))

(defun srecode-document-trim-whitespace (str)
  "Strip stray whitespace from around STR."
  (when (string-match "^\\(\\s-\\|\n\\)+" str)
    (setq str (replace-match "" t t str)))
  (when (string-match "\\(\\s-\\|\n\\)+$" str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defun srecode-document-insert-function-comment (&optional fcn-in)
  "Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provied, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  
  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "function-comment"
					   "declaration"
					   'document)))
    (if (not temp)
	(error "No templates for inserting function comments"))

    ;; Try to figure out the tag we want to use.
    (when (not fcn-in)
      (semantic-fetch-tags)
      (setq fcn-in (semantic-current-tag)))
  
    (when (or (not fcn-in)
	      (not (semantic-tag-of-class-p fcn-in 'function)))
      (error "No tag of class 'function to insert comment for"))

    (if (not (eq (current-buffer) (semantic-tag-buffer fcn-in)))
	(error "Only insert comments for tags in the current buffer"))

    ;; Find any existing doc strings.
    (semantic-go-to-tag fcn-in)
    (beginning-of-line)
    (forward-char -1)

    (let ((lextok (semantic-documentation-comment-preceeding-tag fcn-in 'lex))
	  (doctext
	   (srecode-document-programmer->english (semantic-tag-name fcn-in)))
	  )

      (when lextok
	(let* ((s (semantic-lex-token-start lextok))
	       (e (semantic-lex-token-end lextok))
	       (plaintext
		(srecode-document-trim-whitespace
		 (save-excursion
		   (goto-char s)
		   (semantic-doc-snarf-comment-for-tag nil))))
	       (extract (condition-case nil
			    (srecode-extract temp s e)
			  (error nil))
			)
	       (distance (count-lines e (semantic-tag-start fcn-in)))
	       (belongelsewhere (save-excursion
				  (goto-char s)
				  (back-to-indentation)
				  (semantic-current-tag)))
	       )

	  (when (not belongelsewhere)

	    (pulse-momentary-highlight-region s e)

	    ;; There are many possible states that comment could be in.
	    ;; Take a guess about what the user would like to do, and ask
	    ;; the right kind of question.
	    (when (or (not (> distance 2))
		      (y-or-n-p "Replace this comment? "))

	      (when (> distance 2)
		(goto-char e)
		(delete-horizontal-space)
		(delete-blank-lines))

	      (cond
	       ((and plaintext (not extract))
		(if (y-or-n-p "Convert old-style comment to Template with old text? ")
		    (setq doctext plaintext))
		(delete-region s e)
		(goto-char s))
	       (extract
		(when (y-or-n-p "Refresh pre-existing comment (recycle old doc)? ")
		  (delete-region s e)
		  (goto-char s)
		  (setq doctext
			(srecode-document-trim-whitespace
			 (srecode-dictionary-lookup-name extract "DOC")))))
	       ))
	    )))

      (beginning-of-line)

      ;; Perform the insertion
      (let ((srecode-semantic-selected-tag fcn-in)
	    (srecode-semantic-apply-tag-augment-hook
	     (lambda (tag dict)
	       (srecode-dictionary-set-value
		dict "DOC"
		(if (eq tag fcn-in)
		    doctext
		  (srecode-document-programmer->english
		   (semantic-tag-name tag)))
		)))
	    )
	(srecode-insert-fcn temp dict)
	))
    ))

;;;###autoload
(defun srecode-document-insert-variable-one-line-comment (&optional var-in)
  "Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provied, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  
  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "variable-same-line-comment"
					   "declaration"
					   'document))
	 (extract dict))
    (if (not temp)
	(error "No templates for inserting variable comments"))

    ;; Try to figure out the tag we want to use.
    (when (not var-in)
      (semantic-fetch-tags)
      (setq var-in (semantic-current-tag)))
  
    (when (or (not var-in)
	      (not (semantic-tag-of-class-p var-in 'variable)))
      (error "No tag of class 'variable to insert comment for"))

    (if (not (eq (current-buffer) (semantic-tag-buffer var-in)))
	(error "Only insert comments for tags in the current buffer"))

    ;; Find any existing doc strings.
    (goto-char (semantic-tag-end var-in))
    (skip-syntax-forward "-" (point-at-eol))
    (let ((lextok (semantic-doc-snarf-comment-for-tag 'lex))
	  )

      (when lextok
	(let ((s (semantic-lex-token-start lextok))
	      (e (semantic-lex-token-end lextok)))

	  (pulse-momentary-highlight-region s e)

	  (when (not (y-or-n-p "A comment already exists.  Replace? "))
	    (error "Quit"))

	  ;; Extract text from the existing comment.
	  (setq extract (srecode-extract temp s e))

	  (delete-region s e)
	  (goto-char s) ;; To avoid adding a CR.
	  ))
      )

    ;; Clean up the end of the line and use handy comment-column.
    (end-of-line)
    (delete-horizontal-space)
    (move-to-column comment-column t)
    (when (< (point) (point-at-eol)) (end-of-line))

    ;; Perform the insertion
    (let ((srecode-semantic-selected-tag var-in)
	  (srecode-semantic-apply-tag-augment-hook
	   (lambda (tag dict)
	     (srecode-dictionary-set-value
	      dict "DOC" (srecode-document-programmer->english (semantic-tag-name tag)))))
	  )
      (srecode-insert-fcn temp dict)
      ))
  )

(defun srecode-document-insert-group-comments (beg end)
  "Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain."
  (interactive "r")
  (error "Need to implement group comments")
  ;; @TODO
  )


;;; Document Generation Functions
;;
;; Routines for making up English style comments.
(defcustom srecode-document-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("doc" . "document")
    ("i18n" . "internationalization")
    ("file" . "file")
    ("line" . "line")
    ("l10n" . "localization")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("num\\(ber\\)?" . "number")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;common syllable
    )
  "List of common English abbreviations or full words.
These are nouns (as opposed to verbs) for use in creating expanded
versions of names.This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defun srecode-document-programmer->english (programmer)
  "Take PROGRAMMER and convert it into English.
Works with the following rules:
  1) convert all _ into spaces.
  2) inserts spaces between CamelCasing word breaks.
  3) expands noun names based on common programmer nouns.
  
  This function is designed for variables, not functions.  This does
not account for verb parts."
  (let ((ind 0)				;index in string
	(llow nil)			;lower/upper case flag
	(wlist nil)			;list of words after breaking
	(newstr nil)			;new string being generated
	(al nil))			;autocomment list
    ;;
    ;; 1) Convert underscores
    ;;
    (while (< ind (length programmer))
      (setq newstr (concat newstr
			   (if (= (aref programmer ind) ?_)
			       " " (char-to-string (aref programmer ind)))))
      (setq ind (1+ ind)))
    (setq programmer newstr
	  newstr nil
	  ind 0)
    ;;
    ;; 2) Find word brakes between case changes
    ;;
    (while (< ind (length programmer))
      (setq newstr
	    (concat newstr
		    (let ((tc (aref programmer ind)))
		      (if (and (>= tc ?a) (<= tc ?z))
			  (progn
			    (setq llow t)
			    (char-to-string tc))
			(if llow
			    (progn
			      (setq llow nil)
			      (concat " " (char-to-string tc)))
			  (char-to-string tc))))))
      (setq ind (1+ ind)))
    ;;
    ;; 3) Expand the words if possible
    ;;
    (setq llow nil
	  ind 0
	  programmer newstr
	  newstr nil)
    (while (string-match (concat "^\\s-*\\([^ \t\n]+\\)") programmer)
      (let ((ts (substring programmer (match-beginning 1) (match-end 1)))
	    (end (match-end 1)))
	(setq al srecode-document-autocomment-common-nouns-abbrevs)
	(setq llow nil)
	(while al
	  (if (string-match (car (car al)) (downcase ts))
	      (progn
		(setq newstr (concat newstr (cdr (car al))))
		;; don't terminate because we may actuall have 2 words
		;; next to eachother we didn't identify before
		(setq llow t)))
	  (setq al (cdr al)))
	(if (not llow) (setq newstr (concat newstr ts)))
	(setq newstr (concat newstr " "))
	(setq programmer (substring programmer end))))
    newstr))

;;; UTILS
;;
(defun srecode-document-one-line-tag-p (tag)
  "Does TAG fit on one line with space on the end?"
  (save-excursion
    (semantic-go-to-tag tag)
    (and (<= (semantic-tag-end tag) (point-at-eol))
	 (goto-char (semantic-tag-end tag))
	 (< (current-column) 70))))

;;; TESTS
;;
;;;###autoload
(defun srecode-document-function-comment-extract-test ()
  "Test old comment extraction.
Dump out the extracted dictionary."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  
  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "function-comment"
					   "declaration"
					   'document))
	 (fcn-in (semantic-current-tag)))

    (if (not temp)
	(error "No templates for function comments"))

    ;; Try to figure out the tag we want to use.
    (when (or (not fcn-in)
	      (not (semantic-tag-of-class-p fcn-in 'function)))
      (error "No tag of class 'function to insert comment for"))

    (let ((lextok (semantic-documentation-comment-preceeding-tag fcn-in 'lex))
	  )

      (when (not lextok)
	(error "No comment to attempt an extraction"))

      (let ((s (semantic-lex-token-start lextok))
	    (e (semantic-lex-token-end lextok))
	    (extract nil))

	(pulse-momentary-highlight-region s e)

	;; Extract text from the existing comment.
	(setq extract (srecode-extract temp s e))

	(with-output-to-temp-buffer "*SRECODE DUMP*"
	  (princ "EXTRACTED DICTIONARY FOR ")
	  (princ (semantic-tag-name fcn-in))
	  (princ "\n--------------------------------------------\n")
	  (srecode-dump extract))
	))))

(provide 'srecode-document)
;;; srecode-document.el ends here
