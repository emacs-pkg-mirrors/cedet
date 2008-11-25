;;; semantic-symref.el --- Symbol Reference API

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Semantic Symbol Reference API.
;;
;; Semantic's native parsing tools do not handle symbol references.
;; Tracking such information is a task that requires a huge amount of
;; space and processing not apropriate for an Emacs Lisp program.
;;
;; Many desired tools used in refactoring, however, desire to have
;; such references available to them.  This API aims to provide a
;; range of functions that can be used to identify references.  The
;; API is backed by an OO system that is used to allow multiple
;; external tools to provide the information.
;;
;; To support a new external tool, sublcass `semantic-symref-tool-baseclass'
;; and implement the methods.  The baseclass provides support for
;; managing external processes that produce parsable output.
;;
;; Your tool should then create an instance of `semantic-symref-result'.

;;; Code:
(defvar semantic-symref-tool 'grep
  "*The active symbol reference tool name.
The tool symbol must be a subclass of `semantic-symref-tool-baseclass'.")
(make-variable-buffer-local 'semantic-symref-tool)

(defun semantic-symref-find-references-by-name (name)
  "Find a list of references to NAME in the current project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sName: ")
  (let* ((srt (symbol-name semantic-symref-tool))
	 (class (intern-soft (concat "semantic-symref-tool-" srt)))
	 (inst nil)
	 )
    (when (not (class-p class))
      (error "Unknown symref tool %s" semantic-symref-tool))
    (setq inst (make-instance class :searchfor name :searchtype 'symbol))
    (let ((result (semantic-symref-get-result inst)))
      (when (interactive-p)
	(if result
	    (let* ((ab (data-debug-new-buffer "*Symbol Reference ADEBUG*")))
	      (data-debug-insert-object-slots result "]"))
	  (message "Empty results.")))
      result)
    ))

;;; RESULTS
;;
;; The results class and methods provide features for accessing hits.
(defclass semantic-symref-result ()
  ((hit-alist :initarg :hit-alist
	      :type list
	      :documentation
	      "The list of hits.  Each element is a cons cell
of the form (LINE . FILENAME).")
   (hit-tags :type list
	     :documentation
	     "The list of tags with hits in them.
This list starts as empty.  Use the  `semantic-symref-hit-tags'
method to get this list.")
   )
  "The results from a symbol reference search.")

;;; SYMREF TOOLS
;;
;; The base symref tool provides something to hang new tools off of
;; for finding symbol references.
(defclass semantic-symref-tool-baseclass ()
  ((searchfor :initarg :searchfor
	      :type string
	      :documentation "The thing to search for.")
   (searchtytpe :initarg :searchtype
		:type symbol
		:documentation "The type of search to do.
Values could be `symbol, `regexp, or other.")

   )
  "Baseclass for all symbol references tools.
A symbol reference tool supplies functionality to identify the locations of
where different symbols are used.

Subclasses should be named `semantic-symref-tool-NAME', where
NAME is the name of the tool used in the configuration variable
`semantic-symref-tool'"
  :abstract t)

(defmethod semantic-symref-get-result ((tool semantic-symref-tool-baseclass))
  "Calculate the results of a search based on TOOL.
The symref TOOL should already contain the search criteria."
  (let ((answer (semantic-symref-perform-search tool))
	)

    (when answer
      (semantic-symref-result (oref tool searchfor)
			      :hit-alist answer))
    ))

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-baseclass))
  "Base search for symref tools should throw an error."
  (error "Symref tool objects must implement `semantic-symref-perform-search'"))

(defmethod semantic-symref-parse-tool-output ((tool semantic-symref-tool-baseclass)
					      outputbuffer)
  "Parse the entire OUTPUTBUFFER of a symref tool.
Calls the method `semantic-symref-parse-tool-output-one-line' over and
over until it returns nil."
  (save-excursion
    (set-buffer outputbuffer)
    (goto-char (point-min))
    (let ((result nil)
	  (hit nil))
      (while (setq hit (semantic-symref-parse-tool-output-one-line tool))
	(setq result (cons hit result)))
      (nreverse result)))
  )

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-baseclass))
  "Base tool output parser is not implemented."
  (error "Symref tool objects must implement `semantic-symref-parse-tool-output-one-line'"))

;;; GREP
;;
;; The symref GREP tool uses grep in a project to find symbol references.
;; This is a lowest-common-denominator tool with sucky performance that
;; can be used in small projects to find symbol references.

(defclass semantic-symref-tool-grep (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using grep.
This tool uses EDE to find he root of the project, then executes
find-grep in the project.  The output is parsed for hits
and those hits returned.")

(eval-when-compile (require 'ede))

(defvar semantic-symref-filepattern-alist
  '((c-mode . "\\.[ch]$")
    (c++-mode . "\\.([cChH]|[ch](pp|\\+\\+))$")
    (emacs-lisp-mode . "*.el")
    )
  "List of major modes and file extension pattern regexp.
See find -regex man page for format.")

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Base search for symref tools should throw an error."
  ;; Find the root of the project, and do a find-grep...
  (let* ((rootproj (when (and (featurep 'ede) ede-minor-mode)
		     (ede-toplevel)))
	 (rootdir (if rootproj
		      (ede-project-root-directory rootproj)
		    default-directory))
	 ;; Find the file patterns to use.
	 (pat (cdr (assoc major-mode semantic-symref-filepattern-alist)))
	 (b (get-buffer-create "*Semantic SymRef*"))
	 (ans nil)
	 )
    (save-excursion
      (set-buffer b)
      (erase-buffer)
      (setq default-directory rootdir)
      ;; find . -type f -print0 | xargs -0 -e grep -nH -e 
      (call-process "sh" nil b nil
		    "-c"
		    (concat "find "
			    default-directory
			    " -type f -name \""
			    pat "\" -print0 "
			    "| xargs -0 -e grep -nH -e "
			    "'\\<" (oref tool searchfor) "\\>'")
		    )
      )
    (setq ans (semantic-symref-parse-tool-output tool b))
    ;; Return the answer
    ans))

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-grep))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (when (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
    (cons (string-to-number (match-string 2))
	  (match-string 1))
    ))

(provide 'semantic-symref)
;;; semantic-symref.el ends here
