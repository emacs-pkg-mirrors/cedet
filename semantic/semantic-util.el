;;; semantic-util.el --- Utilities for use with semantic tag tables

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util.el,v 1.117 2003/05/29 00:49:56 zappo Exp $

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
;; API for accessing and searching nonterminal streams from the
;; Semantic Bovinator.
;;

(require 'assoc)
(require 'semantic)
(eval-when-compile
  ;; Emacs 21
  (condition-case nil
      (require 'newcomment)
    (error nil))
  )

;;; Code:

(defvar semantic-type-relation-separator-character '(".")
  "Character strings used to separate a parent/child relationship.
This list of strings are used for displaying or finding separators
in variable field dereferencing.  The first character will be used for
display.  In C, a type field is separated like this: \"type.field\"
thus, the character is a \".\".  In C, and additional value of \"->\"
would be in the list, so that \"type->field\" could be found.")
(make-variable-buffer-local 'semantic-type-relation-separator-character)

(defvar semantic-equivalent-major-modes nil
  "List of major modes which are considered equivalent.
Equivalent modes share a parser, and a set of override methods.
Setup from the BNF code generator.  A value of nil means that
the current major mode is the only one.")
(make-variable-buffer-local 'semantic-equivalent-major-modes)

;; These semanticdb calls will throw warnings in the byte compiler.
;; Doing the right thing to make them available at compile time
;; really messes up the compilation sequence.
(defun semantic-file-tag-table (file &optional checkcache)
  "Return a tag table for FILE.
If it is loaded, return the stream after making sure it's ok.
If FILE is not loaded, check to see if `semanticdb' feature exists,
   and use it to get tags from files not in memory.
If FILE is not loaded, and semanticdb is not available, find the file
   and parse it.
Optional argument CHECKCACHE is passed to `semantic-bovinate-toplevel'."
  (if (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(semantic-bovinate-toplevel checkcache))
    ;; File not loaded
    (if (and (fboundp 'semanticdb-minor-mode-p)
	     (semanticdb-minor-mode-p))
	;; semanticdb is around, use it.
	(semanticdb-file-stream file)
      ;; Get the stream ourselves.
      (save-excursion
	(set-buffer (find-file-noselect file))
	(semantic-bovinate-toplevel checkcache)))))

(semantic-alias-obsolete 'semantic-file-token-stream
			 'semantic-file-tag-table)

(defun semantic-something-to-tag-table (something)
  "Convert SOMETHING into a semantic tag table.
Something can be a tag with a valid BUFFER property, a tag table, a
buffer, or a filename.  If SOMETHING is nil, use the current buffer."
  (cond
   ;; A list of tags
   ((and (listp something)
	 (semantic-tag-p (car something)))
    something)
   ;; A buffer
   ((bufferp something)
    (save-excursion
      (set-buffer something)
      (semantic-bovinate-toplevel t)))
   ;; A Tag: Get that tag's buffer
   ((and (semantic-tag-with-position-p something)
	 (semantic-tag-buffer))
    (save-excursion
      (set-buffer (semantic-tag-buffer something))
      (semantic-bovinate-toplevel t)))
   ;; Tag with a file name in it
   ((and (semantic-tag-p something)
	 (semantic-tag-file-name something)
	 (file-exists-p (semantic-tag-file-name something)))
    (semantic-file-tag-table
     (semantic-tag-file-name something)))
   ;; A file name
   ((and (stringp something)
	 (file-exists-p something))
    (semantic-file-tag-table something nil))
   ;; A Semanticdb table
   ((and (featurep 'semanticdb)
	 (semanticdb-minor-mode-p)
	 (semanticdb-abstract-table-p something))
    (oref something tags))
   ;; Use the current buffer for nil
;;   ((null something)
;;    (semantic-bovinate-toplevel t))
   ;; don't know what it is
   (t nil)))

(semantic-alias-obsolete 'semantic-something-to-stream
			 'semantic-something-to-tag-table)

;;; Recursive searching through dependency trees
;;
;; This will depend on the general searching APIS defined above.
;; but will add full recursion through the dependencies list per
;; stream.
(defun semantic-recursive-find-nonterminal-by-name (name buffer)
  "Recursively find the first occurrence of NAME.
Start search with BUFFER.  Recurse through all dependencies till found.
The return item is of the form (BUFFER TOKEN) where BUFFER is the buffer
in which TOKEN (the token found to match NAME) was found.

THIS ISN'T USED IN SEMANTIC.  DELETE ME SOON.
"
  (save-excursion
    (set-buffer buffer)
    (let* ((stream (semantic-bovinate-toplevel))
	   (includelist (or (semantic-find-tags-by-class 'include stream)
			    "empty.silly.thing"))
	   (found (semantic-find-first-tag-by-name name stream))
	   (unfound nil))
      (while (and (not found) includelist)
	(let ((fn (semantic-find-dependency (car includelist))))
	  (if (and fn (not (member fn unfound)))
	      (save-excursion
		(set-buffer (find-file-noselect fn))
		(message "Scanning %s" (buffer-file-name))
		(setq stream (semantic-bovinate-toplevel))
		(setq found (semantic-find-first-tag-by-name name stream))
		(if found
		    (setq found (cons (current-buffer) (list found)))
		  (setq includelist
			(append includelist
				(semantic-find-tags-by-class
				 'include stream))))
		(setq unfound (cons fn unfound)))))
	(setq includelist (cdr includelist)))
      found)))
(make-obsolete 'semantic-recursive-find-nonterminal-by-name
	       "Do not use this function.")
  
;;; Completion APIs
;;
;; These functions provide minibuffer reading/completion for lists of
;; nonterminals.
(defvar semantic-read-symbol-history nil
  "History for a symbol read.")

(defun semantic-read-symbol (prompt &optional default stream filter)
  "Read a symbol name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from.
FILTER is provides a filter on the types of things to complete.
FILTER must be a function to call on each element."
  (if (not default) (setq default (thing-at-point 'symbol)))
  (if (not stream) (setq stream (semantic-bovinate-toplevel)))
  (setq stream
	(if filter
	    (semantic--find-tags-by-function filter stream)
	  (semantic-brute-find-tag-standard stream)))
  (if (and default (string-match ":" prompt))
      (setq prompt
	    (concat (substring prompt 0 (match-end 0))
		    " (default: " default ") ")))
  (completing-read prompt stream nil t ""
		   'semantic-read-symbol-history
		   default))

(defun semantic-read-variable (prompt &optional default stream)
  "Read a variable name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'variable (or stream (current-buffer)))
       (error "No local variables"))))

(defun semantic-read-function (prompt &optional default stream)
  "Read a function name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tags to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'function (or stream (current-buffer)))
       (error "No local functions"))))

(defun semantic-read-type (prompt &optional default stream)
  "Read a type name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tags to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'type (or stream (current-buffer)))
       (error "No local types"))))



;;; Multi-file Token information
;;
(defvar semantic-dependency-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific to
the file being included.
This variable can also be set to a single function.  If it is a
function, it will be called with one arguments, the file to find as a
string, and  it should return the full path to that file, or nil.")
(make-variable-buffer-local `semantic-dependency-include-path)

(defun semantic-find-dependency (&optional tag)
  "Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths."
  (if (not tag)
      (setq tag (car (semantic-find-tag-by-overlay nil))))

  (if (not (eq (semantic-tag-class tag) 'include))
      (signal 'wrong-type-argument (list tag 'include)))

  ;; First, see if this file exists in the current EDE project
  (if (and (fboundp 'ede-expand-filename) ede-minor-mode
	   (ede-expand-filename (ede-toplevel)
				(semantic-tag-name tag)))
      (ede-expand-filename (ede-toplevel)
			   (semantic-tag-name tag))
  
    (let ((s (semantic-fetch-overload 'find-dependency)))
      (if s (funcall s tag)
	(save-excursion
	  (set-buffer (semantic-tag-buffer tag))
	  (let ((name (semantic-tag-name tag)))
	    (cond ((file-exists-p name)
		   (expand-file-name name))
		  ((and (symbolp semantic-dependency-include-path)
			(fboundp semantic-dependency-include-path))
		   (funcall semantic-dependency-include-path name))
		  (t
		   (let ((p semantic-dependency-include-path)
			 (found nil))
		     (while (and p (not found))
		       (if (file-exists-p (concat (car p) "/" name))
			   (setq found (concat (car p) "/" name)))
		       (setq p (cdr p)))
		     found)))))))))
(put 'semantic-find-dependency 'semantic-overload 'find-dependency)

(defun semantic-find-nonterminal (&optional token parent)
  "Find the location of TOKEN.
TOKEN may be a stripped element, in which case PARENT specifies a
parent token that has position information.
Different behaviors are provided depending on the type of token.
For example, dependencies (includes) will seek out the file that is
depended on, and functions will move to the specified definition."
  (if (not token)
      (setq token (car (semantic-find-tag-by-overlay nil))))
  (if (and (eq (semantic-tag-class token) 'include)
	   (let ((f (semantic-find-dependency token)))
	     (if f (find-file f))))
      nil
    (let ((s (semantic-fetch-overload 'find-nonterminal)))
      (if s (funcall s token parent)
	(if (semantic-tag-buffer token)
	    ;; If the token has no buffer, it may be deoverlayed.
	    ;; Assume the tool doing the finding knows that we came
	    ;; in from a database, and use the current buffer.
	    (set-buffer (semantic-tag-buffer token)))
	(if (semantic-tag-with-position-p token)
	    ;; If it's a number, go there
	    (goto-char (semantic-tag-start token))
	  ;; Otherwise, it's a trimmed vector, such as a parameter,
	  ;; or a structure part.
	  (if (not parent)
	      nil
	    (if (semantic-tag-with-position-p parent)
		(progn
		  (if (semantic-tag-buffer parent)
		      ;; If this parent tag has no buffer, then it
		      ;; may be deoverlayed.
		      (set-buffer (semantic-tag-buffer parent)))
		  (goto-char (semantic-tag-start parent))
		  ;; Here we make an assumption that the text returned by
		  ;; the bovinator and concocted by us actually exists
		  ;; in the buffer.
		  (re-search-forward (semantic-tag-name token) nil t)))))))))
(put 'semantic-find-nonterminal 'semantic-overload 'find-nonterminal)

(defun semantic-find-documentation (&optional token nosnarf)
  "Find documentation from TOKEN and return it as a clean string.
TOKEN might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TOKEN's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the flex token for it.
If nosnarf if 'flex, then only return the flex token."
  (if (not token)
      (setq token (car (semantic-find-tag-by-overlay nil))))
  (let ((s (semantic-fetch-overload 'find-documentation)))
    (if s (funcall s token nosnarf)
      ;; No override.  Try something simple to find documentation nearby
      (save-excursion
	(set-buffer (semantic-tag-buffer token))
	(semantic-find-nonterminal token)
	(or
	 ;; Is there doc in the token???
	 (if (semantic-tag-docstring token)
	     (if (stringp (semantic-tag-docstring token))
		 (semantic-tag-docstring token)
	       (goto-char (semantic-tag-docstring token))
	       (semantic-find-doc-snarf-comment nosnarf)))
	 ;; Check just before the definition.
	 (save-excursion
	   (re-search-backward comment-start-skip nil t)
	   (if (not (semantic-brute-find-tag-by-position
		     (point) (current-buffer) t))
	       ;; We found a comment that doesn't belong to the body
	       ;; of a function.
	       (semantic-find-doc-snarf-comment nosnarf)))
	 ;;  Lets look for comments either after the definition, but before code:
	 ;; Not sure yet.  Fill in something clever later....
	 nil
	 )))))
(put 'semantic-find-documentation 'semantic-overload 'find-documentation)


(defun semantic-find-doc-snarf-comment (nosnarf)
  "Snarf up the comment at POINT for `semantic-find-documentation'.
Attempt to strip out comment syntactic sugar.
Argument NOSNARF means don't modify the found text.
If NOSNARF is 'flex, then return the flex token."
  (let* ((semantic-ignore-comments nil)
	 (semantic-lex-analyzer #'semantic-comment-lexer))
    (if (eq nosnarf 'flex)
	(car (semantic-lex (point) (1+ (point))))
      (let ((ct (semantic-lex-token-text
		 (car (semantic-lex (point) (1+ (point)))))))
	(if nosnarf
	    nil
	  ;; ok, try to clean the text up.
	  ;; Comment start thingy
	  (while (string-match (concat "^\\s-*" comment-start-skip) ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; Arbitrary punctuation at the beginning of each line.
	  (while (string-match "^\\s-*\\s.+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; End of a block comment.
	  (if (and block-comment-end (string-match block-comment-end ct))
	      (setq ct (concat (substring ct 0 (match-beginning 0))
			       (substring ct (match-end 0)))))
	  ;; In case it's a real string, STRIPIT.
	  (while (string-match "\\s-*\\s\"+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0))))))
	;; Now return the text.
	ct))))

(defun semantic-prototype-file (buffer)
  "Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in."
  (let ((s (semantic-fetch-overload 'prototype-file)))
    (if s
	(funcall s buffer)
      ;; Else, perform some default behaviors
      (if (and (fboundp 'ede-header-file) ede-minor-mode)
	  (save-excursion
	    (set-buffer buffer)
	    (ede-header-file))
	;; No EDE options for a quick answer.  Search.
	(save-excursion
	  (set-buffer buffer)
	  (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
	      (match-string 1)))))))


;;;; Mode-specific Token information
;;

(defun semantic-nonterminal-protection (token &optional parent)
  "Return protection information about TOKEN with optional PARENT.
This function returns on of the following symbols:
   nil        - No special protection.  Language dependent.
   'public    - Anyone can access this TOKEN.
   'private   - Only methods in the local scope can access TOKEN.
   'protected - Like private for outside scopes, like public for child
                classes.
Some languages may choose to provide additional return symbols specific
to themselves.  Use of this function should allow for this.

The default behavior (if not overridden with `nonterminal-protection'
is to return a symbol based on type modifiers."
  (let* ((s (semantic-fetch-overload 'nonterminal-protection)))
    (if s (funcall s token parent)
      (semantic-nonterminal-protection-default token parent))))

(defun semantic-nonterminal-protection-default (token &optional parent)
  "Return the protection of TOKEN as a child of PARENT default action.
See `semantic-nonterminal-protection'."
  (let ((mods (semantic-tag-modifiers token))
	(prot nil))
    (while (and (not prot) mods)
      (if (stringp (car mods))
	  (let ((s (car mods)))
	    (setq prot
		  ;; A few silly defaults to get things started.
		  (cond ((or (string= s "public")
			     (string= s "extern")
			     (string= s "export"))
			 'public)
			((string= s "private")
			 'private)
			((string= s "protected")
			 'protected)))))
      (setq mods (cdr mods)))
    prot))

(defun semantic-nonterminal-abstract (token &optional parent)
  "Return non nil if TOKEN is abstract.
Optional PARENT is the parent token of TOKEN.
In UML, abstract methods and classes have special meaning and behavior
in how methods are overridden.  In UML, abstract methods are italicized.

The default behavior (if not overridden with `nonterminal-abstract'
is to return true if `abstract' is in the type modifiers."
  (let* ((s (semantic-fetch-overload 'nonterminal-abstract)))
    (if s (funcall s token parent)
      (semantic-nonterminal-abstract-default token parent))))

(defun semantic-nonterminal-abstract-default (token &optional parent)
  "Return non-nil if TOKEN is abstract as a child of PARENT default action.
See `semantic-nonterminal-abstract'."
  (let ((mods (semantic-tag-modifiers token))
	(abs nil))
    (while (and (not abs) mods)
      (if (stringp (car mods))
	  (setq abs (or (string= (car mods) "abstract")
			(string= (car mods) "virtual"))))
      (setq mods (cdr mods)))
    abs))

(defun semantic-nonterminal-leaf (token &optional parent)
  "Return non nil if TOKEN is leaf.
Optional PARENT is the parent token of TOKEN.
In UML, leaf methods and classes have special meaning and behavior.

The default behavior (if not overridden with `nonterminal-leaf'
is to return true if `leaf' is in the type modifiers."
  (let* ((s (semantic-fetch-overload 'nonterminal-leaf)))
    (if s (funcall s token parent)
      (semantic-nonterminal-leaf-default token parent))))

(defun semantic-nonterminal-leaf-default (token &optional parent)
  "Return non-nil if TOKEN is leaf as a child of PARENT default action.
See `semantic-nonterminal-leaf'."
  (let ((mods (semantic-tag-modifiers token))
	(leaf nil))
    (while (and (not leaf) mods)
      (if (stringp (car mods))
	  ;; Use java FINAL as example default.  There is none
	  ;; for C/C++
	  (setq leaf (string= (car mods) "final")))
      (setq mods (cdr mods)))
    leaf))

(define-overload semantic-nonterminal-static (token &optional parent)
  "Return non nil if TOKEN is static.
Optional PARENT is the parent token of TOKEN.
In UML, static methods and attributes mean that they are allocated
in the parent class, and are not instance specific.
UML notation specifies that STATIC entries are underlined.")

(defun semantic-nonterminal-static-default (token &optional parent)
  "Return non-nil if TOKEN is static as a child of PARENT default action.
See `semantic-nonterminal-static'."
  (let ((mods (semantic-tag-modifiers token))
	(static nil))
    (while (and (not static) mods)
      (if (stringp (car mods))
	  (setq static (string= (car mods) "static")))
      (setq mods (cdr mods)))
    static))

(defun semantic-nonterminal-full-name (token &optional stream-or-buffer)
  "Return the fully qualified name of TOKEN in the package hierarchy.
STREAM-OR-BUFFER can be anything convertable by `semantic-something-to-stream',
but must be a toplevel semantic token stream that contains TOKEN.
A Package Hierarchy is defined in UML by the way classes and methods
are organized on disk.  Some language use this concept such that a
class can be accessed via it's fully qualified name, (such as Java.)
Other languages qualify names within a Namespace (such as C++) which
result in a different package like structure.  Languages which do not
override this function with `nonterminal-full-name' will use
`semantic-token-name'.  Override functions only need to handle
STREAM-OR-BUFFER with a token stream value, or nil."
  (let* ((s (semantic-fetch-overload 'nonterminal-full-name))
	 (stream (semantic-something-to-tag-table (or stream-or-buffer token))))
    (if s (funcall s token stream)
      (semantic-nonterminal-full-name-default token stream))))

(defun semantic-nonterminal-full-name-default (token stream)
  "Default method for `semantic-nonterminal-full-name'.
Return the name of TOKEN found in the toplevel STREAM."
  (semantic-tag-name token))


;;; Do some fancy stuff with overlays
;;
(defun semantic-highlight-token (token &optional face)
  "Specify that TOKEN should be highlighted.
Optional FACE specifies the face to use."
  (let ((o (semantic-tag-overlay token)))
    (semantic-overlay-put o 'old-face
			  (cons (semantic-overlay-get o 'face)
				(semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'face (or face 'highlight))
    ))

(defun semantic-unhighlight-token (token)
  "Unhighlight TOKEN, restoring it's previous face."
  (let ((o (semantic-tag-overlay token)))
    (semantic-overlay-put o 'face (car (semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'old-face (cdr (semantic-overlay-get o 'old-face)))
    ))

(defun semantic-momentary-unhighlight-token (token)
  "Unhighlight TOKEN, restoring it's previous face."
  (semantic-unhighlight-token token)
  (remove-hook 'pre-command-hook
	       `(lambda () (semantic-momentary-unhighlight-token ',token))))

(defun semantic-momentary-highlight-token (token &optional face)
  "Highlight TOKEN, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used."
  (semantic-highlight-token token face)
  (add-hook 'pre-command-hook
	    `(lambda () (semantic-momentary-unhighlight-token ',token))))

(defun semantic-set-token-face (token face)
  "Specify that TOKEN should use FACE for display."
  (semantic-overlay-put (semantic-tag-overlay token) 'face face))

(defun semantic-set-token-invisible (token &optional visible)
  "Enable the text in TOKEN to be made invisible.
If VISIBLE is non-nil, make the text visible."
  (semantic-overlay-put (semantic-tag-overlay token) 'invisible
			(not visible)))

(defun semantic-token-invisible-p (token)
  "Return non-nil if TOKEN is invisible."
  (semantic-overlay-get (semantic-tag-overlay token) 'invisible))

(defun semantic-set-token-intangible (token &optional tangible)
  "Enable the text in TOKEN to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-put (semantic-tag-overlay token) 'intangible
			(not tangible)))

(defun semantic-token-intangible-p (token)
  "Return non-nil if TOKEN is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-get (semantic-tag-overlay token) 'intangible))

(defun semantic-overlay-signal-read-only
  (overlay after start end &optional len)
  "Hook used in modification hooks to prevent modification.
Allows deletion of the entire text.
Argument OVERLAY, AFTER, START, END, and LEN are passed in by the system."
  ;; Stolen blithly from cpp.el in Emacs 21.1
  (if (and (not after)
	   (or (< (semantic-overlay-start overlay) start)
	       (> (semantic-overlay-end overlay) end)))
      (error "This text is read only")))

(defun semantic-set-token-read-only (token &optional writable)
  "Enable the text in TOKEN to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable.
instead of read-only."
  (let ((o (semantic-tag-overlay token))
	(hook (if writable nil '(semantic-overlay-signal-read-only))))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-put o 'read-only (not writable))
      (semantic-overlay-put o 'modification-hooks hook)
      (semantic-overlay-put o 'insert-in-front-hooks hook)
      (semantic-overlay-put o 'insert-behind-hooks hook))))

(defun semantic-token-read-only-p (token)
  "Return non-nil if the current TOKEN is marked read only."
  (let ((o (semantic-tag-overlay token)))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-get o 'read-only)
      (member 'semantic-overlay-signal-read-only
              (semantic-overlay-get o 'modification-hooks)))))

(defun semantic-narrow-to-token (token)
  "Narrow to the region specified by TOKEN."
  (narrow-to-region (semantic-tag-start token)
		    (semantic-tag-end token)))

(defmacro semantic-with-buffer-narrowed-to-current-token (&rest body)
  "Execute BODY with the buffer narrowed to the current nonterminal."
  `(save-restriction
     (semantic-narrow-to-token (semantic-current-tag))
     ,@body))
(put 'semantic-with-buffer-narrowed-to-current-token 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-current-token
	      (def-body))))

(defmacro semantic-with-buffer-narrowed-to-token (token &rest body)
  "Narrow to TOKEN, and execute BODY."
  `(save-restriction
     (semantic-narrow-to-token ,token)
     ,@body))
(put 'semantic-with-buffer-narrowed-to-token 'lisp-indent-function 1)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-token
	      (def-body))))

;;; Interactive Functions for bovination
;;
(defun semantic-describe-token (&optional token)
  "Describe TOKEN in the minibuffer.
If TOKEN is nil, describe the token under the cursor."
  (interactive)
  (if (not token) (setq token (semantic-current-tag)))
  (semantic-bovinate-toplevel t)
  (if token (message (semantic-format-tag-summarize token))))


;;; Putting keys on tokens.
;;
(defun semantic-add-label (label value &optional token)
  "Add a LABEL with VALUE on TOKEN.
If TOKEN is not specified, use the token at point."
  (interactive "sLabel: \nXValue (eval): ")
  (if (not token)
      (progn
	(semantic-bovinate-toplevel t)
	(setq token (semantic-current-tag))))
  (semantic--tag-put-property token (intern label) value)
  (message "Added label %s with value %S" label value))

(defun semantic-show-label (label &optional token)
  "Show the value of LABEL on TOKEN.
If TOKEN is not specified, use the token at point."
  (interactive "sLabel: ")
  (if (not token)
      (progn
	(semantic-bovinate-toplevel t)
	(setq token (semantic-current-tag))))
  (message "%s: %S" label (semantic--tag-get-property token (intern label))))


;;; Hacks
;;
;; Some hacks to help me test these functions
(defun semantic-current-token (p)
  "Display the current token.
Argument P is the point to search from in the current buffer."
  (interactive "d")
  (let ((tok (semantic-brute-find-innermost-tag-by-position
	      p (current-buffer))))
    (message (mapconcat 'semantic-abbreviate-nonterminal tok ","))
    (car tok))
  )

(defun semantic-hack-search ()
  "Display info about something under the cursor using generic methods."
  (interactive)
  (let (
	;(name (thing-at-point 'symbol))
	(strm (cdr (semantic-bovinate-toplevel)))
	(res nil))
;    (if name
	(setq res
;	      (semantic-find-nonterminal-by-name name strm)
;	      (semantic-find-nonterminal-by-type name strm)
;	      (semantic-recursive-find-nonterminal-by-name name (current-buffer))
	      (semantic-brute-find-tag-by-position (point) strm)
	      
	      )
;	)
    (if res
	(progn
	  (pop-to-buffer "*SEMANTIC HACK RESULTS*")
	  (require 'pp)
	  (erase-buffer)
	  (insert (pp-to-string res) "\n")
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer))
      (message "nil"))))

(defun semantic-assert-valid-token (tok)
  "Assert that TOK is a valid token."
  (if (semantic-tag-p tok)
      (if (semantic-tag-with-position-p tok)
	  (let ((o  (semantic-tag-overlay tok)))
	    (if (and (semantic-overlay-p o)
		     (not (semantic-overlay-live-p o)))
		(let ((debug-on-error t))
		  (error "Tag %s is invalid!"))
	      ;; else, tag is OK.
	      ))
	;; Positionless tags are also ok.
	)
    (let ((debug-on-error t))
      (error "Not a semantic tag: %S" tok))))

(defun semantic-sanity-check (&optional cache over notfirst)
  "Perform a sanity check on the current buffer.
The buffer's set of overlays, and those overlays found via the cache
are verified against each other.
CACHE, and OVER are the semantic cache, and the overlay list.
NOTFIRST indicates that this was not the first call in the recursive use."
  (interactive)
  (if (and (not cache) (not over) (not notfirst))
      (setq cache semantic-toplevel-bovine-cache
	    over (semantic-overlays-in (point-min) (point-max))))
  (while cache
    (let ((chil (semantic-tag-components-with-overlays (car cache))))
      (if (not (memq (semantic-tag-overlay (car cache)) over))
	  (message "Tag %s not in buffer overlay list."
		   (semantic-format-tag-concise-prototype (car cache))))
      (setq over (delq (semantic-tag-overlay (car cache)) over))
      (setq over (semantic-sanity-check chil over t))
      (setq cache (cdr cache))))
  (if (not notfirst)
      ;; Strip out all overlays which aren't semantic overlays
      (let ((o nil))
	(while over
	  (when (and (semantic-overlay-get (car over) 'semantic)
		     (not (eq (semantic-overlay-get (car over) 'semantic)
			      'unmatched)))
	    (setq o (cons (car over) o)))
	  (setq over (cdr over)))
	(message "Remaining overlays: %S" o)))
  over)

(provide 'semantic-util)

;;; Minor modes
;;
(require 'semantic-util-modes)

;;; semantic-util.el ends here
