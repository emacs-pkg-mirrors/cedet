;;; semantic-util.el --- Utilities for use with semantic token streams

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util.el,v 1.19 2000/09/19 03:43:30 zappo Exp $

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


;;; Code:

;;; Simple APIs
;;
;; These macros extract parts from the default token types as
;; described by `semantic-toplevel-bovine-table'

;; Check semantic.el for the other token information extraction functions.

(defun semantic-token-type (token)
  "Retrieve the type of TOKEN."
  (if (member (semantic-token-token token)
	      '(function variable type))
      (nth 2 token)))

(defmacro semantic-token-type-parts (token)
  "Retrieve the parts of the type TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-type-parent (token)
  "Retrieve the parent of the type TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-type-modifiers (token)
  "Retrieve the non-type modifiers of the type TOKEN."
  `(nth 5 ,token))

(defmacro semantic-token-function-args (token)
  "Retrieve the arguments of the function TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-function-modifiers (token)
  "Retrieve non-type modifiers for the function TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-function-throws (token)
  "Optional details if this function has a THROWS type.
Determines if it is available based on the length of TOKEN."
  `(if (>= (length ,token) (+ 6 3))
       (nth 5 ,token)
     nil))

(defmacro semantic-token-variable-const (token)
  "Retrieve the status of constantness from the variable TOKEN."
  `(nth 3 ,token))

(defmacro semantic-token-variable-default (token)
  "Retrieve the default value of the variable TOKEN."
  `(nth 4 ,token))

(defmacro semantic-token-variable-modifiers (token)
  "Retrieve non-type modifiers for the variable TOKEN."
  `(nth 5 ,token))

(defmacro semantic-token-variable-optsuffix (token)
  "Optional details if this variable has bit fields, or array dimentions.
Determines if it is available based on the length of TOKEN."
  `(if (>= (length ,token) (+ 7 3))
       (nth 6 ,token)
     nil))

(defmacro semantic-token-include-system (token)
 "Retrieve the flag indicating if the include TOKEN is a sysmtem include."
  `(nth 2 ,token))

;;; Searching APIs
;;
;; These functions search through lists of nonterminals which are in
;; standard form.
(defun semantic-find-nonterminal-by-name (name streamorbuffer)
  "Find a nonterminal NAME within STREAMORBUFFER.  NAME is a string."
  (let* ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	 (m (assoc name stream)))
    (if m
	m
      (let ((toklst (semantic-find-nonterminal-by-token 'type stream)))
	(while (and (not m) toklst)
	  (let ((parts (semantic-token-type-parts (car toklst))))
	    (setq m (if (listp (car parts))
			(semantic-find-nonterminal-by-name name parts)
		      (car-safe (member name parts)))
		  toklst (cdr toklst))))
	(if (not m)
	    ;; Go to dependencies, and search there.
	    nil)
	m))))

(defun semantic-find-nonterminal-by-position (position streamorbuffer
						       &optional nomedian)
  "Find a nonterminal covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil."
  (save-excursion
    (if (markerp position) (set-buffer (marker-buffer position)))
    (let* ((stream (if (bufferp streamorbuffer)
		       (save-excursion
			 (set-buffer streamorbuffer)
			 (semantic-bovinate-toplevel nil t))
		     streamorbuffer))
	   (prev nil)
	   (found nil))
      (while (and stream (not found))
	;; perfect fit
	(if (and (>= position (semantic-token-start (car stream)))
		 (<= position (semantic-token-end (car stream))))
	    (setq found (car stream))
	  ;; Median between to objects.
	  (if (and prev (not nomedian)
		   (>= position (semantic-token-end prev))
		   (<= position (semantic-token-start (car stream))))
	      (let ((median (/ (+ (semantic-token-end prev)
				  (semantic-token-start (car stream)))
			       2)))
		(setq found
		      (if (> position median)
			  (car stream)
			prev)))))
	;; Next!!!
	(setq prev (car stream)
	      stream (cdr stream)))
      found)))

(defun semantic-find-innermost-nonterminal-by-position
  (position streamorbuffer &optional nomedian)
  "Find a list of nonterminals covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable."
  (let* ((returnme nil)
	 (current (semantic-find-nonterminal-by-position
		   position streamorbuffer nomedian))
	 (nextstream (and current
			  (if (eq (semantic-token-token current) 'type)
			      (semantic-token-type-parts current)
			    nil))))
    (while nextstream
      (setq returnme (cons current returnme))
      (setq current (semantic-find-nonterminal-by-position
		     position nextstream nomedian))
      (setq nextstream (and current
			    (if (eq (semantic-token-token current) 'token)
				(semantic-token-type-parts current)
			      nil))))
    (nreverse (cons current returnme))))

(defun semantic-find-nonterminal-by-token (token streamorbuffer)
  "Find all nonterminals with a token TOKEN within STREAMORBUFFER.
TOKEN is a symbol."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	(nl nil))
    (while stream
      (if (eq token (semantic-token-token (car stream)))
	  (setq nl (cons (car stream) nl)))
      (setq stream (cdr stream)))
    (nreverse nl)))

(defun semantic-find-nonterminal-standard (streamorbuffer)
  "Find all nonterminals in STREAMORBUFFER which define simple token types."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	(nl nil))
    (while stream
      (if (member (semantic-token-token (car stream))
		  '(function variable type))
	  (setq nl (cons (car stream) nl)))
      (setq stream (cdr stream)))
    (nreverse nl)))

(defvar semantic-default-built-in-types nil
  "For a given language, a set of built-in types.")
(make-variable-buffer-local 'semantic-default-built-in-types)

(defun semantic-find-nonterminal-by-type (type streamorbuffer)
  "Find all nonterminals with type TYPE within STREAMORBUFFER.
TYPE is a string."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	(nl nil) (ts nil))
    (if (member type semantic-default-built-in-types)
	(setq nl (list (list type 'type "built in")))
      (while stream
	(setq ts (semantic-token-type (car stream)))
	(if (and (listp ts) (eq (semantic-token-token ts) 'type))
	    (setq ts (semantic-token-name ts)))
	(if (equal type ts)
	    (setq nl (cons (car stream) nl)))
	(setq stream (cdr stream))))
    (nreverse nl)))

(defun semantic-find-nonterminal-by-function (function streamorbuffer)
  "Find all nonterminals which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	(nl nil) (ts nil))
    (while stream
      (if (funcall function (car stream))
	  (setq nl (cons (car stream) nl)))
      (setq stream (cdr stream)))
    (nreverse nl)))

(defun semantic-find-nonterminal-by-function-first-match (function
							  streamorbuffer)
  "Find the first nonterminal which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list."
  (let ((stream (if (bufferp streamorbuffer)
		     (save-excursion
		       (set-buffer streamorbuffer)
		       (semantic-bovinate-toplevel nil t))
		   streamorbuffer))
	(ts nil) (found nil))
    (while (and (not found) stream)
      (if (funcall function (car stream))
	  (setq found (car stream)))
      (setq stream (cdr stream)))
    found))

;;; Bucketizing: Take and convert the tokens based on type.
(defun semantic-bucketize (tokens)
  "Sort TOKENS into a group of buckets based on type, and toss the rest.
The buckets should be organized into a form usable by `semantic-sb-buttons'."
  (let ((bins (make-vector (1+ (length semantic-symbol->name-assoc-list)) nil))
	ask toktype
	(sn semantic-symbol->name-assoc-list)
	(nsn nil)
	(num 1)
	(out nil))
    ;; Build up the bucket vector
    (while sn
      (setq nsn (cons (cons (car (car sn)) num) nsn)
	    sn (cdr sn)
	    num (1+ num)))
    ;; Place into buckets
    (while tokens
      (setq toktype (semantic-token-token (car tokens))
	    ask (assq toktype nsn)
	    num (or (cdr ask) 0))
      (aset bins num (cons (car tokens) (aref bins num)))
      (setq tokens (cdr tokens)))
    ;; Remove from buckets into a speedbar supported list.
    (setq num 1)
    (while (< num (length bins))
      (when (aref bins num)
	(setq out
	      (cons (cons
		     (cdr (nth (1- num) semantic-symbol->name-assoc-list))
		     (aref bins num))
		    out)))
      (setq num (1+ num)))
    (if (aref bins 0) (setq out (cons (cons "Misc" (aref bins 0)) out)))
    (nreverse out)))

;;; Recursive searching through dependency trees
;;
;; This will depend on the general searching APIS defined above.
;; but will add full recursion through the dependencies list per
;; stream.
(defun semantic-recursive-find-nonterminal-by-name (name buffer)
  "Recursivly find the first occurance of NAME.
Start search with BUFFER.  Recurse through all dependencies till found.
The return item is of the form (BUFFER TOKEN) where BUFFER is the buffer
in which TOKEN (the token found to match NAME) was found."
  (save-excursion
    (set-buffer buffer)
    (let* ((stream (semantic-bovinate-toplevel nil t))
	   (includelist (or (semantic-find-nonterminal-by-token 'include stream)
			    "empty.silly.thing"))
	   (found (semantic-find-nonterminal-by-name name stream))
	   (unfound nil))
      (while (and (not found) includelist)
	(let ((fn (semantic-find-dependency buffer (car includelist))))
	  (if (and fn (not (member fn unfound)))
	      (save-excursion
		(set-buffer (find-file-noselect fn))
		(message "Scanning %s" (buffer-file-name))
		(setq stream (semantic-bovinate-toplevel nil t))
		(setq found (semantic-find-nonterminal-by-name name stream))
		(if found
		    (setq found (cons (current-buffer) (list found)))
		  (setq includelist
			(append includelist
				(semantic-find-nonterminal-by-token
				 'include stream))))
		(setq unfound (cons fn unfound)))))
	(setq includelist (cdr includelist)))
      found)))
  
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
FILTER must be a function to call on each element.  (See"
  (if (not default) (setq default (thing-at-point 'symbol)))
  (if (not stream) (setq stream (semantic-bovinate-toplevel nil t)))
  (setq stream
	(if filter
	    (semantic-find-nonterminal-by-function filter stream)
	  (semantic-find-nonterminal-standard stream)))
  (completing-read prompt stream nil t ""
		   'semantic-read-symbol-history default))

(defun semantic-read-variable (prompt &optional default stream)
  "Read a variable name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'variable stream)))

(defun semantic-read-function (prompt &optional default stream)
  "Read a function name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'function stream)))

(defun semantic-read-type (prompt &optional default stream)
  "Read a type name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default (semantic-find-nonterminal-by-type 'type stream)))

;;; Behavioral APIs
;;
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.  This section permits the setting of override functions
;; for tasks of that nature, and also provides reasonable defaults.

(defvar semantic-override-table nil
  "Buffer local semantic function overrides alist.
These overrides provide a hook for a `major-mode' to override specific
behaviors with respect to generated semantic toplevel nonterminals and
things that these non-terminals are useful for.
Each element must be of the form: (SYM . FUN)
where SYM is the symbol to override, and FUN is the function to
override it with.
Available override symbols:

  SYBMOL                  PARAMETERS              DESCRIPTION
 `find-dependency'        (buffer token)           Find the dependency file
 `find-nonterminal'       (buffer token & parent)  Find token in buffer.
 `find-documentation'     (buffer token & nosnarf) Find doc comments.
 `abbreviate-nonterminal' (token & parent)         Return summery string.
 `summerize-nonterminal'  (token & parent)         Return summery string.
 `prototype-nonterminal'  (token)                  Return a prototype string.
 `prototype-file'         (buffer)                 Return a file in which
 	                                           prototypes are placed
Parameters mean:

  &      - Following parameters are optional
  buffer - The buffer in which a token was found.
  token  - The nonterminal token we are doing stuff with
  parent - If a TOKEN is stripped (of positional infomration) then
           this will be the parent token which should have positional
           information in it.")
(make-variable-buffer-local 'semantic-override-table)

(defun semantic-fetch-overload (sym)
  "Find and return the overload function for SYM."
  (let ((a (assq sym semantic-override-table)))
    (cdr a)))

(defvar semantic-dependency-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific to
the file being included.
This variable can also be set to a single function.  If it is a
function, it will be called with one arguments, the file to find as a
string, and  it should return the full path to that file, or nil.")
(make-variable-buffer-local `semantic-dependency-include-path)

(defun semantic-find-dependency (buffer token)
  "Find the filename represented from BUFFER's TOKEN.
TOKEN may be a stripped element, in which case PARENT specifies a
parent token that has positinal information.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths."
  (if (or (not (bufferp buffer)) (not token))
      (error "Semantic-find-nonterminal: specify BUFFER and TOKEN"))

  ;; First, see if this file exists in the current EDE projecy
  (if (and (fboundp 'ede-expand-filename) ede-minor-mode
	   (ede-expand-filename (ede-toplevel)
				(semantic-token-name token)))
      (ede-expand-filename (ede-toplevel)
			   (semantic-token-name token))
  
    (let ((s (semantic-fetch-overload 'find-dependency)))
      (if s (funcall s buffer token)
	(save-excursion
	  (set-buffer buffer)
	  (let ((name (semantic-token-name token)))
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

(defun semantic-find-nonterminal (buffer token &optional parent)
  "Find the location from BUFFER belonging to TOKEN.
TOKEN may be a stripped element, in which case PARENT specifies a
parent token that has position information.
Different behaviors are provided depending on the type of token.
For example, dependencies (includes) will seek out the file that is
depended on, and functions will move to the specified definition."
  (if (or (not (bufferp buffer)) (not token))
      (error "Semantic-find-nonterminal: specify BUFFER and TOKEN"))
  
  (if (and (eq (semantic-token-token token) 'include)
	   (let ((f (semantic-find-dependency buffer token)))
	     (if f (find-file f))))
      nil
    (let ((s (semantic-fetch-overload 'find-nonterminal)))
      (if s (funcall s buffer token)
	(set-buffer buffer)
	(let ((start (semantic-token-start token)))
	  (if (numberp start)
	      ;; If it's a number, go there
	      (goto-char start)
	    ;; Otherwise, it's a trimmed vector, such as a parameter,
	    ;; or a structure part.
	    (if (not parent)
		nil
	      (goto-char (semantic-token-start parent))
	      ;; Here we make an assumtion that the text returned by
	      ;; the bovinator and concocted by us actually exists
	      ;; in the buffer.
	      (re-search-forward (semantic-token-name token) nil t))))))))

(defun semantic-find-documentation (buffer token &optional nosnarf)
  "Find documentation from BUFFER/TOKEN and return it as a clean string.
TOKEN might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceeding TOKEN's definition which we
cal look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the flex token for it.
If nosnarf if 'flex, then only return the flex token."
  (if (or (not (bufferp buffer)) (not token))
      (error "Semantic-find-documentation: specify BUFFER and TOKEN"))
  (let ((s (semantic-fetch-overload 'find-documentation)))
    (if s (funcall s buffer token)
      ;; No override.  Try something simple to find documentation in
      ;; BUFFER.
      (save-excursion
	(semantic-find-nonterminal buffer token)
	(or
	 ;; Is there doc in the token???
	 (if (semantic-token-docstring token)
	     (progn (goto-char (semantic-token-docstring token))
		    (semantic-find-doc-snarf-comment nosnarf)))
	 ;; Check just before the definition.
	 (save-excursion
	   (re-search-backward comment-start-skip nil t)
	   (if (not (semantic-find-nonterminal-by-position
		     (point) (current-buffer) t))
	       ;; We found a comment that doesn't belong to the body
	       ;; of a function.
	       (semantic-find-doc-snarf-comment nosnarf)))
	 ;;  Lets look for comments either after the definition, but before code:
	 ;; Not sure yet.  Fill in something clever later....
	 nil
	 )))))

(defun semantic-find-doc-snarf-comment (nosnarf)
  "Snarf up the comment at POINT for `semantic-find-documentation'.
Attempt to strip out comment syntactic sugar.
Argument NOSNARF means don't modify the found text.
If NOSNARF is 'flex, then return the flex token."
  (if (eq nosnarf 'flex)
      (car (semantic-flex (point) (1+ (point))))
    (let ((ct (semantic-flex-text (car (semantic-flex (point) (1+ (point)))))))
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
      ct)))

(defun semantic-abbreviate-nonterminal (token &optional parent)
  "Return an abbreviated string describing TOKEN.
The abbreviation is to be short, with possible symbols indicating
the type of token, or other information.
Optional argument PARENT is the parent type if TOKEN is a detail."
  (let ((s (semantic-fetch-overload 'abbreviate-nonterminal))
	tt)
    (if s
	(funcall s token parent)
      ;; Do lots of complex stuff here.
      (let ((tok (semantic-token-token token))
	    (name (semantic-token-name token)))
	(concat name
		(cond ((eq tok 'function) "()")
		      ((eq tok 'include) "<>")
		      ((and (eq tok 'variable)
			    (semantic-token-variable-default token))
		       "=")))
	))))

(defun semantic-summerize-nonterminal (token &optional parent)
  "Summerize TOKEN in a reasonable way.
Optional argument PARENT is the parent type if TOKEN is a detail."
  (let ((s (semantic-fetch-overload 'prototype-nonterminal)))
    (if s
	(funcall s token parent)
      ;; FLESH THIS OUT MORE
      (concat (or (capitalize
		   (cdr-safe (assoc (semantic-token-token token)
				    semantic-symbol->name-assoc-list)))
		  (capitalize (symbol-name (semantic-token-token token))))
	      ": "
	      (semantic-prototype-nonterminal token)))))

(defun semantic-prototype-nonterminal (token)
  "Return a prototype for TOKEN.
This functin must be overloaded, though it need not be used."
  (let ((tt (semantic-token-token token))
	(s (semantic-fetch-overload 'summerize-nonterminal)))
    (if s
	;; Prototype is non-local
	(funcall s token)
      ;; Cococt a cheap prototype
      (let* ((tok (semantic-token-token token))
	     (type (if (member tok '(function variable type))
		       (semantic-token-type token) ""))
	     (args (cond ((eq tok 'function)
			  (semantic-token-function-args token))
			 ((eq tok 'type)
			  (semantic-token-type-parts token))
			 (t nil)))
	     (mods (cond ((eq tok 'variable)
			  (semantic-token-variable-modifiers token))
			 ((eq tok 'function)
			  (semantic-token-function-modifiers token))
			 ((eq tok 'type)
			  (semantic-token-type-modifiers token))
			 (t nil)))
	     (array (if (eq tok 'variable)
			(semantic-token-variable-optsuffix token)))
	     )
	(if (and (listp mods) mods)
	    (setq mods (concat (mapconcat (lambda (a) a) mods " ") " ")))
	(if args
	    (setq args
		  (concat " " (if (eq tok 'type) "{" "(")
			  (if (stringp (car args))
			      (mapconcat (lambda (a) a) args ",")
			    (mapconcat 'car args ","))
			  (if (eq tok 'type) "}" ")"))))
	(if (and type (listp type))
	    (setq type (car type)))
	(concat (or mods "")
		(if type (concat type " "))
		(semantic-token-name token)
		(or args "")
		(or array ""))))))

(defun semantic-prototype-file (buffer)
  "Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overriden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in."
  (let ((s (semantic-fetch-overload 'prototype-nonterminal)))
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

;;; Hacks
;;
;; Some hacks to help me test these functions
(defun semantic-current-token (p)
  "Display the curent token.
Argument P is the point to search from in the current buffer."
  (interactive "d")
  (message
   (mapconcat
    'semantic-abbreviate-nonterminal
    (semantic-find-innermost-nonterminal-by-position p (current-buffer))
    ",")))

(defun semantic-hack-search ()
  "Disply info about something under the cursor using generic methods."
  (interactive)
  (let (
	;(name (thing-at-point 'symbol))
	(strm (cdr (semantic-bovinate-toplevel nil t)))
	(res nil))
;    (if name
	(setq res
;	      (semantic-find-nonterminal-by-name name strm)
;	      (semantic-find-nonterminal-by-type name strm)
;	      (semantic-recursive-find-nonterminal-by-name name (current-buffer))
	      (semantic-find-nonterminal-by-position (point) strm)
	      
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

(provide 'semantic-util)

;;; semantic-util.el ends here
