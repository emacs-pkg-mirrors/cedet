;;; bovine-debug.el --- Debugger support for bovinator

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: bovine-debug.el,v 1.1 2003/02/13 02:38:54 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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
;; 
;;; Commentary:
;;
;; Implementation of the semantic debug support framework for the
;; bovine parser.
;;

(require 'semantic-debug)

;;; Code:

;;; Support a frame for the Bovinator
;;
(defclass semantic-bovine-debug-frame (semantic-debug-frame)
  ((nonterm :initarg :nonterm
	    :type symbol
	    :documentation
	    "The name of the semantic nonterminal for this frame.")
   (rule :initarg :rule
	 :type number
	 :documentation
	 "The index into NONTERM's rule list.  0 based.")
   (match :initarg :match
	  :type number
	  :documentation
	  "The index into NONTERM's RULE's match.  0 based..")
   (collection :initarg :collection
	       :type list
	       :documentation
	       "List of things matched so far.")
   (lextoken :initarg :lextoken
	     :type list
	     :documentation
	     "A Token created by `semantic-lex-token'.
This is the lexical token being matched by the parser.")
   )
  "Debugger frame representation for the bovinator.")

;;;###autoload
(defun semantic-bovine-debug-create-frame (nonterm rule match collection
						   lextoken)
  "Create one bovine frame.
NONTERM is the name of a rule we are currently parsing.
RULE is the index into the list of rules in NONTERM.
MATCH is the index into the list of matches in RULE.
For example:
  this: that
      | other thing
      | here
      ;
The NONTERM is THIS.
The RULE is for \"thing\" is 1.
The MATCH for \"thing\" is 1.
COLLECTION is a list of `things' that have been matched so far.
LEXTOKEN, is a token returned by the lexer which is being matched."
  (let ((frame (semantic-bovine-debug-frame "frame"
					    :nonterm nonterm
					    :rule rule
					    :match match
					    :collection collection
					    :lextoken lextoken)))
    (semantic-debug-set-frame semantic-debug-current-interface
			      frame)
    frame))

(defmethod semantic-debug-frame-highlight ((frame semantic-debug-frame))
  "Highlight one parser frame."
  (let* ((nonterm (oref frame nonterm))
	 (pb (oref semantic-debug-current-interface parser-buffer))
	 (start (semantic-find-nonterminal-by-token 'start pb))
	)
    ;; Make sure we get a good rule name, and that it is a string
    (if (and (eq nonterm 'bovine-toplevel) start)
	(setq nonterm (semantic-token-name (car start)))
      (setq nonterm (symbol-name nonterm)))

    (semantic-debug-highlight-rule semantic-debug-current-interface
				   nonterm
				   (oref frame rule)
				   (oref frame match))
    (semantic-debug-highlight-lexical-token semantic-debug-current-interface
					    (oref frame lextoken))
    ))

(defmethod semantic-debug-frame-info ((frame semantic-debug-frame))
  "Display info about this one parser frame."
  (message "%S" (oref frame collection))
  )

;;; Parser support for the debugger
;;
;;;###autoload 
(defclass semantic-bovine-debug-parser (semantic-debug-parser)
  (
   )
  "Represents a parser and its state.")


(provide 'bovine-debug)

;;; bovine-debug.el ends here
