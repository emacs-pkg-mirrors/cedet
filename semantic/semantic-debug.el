;;; semantic-debug.el --- Language Debugger framework

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-debug.el,v 1.1 2003/02/04 02:19:56 zappo Exp $

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
;; To provide better support for debugging parsers, this framework
;; provides the interface for debugging.  The work of parsing and
;; controlling and stepping through the parsing work must be implemented
;; by the parser.
;;
;; Fortunatly, the nature of language support files means that the parser
;; may not need to be instrumented first.
;;
;; The debugger uses EIEIO objects.  One object controls the user
;; interface, including stepping, data-view, queries.  A second
;; object implemented here represents the parser itself. A third represents
;; a parser independent frame which knows how to highlight the parser buffer.
;; Each parser must implement the interface and override any methods as needed.
;;

(require 'semantic)
(require 'eieio)

;;; Code:
(defvar semantic-debug-parser-source nil
  "For any buffer, the file name (no path) of the parser.
This would be a parser for a specific language, not the source
to one of the parser generators.")

;;; User Interface Portion
;;
(defclass semantic-debug-interface ()
  ((parser-buffer :initarg :parser-buffer
		  :type buffer
		  :documentation
		  "The buffer containing the parser we are debugging.")
   (parser-local-map :initarg :parser-local-map
		     :type keymap
		     :documentation
		     "The local keymap originally in the PARSER buffer.")
   (source-buffer :initarg :source-buffer
		  :type buffer
		  :documentation
		  "The buffer containing the source we are parsing.
The :parser-buffer defines a parser that can parse the text in the
:source-buffer.")
   (source-local-map :initarg :source-local-map
		     :type keymap
		     :documentation
		     "The local keymap originally in the SOURCE buffer.")
   (data-buffer :initarg :data-buffer
		:type buffer
		:documentation
		"Buffer being used to display some useful data.
These buffers are brought into view when layout occurs.")
   (frame-type :allocation :class
	       :initform 'semantic-debug-frame
	       :documentation
	       "The class used to represent a frame.")
   )
  "Controls action when in `semantic-debug-mode'")

;; Methods
(defmethod semantic-debug-interface-layout ((iface semantic-debug-interface))
  "Layout windows in the current frame to facilitate debugging."
  (delete-other-windows)
  ;; Deal with the data buffer
  (when (oref iface data-buffer)
    (let ((lines (/ (frame-height (selected-frame)) 3))
	  (cnt (save-excursion
		 (set-buffer (oref iface data-buffer))
		 (count-lines (point-min) (point-max))))
	  )
      ;; Set the number of lines to 1/3, or the size of the data buffer.
      (if (< cnt lines) (setq cnt lines))
      
      (split-window-vertically cnt)
      (switch-to-buffer (oref iface data-buffer))
      )
    (other-window 1))
  ;; Parser
  (switch-to-buffer (oref iface parser-buffer))
  (split-window-vertically)
  (other-window 1)
  ;; Source
  (switch-to-buffer (oref iface source-buffer))
  )

(defclass semantic-debug-frame ()
  ((rule :initarg :rule
	 :type string
	 :documentation
	 "The name of the semantic rule for this frame.")
   (match :initarg :match
	  :type number
	  :documentation
	  "The index into the current list of matches for `rule'.")
   (lextoken :initarg :lextoken
	     :type list
	     :documentation
	     "A Token created by `semantic-lex-token'.
This is the lexical token being matched by the parser.")
   )
  "One frame representation.")

(defmethod semantic-debug-create-frame ((iface semantic-debug-interface)
					rule matchindex lextoken)
  "Create one frame.
RULE is the name of a rule we are currently parsing.
MATCHINDEX is the index into the list of matches.
For example:
  this: that other thing ;
The rule is THIS.
The match item THAT is the 1st entry with an index of 0
The third argument is LEXTOKEN, which is a token returned by the lexer
which is being matched."
  ;; Simple, yes. but it may become more complex.
  (semantic-debug-frame rule :rule rule :match matchindex
			:lextoken lextoken)
  )

(defmethod semantic-debug-frame-highlight ((iface semantic-debug-frame))
  "Highlight one parser frame."
  
  )

;;; Major Mode
;;
(defvar semantic-debug-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "n" 'semantic-debug-next)
    (define-key km "s" 'semantic-debug-step)
    (define-key km "u" 'semantic-debug-up)
    (define-key km "d" 'semantic-debug-down)
    (define-key km "f" 'semantic-debug-fail-match)
    (define-key km "h" 'semantic-debug-print-state)
    (define-key km "s" 'semantic-debug-jump-to-source)
    (define-key km "p" 'semantic-debug-jump-to-parser)
    (define-key km "q" 'semantic-debug-quit)
    (define-key km "g" 'semantic-debug-go)
    (define-key km "b" 'semantic-debug-break)
    
    km)
  "Keymap used when in semantic-debug-node.")

(defvar semantic-debug-current-interface nil
  "The debugger interface currently active for this buffer.")

(defvar semantic-debug-current-parser nil
  "The parser current active for this buffer.")

(defun semantic-debug-mode (onoff)
  "Turn `semantic-debug-mode' on and off.
Argument ONOFF is non-nil when we are entering debug mode."
  (if onoff
      (let ((parserb (semantic-debug-find-parser-source))
	    (pmap nil)
	    )
	;; Turn it on
	(save-excursion
	  (set-buffer parserb)
	  ;; Get the map
	  (setq pmap (current-local-map))
	  ;; Install our map onto this buffer
	  (use-local-map semantic-debug-mode-map)
	  )
	(setq semantic-debug-current-interface
	      (semantic-debug-interface
	       "Debug Interface"
	       :parser-buffer parserb
	       :parser-local-map pmap
	       :source-buffer (current-buffer)
	       :source-local-map (current-local-map)
	       ))
	;; Use our map in the source buffer also
	(use-local-map semantic-debug-mode-map)
	;; Hooks
	(run-hooks 'semantic-debug-mode-hooks)
	)
    ;; Restore old mode information
    (save-excursion
      (set-buffer
       (oref semantic-debug-current-interface parser-buffer))
      (use-local-map
       (oref semantic-debug-current-interface parser-local-map))
      )
    (save-excursion
      (set-buffer
       (oref semantic-debug-current-interface source-buffer))
      (use-local-map
       (oref semantic-debug-current-interface source-local-map))
      )
    (run-hooks 'semantic-debug-exit-hooks)
    ))

(defun semantic-debug ()
  "Parse the current buffer and run in debug mode."
  (interactive)
  (if semantic-debug-current-interface
      (error "You are already in a debug session."))
  (semantic-debug-mode t)
  ;; Kick off a parse.  Make sure it does work.
  (semantic-clear-toplevel-cache)
  (semantic-bovinate-toplevel)
  )

(defun semantic-debug-find-parser-source ()
  "Return a buffer containing the parser source file for the current buffer.
The parser needs to be on the load path, or this routine returns nil."
  (if (not semantic-debug-parser-source)
      (error "No parser is associated with this buffer"))
  (let ((lp load-path)
	(parser nil))
    (while (and lp (not parser))
      (let ((tmp (concat (car lp) "/" semantic-debug-parser-source)))
	(if (file-exists-p tmp)
	    (setq parser tmp)))
      (setq lp (cdr lp)))
    (find-file-noselect parser)))

;;; Debugger commands
;;
(defun semantic-debug-next ()
  "Perform one parser operation.
In the recursive parser, this steps past one match rule.
In other parsers, this may be just like `semantic-debug-step'."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-next parser)
    )
  )

(defun semantic-debug-step ()
  "Perform one parser operation."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-step parser)
    )
  )

(defun semantic-debug-up ()
  "Move highlighting representation up one level."
  (interactive)
  (message "Not implemented yet.")
  )

(defun semantic-debug-down ()
  "Move highlighting representation down one level."
  (interactive)
  (message "Not implemented yet.")
  )

(defun semantic-debug-fail-match ()
  "Artificially fail the current match."
  (interactive)
  (message "Not implemented yet.") 
 )

(defun semantic-debug-print-state ()
  "Show interesting parser state."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-print-state parser)
    )
  )

(defun semantic-debug-jump-to-source ()
  "Move cursor to the source code being parsed at the current lexical token."
  (interactive)
  (let* ((interface semantic-debug-current-interface)
	 (buf (oref interface source-buffer)))
    (if (get-buffer-window buf)
	(progn
	  (select-frame (window-frame (get-buffer-window buf)))
	  (select-window (get-buffer-window buf)))
      ;; Technically, this should do a window layout operation
      (switch-to-buffer buf))
    )
  )

(defun semantic-debug-jump-to-parser ()
  "Move cursor to the parser being debugged."
  (interactive)
  (let* ((interface semantic-debug-current-interface)
	 (buf (oref interface parser-buffer)))
    (if (get-buffer-window buf)
	(progn
	  (select-frame (window-frame (get-buffer-window buf)))
	  (select-window (get-buffer-window buf)))
      ;; Technically, this should do a window layout operation
      (switch-to-buffer buf))
    )
  )

(defun semantic-debug-quit ()
  "Exit debug mode, blowing all stack, and leaving the parse incomplete.
Do not update any tokens already parsed."
  (interactive)
  (semantic-debug-mode nil)
  (exit-recursive-edit)
  )

(defun semantic-debug-go ()
  "Continue parsing till finish or breakpoint."
  (interactive)
  (let ((parser semantic-debug-current-parser))
    (semantic-debug-parser-go parser)
    )
  )

(defun semantic-debug-break ()
  "Set a breakpoint at the current rule location."
  (interactive)
  (let ((parser semantic-debug-current-parser)
	;; Get the location as semantic tokens.
	(location (semantic-current-nonterminal))
	)
    (if location
	(semantic-debug-parser-break parser location)
      (error "Not on a rule."))
    )
  )


;;; Debugger superclass
;;
(defclass semantic-debug-parser ()
  (
   )
  "Represents a parser and its state."
  :abstract t)

(defmethod semantic-debug-parser-next ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (error "NEXT not implemented in %s" (object-name parser))
  )

(defmethod semantic-debug-parser-step ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (error "STEP not implemented in %s" (object-name parser))
  )

(defmethod semantic-debug-parser-print-state ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (error "PRINT-STATE not implemented in %s" (object-name parser))
  )

(defmethod semantic-debug-parser-go ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (error "GO not implemented in %s" (object-name parser))
  )

(defmethod semantic-debug-parser-break ((parser semantic-debug-parser))
  "Execute next for this PARSER."
  (error "BREAK not implemented in %s" (object-name parser))
  )

;; Stack stuff
(defmethod semantic-debug-parser-fames ((parser semantic-debug-parser))
  "Return a list of frames for the current parser.
A frame is of the form:
  ( .. .what ? .. )
"
  (error "Parser has not implemented frame values.")
  )


(provide 'semantic-debug)

;;; semantic-debug.el ends here
