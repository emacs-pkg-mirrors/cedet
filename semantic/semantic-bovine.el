;;; semantic-bovine.el --- LL Parser/Analyzer core.

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; X-CVS: $Id: semantic-bovine.el,v 1.4 2002/08/04 01:57:01 zappo Exp $

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
;; Semantix 1.x uses an LL parser named the "bovinator".  This parser
;; had several conveniences in it which made for parsing tags out of
;; languages with list characters easy.  This parser lives on as one
;; of many available parsers for semantic the tool.
;;
;; This parser should be used when the language is simple, such as makefiles
;; or other data-declaritive langauges.

;;; Code:

;;; Semantic Bovination
;;
;; Take a semantic token stream, and convert it using the bovinator.
;; The bovinator takes a state table, and converts the token stream
;; into a new semantic stream defined by the bovination table.
;;

(defun semantic-bovinate-region-default
  (start end &optional reparse-symbol depth returnonerror)
  "Bovinate the area between START and END, and return any tokens found.
If END needs to be extended due to a lexical token being too large,
it will be silently ignored.
Optional argument REPARSE-SYMBOL is the rule to start parsing at if it
is known.
Optional argument DEPTH specifies the lexical depth to scan.
Optional argument RETURNONERROR specifies that parsing should end
when encountering unterminaled syntax."
  (if (or (< end start) (> end (point-max)))
      (error "Invalid bounds passed to `semantic-bovinate-region'"))
  (let ((lexbits (semantic-lex start end depth))
	tokens)
    ;; Init a dump
    ;;    (if semantic-dump-parse
    ;;	      (semantic-dump-buffer-init))
    (setq tokens (semantic-bovinate-nonterminals
		  lexbits
		  reparse-symbol
		  depth
		  returnonerror))
    (nreverse tokens)))

(defsubst semantic-bovinate-symbol-nonterminal-p (sym table)
  "Return non-nil if SYM is in TABLE, indicating it is NONTERMINAL."
  ;; sym is always a sym, so assq should be ok.
  (if (assq sym table) t nil))

(defmacro semantic-bovinate-nonterminal-db-nt ()
  "Return the current nonterminal symbol.
Part of the BNF source debugger.  Depends on the existing environment
of `semantic-bovinate-nonterminal'."
  `(if nt-stack
       (car (aref (car nt-stack) 2))
     nonterminal))

(defun semantic-bovinate-nonterminal-check (stream nonterminal)
  "Check if STREAM not already parsed for NONTERMINAL.
If so abort because an infinite recursive parse is suspected."
  (or (vectorp semantic-bovinate-nonterminal-check-obarray)
      (setq semantic-bovinate-nonterminal-check-obarray
            (make-vector 13 nil)))
  (let* ((nt (symbol-name nonterminal))
         (vs (symbol-value
              (intern-soft
               nt semantic-bovinate-nonterminal-check-obarray))))
    (if (memq stream vs)
        ;; Always enter debugger to see the backtrace
        (let ((debug-on-signal t)
              (debug-on-error  t))
          (setq semantic-bovinate-nonterminal-check-obarray nil)
          (error "Infinite recursive parse suspected on %s" nt))
      (set (intern nt semantic-bovinate-nonterminal-check-obarray)
           (cons stream vs)))))

(defun semantic-bovinate-nonterminal-default (stream &optional nonterminal)
  "Bovinate STREAM based on the TABLE of nonterminal symbols.
Optional argument NONTERMINAL is the nonterminal symbol to start with.
Use `bovine-toplevel' if it is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (if (not nonterminal)
      (setq nonterminal 'bovine-toplevel))

  ;; Try to detect infinite recursive parse when doing a full reparse.
  (or semantic-toplevel-bovine-cache
      (semantic-bovinate-nonterminal-check stream nonterminal))

  (let* ((table semantic-toplevel-bovine-table)
	 (matchlist (cdr (assq nonterminal table)))
	 (starting-stream stream)
	 (nt-loop  t)		  ;non-terminal loop condition
	 nt-popup                 ;non-nil if return from nt recursion
	 nt-stack		  ;non-terminal recursion stack
	 s			  ;Temp Stream Tracker
	 lse			  ;Local Semantic Element
	 lte			  ;Local matchlist element
	 tev			  ;Matchlist entry values from buffer
	 val			  ;Value found in buffer.
	 cvl			  ;collected values list.
	 out			  ;Output
	 end			  ;End of match
	 result
	 )
    (condition-case nil
        (while nt-loop
          (catch 'push-non-terminal
            (setq nt-popup nil
                  end (cdr (cdr (car stream))))
            (while (or nt-loop nt-popup)
              (setq nt-loop nil
                    out     nil)
              (while (or nt-popup matchlist)
                (if nt-popup
                    ;; End of a non-terminal recursion
                    (setq nt-popup nil)
                  ;; New matching process
                  (setq s   stream      ;init s from stream.
                        cvl nil     ;re-init the collected value list.
                        lte (car matchlist) ;Get the local matchlist entry.
                        )
                  (if (or (byte-code-function-p (car lte))
                          (listp (car lte)))
                      ;; In this case, we have an EMPTY match!  Make
                      ;; stuff up.
                      (setq cvl (list nil))))
            
                (while (and lte
                            (not (byte-code-function-p (car lte)))
                            (not (listp (car lte))))

                  ;; BNF SOURCE DEBUGGING!
                  (if semantic-edebug
                      (let* ((db-nt   (semantic-bovinate-nonterminal-db-nt))
                             (db-ml   (cdr (assq db-nt table)))
                             (db-mlen (length db-ml))
                             (db-midx (- db-mlen (length matchlist)))
                             (db-tlen (length (nth db-midx db-ml)))
                             (db-tidx (- db-tlen (length lte))))
                        (if (eq 'fail
                                (semantic-bovinate-show
                                 (car s) db-nt db-midx db-tidx cvl))
                            (setq lte '(trash 0 . 0)))))
                  ;; END BNF SOURCE DEBUGGING!
              
                  (cond
                   ;; We have a nonterminal symbol.  Recurse inline.
                   ((setq nt-loop (assq (car lte) table))
          
                    (setq
                     ;; push state into the nt-stack
                     nt-stack (cons (vector matchlist cvl lte stream end
                                            )
                                    nt-stack)
                     ;; new non-terminal matchlist
                     matchlist   (cdr nt-loop)
                     ;; new non-terminal stream
                     stream      s)
               
                    (throw 'push-non-terminal t)

                    )
                   ;; Default case
                   (t
                    (setq lse (car s)   ;Get the local stream element
                          s   (cdr s))  ;update stream.
                    ;; Do the compare
                    (if (eq (car lte) (car lse)) ;syntactic match
                        (let ((valdot (cdr lse)))
                          (setq val (semantic-flex-text lse))
                          ;; DEBUG SECTION
                          (if semantic-dump-parse
                              (semantic-dump-detail
                               (if (stringp (car (cdr lte)))
                                   (list (car (cdr lte)) (car lte))
                                 (list (car lte)))
                               (semantic-bovinate-nonterminal-db-nt)
                               val
                               (if (stringp (car (cdr lte)))
                                   (if (string-match (car (cdr lte)) val)
                                       "Term Match" "Term Fail")
                                 "Term Type=")))
                          ;; END DEBUG SECTION
                          (setq lte (cdr lte))
                          (if (stringp (car lte))
                              (progn
                                (setq tev (car lte)
                                      lte (cdr lte))
                                (if (string-match tev val)
                                    (setq cvl (cons
                                               (if (memq (car lse)
                                                         '(comment semantic-list))
                                                   valdot val)
                                               cvl)) ;append this value
                                  (setq lte nil cvl nil))) ;clear the entry (exit)
                            (setq cvl (cons
                                       (if (memq (car lse)
                                                 '(comment semantic-list))
                                           valdot val) cvl))) ;append unchecked value.
                          (setq end (cdr (cdr lse)))
                          )
                      (if (and semantic-dump-parse nil)
                          (semantic-dump-detail (car lte)
                                                (semantic-bovinate-nonterminal-db-nt)
                                                (semantic-flex-text lse)
                                                "Term Type Fail"))
                      (setq lte nil cvl nil)) ;No more matches, exit
                    )))
                (if (not cvl)           ;lte=nil;  there was no match.
                    (setq matchlist (cdr matchlist)) ;Move to next matchlist entry
                  (let ((start (car (cdr (car stream)))))
                    (setq out (cond
                               ((car lte)
                        
              ;; REMOVE THIS TO USE THE REFERENCE/COMPARE CODE
              ;;(let ((o (apply (car lte) ;call matchlist fn on values
              ;;                (nreverse cvl) start (list end))))
              ;;  (if semantic-bovinate-create-reference
              ;;      (semantic-bovinate-add-reference o))
              ;;  (if semantic-bovinate-compare-reference
              ;;      (semantic-bovinate-compare-against-reference o))
              ;;  o
              ;;  )
                            
                                (funcall (car lte) ;call matchlist fn on values
                                         (nreverse cvl) start end))
                               ((and (= (length cvl) 1)
                                     (listp (car cvl))
                                     (not (numberp (car (car cvl)))))
                                (append (car cvl) (list start end)))
                               (t
                                ;;(append (nreverse cvl) (list start end))))
                                ;; MAYBE THE FOLLOWING NEEDS LESS CONS
                                ;; CELLS THAN THE ABOVE?
                                (nreverse (cons end (cons start cvl)))))
                          matchlist nil) ;;generate exit condition
                    (if (not end)
                        (setq out nil)))
                  ;; Nothin?
                  ))
              (setq result
                    (if (eq s starting-stream)
                        (list (cdr s) nil)
                      (list s out)))
              (if nt-stack
                  ;; pop previous state from the nt-stack
                  (let ((state (car nt-stack)))

                    (setq nt-popup    t
                          ;; pop actual parser state
                          matchlist   (aref state 0)
                          cvl         (aref state 1)
                          lte         (aref state 2)
                          stream      (aref state 3)
                          end         (aref state 4)
                          ;; update the stack
                          nt-stack    (cdr nt-stack))
                
                    (if out
                        (let ((len (length out))
                              (strip (nreverse (cdr (cdr (reverse out))))))
                          (if semantic-dump-parse
                              (semantic-dump-detail (cdr result)
                                                    (car lte)
                                                    ""
                                                    "NonTerm Match"))
                          (setq end (nth (1- len) out) ;reset end to the end of exp
                                cvl (cons strip cvl) ;prepend value of exp
                                lte (cdr lte)) ;update the local table entry
                          )
                      ;; No value means that we need to terminate this
                      ;; match.
                      (setq lte nil cvl nil)) ;No match, exit
                    )))))
      (error
       ;; On error just move forward the stream of lexical tokens
       (setq result (list (cdr starting-stream) nil))))
      result))

;;; Debugging in bovine tables
;;
(defun semantic-dump-buffer-init ()
  "Initialize the semantic dump buffer."
  (save-excursion
    (let ((obn (buffer-name)))
      (set-buffer (get-buffer-create "*Semantic Dump*"))
      (erase-buffer)
      (insert "Parse dump of " obn "\n\n")
      (insert (format "%-15s %-15s %10s %s\n\n"
		      "Nonterm" "Comment" "Text" "Context"))
      )))

(defun semantic-dump-detail (lse nonterminal text comment)
  "Dump info about this match.
Argument LSE is the current syntactic element.
Argument NONTERMINAL is the nonterminal matched.
Argument TEXT is the text to match.
Argument COMMENT is additional description."
  (save-excursion
    (set-buffer "*Semantic Dump*")
    (goto-char (point-max))
    (insert (format "%-15S %-15s %10s %S\n" nonterminal comment text lse)))
  )

(defvar semantic-bovinate-debug-table nil
  "A marker where the current table we are debugging is.")

(defun semantic-bovinate-debug-set-table (&optional clear)
  "Set the table for the next debug to be here.
Optional argument CLEAR to unset the debug table."
  (interactive "P")
  (if clear (setq semantic-bovinate-debug-table nil)
    (if (not (eq major-mode 'emacs-lisp-mode))
	(error "Not an Emacs Lisp file"))
    (beginning-of-defun)
    (setq semantic-bovinate-debug-table (point-marker))))
  
;; We will get warnings in here about semantic-bnf-* fns.
;; We cannot require semantic-bnf due to compile errors.
(defun semantic-bovinate-debug-buffer ()
  "Bovinate the current buffer in debug mode."
  (interactive)
  (if (and (not semantic-toplevel-bovine-table-source)
	   (not semantic-bovinate-debug-table))
      (error
       "Call `semantic-bovinate-debug-set-table' from your semantic table"))
  (delete-other-windows)
  (split-window-vertically)
  (if semantic-bovinate-debug-table
      (switch-to-buffer (marker-buffer semantic-bovinate-debug-table))
    (if (not semantic-toplevel-bovine-table-source)
        (error "No debuggable BNF source found"))
    (require 'semantic-bnf)
    (switch-to-buffer (semantic-bnf-find-source-on-load-path
                       semantic-toplevel-bovine-table-source)))
  (other-window 1)
  (semantic-clear-toplevel-cache)
  (let ((semantic-edebug t))
    (semantic-bovinate-toplevel)))

(defun semantic-bovinate-show (lse nonterminal matchlen tokenlen collection)
  "Display some info about the current parse.
Returns 'fail if the user quits, nil otherwise.
LSE is the current listed syntax element.
NONTERMINAL is the current nonterminal being parsed.
MATCHLEN is the number of match lists tried.
TOKENLEN is the number of match tokens tried.
COLLECTION is the list of things collected so far."
  (let* ((semantic-edebug nil)
         (ol1 nil) (ol2 nil) (ret nil)
         (bnf-buffer (semantic-bnf-find-source-on-load-path
                      semantic-toplevel-bovine-table-source)))
    (unwind-protect
	(progn
	  (goto-char (car (cdr lse)))
	  (setq ol1 (semantic-make-overlay (car (cdr lse)) (cdr (cdr lse))))
	  (semantic-overlay-put ol1 'face 'highlight)
	  (goto-char (car (cdr lse)))
	  (if window-system nil (sit-for 1))
	  (other-window 1)
	  (let (s e)
	    (if semantic-bovinate-debug-table
		(progn
		  (set-buffer (marker-buffer semantic-bovinate-debug-table))
		  (goto-char semantic-bovinate-debug-table)
		  (re-search-forward
		   (concat "^\\s-*\\((\\|['`]((\\)\\(" (symbol-name nonterminal)
			   "\\)[ \t\n]+(")
		   nil t)
		  (setq s (match-beginning 2)
			e (match-end 2))
		  (forward-char -2)
		  (forward-list matchlen)
		  (skip-chars-forward " \t\n(")
		  (forward-sexp tokenlen)
		  )
	      ;; The user didn't specify a lisp level table.
	      ;; go to the source...
	      (set-buffer bnf-buffer)
	      (semantic-bnf-find-state-position
	       nonterminal matchlen tokenlen)
	      (save-excursion
		(goto-char (semantic-token-start (semantic-current-nonterminal)))
		(setq s (point)
		      e (progn (forward-sexp 1) (point))))
	      )
	    (setq ol2 (semantic-make-overlay s e))
	    (semantic-overlay-put ol2 'face 'highlight)
	    )
	  (message "%s: %S" lse collection)
	  (let ((e (semantic-read-event)))
	    (cond ((eq e ?f)		;force a failure on this symbol.
		   (setq ret 'fail))
		  ((eq e ?a)		;Abort this syntax element
		   (error "Abort"))
		  ((eq e ?q)		;Quit this debug session
		   (signal 'quit "Abort"))
		  (t nil)))
	  (other-window 1)
	  )
      (semantic-overlay-delete ol1)
      (semantic-overlay-delete ol2))
    ret))

;;; Reference Debugging
;;
(defvar semantic-bovinate-create-reference nil
  "Non nil to create a reference.")

(defvar semantic-bovinate-reference-token-list nil
  "A list generated as a reference (assumed valid).
A second pass compares return values against this list.")

(defun semantic-bovinate-add-reference (ref)
  "Add REF to the reference list."
  (setq semantic-bovinate-reference-token-list
	(cons ref semantic-bovinate-reference-token-list)))

(defvar semantic-bovinate-compare-reference nil
  "Non nil to compare against a reference list.")

(defvar semantic-bovinate-reference-temp-list nil
  "List used when doing a compare.")

(defun semantic-bovinate-compare-against-reference (ref)
  "Compare REF against what was returned last time."
  (if (not (equal ref (car semantic-bovinate-reference-temp-list)))
      (let ((debug-on-error t))
	(error "Stop: %d %S != %S"
	       (- (length semantic-bovinate-reference-token-list)
		  (length semantic-bovinate-reference-temp-list))
	       (car semantic-bovinate-reference-temp-list)
	       ref))
    (setq semantic-bovinate-reference-temp-list
	  (cdr semantic-bovinate-reference-temp-list))))
	   
(defun bovinate-create-reference ()
  "Create a reference list."
  (interactive)
  (condition-case nil
      (progn
	(semantic-clear-toplevel-cache)
	(setq semantic-bovinate-create-reference t
	      semantic-bovinate-reference-token-list nil)
	(bovinate)
	(setq semantic-bovinate-reference-token-list
	      (nreverse semantic-bovinate-reference-token-list)))
    (error nil))
  (setq semantic-bovinate-create-reference nil))

(defun bovinate-reference-compare ()
  "Compare the current parsed output to the reference list.
Create a reference with `bovinate-create-reference'."
  (interactive)
  (let ((semantic-bovinate-compare-reference t))
    (semantic-clear-toplevel-cache)
    (setq semantic-bovinate-reference-temp-list
	  semantic-bovinate-reference-token-list)
    (bovinate)))

(provide 'semantic-bovine)

;;; semantic-bovine.el ends here
