;;; working --- Display a "working" message in the minibuffer.

;;;  Copyright (C) 1998  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.0
;; Keywords: status

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Working lets Emacs Lisp programmers easily display working messages.
;; These messages typically come in the form of a percentile, or generic
;; doodles if a maximum is unknown.
;;
;; The working entry points are quite simple.  If you have a loop that needs
;; to display a status as it goes along, it would look like this:
;;
;;  (working-status-forms "Doing stuff" "done"
;;    (while condition
;;  	(working-status (calc-percentile))
;;  	(my-work))
;;    (working-status t))
;;
;; If you cannot calculate a percentile, use the function
;; `working-static-status' instead, and pass in what you know.  For
;; both status printing functions, the first argument is optional,
;; and you may pass in additional arguments as `format' elements
;; to the first argument of `working-status-forms'.
;;
;; See the examples at the end of the buffer.

;;; Backwards Compatibility:
;;
;; If you want to use working in your program, but don't want to force people
;; to install working, use could this at the beginning of your program for
;; compatibility.
;;
;; (eval-and-compile
;;   (condition-case nil
;; 	 (require 'working)
;;     (error
;; 	(progn
;; 	  (defmacro working-status-forms (message donestr &rest forms)
;; 	    "Contain a block of code during which a working status is shown."
;; 	    (list 'let (list (list 'msg message) (list 'dstr donestr)
;; 			     '(ref1 0))
;; 		  (cons 'progn forms)))
;;   
;; 	  (defun working-status (&optional percent &rest args)
;; 	    "Called within the macro `working-status-forms', show the status."
;; 	    (message "%s%s" (apply 'format msg args)
;; 		     (if (eq percent t) (concat "... " dstr)
;; 		       (format "... %3d%%"
;; 			       (or percent
;; 				   (floor (* 100.0 (/ (float (point))
;; 						      (point-max)))))))))
;;   
;; 	  (defun working-static-status (&optional number &rest args)
;; 	    "Called within the macro `working-status-forms', show the status."
;; 	    (message "%s%s" (apply 'format msg args)
;; 		     (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% ref1 4))))
;; 	    (setq ref1 (1+ ref1)))
;;   
;; 	  (put 'working-status-forms 'lisp-indent-function 2)))))
;;
;; Depending on what features you use, it is, of course, easy to
;; reduce the total size of the above by omitting those features you
;; do not use.

;;; History:
;; 
;; 1.0 First Version

(require 'custom)

;;; Code:
(defgroup working nil
  "Working messages display."
  :prefix "working"
  :group 'lisp
;  :version "20.3"
  )

;;; User configurable variables
;;
(defcustom working-status-percentage-type 'working-bar-percent-display
  "Function used to display the percent status.
Functions provided in `working' are:
  `working-percent-display'
  `working-bar-display'
  `working-bar-percent-display'
  `working-percent-bar-display'
  `working-celeron-percent-display'"
  :group 'working
  :type '(choice (const working-percent-display)
		 (const working-bar-display)
		 (const working-bar-percent-display)
		 (const working-percent-bar-display)
		 (const celeron-percent-display)))

(defcustom working-status-static-type 'working-celeron-display
  "Function used to display a celeron working display.
Static working types occur when the program does not know how long
it will take ahead of time.  Functions provided in `working' are:
  `working-number-display'
  `working-spinner-display'
  `working-dotgrowth-display'
  `working-celeron-display'"
  :group 'working
  :type '(choice (const working-number-display)
		 (const working-spinner-display)
		 (const working-dotgrowth-display)
		 (const working-celeron-display)))

;;; Variables used in stages
;;
(defvar working-message nil
  "Message stored when in a status loop.")
(defvar working-donestring nil
  "Done string stored when in a status loop.")
(defvar working-working-ref1 nil
  "A reference number used in a status loop.")

;;; Programmer functions
;;
(defmacro working-status-forms (message donestr &rest forms)
  "Contain a block of code during which a working status is shown.
MESSAGE is the message string to use and DONESTR is the completed text
to use when the functions `working-status' is called from FORMS."
  (list 'let (list (list 'working-message message)
		   (list 'working-donestring donestr)
		   '(working-ref1 0))
	(cons 'progn forms)))
(put 'working-status-forms 'lisp-indent-function 2)

(defun working-status (&optional percent &rest args)
  "Called within the macro `working-status-forms', show the status.
If PERCENT is nil, then calculate PERCENT from the value of `point' in
the current buffer.  If it is a number or float, use it as the raw
percentile.  If it is a string, then consider the job done, and
display this string where numbers would appear.
Additional ARGS are passed to fill on % elements of MESSAGE from the
macro `working-status-forms'."
  (let* ((p (or percent (floor (* 100.0 (/ (float (point)) (point-max))))))
	 (m1 (apply 'format working-message args))
	 (m2 (funcall working-status-percentage-type (length m1) p)))
    (message "%s%s" m1 m2)))

(defun working-static-status (&optional number &rest args)
  "Called within the macro `working-status-forms', show the status.
If NUMBER is nil, then increment NUMBER from 0 with each call.  If it
is a number or float, use it as the raw percentile.  If it is a
string, then consider the job done, and display this string where
numbers would appear.  Additional ARGS are passed to fill on %
elements of MESSAGE from the macro `working-status-forms'."
  (let* ((n (or number working-ref1))
	 (m1 (apply 'format working-message args))
	 (m2 (funcall working-status-static-type (length m1) n)))
    (message "%s%s" m1 m2)
    (setq working-ref1 (1+ working-ref1))))

;;; Percentage display types.
;;
(defun working-percent-display (length percent)
  "Return the percentage of the buffer that is done in a string.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (cond ((eq percent t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %3d%%" percent))))

(defun working-bar-display (length percent)
  "Return a string with a bar-graph showing percent.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let ((bs (- (frame-width) length 4)))
    (cond ((eq percent t)
	   (concat ": [" (make-string bs ?#) "] " working-donestring))
	  (t (let ((bsl (floor (* (/ percent 100.0) bs))))
	       (concat ": ["
		       (make-string bsl ?#)
		       (make-string (- bs bsl) ?.)
		       "]"))))))

(defun working-bar-percent-display (length percent)
  "Return a string with a bar-graph and percentile showing percentage.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let* ((ps (if (eq percent t)
		 (concat "... " working-donestring)
	       (working-percent-display length percent)))
	 (psl (+ 3 length (if (eq percent t) working-ref1 (length ps)))))
    (cond ((eq percent t)
	   (concat (working-bar-display psl 100) " " ps))
	  (t
	   (setq working-ref1 (length ps))
	   (concat (working-bar-display psl percent) " " ps)))))

(defun working-percent-bar-display (length percent)
  "Return a string with a percentile and bar-graph showing percentage.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (let* ((ps (if (eq percent t)
		 (concat "... " working-donestring)
	       (working-percent-display length percent)))
	 (psl (+ 1 length (if (eq percent t) working-ref1 (length ps)))))
    (cond ((eq percent t)
	   (concat ps " " (working-bar-display psl 100)))
	  (t
	   (setq working-ref1 (length ps))
	   (concat ps " " (working-bar-display psl percent))))))

(defun working-celeron-percent-display (length percent)
  "Return a string with a celeron and string showing percent.
LENGTH is the amount of display that has been used.  PERCENT
is t to display the done string, or the percentage to display."
  (prog1
      (cond ((eq percent t) (working-celeron-display length t))
	    ;; All the % signs because it then gets passed to message.
	    (t (format "%s %3d%%"
		       (working-celeron-display length 0)
		       percent)))
    (setq working-ref1 (1+ working-ref1))))

;;; Static display types.
;;
(defun working-number-display (length number)
  "Return a string display the number of things that happened.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %d" number))))

(defun working-spinner-display (length number)
  "Return a string displaying a spinner based on a number.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t) (concat "... " working-donestring))
	;; All the % signs because it then gets passed to message.
	(t (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% working-ref1 4))))))

(defun working-dotgrowth-display (length number)
  "Return a string displaying growing dots due to activity.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (concat " (" (make-string working-ref1 ?.) ")"
	  (if (eq number t) (concat " " working-donestring) "")))

(defvar working-celeron-strings
  [ "[O     ]" "[oO    ]" "[-oO   ]" "[ -oO  ]" "[  -oO ]" "[   -oO]"
    "[    -O]" "[     O]" "[    Oo]" "[   Oo-]"  "[  Oo- ]" "[ Oo-  ]"
    "[Oo-   ]" "[O-    ]"]
  "Strings representing a silly celeron.")

(defun working-celeron-display (length number)
  "Return a string displaying a celeron as things happen.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (cond ((eq number t)
	 (if (< (length working-donestring) 6)
	     (concat " ["
		     (make-string (ceiling (/ (- 6.0 (length working-donestring)) 2)) ? )
		     working-donestring
		     (make-string (floor (/ (- 6.0 (length working-donestring)) 2)) ? )
		     "]")
	   (concat " " (aref working-celeron-strings
			     (% working-ref1 (length working-celeron-strings)))
		   " " working-donestring)))
	;; All the % signs because it then gets passed to message.
	(t (concat " " (aref working-celeron-strings
			     (% working-ref1 (length working-celeron-strings)))))))

;;; Example function using `working'
;;
(defun working-verify-parenthisis-a ()
  "Verify all the parenthisis in an elisp program buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (working-status-forms "Scanning" "done"
	(while (not (eobp))
	  ;; Use default buffer position.
	  (working-status)
	  (forward-sexp 1)
	  (sleep-for 0.05)
	  )
	(working-status t))))
 
(defun working-verify-parenthisis-b ()
  "Verify all the parenthisis in an elisp program buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (working-status-forms "Scanning" "done"
	(while (not (eobp))
	  ;; Use default buffer position.
	  (working-static-status)
	  (forward-sexp 1)
	  (sleep-for 0.05)
	  )
	(working-static-status t))))

(provide 'working)

;;; working.el ends here
