;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.1
;; Keywords: goofy
;; X-RCS: $Id: semantic-make.el,v 1.1 2000/06/11 02:19:01 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-ex is free software; you can redistribute it and/or modify
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

(require 'semantic)
(require 'backquote)

(defvar semantic-toplevel-make-bovine-table
  `((bovine-toplevel
     ( variable)
     ( rule)
     ( conditional)
     ) ; end bovine-toplevel
    (variable
     ( symbol punctuation "=" elements
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals) 'variable nil nil (nth 2 vals) nil nil)
			  (list start end))))
     ) ; end variable
    (rule
     ( symbol punctuation ":" elements commands
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals) 'function nil (nth 2 vals) nil)
			  (list start end))))
     ) ; end rule
    (conditional
     ( symbol "if" symbol newline
	      ,(lambda (vals start end)
		 (append  (list nil)
			  (list start end))))
     ( symbol "else" newline
	      ,(lambda (vals start end)
		 (append  (list nil)
			  (list start end))))
     ( symbol "endif" newline
	      ,(lambda (vals start end)
		 (append  (list nil)
			  (list start end))))
     ) ; end conditional
    (elements
     ( symbol elements
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals)) (nth 1 vals)
			  (list start end))))
     ( symbol newline
	      ,(lambda (vals start end)
		 (append  (list (nth 0 vals))
			  (list start end))))
     ( newline
       ,(lambda (vals start end)
	  (append 
	   (list start end))))
     ) ; end elements
    (commands
     ( shell-command newline commands
		     ,(lambda (vals start end)
			(append  (list (nth 0 vals)) (nth 1 vals)
				 (list start end))))
     (
      ,(lambda (vals start end)
	 (append 
	  (list start end))))
     ) ; end commands
    )
"Table for parsing Makefiles.")

(defvar semantic-flex-make-extentions
  '(("^\\(\t\\)" . semantic-flex-make-command)
    ("\\(\\\\\n\\)" . semantic-flex-nonewline))
  "Extensions to the flexer for make.")

(defun semantic-flex-make-command ()
  "Move the cursor and return nil when a tab starting line is found.
These command lines continue to additional lines when the end with \\"
  (let ((start (match-end 0)))
    (while (progn (end-of-line)
		  (save-excursion (forward-char -1) (looking-at "\\\\")))
      (forward-char 1))
    (cons 'shell-command (cons start (point)))))

(defun semantic-flex-nonewline ()
  "If there is a \ ending a line, then it isn't really a newline."
  (goto-char (match-end 0))
  nil)

(add-hook
 'makefile-mode-hook
 (lambda ()
   (setq semantic-toplevel-bovine-table semantic-toplevel-make-bovine-table
	 semantic-flex-extensions semantic-flex-make-extentions
	 semantic-flex-syntax-modifications '((?. "_")
					      (?= ".")
					      (?/ "_")
					      (?\t ".")
					      (?( "_")
					      (?) "_")
					      (?{ "_")
					      (?} "_")
					      (?$ "_")
					      )
	 semantic-flex-enable-newlines t
	 )))