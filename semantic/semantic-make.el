;;; semantic-make.el --- Makefile parsing rules.

;; Copyright (C) 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-make.el,v 1.6 2000/09/14 20:04:00 zappo Exp $

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
;; Use the Semantic Bovinator to parse Makefiles.
;; Concocted as an experiment for nonstandard languages.

(require 'semantic)
(require 'backquote)

;;; Code:
(defvar semantic-toplevel-make-bovine-table
  `((bovine-toplevel
     ( variable)
     ( rule)
     ( conditional)
     ) ; end Makefile
    (variable
     ( symbol equals elements
	      ,(semantic-lambda
		 (list (nth 0 vals) 'variable nil nil (nth 2 vals) nil nil)))
     ) ; end variable
    (rule
     ( symbol colons elements commands
	      ,(semantic-lambda
		 (list (nth 0 vals) 'function nil (nth 2 vals) nil nil)))
     ) ; end rule
    (conditional
     ( symbol "if" symbol newline
	      ,(semantic-lambda
		 (list nil)))
     ( symbol "else" newline
	      ,(semantic-lambda
		 (list nil)))
     ( symbol "endif" newline
	      ,(semantic-lambda
		 (list nil)))
     ) ; end conditional
    (equals
     ( punctuation ":" punctuation "="
		   ,(semantic-lambda
		     ))
     ( punctuation "+" punctuation "="
		   ,(semantic-lambda
		     ))
     ( punctuation "="
		   ,(semantic-lambda
		     ))
     ) ; end equals
    (colons
     ( punctuation ":" punctuation ":"
		   ,(semantic-lambda
		     ))
     ( punctuation ":"
		   ,(semantic-lambda
		     ))
     ) ; end colons
    (elements
     ( symbol elements
	      ,(semantic-lambda
		 (list (nth 0 vals)) (nth 1 vals)))
     ( symbol newline
	      ,(semantic-lambda
		 (list (nth 0 vals))))
     ( newline
       ,(semantic-lambda
	 ))
     ) ; end elements
    (commands
     ( shell-command newline commands
		     ,(semantic-lambda
		        (list (nth 0 vals)) (nth 1 vals)))
     (
      ,(semantic-lambda
	))
     ) ; end commands
    )
  "Table for parsing Makefiles.")

(defvar semantic-flex-make-extensions
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

(defun semantic-default-make-setup ()
  "Set up a Makefile buffer for parsing with semantic."
  (setq semantic-flex-extensions semantic-flex-make-extensions)
  ;; Code generated from make.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-make-bovine-table)
  (setq semantic-flex-enable-newlines t
	semantic-symbol->name-assoc-list '((variable . "Variables")
					   (function . "Rules")
					   (include . "Dependencies"))
	semantic-case-fold t
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
	imenu-create-index-function 'semantic-create-imenu-index
	)

  ;; End code generated from make.bnf
  )

(add-hook 'makefile-mode-hook 'semantic-default-make-setup)



(provide 'semantic-make)

;;; semantic-make.el ends here
