;;; semantic-skel.el --- Semantic details for skel

;;; Copyright (C) 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-skel.el,v 1.2 2001/11/17 15:41:27 zappo Exp $

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

;;; Commentary:
;;

;;; History:
;; 

(require 'semantic)
(require 'backquote)

;; Depending on what elements you include specialized support for
(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator))

;;; Code:
(defvar semantic-toplevel-skeleton-bovine-table
  nil
  "Skeleton language specification.")

(defvar semantic-flex-skeleton-extensions
  '(
    ;;Exmple of Cs' ifdef removal mechanism
    ;;("^#\\(if\\(def\\)?\\|else\\|endif\\)" . semantic-flex-c-if)
    )
  "Extensions to the flexer for C.")

;; You do not need to use this function unless you have compound
;; definitions.  For example, in C, the following is expanded:
;;  int x, y;
(defun semantic-expand-skeleton-nonterminal (nonterm)
  "Expand NONTERM into a list of equivalent nonterminals, or nil."
  nil)

(defvar semantic-skeleton-keyword-table
  nil
  "Some keywords used in skeleton.")


;;; Override methods & Variables
;;

;; Add methods to the override table here.  See
;; `semantic-install-function-overrides' for more details.


;;; Setup function
;;
;;;###autoload
(defun semantic-default-skel-setup ()
  "Set up a buffer for semantic parsing of the skeleton language."
;; Use this after defining override functions.
;;  (semantic-install-function-overrides
;;   '(
;;     ))
  )

;; Loading this file will install the parser.  Add this line
;; to a .emacs file, or other setup file along with an autoload
;; for the setup function to dynamically install the parser
;; when a file of that type is read into Emacs.
(add-hook 'skeleton-mode-hook 'semantic-default-skeleton-setup)

(provide 'semantic-skel)

;;; semantic-skel.el ends here
