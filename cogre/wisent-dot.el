;;; wisent-dot.el --- GraphViz DOT parser

;; Copyright (C) 2003, 2004 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: wisent-dot.el,v 1.5 2004/01/15 01:30:58 zappo Exp $

;; This file is not part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Parser for GraphViz DOT language.
;; The language is declaritive and the whole thing is parsed.
;; The result could be used as a data structure representing a graph.

;; This depends on graphics dot mode by
;;   Pieter E.J. Pareit <pieter.pareit@planetinternet.be>
;;   http://users.skynet.be/ppareit/graphviz-dot-mode.el
;;   with the following patch:
;;
;;
;; *** graphviz-dot-mode.el	2003/03/23 17:14:22	1.1
;; --- graphviz-dot-mode.el	2003/03/26 03:39:21
;; ***************
;; *** 98,103 ****
;; --- 98,109 ----
;;       (modify-syntax-entry ?/ ". 124b" st)
;;       (modify-syntax-entry ?* ". 23" st)
;;       (modify-syntax-entry ?\n "> b" st)
;; +     (modify-syntax-entry ?= "." st)
;; +     (modify-syntax-entry ?, "." st)
;; +     (modify-syntax-entry ?\; "." st)
;; +     (modify-syntax-entry ?- "." st)
;; +     (modify-syntax-entry ?> "." st)
;; +     (modify-syntax-entry ?< "." st)
;;       st)
;;     "Syntax table for `graphviz-dot-mode'.")
;;   


;;; Code:
(require 'wisent-bovine)
(require 'semantic)
(require 'wisent-dot-wy)

(define-lex wisent-dot-lexer
  "Lexical analyzer that handles DOT buffers.
It ignores whitespace, newlines nad comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  semantic-lex-number
  semantic-lex-symbol-or-keyword
  wisent-dot-wy--block-block-analyzer
  ;; ?? semantic-lex-close-paren
  semantic-lex-string
  wisent-dot-wy--punctuation-string-analyzer
  semantic-lex-default-action)

;;;###autoload
(defun wisent-dot-setup-parser ()
  "Setup buffer for parse."
  (wisent-dot-wy--install-parser)

  (setq 
   ;; Lexical Analysis
   semantic-lex-analyzer 'wisent-dot-lexer
   ;; Parsing
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character ";"
   ;; Speedbar
   semantic-symbol->name-assoc-list
   '((graph . "Graph")
     (digraph . "Directed Graph")
     (node . "Node")
     )
   ;; Navigation
   senator-step-at-tag-classes '(graph digraph)
   ))

;;;###autoload
(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

(provide 'wisent-dot)

;;; wisent-dot.el ends here
