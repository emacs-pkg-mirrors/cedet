;;; wisent-dot.el --- GraphViz DOT parser

;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: wisent-dot.el,v 1.2 2003/03/26 04:06:54 zappo Exp $

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

;;; Code:
(require 'wisent-bovine)

(defconst wisent-dot-automaton
  ;;DO NOT EDIT! Generated from wisent-dot.wy - 2003-03-25 23:05-0500
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((DIGRAPH GRAPH SUBGRAPH NODE SHAPE LABEL COLOR STYLE LEN FONTNAME FONTSIZE WIDTH HEIGHT SPLINES OVERLAP DILINK LINK symbol string number EQUAL SEMI COMMA BRACKET_BLOCK BRACE_BLOCK LBRACE RBRACE LBRACKET RBRACKET)
       nil
       (dot_file
	((digraph))
	((graph)))
       (digraph
	((DIGRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'digraph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (graph
	((GRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'graph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (graph-contents
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((label))
	((style))
	((graph-attributes))
	((subgraph))
	((node))
	((named-node))
	((links)))
       (label
	((LABEL EQUAL string SEMI)
	 (wisent-raw-tag
	  (semantic-tag $3 'label))))
       (style
	((STYLE EQUAL symbol SEMI)
	 (wisent-raw-tag
	  (semantic-tag $3 'style))))
       (subgraph
	((SUBGRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'graph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (node
	((NODE BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag "NODE" 'generic-node :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (graph-attributes
	((GRAPH BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag "GRAPH" 'graph-attributes :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (named-node
	((symbol BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag $1 'node :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (node-description
	((LBRACKET)
	 nil)
	((RBRACKET)
	 nil)
	((COMMA)
	 nil)
	((SHAPE EQUAL symbol)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((LABEL EQUAL string)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((FONTNAME EQUAL string)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((FONTSIZE EQUAL number)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((symbol EQUAL symbol)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3))))
       (links
	((symbol DILINK symbol opt-link-attributes opt-semi)
	 (wisent-raw-tag
	  (semantic-tag $1 'link :to $3 :attributes $4)))
	((BRACE_BLOCK)))
       (opt-semi
	((SEMI)
	 nil)
	(nil))
       (opt-link-attributes
	((BRACKET_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'node-description 1))
	(nil)))
     '(dot_file graph-contents node-description)))
  )


(defconst wisent-dot-keywords
  ;;DO NOT EDIT! Generated from wisent-dot.wy - 2003-03-25 23:05-0500
  (semantic-lex-make-keyword-table
   '(("digraph" . DIGRAPH)
     ("graph" . GRAPH)
     ("subgraph" . SUBGRAPH)
     ("node" . NODE)
     ("shape" . SHAPE)
     ("label" . LABEL)
     ("color" . COLOR)
     ("style" . STYLE)
     ("len" . LEN)
     ("fontname" . FONTNAME)
     ("fontsize" . FONTSIZE)
     ("width" . WIDTH)
     ("height" . HEIGHT)
     ("splines" . SPLINES)
     ("overlap" . OVERLAP))
   '(("fontsize" summary "fontsize=<font-size-number>")
     ("fontname" summary "fontname=<font-spec>")
     ("len" summary "len=<value>")
     ("style" summary "style=<style-spec>")
     ("color" summary "color=<color-spec>")
     ("label" summary "label=\"string\"")
     ("shape" summary "shape=<shape-type>")
     ("node" summary "node [<attribute>...];")
     ("subgraph" summary "subgraph <name> { <graph elements> ... }")
     ("graph" summary "graph <name> { <graph elements> ... }")
     ("digraph" summary "digraph <name> { <graph elements> ... }")))
  "Keywords.")


(defconst wisent-dot-tokens
  ;;DO NOT EDIT! Generated from wisent-dot.wy - 2003-03-25 23:05-0500
  (wisent-lex-make-token-table
   '(("close-paren"
      (RBRACKET . "]")
      (RBRACE . "}"))
     ("open-paren"
      (LBRACKET . "[")
      (LBRACE . "{"))
     ("semantic-list"
      (BRACE_BLOCK . "^{")
      (BRACKET_BLOCK . "^\\["))
     ("number"
      (number))
     ("string"
      (string))
     ("symbol"
      (symbol))
     ("punctuation"
      (COMMA . ",")
      (SEMI . ";")
      (EQUAL . "=")
      (LINK . "--")
      (DILINK . "->")))
   'nil)
  "Tokens.")

(define-lex-block-analyzer semantic-lex-dot-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACKET_BLOCK ("[" LBRACKET) ("]" RBRACKET)))

(define-lex wisent-dot-lexer
  "Lexical analyzer that handles DOT buffers.
It ignores whitespace, newlines nad comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  semantic-lex-number
  semantic-lex-symbol-or-keyword
  semantic-lex-dot-blocks
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-punctuation-type
  semantic-lex-punctuation
  semantic-lex-default-action)

;;;###autoload
(defun wisent-dot-setup-parser ()
  "Setup buffer for parse."
  ;;DO NOT EDIT! Generated from wisent-dot.wy - 2003-03-25 23:05-0500
  (progn
    (semantic-install-function-overrides
     '((parse-stream . wisent-parse-stream)))
    (setq semantic-parser-name "LALR"
	  semantic-toplevel-bovine-table wisent-dot-automaton
	  semantic-debug-parser-source "wisent-dot.wy"
	  semantic-flex-keywords-obarray wisent-dot-keywords
	  semantic-lex-types-obarray wisent-dot-tokens)
    ;; Collect unmatched syntax lexical tokens
    (semantic-make-local-hook 'wisent-discarding-token-functions)
    (add-hook 'wisent-discarding-token-functions
	      'wisent-collect-unmatched-syntax nil t)
    (setq 
     ;; Lexical Analysis
     semantic-lex-analyzer 'wisent-dot-lexer
     ;; Parsing
     ;; Environment
     semantic-imenu-summary-function 'semantic-name-nonterminal
     imenu-create-index-function 'semantic-create-imenu-index
     semantic-command-separation-character ";"
     ;; Speedbar
     semantic-symbol->name-assoc-list
     '((graph . "Graph")
       (digraph . "Directed Graph")
       (node . "Node")
       )
     senator-step-at-token-ids '(graph digraph)
     ))
  (setq semantic-lex-analyzer #'wisent-dot-lexer))


;;;###autoload
(add-hook 'graphviz-dot-mode-hook #'wisent-dot-setup-parser)

(provide 'wisent-dot)

;;; wisent-dot.el ends here
