;;; semantic-erlang.el --- Semantic Erlang gramma

;; Copyright (C) 2001, 2002 Vladimir G. Sekissov

;; Author:  <svg@surnet.ru>
;; Keywords: languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'semantic)
(require 'backquote)
(require 'erlang-edoc)

;; Depending on what elements you include specialized support for
(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator))

(defvar semantic-toplevel-erlang-bovine-table
`((add-op
 ( punctuation "\\b\\+\\b")
 ( punctuation "\\b-\\b")
 ( BOR)
 ( BXOR)
 ( BSL)
 ( BSR)
 ) ; end add-op
 (list-conc-op
 ( punctuation "\\b\\+\\b" punctuation "\\b\\+\\b")
 ( punctuation "\\b-\\b" punctuation "\\b-\\b")
 ) ; end list-conc-op
 (comp-op
 ( punctuation "\\b=\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "==")))
 ( punctuation "\\b=\\b" punctuation "\\b:\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "=:=")))
 ( punctuation "\\b=\\b" punctuation "\\b/\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "=/=")))
 ( punctuation "\\b/\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list "/=")))
 ( punctuation "\\b=\\b" punctuation "\\b<\\b"
  ,(semantic-lambda
  (list "=<")))
 ( punctuation "\\b>\\b" punctuation "\\b=\\b"
  ,(semantic-lambda
  (list ">=")))
 ( punctuation "\\b<\\b")
 ( punctuation "\\b>\\b")
 ( OR)
 ( AND)
 ) ; end comp-op
 (multi-op
 ( punctuation "\\b\\*\\b")
 ( punctuation "\\b/\\b")
 ( DIV)
 ( REM)
 ( BAND)
 ) ; end multi-op
 (prefix-op
 ( punctuation "\\b\\+\\b")
 ( punctuation "\\b-\\b")
 ( BNOT)
 ) ; end prefix-op
 (basic-type
 ( float-literal)
 ( integer-literal)
 ( char-literal)
 ( atom)
 ( var)
 ( string)
 ( TRUE)
 ) ; end basic-type
 (atom
 ( symbol "[a-z][a-zA-Z0-9_@]*"
  ,(semantic-lambda
  (list (nth 0 vals))))
 ( symbol "'.+'"
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end atom
 (float-literal
 ( symbol "[0-9]+" punctuation "\\b\\.\\b" symbol "[0-9]+" exp-part)
 ( punctuation "\\b\\.\\b" symbol "[0-9]+" exp-part)
 ) ; end float-literal
 (exp-part
 ( symbol "[eE]" punctuation "[-+]" symbol "[0-9]+")
 ( symbol "[eE]" symbol "[0-9]+")
 ()
 ) ; end exp-part
 (integer-literal
 ( symbol "[0-9a-eA-E]+")
 ( symbol "[0-9]\\{1,2\\}" punctuation "\\b#\\b" "[0-9a-eA-E]+")
 ) ; end integer-literal
 (char-literal
 ( CHAR)
 ) ; end char-literal
 (var
 ( symbol "^_[a-zA-Z0-9]+"
  ,(semantic-lambda
  (list (nth 0 vals) 'variable nil nil nil nil)))
 ( symbol "^[A-Z][_a-zA-Z0-9]*"
  ,(semantic-lambda
  (list (nth 0 vals) 'variable nil nil nil nil)))
 ) ; end var
 (uni-pattern
 ( symbol "_")
 ) ; end uni-pattern
 (binary
 ( punctuation "\\b<\\b" punctuation "\\b<\\b" punctuation "\\b>\\b" punctuation "\\b>\\b"
  ,(semantic-lambda
  (list "<<>>" 'binary nil nil)))
 ( punctuation "\\b<\\b" punctuation "\\b<\\b" binary-segments punctuation "\\b>\\b" punctuation "\\b>\\b"
  ,(semantic-lambda
  (list "<<Binary>>" 'binary nil nil)))
 ) ; end binary
 (binary-segments
 ( binary-segment binary-segments-rest)
 ) ; end binary-segments
 (binary-segments-rest
 ( punctuation "\\b,\\b" binary-segments
  ,(semantic-lambda
  (nth 1 vals)))
 ()
 ) ; end binary-segments-rest
 (binary-segment
 ( basic-type binary-segment-rest)
 ( expr binary-segment-rest)
 ) ; end binary-segment
 (binary-segment-rest
 ( punctuation "\\b:\\b" integer-literal punctuation "\\b/\\b" basic-type)
 ( punctuation "\\b:\\b" basic-type)
 ( punctuation "\\b/\\b" basic-type)
 ()
 ) ; end binary-segment-rest
 (bovine-toplevel
 ( module-attr)
 ( function-decl)
 ( header-form)
 ( directive)
 ( file-attr)
 ) ; end module-decl
 (module-attr
 ( punctuation "\\b-\\b" MODULE semantic-list "^(" full-stop
  ,(semantic-lambda
  (list ( car
 (semantic-bovinate-from-nonterminal (car (nth 2 vals)) (cdr (nth 2 vals)) 'module-attr-name)
 ) 'package nil nil)))
 ) ; end module-attr
 (module-attr-name
 ( open-paren "(" module-name close-paren ")"
  ,(semantic-lambda
  (list ( car (nth 1 vals)))))
 ) ; end module-attr-name
 (module-name
 ( atom module-name-rest
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ( module-name-rest
  ,(semantic-lambda
  (list ( car (nth 0 vals)))))
 ) ; end module-name
 (module-name-rest
 ( punctuation "\\b\\.\\b" atom module-name-rest
  ,(semantic-lambda
  (list ( concat (nth 0 vals) ( car (nth 1 vals)) ( car (nth 2 vals))))))
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end module-name-rest
 (header-form
 ( header-attr)
 ( anywhere-attr)
 ) ; end header-form
 (header-attr
 ( export-attr)
 ( import-attr)
 ( compile-attr)
 ( whild-attr)
 ) ; end header-attr
 (anywhere-attr
 ( file-attr)
 ( macro-def)
 ( record-decl)
 ) ; end anywhere-attr
 (export-attr
 ( punctuation "\\b-\\b" EXPORT semantic-list "^(" full-stop)
 ) ; end export-attr
 (export-name-list
 ( open-paren semantic-list "^\\[" close-paren)
 ) ; end export-name-list
 (import-attr
 ( punctuation "\\b-\\b" IMPORT semantic-list "^(" full-stop
  ,(semantic-lambda
  (list ( car
 (semantic-bovinate-from-nonterminal (car (nth 2 vals)) (cdr (nth 2 vals)) 'import-name-list)
 ) 'import nil nil)))
 ) ; end import-attr
 (import-name-list
 ( open-paren module-name punctuation "\\b,\\b" semantic-list "^\\[" close-paren
  ,(semantic-lambda
  (nth 1 vals) (list
 (semantic-bovinate-from-nonterminal (car (nth 3 vals)) (cdr (nth 3 vals)) 'function-name-list)
 )))
 ( open-paren module-name close-paren
  ,(semantic-lambda
  (nth 1 vals)))
 ) ; end import-name-list
 (function-name-list
 ( open-paren close-paren
  ,(semantic-lambda
  (list nil)))
 ( open-paren function-names close-paren
  ,(semantic-lambda
  (list (nth 1 vals))))
 ) ; end function-name-list
 (function-names
 ( function-arity punctuation "\\b,\\b" function-names
  ,(semantic-lambda
  (list (nth 0 vals) (nth 2 vals))))
 ( function-arity
  ,(semantic-lambda
  (list (nth 0 vals))))
 ) ; end function-names
 (function-arity
 ( atom punctuation "\\b/\\b" integer-literal
  ,(semantic-lambda
  (nth 0 vals) (list (nth 1 vals)) (nth 2 vals)))
 ) ; end function-arity
 (compile-attr
 ( punctuation "\\b-\\b" COMPILE semantic-list "^(" full-stop)
 ) ; end compile-attr
 (file-attr
 ( punctuation "\\b-\\b" symbol "file" semantic-list "^(" full-stop)
 ) ; end file-attr
 (file-attr-list
 ( open-paren string punctuation "\\b,\\b" integer-literal close-paren
  ,(semantic-lambda
  (list ( cons ( read (nth 1 vals)) (nth 3 vals)))))
 ) ; end file-attr-list
 (whild-attr
 ( punctuation "\\b-\\b" atom semantic-list "^(" full-stop)
 ) ; end whild-attr
 (function-decl
 ( function-clauses full-stop
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end function-decl
 (function-clauses
 ( function-clause function-clauses-rest
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end function-clauses
 (function-clauses-rest
 ( punctuation "\\b;\\b" function-clauses)
 ()
 ) ; end function-clauses-rest
 (function-clause
 ( atom fun-clause
  ,(semantic-lambda
  (list ( concat ( car (nth 0 vals)) "/" ( number-to-string ( length ( car (nth 1 vals))))) 'function nil) (nth 1 vals) (list nil nil)))
 ) ; end function-clause
 (record-decl
 ( punctuation "\\b-\\b" RECORD semantic-list "(" full-stop
  ,(semantic-lambda
  ( car
 (semantic-bovinate-from-nonterminal-full (car (nth 2 vals)) (cdr (nth 2 vals)) 'record-def)
 )))
 ) ; end record-decl
 (record-def
 ( open-paren atom punctuation "\\b,\\b" semantic-list "^{" close-paren
  ,(semantic-lambda
  (nth 1 vals) (list 'type "record"
 (semantic-bovinate-from-nonterminal-full (car (nth 3 vals)) (cdr (nth 3 vals)) 'record-field-decl)
  nil nil)))
 ) ; end record-def
 (record-decl-tuple
 ( open-paren record-field-decls close-paren
  ,(semantic-lambda
  (list (nth 1 vals))))
 ( open-paren close-paren
  ,(semantic-lambda
  (list nil)))
 ) ; end record-decl-tuple
 (record-field-decl
 ( atom record-field-value
  ,(semantic-lambda
  (nth 0 vals) (list 'variable nil "" nil nil)))
 ( atom
  ,(semantic-lambda
  (nth 0 vals) (list 'variable nil "" nil nil)))
 ( open-paren "{"
  ,(semantic-lambda
  (list nil)))
 ( close-paren "}"
  ,(semantic-lambda
  (list nil)))
 ( punctuation "\\b,\\b"
  ,(semantic-lambda
  (list nil)))
 ) ; end record-field-decl
 (pattern
 ( pattern-expr
  ,(semantic-lambda
  (nth 0 vals)))
 ( uni-pattern)
 ( binary)
 ( basic-type)
 ( semantic-list "^\\["
  ,(semantic-lambda
  (list "List" 'list nil nil)))
 ( semantic-list "^{"
  ,(semantic-lambda
  (list "Tuple" 'tuple nil nil)))
 ( record-pattern
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end pattern
 (pattern-expr
 ( pattern-conc-expr pattern-conc-expr-rest
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end pattern-expr
 (pattern-conc-expr
 ( string-literal)
 ( var)
 ) ; end pattern-conc-expr
 (pattern-conc-expr-rest
 ( list-conc-op pattern-expr
  ,(semantic-lambda
 ))
 ()
 ) ; end pattern-conc-expr-rest
 (tuple-pattern
 ( open-paren patterns close-paren)
 ( open-paren close-paren
  ,(semantic-lambda
  (list nil)))
 ) ; end tuple-pattern
 (list-pattern
 ( open-paren patterns list-pattern-tail close-paren)
 ( open-paren close-paren
  ,(semantic-lambda
  (list nil)))
 ) ; end list-pattern
 (list-pattern-tail
 ( punctuation "\\b|\\b" pattern)
 (
  ,(semantic-lambda
  (list nil)))
 ) ; end list-pattern-tail
 (patterns
 ( pattern patterns-rest
  ,(semantic-lambda
  (list ( cons ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ) ; end patterns
 (patterns-rest
 ( punctuation "\\b,\\b" patterns
  ,(semantic-lambda
  (nth 1 vals)))
 ()
 ) ; end patterns-rest
 (record-pattern
 ( punctuation "\\b#\\b" atom semantic-list "^{"
  ,(semantic-lambda
  (list ( car (nth 1 vals)))))
 ) ; end record-pattern
 (record-pattern-tuple
 ( open-paren record-field-patterns close-paren
  ,(semantic-lambda
  (list (nth 1 vals))))
 ( open-paren close-paren
  ,(semantic-lambda
  (list nil)))
 ) ; end record-pattern-tuple
 (record-field-patterns
 ( record-field-patterns punctuation "\\b,\\b" record-field-pattern)
 ( record-field-pattern)
 ) ; end record-field-patterns
 (record-field-pattern
 ( atom punctuation "\\b=\\b" pattern)
 ) ; end record-field-pattern
 (body
 ( exprs)
 ) ; end body
 (exprs
 ( expr exprs-rest)
 ) ; end exprs
 (exprs-rest
 ( punctuation "\\b,\\b" exprs)
 ()
 ) ; end exprs-rest
 (expr
 ( CATCH expr)
 ( match-expr)
 ) ; end expr
 (match-expr
 ( pattern punctuation "\\b=\\b" match-expr)
 ( send-expr)
 ) ; end match-expr
 (send-expr
 ( compare-expr send-expr-rest)
 ) ; end send-expr
 (send-expr-rest
 ( punctuation "\\b!\\b" send-expr)
 ()
 ) ; end send-expr-rest
 (compare-expr
 ( list-conc-expr compare-expr-rest)
 ) ; end compare-expr
 (compare-expr-rest
 ( comp-op list-conc-expr)
 ()
 ) ; end compare-expr-rest
 (list-conc-expr
 ( add-expr list-conc-expr-rest)
 ) ; end list-conc-expr
 (list-conc-expr-rest
 ( list-conc-op list-conc-expr)
 ()
 ) ; end list-conc-expr-rest
 (add-expr
 ( multi-expr add-expr-rest)
 ) ; end add-expr
 (add-expr-rest
 ( add-op add-expr)
 ()
 ) ; end add-expr-rest
 (multi-expr
 ( prefix-expr multi-expr-rest)
 ) ; end multi-expr
 (multi-expr-rest
 ( multi-op multi-expr)
 ()
 ) ; end multi-expr-rest
 (prefix-expr
 ( prefix-op record-expr)
 ( record-expr)
 ) ; end prefix-expr
 (record-expr
 ( punctuation "\\b#\\b" record-expr-field)
 ( application-expr record-expr-rest)
 ) ; end record-expr
 (record-expr-rest
 ( punctuation "\\b#\\b" record-expr-field)
 ()
 ) ; end record-expr-rest
 (record-expr-field
 ( atom punctuation "\\b\\.\\b" atom)
 ( atom semantic-list "^{")
 ( record-expr)
 ) ; end record-expr-field
 (record-update-tuple
 ( open-paren close-paren)
 ( open-paren record-field-updates close-paren)
 ) ; end record-update-tuple
 (record-field-updates
 ( record-field-update record-field-updates-rest)
 ) ; end record-field-updates
 (record-field-updates-rest
 ( punctuation "\\b,\\b" record-field-updates)
 ()
 ) ; end record-field-updates-rest
 (record-field-update
 ( atom record-field-value)
 ) ; end record-field-update
 (record-field-value
 ( punctuation "\\b=\\b" expr)
 ()
 ) ; end record-field-value
 (application-expr
 ( module-name punctuation "\\b:\\b" primary-expr semantic-list "^(")
 ( primary-expr application-expr-rest)
 ) ; end application-expr
 (application-expr-rest
 ( semantic-list "^(")
 ( punctuation "\\b:\\b" primary-expr semantic-list "^(")
 ()
 ) ; end application-expr-rest
 (application-expr-list
 ( open-paren close-paren)
 ( open-paren exprs close-paren)
 ) ; end application-expr-list
 (primary-expr
 ( binary)
 ( string-literal)
 ( basic-type)
 ( semantic-list "^{")
 ( semantic-list "^\\[")
 ( semantic-list "^\\[")
 ( block-expr)
 ( if-expr)
 ( case-expr)
 ( receive-expr)
 ( fun-expr)
 ( query-expr)
 ( paren-expr)
 ) ; end primary-expr
 (string-literal
 ( string)
 ( macro-app)
 ( string string-literal)
 ) ; end string-literal
 (tuple-skel
 ( open-paren close-paren)
 ( open-paren exprs close-paren)
 ) ; end tuple-skel
 (list-skel
 ( open-paren close-paren)
 ( open-paren exprs list-skel-tail close-paren)
 ) ; end list-skel
 (list-skel-tail
 ( punctuation "\\b|\\b" expr)
 ()
 ) ; end list-skel-tail
 (list-compr
 ( open-paren expr punctuation "\\b|\\b" punctuation "\\b|\\b" list-compr-exprs close-paren)
 ) ; end list-compr
 (list-compr-exprs
 ( list-compr-expr list-compr-exprs-rest)
 ) ; end list-compr-exprs
 (list-compr-exprs-rest
 ( punctuation "\\b,\\b" list-compr-exprs)
 ()
 ) ; end list-compr-exprs-rest
 (list-compr-expr
 ( generator)
 ( filter)
 ) ; end list-compr-expr
 (generator
 ( pattern punctuation "\\b<\\b" punctuation "\\b-\\b" expr)
 ) ; end generator
 (filter
 ( expr)
 ) ; end filter
 (block-expr
 ( BEGIN body END)
 ) ; end block-expr
 (if-expr
 ( IF if-clauses END)
 ) ; end if-expr
 (if-clauses
 ( if-clause punctuation "\\b;\\b" if-clauses)
 ( if-clause)
 ) ; end if-clauses
 (if-clause
 ( guard clause-body)
 ( expr clause-body)
 ) ; end if-clause
 (clause-body
 ( punctuation "\\b-\\b" punctuation "\\b>\\b" body)
 ) ; end clause-body
 (case-expr
 ( CASE expr OF cr-clauses END)
 ) ; end case-expr
 (cr-clauses
 ( cr-clause punctuation "\\b;\\b" cr-clauses)
 ( cr-clause)
 ) ; end cr-clauses
 (cr-clause
 ( clause-pattern clause-guard clause-body)
 ) ; end cr-clause
 (clause-guard
 ( WHEN guard)
 ()
 ) ; end clause-guard
 (receive-expr
 ( RECEIVE cr-clauses receive-after END)
 ( RECEIVE receive-after END)
 ( RECEIVE cr-clauses AFTER expr clause-body END)
 ) ; end receive-expr
 (receive-after
 ( AFTER expr clause-body)
 ()
 ) ; end receive-after
 (fun-expr
 ( FUN function-arity)
 ( FUN fun-clauses END)
 ) ; end fun-expr
 (fun-clauses
 ( fun-clause fun-clauses-rest)
 ) ; end fun-clauses
 (fun-clauses-rest
 ( punctuation "\\b;\\b" fun-clauses)
 ()
 ) ; end fun-clauses-rest
 (fun-clause
 ( semantic-list "^(" clause-guard clause-body
  ,(semantic-lambda
  ( car
 (semantic-bovinate-from-nonterminal (car (nth 0 vals)) (cdr (nth 0 vals)) 'clause-pattern-list)
 )))
 ) ; end fun-clause
 (clause-pattern-list
 ( open-paren clause-patterns close-paren
  ,(semantic-lambda
  (list (nth 1 vals))))
 ) ; end clause-pattern-list
 (clause-patterns
 ( clause-pattern clause-patterns-rest
  ,(semantic-lambda
  (list ( cons ( car (nth 0 vals)) ( car (nth 1 vals))))))
 ) ; end clause-patterns
 (clause-patterns-rest
 ( punctuation "\\b,\\b" clause-patterns
  ,(semantic-lambda
  (nth 1 vals)))
 ()
 ) ; end clause-patterns-rest
 (clause-pattern
 ( match-pattern)
 ( pattern)
 ) ; end clause-pattern
 (match-pattern
 ( var punctuation "\\b=\\b" pattern
  ,(semantic-lambda
  (nth 0 vals)))
 ( pattern punctuation "\\b=\\b" var
  ,(semantic-lambda
  (nth 0 vals)))
 ) ; end match-pattern
 (query-expr
 ( QUERY semantic-list "^\\[" END)
 ) ; end query-expr
 (paren-expr
 ( semantic-list "^(")
 ) ; end paren-expr
 (paren-expr-list
 ( open-paren expr close-paren)
 ) ; end paren-expr-list
 (guard
 ( guard-test guard-rest)
 ) ; end guard
 (guard-rest
 ( punctuation "\\b,\\b" guard)
 ( punctuation "\\b;\\b" guard)
 ()
 ) ; end guard-rest
 (guard-test
 ( TRUE)
 ( guard-record-test)
 ( guard-term-cmp)
 ( guard-recognizer)
 ( semantic-list "^(")
 ) ; end guard-test
 (guard-record-test
 ( RECORD semantic-list "^(")
 ( open-paren guard-expr punctuation "\\b,\\b" symbol close-paren)
 ) ; end guard-record-test
 (guard-recognizer
 ( symbol semantic-list "^(")
 ) ; end guard-recognizer
 (guard-term-cmp
 ( guard-expr guard-term-op guard-expr)
 ) ; end guard-term-cmp
 (guard-term-op
 ( comp-op)
 ( punctuation "\\b=\\b")
 ) ; end guard-term-op
 (paren-guard-test
 ( open-paren guard-test close-paren)
 ) ; end paren-guard-test
 (guard-expr
 ( guard-add-expr)
 ) ; end guard-expr
 (guard-add-expr
 ( guard-multi-expr guard-add-expr-rest)
 ) ; end guard-add-expr
 (guard-add-expr-rest
 ( add-op guard-add-expr)
 ()
 ) ; end guard-add-expr-rest
 (guard-multi-expr
 ( guard-prefix-expr guard-multi-expr-rest)
 ) ; end guard-multi-expr
 (guard-multi-expr-rest
 ( multi-op guard-multi-expr)
 ()
 ) ; end guard-multi-expr-rest
 (guard-prefix-expr
 ( prefix-op guard-application-expr)
 ( guard-application-expr)
 ) ; end guard-prefix-expr
 (guard-application-expr
 ( atom semantic-list "^(")
 ( guard-record-expr)
 ( guard-primary-expr)
 ) ; end guard-application-expr
 (guard-exprs-list
 ( open-paren close-paren)
 ( open-paren guard-exprs close-paren)
 ) ; end guard-exprs-list
 (guard-exprs
 ( guard-expr guard-exprs-rest)
 ) ; end guard-exprs
 (guard-exprs-rest
 ( punctuation "\\b,\\b" guard-exprs)
 ( punctuation "\\b;\\b" guard-exprs)
 ()
 ) ; end guard-exprs-rest
 (guard-record-expr
 ( punctuation "\\b#\\b" atom punctuation "\\b\\.\\b" atom)
 ( guard-primary-expr punctuation "\\b#\\b" atom punctuation "\\b\\.\\b" atom)
 ) ; end guard-record-expr
 (guard-primary-expr
 ( basic-type)
 ( macro-app)
 ( semantic-list "^{")
 ( semantic-list "^\\[")
 ( semantic-list "^(")
 ) ; end guard-primary-expr
 (guard-tuple-skel
 ( open-paren close-paren)
 ( open-paren guard-exprs close-paren)
 ) ; end guard-tuple-skel
 (guard-list-skel
 ( open-paren close-paren)
 ( open-paren guard-exprs guard-list-skel-tail close-paren)
 ) ; end guard-list-skel
 (guard-list-skel-tail
 ( punctuation "\\b|\\b" guard-expr)
 ()
 ) ; end guard-list-skel-tail
 (guard-paren-expr
 ( open-paren guard-expr close-paren)
 ) ; end guard-paren-expr
 (directive
 ( macro-def)
 ( macro-undef)
 ( include-dir)
 ( include-lib-dir)
 ( ifdef-dir)
 ( ifndef-dir)
 ( else-dir)
 ( endif-dir)
 ) ; end directive
 (macro-def
 ( punctuation "\\b-\\b" DEFINE semantic-list "^(" full-stop
  ,(semantic-lambda
 
 (semantic-bovinate-from-nonterminal (car (nth 2 vals)) (cdr (nth 2 vals)) 'macro-def-list)
 ))
 ) ; end macro-def
 (macro-def-list
 ( open-paren symbol macro-def-opt punctuation "\\b,\\b" macro-def-opt close-paren
  ,(semantic-lambda
  (list (nth 1 vals) 'variable nil (nth 4 vals) ( semantic-bovinate-make-assoc-list 'const t) nil)))
 ) ; end macro-def-list
 (macro-def-opt
 ( semantic-list)
 ( expr)
 ()
 ) ; end macro-def-opt
 (macro-undef
 ( punctuation "\\b-\\b" UNDEF semantic-list "^(" full-stop)
 ) ; end macro-undef
 (macro-app
 ( punctuation "\\b\\?\\b" symbol semantic-list "^(")
 ( punctuation "\\b\\?\\b" symbol)
 ) ; end macro-app
 (include-dir
 ( punctuation "\\b-\\b" INCLUDE semantic-list "^(" full-stop
  ,(semantic-lambda
  (list ( car
 (semantic-bovinate-from-nonterminal (car (nth 2 vals)) (cdr (nth 2 vals)) 'include-file-name)
 ) 'include nil nil)))
 ) ; end include-dir
 (include-lib-dir
 ( punctuation "\\b-\\b" INCLUDE_LIB semantic-list "^(" full-stop
  ,(semantic-lambda
  (list ( car
 (semantic-bovinate-from-nonterminal (car (nth 2 vals)) (cdr (nth 2 vals)) 'include-file-name)
 ) 'include nil nil)))
 ) ; end include-lib-dir
 (include-file-name
 ( open-paren string close-paren
  ,(semantic-lambda
  (list ( read (nth 1 vals)))))
 ) ; end include-file-name
 (ifdef-dir
 ( punctuation "\\b-\\b" IFDEF semantic-list "^(" full-stop)
 ) ; end ifdef-dir
 (ifndef-dir
 ( punctuation "\\b-\\b" IFNDEF semantic-list "^(" full-stop)
 ) ; end ifndef-dir
 (else-dir
 ( punctuation "\\b-\\b" ELSE full-stop)
 ) ; end else-dir
 (endif-dir
 ( punctuation "\\b-\\b" ENDIF full-stop)
 ) ; end endif-dir
 (full-stop
 ( punctuation "\\b\\.\\b")
 ) ; end full-stop
 )
)
(defvar semantic-erlang-keyword-table
  (semantic-flex-make-keyword-table 
   `( ("begin" . BEGIN)
      ("end" . END)
      ("case" . CASE)
      ("of" . OF)
      ("if" . IF)
      ("when" . WHEN)
      ("true" . TRUE)
      ("receive" . RECEIVE)
      ("after" . AFTER)
      ("or" . OR)
      ("xor" . XOR)
      ("bor" . BOR)
      ("bxor" . BXOR)
      ("bsl" . BSL)
      ("bsr" . BSR)
      ("div" . DIV)
      ("rem" . REM)
      ("band" . BAND)
      ("and" . AND)
      ("bnot" . BNOT)
      ("not" . NOT)
      ("catch" . CATCH)
      ("fun" . FUN)
      ("query" . QUERY)
      ("let" . LET)
      ("module" . MODULE)
      ("include" . INCLUDE)
      ("include_lib" . INCLUDE_LIB)
      ("behaviour" . BEHAVIOUR)
      ("define" . DEFINE)
      ("undef" . UNDEF)
      ("ifdef" . IFDEF)
      ("ifndef" . IFNDEF)
      ("else" . ELSE)
      ("endif" . ENDIF)
      ("export" . EXPORT)
      ("import" . IMPORT)
      ("record" . RECORD)
      )
   '(
     ))
  "Table for use with semantic for keywords.")

(defun semantic-default-erlang-setup ()
  "Setup hook function for Erlang files and Semantic."

  ;; Code generated from erlang.bnf
  (setq semantic-toplevel-bovine-table semantic-toplevel-erlang-bovine-table
	semantic-toplevel-bovine-table-source "erlang.bnf")
  (setq semantic-flex-keywords-obarray semantic-erlang-keyword-table)
  (setq semantic-symbol->name-assoc-list '( (variable . "Definitions")
					    (type     . "Records")
					    (function . "Functions")
					    (include  . "Includes")
					    (package  . "Module"))
	semantic-number-expression nil
	imenu-create-index-function 'semantic-create-imenu-index
	semantic-type-relation-separator-character '(".")
	semantic-command-separation-character ","
	semantic-ignore-comments t
	document-comment-start "%%**"
	document-comment-line-prefix "%%"
	document-comment-end "%%*"
	semantic-flex-syntax-modifications '((?' "_")
					     (?$ "/")
					     )
	semantic-flex-extensions
	`(("\\s/\\(\\(\\\\[0-9]\\{3\\}\\)\\|[^\\]\\|\\\\\\\\\\)"
	   . ,(lambda ()
		(goto-char (match-end 0))
		(cons 'CHAR (cons (match-beginning 0) (match-end 0))))))
	)
 
 ;; End code generated from erlang.bnf
 )

(add-hook 'erlang-mode-hook 'semantic-default-erlang-setup)

(provide 'semantic-erlang)
;;; semantic-erlang.el ends here
