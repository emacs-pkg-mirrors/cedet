;;; wisent.el --- Wisent (European Bison) for Emacs

;; Copyright (C) 2001 David Ponce
;; Copyright (C) 1994, 1996 Dominique Boucher
;;               (Bison Scheme port lalr.scm, lr-dvr.scm)
;; Copyright (C) 1984, 86, 88, 89, 90, 91, 92, 93, 95, 98, 99
;;               Free Software Foundation, Inc. (Bison 1.28)

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent.el,v 1.18 2001/12/18 06:49:15 ponced Exp $

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
;; Wisent is an Elisp implementation of a Bison like parser generator.
;; The code is freely inspired from Dominique Boucher's Scheme
;; implementation of Bison and from the sources of GNU Bison 1.28.
;; Wisent uses the Bison algorithm to compute the LALR(1) look-ahead
;; set.  It is described in:
;;
;; "Efficient Computation of LALR(1) Look-Ahead Set", F.  DeRemer et
;; T.  Pennello, TOPLAS, vol.  4, no.  4, October 1982.
;;
;; Unlike the Dominique Boucher's Scheme implementation Wisent has a
;; Bison like mechanism to recover from parse errors and can resolve
;; shift/reduce conflicts using operator precedence declarations.
;;
;; For more details on the basic concepts for understanding Wisent,
;; read the Bison manual ;)
;;
;; 1. Grammar
;; ==========
;;
;; In order for Wisent to parse a language, it must be described by a
;; Context Free Grammar (CFG).  This grammar is then compiled, using
;; the function `wisent-compile-grammar'.  This produces the tables
;; needed by the parser.  The table is an Emacs Lisp `vector' of
;; vectors and listes.  See `wisent-compile-grammar' for details on
;; the format of the table.
;;
;; `wisent-compile-grammar' is called like this:
;;
;;    (wisent-compile-grammar GRAMMAR [STARTS] [STREAM])
;;
;; - GRAMMAR is the CFG to process.
;;
;; - STARTS (optional) is a list of nonterminal symbols defined as
;;   extra entry points in the grammar.  The first nonterminal
;;   specified in the grammar is always defined as the main entry
;;   point in the grammar.
;;
;; - STREAM (optional) is a `standard-output' stream where the tables
;;   will be pretty-printed.
;;
;;   ****************************************************************
;;   WARNING: As the tables resulting of grammar compilation can be
;;   very large pretty-printing (done via the standard function `pp')
;;   can take a while and consume a lot of computer resources!
;;   ****************************************************************
;;
;; For example:
;;
;;    (wisent-compile-grammar my-grammar nil (current-buffer))
;;
;; inserts the parser tables value at point in current buffer.
;;
;; 1.1 Grammar format
;; ==================
;; 
;; This is a list (TERMINALS ASSOCS . NON-TERMINALS) where:
;;
;; - TERMINALS is the list terminal symbols used in the grammar.
;;
;; - ASSOCS specifies the associativity of terminals like does the
;;   Bison %nonassoc, %left and %right statements.  It is nil or a
;;   list of elements of the form (ASSOC-TYPE . ASSOC-TERMS) where
;;   ASSOC-TYPE is one of 'nonassoc, 'left or 'right, and TERMS is a
;;   list of terminal symbols which must be declared in TERMINALS.
;;
;; - NON-TERMINALS is the list of non terminal definitions.  Each
;;   definition has the form (NONTERM . PRODS) where NONTERM is the
;;   non terminal symbol and PRODS the list of productions associated
;;   to this non terminal.  Each element of PRODS has the form:
;;
;;   (ITEMS [PREC] [ACTION]) where:
;;
;;   -- ITEMS is a list of terminal and/or non terminal symbols to
;;      match.  Or nil for an empty match.
;;
;;   -- PREC (optional) is a vector of one element: a terminal symbol.
;;      The rule will be given the precedence level of this terminal
;;      symbol.  By default the rule precedence is the one of the last
;;      terminal symbol in it.  Only terminals declared in ASSOCS have
;;      a precedence level.
;;
;;   -- ACTION (optional) is one Elisp expression: the semantic action
;;      to do when the rule is matched.  Like in Bison, action accept
;;      `$<i>' placeholders.  Each `$<i>' placeholder will receive the
;;      value of i-th element of ITEMS.  The first element is `$1',
;;      the second one `$2', etc..  Unlike Bison, Wisent does not allow
;;      the use of `$<i>' placeholders with i <= 0.
;;
;;      Also the special variable `$region' contains the start/end
;;      positions of text matched by the rule, as a pair (START-POS
;;      . END-POS).  `$region' is nil when used in an empty match
;;      rule.  Actions can use `wisent-set-region' to modify the
;;      region, this could be useful in actions used to recover from
;;      syntax errors.
;;
;;      Finally the special symbol `$nterm' contain the nonterminal
;;      symbol the action belongs to.  It could be useful to improve
;;      error reporting or debugging.
;;
;;      If you don't specify an action for a rule, Wisent (like Bison)
;;      supplies a default: `$1'.  Thus, the value of the first symbol
;;      in the rule becomes the value of the whole rule.  The default
;;      action for an empty match returns nil.
;;
;; 1.2 Conflicts
;; =============
;;
;; Wisent resolves conflicts in grammar the same way Bison does.
;;
;; It resolves a reduce/reduce conflict by choosing to use the rule
;; that appears first in the grammar.
;;
;; It resolves shift/reduce conflicts by choosing to shift, unless
;; otherwise directed by operator precedence declarations (see ASSOCS
;; and NON-TERMINALS in chapter 1.1 "Grammar format" above).
;;
;; 1.3 Example
;; ===========
;;
;; Here is an example of a Bison like grammar to parse simple infix
;; arithmetic expressions.  It is explained more fully in the Bison
;; manual: "Infix Calc(bison.info)".
;;
;;   %token NUM
;;   %token NL
;;   %token EQ
;;   %token PLUS
;;   %token MINUS
;;   %token NEG
;;   %token MULT
;;   %token DIV
;;   %token EXP
;;   %token LPAREN
;;   %token RPAREN
;;
;;   %nonassoc EQ
;;   %left     PLUS MINUS
;;   %left     MULT DIV
;;   %left     NEG
;;   %right    EXP
;;
;;   input:
;;       /* empty */
;;     | input line
;;     ;

;;   line:
;;       NL
;;     | exp NL
;;     ;

;;   exp:
;;       NUM
;;     | exp EQ exp
;;     | exp PLUS exp
;;     | exp MINUS exp
;;     | exp MULT exp
;;     | exp DIV exp
;;     | MINUS exp %prec NEG
;;     | exp EXP exp
;;     | LPAREN exp RPAREN
;;     ;
;;
;; And following is the corresponding form accepted by Wisent:
;;
;;   ((NUM NL EQ PLUS MINUS NEG MULT DIV EXP LPAREN RPAREN)
;;    ((nonassoc EQ)
;;     (left PLUS MINUS)
;;     (left MULT DIV)
;;     (left NEG)
;;     (right EXP))
;;    (input
;;     (nil)
;;     ((input line)))
;;    (line
;;     ((NL))
;;     ((exp NL)))
;;    (exp
;;     ((NUM))
;;     ((exp EQ exp))
;;     ((exp PLUS exp))
;;     ((exp MINUS exp))
;;     ((exp MULT exp))
;;     ((exp DIV exp))
;;     ((MINUS exp) [NEG])
;;     ((exp EXP exp))
;;     ((LPAREN exp RPAREN))))
;;
;; 2. Parsing
;; ==========
;;
;; The parser used by Wisent is a bottom-up LR parser driven by the
;; tables produced by `wisent-compile-grammar' from a CFG grammar.  To
;; start the parser call the function `wisent-parse' like this:
;;
;;   (wisent-parse TABLES LEXER ERROR-FUNCTION [START])
;;
;; It returns the semantic value of the START nonterminal (the goal).
;;
;; - TABLES are the parser tables produced by
;;   `wisent-compile-grammar'.
;;
;; - LEXER is a function with no argument called by the parser to
;;   obtain the next terminal in input.  LEXER must return a list
;;   (TERM TERM-VALUE [TERM-START . TERM-END]) or (`wisent-eoi-term')
;;   when at the end of the input.
;;
;;   -- TERM is the unique symbol identifying a terminal as specified
;;      in the grammar.
;;
;;   -- TERM-VALUE is the actual value of the terminal.
;;
;;   -- TERM-START and TERM-END (optionals) are the start and end
;;      positions of the terminal string value in the input stream.
;;      You can use the macro `wisent-lexer' to call LEXER in semantic
;;      actions.
;;
;; - ERROR-FUNCTION is a reporting function called when a parse error
;;   occurs.  It receives a message string to report.  When
;;   ERROR-FUNCTION is called by the parser the variable
;;   `wisent-input' contains the unexpected token as returned by
;;   LEXER.  This function can also be called from semantic actions
;;   using the special macro `wisent-error'.  When called from a
;;   recovery action the value of the variable `wisent-recovering' is
;;   non-nil.
;;
;; - START specify the nonterminal symbol used by the parser as its
;;   goal.  It defaults to the first nonterminal specified in the
;;   grammar.  START must be one the STARTS symbol specified at
;;   compilation time (see chapter 1 "Grammar" above).

;;; History:
;; 

;;; Code:
(require 'working)

;;;;
;;;; Global constants
;;;;

;; Reserved symbols
(defconst wisent-start-nonterm    '$START
  "Main entry point nonterminal.")
(defconst wisent-starts-nonterm   '$STARTS
  "Secondary entry point nonterminal.
The associated production gives the rules for entry point
nonterminals.")
(defconst wisent-eoi-term         '$EOI
  "End of input terminal.")
(defconst wisent-error-term       'error
  "Error recovery terminal.")

(defconst wisent-reserved-symbols
  (list wisent-error-term)
  "The list of reserved symbols.
Also all symbol starting with '$' are reserved for internal use.")

;; Special tags in action table
(defconst wisent-accept-tag 'accept
  "Accept result after input successfully parsed.")
(defconst wisent-error-tag 'error
  "Process a syntax error.")
(defconst wisent-default-tag 'default
  "Indicate the default action.")

;; System dependent
(defconst wisent-bits-per-word 24
  "Number of bits per word used in bit vector management.")

;;;;
;;;; Customization
;;;;

(defgroup wisent nil
  "The Bison like parser generator for Emacs.

                  /\\_.-^^^-._/\\
                  \\__       __/
                    `    `o `
                     \\     `/
                     (  D ,¨
                      \"\"\""
  :group 'semantic)

(defcustom wisent-parse-max-stack-size 500
  "*The parser stack size."
  :type 'integer
  :group 'wisent)

(defcustom wisent-parse-max-recover 3
  "*Number of tokens to shift before turning off error status."
  :type 'integer
  :group 'wisent)

(defcustom wisent-state-table-size 1009
  "The size of `wisent--state-table'."
  :type 'integer
  :group 'wisent)

(defvar wisent-skip-token-hook nil
  "Hook run when the parser skips a lexical token.
The hook function receives the lexical token skipped.  For language
specific hooks, make sure you define this as a local hook.")

;;;;
;;;; Bit vector management
;;;;

(defsubst wisent-set-bit (x i)
  "Set bit vector X with I.
This function is equivalent to the Bison 1.28 macro SET-BIT:
SETBIT(x, i) ((x)[(i)/BITS_PER_WORD] |= (1<<((i) % BITS_PER_WORD)))."
  (let ((k (/ i wisent-bits-per-word)))
    (aset x k (logior (aref x k)
                      (ash 1 (% i wisent-bits-per-word))))))

(defsubst wisent-bit-union (v1 v2 n)
  "Do `logior' of bit vectors V1 and V2.
N is the number of elements to process.  The result is stored in V1."
  (let ((i 0))
    (while (< i n)
      (aset v1 i (logior (aref v1 i) (aref v2 i)))
      (setq i (1+ i)))))

;; Data structures macros
(defmacro wisent-new-core ()
  "Create a new core structure."
  `(make-vector 4 0))

(defmacro wisent-set-core-number (c n)
  "Set the 'number' part in core structure C to N."
  `(aset ,c 0 ,n))

(defmacro wisent-set-core-acc-sym (c s)
  "Set the 'acc-sym' part in core structure C to S."
  `(aset ,c 1 ,s))

(defmacro wisent-set-core-nitems (c n)
  "Set the 'nitems' part in core structure C to N."
  `(aset ,c 2 ,n))

(defmacro wisent-set-core-items (c i)
  "Set the 'items' part in core structure C to I."
  `(aset ,c 3 ,i))

(defmacro wisent-core-number (c)
  "Return the 'number' part of core structure C."
  `(aref ,c 0))

(defmacro wisent-core-acc-sym (c)
  "Return the 'acc-sym' part of core structure C."
  `(aref ,c 1))

(defmacro wisent-core-nitems (c)
  "Return the 'nitems' part of core structure C."
  `(aref ,c 2))

(defmacro wisent-core-items (c)
  "Return the 'items' part of core structure C."
  `(aref ,c 3))

(defmacro wisent-new-shift ()
  "Create a new shift structure."
  `(make-vector 3 0))

(defmacro wisent-set-shift-number (s x)
  "Set the 'number' part in shift structure S to X."
  `(aset ,s 0 ,x))

(defmacro wisent-set-shift-nshifts (s x)
  "Set the 'nshift' part in shift structure S to X."
  `(aset ,s 1 ,x))

(defmacro wisent-set-shift-shifts (s x)
  "Set the 'shifts' part in shift structure S to X."
  `(aset ,s 2 ,x))

(defmacro wisent-shift-number (s)
  "Return the 'number' part of shift structure S."
  `(aref ,s 0))

(defmacro wisent-shift-nshifts (s)
  "Return the 'nshifts' part of shift structure S."
  `(aref ,s 1))

(defmacro wisent-shift-shifts (s)
  "Return the 'shifts' part of shift structure S."
  `(aref ,s 2))

(defmacro wisent-new-red ()
  "Create a new red structure."
  `(make-vector 3 0))

(defmacro wisent-set-red-number (r x)
  "Set the 'number' part in red structure R to X."
  `(aset ,r 0 ,x))

(defmacro wisent-set-red-nreds (r x)
  "Set the 'nreds' part in red structure R to X."
  `(aset ,r 1 ,x))

(defmacro wisent-set-red-rules (r x)
  "Set the 'rules' part in red structure R to X."
  `(aset ,r 2 ,x))

(defmacro wisent-red-number (r)
  "Return the 'number' part of red structure R."
  `(aref ,r 0))

(defmacro wisent-red-nreds (r)
  "Return the 'nreds' part of red structure R."
  `(aref ,r 1))

(defmacro wisent-red-rules (r)
  "Return the 'rules' part of red structure R."
  `(aref ,r 2))

(defmacro wisent-new-set (nelem)
  "Create a new set of NELEM elements."
  `(make-vector ,nelem 0))

;; Tables
(defvar wisent--terms nil
  "Hold the list of terminal symbols.")
(defvar wisent--vars  nil
  "Hold the list of nonterminal symbols.")
(defvar wisent--rrhs            nil
  "Rule right hand side.")
(defvar wisent--rlhs            nil
  "Rule left hand side.")
(defvar wisent--ritem           nil
  "Rule item numbers.")
(defvar wisent--nullable        nil
  "Nullable nonterminal flags.")
(defvar wisent--derives         nil
  "Rules that can derive each nonterminal.")
(defvar wisent--fderives        nil
  "Rules that can help derive beginning of data of each nonterminal.")
(defvar wisent--firsts          nil
  "Vector of item number listes indicating first items.")
(defvar wisent--kernel-base     nil
  "Start indexes of item numbers activated if a symbol is shifted.")
(defvar wisent--kernel-end      nil
  "End indexes of item numbers activated if a symbol is shifted.")
(defvar wisent--shift-symbol    nil
  "Symbols that can be shifted.")
(defvar wisent--shift-set       nil
  "State numbers reached by each shift transition.")
(defvar wisent--state-table     nil
  "The state table.")
(defvar wisent--access-symbol   nil
  "Indexes of accessing symbol of each state.")
(defvar wisent--reduction-table nil
  "The reduction table.")
(defvar wisent--shift-table     nil
  "The shift table.")
(defvar wisent--consistent      nil
  "For each state, non-nil if no LA is needed to decide what to do.")
(defvar wisent--lookaheads      nil
  "Lookaheads.")
(defvar wisent--la              nil
  "Matrix of bits.")
(defvar wisent--laruleno        nil
  "Rules that need lookahead in various states.")
(defvar wisent--lookback        nil
  "Lookback edges.")
(defvar wisent--goto-map        nil
  "Indexes of first transition states.")
(defvar wisent--from-state      nil
  "State numbers which transitions lead from.")
(defvar wisent--to-state        nil
  "State numbers which transitions lead to.")
(defvar wisent--includes        nil
  "Includes.")
(defvar wisent--f               nil
  "Working table to compute lookaheads.")
(defvar wisent--action-table    nil
  "Action table.")
(defvar wisent--rules           nil
  "Vector of grammar rules in internal format.")
(defvar wisent--rprec           nil
  "Precedence of rules.")
(defvar wisent--conflicts-by-state nil
  "Number of conflicts by state.
This is a vector of pairs (SR-conflicts . RR-conflicts).")

;; Variables
(defvar wisent--nitems          nil
  "Number of items.")
(defvar wisent--nrules          nil
  "Number of grammar rules.")
(defvar wisent--nvars           nil
  "Number of non terminals.")
(defvar wisent--nterms          nil
  "Number of terminals.")
(defvar wisent--nsyms           nil
  "Number of non terminals and terminals.")
(defvar wisent--nstates         nil
  "Number of states.")
(defvar wisent--first-state     nil
  "Record first state.")
(defvar wisent--last-state      nil
  "Record last state.")
(defvar wisent--final-state     nil
  "Record final state.")
(defvar wisent--first-shift     nil
  "Record first shift.")
(defvar wisent--last-shift      nil
  "Record last shift.")
(defvar wisent--first-reduction nil
  "Record first reduction.")
(defvar wisent--last-reduction  nil
  "Record last reduction.")
(defvar wisent--nshifts         nil
  "Number of symbols that can be shifted.")
(defvar wisent--ngotos          nil
  "Number of transitions which accept a nonterminal.")
(defvar wisent--token-set-size  nil
  "Size of a token set.")
(defvar wisent--conflicts nil
  "Total number of conflicts found.
The value is a pair (SR-conflicts . RR-conflicts).")

;; Symbol access
(defsubst wisent-terminal-symbol (sym)
  "Return symbol value of terminal ID SYM."
  (if (numberp sym)
      (elt wisent--terms sym)
    sym))

(defsubst wisent-item-symbol (i)
  "Return symbol value of item number I."
  (if (>= i wisent--nvars)
      (elt wisent--terms (- i wisent--nvars))
    (elt wisent--vars i)))

;; Misc.
(eval-when-compile
  (or (fboundp 'noninteractive)
      ;; Silence the Emacs byte compiler
      (defun noninteractive nil))
  )

(defsubst wisent-noninteractive ()
  "Return non-nil if running without interactive terminal."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

;; Logging/Output
(defvar wisent-log-buffer nil
  "Hold the logging buffer.")

(defmacro wisent-output (string &rest objects)
  "Using control-string STRING, format and insert OBJECTS arguments."
  `(insert (format ,string ,@objects)))

(defun wisent-log-buffer ()
  "Return the logging buffer."
  (or wisent-log-buffer
      (progn
        (setq wisent-log-buffer "*wisent-log*")
        (with-current-buffer (get-buffer-create wisent-log-buffer)
          (wisent-output (format-time-string
                          "*** %x %X - wisent log started ***\n\n")))))
  (get-buffer-create wisent-log-buffer))

(defun wisent-log (&rest args)
  "Insert an element into the logging buffer.
`format' is applied to ARGS and the result string is inserted into the
logging buffer."
  (with-current-buffer (wisent-log-buffer)
    (insert (apply #'format args))))

(defconst wisent-log-file "wisent.output"
  "The logging file.
Used when running without interactive terminal.")

(defun wisent-append-to-log-file ()
  "Append contents of `wisent-log-buffer' to `wisent-log-file'."
  (if wisent-log-buffer
      (condition-case err
          (with-current-buffer (wisent-log-buffer)
            (widen)
            (write-region (point-min) (point-max) wisent-log-file t))
        (error
         (message "%s" (error-message-string err))))))

(defun wisent-log-starts (starts)
  "Log the list of start nonterminals.
STARTS is the table of defined entry points."
  (with-current-buffer (wisent-log-buffer)
    (wisent-output "\n;;; entry point nonterminals:\n")
    (while starts
      (wisent-output ";; %s\n" (caar starts))
      (setq starts (cdr starts)))))

(defun wisent-log-nullables ()
  "Log the list of nullable nonterminals."
  (with-current-buffer (wisent-log-buffer)
    (wisent-output "\n;;; nullable nonterminals:\n")
    (let ((i 0))
      (while (< i wisent--nvars)
        (if (aref wisent--nullable i)
            (wisent-output ";; %d - %s\n" i (elt wisent--vars i)))
        (setq i (1+ i))))))

(defun wisent-log-rules ()
  "Log grammar rules."
  (let ((i 1)
        lhs rhs)
    (with-current-buffer (wisent-log-buffer)
      (wisent-output "\n;;; rules:\n")
      (while (< i wisent--nrules)
        (wisent-output ";; %d - " i)
        (setq lhs (car (aref wisent--rules i))
              rhs (cdr (aref wisent--rules i)))
        (wisent-output "%s -> %s\n"
                       (elt wisent--vars lhs)
                       (if rhs
                           (mapconcat
                            #'(lambda (i)
                                (format "%s" (wisent-item-symbol i)))
                            rhs
                            " ")
                         "/* empty */"))
        (setq i (1+ i))))))

(defun wisent-log-conflicts ()
  "Log summary of grammar conflicts."
  (if wisent--conflicts
      (let ((sr  (car wisent--conflicts))
            (rr  (cdr wisent--conflicts))
            (msg "\nGrammar contains ")
            (i   0))
        (if (> sr 0)
            (setq msg (format "%s%d shift/reduce conflict%s%s"
                              msg
                              sr
                              (if (> sr 1) "s" "")
                              (if (> rr 0) " and " "."))))
        (if (> rr 0)
            (setq msg (format "%s%d reduce/reduce conflict%s."
                              msg
                              rr
                              (if (> rr 1) "s" ""))))
        (message msg)
        (with-current-buffer (wisent-log-buffer)
          (wisent-output "%s\n\n" msg)
          (while (< i wisent--nstates)
            (if (aref wisent--conflicts-by-state i)
                (progn
                  (setq sr (car (aref wisent--conflicts-by-state i))
                        rr (cdr (aref wisent--conflicts-by-state i)))
                  (wisent-output "State %d contains " i)
                  (if (> sr 0)
                      (wisent-output "%d shift/reduce conflict%s%s"
                                     sr
                                     (if (> sr 1) "s" "")
                                     (if (> rr 0) " and " ".\n")))
                  (if (> rr 0)
                      (wisent-output "%d reduce/reduce conflict%s.\n"
                                     rr
                                     (if (> rr 1) "s" "")))))
            (setq i (1+ i)))))))

(defun wisent-log-rr-conflict (state termid r1 r2 rs)
  "Log a reduce/reduce conflict.
STATE is the state at which the conflict occurs.  TERMID is the
terminal id which raises the conflict.  R1 and R2 are the rule numbers
in conflict.  RS is the rule number used to solve the conflict."
  (let ((conflicts (aref wisent--conflicts-by-state state)))
    (if (consp conflicts)
        (setcdr conflicts (1+ (cdr conflicts)))
      (aset wisent--conflicts-by-state state (cons 0 1)))
    (if (consp wisent--conflicts)
        (setcdr wisent--conflicts (1+ (cdr wisent--conflicts)))
      (setq wisent--conflicts (cons 0 1))))
  (wisent-log
   "Conflict in state %d between rules %d and %d on token %s\
 resolved as rule %d.\n"
   state r1 r2 (wisent-terminal-symbol termid) rs))

(defun wisent-log-sr-conflict (state termid r1 r2 rs)
  "Log a shift/reduce conflict.
STATE is the state at which the conflict occurs.  TERMID is the
terminal id which which raises the conflict.  R1 is the shift rule
number.  R2 is the reduce rule number in conflict with TERMID.  RS
describes the resolve choice, that is \"shift\" or \"reduce\"."
  (let ((conflicts (aref wisent--conflicts-by-state state)))
    (if (consp conflicts)
        (setcar conflicts (1+ (car conflicts)))
      (aset wisent--conflicts-by-state state (cons 1 0)))
    (if (consp wisent--conflicts)
        (setcar wisent--conflicts (1+ (car wisent--conflicts)))
      (setq wisent--conflicts (cons 1 0))))
  (wisent-log
   "Conflict in state %d between rule %d and token %s\
 resolved as %s.\n"
   state r2 (wisent-terminal-symbol termid) rs))
   
;; Debug
(defvar wisent-debug-flag nil
  "Non-nil means enable some debug stuff.")

;; Utilities
(defun wisent-pos-in-list (x lst)
  "Return the position of X in list LST or nil if not found.
Use `equal' to compare elements."
  (let ((i 0) p)
    (while (and (consp lst) (not p))
      (setq p (and (equal (car lst) x) i)
            lst (cdr lst)
            i (1+ i)))
    p))

(defun wisent-sunion (l1 l2)
  "Return the sorted union of listes L1 and L2."
  (let ((l nil) x y)
    (while (and l1 l2)
      (setq x (car l1)
            y (car l2))
      (cond ((> x y)
             (setq l  (cons y l)
                   l2 (cdr l2)))
            ((< x y)
             (setq l  (cons x l)
                   l1 (cdr l1)))
            (t
             (setq l1 (cdr l1)))))
    (append (nreverse l) (or l1 l2))))

(defun wisent-sinsert (elem l)
  "Insert number ELEM in list L and return the new list.
Keep order of elements in L."
  (if (null l)
      (cons elem l)
    (let ((l1 l)
          (l2 nil)
          x)
      (while (and l1 (> elem (setq x (car l1))))
        (setq l2 (cons x l2)
              l1 (cdr l1)))
      (cond ((> elem x)
             (nreverse (cons elem l2)))
            ((< elem x)
             (append (nreverse l2) (cons elem l1)))
            (t
             l)))))

(defun wisent-filter (p lst)
  "Filter with predicate P the list LST and return the new list.
Each element of the returned list verifies that (funcall P element)
returns non-nil."
  (let (l e)
    (while lst
      (setq e (car lst)
            lst (cdr lst))
      (if (funcall p e)
          (setq l (cons e l))))
    (nreverse l)))

;; Cleanup/initialization
(defun wisent-free-all ()
  "Cleanup all global variables."
  (setq wisent--state-table        nil
        wisent--rrhs               nil
        wisent--rlhs               nil
        wisent--ritem              nil
        wisent--nullable           nil
        wisent--derives            nil
        wisent--fderives           nil
        wisent--firsts             nil
        wisent--kernel-base        nil
        wisent--kernel-end         nil
        wisent--shift-symbol       nil
        wisent--shift-set          nil
        wisent--access-symbol      nil
        wisent--reduction-table    nil
        wisent--shift-table        nil
        wisent--consistent         nil
        wisent--lookaheads         nil
        wisent--la                 nil
        wisent--laruleno           nil
        wisent--lookback           nil
        wisent--goto-map           nil
        wisent--from-state         nil
        wisent--to-state           nil
        wisent--includes           nil
        wisent--f                  nil
        wisent--action-table       nil
        wisent--terms              nil
        wisent--vars               nil
        wisent--rules              nil
        wisent--rprec              nil
        wisent--conflicts-by-state nil
        wisent--first-state        nil
        wisent--last-state         nil
        wisent--final-state        nil
        wisent--first-shift        nil
        wisent--last-shift         nil
        wisent--first-reduction    nil
        wisent--last-reduction     nil
        wisent--nshifts            nil
        wisent--ngotos             nil
        wisent--nstates            nil
        wisent--nvars              nil
        wisent--nterms             nil
        wisent--nrules             nil
        wisent--token-set-size     nil
        wisent-log-buffer          nil))

(defun wisent-initialize-all ()
  "Set up all global variables to initial state."
  (wisent-free-all)
  (setq wisent--state-table (make-vector
                             wisent-state-table-size nil)))

(defun wisent-pack-grammar (gram)
  "Setup the data structures from input grammar GRAM.
The input grammar is in an internal format built by
`wisent-process-grammar'."
  (working-dynamic-status "(pack-grammar)")
  (setq wisent--rlhs  (make-vector wisent--nrules nil)
        wisent--rrhs  (make-vector wisent--nrules nil)
        wisent--ritem (make-vector (1+ wisent--nitems) nil))
  (let ((p gram)
        (item-no 0)
        (rule-no 1)
        nt prods it-no2 rl-no2 rhs it-no3)
    (while p
      (setq nt     (caar p)
            prods  (cdar p)
            it-no2 item-no
            rl-no2 rule-no)
      (while prods
        (aset wisent--rlhs rl-no2 nt)
        (aset wisent--rrhs rl-no2 it-no2)
        (setq rhs (car prods) it-no3 it-no2)
        (while rhs

          ;; Setup default precedence level of rule.
          (or (aref wisent--rprec rl-no2) ;; Already set by %prec
              (< (car rhs) wisent--nvars) ;; non terminal
              ;; A rule gets by default the precedence of the last
              ;; terminal in it.
              (let ((term (elt wisent--terms (- (car rhs) wisent--nvars))))
                (aset wisent--rprec rl-no2 (get term 'wisent--prec))))
          
          (aset wisent--ritem it-no3 (car rhs))
          (setq rhs    (cdr rhs)
                it-no3 (1+ it-no3)))
        (aset wisent--ritem it-no3 (- rl-no2))
        (setq prods  (cdr prods)
              it-no2 (1+ it-no3)
              rl-no2 (1+ rl-no2)))
      (setq p       (cdr p)
            item-no it-no2
            rule-no rl-no2))))

(defun wisent-set-derives ()
  "Set up `wisent--derives'.
`wisent-set-derives' finds, for each variable (nonterminal), which
rules can derive it.  It sets up the value of `wisent--derives' so
that wisent--derives[i - ntokens] points to a list of rule numbers,
terminated with -1."
  (working-dynamic-status "(set-derives)")
  (let ((delts (make-vector (1+ wisent--nrules) 0))
        (dset  (make-vector wisent--nvars -1))
        (i 1) ;; i = 0
        (j 0)
        s x lhs)
    (while (< i wisent--nrules)
      (setq lhs (aref wisent--rlhs i))
      (if (>= lhs 0)
          (progn
            (aset delts j (cons i (aref dset lhs)))
            (aset dset lhs j)
            (setq i (1+ i) j (1+ j)))
        (setq i (1+ i))))
    (setq wisent--derives (make-vector wisent--nvars 0)
          i 0)
    (while (< i wisent--nvars)
      (setq j (aref dset i)
            s nil)
      (while (>= j 0)
        (setq x (aref delts j)
              j (cdr x)
              s (cons (car x) s)))
      (aset wisent--derives i s)
      (setq i (1+ i)))))

(defun wisent-set-nullable ()
  "Set up `wisent--nullable'.
That is a vector saying which nonterminals can expand into the null
string.  wisent--nullable[i - ntokens] is non-nil if symbol i can do
so."
  (working-dynamic-status "(set-nullable)")
  (setq wisent--nullable (make-vector wisent--nvars nil))
  (let ((squeue (make-vector wisent--nvars 0))
        (rcount (make-vector (1+ wisent--nrules) 0))
        (rsets  (make-vector wisent--nvars nil))
        (relts  (make-vector (+ wisent--nitems wisent--nvars 1) nil))
        (r 0) (s1 0) (s2 0) (p 0)
        *r symbol r1 any-tokens ruleno n/v)
    
    (while (setq *r (aref wisent--ritem r))
      (if (< *r 0)
          (progn
            (setq symbol (aref wisent--rlhs (- *r))
                  r      (1+ r))
            (if (and (>= symbol 0)
                     (not (aref wisent--nullable symbol)))
                (progn
                  (aset wisent--nullable symbol t)
                  (aset squeue s2 symbol)
                  (setq s2 (1+ s2)))))
        ;; else
        (setq r1 r
              any-tokens nil)
        (setq symbol (aref wisent--ritem r)
              r (1+ r))
        (while (> symbol 0)
          (if (>= symbol wisent--nvars)
              (setq any-tokens t))
          (setq symbol (aref wisent--ritem r)
                r (1+ r)))
        (if (not any-tokens)
            (progn
              (setq ruleno (- symbol)
                    r r1)
              (setq symbol (aref wisent--ritem r)
                    r (1+ r))
              (while (> symbol 0)
                (aset rcount ruleno (1+ (aref rcount ruleno)))
                (aset relts p (cons (aref rsets symbol) ruleno))
                (aset rsets symbol p)
                (setq p (1+ p)
                      symbol (aref wisent--ritem r)
                      r (1+ r)))))))
    
    (while (< s1 s2)
      (setq p (aref rsets (aref squeue s1))
            s1 (1+ s1))
      (while p
        (setq n/v    (aref relts p)
              p      (car n/v)
              ruleno (cdr n/v))
        (if (= (aset rcount ruleno (1- (aref rcount ruleno))) 0)
            (progn
              (setq symbol (aref wisent--rlhs ruleno))
              (if (and (>= symbol 0) (not (aref wisent--nullable symbol)))
                  (progn
                    (aset wisent--nullable symbol t)
                    (aset squeue s2 symbol)
                    (setq s2 (1+ s2))))))))
    ))

(defun wisent-set-firsts ()
  "Set up `wisent--firsts'.
That is a vector of item number listes indicating which items can
represent the beginning of the input corresponding to which other
items."
  (setq wisent--firsts (make-vector wisent--nvars nil))
  ;; -- initialization
  (let ((i 0)
        sp sym cont x y l)
    (while (< i wisent--nvars)
      (setq sp (aref wisent--derives i))
      (while sp
        (setq sym (aref wisent--ritem (aref wisent--rrhs (car sp))))
        (if (and (< -1 sym) (< sym wisent--nvars))
            (aset wisent--firsts i
                  (wisent-sinsert sym (aref wisent--firsts i))))
        (setq sp (cdr sp)))
      (setq i (1+ i)))
    ;; -- reflexive and transitive closure
    (setq cont t)
    (while cont
      (setq i    0
            cont nil)
      (while (< i wisent--nvars)
        (setq x (aref wisent--firsts i)
              l x
              y x)
        (while l
          (setq y (wisent-sunion (aref wisent--firsts (car l)) y)
                l (cdr l)))
        (if (equal x y)
            nil
          (setq cont t)
          (aset wisent--firsts i y))
        (setq i (1+ i))))
    (setq i 0)
    (while (< i wisent--nvars)
      (aset wisent--firsts i
            (wisent-sinsert i (aref wisent--firsts i)))
      (setq i (1+ i)))))

(defun wisent-set-fderives ()
  "Set up `wisent--fderives'.
That is a vector of item number listes indicating which rules can help
derive the beginning of the data for each nonterminal."
  (setq wisent--fderives (make-vector wisent--nvars nil))
  (wisent-set-firsts)
  (let ((i 0) fd l)
    (while (< i wisent--nvars)
      (setq l (aref wisent--firsts i)
            fd nil)
      (while l
        (setq fd (wisent-sunion (aref wisent--derives (car l)) fd)
              l  (cdr l)))
      (aset wisent--fderives i fd)
      (setq i (1+ i)))))

(defun wisent-closure (core)
  "Return closure of the list of item numbers CORE.
Set up 'ruleset' and 'itemset' to indicate what rules could be run and
which items could be accepted when those items are the active ones.

'ruleset' contains a flag for each rule.  `wisent-closure' sets the
flag to non-nil for all rules which could potentially describe the
next input to be read.

'itemset' is the list of item numbers returned.  `wisent-closure'
places there the indices of all items which represent units of input
that could arrive next."
  (let ((ruleset (make-vector wisent--nrules nil))
        (csp     core)
        (ruleno  1)
        dsp sym itemsetv itemsetv2 itemno c)
    (while csp
      (setq sym (aref wisent--ritem (car csp)))
      (if (and (< -1 sym) (< sym wisent--nvars))
          (progn
            (setq dsp (aref wisent--fderives sym))
            (while dsp
              (aset ruleset (car dsp) t)
              (setq dsp (cdr dsp)))))
      (setq csp (cdr csp)))
    (setq csp core)
    (while (< ruleno wisent--nrules)
      (if (aref ruleset ruleno)
          (progn
            (setq itemno (aref wisent--rrhs ruleno)
                  c csp
                  itemsetv2 itemsetv)
            (while (and (consp c) (< (car c) itemno))
              (setq itemsetv2 (cons (car c) itemsetv2)
                    c (cdr c)))
            (setq csp c
                  itemsetv (cons itemno itemsetv2))))
      (setq ruleno (1+ ruleno)))
    (while (consp csp)
      (setq itemsetv (cons (car csp) itemsetv)
            csp (cdr csp)))
    (nreverse itemsetv)))

(defun wisent-initialize-states ()
  "Initialize states."
  (let ((p (wisent-new-core)))
    (wisent-set-core-number   p 0)
    (wisent-set-core-acc-sym  p nil)
    (wisent-set-core-nitems   p 1)
    (wisent-set-core-items    p '(0))
    (setq wisent--first-state (list p)
          wisent--last-state  wisent--first-state
          wisent--nstates     1)))

(defun wisent-generate-states ()
  "Compute the nondeterministic finite state machine from the grammar."
  (working-dynamic-status "(generate-states)")
  ;; Allocate storage
  (setq wisent--kernel-base (make-vector wisent--nsyms 0)
        wisent--kernel-end  (make-vector wisent--nsyms nil))
  (wisent-set-fderives)
  (wisent-initialize-states)
  (let ((this-state wisent--first-state)
        x is)
    (while (consp this-state)
      (setq x  (car this-state)
            is (wisent-closure (wisent-core-items x)))
      (wisent-save-reductions x is)
      (wisent-new-itemsets is)
      (wisent-append-states)
      (if (> wisent--nshifts 0)
          (wisent-save-shifts x))
      ;; States are queued (that is `this-state' value has been
      ;; changed by side effect!) when they are created; process them
      ;; all.
      (setq this-state (cdr this-state)))
    ))

(defun wisent-new-itemsets (itemset)
  "Find which symbols can be shifted in the current state.
And for each one record which items would be active after that shift.
Uses the contents of ITEMSET.  `wisent--shift-symbol' is set to a
vector of the symbols that can be shifted.  For each symbol in the
grammar, wisent--kernel-base[symbol] points to a vector of item
numbers activated if that symbol is shifted, and
wisent--kernel-end[symbol] points after the end of that vector."
  (setq wisent--shift-symbol nil)
  (fillarray wisent--kernel-end nil)
  (let ((isp itemset)
        i sym x)
    (while (consp isp)
      (setq i (car isp)
            sym (aref wisent--ritem i))
      (if (>= sym 0)
          (progn
            (setq wisent--shift-symbol
                  (wisent-sinsert sym wisent--shift-symbol)
                  x (aref wisent--kernel-end sym))
            (if (null x)
                (progn
                  (aset wisent--kernel-base
                        sym (cons (1+ i) x))
                  (aset wisent--kernel-end
                        sym (aref wisent--kernel-base sym)))
              (setcdr x (list (1+ i)))
              (aset wisent--kernel-end sym (cdr x)))))
      (setq isp (cdr isp))))
  (setq wisent--nshifts (length wisent--shift-symbol)))

(defun wisent-get-state (sym)
  "Find SYM state number.
That is the state number for the state we would get to (from the
current state) by shifting symbol SYM.  Create a new state if no
equivalent one exists already.  Used by `wisent-append-states'."
  (let* ((isp (aref wisent--kernel-base sym))
         (n   (length isp))
         (x   isp)
         (y   0)
         key sp)
    (while x
      (setq y (+ y (car x))
            x (cdr x)))
    (setq key (mod y wisent-state-table-size)
          sp  (aref wisent--state-table key))
    (if (null sp)
        (progn
          (setq x (wisent-new-state sym))
          (aset wisent--state-table key (list x)))
      (setq x nil)
      (while (not x)
        (setq y (wisent-core-items (car sp)))
        (while (and (consp isp) (= (car isp) (car y)))
          (setq isp (cdr isp)
                y   (cdr y)))
        (if (and (= n (wisent-core-nitems (car sp))) (null isp))
            (setq x (car sp))
          (if (null (cdr sp))
              (progn
                (setq x (wisent-new-state sym))
                (setcdr sp (list x)))
            (setq sp (cdr sp))))))
    (wisent-core-number x)))

(defun wisent-new-state (sym)
  "Create a new state for SYM items, if necessary.
Subroutine of `wisent-get-state'."
  (let* ((isp (aref wisent--kernel-base sym))
         (n   (length isp))
         (p   (wisent-new-core)))
    (wisent-set-core-number  p wisent--nstates)
    (wisent-set-core-acc-sym p sym)
    (if (= sym wisent--nvars)
        (setq wisent--final-state wisent--nstates))
    (wisent-set-core-nitems p n)
    (wisent-set-core-items  p isp)
    (setcdr wisent--last-state (list p))
    (setq wisent--last-state (cdr wisent--last-state)
          wisent--nstates    (1+ wisent--nstates))
    p))

(defun wisent-append-states ()
  "Find or create the core structures for states.
Use the information computed by `wisent-new-itemsets' to find the
state numbers reached by each shift transition from the current state.
`wisent--shift-set' is set up as a list of state numbers of those
states."
  (let ((l wisent--shift-symbol) ss)
    (while l
      (setq ss (cons (wisent-get-state (car l)) ss)
            l  (cdr l)))
    (setq wisent--shift-set ss)))

(defun wisent-save-shifts (core)
  "Create the shifts structures for the shifts to CORE states."
  (let ((p (wisent-new-shift)))
    (wisent-set-shift-number  p (wisent-core-number core))
    (wisent-set-shift-nshifts p wisent--nshifts)
    (wisent-set-shift-shifts  p wisent--shift-set)
    (if wisent--last-shift
        (progn
          (setcdr wisent--last-shift (list p))
          (setq wisent--last-shift (cdr wisent--last-shift)))
      (setq wisent--first-shift (list p))
      (setq wisent--last-shift wisent--first-shift))))

(defun wisent-save-reductions (core itemset)
  "Record the reductions allowed out of state CORE.
ITEMSET is the closure of CORE core items.
Find which rules can be used for reduction transitions from the
current state and make a reductions structure for the state to record
their rule numbers."
  (let ((l itemset)
        rs item p)
    (while l
      (setq item (aref wisent--ritem (car l)))
      (if (< item 0)
          (setq rs (cons (- item) rs)))
      (setq l (cdr l)))
    (setq rs (nreverse rs))
    (if (consp rs)
        (progn
          (setq p (wisent-new-red))
          (wisent-set-red-number p (wisent-core-number core))
          (wisent-set-red-nreds  p (length rs))
          (wisent-set-red-rules  p rs)
          (if wisent--last-reduction
              (progn
                (setcdr wisent--last-reduction (list p))
                (setq wisent--last-reduction
                      (cdr wisent--last-reduction)))
            (setq wisent--first-reduction (list p)
                  wisent--last-reduction wisent--first-reduction))))))

(defun wisent-lalr ()
  "Compute how to make the finite state machine deterministic.
Find which rules need lookahead in each state, and which lookahead
tokens they accept.

`wisent-lalr' builds these data structures:

- `wisent--goto-map', `wisent--from-state' and `wisent--to-state'
  record each shift transition which accepts a variable (a
  nonterminal).  `wisent--ngotos' is the number of such transitions.
  wisent--from_state[t] is the state number which a transition leads
  from and wisent--to-state[t] is the state number it leads to.  All
  the transitions that accept a particular variable are grouped
  together and wisent--goto-map[i - ntokens] is the index in
  wisent--from-state and wisent--to-state of the first of them.

- `wisent--consistent'[s] is non-nil if no lookahead is needed to
  decide what to do in state s.

- `wisent--laruleno' is a vector which records the rules that need
  lookahead in various states.  The elements of wisent--laruleno that apply to
  state s are those from `wisent--lookaheads'[s] through
  wisent--lookaheads[s+1]-1.  Each element of wisent--laruleno is a
  rule number.  If lr is the length of wisent--laruleno, then a number
  from 0 to lr-1 can specify both a rule and a state where the rule
  might be applied.

- `wisent--la' is a lr by ntokens matrix of bits.  wisent--la[l, i] is
  1 if the rule wisent--laruleno[l] is applicable in the appropriate
  state when the next token is symbol i.  If wisent--la[l, i] and
  wisent--la[l, j] are both 1 for i != j, it is a conflict."
  (working-dynamic-status "(lalr)")
  (setq wisent--token-set-size
        (1+ (/ wisent--nterms wisent-bits-per-word)))
  (wisent-set-accessing-symbol)
  (wisent-set-shift-table)
  (wisent-set-reduction-table)
  (wisent-initialize-la)
  (wisent-set-goto-map)
  (wisent-initialize-f)
  (wisent-build-relations)
  (wisent-digraph wisent--includes)
  (wisent-compute-lookaheads))

(defun wisent-set-accessing-symbol ()
  "Initialize `wisent--access-symbol'."
  (setq wisent--access-symbol (make-vector wisent--nstates nil))
  (let ((l wisent--first-state) x)
    (while (consp l)
      (setq x (car l)
            l (cdr l))
      (aset wisent--access-symbol
            (wisent-core-number  x)
            (wisent-core-acc-sym x)))))

(defun wisent-set-shift-table ()
  "Initialize `wisent--shift-table'."
  (setq wisent--shift-table (make-vector wisent--nstates nil))
  (let ((l wisent--first-shift) x)
    (while (consp l)
      (setq x (car l)
            l (cdr l))
      (aset wisent--shift-table (wisent-shift-number x) x))))

(defun wisent-set-reduction-table ()
  "Initialize `wisent--reduction-table'."
  (setq wisent--reduction-table (make-vector wisent--nstates nil))
  (let ((l wisent--first-reduction) x)
    (while (consp l)
      (setq x (car l)
            l (cdr l))
      (aset wisent--reduction-table (wisent-red-number x) x))))

(defun wisent-initialize-la ()
  "Initialize lookaheads."
  (setq wisent--consistent (make-vector wisent--nstates nil)
        wisent--lookaheads (make-vector (1+ wisent--nstates) nil))
  (let ((count 0)
        (i     0)
        j np rp sp)
    (while (< i wisent--nstates)
      (aset wisent--lookaheads i count)
      (setq rp (aref wisent--reduction-table i)
            sp (aref wisent--shift-table i))
      (if (and
           rp
           (or (> (wisent-red-nreds rp) 1)
               (and
                sp
                (not (< (aref wisent--access-symbol
                              (car (last (wisent-shift-shifts sp))))
                        wisent--nvars)))))
          (setq count (+ count (wisent-red-nreds rp)))
        (aset wisent--consistent i t))
      (setq i (1+ i)))
    (aset wisent--lookaheads wisent--nstates count)
    (setq j 0
          count (max count 1)
          wisent--la (make-vector count nil))
    (while (< j count)
      (aset wisent--la j (wisent-new-set wisent--token-set-size))
      (setq j (1+ j)))
    (setq wisent--laruleno (make-vector count -1)
          wisent--lookback (make-vector count nil)
          i  0
          np 0)
    (while (< i wisent--nstates)
      (if (aref wisent--consistent i)
          nil
        (setq rp (aref wisent--reduction-table i))
        (if rp
            (progn
              (setq j (wisent-red-rules rp))
              (while j
                (aset wisent--laruleno np (car j))
                (setq j  (cdr j)
                      np (1+ np))))))
      (setq i (1+ i)))))

(defun wisent-set-goto-map ()
  "Setup `wisent--goto-map'."
  (setq wisent--goto-map (make-vector (1+ wisent--nvars) 0))
  (let ((temp-map (make-vector (1+ wisent--nvars) 0))
        (ng 0)
        (sp wisent--first-shift)
        i k symbol x state1 state2)
    (while (consp sp)
      (setq i (reverse (wisent-shift-shifts (car sp))))
      (while (consp i)
        (setq symbol (aref wisent--access-symbol (car i))
              i (cdr i))
        (if (< symbol wisent--nvars)
            (progn
              (aset wisent--goto-map
                    symbol (1+ (aref wisent--goto-map symbol)))
              (setq ng (1+ ng)))))
      (setq sp (cdr sp)))
    (setq i 0
          k 0)
    (while (< i wisent--nvars)
      (aset temp-map i k)
      (setq k (+ k (aref wisent--goto-map i))
            i (1+ i)))
    (setq i 0)
    (while (< i wisent--nvars)
      (aset wisent--goto-map i (aref temp-map i))
      (setq i (1+ i)))
    (setq wisent--ngotos ng)
    (aset wisent--goto-map wisent--nvars wisent--ngotos)
    (aset temp-map wisent--nvars wisent--ngotos)
    (setq wisent--from-state (make-vector wisent--ngotos nil))
    (setq wisent--to-state (make-vector wisent--ngotos nil))
    (setq sp wisent--first-shift)
    (while sp
      (setq x      (car sp)
            sp     (cdr sp)
            state1 (wisent-shift-number x)
            i      (wisent-shift-shifts x))
      (while i
        (setq state2 (car i)
              symbol (aref wisent--access-symbol state2))
        (if (< symbol wisent--nvars)
            (progn
              (setq k (aref temp-map symbol))
              (aset temp-map symbol (1+ k))
              (aset wisent--from-state k state1)
              (aset wisent--to-state k state2)))
        (setq i (cdr i))))))

(defun wisent-map-goto (state symbol)
  "Map a STATE/SYMBOL pair into its numeric representation."
  (let ((low  (aref wisent--goto-map symbol))
        (high (1- (aref wisent--goto-map (1+ symbol))))
        middle s)
    (while (and (<= low high)
                (not (= state
                        (setq middle (/ (+ low high) 2)
                              s (aref wisent--from-state middle)))))
      (if (< s state)
          (setq low (1+ middle))
        (setq high (1- middle))))
    (or (<= low high)
        (error "Error in map-goto %s %s" state symbol))
    middle))

(defun wisent-initialize-f ()
  "Initialize `wisent--f'."
  (setq wisent--f (make-vector wisent--ngotos nil))
  (let ((reads (make-vector wisent--ngotos nil))
        (i     0)
        (rowp  0)
        rowf stateno sp j edges symbol)
    (while (< i wisent--ngotos)
      (aset wisent--f i (wisent-new-set wisent--token-set-size))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i wisent--ngotos)
      (setq rowf    (aref wisent--f rowp)
            stateno (aref wisent--to-state i)
            sp      (aref wisent--shift-table stateno))
      (if sp
          (progn
            (setq j     (wisent-shift-shifts sp)
                  edges nil)
            (while (consp j)
              (setq symbol (aref wisent--access-symbol (car j)))
              (if (< symbol wisent--nvars)
                  (if (aref wisent--nullable symbol)
                      (setq edges (cons
                                   (wisent-map-goto stateno symbol)
                                   edges)))
                (wisent-set-bit rowf (- symbol wisent--nvars)))
              (setq j (cdr j)))
            (if (consp edges)
                (aset reads i (nreverse edges)))))
      (setq i    (1+ i)
            rowp (1+ rowp)))
    (wisent-digraph reads)))

(defun wisent-add-lookback-edge (stateno ruleno gotono)
  "Add a lookback edge.
STATENO, RULENO and GOTONO are state, rule and goto numbers."
  (let ((k (aref wisent--lookaheads (1+ stateno)))
        (found nil)
        (i (aref wisent--lookaheads stateno)))
    (while (and (not found) (< i k))
      (or (setq found (= (aref wisent--laruleno i) ruleno))
          (setq i (1+ i))))
    (or found
        (error "Error in add-lookback-edge %s %s %s"
               stateno ruleno gotono))
    (aset wisent--lookback i
          (cons gotono (aref wisent--lookback i)))))

(defun wisent-transpose (r n)
  "Transpose vector R of N elements."
  (let ((new-end (make-vector n nil))
        (new-r   (make-vector n nil))
        (i       0)
        sp x y)
    (while (< i n)
      (setq x (list 'dummy))
      (aset new-r   i x)
      (aset new-end i x)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i n)
      (setq sp (aref r i))
      (while (consp sp)
        (setq x  (car sp)
              y  (aref new-end x)
              sp (cdr sp))
        (setcdr y (cons i (cdr y)))
        (aset new-end x (cdr y)))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i n)
      (aset new-r i (cdr (aref new-r i)))
      (setq i (1+ i)))
    new-r))

(defun wisent-symbol-state (stateno symbol)
  "Return, from state STATENO, SYMBOL state."
  (let ((j (wisent-shift-shifts (aref wisent--shift-table stateno)))
        (stno stateno))
    (while j
      (setq stno (car j)
            j    (cdr j))
      (if (= (aref wisent--access-symbol stno) symbol)
          (setq j nil)))
    stno))

(defun wisent-build-relations ()
  "Build relations."
  (setq wisent--includes (make-vector wisent--ngotos nil))
  (let ((i 0)
        state1 symbol1 rulep edges
        *rulep rp stateno states *rp
        done stp rp2 edgp)
    (while (< i wisent--ngotos)
      (setq state1  (aref wisent--from-state i)
            symbol1 (aref wisent--access-symbol
                          (aref wisent--to-state i))
            rulep   (aref wisent--derives symbol1)
            edges   nil)
      (while (consp rulep)
        (setq *rulep (car rulep)
              rp (aref wisent--rrhs *rulep)
              stateno state1
              states (list state1))
        (while (> (setq *rp (aref wisent--ritem rp)) 0)
          (setq stateno (wisent-symbol-state stateno *rp)
                rp      (1+ rp)
                states  (cons stateno states)))
        (or (aref wisent--consistent stateno)
            (wisent-add-lookback-edge stateno *rulep i))
        (setq done nil
              stp (cdr states)
              rp2 (1- rp)
              edgp edges)
        (while (not done)
          (setq *rp (aref wisent--ritem rp2))
          (if (and (< -1 *rp) (< *rp wisent--nvars))
              (setq done (not (aref wisent--nullable *rp))
                    edgp (cons (wisent-map-goto (car stp) *rp) edgp)
                    stp  (cdr stp)
                    rp2  (1- rp2))
            (setq done t)))
        (setq rulep (cdr rulep)
              edges edgp))
      (aset wisent--includes i edges)
      (setq i (1+ i))))
  (setq wisent--includes (wisent-transpose
                          wisent--includes wisent--ngotos)))

(defun wisent-compute-lookaheads ()
  "Compute lookaheads."
  (let ((n (aref wisent--lookaheads wisent--nstates))
        (i 0)
        sp)
    (while (< i n)
      (setq sp (aref wisent--lookback i))
      (while (consp sp)
        (wisent-bit-union (aref wisent--la i)
                          (aref wisent--f (car sp))
                          wisent--token-set-size)
        (setq sp (cdr sp)))
      (setq i (1+ i)))))

(defvar wisent--infinity nil)
(defvar wisent--index    nil)
(defvar wisent--vertices nil)
(defvar wisent--top      nil)
(defvar wisent--r        nil)

(defun wisent-traverse (i)
  "Do a depth-first traversal of a digraph starting at node I."
  (setq wisent--top (1+ wisent--top))
  (aset wisent--vertices wisent--top i)
  (let ((height wisent--top)
        (rp (aref wisent--r i))
        rp2 j break)
    (aset wisent--index i height)
    (if (consp rp)
        (progn
          (setq rp2 rp)
          (while (consp rp2)
            (setq j (car rp2))
            (if (= 0 (aref wisent--index j))
                (wisent-traverse j))
            (if (> (aref wisent--index i) (aref wisent--index j))
                (aset wisent--index i (aref wisent--index j)))
            (wisent-bit-union (aref wisent--f i) (aref wisent--f j)
                              wisent--token-set-size)
            (setq rp2 (cdr rp2)))))
      (if (= (aref wisent--index i) height)
          (while (not break)
            (setq j (aref wisent--vertices wisent--top)
                  wisent--top (1- wisent--top))
            (aset wisent--index j wisent--infinity)
            (or (setq break (= i j))
                (wisent-bit-union (aref wisent--f i)
                                  (aref wisent--f j)
                                   wisent--token-set-size))))))

(defun wisent-digraph (relation)
  "Do a depth-first traversal of all components of digraph RELATION."
  (setq wisent--infinity (+ wisent--ngotos 2)
        wisent--index    (make-vector (1+ wisent--ngotos) 0)
        wisent--vertices (make-vector (1+ wisent--ngotos) 0)
        wisent--top      0
        wisent--r        relation)
  (let ((i 0))
    (while (< i wisent--ngotos)
      (and (= 0 (aref wisent--index i))
           (consp (aref wisent--r i))
           (wisent-traverse i))
      (setq i (1+ i)))))

(defun wisent-add-action (st sym act)
  "Add at state ST on terminal SYM action ACT."
  (let* ((x (aref wisent--action-table st))
         (y (assoc sym x))
         (resol "shift")
         sprec rprec sassoc term ruleno)
    (if y
        (if (not (= act (cdr y)))
            ;; -- there is a conflict
            (if (and (<= (cdr y) 0) (<= act 0))
                ;; R/R conflict
                (progn
                  (wisent-log-rr-conflict
                   st sym (- act) (- (cdr y)) (- (max (cdr y) act)))
                  (setcdr y (max (cdr y) act)))
              ;; S/R conflict
              (setq ruleno (- (cdr y)))
              (if (setq rprec (aref wisent--rprec ruleno))
                  (progn
                    (setq term (elt wisent--terms sym)
                          sprec (get term 'wisent--prec))
                    (cond
                     ;; resolve as reduce
                     ((< sprec rprec)
                      (setq resol "reduce"))
                     ;; resolve as shift
                     ((> sprec rprec)
                      (setcdr y act))
                     ;;	matching precedence levels.
                     (t
                      (setq sassoc (get term 'wisent--assoc))
                      (cond
                       ;; For right association, keep only the shift.
                       ((eq sassoc 'right)
                        (setcdr y act))
                       ;; For left association, keep only the reduction.
                       ((eq sassoc 'left)
                        (setq resol "reduce"))
                       ;; For nonassociation, keep neither.
                       (t
                        (setcdr y 'error)
                        (setq resol "an error")))))))
              (wisent-log-sr-conflict st sym act ruleno resol)))
      (aset wisent--action-table st (cons (cons sym act) x)))))
        
(defun wisent-build-states ()
  "Build states in the action table."
  (working-dynamic-status "(build-states)")
  (setq wisent--action-table       (make-vector wisent--nstates nil)
        wisent--conflicts-by-state (make-vector wisent--nstates nil)
        wisent--conflicts          nil)
  (let ((i 0)
        red k j x y z rule lav token shiftp state symbol)
    (while (< i wisent--nstates)
      (setq red (aref wisent--reduction-table i))
      (if (and red (>= (wisent-red-nreds red) 1))
          (if (and (= (wisent-red-nreds red) 1)
                   (aref wisent--consistent i))
              (wisent-add-action
               i wisent-default-tag (- (car (wisent-red-rules red))))
            (setq k (aref wisent--lookaheads (1+ i))
                  j (aref wisent--lookaheads i))
            (while (< j k)
              (setq rule  (- (aref wisent--laruleno j))
                    lav   (aref wisent--la j)
                    token 0
                    x (aref lav 0)
                    y 1
                    z 0)
              (while (< token wisent--nterms)
                (if (= (mod x 2) 1)
                    (wisent-add-action i token rule))
                (if (= y wisent-bits-per-word)
                    (setq token (1+ token)
                          z     (1+ z)
                          x     (aref lav z)
                          y     1)
                  (setq token (1+ token)
                        x     (/ x 2)
                        y     (1+ y))))
              (setq j (1+ j)))))
      (setq shiftp (aref wisent--shift-table i))
      (if shiftp
          (progn
            (setq k (wisent-shift-shifts shiftp))
            (while (consp k)
              (setq state  (car k)
                    symbol (aref wisent--access-symbol state))
              (if (>= symbol wisent--nvars)
                  (wisent-add-action
                   i (- symbol wisent--nvars) state))
              (setq k (cdr k)))))
      (setq i (1+ i))))
  (wisent-add-action wisent--final-state 0 wisent-accept-tag))

(defun wisent-most-common-action (acts)
  "Return the most common action from the list of actions ACTS."
  (let ((counters nil)
        (max      0)
        act counter)
    (while (consp acts)
      (setq act  (cdar acts)
            acts (cdr acts))
      (and (numberp act)
           (< act 0)
           (if (setq counter (assq act counters))
               (setcdr counter (1+ (cdr counter)))
             (setq counters (cons (cons act 1) counters)))))
    (setq act wisent-error-tag)
    (while counters
      (setq counter  (car counters)
            counters (cdr counters))
      (if (> (cdr counter) max)
          (setq max (cdr counter)
                act (car counter))))
    act))

(defun wisent-compact-action-table ()
  "Compact the action table `wisent--action-table'.
That is group together most common actions in each state."
  (working-dynamic-status "(compact-action-table)")
  (let ((i 0)
        acts act)
    (while (< i wisent--nstates)
      (setq acts (aref wisent--action-table i))
      (if (vectorp (aref wisent--reduction-table i))
          (progn
            (setq act (wisent-most-common-action acts))
            (aset wisent--action-table i
                  (cons (cons wisent-default-tag act)
                        (wisent-filter
                         #'(lambda (x)
                             (not (eq (cdr x) act)))
                         acts))))
        (aset wisent--action-table i
              (cons (cons wisent-default-tag
                          wisent-error-tag) acts)))
      (setq i (1+ i)))))

(defmacro wisent-valid-nonterminal-p (x)
  "Return non-nil if X is a valid nonterminal symbol."
  `(and ,x
        (symbolp ,x)
        (not (char-equal (aref (symbol-name ,x) 0) ?\$))
        (not (memq ,x wisent-reserved-symbols))))

(defmacro wisent-valid-terminal-p (x)
  "Return non-nil if X is a valid terminal symbol."
  `(wisent-valid-nonterminal-p ,x))

(defun wisent-build-action-table ()
  "Build and return the parser action table."
  (working-dynamic-status "(building action-table)")
  wisent--action-table)

(defun wisent-build-goto-table ()
  "Build and return the parser goto table."
  (working-dynamic-status "(building goto-table)")
  (let ((i 0)
        shifts states state symbol gotos table)
    (while (< i wisent--nstates)
      (setq shifts (aref wisent--shift-table i))
      (setq gotos nil)
      (if shifts
          (progn
            (setq states (wisent-shift-shifts shifts))
            (while states
              (setq state  (car states)
                    states (cdr states)
                    symbol (aref wisent--access-symbol state))
              (if (< symbol wisent--nvars)
                  (setq gotos (cons (cons symbol state) gotos))))
            (setq gotos (nreverse gotos))))
      (setq table (cons gotos table))
      (setq i (1+ i)))
    (apply #'vector (nreverse table))))

(defsubst wisent-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst wisent-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

(defun wisent-action-expand (expr &optional phlds)
  "Expand semantic action expression EXPR.
Optional PHLDS is the accumulated list of '$i' placeholders.
Return a cons (PHLDS . EEXPR) where:

- - PHLDS is the updated list of '$i' placeholders found in EXPR.
- - EEXPR is EXPR with `backquote' forms expanded."
  (if (not (listp expr))
      ;; EXPR is an atom, no expansion needed
      (progn
        (and (symbolp expr)
             (string-match "^[$][1-9][0-9]*$" (symbol-name expr))
             ;; Accumulate $i symbol
             (add-to-list 'phlds expr))
        (cons phlds expr))
    ;; EXPR is a list, expand inside it
    (let (eexpr sexpr bltn)
      ;; If backquote expand it first
      (if (wisent-backquote-p (car expr))
          (setq expr (macroexpand expr)))
      (while expr
        (setq sexpr (car expr)
              expr  (cdr expr))
        (cond
         ;; Function call excepted quote expression
         ((and (consp sexpr)
               (not (wisent-quote-p (car sexpr))))
          (setq sexpr (wisent-action-expand sexpr phlds)
                phlds (car sexpr)
                sexpr (cdr sexpr)))
         ;; $i symbol
         ((and (symbolp sexpr)
               (string-match "^[$][1-9][0-9]*$" (symbol-name sexpr)))
          ;; Accumulate $i symbol
          (add-to-list 'phlds sexpr))
         )
        ;; Accumulate expanded forms
        (setq eexpr (nconc eexpr (list sexpr))))
      (cons phlds eexpr))))

(defun wisent-reduce-action (p)
  "Given the reduction rule P return the action function to apply.

An action function receives three arguments:

- - the state/value stack
- - the top-of-stack index
- - the goto table

And returns the updated top-of-stack index."
  (let* ((nt/rh (car p))
         (xa    (wisent-action-expand (cdr p)))
         ($l    (car xa))               ; $<i> found in action
         (expr  (cdr xa))               ; expanded form of action
         (nt    (car nt/rh))            ; reduced nonterminal ID
         (rh    (cdr nt/rh))            ; right hand side nt IDs
         (n     (length rh))            ; number of values in stack
         (vi    n)                      ; $<i> index
         (spi   1)                      ; $<i> value index in stack
         (pl    nil)                    ; list of $<i> positions
         (vbl   nil)                    ; $<i> binding list
         (i     1)
         $i)
    (while (<= i n)
      (setq $i  (intern (format "$%d" vi))
            pl  (cons `(cdr (aref stack (- sp ,spi))) pl))
      (if (memq $i $l) ;; Only bind $<i> if used in action
          (setq vbl (cons `(,$i (car (aref stack (- sp ,spi)))) vbl)))
      (setq i   (1+ i)
            vi  (1- vi)
            spi (+ spi 2)))
    (setq vbl `(,@vbl
                ($region (wisent-region ,@pl))
                ($nterm  ',(nth nt wisent--vars))))
    `(lambda (stack sp gotos)
       (let ,vbl
         ,(if (= nt 0) ;; If `wisent-start-nonterm'
              '(identity $1) ;; Dummy accept action never called
            `(wisent-push stack (- sp ,(1- spi)) ,nt gotos
                          (cons ,expr $region)))))))

(defun wisent-build-reduction-table (gram/acts)
  "Build and return the parser reduction table.
GRAM/ACTS is the list of actions associated to nonterminals."
  (working-dynamic-status "(building reduction-table)")
  (apply #'vector (cons nil (mapcar #'wisent-reduce-action
                                    gram/acts))))

(defun wisent-build-terminal-table ()
  "Build and return the parser terminal table."
  (working-dynamic-status "(building terminal-table)")
  (let ((i 0)
        (l wisent--terms)
        tokens)
    (while (consp l)
      (setq tokens (cons (cons i (car l)) tokens)
            i (1+ i)
            l (cdr l)))
    (nreverse tokens)))

(defun wisent-build-tables (gram gram/acts starts &optional stream)
  "Compute and return the LALR tables needed by the parser.
GRAM is the grammar in internal format.  GRAM/ACTS are grammar rules
in internal format.  STARTS is the table of entry point nonterminals.
If optional STREAM is non-nil it receives a readable representation of
the tables."
  (setq wisent--nterms (length wisent--terms)
        wisent--nvars  (length wisent--vars)
        wisent--nsyms  (+ wisent--nterms wisent--nvars)
        wisent--rules  (apply #'vector
                              (cons nil (mapcar #'car gram/acts)))
        wisent--nrules (length wisent--rules)
        wisent--nitems 0)
  (let ((l gram/acts)
        tables)
    (while l
      (setq wisent--nitems (+ wisent--nitems (length (caar l)))
            l (cdr l)))
    (wisent-pack-grammar gram)
    (wisent-set-derives)
    (wisent-set-nullable)
    (wisent-generate-states)
    (wisent-lalr)
    (wisent-build-states)
    (wisent-compact-action-table)
    (prog1
        (setq tables (vector
                      (wisent-build-action-table)
                      (wisent-build-goto-table)
                      (wisent-build-reduction-table gram/acts)
                      (wisent-build-terminal-table)
                      starts))
      (if wisent--conflicts
          (wisent-log-conflicts))
      (if (or wisent--conflicts wisent-debug-flag)
          (progn
            (wisent-log "\n\nGrammar\n")
            (wisent-log-starts starts)
            (wisent-log-nullables)
            (wisent-log-rules)
            (wisent-log "\n\n")
            (if (wisent-noninteractive)
                (wisent-append-to-log-file))))
      (if stream
          (save-excursion
            (working-dynamic-status
             (format "(pretty-print tables on %S)" stream))
            (pp tables stream))))
    ))

(defvar wisent-nonterm-count nil
  "Hold the number of nonterminals.
Used in `wisent-encode' to assign terminal/nonterminal numbers.")

(defun wisent-encode (x)
  "Return the number associated to the terminal/nonterminal symbol X."
  (let ((pos-in-nt (wisent-pos-in-list x wisent--vars)))
    (if pos-in-nt
        pos-in-nt
      (let ((pos-in-t (wisent-pos-in-list x wisent--terms)))
        (if pos-in-t
            (+ wisent-nonterm-count pos-in-t)
          (error "Undefined symbol %s" x))))))

(defun wisent-default-action (rhs-length)
  "Return a nonterminal default action.
RHS-LENGTH is the number of symbols in the right hand side of the
rule.  The default action returns nil for an empty rule or the value
of the first symbol in the rule that is $1."
  (if (> rhs-length 0)
      '$1
    '()))

(defun wisent-process-nonterminal (def)
  "Check the nonterminal definition DEF.
Return its internal form.  On entry DEF has the form:

  (nonterm . RULES)

where RULES is a list of elements of the form:

  (MATCHINGS [PREC-TERM] [ACTION])

where MATCHINGS is the list of terminals and non terminals to match.
Optional value PREC-TERM is a vector of one element: the terminal
symbol from which the rule gets its precedence level.  And optional
value ACTION is a semantic action statement."
  (setq wisent-nonterm-count (length wisent--vars))
  (let ((nonterm (car def))
        (rules   (cdr def))
        prod/acts rule rhs rest prod item items)
    (or (consp rules)
        (error "At least one production needed for nonterminal %s"
               nonterm))
    (while rules
      (setq rule  (car rules)
            rules (cdr rules)
            rhs   (car rule)
            rest  (cdr rule)
            prod  (mapcar #'wisent-encode (cons nonterm rhs))
            items rhs)
      (while items
        (setq item  (car items)
              items (cdr items))
        (or (memq item wisent--terms)
            (memq item wisent--vars)
            (error "Invalid terminal or nonterminal %s" item)))
      
      ;; Rule has precedence level
      (if (vectorp (car rest))
          (progn
            (setq item (car rest))
            (or (and (= (length item) 1) (memq (aref item 0) wisent--terms))
                (error "Invalid rule precedence level %S" item))
            (setq wisent--rprec (cons (get (aref item 0) 'wisent--prec)
                                      wisent--rprec)
                  rest (cdr rest)))
        ;; No precedence level
        (setq wisent--rprec (cons nil wisent--rprec)))

      ;; Rule has semantic action
      (if rest
          (progn
            (or (null (cdr rest))
                (error "Invalid rule semantic action %S" rest))
            (setq prod/acts (cons (cons prod (car rest)) prod/acts)))
        (setq prod/acts (cons
                         (cons prod
                               (wisent-default-action (length rhs)))
                         prod/acts))))
    (nreverse prod/acts)))

(defsubst wisent-grammar-production-lhs (def)
  "Return the left hand side nonterminal number from DEF.
DEF is the internal representation of a nonterminal definition."
  (car (caar def)))

(defsubst wisent-grammar-rule-rhs (r)
  "Return the right hand side list of item numbers from rule R.
R is in internal format."
  (cdar r))

(defsubst wisent-grammar-production (def)
  "Return internal form of production for DEF.
DEF is the internal representation of a nonterminal definition."
  (cons (wisent-grammar-production-lhs def)
        (mapcar #'wisent-grammar-rule-rhs def)))

(defun wisent-process-grammar (grammar &optional starts stream)
  "Process the given external representation of GRAMMAR.
That check and convert external representation to internal format.
Then compute and generate the LALR tables needed by the parser.
Optional argument STARTS is a list of entry point nonterminals.  If
optional STREAM is non-nil it receives a readable representation of
the tables.  GRAMMAR is a list of form: (TERMS ASSOCS . NONTERMS)
where TERMS is a list of terminal symbols, ASSOCS is a list describing
the associativity of terminals or nil.  And NONTERMS is the list of
non terminal definitions (see function `wisent-process-nonterminal').
ASSOCS is a list of elements: (ASSOC-TYPE . ASSOC-TERMS) where
ASSOC-TYPE is one of 'nonassoc, 'left or 'right, and ASSOC-TERMS is a
list of terminal symbols which must have been declared in TERMS."
  ;; Check and convert grammar to a suitable internal representation
  (or (and (consp grammar) (> (length grammar) 2))
      (error "Invalid grammar definition"))
  
  (let ((lastprec 0)
        lst term terms var def defs r-vars r-gram gram/acts
        a-type a-terms ep-var ep-term ep-def)

    ;; terminals
    (setq lst (car grammar))
    (while lst
      (setq term (car lst)
            lst  (cdr lst))
      (or (wisent-valid-terminal-p term)
          (error "Invalid terminal %s" term))
      (if (memq term terms)
          (error "Terminal previously defined %s" term))
      ;; Cleanup precedence and associativity
      (put term 'wisent--prec  nil)
      (put term 'wisent--assoc nil)
      (setq terms (cons term terms)))

    ;; assocativity
    (setq lst (nth 1 grammar))
    (while lst
      (setq def      (car lst)
            a-type   (car def)
            a-terms  (cdr def)
            lst      (cdr lst)
            lastprec (1+ lastprec))
      (or (memq a-type '(left right nonassoc))
          (error "Invalid associativity type %s" a-type))
      (while a-terms
        (setq term     (car a-terms)
              a-terms  (cdr a-terms))
        (or (memq term terms)
            (error "Invalid associativity, terminal %s undefined" term))
        (if (memq term defs)
            (error "Duplicate associativity for terminal %s" term))
        (setq defs (cons term defs))
        ;; Record the precedence and associativity of the terminal in
        ;; respectively the `wisent--prec' and `wisent--assoc' symbol
        ;; properties.
        (put term 'wisent--prec  lastprec)
        (put term 'wisent--assoc a-type)))
    
    ;; nonterminals
    (setq lst  (nthcdr 2 grammar)
          defs nil)
    (while lst
      (setq def (car lst)
            lst (cdr lst))
      (or (consp def)
          (error "Nonterminal definition must be a non-empty list"))
      (setq var (car def))
      (or (wisent-valid-nonterminal-p var)
          (error "Invalid nonterminal %s" var))
      (if (or (memq var terms) (assq var defs))
          (error "Nonterminal previously defined %s" var))
      (setq defs (cons def defs)))
    (if (= (length defs) 0)
        (error "Grammar must contain at least one nonterminal"))
    
    ;; Check entry point nonterminals
    (setq defs   (nreverse defs)
          lst    (nreverse (cons (caar defs) starts))
          starts nil)
    (while lst
      (setq var (car lst)
            lst (cdr lst))
      (or (assq var defs)
          (error "Entry point nonterminal %s not found" var))
      (or (assq var starts) ;; Ignore duplicates
          ;; For each <nonterm> entry point:
          (setq ep-var  (make-symbol (format "$%s"  var)) ; nonterm
                ep-term (make-symbol (format "$$%s" var)) ; terminal
                terms   (cons ep-term terms)
                ;; Add entry (<nonterm> . $$<nonterm>) to start table
                starts  (cons (cons var ep-term) starts)
                ;; Add prod. ($<nonterm> (($$<nonterm> nonterm) $2))
                defs    (cons (list ep-var
                                    (list (list ep-term var) '$2))
                              defs)
                ;; Add start rule (($<nonterm>) $1)
                ep-def  (cons (list (list ep-var) '$1) ep-def))))
    ;; Build grammar internal representation
    ;;
    ;; First push start productions in the grammar definition, that
    ;; is, for start nonterminals ($<nonterm-1> ... $<nonterm-N>):
    ;;
    ;; ($START       (($STARTS $EOI) $1))
    ;; ($STARTS      (($<nonterm-1>) $1)
    ;;                ...
    ;;               (($<nonterm-N>) $1))
    ;; ($<nonterm-1> (($$<nonterm-1> <nonterm-1>) $2))
    ;; ...
    ;; ($<nonterm-N> (($$<nonterm-N> <nonterm-N>) $2))
    ;; 
    (setq defs (cons (list wisent-start-nonterm
                           (list
                            (list wisent-starts-nonterm wisent-eoi-term)
                            '$1))
                     (cons (cons wisent-starts-nonterm ep-def) defs))
          wisent--terms (cons
                         wisent-eoi-term
                         (nreverse (cons wisent-error-term terms)))
          wisent--vars  (mapcar #'car defs)
          wisent--rprec nil
          r-vars        (mapcar #'wisent-process-nonterminal defs)
          wisent--rprec (apply #'vector
                               (cons nil (nreverse wisent--rprec)))
          r-gram        (mapcar #'wisent-grammar-production r-vars)
          gram/acts     (apply #'nconc r-vars))
    ;; Build the LALR tables
    (wisent-build-tables r-gram gram/acts starts stream)))

(defun wisent-byte-compile-reduction-table (rt)
  "Byte compile the reduction table RT.
That is `byte-compile' action functions built by
`wisent-reduce-action'.  This significantly improves the performance
of the parser!"
  (let ((i 1)
        (n (length rt)))
    (while (< i n)
      (aset rt i (byte-compile (aref rt i)))
      (setq i (1+ i)))))

(defalias 'wisent-compiled-grammar-p 'vectorp)

(defun wisent-compile-grammar (gram &optional starts stream)
  "Compile grammar GRAM and return the tables needed by the parser.
Optional argument STARTS is a list of nonterminal symbols defined as
entry point in the grammar.  If nil the first nonterminal specified in
the grammar will be the only start symbol defined.  Optional STREAM is
a `standard-output' stream where the tables will be pretty-printed.
The value returned by `wisent-compile-grammar' is a vector of vectors
and listes: [ACTIONS GOTOS REDUCTIONS TERMS STARTS] where:

- - ACTIONS/GOTOS tell the parser what action to do at each state.
- - REDUCTIONS holds the byte code of semantic actions.
- - TERMS associates terminal symbols to their internal ID.
- - STARTS gives the defined start nonterminals."
  (if (wisent-compiled-grammar-p gram)
      gram ;; Grammar already compiled just return it
    (let ((working-status-dynamic-type #'working-text-display))
      (working-status-forms "Compiling grammar" "done"
        (working-dynamic-status "(initializing)")
        (wisent-initialize-all)
        (let ((tables (wisent-process-grammar gram starts stream)))
          (if wisent-debug-flag
              nil ;; disable compilation when debugging
            ;; Compile the reduction table
            (working-dynamic-status "(byte-compiling reduction table)")
            (wisent-byte-compile-reduction-table (aref tables 2)))
          ;; Cleanup storage!
          (wisent-free-all)
          (working-dynamic-status t)
          tables)))))

;;;;
;;;; The LR parser driver
;;;;

(defvar wisent-nerrs nil
  "Hold the number of parse errors.")

(defvar wisent-lookahead nil
  "Hold the lookahead lexical token.
This value is non-nil if the parser terminated because of an
unrecoverable error.")

;;;; Variables and macros that are useful in grammar actions.

(defvar wisent-parse-lexer-function nil
  "The user supplied lexer function.
This function don't have arguments.  The actual value of this variable
is local to the parser.")

(defvar wisent-input nil
  "The last token read.
The actual value of this variable is local to the parser.")

(defmacro wisent-lexer ()
  "Obtain the next terminal in input."
  '(funcall wisent-parse-lexer-function))

(defvar wisent-recovering nil
  "Non nil when the parser is recovering.
The actual value of this variable is local to the parser.")

(defvar wisent-parse-error-function nil
  "The user supplied error function.
This function must accept one argument, a message string.  The actual
value of this variable is local to the parser.")

(defmacro wisent-error (msg)
  "Call the user supplied error reporting function with messsage MSG."
  `(funcall wisent-parse-error-function ,msg))
  
(defmacro wisent-errok ()
  "Cause parser to recover immediately to its normal mode."
  '(setq wisent-recovering nil))
  
(defmacro wisent-clearin ()
  "Cause a token to be read."
  '(setq wisent-input nil))

(defvar $region nil
  "Unused global definition to avoid compiler warnings.
The actual value of this variable is local to each semantic action.")

(defvar $nterm nil
  "Unused global definition to avoid compiler warnings.
The actual value of this variable is local to each semantic action.")

(defmacro wisent-set-region (start end)
  "Change the region of text matched by the current nonterminal.
START and END are respectively the beginning and end positions of the
region.  If START or END values are not a valid positions the region
is set to nil."
  `(setq $region (and (number-or-marker-p ,start)
                      (number-or-marker-p ,end)
                      (cons ,start ,end))))

(defsubst wisent-skip-token ()
  "Skip an invalid token and try to continue parsing.
To be used in grammar recovery actions."
  (wisent-error (format "Skipping invalid '%s'" $nterm))
  ;; Clear the lookahead token
  (if (eq (car wisent-input) wisent-eoi-term)
      ;; does nothing at EOI to avoid infinite recovery loop
      nil
    (run-hook-with-args 'wisent-skip-token-hook wisent-input)
    (wisent-clearin)
    (wisent-errok)))

(defun wisent-skip-block ()
  "Safely skip a parenthesized block and try to continue parsing.
To be used in grammar recovery actions."
  (let ((start (car $region))
        end input block)
    (if (not (number-or-marker-p start))
        ;; No nonterminal region available, skip
        ;; token and try to continue?
        (wisent-skip-token)

      ;; Try to skip a block
      (if (not (setq end (save-excursion
                           (goto-char start)
                           (and (looking-at "\\s(")
                                (condition-case nil
                                    (1- (scan-lists (point) 1 0))
                                  (error nil))))))
          ;; Not actually a block, skip token and try to continue?
          (wisent-skip-token)
        
        ;; Ok to safely skip the block
        (wisent-error (format "Skipping invalid '%s' from %s to %s"
                              $nterm start end))
        ;; read input until matching close paren or EOI
        (setq input wisent-input)
        (while (and (not (eq (car input) wisent-eoi-term))
                    (< (nth 2 input) end))
          (run-hook-with-args 'wisent-skip-token-hook input)
          (setq input (wisent-lexer)))
        ;; Clear the lookahead token
        (if (eq (car wisent-input) wisent-eoi-term)
            ;; does nothing at EOI to avoid infinite recovery loop
            nil
          (wisent-clearin)
          (wisent-errok))
        ;; Return a nil value with adjusted start/end positions
        (cons nil (wisent-set-region start (1+ end)))))))

;;;; Other parser stuff

(defsubst wisent-region (&rest positions)
  "Return the start/end positions of the region including POSITIONS.
Each element of POSITIONS is a pair (START-POS . END-POS) or nil.  The
returned value is the pair (MIN-START-POS . MAX-END-POS) or nil if no
POSITIONS are available."
  (let ((pl (delq nil positions)))
    (if pl
        (cons (apply #'min (mapcar #'car pl))
              (apply #'max (mapcar #'cdr pl))))))

(defsubst wisent-push (stack sp nt goto-table value)
  "Push a nonterminal reduced value and next state on parser STACK.
SP is the top of stack index.  NT is the reduced nonterminal ID.
GOTO-TABLE is the parser goto table.  VALUE is the nonterminal reduced
value.  Return the new top of stack index."
  (let ((state (aref stack sp)))
    (setq sp (+ sp 2))
    (aset stack sp (cdr (assq nt (aref goto-table state))))
    (aset stack (1- sp) value)
    sp))

(defmacro wisent-translate (term terminals)
  "Return the terminal number from the terminal symbol TERM.
TERMINALS is the table of terminals."
  `(car (rassq ,term ,terminals)))

(defmacro wisent-untranslate (term-id terminals)
  "Return the terminal symbol from the terminal number TERM-ID.
TERMINALS is the table of terminals."
  `(cdr (assq ,term-id ,terminals)))

(defmacro wisent-parse-action (x l)
  "Return the next parser action.
X is a terminal ID (number) and L is the alist of (term-id . action)
available at current state."
  `(cdr (or (assq ,x ,l) (car ,l))))

(defsubst wisent-parse-start (start starts)
  "Return the first lexical token to parse.
START is the entry point nonterminal.  STARTS the table of defined
entry points."
  (let ((term (if start
                  (cdr (assq start starts))
                (cdar starts))))
    (if term
        (list term (symbol-name term))
      (error "Invalid start nonterminal %s" start))))

(defun wisent-parse (tables lexer error &optional start)
  "Parse data.
The LALR(1) parser is driven by the TABLES built by
`wisent-compile-grammar'.  LEXER is a no argument function called by
the parser to obtain the next input token.  ERROR is an error
reporting function called when a parse error occurs.  This function
receives a message string to report.  START specify the nonterminal
symbol used by the parser as its goal.  It defaults to the first
nonterminal specified in the grammar.  START must be one the STARTS
symbol specified at compilation time (see `wisent-compile-grammar')."
  (let* ((actions    (aref tables 0))
         (gotos      (aref tables 1))
         (reductions (aref tables 2))
         (terminals  (aref tables 3))
         (starts     (aref tables 4))
         (error-term (wisent-translate wisent-error-term terminals))
         (stack      (make-vector wisent-parse-max-stack-size nil))
         (sp     0)
         (action t)
         (wisent-parse-error-function error)
         (wisent-parse-lexer-function lexer)
         (wisent-recovering nil)
         (wisent-input (wisent-parse-start start starts))
         state tokid choices choice)
    (setq wisent-nerrs     0 ;; Reset parse error counter
          wisent-lookahead nil) ;; and lookahead token
    (aset stack 0 0) ;; Initial state
    (while action
      (setq state  (aref stack sp)
            tokid  (wisent-translate (car wisent-input) terminals)
            action (wisent-parse-action tokid (aref actions state)))
      (cond
       
       ;; Input succesfully parsed
       ;; ------------------------
       ((eq action wisent-accept-tag)
        (setq action nil))
       
       ;; Syntax error in input
       ;; ---------------------
       ((eq action wisent-error-tag)
        ;; Report this error if not already recovering from an error.
        (or wisent-recovering
            (wisent-error
             (format "Parse error - unexpected token %s(%S)%s"
                     (car wisent-input)
                     (cadr wisent-input)
                     (if (cddr wisent-input)
                         (format " at %s" (cddr wisent-input))
                       ""))))
        ;; Increment the error counter
        (setq wisent-nerrs (1+ wisent-nerrs))
        ;; If just tried and failed to reuse lookahead token after an
        ;; error, discard it.
        (if (eq wisent-recovering wisent-parse-max-recover)
            (if (eq (car wisent-input) wisent-eoi-term)
                (setq action nil) ;; Terminate if at end of input.
              (run-hook-with-args 'wisent-skip-token-hook wisent-input)
              (setq wisent-input (wisent-lexer)))

          ;; Else will try to reuse lookahead token after shifting the
          ;; error token.
          
          ;; Each real token shifted decrements this.
          (setq wisent-recovering wisent-parse-max-recover)
          ;; Pop the value/state stack to see if an action associated
          ;; to special terminal symbol 'error exists.
          (setq choices (aref actions state))
          (while (and (>= sp 0)
                      (not (and (setq state   (aref stack sp)
                                      choices (aref actions state)
                                      choice  (assq error-term choices))
                                (numberp (cdr choice))
                                (>= (cdr choice) 0))))
            (setq sp (- sp 2)))

          (if (not choice)
              ;; No 'error terminal was found.  Just terminate.
              (setq wisent-lookahead wisent-input
                    action nil)
            
            ;; Try to recover and continue parsing.
            ;; Shift the error terminal.
            (setq state (cdr choice)    ; new state
                  sp    (+ sp 2))
            (aset stack (1- sp) nil)    ; push value
            (aset stack sp state)       ; push new state
            ;; Try to adjust input to error recovery state.
            (setq choices (aref actions state))
            ;; If 'error is followed by an action continue parsing
            ;; (that is do the action).  Otherwise read until the
            ;; input token matches the terminal symbol following
            ;; 'error or EOI is reached.
            (if (cdr choices)
                (while
                    (progn
                      (or wisent-input
                          (setq wisent-input (wisent-lexer)
                                choice (car wisent-input)))
                      (not (or (eq wisent-eoi-term choice)
                               (assq (wisent-translate
                                      choice terminals) choices))))
                  (run-hook-with-args 'wisent-skip-token-hook wisent-input)
                  (setq wisent-input nil))))))
        
       ;; Shift current token on top of the stack
       ;; ---------------------------------------
       ((>= action 0)
        ;; Count tokens shifted since error; after
        ;; `wisent-parse-max-recover', turn off error status.
        (setq wisent-recovering (and (natnump wisent-recovering)
                                     (> wisent-recovering 1)
                                     (1- wisent-recovering)))
        (setq sp (+ sp 2))
        (aset stack (1- sp) (cdr wisent-input))
        (aset stack sp action)
        (setq wisent-input (wisent-lexer)))
       
       ;; Reduce by rule (- action)
       ;; -------------------------
       (t
        (setq sp (funcall (aref reductions (- action)) stack sp gotos))
        (or wisent-input (setq wisent-input (wisent-lexer))))))
    (car (aref stack 1))))

(provide 'wisent)

;;; wisent.el ends here
