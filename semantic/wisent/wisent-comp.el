;;; wisent-comp.el --- GNU Bison for Emacs - Parser generator

;; Copyright (C) 2002 David Ponce
;; Copyright 1984, 1986, 1989, 1992, 1995, 2000, 2001
;; Free Software Foundation, Inc. (Bison)

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Janvier 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-comp.el,v 1.2 2002/02/02 00:21:49 ponced Exp $

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
;; Wisent (the European Bison ;-) is an Elisp implementation of the
;; GNU Compiler Compiler Bison.  The Elisp code is a port of the C
;; code of GNU Bison 1.28 & 1.31.
;;
;; For more details on the basic concepts for understanding Wisent,
;; read the Bison manual ;)
;;
;; For more details on Wisent itself read the Wisent manual.

;;; History:
;; 

;;; Code:
(require 'wisent)
(require 'working)


;;;; -------------------
;;;; Misc. useful things
;;;; -------------------

;; As much as possible I would like to keep the name of global
;; variables used in Bison without polluting too much the Elisp global
;; name space.  Elisp dynamic binding allows that ;-)

;; Here are simple macros to easily define and use set of variables
;; binded locally, without all these "reference to free variable"
;; compiler warnings!

(defmacro wisent-context-name (name)
  "Return the context name from NAME."
  `(if (and ,name (symbolp ,name))
       (intern (format "wisent-context-%s" ,name))
     (error "Invalid context name %S" ,name)))
  
(defmacro wisent-context-bindings (name)
  "Return the variables in context NAME."
  `(symbol-value (wisent-context-name ,name)))
    
(defmacro wisent-defcontext (name &rest vars)
  "Define a context NAME that will bind variables VARS."
  (let* ((context (wisent-context-name name))
         (bindings (mapcar #'(lambda (v) (list 'defvar v)) vars)))
    `(eval-when-compile
       ,@bindings
       (defvar ,context',vars))))
(put 'wisent-defcontext 'lisp-indent-function 1)

(defmacro wisent-with-context (name &rest body)
  "Bind variables in context NAME then eval BODY."
  `(let* ,(wisent-context-bindings name)
     ,@body))
(put 'wisent-with-context 'lisp-indent-function 1)

;; A naive implementation of data structures!  But it suffice here ;-)

(defmacro wisent-struct (name &rest fields)
  "Defines a simple data structure called NAME.
Wich contains data stored in FIELDS.  FIELDS is a list of symbols
which are field names or pairs (FIELD INITIAL-VALUE) where
INITIAL-VALUE is a constant used as the initial value of FIELD when
the data structure is created.  INITIAL-VALUE defaults to nil.

This defines a `make-NAME' constructor, get-able `NAME-FIELD' and
set-able `set-NAME-FIELD' accessors."
  (let ((size (length fields))
        (i    0)
        accors field sufx fun ivals)
    (while (< i size)
      (setq field  (car fields)
            fields (cdr fields))
      (if (consp field)
          (setq ivals (cons (cadr field) ivals)
                field (car field))
        (setq ivals (cons nil ivals)))
      (setq sufx   (format "%s-%s" name field)
            fun    (intern (format "%s" sufx))
            accors (cons `(defmacro ,fun (s)
                            (list 'aref s ,i))
                         accors)
            fun    (intern (format "set-%s" sufx))
            accors (cons `(defmacro ,fun (s v)
                            (list 'aset s ,i v))
                         accors)
            i      (1+ i)))
    `(progn
      (defmacro ,(intern (format "make-%s" name)) ()
        (cons 'vector ',(nreverse ivals)))
      ,@accors)))
(put 'wisent-struct 'lisp-indent-function 1)

;; Other utilities

(defsubst wisent-pad-string (s n &optional left)
  "Fill string S with spaces.
Return a new string of at least N characters.  Insert spaces on right.
If optional LEFT is non-nil insert spaces on left."
  (let ((i (length s)))
    (if (< i n)
        (if left
            (concat (make-string (- n i) ?\ ) s)
          (concat s (make-string (- n i) ?\ )))
      s)))

;;;; ------------------------
;;;; Environment dependencies
;;;; ------------------------

(defconst wisent-BITS-PER-WORD
  (let ((i 1))
    (while (not (zerop (lsh 1 i)))
      (setq i (1+ i)))
    i))

(defsubst wisent-WORDSIZE (n)
  "(N + BITS-PER-WORD - 1) / BITS-PER-WORD."
  (/ (1- (+ n wisent-BITS-PER-WORD)) wisent-BITS-PER-WORD))

(defsubst wisent-SETBIT (x i)
  "X[I/BITS-PER-WORD] |= 1 << (I % BITS-PER-WORD)."
  (let ((k (/ i wisent-BITS-PER-WORD)))
    (aset x k (logior (aref x k)
                      (lsh 1 (% i wisent-BITS-PER-WORD))))))

(defsubst wisent-RESETBIT (x i)
  "X[I/BITS-PER-WORD] &= ~(1 << (I % BITS-PER-WORD))."
  (let ((k (/ i wisent-BITS-PER-WORD)))
    (aset x k (logand (aref x k)
                      (lognot (lsh 1 (% i wisent-BITS-PER-WORD)))))))

(defsubst wisent-BITISSET (x i)
  "(X[I/BITS-PER-WORD] & (1 << (I % BITS-PER-WORD))) != 0."
  (not (zerop (logand (aref x (/ i wisent-BITS-PER-WORD))
                      (lsh 1 (% i wisent-BITS-PER-WORD))))))

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

(defvar wisent-debug-flag nil
  "Non-nil means enable some debug stuff.")

;;;; --------------
;;;; Logging/Output
;;;; --------------
(defconst wisent-log-buffer-name "*wisent-log*"
  "Name of the log buffer.")

(defvar wisent-new-log-flag nil
  "Non-nil means to start a new report.")

(defvar wisent-verbose-flag nil
  "*Non-nil means to report verbose information on generated parser.")

(defmacro wisent-log-buffer ()
  "Return the log buffer.
Its name is defined in constant `wisent-log-buffer-name'."
  `(get-buffer-create wisent-log-buffer-name))

(defmacro wisent-clear-log ()
  "Delete the entire contents of the log buffer."
  `(with-current-buffer (wisent-log-buffer)
     (erase-buffer)))

(eval-when-compile (defvar byte-compile-current-file))

(defun wisent-source ()
  "Return the current source file name or nil."
  (let ((source (or (and (boundp 'byte-compile-current-file)
                         byte-compile-current-file)
                    load-file-name (buffer-file-name))))
    (if source
        (file-relative-name source))))

(defun wisent-new-log ()
  "Start a new entry into the log buffer."
  (setq wisent-new-log-flag nil)
  (let ((text (format "\n\n*** Wisent %s - %s\n\n"
                      (or (wisent-source) (buffer-name))
                      (format-time-string "%Y-%m-%d %R"))))
    (with-current-buffer (wisent-log-buffer)
      (goto-char (point-max))
      (insert text))))

(defsubst wisent-log (&rest args)
  "Insert text into the log buffer.
`format' is applied to ARGS and the result string is inserted into the
log buffer returned by the function `wisent-log-buffer'."
  (and wisent-new-log-flag (wisent-new-log))
  (with-current-buffer (wisent-log-buffer)
    (insert (apply #'format args))))

(defconst wisent-log-file "wisent.output"
  "The log file.
Used when running without interactive terminal.")

(defun wisent-append-to-log-file ()
  "Append contents of loggin buffer to `wisent-log-file'."
  (if (get-buffer wisent-log-buffer-name)
      (condition-case err
          (with-current-buffer (wisent-log-buffer)
            (widen)
            (write-region (point-min) (point-max) wisent-log-file t))
        (error
         (message "%s" (error-message-string err))))))

;;;; -----------------------------------
;;;; Representation of the grammar rules
;;;; -----------------------------------

;; ntokens is the number of tokens, and nvars is the number of
;; variables (nonterminals).  nsyms is the total number, ntokens +
;; nvars.

;; Each symbol (either token or variable) receives a symbol number.
;; Numbers 0 to ntokens-1 are for tokens, and ntokens to nsyms-1 are
;; for variables.  Symbol number zero is the end-of-input token.  This
;; token is counted in ntokens.

;; The rules receive rule numbers 1 to nrules in the order they are
;; written.  Actions and guards are accessed via the rule number.

;; The rules themselves are described by three arrays: rrhs, rlhs and
;; ritem.  rlhs[R] is the symbol number of the left hand side of rule
;; R.  The right hand side is stored as symbol numbers in a portion of
;; ritem.  rrhs[R] contains the index in ritem of the beginning of the
;; portion for rule R.

;; The length of the portion is one greater than the number of symbols
;; in the rule's right hand side.  The last element in the portion
;; contains minus R, which identifies it as the end of a portion and
;; says which rule it is for.

;; The portions of ritem come in order of increasing rule number and
;; are followed by an element which is nil to mark the end.  nitems is
;; the total length of ritem, not counting the final nil.  Each
;; element of ritem is called an "item" and its index in ritem is an
;; item number.

;; Item numbers are used in the finite state machine to represent
;; places that parsing can get to.

;; The precedence level of each rule is recorded in the vector rprec.
;; The precedence level and associativity of each symbol is recorded
;; in respectively the properties 'wisent--prec and 'wisent--assoc.

;; Precedence levels are assigned in increasing order starting with 1
;; so that numerically higher precedence values mean tighter binding
;; as they ought to.  nil as a symbol or rule's precedence means none
;; is assigned.

(defcustom wisent-state-table-size 1009
  "The size of the state table."
  :type 'integer
  :group 'wisent)

;; These variables only exist locally in the function
;; `wisent-compile-grammar' and are shared by all other nested
;; callees.
(wisent-defcontext compile-grammar
  F LA LAruleno accessing-symbol any-conflicts conflicts consistent
  derives err-table fderives final-state first-reduction first-shift
  first-state firsts from-state goto-map includes itemset nitemset
  kernel-base kernel-end kernel-items last-reduction last-shift
  last-state lookaheads lookaheadset lookback maxrhs ngotos nitems
  nrules nshifts nstates nsyms ntokens nullable nvars rassoc redset
  reduction-table ritem rlhs rprec rprecsym rrc-count rrc-total rrhs
  rule-table ruleset rulesetsize shift-symbol shift-table shiftset
  src-count src-total start-table state-table tags this-state to-state
  tokensetsize ;; nb of words req. to hold a bit for each rule
  varsetsize ;; nb of words req. to hold a bit for each variable
  error-token-number start-symbol token-list var-list
  )

(defmacro wisent-ISTOKEN (s)
  "Return non-nil if item number S defines a token (terminal).
That is if S < `ntokens'."
  `(< ,s ntokens))

(defmacro wisent-ISVAR(s)
  "Return non-nil if item number S defines a nonterminal.
That is if S >= `ntokens'."
  `(>= ,s ntokens))

;;;; ----------------------------------------------------------
;;;; Type definitions for nondeterministic finite state machine
;;;; ----------------------------------------------------------

;; These type definitions are used to represent a nondeterministic
;; finite state machine that parses the specified grammar.  This
;; information is generated by the function `wisent-generate-states'.

;; Each state of the machine is described by a set of items --
;; particular positions in particular rules -- that are the possible
;; places where parsing could continue when the machine is in this
;; state.  These symbols at these items are the allowable inputs that
;; can follow now.

;; A core represents one state.  States are numbered in the number
;; field.  When `wisent-generate-states' is finished, the starting
;; state is state 0 and `nstates' is the number of states.  (A
;; transition to a state whose state number is `nstates' indicates
;; termination.)  All the cores are chained together and `first-state'
;; points to the first one (state 0).

;; For each state there is a particular symbol which must have been
;; the last thing accepted to reach that state.  It is the
;; accessing-symbol of the core.

;; Each core contains a vector of `nitems' items which are the indices
;; in the `ritems' vector of the items that are selected in this
;; state.

;; The link field is used for chaining buckets that hash states by
;; their itemsets.  This is for recognizing equivalent states and
;; combining them when the states are generated.

;; The two types of transitions are shifts (push the lookahead token
;; and read another) and reductions (combine the last n things on the
;; stack via a rule, replace them with the symbol that the rule
;; derives, and leave the lookahead token alone).  When the states are
;; generated, these transitions are represented in two other lists.

;; Each shifts structure describes the possible shift transitions out
;; of one state, the state whose number is in the number field.  The
;; shifts structures are linked through next and first-shift points to
;; them.  Each contains a vector of numbers of the states that shift
;; transitions can go to.  The accessing-symbol fields of those
;; states' cores say what kind of input leads to them.

;; A shift to state zero should be ignored.  Conflict resolution
;; deletes shifts by changing them to zero.

;; Each reductions structure describes the possible reductions at the
;; state whose number is in the number field.  The data is a list of
;; nreds rules, represented by their rule numbers.  `first-reduction'
;; points to the list of these structures.

;; Conflict resolution can decide that certain tokens in certain
;; states should explicitly be errors (for implementing %nonassoc).
;; For each state, the tokens that are errors for this reason are
;; recorded in an errs structure, which has the state number in its
;; number field.  The rest of the errs structure is full of token
;; numbers.

;; There is at least one shift transition present in state zero.  It
;; leads to a next-to-final state whose accessing-symbol is the
;; grammar's start symbol.  The next-to-final state has one shift to
;; the final state, whose accessing-symbol is zero (end of input).
;; The final state has one shift, which goes to the termination state
;; (whose number is `nstates'-1).
;; The reason for the extra state at the end is to placate the
;; parser's strategy of making all decisions one token ahead of its
;; actions.

(wisent-struct core
  next                                  ; -> core
  link                                  ; -> core
  (number 0)
  (accessing-symbol 0)
  (nitems 0)
  (items [0]))

(wisent-struct shifts
  next                                  ; -> shifts
  (number 0)
  (nshifts 0)
  (shifts [0]))

(wisent-struct reductions
  next                                  ; -> reductions
  (number 0)
  (nreds 0)
  (rules [0]))

(wisent-struct errs
  (nerrs 0)
  (errs [0]))

;;;; -----------------------------
;;;; Match rules with nonterminals
;;;; -----------------------------

(defun wisent-set-derives ()
  "Find, for each variable (nonterminal), which rules can derive it.
It sets up the value of DERIVES so that DERIVES[i - NTOKENS] points to
a list of rule numbers, terminated with -1."
  (let (i lhs p q dset delts)
    (setq dset (make-vector nvars nil)
          delts (make-vector (1+ nrules) 0))
    (setq p 0 ;; p = delts
          i nrules)
    (while (> i 0)
      (setq lhs (aref rlhs i))
      (when (>= lhs 0)
        ;; p->next = dset[lhs];
        ;; p->value = i;
        (aset delts p (cons i (aref dset (- lhs ntokens)))) ;; (value . next)
        (aset dset (- lhs ntokens) p) ;; dset[lhs] = p
        (setq p (1+ p)) ;; p++
        )
      (setq i (1- i)))
    
    (setq derives (make-vector nvars nil)
          i       ntokens)
    
    (while (< i nsyms)
      (setq q nil
            p (aref dset (- i ntokens))) ;; p = dset[i]
            
      (while p
        (setq p (aref delts p)
              q (cons (car p) q) ;;q++ = p->value
              p (cdr p))) ;; p = p->next
      (setq q (nreverse (cons -1 q))) ;; *q++ = -1
      (aset derives (- i ntokens) q) ;; derives[i] = q
      (setq i (1+ i)))
    ))

;;;; --------------------------------------------------------
;;;; Find which nonterminals can expand into the null string.
;;;; --------------------------------------------------------

(defun wisent-print-nullable ()
  "Print NULLABLE."
  (let (i)
    (wisent-log "NULLABLE\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\t%s: %s\n" (aref tags i)
                  (if (aref nullable (- i ntokens))
                      "yes" : "no"))
      (setq i (1+ i)))
    (wisent-log "\n\n")))

(defun wisent-set-nullable ()
  "Set up NULLABLE.
A vector saying which nonterminals can expand into the null string.
NULLABLE[i - NTOKENS] is nil if symbol I can do so."
  (let (r s1 s2 ruleno symbol p squeue rcount rsets relts any-tokens r1)
    (setq nullable (make-vector nvars nil)
          squeue (make-vector nvars 0)
          s1 0 s2 0 ;; s1 = s2 = squeue
          rcount (make-vector (1+ nrules) 0)
          rsets  (make-vector nvars nil)
          relts  (make-vector (+ nitems nvars 1) nil)
          p 0  ;; p = relts
          r 0) ;; r = ritem
    
    (while (aref ritem r)
      (if (< (aref ritem r) 0)
          (progn
            ;; symbol = rlhs[-(*r++)];
            (setq symbol (aref rlhs (- (aref ritem r))))
            (setq r (1+ r))
            (when (and (>= symbol 0)
                       (not (aref nullable (- symbol ntokens))))
              (aset nullable (- symbol ntokens) t)
              (aset squeue s2 symbol) ;; *s2++ = symbol
              (setq s2 (1+ s2))))
        (setq r1 r
              any-tokens nil
              symbol (aref ritem r)
              r (1+ r))
        (while (> symbol 0)
          (if (wisent-ISTOKEN symbol)
              (setq any-tokens t))
          (setq symbol (aref ritem r)
                r (1+ r)))
        (when (not any-tokens)
          (setq ruleno (- symbol)
                r r1
                symbol (aref ritem r)
                r (1+ r))
          (while (> symbol 0)
            ;; rcount[ruleno]++;
            (aset rcount ruleno (1+ (aref rcount ruleno)))
            ;; p->next = rsets[symbol];
            ;; p->value = ruleno;
            (aset relts p (cons ruleno (aref rsets (- symbol ntokens))))
            ;; rsets[symbol] = p;
            (aset rsets (- symbol ntokens) p)
            (setq p (1+ p)) ;; p++
            (setq symbol (aref ritem r)
                  r (1+ r))))))
    
    (while (< s1 s2)
      ;; p = rsets[*s1++];
      (setq p (aref rsets (- (aref squeue s1) ntokens))
            s1 (1+ s1))
      (while p
        (setq p (aref relts p)
              ruleno (car p)
              p (cdr p))
        ;; if (--rcount[ruleno] == 0)
        (when (zerop (aset rcount ruleno (1- (aref rcount ruleno))))
          (setq symbol (aref rlhs ruleno)) ;; symbol must be >= ntokens
          (when (not (aref nullable (- symbol ntokens)))
            (aset nullable (- symbol ntokens) t)
            (aset squeue s2 symbol) ;; *s2++ = symbol
            (setq s2 (1+ s2))))))
    
    (if wisent-debug-flag
        (wisent-print-nullable))
    ))

;;;; -----------
;;;; Subroutines
;;;; -----------

(defun wisent-print-fderives ()
  "Print FDERIVES."
  (let (i j rp)
    (wisent-log "\n\n\nFDERIVES\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\n\n%s derives\n\n" (aref tags i))
      (setq rp (aref fderives (- i ntokens))
            j  0)
      (while (<= j nrules)
	(if (wisent-BITISSET rp j)
            (wisent-log "   %d\n" j))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-set-fderives ()
  "Set up FDERIVES.
An NVARS by NRULES matrix of bits indicating which rules can help
derive the beginning of the data for each nonterminal.  For example,
if symbol 5 can be derived as the sequence of symbols 8 3 20, and one
of the rules for deriving symbol 8 is rule 4, then the
\[5 - NTOKENS, 4] bit in FDERIVES is set."
  (let (i j k)
    (setq fderives (make-vector nvars nil))
    (setq i 0)
    (while (< i nvars)
      (aset fderives i (make-vector rulesetsize 0))
      (setq i (1+ i)))
    
    (wisent-set-firsts)
    
    (setq i ntokens)
    (while (< i nsyms)
      (setq j ntokens)
      (while (< j nsyms)
        ;; if (BITISSET (FIRSTS (i), j - ntokens))
        (when (wisent-BITISSET (aref firsts (- i ntokens)) (- j ntokens))
          (setq k (aref derives (- j ntokens)))
          (while (> (car k) 0) ;; derives[j][k] > 0
            ;; SETBIT (FDERIVES (i), derives[j][k]);
            (wisent-SETBIT (aref fderives (- i ntokens)) (car k))
            (setq k (cdr k))))
        (setq j (1+ j)))
      (setq i (1+ i)))
    
    (if wisent-debug-flag
        (wisent-print-fderives))
    ))

(defun wisent-print-firsts ()
  "Print FIRSTS."
  (let (i j v)
    (wisent-log "\n\n\nFIRSTS\n\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\n\n%s firsts\n\n" (aref tags i))
      (setq v (aref firsts (- i ntokens))
            j 0)
      (while (< j nvars)
        (if (wisent-BITISSET v j)
            (wisent-log "\t\t%d (%s)\n"
                        (+ j ntokens) (aref tags (+ j ntokens))))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-TC (R n)
  "Transive closure.
Given R an N by N matrix of bits, modify its contents to be the
transive closure of what was given."
  (let (i j k)
    ;; R (J, I) && R (I, K) => R (J, K).
    ;; I *must* be the outter loop.
    (setq i 0)
    (while (< i n)
      (setq j 0)
      (while (< j n)
        (when (wisent-BITISSET (aref R j) i)
          (setq k 0)
          (while (< k n)
            (if (wisent-BITISSET (aref R i) k)
                (wisent-SETBIT (aref R j) k))
            (setq k (1+ k))))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-RTC (R n)
  "Reflexive Transitive Closure.
Same as `wisent-TC' and then set all the bits on the diagonal of R, an
N by N matrix of bits."
  (let (i)
    (wisent-TC R n)
    (setq i 0)
    (while (< i n)
      (wisent-SETBIT (aref R i) i)
      (setq i (1+ i)))))

(defun wisent-set-firsts ()
  "Set up FIRSTS.
An NVARS by NVARS bit matrix indicating which items can represent the
beginning of the input corresponding to which other items.  For
example, if some rule expands symbol 5 into the sequence of symbols 8
3 20, the symbol 8 can be the beginning of the data for symbol 5, so
the bit [8 - NTOKENS, 5 - NTOKENS] in FIRSTS is set."
  (let (row symbol sp rowsize i)
    (setq rowsize (wisent-WORDSIZE nvars)
          varsetsize rowsize
          firsts (make-vector nvars nil)
          i 0)
    (while (< i nvars)
      (aset firsts i (make-vector rowsize 0))
      (setq i (1+ i)))
    
    (setq row 0 ;; row = firsts
          i ntokens)
    (while (< i nsyms)
      (setq sp (aref derives (- i ntokens)))
      (while (>= (car sp) 0)
        (setq symbol (aref ritem (aref rrhs (car sp)))
              sp (cdr sp))
        (when (wisent-ISVAR symbol)
          (setq symbol (- symbol ntokens))
          (wisent-SETBIT (aref firsts row) symbol)
          ))
      (setq row (1+ row)
            i   (1+ i)))
    
    (wisent-RTC firsts nvars)
    
    (if wisent-debug-flag
        (wisent-print-firsts))
    ))

(defun wisent-initialize-closure (n)
  "Allocates the ITEMSET and RULESET vectors.
And precomputes useful data so that `wisent-closure' can be called.
N is the number of elements to allocate for ITEMSET."
  (setq itemset (make-vector n 0)
        rulesetsize (wisent-WORDSIZE (1+ nrules))
        ruleset (make-vector rulesetsize 0))
  
  (wisent-set-fderives))

(defun wisent-print-closure ()
  "Print ITEMSET."
  (let (i)
    (wisent-log "\n\nclosure n = %d\n\n" nitemset)
    (setq i 0) ;; isp = itemset
    (while (< i nitemset)
      (wisent-log "   %d\n" (aref itemset i))
      (setq i (1+ i)))))

(defun wisent-closure (core n)
  "Set up RULESET and ITEMSET for the transitions out of CORE state.
Given a vector of item numbers items, of length N, set up RULESET and
ITEMSET to indicate what rules could be run and which items could be
accepted when those items are the active ones.

RULESET contains a bit for each rule.  `wisent-closure' sets the bits
for all rules which could potentially describe the next input to be
read.

ITEMSET is a vector of item numbers; NITEMSET is the number of items
in ITEMSET.  `wisent-closure' places there the indices of all items
which represent units of input that could arrive next."
  (let (c r v symbol ruleno itemno)
    (if (zerop n)
        (progn
          (setq r 0
                v (aref fderives (- start-symbol ntokens)))
          (while (< r rulesetsize)
            ;; ruleset[r] = FDERIVES (start-symbol)[r];
            (aset ruleset r (aref v r))
            (setq r (1+ r)))
          )
      (fillarray ruleset 0)
      (setq c 0)
      (while (< c n)
        (setq symbol (aref ritem (aref core c)))
        (when (wisent-ISVAR symbol)
          (setq r 0
                v (aref fderives (- symbol ntokens)))
          (while (< r rulesetsize)
            ;; ruleset[r] |= FDERIVES (ritem[core[c]])[r];
            (aset ruleset r (logior (aref ruleset r) (aref v r)))
            (setq r (1+ r))))
        (setq c (1+ c)))
      )
    (setq nitemset 0
          c 0
          ruleno 0
          r (* rulesetsize wisent-BITS-PER-WORD))
    (while (< ruleno r)
      (when (wisent-BITISSET ruleset ruleno)
        (setq itemno (aref rrhs ruleno))
        (while (and (< c n) (< (aref core c) itemno))
          (aset itemset nitemset (aref core c))
          (setq nitemset (1+ nitemset)
                c (1+ c)))
        (aset itemset nitemset itemno)
        (setq nitemset (1+ nitemset)))
      (setq ruleno (1+ ruleno)))
    
    (while (< c n)
      (aset itemset nitemset (aref core c))
      (setq nitemset (1+ nitemset)
            c (1+ c)))
    
    (if wisent-debug-flag
        (wisent-print-closure))
    ))

;;;; --------------------------------------------------
;;;; Generate the nondeterministic finite state machine
;;;; --------------------------------------------------

(defun wisent-allocate-itemsets ()
  "Allocate storage for itemsets."
  (let (symbol i count symbol-count)
    ;; Count the number of occurrences of all the symbols in RITEMS.
    (setq count 0
          symbol-count (make-vector nsyms 0)
          i 0)
    (while (setq symbol (aref ritem i))
      (when (> symbol 0)
        (setq count (1+ count))
        (aset symbol-count symbol (1+ (aref symbol-count symbol))))
      (setq i (1+ i)))
    ;; See comments before `wisent-new-itemsets'.  All the vectors of
    ;; items live inside kernel-items.  The number of active items
    ;; after some symbol cannot be more than the number of times that
    ;; symbol appears as an item, which is symbol-count[symbol].  We
    ;; allocate that much space for each symbol.
    (setq kernel-base (make-vector nsyms nil)
          kernel-items (make-vector count 0)
          count 0
          i 0)
    (while (< i nsyms)
      (aset kernel-base i count)
      (setq count (+ count (aref symbol-count i))
            i (1+ i)))
    (setq shift-symbol symbol-count
          kernel-end (make-vector nsyms nil))
    ))

(defun wisent-allocate-storage ()
  "Allocate storage for the state machine."
  (wisent-allocate-itemsets)
  (setq shiftset (make-vector nsyms 0)
        redset (make-vector (1+ nrules) 0)
        state-table (make-vector wisent-state-table-size nil)))

(defun wisent-new-itemsets ()
  "Find which symbols can be shifted in the current state.
And for each one record which items would be active after that shift.
Uses the contents of ITEMSET.  SHIFT-SYMBOL is set to a vector of the
symbols that can be shifted.  For each symbol in the grammar,
KERNEL-BASE[symbol] points to a vector of item numbers activated if
that symbol is shifted, and KERNEL-END[symbol] points after the end of
that vector."
  (let (i shiftcount isp ksp symbol)
    (fillarray kernel-end nil)
    (setq shiftcount 0
          isp 0)
    (while (< isp nitemset)
      (setq i (aref itemset isp)
            isp (1+ isp)
            symbol (aref ritem i))
      (when (> symbol 0)
        (setq ksp (aref kernel-end symbol))
        (when (not ksp)
          ;; shift-symbol[shiftcount++] = symbol;
          (aset shift-symbol shiftcount symbol)
          (setq shiftcount (1+ shiftcount)
                ksp (aref kernel-base symbol)))
        ;; *ksp++ = i + 1;
        (aset kernel-items ksp (1+ i))
        (setq ksp (1+ ksp))
        (aset kernel-end symbol ksp)))
    (setq nshifts shiftcount)))

(defun wisent-new-state (symbol)
  "Create a new state for those items, if necessary.
SYMBOL is the core accessing-symbol.
Subroutine of `wisent-get-state'."
  (let (n p isp1 isp2 iend items)
    (setq isp1  (aref kernel-base symbol)
          iend  (aref kernel-end symbol)
          n     (- iend isp1)
          p     (make-core)
          items (make-vector n 0))
    (set-core-accessing-symbol p symbol)
    (set-core-number p nstates)
    (set-core-nitems p n)
    (set-core-items  p items)
    (setq isp2 0) ;; isp2 = p->items
    (while (< isp1 iend)
      ;; *isp2++ = *isp1++;
      (aset items isp2 (aref kernel-items isp1))
      (setq isp1 (1+ isp1)
            isp2 (1+ isp2)))
    (set-core-next last-state p)
    (setq last-state p
          nstates (1+ nstates))
    p))

(defun wisent-get-state (symbol)
  "Find the state we would get to by shifting SYMBOL.
Return the state number for the state we would get to (from the
current state) by shifting SYMBOL.  Create a new state if no
equivalent one exists already.  Used by `wisent-append-states'."
  (let (key isp1 isp2 iend sp sp2 found n)
    (setq isp1 (aref kernel-base symbol)
          iend (aref kernel-end symbol)
          n    (- iend isp1)
          key  0)
    ;; Add up the target state's active item numbers to get a hash key
    (while (< isp1 iend)
      (setq key (+ key (aref kernel-items isp1))
            isp1 (1+ isp1)))
    (setq key (% key wisent-state-table-size)
          sp (aref state-table key))
    (if sp
        (progn
          (setq found nil)
          (while (not found)
            (when (= (core-nitems sp) n)
              (setq found t
                    isp1 (aref kernel-base symbol)
                    ;; isp2 = sp->items;
                    sp2  (core-items sp)
                    isp2 0)

              (while (and found (< isp1 iend))
                ;; if (*isp1++ != *isp2++)
                (if (not (= (aref kernel-items isp1)
                            (aref sp2 isp2)))
                    (setq found nil))
                (setq isp1 (1+ isp1)
                      isp2 (1+ isp2))))
            (if (not found)
                (if (core-link sp)
                    (setq sp (core-link sp))
 		  ;; sp = sp->link = new-state(symbol)
                  (setq sp (set-core-link sp (wisent-new-state symbol))
                        found t)))))
      ;; bucket is empty
      ;; state-table[key] = sp = new-state(symbol)
      (setq sp (wisent-new-state symbol))
      (aset state-table key sp))
    ;; return (sp->number);
    (core-number sp)))

(defun wisent-append-states ()
  "Find or create the core structures for states.
Use the information computed by `wisent-new-itemsets' to find the
state numbers reached by each shift transition from the current state.
SHIFTSET is set up as a vector of state numbers of those states."
  (let (i j symbol)
    ;; First sort shift-symbol into increasing order
    (setq i 1)
    (while (< i nshifts)
      (setq symbol (aref shift-symbol i)
            j i)
      (while (and (> j 0) (> (aref shift-symbol (1- j)) symbol))
        (aset shift-symbol j (aref shift-symbol (1- j)))
        (setq j (1- j)))
      (aset shift-symbol j symbol)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i nshifts)
      (setq symbol (aref shift-symbol i))
      (aset shiftset i (wisent-get-state symbol))
      (setq i (1+ i)))
    ))

(defun wisent-initialize-states ()
  "Initialize states."
  (let ((p (make-core)))
    (setq first-state p
          last-state  p
          this-state  p
          nstates     1)))

(defun wisent-save-shifts ()
  "Save the NSHIFTS of SHIFTSET into the current linked list."
  (let (p i shifts)
    (setq p      (make-shifts)
          shifts (make-vector nshifts 0)
          i 0)
    (set-shifts-number p (core-number this-state))
    (set-shifts-nshifts p nshifts)
    (set-shifts-shifts  p shifts)
    (while (< i nshifts)
      ;; (p->shifts)[i] = shiftset[i];
      (aset shifts i (aref shiftset i))
      (setq i (1+ i)))

    (if last-shift
        (set-shifts-next last-shift p)
      (setq first-shift p))
    (setq last-shift p)))

(defun wisent-insert-start-shift ()
  "Create the next-to-final state.
That is the state to which a shift has already been made in the
initial state.  Subroutine of `wisent-augment-automaton'."
  (let (statep sp)
    (setq statep (make-core))
    (set-core-number statep nstates)
    (set-core-accessing-symbol statep start-symbol)
    (set-core-next last-state statep)
    (setq last-state statep)
    ;; Make a shift from this state to (what will be) the final state.
    (setq sp (make-shifts))
    (set-shifts-number sp nstates)
    (setq nstates (1+ nstates))
    (set-shifts-nshifts sp 1)
    (set-shifts-shifts sp (vector nstates))
    (set-shifts-next last-shift sp)
    (setq last-shift sp)))

(defun wisent-augment-automaton ()
  "Set up initial and final states as parser wants them.
Make sure that the initial state has a shift that accepts the
grammar's start symbol and goes to the next-to-final state, which has
a shift going to the final state, which has a shift to the termination
state.  Create such states and shifts if they don't happen to exist
already."
  (let (i k statep sp sp2 sp1 shifts)
    (setq sp first-shift)
    (if sp
        (progn
          (if (zerop (shifts-number sp))
              (progn
                (setq k (shifts-nshifts sp)
                      statep (core-next first-state))
                ;; The states reached by shifts from first-state are
                ;; numbered 1...K.  Look for one reached by
                ;; START-SYMBOL.
                (while (and (< (core-accessing-symbol statep) start-symbol)
                            (< (core-number statep) k))
                  (setq statep (core-next statep)))
                (if (= (core-accessing-symbol statep) start-symbol)
                    (progn
                      ;; We already have a next-to-final state.  Make
                      ;; sure it has a shift to what will be the final
                      ;; state.
                      (setq k (core-number statep))
                      (while (and sp (< (shifts-number sp) k))
                        (setq sp1 sp
                              sp (shifts-next sp)))
                      (if (and sp (= (shifts-number sp) k))
                          (progn
                            (setq i (shifts-nshifts sp)
                                  sp2 (make-shifts)
                                  shifts (make-vector (1+ i) 0))
                            (set-shifts-number sp2 k)
                            (set-shifts-nshifts sp2 (1+ i))
                            (set-shifts-shifts sp2 shifts)
                            (aset shifts 0 nstates)
                            (while (> i 0)
                              ;; sp2->shifts[i] = sp->shifts[i - 1];
                              (aset shifts i (aref (shifts-shifts sp) (1- i)))
                              (setq i (1- i)))
                            ;; Patch sp2 into the chain of shifts in
                            ;; place of sp, following sp1.
                            (set-shifts-next sp2 (shifts-next sp))
                            (set-shifts-next sp1 sp2)
                            (if (eq sp last-shift)
                                (setq last-shift sp2))
                            )
                        (setq sp2 (make-shifts))
                        (set-shifts-number sp2 k)
                        (set-shifts-nshifts sp2 1)
                        (set-shifts-shifts sp2 (vector nstates))
                        ;; Patch sp2 into the chain of shifts between
                        ;; sp1 and sp.
                        (set-shifts-next sp2 sp)
                        (set-shifts-next sp1 sp2)
                        (if (not sp)
                            (setq last-shift sp2))
                        )
                      )
                  ;; There is no next-to-final state as yet.
                  ;; Add one more shift in FIRST-SHIFT, going to the
                  ;; next-to-final state (yet to be made).
                  (setq sp first-shift
                        sp2 (make-shifts)
                        i   (shifts-nshifts sp)
                        shifts (make-vector (1+ i) 0))
                  (set-shifts-nshifts sp2 (1+ i))
                  (set-shifts-shifts sp2 shifts)
                  ;; Stick this shift into the vector at the proper place.
                  (setq statep (core-next first-state)
                        k 0
                        i 0)
                  (while (< i (shifts-nshifts sp))
                    (when (and (> (core-accessing-symbol statep) start-symbol)
                               (= i k))
                      (aset shifts k nstates)
                      (setq k (1+ k)))
                    (aset shifts k (aref (shifts-shifts sp) i))
                    (setq statep (core-next statep))
                    (setq i (1+ i)
                          k (1+ k)))
                  (when (= i k)
                    (aset shifts k nstates)
                    (setq k (1+ k)))
                  ;; Patch sp2 into the chain of shifts in place of
                  ;; sp, at the beginning.
                  (set-shifts-next sp2 (shifts-next sp))
                  (setq first-shift sp2)
                  (if (eq last-shift sp)
                      (setq last-shift sp2))
                  ;; Create the next-to-final state, with shift to
                  ;; what will be the final state.
                  (wisent-insert-start-shift)))
            ;; The initial state didn't even have any shifts.  Give it
            ;; one shift, to the next-to-final state.
            (setq sp (make-shifts))
            (set-shifts-nshifts sp 1)
            (set-shifts-shifts sp (vector nstates))
            ;; Patch sp into the chain of shifts at the beginning.
            (set-shifts-next sp first-shift)
            (setq first-shift sp)
            ;; Create the next-to-final state, with shift to what will
            ;; be the final state.
            (wisent-insert-start-shift)))
      ;; There are no shifts for any state.  Make one shift, from the
      ;; initial state to the next-to-final state.
      (setq sp (make-shifts))
      (set-shifts-nshifts sp 1)
      (set-shifts-shifts sp (vector nstates))
      ;; Initialize the chain of shifts with sp.
      (setq first-shift sp
            last-shift sp)
      ;; Create the next-to-final state, with shift to what will be
      ;; the final state.
      (wisent-insert-start-shift))
    ;; Make the final state--the one that follows a shift from the
    ;; next-to-final state.  The symbol for that shift is 0
    ;; (end-of-file).
    (setq statep (make-core))
    (set-core-number statep nstates)
    (set-core-next last-state statep)
    (setq last-state statep)
    ;; Make the shift from the final state to the termination state.
    (setq sp (make-shifts))
    (set-shifts-number sp nstates)
    (setq nstates (1+ nstates))
    (set-shifts-nshifts sp 1)
    (set-shifts-shifts sp (vector nstates))
    (set-shifts-next last-shift sp)
    (setq last-shift sp)
    ;; Note that the variable FINAL-STATE refers to what we sometimes
    ;; call the termination state.
    (setq final-state nstates)
    ;; Make the termination state.
    (setq statep (make-core))
    (set-core-number statep nstates)
    (setq nstates (1+ nstates))
    (set-core-next last-state statep)
    (setq last-state statep)))

(defun wisent-save-reductions ()
  "Make a reductions structure.
Find which rules can be used for reduction transitions from the
current state and make a reductions structure for the state to record
their rule numbers."
  (let (i item count p rules)
    ;; Find and count the active items that represent ends of rules.
    (setq count 0
          i 0)
    (while (< i nitemset)
      (setq item (aref ritem (aref itemset i)))
      (when (< item 0)
        (aset redset count (- item))
        (setq count (1+ count)))
      (setq i (1+ i)))
    ;; Make a reductions structure and copy the data into it.
    (when (> count 0)
      (setq p (make-reductions)
            rules (make-vector count 0))
      (set-reductions-number p (core-number this-state))
      (set-reductions-nreds  p count)
      (set-reductions-rules  p rules)
      (setq i 0)
      (while (< i count)
        ;; (p->rules)[i] = redset[i]
        (aset rules i (aref redset i))
        (setq i (1+ i)))
      (if last-reduction
          (set-reductions-next last-reduction p)
        (setq first-reduction p))
      (setq last-reduction p))))

(defun wisent-generate-states ()
  "Compute the nondeterministic finite state machine from the grammar."
  (working-dynamic-status "(compute nondeterministic finite state machine)")
  (wisent-allocate-storage)
  (wisent-initialize-closure nitems)
  (wisent-initialize-states)
  (while this-state
    ;; Set up RULESET and ITEMSET for the transitions out of this
    ;; state.  RULESET gets a 1 bit for each rule that could reduce
    ;; now.  ITEMSET gets a vector of all the items that could be
    ;; accepted next.
    (wisent-closure (core-items this-state) (core-nitems this-state))
    ;; Record the reductions allowed out of this state.
    (wisent-save-reductions)
    ;; Find the itemsets of the states that shifts can reach.
    (wisent-new-itemsets)
    ;; Find or create the core structures for those states.
    (wisent-append-states)
    ;; Create the shifts structures for the shifts to those states,
    ;; now that the state numbers transitioning to are known.
    (if (> nshifts 0)
        (wisent-save-shifts))
    ;; States are queued when they are created; process them all.
    (setq this-state (core-next this-state)))
  ;; Set up initial and final states as parser wants them.
  (wisent-augment-automaton))

;;;; ---------------------------
;;;; Compute look-ahead criteria
;;;; ---------------------------

;; Compute how to make the finite state machine deterministic; find
;; which rules need lookahead in each state, and which lookahead
;; tokens they accept.

;; `wisent-lalr', the entry point, builds these data structures:

;; GOTO-MAP, FROM-STATE and TO-STATE record each shift transition
;; which accepts a variable (a nonterminal).  NGOTOS is the number of
;; such transitions.
;; FROM-STATE[t] is the state number which a transition leads from and
;; TO-STATE[t] is the state number it leads to.
;; All the transitions that accept a particular variable are grouped
;; together and GOTO-MAP[i - NTOKENS] is the index in FROM-STATE and
;; TO-STATE of the first of them.

;; CONSISTENT[s] is non-nil if no lookahead is needed to decide what
;; to do in state s.

;; LARULENO is a vector which records the rules that need lookahead in
;; various states.  The elements of LARULENO that apply to state s are
;; those from LOOKAHEADS[s] through LOOKAHEADS[s+1]-1.  Each element
;; of LARULENO is a rule number.

;; If LR is the length of LARULENO, then a number from 0 to LR-1 can
;; specify both a rule and a state where the rule might be applied.
;; LA is a LR by NTOKENS matrix of bits.
;; LA[l, i] is 1 if the rule LARULENO[l] is applicable in the
;; appropriate state when the next token is symbol i.
;; If LA[l, i] and LA[l, j] are both 1 for i != j, it is a conflict.

(wisent-defcontext digraph
  INDEX R VERTICES
  infinity top)

(defun wisent-traverse (i)
  "Traverse I."
  (let (j k height Ri Fi break)
    (setq top (1+ top)
          height top)
    (aset VERTICES top i) ;; VERTICES[++top] = i
    (aset INDEX i top) ;; INDEX[i] = height = top
    
    (setq Ri (aref R i))
    (when Ri
      (setq j 0)
      (while (>= (aref Ri j) 0)
        (if (zerop (aref INDEX (aref Ri j)))
            (wisent-traverse (aref Ri j)))
        ;; if (INDEX[i] > INDEX[R[i][j]])
        (if (> (aref INDEX i) (aref INDEX (aref Ri j)))
            ;; INDEX[i] = INDEX[R[i][j]];
            (aset INDEX i (aref INDEX (aref Ri j))))
        (setq Fi (aref F i)
              k 0)
        (while (< k tokensetsize)
          ;; F (i)[k] |= F (R[i][j])[k];
          (aset Fi k (logior (aref Fi k)
                             (aref (aref F (aref Ri j)) k)))
           (setq k (1+ k)))
        (setq j (1+ j))))
    
    (when (= (aref INDEX i) height)
      (setq break nil)
      (while (not break)
        (setq j (aref VERTICES top) ;; j = VERTICES[top--]
              top (1- top))
        (aset INDEX j infinity)
        (if (= i j)
            (setq break t)
          (setq k 0)
          (while (< k tokensetsize)
            ;; F (j)[k] = F (i)[k];
            (aset (aref F j) k (aref (aref F i) k))
            (setq k (1+ k))))))
    ))

(defun wisent-digraph (relation)
  "Digraph RELATION."
  (wisent-with-context digraph
    (setq infinity (+ ngotos 2)
          INDEX    (make-vector (1+ ngotos) 0)
          VERTICES (make-vector (1+ ngotos) 0)
          top      0
          R        relation)
    (let ((i 0))
      (while (< i ngotos)
        (if (and (= (aref INDEX i) 0) (aref R i))
            (wisent-traverse i))
        (setq i (1+ i))))))

(defun wisent-set-state-table ()
  "Build state table."
  (let (sp)
    (setq state-table (make-vector nstates nil)
          sp first-state)
    (while sp
      (aset state-table (core-number sp) sp)
      (setq sp (core-next sp)))))

(defun wisent-set-accessing-symbol ()
  "Build accessing symbol table."
  (let (sp)
    (setq accessing-symbol (make-vector nstates 0)
          sp first-state)
    (while sp
      (aset accessing-symbol (core-number sp) (core-accessing-symbol sp))
      (setq sp (core-next sp)))))

(defun wisent-set-shift-table ()
  "Build shift table."
  (let (sp)
    (setq shift-table (make-vector nstates nil)
          sp first-shift)
    (while sp
      (aset shift-table (shifts-number sp) sp)
      (setq sp (shifts-next sp)))))

(defun wisent-set-reduction-table ()
  "Build reduction table."
  (let (rp)
    (setq reduction-table (make-vector nstates nil)
          rp first-reduction)
    (while rp
      (aset reduction-table (reductions-number rp) rp)
      (setq rp (reductions-next rp)))))

(defun wisent-set-maxrhs ()
  "Setup MAXRHS length."
  (let (i len max)
    (setq len 0
          max 0
          i   0)
    (while (aref ritem i)
      (if (> (aref ritem i) 0)
          (setq len (1+ len))
        (if (> len max)
            (setq max len))
        (setq len 0))
      (setq i (1+ i)))
    (setq maxrhs max)))

(defun wisent-initialize-LA ()
  "Set up LA."
  (let (i j k count rp sp np v)
    (setq consistent (make-vector nstates nil)
          lookaheads (make-vector (1+ nstates) 0)
          count 0
          i 0)
    (while (< i nstates)
      (aset lookaheads i count)
      (setq rp (aref reduction-table i)
            sp (aref shift-table i))
      ;; if (rp &&
      ;;     (rp->nreds > 1
      ;;      || (sp && ! ISVAR(accessing-symbol[sp->shifts[0]]))))
      (if (and rp
               (or (> (reductions-nreds rp) 1)
                   (and sp
                        (not (wisent-ISVAR
                              (aref accessing-symbol
                                    (aref (shifts-shifts sp) 0)))))))
          (setq count (+ count (reductions-nreds rp)))
        (aset consistent i t))

      (when sp
        (setq k 0
              j (shifts-nshifts sp)
              v (shifts-shifts sp))
        (while (< k j)
          (when (= (aref accessing-symbol (aref v k))
                   error-token-number)
            (aset consistent i nil)
            (setq k j)) ;; break
          (setq k (1+ k))))
      (setq i (1+ i)))
    
    (aset lookaheads nstates count)
    
    (if (zerop count)
        (progn
          (setq LA (make-vector 1 nil)
                LAruleno (make-vector 1 0)
                lookback (make-vector 1 nil)))
      (setq LA (make-vector count nil)
            LAruleno (make-vector count 0)
            lookback (make-vector count nil)))
    (setq i 0 j (length LA))
    (while (< i j)
      (aset LA i (make-vector tokensetsize 0))
      (setq i (1+ i)))
    
    (setq np 0
          i  0)
    (while (< i nstates)
      (when (not (aref consistent i))
        (setq rp (aref reduction-table i))
        (when rp
          (setq j 0
                k (reductions-nreds rp)
                v (reductions-rules rp))
          (while (< j k)
            (aset LAruleno np (aref v j))
            (setq np (1+ np)
                  j  (1+ j)))))
      (setq i (1+ i)))))

(defun wisent-set-goto-map ()
  "Set up GOTO-MAP."
  (let (sp i j symbol k temp-map state1 state2 v)
    (setq goto-map (make-vector (1+ nvars) 0)
          temp-map (make-vector (1+ nvars) 0))
    
    (setq ngotos 0
          sp first-shift)
    (while sp
      (setq i (1- (shifts-nshifts sp))
            v (shifts-shifts sp))
      (while (>= i 0)
        (setq symbol (aref accessing-symbol (aref v i)))
        (if (wisent-ISTOKEN symbol)
            (setq i 0) ;; break
          (setq ngotos (1+ ngotos))
          ;; goto-map[symbol]++;
          (aset goto-map (- symbol ntokens)
                (1+ (aref goto-map (- symbol ntokens)))))
        (setq i (1- i)))
      (setq sp (shifts-next sp)))

    (setq k 0
          i ntokens
          j 0)
    (while (< i nsyms)
      (aset temp-map j k)
      (setq k (+ k (aref goto-map j))
            i (1+ i)
            j (1+ j)))
    (setq i ntokens
          j 0)
    (while (< i nsyms)
      (aset goto-map j (aref temp-map j))
      (setq i (1+ i)
            j (1+ j)))
    ;; goto-map[nsyms] = ngotos;
    ;; temp-map[nsyms] = ngotos;
    (aset goto-map j ngotos)
    (aset temp-map j ngotos)
    
    (setq from-state (make-vector ngotos 0)
          to-state   (make-vector ngotos 0)
          sp first-shift)
    (while sp
      (setq state1 (shifts-number sp)
            v      (shifts-shifts sp)
            i      (1- (shifts-nshifts sp)))
      (while (>= i 0)
        (setq state2 (aref v i)
              symbol (aref accessing-symbol state2))
        (if (wisent-ISTOKEN symbol)
            (setq i 0) ;; break
          ;; k = temp-map[symbol]++;
          (setq k (aref temp-map (- symbol ntokens)))
          (aset temp-map (- symbol ntokens) (1+ k))
          (aset from-state k state1)
          (aset to-state k state2))
        (setq i (1- i)))
      (setq sp (shifts-next sp)))
  ))

(defun wisent-map-goto (state symbol)
  "Maps a STATE/SYMBOL pair into its numeric representation."
  (let (high low middle s result)
    ;; low = goto-map[symbol];
    ;; high = goto-map[symbol + 1] - 1;
    (setq low (aref goto-map (- symbol ntokens))
          high (1- (aref goto-map (- (1+ symbol) ntokens))))
    (while (and (not result) (<= low high))
      (setq middle (/ (+ low high) 2)
            s (aref from-state middle))
      (cond
       ((= s state)
        (setq result middle))
       ((< s state)
        (setq low (1+ middle)))
       (t
        (setq high (1- middle)))))
    (or result
        (error "In wisent-map-goto"))
    ))

(defun wisent-initialize-F ()
  "Set up F."
  (let (i j k sp edge rowp rp reads nedges stateno symbol v break)
    (setq F (make-vector ngotos nil)
          i 0)
    (while (< i ngotos)
      (aset F i (make-vector tokensetsize 0))
      (setq i (1+ i)))
    
    (setq reads (make-vector ngotos nil)
          edge  (make-vector (1+ ngotos) 0)
          nedges 0
          rowp 0 ;; rowp = F
          i 0)
    (while (< i ngotos)
      (setq stateno (aref to-state i)
            sp (aref shift-table stateno))
      (when sp
        (setq k (shifts-nshifts sp)
              v (shifts-shifts sp)
              j 0
              break nil)
        (while (and (not break) (< j k))
          ;; symbol = accessing-symbol[sp->shifts[j]];
          (setq symbol (aref accessing-symbol (aref v j)))
          (if (wisent-ISVAR symbol)
              (setq break t) ;; break
            (wisent-SETBIT (aref F rowp) symbol)
            (setq j (1+ j))))
        
        (while (< j k)
          ;; symbol = accessing-symbol[sp->shifts[j]];
          (setq symbol (aref accessing-symbol (aref v j)))
          (when (aref nullable (- symbol ntokens))
            (aset edge nedges (wisent-map-goto stateno symbol))
            (setq nedges (1+ nedges)))
          (setq j (1+ j)))
        
	(when (> nedges 0)
          ;; reads[i] = rp = NEW2(nedges + 1, short);
          (setq rp (make-vector (1+ nedges) 0)
                j 0)
          (aset reads i rp)
          (while (< j nedges)
            ;; rp[j] = edge[j];
            (aset rp j (aref edge j))
            (setq j (1+ j)))
          (aset rp nedges -1)
          (setq nedges 0)))
      (setq rowp (1+ rowp))
      (setq i (1+ i)))
    (wisent-digraph reads)
    ))

(defun wisent-add-lookback-edge (stateno ruleno gotono)
  "Add a lookback edge.
STATENO, RULENO, GOTONO are self-explanatory."
  (let (i k found)
    (setq i (aref lookaheads stateno)
          k (aref lookaheads (1+ stateno))
          found nil)
    (while (and (not found) (< i k))
      (if (= (aref LAruleno i) ruleno)
          (setq found t)
        (setq i (1+ i))))
    
    (or found
        (error "In wisent-add-lookback-edge"))
    
    ;;                value  . next
    ;; lookback[i] = (gotono . lookback[i])
    (aset lookback i (cons gotono (aref lookback i)))))

(defun wisent-transpose (R-arg n)
  "Return the transpose of R-ARG, of size N.
Destroy R-ARG, as it is replaced with the result.  R-ARG[I] is nil or
a -1 terminated list of numbers.  RESULT[NUM] is nil or the -1
terminated list of the I such as NUM is in R-ARG[I]."
  (let (i j new-R end-R nedges v sp)
    (setq new-R  (make-vector n nil)
          end-R  (make-vector n nil)
          nedges (make-vector n 0))

    ;; Count.
    (setq i 0)
    (while (< i n)
      (setq v (aref R-arg i))
      (when v
        (setq j 0)
        (while (>= (aref v j) 0)
          (aset nedges (aref v j) (1+ (aref nedges (aref v j))))
          (setq j (1+ j))))
      (setq i (1+ i)))
    
    ;; Allocate.
    (setq i 0)
    (while (< i n)
      (when (> (aref nedges i) 0)
        (setq sp (make-vector (1+ (aref nedges i)) 0))
        (aset sp (aref nedges i) -1)
        (aset new-R i sp)
        (aset end-R i 0))
      (setq i (1+ i)))
    
    ;; Store.
    (setq i 0)
    (while (< i n)
      (setq v (aref R-arg i))
      (when v
        (setq j 0)
        (while (>= (aref v j) 0)
          (aset (aref new-R (aref v j)) (aref end-R (aref v j)) i)
          (aset end-R (aref v j) (1+ (aref end-R (aref v j))))
          (setq j (1+ j))))
      (setq i (1+ i)))
    
    new-R))

(defun wisent-build-relations ()
  "Build relations."
  (let (i j k rulep rp sp length nedges done state1 stateno
          symbol1 symbol2 edge states new-includes v)
    (setq includes (make-vector ngotos nil)
          edge (make-vector (1+ ngotos) 0)
          states (make-vector (1+ maxrhs) 0)
          i 0)
    
    (while (< i ngotos)
      (setq nedges 0
            state1 (aref from-state i)
            symbol1 (aref accessing-symbol (aref to-state i))
            rulep (aref derives (- symbol1 ntokens)))
      
      (while (> (car rulep) 0)
        (aset states 0 state1)
        (setq length 1
              stateno state1
              rp (aref rrhs (car rulep))) ;; rp = ritem + rrhs[*rulep]
        (while (> (aref ritem rp) 0) ;; *rp > 0
          (setq symbol2 (aref ritem rp)
                sp (aref shift-table stateno)
                k  (shifts-nshifts sp)
                v  (shifts-shifts sp)
                j  0)
          (while (< j k)
            (setq stateno (aref v j))
            (if (= (aref accessing-symbol stateno) symbol2)
                (setq j k) ;; break
              (setq j (1+ j))))
          ;; states[length++] = stateno;
          (aset states length stateno)
          (setq length (1+ length))
          (setq rp (1+ rp)))
        
        (if (not (aref consistent stateno))
            (wisent-add-lookback-edge stateno (car rulep) i))
        
        (setq length (1- length)
              done nil)
        (while (not done)
          (setq done t
                rp (1- rp))
          (when (and (>= rp 0) (wisent-ISVAR (aref ritem rp)))
            ;; stateno = states[--length];
            (setq length (1- length)
                  stateno (aref states length))
            (aset edge nedges (wisent-map-goto stateno (aref ritem rp)))
            (setq nedges (1+ nedges))
            (if (aref nullable (- (aref ritem rp) ntokens))
                (setq done nil))))
        (setq rulep (cdr rulep)))
      
      (when (> nedges 0)
        (setq v (make-vector (1+ nedges) 0)
              j 0)
        (aset includes i v)
        (while (< j nedges)
          (aset v j (aref edge j))
          (setq j (1+ j)))
        (aset v nedges -1))
      (setq i (1+ i)))

    (setq includes (wisent-transpose includes ngotos))
    ))

(defun wisent-compute-FOLLOWS ()
  "Compute follows."
  (wisent-digraph includes))

(defun wisent-compute-lookaheads ()
  "Compute lookaheads."
  (let (i j n v1 v2 sp)
    (setq n (aref lookaheads nstates)
          i 0)
    (while (< i n)
      (setq sp (aref lookback i))
      (while sp
        (setq v1 (aref LA i)
              v2 (aref F (car sp))
              j  0)
        (while (< j tokensetsize)
          ;; LA (i)[j] |= F (sp->value)[j]
          (aset v1 j (logior (aref v1 j) (aref v2 j)))
          (setq j (1+ j)))
        (setq sp (cdr sp)))
      (setq i (1+ i)))))

(defun wisent-lalr ()
  "Make the nondeterministic finite state machine deterministic."
  (working-dynamic-status "(make finite state machine deterministic)")
  (setq tokensetsize (wisent-WORDSIZE ntokens))
  (wisent-set-state-table)
  (wisent-set-accessing-symbol)
  (wisent-set-shift-table)
  (wisent-set-reduction-table)
  (wisent-set-maxrhs)
  (wisent-initialize-LA)
  (wisent-set-goto-map)
  (wisent-initialize-F)
  (wisent-build-relations)
  (wisent-compute-FOLLOWS)
  (wisent-compute-lookaheads))

;;;; -----------------------------------------------
;;;; Find and resolve or report look-ahead conflicts
;;;; -----------------------------------------------

(defsubst wisent-log-resolution (state LAno token resolution)
  "Log a shift-reduce conflict resolution.
In specified STATE between rule pointed by lookahead number LANO and
TOKEN, resolved as RESOLUTION."
  (wisent-log
   "Conflict in state %d between rule %d and token %s resolved as %s.\n"
   state (aref LAruleno LAno) token resolution))

(defun wisent-flush-shift (state token)
  "Turn off the shift recorded in the specified STATE for TOKEN.
Used when we resolve a shift-reduce conflict in favor of the reduction."
  (let (shiftp i k v)
    (when (setq shiftp (aref shift-table state))
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (if (and (not (zerop (aref v i)))
                 (= token (aref accessing-symbol (aref v i))))
            (aset v i 0))
        (setq i (1+ i))))))

(defun wisent-resolve-sr-conflict (state lookaheadnum)
  "Attempt to resolve shift-reduce conflict for one rule.
Resolve by means of precedence declarations.  The conflict occurred in
specified STATE for the rule pointed by the lookahead symbol
LOOKAHEADNUM.  It has already been checked that the rule has a
precedence.  A conflict is resolved by modifying the shift or reduce
tables so that there is no longer a conflict."
  (let (i redprec errp errs nerrs token sprec sassoc)
    ;; Find the rule to reduce by to get precedence of reduction
    (setq redprec (aref rprec (aref LAruleno lookaheadnum))
          errp  (make-errs)
          errs  (make-vector ntokens 0)
          nerrs 0
          i 0)
    (set-errs-errs errp errs)
    (while (< i ntokens)
      (setq token (aref tags i))
      (when (and (wisent-BITISSET (aref LA lookaheadnum) i)
                 (wisent-BITISSET lookaheadset i)
                 (setq sprec (get token 'wisent--prec)))
        ;; Shift-reduce conflict occurs for token number I and it has
        ;; a precedence.  The precedence of shifting is that of token
        ;; I.
        (cond
         ((< sprec redprec)
          (wisent-log-resolution state lookaheadnum token "reduce")
          ;;  Flush the shift for this token
          (wisent-RESETBIT lookaheadset i)
          (wisent-flush-shift state i)
          )
         ((> sprec redprec)
          (wisent-log-resolution state lookaheadnum token "shift")
          ;; Flush the reduce for this token
          (wisent-RESETBIT (aref LA lookaheadnum) i)
          )
         (t
          ;; Matching precedence levels.
          ;; For left association, keep only the reduction.
          ;; For right association, keep only the shift.
          ;; For nonassociation, keep neither.
          (setq sassoc (get token 'wisent--assoc))
          (cond
           ((eq sassoc 'right)
            (wisent-log-resolution state lookaheadnum token "shift"))
           ((eq sassoc 'left)
            (wisent-log-resolution state lookaheadnum token "reduce"))
           ((eq sassoc 'nonassoc)
            (wisent-log-resolution state lookaheadnum token "an error"))
           )
          (when (not (eq sassoc 'right))
            ;; Flush the shift for this token
            (wisent-RESETBIT lookaheadset i)
            (wisent-flush-shift state i))
          (when (not (eq sassoc 'left))
            ;; Flush the reduce for this token
            (wisent-RESETBIT (aref LA lookaheadnum) i))
          (when (eq sassoc 'nonassoc)
            ;; Record an explicit error for this token
            (aset errs nerrs i)
            (setq nerrs (1+ nerrs)))
          )))
      (setq i (1+ i)))
    (when (> nerrs 0)
      (set-errs-nerrs errp nerrs)
      (aset err-table state errp))
    ))

(defun wisent-set-conflicts (state)
  "Find and attempt to resolve conflicts in specified STATE."
  (let (i j k v c shiftp symbol)
    (if (aref consistent state)
        nil
      (fillarray lookaheadset 0)
      (when (setq shiftp (aref shift-table state))
        (setq k (shifts-nshifts shiftp)
              v (shifts-shifts shiftp)
              i 0)
        (while (< i k)
          (setq symbol (aref accessing-symbol (aref v i)))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            (wisent-SETBIT lookaheadset symbol)
            (setq i (1+ i)))))
      
      ;; Loop over all rules which require lookahead in this state
      ;; first check for shift-reduce conflict, and try to resolve
      ;; using precedence
      (setq i (aref lookaheads state)
            k (aref lookaheads (1+ state)))
      (while (< i k)
        (when (aref rprec (aref LAruleno i))
          (setq v (aref LA i)
                j 0)
          (while (< j tokensetsize)
            (if (zerop (logand (aref v j) (aref lookaheadset j)))
                (setq j (1+ j))
              ;; if (LA (i)[j] & lookaheadset[j])
              (wisent-resolve-sr-conflict state i)
              (setq j tokensetsize)))) ;; break
        (setq i (1+ i)))
      
      ;; Loop over all rules which require lookahead in this state
      ;; Check for conflicts not resolved above.
      (setq i (aref lookaheads state))
      (while (< i k)
          (setq v (aref LA i)
                j 0)
          (while (< j tokensetsize)
            ;; if (LA (i)[j] & lookaheadset[j])
            (when (not (zerop (logand (aref v j) (aref lookaheadset j))))
              (aset conflicts state t)
              (setq any-conflicts t))
            (setq j (1+ j)))
          (setq j 0)
          (while (< j tokensetsize)
            ;; lookaheadset[j] |= LA (i)[j];
            (aset lookaheadset j (logior (aref lookaheadset j)
                                         (aref v j)))
            (setq j (1+ j)))
        (setq i (1+ i)))
      )))

(defun wisent-resolve-conflicts ()
  "Find and resolve conflicts."
  (working-dynamic-status "(resolve conflicts)")
  (let (i)
    (setq conflicts    (make-vector nstates nil)
          shiftset     (make-vector tokensetsize 0)
          lookaheadset (make-vector tokensetsize 0)
          err-table    (make-vector nstates nil)
          any-conflicts 0
          i 0)
    (while (< i nstates)
      (wisent-set-conflicts i)
      (setq i (1+ i)))))

(defun wisent-count-sr-conflicts (state)
  "Count the number of shift/reduce conflicts in specified STATE."
  (let (i j k mask shiftp symbol v)
    (setq src-count 0
          shiftp (aref shift-table state))
    (when shiftp
      (fillarray shiftset 0)
      (fillarray lookaheadset 0)
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (when (not (zerop (aref v i)))
          (setq symbol (aref accessing-symbol (aref v i)))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            (wisent-SETBIT shiftset symbol)))
        (setq i (1+ i)))
      
      (setq k (aref lookaheads (1+ state))
            i (aref lookaheads state))
      (while (< i k)
        (setq v (aref LA i)
              j 0)
        (while (< j tokensetsize)
          ;; lookaheadset[j] |= LA (i)[j]
          (aset lookaheadset j (logior (aref lookaheadset j)
                                       (aref v j)))
          (setq j (1+ j)))
        (setq i (1+ i)))
      
      (setq k 0)
      (while (< k tokensetsize)
        ;; lookaheadset[k] &= shiftset[k];
        (aset lookaheadset k (logand (aref lookaheadset k)
                                     (aref shiftset k)))
        (setq k (1+ k)))
      
      (setq i 0)
      (while (< i ntokens)
        (if (wisent-BITISSET lookaheadset i)
            (setq src-count (1+ src-count)))
        (setq i (1+ i))))
    src-count))

(defun wisent-count-rr-conflicts (state)
  "Count the number of reduce/reduce conflicts in specified STATE."
  (let (i j count n m)
    (setq rrc-count 0
          m (aref lookaheads state)
          n (aref lookaheads (1+ state)))
    (when (>= (- n m) 2)
      (setq i 0)
      (while (< i ntokens)
        (setq count 0
              j m)
        (while (< j n)
          (if (wisent-BITISSET (aref LA j) i)
              (setq count (1+ count)))
          (setq j (1+ j))
          (if (>= count 2)
              (setq rrc-count (1+ rrc-count)))
          (setq i (1+ i)))))
    rrc-count))

(defvar wisent-expected-conflicts nil
  "*If non-nil suppress the warning about shift/reduce conflicts.
It is a decimal integer N that says there should be no warning if
there are N shift/reduce conflicts and no reduce/reduce conflicts.  A
warning is given if there are either more or fewer conflicts, or if
there are any reduce/reduce conflicts.")

(defun wisent-total-conflicts ()
  "Report the total number of conflicts."
  (unless (and (zerop rrc-total)
               (or (zerop src-total)
                   (= src-total (or wisent-expected-conflicts 0))))
    (let* ((src (wisent-source))
           (src (if src (concat " in " src) ""))
           (msg (format "Grammar%s contains" src)))
      (if (> src-total 0)
          (setq msg (format "%s %d shift/reduce conflict%s"
                            msg src-total (if (> src-total 1)
                                              "s" ""))))
      (if (and (> src-total 0) (> rrc-total 0))
          (setq msg (format "%s and" msg)))
      (if (> rrc-total 0)
        (setq msg (format "%s %d reduce/reduce conflict%s"
                          msg rrc-total (if (> rrc-total 1)
                                            "s" ""))))
      (message msg))))

(defun wisent-print-conflicts ()
  "Report conflicts."
  (let (i)
    (setq  src-total 0
           rrc-total 0
           i 0)
    (while (< i nstates)
      (when (aref conflicts i)
        (wisent-count-sr-conflicts i)
        (wisent-count-rr-conflicts i)
        (setq src-total (+ src-total src-count)
              rrc-total (+ rrc-total rrc-count))
        
        (wisent-log "State %d contains" i)
        (if (> src-count 0)
            (wisent-log " %d shift/reduce conflict%s"
                        src-count (if (> src-count 1) "s" "")))
        
        (if (and (> src-count 0) (> rrc-count 0))
            (wisent-log " and"))

        (if (> rrc-count 0)
            (wisent-log " %d reduce/reduce conflict%s"
                        i rrc-count (if (> rrc-count 1) "s" "")))
        
        (wisent-log ".\n"))
      (setq i (1+ i)))
    (wisent-total-conflicts)))

;;;; --------------------------------------
;;;; Report information on generated parser
;;;; --------------------------------------
(defun wisent-print-grammar ()
  "Print grammar."
  (let (i rule lhs rhs)
    (wisent-log "\n\nGrammar\n\n  Number, Rule\n")
    (setq i 1)
    (while (< i nrules)
      (setq rule (car (aref rule-table i))
            lhs  (car rule)
            rhs  (cdr rule))
      (wisent-log "  %s  %s -> %s\n"
                  (wisent-pad-string (number-to-string i) 6)
                  (aref tags lhs)
                  (if rhs
                      (mapconcat #'(lambda (i)
                                     (symbol-name (aref tags i)))
                                 rhs " ")
                    "/* empty */"))
      (setq i (1+ i)))))

(defun wisent-print-reductions (state)
  "Print reductions on STATE."
  (let (i j k v rule symbol mask m n defaulted
          default-LA default-rule cmax count shiftp errp nodefault)
    (setq default-rule 0
          nodefault nil
          i 0)
    (fillarray shiftset 0)
    (setq shiftp (aref shift-table state))
    (when shiftp
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts  shiftp)
            i 0)
      (while (< i k)
        (when (aref v i)
          (setq symbol (aref accessing-symbol(aref v i)))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            ;; If this state has a shift for the error token, don't
            ;; use a default rule.
            (if (= symbol error-token-number)
                (setq nodefault t))
            (wisent-SETBIT shiftset symbol)))
        (setq i (1+ i))))
    
    (setq errp (aref err-table state))
    (when errp
      (setq k (errs-nerrs errp)
            v (errs-errs errp)
            i 0)
      (while (< i k)
        (if (setq symbol (aref v i))
            (wisent-SETBIT shiftset symbol))
        (setq i (1+ i))))
    
    (setq m (aref lookaheads state)
          n (aref lookaheads (1+ state)))
    
    (cond
     ((and (= (- n m) 1) (not nodefault))
      (setq default-rule (aref LAruleno m)
            v (aref LA m)
            k 0)
      (while (< k tokensetsize)
        (aset lookaheadset k (logand (aref v k)
                                     (aref shiftset k)))
        (setq k (1+ k)))
          
      (setq i 0)
      (while (< i ntokens)
        (if (wisent-BITISSET lookaheadset i)
            (wisent-log "    %s\t[reduce using rule %d (%s)]\n"
                        (aref tags i) default-rule
                        (aref tags (aref rlhs default-rule))))
        (setq i (1+ i)))
      (wisent-log "    $default\treduce using rule %d (%s)\n\n"
                  default-rule
                  (aref tags (aref rlhs default-rule)))
      )
     ((>= (- n m) 1)
      (setq cmax 0
            default-LA -1
            default-rule 0)
      (when (not nodefault)
        (setq i m)
        (while (< i n)
          (setq v (aref LA i)
                count 0
                k 0)
          (while (< k tokensetsize)
            ;; lookaheadset[k] = LA (i)[k] & ~shiftset[k]
            (aset lookaheadset k
                  (logand (aref v k)
                          (lognot (aref shiftset k))))
            (setq k (1+ k)))
          (setq j 0)
          (while (< j ntokens)
            (if (wisent-BITISSET lookaheadset j)
                (setq count (1+ count)))
            (setq j (1+ j)))
          (if (> count cmax)
              (setq cmax count
                    default-LA i
                    default-rule (aref LAruleno i)))
          (setq k 0)
          (while (< k tokensetsize)
            (aset shiftset k (logior (aref shiftset k)
                                     (aref lookaheadset k)))
            (setq k (1+ k)))
          (setq i (1+ i))))
            
      (fillarray shiftset 0)

      (when shiftp
        (setq k (shifts-nshifts shiftp)
              v (shifts-shifts  shiftp)
              i 0)
        (while (< i k)
          (when (aref v i)
            (setq symbol (aref accessing-symbol(aref v i)))
            (if (wisent-ISVAR symbol)
                (setq i k) ;; break
              (wisent-SETBIT shiftset symbol)))
          (setq i (1+ i))))
            
      (setq i 0)
      (while (< i ntokens)
        (setq defaulted nil
              count (if (wisent-BITISSET shiftset i) 1 0)
              j m)
        (while (< j n)
          (when (wisent-BITISSET (aref LA j) i)
            (if (zerop count)
                (progn
                  (if (not (= j default-LA))
                      (wisent-log
                       "    %s\treduce using rule %d (%s)\n"
                       (aref tags i) (aref LAruleno j)
                       (aref tags (aref rlhs (aref LAruleno j))))
                    (setq defaulted t))
                  (setq count (1+ count)))
              (if defaulted
                  (wisent-log
                   "    %s\treduce using rule %d (%s)\n"
                   (aref tags i) (aref LAruleno default-LA)
                   (aref tags (aref rlhs (aref LAruleno default-LA)))))
              (setq defaulted nil)
              (wisent-log
               "    %s\treduce using rule %d (%s)\n"
               (aref tags i) (aref LAruleno j)
               (aref tags (aref rlhs (aref LAruleno j))))))
          (setq j (1+ j)))
        (setq i (1+ i)))
            
      (if (>= default-LA 0)
          (wisent-log
           "    $default\treduce using rule %d (%s)\n"
           default-rule
           (aref tags (aref rlhs default-rule))))
      ))))

(defun wisent-print-actions (state)
  "Print actions on STATE."
  (let (i j k v state1 symbol shiftp errp redp rule nerrs break)
    (setq shiftp (aref shift-table state)
          redp   (aref reduction-table state)
          errp   (aref err-table state))
    (if (and (not shiftp) (not redp))
        (if (= final-state state)
            (wisent-log "    $default\taccept\n")
          (wisent-log "    NO ACTIONS\n"))
     (if (not shiftp)
         (setq i 0
               k 0)
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0
            break nil)
      (while (and (not break) (< i k))
        (if (zerop (setq state1 (aref v i)))
            (setq i (1+ i))
          (setq symbol (aref accessing-symbol state1))
          ;;  The following line used to be turned off.
          (if (wisent-ISVAR symbol)
              (setq break t) ;; break
            (wisent-log "    %s\tshift, and go to state %d\n"
                        (aref tags symbol) state1)
            (setq i (1+ i)))))
      (if (> i 0)
          (wisent-log "\n")))
     
     (when errp
       (setq nerrs (errs-nerrs errp)
             v (errs-errs errp)
             j 0)
       (while (< j nerrs)
         (if (aref v j)
             (wisent-log "    %s\terror (nonassociative)\n"
                         (aref tags (aref v j))))
         (setq j (1+ j)))
       (if (> j 0)
           (wisent-log "\n")))
    
     (cond
      ((and (aref consistent state) redp)
       (setq rule (aref (reductions-rules redp) 0)
             symbol (aref rlhs rule))
       (wisent-log "    $default\treduce using rule %d (%s)\n\n"
                   rule (aref tags symbol))
       )
      (redp
       (wisent-print-reductions state)
       ))
    
     (when (< i k)
       (setq v (shifts-shifts shiftp))
       (while (< i k)
         (when (setq state1 (aref v i))
           (setq symbol (aref accessing-symbol state1))
           (wisent-log "    %s\tgo to state %d\n"
                       (aref tags symbol) state1))
         (setq i (1+ i)))
       (wisent-log "\n"))
     )))

(defun wisent-print-core (state)
  "Print STATE core."
  (let (i k rule statep sp sp1)
    (setq statep (aref state-table state)
          k (core-nitems statep))
    (when (> k 0)
      (setq i 0)
      (while (< i k)
        ;; sp1 = sp = ritem + statep->items[i];
        (setq sp1 (aref (core-items statep) i)
              sp  sp1)
        (while (> (aref ritem sp) 0)
          (setq sp (1+ sp)))

        (setq rule (- (aref ritem sp)))
        (wisent-log "    %s  ->  " (aref tags (aref rlhs rule)))
        
        (setq sp (aref rrhs rule))
        (while (< sp sp1)
          (wisent-log "%s " (aref tags (aref ritem sp)))
          (setq sp (1+ sp)))
        (wisent-log ".")
        (while (> (aref ritem sp) 0)
          (wisent-log " %s" (aref tags (aref ritem sp)))
          (setq sp (1+ sp)))
        (wisent-log "   (rule %d)\n" rule)
        (setq i (1+ i)))
      (wisent-log "\n"))))

(defun wisent-print-state (state)
  "Print information on STATE."
  (wisent-log "\n\nstate %d\n\n" state)
  (wisent-print-core state)
  (wisent-print-actions state))

(defun wisent-print-results ()
  "Print information on generated parser."
  ;; Always report conflicts
  (if any-conflicts
      (wisent-print-conflicts))
  ;; Report more information if verbose option set
  (when (or wisent-verbose-flag wisent-debug-flag)
    (wisent-print-grammar)
    (let ((i 0))
      (while (< i nstates)
        (wisent-print-state i)
        (setq i (1+ i)))))
  ;; Append output to log file when running in batch mode
  (when (wisent-noninteractive)
    (wisent-append-to-log-file)
    (wisent-clear-log)))

;;;; ---------------------------------
;;;; Build the generated parser tables
;;;; ---------------------------------

(defun wisent-action-row (state actrow)
  "Figure out the actions for the specified STATE.
Decide what to do for each type of token if seen as the lookahead
token in specified state.  The value returned is used as the default
action for the state.  In addition, ACTROW is filled with what to do
for each kind of token, index by symbol number, with nil meaning do
the default action.  The value 'error, means this situation is an
error.  The parser recognizes this value specially.

This is where conflicts are resolved.  The loop over lookahead rules
considered lower-numbered rules last, and the last rule considered
that likes a token gets to handle it."
  (let (i j k m n v default-rule nreds rule max count
          shift-state symbol redp shiftp errp nodefault)
    
    (fillarray actrow nil)
  
    (setq default-rule 0
          nodefault nil ;; nil inhibit having any default reduction
          nreds 0
          m 0
          n 0
          redp (aref reduction-table state))
  
    (when redp
      (setq nreds (reductions-nreds redp))
      (when (>= nreds 1)
        ;; loop over all the rules available here which require
        ;; lookahead
        (setq m (aref lookaheads state)
              n (aref lookaheads (1+ state))
              i (1- n))
        (while (>= i m)
          ;; and find each token which the rule finds acceptable to
          ;; come next
          (setq j 0)
          (while (< j ntokens)
            ;; and record this rule as the rule to use if that token
            ;; follows.
            (if (wisent-BITISSET (aref LA i) j)
                (aset actrow j (- (aref LAruleno i)))
              )
            (setq j (1+ j)))
          (setq i (1- i)))))
    
    ;; Now see which tokens are allowed for shifts in this state.  For
    ;; them, record the shift as the thing to do.  So shift is
    ;; preferred to reduce.
    (setq shiftp (aref shift-table state))
    (when shiftp
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (setq shift-state (aref v i))
        (if (zerop shift-state)
            nil ;; continue
          (setq symbol (aref accessing-symbol shift-state))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            (aset actrow symbol shift-state)
            ;; Do not use any default reduction if there is a shift
            ;; for error
            (if (= symbol error-token-number)
                (setq nodefault t))))
        (setq i (1+ i))))

    ;; See which tokens are an explicit error in this state (due to
    ;; %nonassoc).  For them, record error as the action.
    (setq errp (aref err-table state))
    (when errp
      (setq k (errs-nerrs errp)
            v (errs-errs errp)
            i 0)
      (while (< i k)
        (aset actrow (aref v i) wisent-error-tag)
        (setq i (1+ i))))

    ;; Now find the most common reduction and make it the default
    ;; action for this state.
    (when (and (>= nreds 1) (not nodefault))
      (if (aref consistent state)
          (setq default-rule (- (aref (reductions-rules redp) 0)))
        (setq max 0
              i m)
        (while (< i n)
          (setq count 0
                rule (- (aref LAruleno i))
                j 0)
          (while (< j ntokens)
            (if (and (numberp (aref actrow j))
                     (= (aref actrow j) rule))
                (setq count (1+ count)))
            (setq j (1+ j)))
          (if (> count max)
              (setq max count
                    default-rule rule))
          (setq i (1+ i)))
        ;; actions which match the default are replaced with zero,
        ;; which means "use the default"
        (when (> max 0)
          (setq j 0)
          (while (< j ntokens)
            (if (and (numberp (aref actrow j))
                     (= (aref actrow j) default-rule))
                (aset actrow j nil))
            (setq j (1+ j)))
          )))

    ;; If have no default rule, if this is the final state the default
    ;; is accept else it is an error.  So replace any action which
    ;; says "error" with "use default".
    (when (zerop default-rule)
      (if (= final-state state)
          (setq default-rule wisent-accept-tag)
        (setq j 0)
        (while (< j ntokens)
          (if (eq (aref actrow j) wisent-error-tag)
              (aset actrow j nil))
          (setq j (1+ j)))
        (setq default-rule wisent-error-tag)))
    default-rule))

(defconst wisent-default-tag 'default
  "Tag used in an action table to indicate a default action.")

(defun wisent-token-actions ()
  "Figure out the actions for every state.
Return the action table."
  (working-dynamic-status "(building token actions)")
  (let (i j action-table actrow)
    (setq action-table (make-vector nstates nil)
          actrow (make-vector ntokens nil)
          i 0)
    (while (< i nstates)
      (aset action-table i (list (cons wisent-default-tag
                                       (wisent-action-row i actrow))))
      (setq j 0)
      (while (< j ntokens)
        (if (aref actrow j)
            (aset action-table i (cons (cons j (aref actrow j))
                                       (aref action-table i))))
        (setq j (1+ j)))
      (aset action-table i (nreverse (aref action-table i)))
      (setq i (1+ i)))
    action-table))

(defun wisent-goto-actions ()
  "Figure out what to do after reducing with each rule.
Depending on the saved state from before the beginning of parsing the
data that matched this rule.  Return the goto table."
  (working-dynamic-status "(building goto actions)")
  (let (i j m n symbol state goto-table)
    (setq goto-table (make-vector nstates nil)
          i ntokens)
    (while (< i nsyms)
      (setq symbol (- i ntokens)
            m (aref goto-map symbol)
            n (aref goto-map (1+ symbol))
            j m)
      (while (< j n)
        (setq state (aref from-state j))
        (aset goto-table state (cons (cons i (aref to-state j))
                                     (aref goto-table state)))
        (setq j (1+ j)))
      (setq i (1+ i)))
    goto-table))

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

(defun wisent-semantic-action-expand-body (expr &optional phlds)
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
          (setq sexpr (wisent-semantic-action-expand-body sexpr phlds)
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

(defun wisent-semantic-action (r)
  "Return an Elisp function from the semantic action body in rule R.

An semantic action function receives three arguments:

- the state/value stack
- the top-of-stack index
- the goto table

And returns the updated top-of-stack index."
  (if r
      (let* ((nt/rh (car r))
             (xa    (wisent-semantic-action-expand-body (cdr r)))
             ($l    (car xa))           ; $<i> found in action
             (expr  (cdr xa))           ; expanded form of action
             (nt    (car nt/rh))        ; reduced nonterminal ID
             (rh    (cdr nt/rh))        ; right hand side nt IDs
             (n     (length rh))        ; number of values in stack
             (vi    n)                  ; $<i> index
             (spi   1)                  ; $<i> value index in stack
             (rl    nil)                ; list of binded $region<i>
             (bl    nil)                ; let* binding list
             (i     1)
             $i $ri)
        (while (<= i n)
          (setq $i  (intern (format "$%d" vi))
                $ri (intern (format "$region%d" vi))
                rl  (cons $ri rl)
                bl  (cons `(,$ri (cdr (aref stack (- sp ,spi)))) bl))
          (if (memq $i $l) ;; Only bind $<i> if used in action
              (setq bl (cons `(,$i (car (aref stack (- sp ,spi)))) bl)))
          (setq i   (1+ i)
                vi  (1- vi)
                spi (+ spi 2)))
        (setq bl `(,@bl
                   ($region (wisent-region ,@rl))
                   ($nterm  ',(aref tags nt))))
        `(lambda (stack sp gotos)
           (let* ,bl
             (wisent-push stack (- sp ,(1- spi)) ,nt gotos
                          (cons ,expr $region))))
        )))

(defun wisent-byte-compile-semantic-actions (table)
  "Byte compile semantic actions in TABLE.
This significantly improves the performance of the parser!"
  (let ((i 1)
        (n (length table)))
    (while (< i n)
      (aset table i (byte-compile (aref table i)))
      (setq i (1+ i)))))

(defun wisent-semantic-actions ()
  "Turn semantic actions into Lisp functions.
Return the semantic action table."
  (working-dynamic-status "(building semantic actions)")
  (let ((table (vconcat (mapcar #'wisent-semantic-action rule-table))))
    (unless wisent-debug-flag
      ;; Byte-compile the semantic action functions
      (working-dynamic-status "(byte-compiling semantic actions)")
      (wisent-byte-compile-semantic-actions table))
    table))

(defun wisent-token-translations ()
  "Build the translation table.
This table maps tokens into item numbers."
  (working-dynamic-status "(building token translations)")
  (let (i translate)
    (setq i ntokens)
    (while (> i 0)
      (setq i (1- i)
            translate (cons (cons i (aref tags i)) translate)))
    translate))

;;;; -------------------
;;;; Build parser tables
;;;; -------------------

(defun wisent-parser-automaton ()
  "Compute and return LALR automaton from GRAMMAR.
GRAMMAR is in internal format.  GRAM/ACTS are grammar rules
in internal format.  STARTS defines the start symbols."
  (let (tables)
    (wisent-set-derives)
    (wisent-set-nullable)
    ;; convert to nondeterministic finite state machine.
    (wisent-generate-states)
    ;; make it deterministic.
    (wisent-lalr)
    ;; Find and record any conflicts: places where one token of
    ;; lookahead is not enough to disambiguate the parsing.  Also
    ;; resolve s/r conflicts based on precedence declarations.
    (wisent-resolve-conflicts)
    (wisent-print-results)
    
    (setq tables
          (vector (wisent-token-actions)
                  (wisent-goto-actions)
                  (wisent-semantic-actions)
                  (wisent-token-translations)
                  start-table))
    
    ))

;;;; -------------------
;;;; Parse input grammar
;;;; -------------------

(wisent-defcontext item-numbers
  var-no    ;; Must be initialised with ntokens
  token-no) ;; Must be initialised with 0

(defsubst wisent-item-number (x)
  "Return the item number of symbol X."
  (get x 'wisent--item-no))

(defun wisent-clear-item-number (x)
  "Clear the item number of symbol X."
  (put x 'wisent--item-no nil))

(defun wisent-set-token-item-number (x)
  "Set up the item number of terminal symbol X."
  (or (wisent-item-number x)
      (progn
        (put x 'wisent--item-no token-no)
        (setq token-no (1+ token-no)))))

(defun wisent-set-var-item-number (x)
  "Set up the item number of nonterminal symbol X."
  (or (wisent-item-number x)
      (progn
        (put x 'wisent--item-no var-no)
        (setq var-no (1+ var-no)))))

(defun wisent-set-item-numbers ()
  "Set up the item numbers.
Keep symbols in the TAGS vector so that TAGS[I] is the symbol
associated to item number I."
  (setq rprec   nil
        ntokens (length token-list)
        nvars   (length var-list)
        nsyms   (+ ntokens nvars)
        tags    (vconcat token-list var-list))
  (wisent-with-context item-numbers
    (setq var-no ntokens
          token-no 0)
    ;; Clear item numbers
    (mapcar #'wisent-clear-item-number tags)
    ;; Setup item numbers
    (mapcar #'wisent-set-token-item-number token-list)
    (mapcar #'wisent-set-var-item-number var-list)))

(defconst wisent-reserved-symbols (list wisent-error-term)
  "The list of reserved symbols.
Also all symbol starting with '$' are reserved for internal use.")

(defmacro wisent-ISVALID (x)
  "Return non-nil if X is an allowed symbol."
  `(and ,x
        (symbolp ,x)
        (not (char-equal (aref (symbol-name ,x) 0) ?\$))
        (not (memq ,x wisent-reserved-symbols))))

(defsubst wisent-default-semantic-action-body (rhs-length)
  "Return a default semantic action body.
RHS-LENGTH is the number of symbols in the right hand side of the
rule.  The default body returns nil for an empty rule or the value
of the first symbol in the rule, that is $1."
  (if (> rhs-length 0)
      '$1
    '()))

(defun wisent-parse-nonterminal (def)
  "Parse nonterminal definition DEF.
On entry DEF has the form:  (NONTERM . RULES)

where RULES is a list of elements of the form:

  (MATCHINGS [PREC-TERM] [ACTION])

where MATCHINGS is the list of terminals and non terminals to match.
Optional value PREC-TERM is a vector of one element: the terminal
symbol from which the rule gets its precedence level.  And optional
value ACTION is a semantic action statement.
Return an internal form."
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
            prod  (mapcar #'wisent-item-number (cons nonterm rhs))
            items rhs)
      (while items
        (setq item  (car items)
              items (cdr items))
        (or (memq item token-list)
            (memq item var-list)
            (error "Invalid terminal or nonterminal %s" item)))
      
      ;; Rule has precedence level
      (if (vectorp (car rest))
          (progn
            (setq item (car rest))
            (or (and (= (length item) 1) (memq (aref item 0) token-list))
                (error "Invalid rule precedence level %S" item))
            (setq rprec (cons (get (aref item 0) 'wisent--prec)
                                      rprec)
                  rest (cdr rest)))
        ;; No precedence level
        (setq rprec (cons nil rprec)))

      ;; Rule has semantic action
      (if rest
          (progn
            (or (null (cdr rest))
                (error "Invalid rule semantic action %S" rest))
            (setq prod/acts (cons (cons prod (car rest)) prod/acts)))
        (setq prod/acts (cons
                         (cons prod
                               (wisent-default-semantic-action-body
                                (length rhs)))
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

(defconst wisent-starts-nonterm '$STARTS
  "Main start symbol.
It gives the rules for start symbols.")

(defun wisent-parse-grammar (grammar &optional start-list)
  "Parse GRAMMAR and build a suitable internal representation.
Optional argument START-LIST is a list of extra start symbols.
GRAMMAR is a list of form: (TOKENS ASSOCS . NONTERMS) where TOKENS is
a list of terminal symbols (tokens), ASSOCS is a list describing the
associativity of terminals or nil.  And NONTERMS is the list of non
terminal definitions (see function `wisent-parse-nonterminal').
ASSOCS is a list of elements: (ASSOC-TYPE . ASSOC-TOKENS) where
ASSOC-TYPE is one of 'nonassoc, 'left or 'right, and ASSOC-TOKENS is a
list of tokens which must have been declared in TOKENS."
  (working-dynamic-status "(parse input grammar)")
  (or (and (consp grammar) (> (length grammar) 2))
      (error "Invalid grammar definition"))
  (let (i r nt pl rhs pre lst lastprec a-type a-tokens
          gram ep-token token tokens ep-var var vars ep-def def defs)
    ;; terminals
    (setq lst (car grammar))
    (while lst
      (setq token (car lst)
            lst  (cdr lst))
      (or (wisent-ISVALID token)
          (error "Invalid terminal symbol %s" token))
      (if (memq token tokens)
          (error "Terminal previously defined %s" token))
      ;; Cleanup precedence and associativity
      (put token 'wisent--prec  nil)
      (put token 'wisent--assoc nil)
      (setq tokens (cons token tokens)))

    ;; assocativity
    (setq lst (nth 1 grammar)
          lastprec 0)
    (while lst
      (setq def      (car lst)
            a-type   (car def)
            a-tokens (cdr def)
            lst      (cdr lst)
            lastprec (1+ lastprec))
      (or (memq a-type '(left right nonassoc))
          (error "Invalid associativity type %s" a-type))
      (while a-tokens
        (setq token    (car a-tokens)
              a-tokens (cdr a-tokens))
        (or (memq token tokens)
            (error "Invalid associativity, terminal %s undefined" token))
        (if (memq token defs)
            (error "Duplicate associativity for terminal %s" token))
        (setq defs (cons token defs))
        ;; Record the precedence and associativity of the terminal in
        ;; respectively the `wisent--prec' and `wisent--assoc' symbol
        ;; properties.
        (put token 'wisent--prec  lastprec)
        (put token 'wisent--assoc a-type)))
    
    ;; nonterminals
    (setq lst  (nthcdr 2 grammar)
          defs nil)
    (while lst
      (setq def (car lst)
            lst (cdr lst))
      (or (consp def)
          (error "Nonterminal definition must be a non-empty list"))
      (setq var (car def))
      (or (wisent-ISVALID var)
          (error "Invalid nonterminal symbol %s" var))
      (if (or (memq var tokens) (assq var defs))
          (error "Nonterminal previously defined %s" var))
      (setq defs (cons def defs)))
    (if (= (length defs) 0)
        (error "Grammar must contain at least one nonterminal"))
    
    ;; Parse start symbols.
    ;; STARTS is a list of symbols '(nt0 ... ntN).
    ;; Build and push start rules in the grammar.  That is
    ;; something like this:
    ;;
    ;; ($STARTS ((nt0) $1) ... ((ntN) $1))
    ;;
    ;; ($nt1    (($$nt1 nt1) $2))
    ;; ...
    ;; ($ntN    (($$ntN ntN) $2))

    (setq defs   (nreverse defs)
          lst    (nreverse (cons (caar defs) start-list))
          start-table nil)
    (while lst
      (setq var (car lst)
            lst (cdr lst))
      (or (assq var defs)
          (error "Start symbol %s not found" var))
      (or (assq var start-table) ;; Ignore duplicates
          ;; For each nt start symbol
          (setq ep-var   (make-symbol (format "$%s"  var))
                ep-token (make-symbol (format "$$%s" var))
                tokens   (cons ep-token tokens)
                ;; Add entry (nt . $$nt) to start-table
                start-table (cons (cons var ep-token) start-table)
                ;; Add rule ($nt (($$nt nt) $2))
                defs (cons (list ep-var (list (list ep-token var) '$2))
                           defs)
                ;; Add start rule (($nt) $1)
                ep-def (cons (list (list ep-var) '$1) ep-def))))
    
    (setq defs (cons (cons wisent-starts-nonterm ep-def) defs)
          tokens (nreverse (cons wisent-error-term tokens))
          token-list (cons wisent-eoi-term tokens)
          var-list (mapcar #'car defs))
    
    ;; Concoct an ad hoc internal representation replacing symbols by
    ;; item numbers
    (wisent-set-item-numbers)
    (setq error-token-number (wisent-item-number wisent-error-term)
          start-symbol       (wisent-item-number (car var-list)))
          
    (setq vars (mapcar #'wisent-parse-nonterminal defs)
          gram (mapcar #'wisent-grammar-production vars))
          
    (setq rprec      (vconcat (cons nil (nreverse rprec)))
          rule-table (vconcat (cons nil (apply #'nconc vars)))
          nrules     (1- (length rule-table))
          nitems     0)
    
    ;; Count items
    (setq r 1)
    (while (<= r nrules)
      (setq nitems (+ nitems (length (car (aref rule-table r))))
            r (1+ r)))
    
    ;; Set up RLHS RRHS & RITEM data structures
    (setq rlhs  (make-vector (1+ nrules) nil)
          rrhs  (make-vector (1+ nrules) nil)
          ritem (make-vector (1+ nitems) nil)
          i 0
          r 1)
    (while gram
      (setq nt (caar gram)
            pl (cdar gram))
      (while pl
        (aset rlhs r nt)
        (aset rrhs r i)
        (setq rhs (car pl)
              pre nil)
        (while rhs
          ;; Get default precedence level of rule, that is the
          ;; precedence of the last terminal in it.
          (if (wisent-ISTOKEN (car rhs))
              (setq pre (get (aref tags (car rhs)) 'wisent--prec)))
          
          (aset ritem i (car rhs))
          (setq i (1+ i)
                rhs (cdr rhs)))
        ;; Setup the precedence level of the rule, that is the one
        ;; specified by %prec or the default one.
        (or (aref rprec r) ;; Already set by %prec
            (aset rprec r pre))
        (aset ritem i (- r))
        (setq i (1+ i)
              r (1+ r)
              pl (cdr pl)))
      (setq gram (cdr gram)))
    ))

(defalias 'wisent-compiled-grammar-p 'vectorp)

;;;###autoload
(defun wisent-compile-grammar (grammar &optional start-list)
  "Compile GRAMMAR and return the tables needed by the parser.
Optional argument START-LIST is a list of extra start symbols
\(nonterminals).  The first nonterminal defined in the grammar is
always the default start symbol.

Return an LALR automaton of the form:

\[ACTIONS GOTOS FUNCTIONS TRANSLATE STARTS]

- ACTIONS a state/token matrix telling the parser what to do at every
  state based on the current lookahead token.  That is shift, reduce,
  accept or error.

- GOTOS a state/nonterminal matrix telling the parser the next state
  to go to after reducing with each rule.

- FUNCTIONS a vector of semantic actions.  A semantic action is
  actually an Elisp function (lambda expression).  By default these
  functions are byte-compiled to achieve best performance of the
  parser.

- TRANSLATE an alist which maps tokens returned by lexical analysis
  into item numbers used internally.

- STARTS an alist which maps the allowed start symbols (nonterminal)
  to tokens that will be first shifted into the parser stack."
  
  (if (wisent-compiled-grammar-p grammar)
      grammar ;; Grammar already compiled just return it
    (wisent-with-context compile-grammar
      (let* ((working-status-dynamic-type #'working-text-display)
             (gc-cons-threshold 1000000)
             automaton)
        (garbage-collect)
        (working-status-forms "Compiling grammar" "done"
          (setq wisent-new-log-flag t)
          ;; Parse input grammar
          (wisent-parse-grammar grammar start-list)
          ;; Generate the LALR automaton
          (setq automaton (wisent-parser-automaton))
          (working-dynamic-status t)
          automaton)))))

(provide 'wisent-comp)

;;; wisent-comp.el ends here
