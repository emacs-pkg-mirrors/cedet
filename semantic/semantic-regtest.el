;;; semantic-regtest.el --- Perform regression tests for grammars

;;; Copyright (C) 2003 Klaus Berndl

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: syntax test
;; X-RCS: $Id: semantic-regtest.el,v 1.4 2003/11/20 04:11:34 zappo Exp $

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
;; This library implements regression-tests for testing grammars and parsers
;; of semantic.
;;
;; This library offers:
;;
;; 1. Commands to run regression tests for grammar/parser tests. See the
;;    commands
;;    - `semantic-regtest-run-test'
;;    - `semantic-regtest-create-output'
;;    - `semantic-regtest-cmp-results'
;;    for a first description what this library can do with this respect.
;;
;;    It should not be hard to run these functions from within a Makefile to
;;    run all regression-tests in batch-mode - e.g. before releasing a new
;;    release.
;;
;; 2. A new major-mode `semantic-regtest-mode' which is added to the
;;    `auto-mode-alist' for files ending with "*.regtest.result' (e.g. the
;;    command `semantic-regtest-run-test' creates autom. a result-file with
;;    such a extension). This new major-mode makes a lot of stuff in the
;;    result-file clickable - for details and keybindings see
;;    `semantic-regtest-mode'.
;;
;;
;; Currently this code is tested with GNU Emacs 21.2 and CVS semantic v1p4

;;; TODO:
;;
;;  - testing with XEmacs
;;  - writing a new token-output-function which is used instead of
;;    `semantic-prin1-nonterminal' and which produces a
;;    token-structure-independent output-string so it can be used to run with
;;    semantic 1.4.X and semantic 2.X and so test-runs between different
;;    semantic-versions are possible.
;;  - defining some constants, e.g. for the separtor-string " |###| " and some
;;    other currently hard coded stuff.
;;  - maybe using another parent-major-mode instead of `view-mode'?
;;  - testing when driven by a Makefile
;;  - testing with other code than c++, e.g. java, elisp....

;;; Code

(require 'semantic)

(defgroup semantic-regtest nil
  "Settings for semantic grammar/parser regression-tests."
  :group 'semantic
  :prefix "semantic-regtest-")

(defcustom semantic-regtest-functions
  '(semantic-regtest-prin1-nonterminal)
  "*Functions used for the grammar/parser regression-test.

Every element must be a function which gets one token-argument and must return
a string which is the printed information about this token.

If nil then always `semantic-regtest-prin1-nonterminal' is used."
  :group 'semantic-regtest
  :type '(repeat (function :tag "Regression-test function")))

(defcustom semantic-regtest-highlight-token t
  "*Highlight token in the source-file.

This highlights the token jumped to by `semantic-regtest-open-source-file' or
`semantic-regtest-mouse-open-source-file'."
  :group 'semantic-regtest
  :type 'boolean)

(defcustom semantic-regtest-find-file-function 'find-file-other-window
  "*Displayfunction for the files of `semantic-regtest-mode'.

This function is used to display a file in a window if one of the commands of
`semantic-regtest-mode' is used. The function gets one argument - a filename -
and has to display this file in a window.

Default is `find-file-other-window'."
  :group 'semantic-regtest
  :type 'function)

(defface semantic-regtest-test-button-face
  '((((class color) (background dark))
     (:forground "blue" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t)))
  "*Face used to show clickable buttons for the test files.
This can be the source-file and the test output file."
  :group 'semantic-regtest)

(defface semantic-regtest-reference-button-face
  '((((class color) (background dark))
     (:forground "ForestGreen" :bold t))
    (((class color) (background light))
     (:foreground "ForestGreen" :bold t)))
  "*Face used to show clickable buttons for the reference file."
  :group 'semantic-regtest)


(defun semantic-regtest-run-test (test-source-file)
  "Run a regression test for TEST-SOURCE-FILE and opens the result in another
window. If the regression-tests fails - i.e. if there are differences to the
reference-file - then the result will be displayed in another window with
active `semantic-regtest-mode'.

`semantic-regtest-run-test' is a regression test function which uses all the
utility functions of this library to run a regression test for a source-file.
The function assumes the following dir- and file-structure:
- all files reside in the same subdir
- Name of the reference output-file: TEST-SOURCE-FILE.reference.output
  (Must already be generated with `semantic-regtest-create-output'!)
- Name of the test output-file: TEST-SOURCE-FILE.regtest.output
  (Will be generated with `semantic-regtest-create-output')
- Name of the result file of the test: TEST-SOURCE-FILE.regtest.result (Will
  be generated with `semantic-regtest-cmp-results' by comparing
  TEST-SOURCE-FILE.regtest.output with TEST-SOURCE-FILE.reference.output

Example for test.cpp:
- Reference output-file: test.cpp.reference.output
- Test output-file: test.cpp.regtest.output
- Result file of the regression-test: test.cpp.regtest.result

The format of the file TEST-SOURCE-FILE.regtest.result is described in
`semantic-regtest-cmp-results'. Also how to interpret and use the file
TEST-SOURCE-FILE.regtest.result."
  (interactive "FTest Sourcefile: ")
  
  (let* ((test-file (expand-file-name test-source-file))
         (ref-output-file (concat test-file ".reference.output"))
         (test-output-file (concat test-file ".regtest.output"))
         (result-file (concat test-file ".regtest.result")))
    ;; opening the test source-file
    (save-excursion
      (set-buffer (find-file-noselect test-file))
      ;; generating the output of the grammar/parser test
      (semantic-regtest-create-output test-output-file))
    ;; comparing with the reference output and writing a result-file.
    (if (semantic-regtest-cmp-results test-file test-output-file
                                      ref-output-file result-file)
        ;; now opening the result file in `semantic-regtest-mode'
        (find-file-other-window result-file)
      (message "Regressiontest succeeds - no differences to the reference-file!"))))


  
(defun semantic-regtest-create-output (file &optional with-location-info)
  "Runs the functions in `semantic-regtest-functions' \(if nil then
`semantic-prin1-nonterminal' is used) on every token in current buffer and
writes the output to FILE. This gives a regression-able test of a
grammar/parser because you can run this function on a testfile F before
grammar-changes and after grammar-changes and compare the two output-files
with diff.

IMPORTANT: ALL information about a token is written in ONE line. This is for
better comparsion with line-oriented tools like diff. The format of a line is:

  <token-name> |###| <token-type> |###| <full token-text> |###|
     <output of print-function-1> |###| <output of print-function-2> |###|
     ... |###|

whereas <output of print-function-1> is \"<print-function-1>: <print-text>\"

Return the number of tokens.

Normally the position-informations in form of the #<overlay from X to Y> are
filtered out by this function so the parsing check is independent from
changing whitespace or comments in the testfiles - which would not changing
the token-data itself but the data-locations. But if WITH-LOCATION-INFO is not
nil \(i.e. if called with a prefix arg) the location-informations of the
token-data are preserved also in the outputfile - in form of
\[<token-start> <token-end>]."
  (interactive "FInsert file-name for test-output: \nP")
  (goto-char (point-min))
  (let ((buf (get-buffer-create "*Semantic regression test*"))
        (test-functions (or semantic-regtest-functions
                            '(semantic-prin1-nonterminal)))
        (token-counter 0)
        token token-extend token-text output-str)

    ;; clean the output buffer
    (save-excursion
      (set-buffer buf)
      (erase-buffer))
    
    ;; reparse the whole source-buffer
    (semantic-bovinate-toplevel t)

    ;; print out the token informations of all tokens. IMPORTANT: ALL
    ;; information about a token is written in ONE line. This is for better
    ;; comparsion with line-oriented tools like diff.
    ;; The format of a line is:
    ;; <token-name> |###| <full token-text> |###| <output of print-function-1>
    ;;    |###| <output of print-function-2> |###| ... |###|
    ;; whereas <output of print-function-1> is "<print-function-1>: <print-text>"
    ;; (all in one single line without linebreaks!)
    
    (while (setq token (semantic-find-nonterminal-by-overlay-next))
      (setq token-counter (1+ token-counter))
      (if (not (semantic-token-with-position-p token))
          (setq token-text "This is a positionless token")
        (setq token-extend (semantic-token-extent token))
        (setq token-text (buffer-substring-no-properties (nth 0 token-extend)
                                                         (nth 1 token-extend))))
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should use a more
      ;; unique separator than " |###| "
      (setq output-str (format "%s |###| %s |###| %s |###|"
                               (semantic-token-name token)
                               (symbol-name (semantic-token-token token))
                               (subst-char-in-string ?\n 32 token-text)))
      (dolist (fnc test-functions)
        (setq output-str
              (concat output-str (format " %s: %s |###|"
                                         (symbol-name fnc)
                                         (funcall fnc token)))))
      (save-excursion
        (set-buffer buf)
        (goto-char (point-max))
        (insert output-str)
        (insert "\n"))
      (goto-char (semantic-token-start token)))

    ;; write the generated token-informations into FILE
    (save-excursion
      (set-buffer buf)
      ;; maybe removing the overlay-positions
      (goto-char (point-min))
      (if with-location-info
          (while (re-search-forward
                  "#<overlay from \\([0-9]+\\) to \\([0-9]+\\) in [^>]+>"
                  nil t)
            (replace-match "[\\1 \\2]"))
        (while (re-search-forward "#<overlay from [0-9]+ to [0-9]+ in [^>]+>"
                                  nil t)
          (replace-match "[Location info filtered out]")))
      (write-region (point-min) (point-max) file))

    ;; clean up
    (kill-buffer buf)
    (goto-char (point-min))

    ;; return number of printed tokens
    token-counter))

(defun semantic-regtest-convert-difference (buffer start end)
  "Parse the diff-difference located in BUFFER between START and END. Cause of
the facts that each line in the output of `semantic-regtest-create-output'
represents exactly one token and \[START, END] always define a
set of complete lines of BUFFER \(and therefore a set of token-outputs) the
text between START and END can be splitted in lines and each of these lines is
splitted by the separator \" |###| \".

Result is either nil \(if START = END) or a list of sublists whereas each
sublist represents one line resp. token between START and END and consist
therefore of the following elements:
0. token-number of token in the test-file (= line-number in the test-file)
1. name of the token
2. type of the token \(function, variable, type, include etc...)
3. the complete token text
4. the token-string of the first token-print-function. This string looks like
   \"<print-function>: <print-output>\", e.g. \"semantic-prin1-nonterminal:
   \(\\\"c++-test.hh\\\" include nil nil nil \[Location info filtered out])\"
   \(all output of a token is in one line - no linebreaks!)
5. the token-string of the second token-print-function
6. ...
If a list then every sublist contains at least 5 elements \(0. to 4.)."
  (and (not (= start end))
       (save-excursion
         (set-buffer buffer)
         (let ((line-list (split-string (buffer-substring-no-properties start
                                                                        end)
                                        "\n"))
               (line-counter (1+ (count-lines (point-min) start)))
               result)
           (dolist (line line-list)
             (setq result
                   (cons
                    (append (list line-counter)
                            (split-string line " |###| ?"))
                    result))
             (setq line-counter (1+ line-counter)))
           (nreverse result)))))

;; The following two function are examples how to print the data of one
;; diff-difference (can contain data for more than 1 line (resp. token)!).
(defun semantic-regtest-1-diffdata2str (diff-data file &optional prefix)
  "Convert the data of DIFF-DATA into a suitable string-representation where
each element of DIFF-DATA is separated by a newline within this string. PREFIX
is the prefix for each line if a string."
  (let ((output-str nil))
    (dolist (elem diff-data output-str)
      (setq output-str
            (concat output-str
                    (format "%s%s (token-type: %s, [%d. token of %s file])\n"
                            (or prefix
                                "")
                            (nth 1 elem) (nth 2 elem) (nth 0 elem) file))))))

(defun semantic-regtest-2-diffdata2str (a-diff-data b-diff-data
                                                    &optional prefix)
  "Convert the data of A-DIFF-DATA into a suitable string-representation by
comparing each elem of A-DIFF-DATA with the related elem of B-DIFF-DATA where
each element of A-DIFF-DATA is printed by two lines whereas the first line
contains the token-name of the A-DIFF-DATA-elem and the token-numbers and the
second line contains the kind of difference between the two elements \(
different token-name, token-type, token-text and/or token-output). PREFIX is
the prefix for the first line of such a two-line-block - the second line gets
a prefix with same length as PREFIX but filled with spaces.

If the length of A-DIFF-DATA and B-DIFF-DATA is unequal then an error is
reported."
  (if (not (= (length a-diff-data) (length b-diff-data)))
      (error "Can not compare diff-lists with unequal length!")
    (let ((b-diff-data-copy b-diff-data)
          str)
      (dolist (elem a-diff-data str)
        (setq str
              (concat str
                      (format "%s%s (type: %s, [%d. token of test file], [%d. token of reference file])\n"
                              (or prefix
                                  "")
                              (nth 1 elem)
                              (nth 2 elem)
                              (nth 0 elem)
                              (nth 0 (car b-diff-data-copy)))
                      (format "%s%s%s%s%s\n"
                              (make-string (length prefix) 32)
                              (if (not (string= (nth 1 elem)
                                                (nth 1 (car b-diff-data-copy))))
                                  "Different token-name, "
                                "")
                              (if (not (string= (nth 2 elem)
                                                (nth 2 (car b-diff-data-copy))))
                                  "Different token-type, "
                                "")
                              (if (not (string= (nth 3 elem)
                                                (nth 3 (car b-diff-data-copy))))
                                  "Different token-text, "
                                "")
                              (if (not (string= (nth 4 elem)
                                                (nth 4 (car b-diff-data-copy))))
                                  "Different token-output"
                                ""))))
        (setq b-diff-data-copy (cdr b-diff-data-copy))))))

;; this is the only function where ediff-stuff is used!
(defun semantic-regtest-ediff (file-a file-b)
  "Run ediff noninteractively to compare FILE-A and FILE-B. The result
is is list with contains for every difference between FILE-A and FILE-B a
vector: \[a-start a-end b-start b-end nil nil nil nil nil nil nil]

What is the \"semantic\" of such a difference-result-vector:

If \(a-start = a-end) Then lines \(= tokens) between b-start and b-end of
                          FILE-B are missed in FILE-A
ElseIf \(b-start = b-end) Then lines \(= tokens between a-start and a-end are
                              new in FILE-A (missed in the FILE-B) 
Else lines \(= tokens between a-start and a-end are parsed differently.

If there are no differences between FILE-A and FILE-B then nil is returned."      
  (require 'ediff)
  ;; we must set ediff-buffer-A, ediff-buffer-B and ediff-buffer-C because
  ;; these buffers are needed by ediff to work
  (let ((ediff-buffer-A (find-file-noselect (expand-file-name file-a)))
        (ediff-buffer-B (find-file-noselect (expand-file-name file-b)))
        (ediff-buffer-C nil))

    (if (string-match "c" ediff-diff-options)
        (error "Option `-c' is not allowed in `ediff-diff-options'"))

    ;; use some ediff stuff to produce correct differences between test-file
    ;; and ref-file
    (or (and ediff-diff-buffer (buffer-live-p ediff-diff-buffer))
        (setq ediff-diff-buffer
              (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))
    (ediff-make-diff2-buffer ediff-diff-buffer file-a file-b)
    (ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
    (cdr (ediff-extract-diffs ediff-diff-buffer nil nil))))


(defun semantic-regtest-cmp-results (source-file
                                     test-file
                                     ref-file
                                     result-file
                                     &optional use-full-path-name)
  "Compare TEST-FILE and REF-FILE and write the results in proper format to
RESULT-FILE. SOURCE-FILE is only used to write it into RESULT-FILE.

Return nil if there no differences between TEST-FILE and REF-FILE otherwise
return not nil.

Format of RESULT-FILE is:

------------------------------------------------------------------------
Semantic grammar/parser regression-test

Source file: SOURCE-FILE
Test output file: TEST-FILE
Reference file: REF-FILE

<Here are listed all token-parsing differences: This can be missed tokens
\(i.e. token which are only in REF-FILE), new tokens \(token which are only in
TEST-FILE) and differently parsed tokens. Each type can occur multiple times
and the sequence follows the original sequence of the differences detected by
the ediff-comparison>
------------------------------------------------------------------------

If USE-FULL-PATH-NAME is nil then these three filesnames are without
path-information because normally all four files \(SOURCE-FILE TEST-FILE
REF-FILE and RESULT-FILE) should reside in the same directory so the path-info
is not needed to open these files from within `semantic-regtest-mode'. If
USE-FULL-PATH-NAME is not nil \(called with a prefix arg) filenames include
full path-info.

How to interpret and use the created RESULT-FILE:
  
For all differences reported in RESULT-FILE the number N of the each missed,
new or differently parsed token is printed out. With this number you can
- use `semantic-regtest-goto-token' to jump to the N-th token in the
  source-file for which TEST-FILE is generated to check the token in the
  source-code
- use `goto-line' to go to the N-th line in either TEST-FILE or REF-FILE to
  check the output of `semantic-regtest-create-output' for this token."

  (interactive "FSource-file: \nFTest output file: \nFReference file: \nFResult file: \nP")
  
  (let ((diff-result (semantic-regtest-ediff test-file ref-file))
        (test-buffer (find-file-noselect (expand-file-name test-file)))
        (ref-buffer (find-file-noselect (expand-file-name ref-file)))
        a-start a-end a-diff-data b-start b-end b-diff-data output-str)
    
    (with-temp-file (expand-file-name result-file)
      (erase-buffer)
      (insert "Semantic grammar/parser regression-test\n\n")
      (insert (format "Source file: [%s]\n"
                      (if use-full-path-name
                          source-file
                        (file-name-nondirectory source-file))))
      (insert (format "Test output file: [%s]\n"
                      (if use-full-path-name
                          test-file
                        (file-name-nondirectory test-file))))
      (insert (format "Reference file: [%s]\n"
                      (if use-full-path-name
                          ref-file
                        (file-name-nondirectory ref-file))))
      (insert "\n\n")
      
      (if (null diff-result)
          (insert "No differences!\n")
        ;; evaluating the ediff-result
        (dolist (diff-elem diff-result)
          (setq a-start (aref diff-elem 0)
                a-end (aref diff-elem 1)                
                a-diff-data (semantic-regtest-convert-difference
                             test-buffer a-start a-end)
                
                b-start (aref diff-elem 2)
                b-end (aref diff-elem 3)
                b-diff-data (semantic-regtest-convert-difference
                             ref-buffer b-start b-end))
          
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: The following is just a
          ;; first example how the output of the test-result could look. Maybe
          ;; it would be useful to print out more data about differences - but
          ;; this is not a problem, because we have all data we need in the
          ;; a-diff-data resp. b-diff-data.
          
          (cond ((null a-diff-data) ;; tokens are missed
                 (setq output-str
                       (concat "These tokens are only in the reference file:\n"
                               (semantic-regtest-1-diffdata2str b-diff-data
                                                                "reference"
                                                                "- "))))
                ((null b-diff-data) ;; tokens are new
                 (setq output-str
                       (concat "These tokens are only in the test file:\n"
                               (semantic-regtest-1-diffdata2str a-diff-data
                                                                "test"
                                                                "+ "))))
                (t ;; token are parsed differently
                 ;; if a-diff-data and b-diff-data contain the same number of
                 ;; elements then we can compare the tokens of a-diff-data and
                 ;; b-diff-data on a pair-basis. Otherwise we simply list the
                 ;; tokens of a-diff-data and then the tokens of b-diff-data.
                 (if (= (length a-diff-data) (length b-diff-data))
                     (setq output-str
                           (concat "These tokens are parsed differently:\n"
                                   (semantic-regtest-2-diffdata2str a-diff-data
                                                                    b-diff-data
                                                                    "* ")))
                   (setq output-str
                         (concat "These token of a the test- and the reference-file are parsed differently:\n"
                                 (semantic-regtest-1-diffdata2str a-diff-data
                                                                  "test"
                                                                  "-t- ")
                                 (semantic-regtest-1-diffdata2str b-diff-data
                                                                  "reference"
                                                                  "-r- "))))))
          
          (insert output-str)
          (insert "\n\n"))))

    ;; clean up
    (kill-buffer test-buffer)
    (kill-buffer ref-buffer)
    diff-result))

(defun semantic-regtest-goto-token (token-number)
  "Jump to the token with number TOKEN-NUMBER in current buffer. Counting
starts always at the beginning of current buffer.

This function can be used for fast and easy jumping to the differences
reported by `semantic-regtest-cmp-results'."
  (interactive "nNumber of token to jump: ")
  (goto-char (point-min))
  (let ((token-counter 0)
        token)
    (while (and (< token-counter token-number)
                (setq token (semantic-find-nonterminal-by-overlay-next)))
      (setq token-counter (1+ token-counter))
      (goto-char (semantic-token-start token)))))


;; ------ code for the new major-mode semantic-regtest-mode -----------------

(defun semantic-regtest-mouse-open-source-file (e)
  "See `semantic-regtest-open-source-file'"
  (interactive "e")
  (mouse-set-point e)
  (semantic-regtest-goto-file 'source))

(defun semantic-regtest-mouse-open-output-file (e)
  "See `semantic-regtest-open-output-file'"
  (interactive "e")
  (mouse-set-point e)
  (semantic-regtest-goto-file 'output))
      
(defun semantic-regtest-open-source-file ()
  "Open the source-file of this button in another window. If the button is a
token-number then jump also to this token."
  (interactive)
  (semantic-regtest-goto-file 'source))

(defun semantic-regtest-open-output-file ()
  "Open the output-file of this button in another window. If the button is a
token-number then jump also to this line in the output-file."
  (interactive)
  (semantic-regtest-goto-file 'output))
      
    
(defun semantic-regtest-goto-file (type)
  "Action function for all clickable buttons in `semantic-regtest-mode'.
TYPE can be one of the symbols `output' or `source'. In case of the former one
it tries to open the right output-file in the other-window and tries to jump
to the right line. In case of the latter one it opens the source-file in the
other window and tries to jump to the right token."
  (let ((file (if (equal type 'output)
                  (or (get-text-property (point)
                                         'semantic-regtest-mode-test-file)
                      (get-text-property (point)
                                         'semantic-regtest-mode-ref-file))
                (get-text-property (point)
                                   'semantic-regtest-mode-source-file)))
        (token-number (ignore-errors
                        (string-to-number
                         (get-text-property
                          (point)
                          'semantic-regtest-mode-token-number)))))
    (when file
      (message "Opening file: %s" (file-name-nondirectory file))
      (funcall semantic-regtest-find-file-function file)
      (when token-number
        (if (equal type 'output)
            (goto-line token-number)
          (semantic-regtest-goto-token token-number)
          (if semantic-regtest-highlight-token
              (semantic-momentary-highlight-token
               (semantic-current-nonterminal))))))))


(defun semantic-regtest-mode-init ()
  "Initializes `semantic-regtest-mode'. This means making all token-numbers
and the source-file, the test output file and the reference file clickable."
  (let ((buffer-read-only nil)
        regtest-mode-source-file
        regtest-mode-test-file
        regtest-mode-ref-file)
    (goto-char (point-min))

    ;; make the 3 files clickable

    (if (re-search-forward "^Source file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-source-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 face
                                 semantic-regtest-test-button-face
                                 semantic-regtest-mode-source-file
                                 ,regtest-mode-source-file)))
      (error "No source file found in the regtest result!"))
    (goto-char (point-min))
    (if (re-search-forward "^Test output file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-test-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 face
                                 semantic-regtest-test-button-face
                                 semantic-regtest-mode-test-file
                                 ,regtest-mode-test-file)))
      (error "No test ouput file found in the regtest result!"))
    (goto-char (point-min))
    (if (re-search-forward "^Reference file: \\[\\(.+\\)\\]$" nil t)
        (progn
          (setq regtest-mode-ref-file (match-string 1))
          (add-text-properties (1- (match-beginning 1))
                               (1+ (match-end 1))
                               `(mouse-face
                                 highlight
                                 face
                                 semantic-regtest-reference-button-face
                                 semantic-regtest-mode-ref-file
                                 ,regtest-mode-ref-file)))
      (error "No reference-file file found in the regtest result!"))

    ;; now make all token-numbers clickable
    
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]+\\)\\. token of test file" nil t)
      (add-text-properties (1- (match-beginning 0))
                           (1+ (match-end 0))
                           `(mouse-face
                             highlight
                             face
                             semantic-regtest-test-button-face
                             semantic-regtest-mode-token-number
                             ,(match-string 1)
                             semantic-regtest-mode-source-file
                             ,regtest-mode-source-file
                             semantic-regtest-mode-test-file
                             ,regtest-mode-test-file))
      )
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]+\\)\\. token of reference file" nil t)
      (add-text-properties (1- (match-beginning 0))
                           (1+ (match-end 0))
                           `(mouse-face
                             highlight
                             face
                             semantic-regtest-reference-button-face
                             semantic-regtest-mode-token-number
                             ,(match-string 1)
                             semantic-regtest-mode-ref-file
                             ,regtest-mode-ref-file))
      )
    (set-buffer-modified-p nil)
    (goto-char (point-min))))
  

(define-derived-mode semantic-regtest-mode
  view-mode "se-re-te"
  "Major mode for viewing result files of semantic regression tests. The main
purpose of this mode is to make all token-numbers and the source-file, the
test output file and the reference file clickable.
\\{semantic-regtest-mode-map}"
  (semantic-regtest-mode-init))

;; mouse-bindings
(define-key semantic-regtest-mode-map
  (if (featurep 'xemacs) '(button1) [mouse-1])
  'semantic-regtest-mouse-open-output-file)

(define-key semantic-regtest-mode-map
  (if (featurep 'xemacs) '(button2) [mouse-2])
  'semantic-regtest-mouse-open-source-file)

;; keyboard bindings:
(define-key semantic-regtest-mode-map
  (kbd "O")
  'semantic-regtest-open-output-file)

(define-key semantic-regtest-mode-map
  (kbd "S")
  'semantic-regtest-open-source-file)


;; adding result-files to the auto-mode-alist
(setq auto-mode-alist
      (append '(("\\.regtest\\.result\\'" . semantic-regtest-mode))
              auto-mode-alist))


;;; Generic format
;;
;; Most tags have data in them unrelated to the details parsed out of
;; a file.  Remove that, and format them in a simple way.

;; some code if this library runs with semantic 1.4
(or (fboundp 'semantic-tag-name)
    (defun semantic-tag-name (tag)
      "See semantic 2.X for a description of this function."
      (car tag)))

(or (fboundp 'semantic-tag-class)
    (defun semantic-tag-class (tag)
      "See semantic 2.X for a description of this function."
      (nth 1 tag)))

(or (fboundp 'semantic-tag-attributes)
    (defun semantic-tag-attributes (tag)
      "See semantic 2.X for a description of this function."
      (nth 2 tag)))

(or (fboundp 'semantic-tag-p)
    (defun semantic-tag-p (tag)
      "See semantic 2.X for a description of this function."
      (and (consp tag)
           (stringp (car tag))
           (symbolp (nth 1 tag)) (nth 1 tag)
           (listp (nth 2 tag))
           (listp (nth 3 tag))
           )))

(or (fboundp 'semantic-tag-make-plist)
    (defun semantic-tag-make-plist (args)
      "See semantic 2.X for a description of this function."
      (let (plist key val)
        (while args
          (setq key  (car args)
                val  (nth 1 args)
                args (nthcdr 2 args))
          (or (null key)
              (overlayp key)
              (overlayp val)
              (member val '("" nil))
              (and (numberp val) (zerop val))
              (setq plist (cons key (cons val plist)))))
        ;; It is not useful to reverse the new plist.
        plist)))

(or (fboundp 'semantic-tag)
    (defun semantic-tag (name class &rest attributes)
      "See semantic 2.X for a description of this function."
      (list name class (semantic-tag-make-plist attributes) nil nil)))

(or (fboundp 'semantic-tag-new-variable)
    (defun semantic-tag-new-variable (name type default-value &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'variable
             :type type
             :default-value default-value
             attributes)))

(or (fboundp 'semantic-tag-new-function)
    (defun semantic-tag-new-function (name type arg-list &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'function
             :type type
             :arguments arg-list
             attributes)))

(or (fboundp 'semantic-tag-new-type)
    (defun semantic-tag-new-type (name type members parents &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'type
             :type type
             :members members
             :superclasses (car parents)
             :interfaces (cdr parents)
             attributes)))

(or (fboundp 'semantic-tag-new-include)
    (defun semantic-tag-new-include (name system-flag &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'include
             :system-flag system-flag
             attributes)))

(or (fboundp 'semantic-tag-new-package)
    (defun semantic-tag-new-package (name detail &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'package
             :detail detail
             attributes)))

(or (fboundp 'semantic-tag-new-code)
    (defun semantic-tag-new-code (name detail &rest attributes)
      "See semantic 2.X for a description of this function."
      (apply 'semantic-tag name 'code
             :detail detail
             attributes)))


(defconst semantic-regtest-new-tag-alist
  '((type     . semantic-tag-new-type)
    (function . semantic-tag-new-function)
    (variable . semantic-tag-new-variable)
    (include  . semantic-tag-new-include)
    (package  . semantic-tag-new-package)
    (code     . semantic-tag-new-code)))

(defun semantic-regtest-convert-tag-14-to-20 (tag)
  (if (semantic-token-p tag)
      (let ((token-name (semantic-token-name tag))
            (tag-creation-fcn (cdr (assoc (semantic-token-token tag)
                                          semantic-regtest-new-tag-alist))))
        (if tag-creation-fcn
            (apply tag-creation-fcn token-name (cdr (cdr tag)))
          ;; there is no predefined tag-creation function, so we use the generic
          ;; one.
          (semantic-tag token-name (semantic-token-token tag)
                        (cdr (cdr tag)))))
    tag))

(defun semantic-regtest-convert-tag-table (table)
  "Convert the tag table TABLE to a generic format."
  (mapcar #'semantic-regtest-convert-tag table))

(defun semantic-regtest--convert-tag (tag)
  "Convert TAG into a generic format.
Recurses over children when they are found."
    (let* ((version-2 (not (semantic-require-version 2 0 1)))
           (normed-tag (or (and version-2
                                tag)
                           (semantic-regtest-convert-tag-14-to-20 tag)))
           (name (semantic-tag-name normed-tag))
           (class (semantic-tag-class normed-tag))
           (attr (semantic-tag-attributes normed-tag))
           (generic nil))
      (while attr
        (let ((sym (car attr))
              (val (if version-2
                       (car (cdr attr))
                     (semantic-regtest-convert-tag-14-to-20 (car (cdr attr))))))
          (cond ((semantic-tag-p val)
                 ;; This attribute is a tag (ie, a type perhaps?)
                 (setq val (semantic-regtest-convert-tag val)))
                ((and (listp val)
                      (semantic-tag-p (if version-2 (car val)
                                        (semantic-regtest-convert-tag-14-to-20 (car val)))))
                 ;; List of more tags in this property.  Children/members
                 (setq val (semantic-regtest-convert-tag-table val)))
                (t nil))
          (setq generic (cons (list sym val) generic))
          (setq attr (cdr (cdr attr)))))
      ;; At this point, generic is an ALIST, not a PROPERTY LIST.
      ;; We need to sort it so that order changes do not effect the
      ;; test.
      (setq generic (sort generic (lambda (a b)
                                    (string< (symbol-name (car a))
                                             (symbol-name (car b))))))
      (append (list name class) 
              (apply 'append generic))
      ))


(if (fboundp 'define-overload)
    (define-overload semantic-regtest-convert-tag (tag)
      "Convert TAG into a generic format.
Recurses over children when they are found."
      (semantic-regtest--convert-tag tag))
  (defun semantic-regtest-convert-tag (tag)
    "Convert TAG into a generic format.
Recurses over children when they are found."
    (semantic-regtest--convert-tag tag)))


(defun semantic-regtest-prin1-nonterminal (tag)
  (prin1-to-string (semantic-regtest-convert-tag tag)))


(provide 'semantic-regtest)

;;; semantic-regtest.el ends here
