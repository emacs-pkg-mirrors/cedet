;;;
;;; Major mode for configure.in scripts
;;;

(if (locate-library "autoconf")

    (require 'autoconf)

;; Only use this copy of autoconf.el in Emacs21 if it isn't already
;; available.
;;
;;  Originally written by Dave Love for Emacs 21
(defvar autoconf-mode-map (make-sparse-keymap))

(defvar autoconf-mode-hook nil
  "Hook run by `autoconf-mode'.")

(defconst autoconf-font-lock-syntactic-keywords
  '(("\\<dnl\\>" 0 '(11))))

(defconst autoconf-definition-regexp
  "AC_\\(SUBST\\|DEFINE\\(_UNQUOTED\\)?\\)(\\(\\sw+\\)")

(defvar autoconf-font-lock-keywords
  `(("A[CM]_\\sw+" . font-lock-keyword-face)
    (,autoconf-definition-regexp
     3 font-lock-function-name-face)
    ;; Are any other M4 keywords really appropriate for configure.in,
    ;; given that we do `dnl'?
    ("changequote" . font-lock-keyword-face)))

(defvar autoconf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?# "<" table)
    table))

(defvar autoconf-imenu-generic-expression
  (list (list nil autoconf-definition-regexp 3)))

;; It's not clear how best to implement this.
(defun autoconf-current-defun-function ()
  "Function to use for `add-log-current-defun-function' in Autoconf mode.
This version looks back for an AC_DEFINE or AC_SUBST.  It will stop
searching backwards at another AC_... command."
  (save-excursion
    (with-syntax-table autoconf-mode-syntax-table
      (modify-syntax-entry ?_ "w")
      (if (re-search-backward autoconf-definition-regexp
			      (save-excursion (beginning-of-defun) (point))
			      t)
	  (match-string-no-properties 3)))))

;;;###autoload
(defun autoconf-mode ()
  "Major mode for editing Autoconf configure.in files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map autoconf-mode-map)
  (setq major-mode 'autoconf-mode)
  (setq mode-name "Autoconf")
  (set-syntax-table autoconf-mode-syntax-table)
  (set (make-local-variable 'parens-require-spaces) nil) ; for M4 arg lists
  (set (make-local-variable 'defun-prompt-regexp)
       "^[ \t]*A[CM]_\\(\\sw\\|\\s_\\)+")
  (set (make-local-variable 'comment-start) "dnl ")
  (set (make-local-variable 'comment-start-skip) "\\(\\<dnl\\|#\\) +")
  (set (make-local-variable 'font-lock-syntactic-keywords)
       autoconf-font-lock-syntactic-keywords)
  (set (make-local-variable 'font-lock-defaults)
       `(autoconf-font-lock-keywords nil nil (("_" . "w"))))
  (set (make-local-variable 'imenu-generic-expression)
       autoconf-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist) '(("_" . "w")))
  (set (make-local-variable 'indent-line-function) #'indent-relative)
  (set (make-local-variable 'add-log-current-defun-function)
	#'autoconf-current-defun-function)
  (run-hooks 'autoconf-mode-hook))

)

;; This part is not in autoconf.el
(add-to-list 'auto-mode-alist '("\\<configure\\.in$" . autoconf-mode))


(defvar autoconf-new-automake-string
  "dnl Process this file with autoconf to produce a configure script

AC_INIT(%s)
AM_INIT_AUTOMAKE(%s, 0)
AM_CONFIG_HEADER(config.h)

dnl End the configure script.
AC_OUTPUT(Makefile, [date > stamp-h] );\n"
  "This string is used to initialize a new configure.in.
The default is designed to be used with automake.
The first %s will be filled with the test file.
The second %s will be filled with the program name.")

(defun autoconf-new-program (rootdir program testfile)
  "Initialize a new configure.in in ROOTDIR for PROGRAM using TESTFILE.
ROOTDIR is the root directory of a given autoconf controlled project.
PROGRAM is the program to be configured.
TESTFILE is the file used with AC_INIT.
configure the initial configure script using `autoconf-new-automake-string'"
  (interactive "dRoot Dir: \nsProgram: \nsTest File: ")
  (if (bufferp rootdir)
      (set-buffer rootdir)
    (if (not (string-match "\\(/\\|\\\\)$" rootdir))
	(setq rootdir (concat rootdir "/")))
    (let ((cf (concat rootdir "configure.in")))
      (if (and (file-exists cf)
	       (not (y-or-n-p (format "File %s exists.  Start Over? " cf))))
	  (error "Quit"))
      (find-file cf)))
  ;; Note, we only ask about overwrite if a string/path is specified.
  (erase-buffer)
  (insert (format autoconf-new-automake-string testfile program)))

(defvar autoconf-preferred-macro-order
  '("AC_INIT"
    "AM_INIT_AUTOMAKE"
    "AM_CONFIG_HEADER"
    ;; Arg parsing
    "AC_ARG_ENABLE"
    "AC_ARG_WITH"
    ;; Programs
    "AC_PROG_MAKE_SET"
    "AC_PROG_AWK"
    "AC_PROG_CC"
    "AC_PROG_CPP"
    "AC_PROG_CXX"
    "AC_PROG_CXXCPP"
    "AC_ISC_POSIX"
    "AC_PROG_F77"
    "AC_PROG_GCC_TRADITIONAL"
    "AC_PROG_INSTALL"
    "AC_PROG_LEX"
    "AC_PROG_LN_S"
    "AC_PROG_RANLIB"
    "AC_PROG_YACC"
    "AC_CHECK_PROGS"
    ;; Libraries
    "AC_CHECK_LIB"
    "AC_PATH_XTRA"
    ;; Headers
    "AC_HEADER_STDC"
    "AC_HEADER_SYS_WAIT"
    "AC_HEADER_TIME"
    "AC_HEADERS"
    ;; Typedefs, structures
    "AC_TYPE_PID_T"
    "AC_TYPE_SIGNAL"
    "AC_TYPE_UID_T"
    "AC_STRUCT_TM"
    ;; Compiler characteristics
    "AC_CHECK_SIZEOF"
    "AC_C_CONST"
    ;; Library functions
    "AC_CHECK_FUNCS"
    "AC_TRY_LINK"
    ;; System Services
    ;; Other
    "AM_PATH_LISPDIR"
    ;; AC_OUTPUT is always last
    "AC_OUTPUT"
    )
  "List of macros in the order that they prefer to occur in.
This helps when inserting a macro which doesn't yet exist
by positioning it near other macros which may exist.
From the autoconf manual:
     `AC_INIT(FILE)'
     checks for programs
     checks for libraries
     checks for header files
     checks for typedefs
     checks for structures
     checks for compiler characteristics
     checks for library functions
     checks for system services
     `AC_OUTPUT([FILE...])'")

(defvar autoconf-multiple-macros
  '("AC_ARG_ENABLE"
    "AC_ARG_WITH"
    "AC_CHECK_PROGS"
    "AC_CHECK_LIB"
    "AC_CHECK_SIZEOF"
    "AC_TRY_LINK"
    )
  "Macros which appear multiple times.")

(defvar autoconf-multiple-multiple-macros
  '("AC_HEADERS" "AC_CHECK_FUNCS")
  "Macros which appear multiple times, and perform multiple queries.")

(defun autoconf-in-macro (macro)
  "Non-nil if point is in a macro of type MACRO."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "\\(A[CM]_" macro "\\|" macro "\\)"))))

(defun autoconf-find-last-macro (macro)
  "Move to the last occurance of MACRO in FILE, and return that point.
The last macro is usually the one in which we would like to insert more
items such as CHECK_HEADERS."
  (let ((op (point)))
    (goto-char (point-max))
    (if (re-search-backward (concat "^" (regexp-quote macro) "\\s-*(") nil t)
	(progn
	  (beginning-of-line)
	  (point))
      (goto-char op)
      nil)))

(defun autoconf-position-for-macro (macro)
  "Position the cursor where a new MACRO could be inserted.
This will appear at the BEGINNING of the macro MACRO should appear AFTER.
This is to make it compatible with `autoconf-find-last-macro'.
Assume that MACRO doesn't appear in the buffer yet, so search
the ordering list `autoconf-preferred-macro-order'."
  ;; Search this list backwards.. heh heh heh
  ;; This lets us do a reverse search easilly.
  (let ((ml (member macro (reverse autoconf-preferred-macro-order))))
    (if (not ml) (error "Don't know how to position for %s yet" macro))
    (setq ml (cdr ml))
    (goto-char (point-max))
    (while (and ml (not (autoconf-find-last-macro (car ml))))
      (setq ml (cdr ml)))
    (if (not ml) (error "Could not find context for positioning %s" macro))))

(defun autoconf-insert-macro-at-point (macro &optional param)
  "Add MACRO at the current point with PARAM."
  (insert macro)
  (if param
      (progn
	(insert "(" param ")")
	(if (< (current-column) 3) (insert " dnl")))))
    
(defun autoconf-insert-new-macro (macro &optional param)
  "Adds a call to MACRO in the current autoconf file.  Deals with
macro order.  See `autoconf-preferred-macro-order' and
`autoconf-multi-macros'."
  (cond ((member macro autoconf-multiple-macros)
	 ;; This occurs multiple times
	 (or (autoconf-find-last-macro macro)
	     (autoconf-position-for-macro macro))
	 (forward-sexp 2)
	 (end-of-line)
	 (insert "\n")
	 (autoconf-insert-macro-at-point macro param))
	((member macro autoconf-multiple-multiple-macros)
	 (if (not param)
	     (error "You must have a paramter for %s" macro))
	 (if (not (autoconf-find-last-macro macro))
	     (progn
	       ;; Doesn't exist yet....
	       (autoconf-position-for-macro macro)
	       (forward-sexp 2)
	       (end-of-line)
	       (insert "\n")
	       (autoconf-insert-macro-at-point macro param))
	   ;; Does exist, can we fit onto the current line?
	   (forward-sexp 2)
	   (down-list -1)
	   (if (> (+ (current-column) (length param))  fill-column)
	       (insert " " param)
	     (up-list 1)
	     (end-of-line)
	     (insert "\n")
	     (autoconf-insert-macro-at-point macro param))))
	((autoconf-find-last-macro)
	 ;; If it isn't one of the multi's, it's a singleton.
	 ;; If it exists, ignore it.
	 nil)
	(t
	 (autoconf-position-for-macro macro)
	 (forward-sexp 2)
	 (end-of-line)
	 (insert "\n")
	 (autoconf-insert-macro-at-point macro param))))

(defun autoconf-find-query-for-header (header)
  "Position the cursor where HEADER is queried."
  (interactive "sHeader: ")
  (let ((op (point))
	(found t))
    (goto-char (point-min))
    (condition-case nil
	(while (not
		(progn
		  (re-search-forward
		   (concat "\\b" (regexp-quote header) "\\b"))
		  (save-excursion
		    (beginning-of-line)
		    (looking-at "AC_CHECK_HEADERS")))))
      ;; We depend on the search failing to exit our loop on failure.
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-header (header)
  "Add in HEADER to be queried for in our autoconf file."
  (interactive "sHeader: ")
  (or (autoconf-find-query-for-header header)
      (autoconf-insert-new-macro "AC_CHECK_HEADERS" header)))


(defun autoconf-find-query-for-func (func)
  "Position the cursor where func is queried."
  (interactive "sFunction: ")
  (let ((op (point))
	(found t))
    (goto-char (point-min))
    (condition-case nil
	(while (not
		(progn
		  (re-search-forward
		   (concat "\\b" (regexp-quote func) "\\b"))
		  (save-excursion
		    (beginning-of-line)
		    (looking-at "AC_CHECK_FUNCS")))))
      ;; We depend on the search failing to exit our loop on failure.
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-func (func)
  "Add in FUNC to be queried for in our autoconf file."
  (interactive "sFunction: ")
  (or (autoconf-find-query-for-func func)
      (autoconf-insert-new-macro "AC_CHECK_FUNCS" func)))

(defvar autoconf-program-builtin
  '(("AWK" . "AC_PROG_AWK")
    ("CC" . "AC_PROG_CC")
    ("CPP" . "AC_PROG_CPP")
    ("CXX" . "AC_PROG_CXX")
    ("CXXCPP" . "AC_PROG_CXXCPP")
    ("F77" . "AC_PROG_F77")
    ("GCC_TRADITIONAL" . "AC_PROG_GCC_TRADITIONAL")
    ("INSTALL" . "AC_PROG_INSTALL")
    ("LEX" . "AC_PROG_LEX")
    ("LN_S" . "AC_PROG_LN_S")
    ("RANLIB" . "AC_PROG_RANLIB")
    ("YACC" . "AC_PROG_YACC")
    )
  "Association list of PROGRAM variables and their built-in MACRO.")

(defun autoconf-find-query-for-program (prog)
  "Position the cursor where PROG is queried.
PROG is the VARIABLE to use in autoconf to identify the program.
PROG excludes the _PROG suffix.  Thus if PROG were EMACS, then the
variable in configure.in would be EMACS_PROG."
  (let ((op (point))
	(found t)
	(builtin (assoc prog autoconf-program-builtin)))
    (goto-char (point-min))
    (condition-case nil
	(re-search-forward
	 (concat "^"
		 (or (cdr-safe builtin)
		     (concat "AC_CHECK_PROG\\s-*(\\s-*" prog "_PROG"))
		 "\\>"))
      (error (setq found nil)))
    (if (not found) (goto-char op))
    found))

(defun autoconf-add-query-for-program (prog &optional names)
  "Add in PROG to be queried for in our autoconf file.
Optional NAMES is for non-built-in programs, and is the list
of possible names."
  (interactive "sProgram: ")
  (if (autoconf-find-query-for-program prog)
      nil
    (let ((builtin (assoc prog autoconf-program-builtin)))
      (if builtin
	  (autoconf-insert-new-macro (cdr builtin))
	;; Not built in, try the params item
	(autoconf-insert-new-macro "AC_CHECK_PROGS" (concat prog "," names))
	))))

;;; Scrappy little changes
;;
(defun autoconf-delete-parameter (index)
  "Delete the INDEXth parameter from the macro starting on the current line.
Leaves the cursor where a new parameter can be inserted.
INDEX starts at 1."
  (beginning-of-line)
  (down-list 1)
  (re-search-forward ", ?" nil nil (1- index))
  (delete-region (point)
		 (progn
		   (re-search-forward ",\\|)" (save-excursion
						(end-of-line)
						(point)))
		   (forward-char -1)
		   (point))))

(defun autoconf-set-version (version)
  "Set the version used with automake to VERSION."
  (if (not (stringp version))
      (signal 'wrong-type-argument '(stringp version)))
  (if (not (autoconf-find-last-macro "AM_INIT_AUTOMAKE"))
      (error "Cannot update version")
    ;; Move to correct position.
    (autoconf-delete-parameter 2)
    (insert version)))

(defun autoconf-set-output (outputlist)
  "Set the files created in AC_OUTPUT to OUTPUTLIST.
OUTPUTLIST is a list of strings representing relative paths
to Makefiles, or other files using Autoconf substitution."
  (if (not (autoconf-find-last-macro "AC_OUTPUT"))
      (error "Cannot update version")
    (autoconf-delete-parameter 1)
    (insert (mapconcat (lambda (a) a) outputlist " "))))

;;; end of lisp
(provide 'autoconf-edit)
