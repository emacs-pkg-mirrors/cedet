;;;
;;; Major mode for configure.in scripts
;;;

(defvar autoconf-syntax-table nil
  "Syntax table used in an Autoconf file")

(if autoconf-syntax-table
    nil
  (setq autoconf-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n ">1" autoconf-syntax-table)
  (modify-syntax-entry ?\" "\"" autoconf-syntax-table)
  ;(modify-syntax-entry ?` "\"" autoconf-syntax-table)
)


(defvar autoconf-font-lock-keywords-1
  '(("\\<\\(dnl[^\n]*\\)$" 1 font-lock-comment-face t)
    ("\\<\\(AC_\\w+\\)\\>" 1 font-lock-function-name-face)
    ("\\<\\(if\\|then\\|fi\\|else\\|case\\|for\\|end\\|done\\|in\\|break\\)\\>"
     1 font-lock-keyword-face)
    ("\\<\\$\\(\\w+\\)\\>" 1 font-lock-type-face)

    )
  "Keywords for fontifying.")

(defvar autoconf-font-lock-keywords-2
  (append
   autoconf-font-lock-keywords-1
   '(("\\<\\(--\\w+\\)\\>" 1 font-lock-reference-face)
     ("^\\s-*\\(\\w+\\)=" 1 font-lock-variable-name-face)
     ))
  "Advanced keywords for fontifying.")

(defvar autoconf-font-lock-defaults
  '((autoconf-font-lock-keywords-1 autoconf-font-lock-keywords-2)
    nil nil
    (("_-" . "w"))
    )
  "Keywords used for font lock")

(defun autoconf-mode ()
  "Major mode for editing autoconf scripts."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table autoconf-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (setq major-mode 'autoconf-mode
	mode-name "Autoconf"
	font-lock-defaults autoconf-font-lock-defaults
	comment-start-skip "dnl \\s-*"
	comment-start "dnl ")
  )

;;; end of lisp
(provide 'autoconf)
	     