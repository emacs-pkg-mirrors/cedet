;;; semantic-gcc.el --- gcc querying special code for the C parser

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-gcc.el,v 1.1 2008/08/27 03:14:50 zappo Exp $

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; GCC stores things in special places.  These functions will query
;; GCC, and set up the preprocessor and include paths.

;;; Code:

(defun semantic-gcc-query (&optional gcc-cmd)
  "Query gcc.  Return a list of configurations.
GCC-CMD is an optional command to execute instead of \"gcc\""
  ;; $ gcc -v
  ;;
  ;; Reading specs from /usr/lib/gcc-lib/i386-redhat-linux/3.2.2/specs
  ;; Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --host=i386-redhat-linux
  ;; Thread model: posix
  ;; gcc version 3.2.2 20030222 (Red Hat Linux 3.2.2-5)
  (let ((buff (get-buffer-create " *gcc-query*")))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (call-process (or gcc-cmd "gcc") nil buff nil "-v")
      (prog1
	  (buffer-string)
	(kill-buffer buff)
	)))
  )

(defun semantic-gcc-fields (str)
  "Convert GCC output STR into an alist of fields."
  (let ((fields nil)
	(lines (split-string str "\n"))
	)
    ;; Line 1 is configuration file
    (setq lines (cdr lines))
    ;; Line 2 is "Configured with:"
    (let* ((parts (split-string (car lines) ":" t))
	   (opts (split-string (car (cdr parts)) " " t))
	   )
      (dolist (O (cdr opts))
	(let* ((data (split-string O "="))
	       (sym (intern (car data)))
	       (val (car (cdr data))))
	  (push (cons sym val) fields)
	  ))
      )
    (setq lines (cdr lines))
    ;; Line 3 is thread model
    (setq lines (cdr lines))
    ;; Line 4 is gcc version
    (let ((parts (split-string (car lines) " ")))
      (push (cons 'version (nth 2 parts)) fields))
    ;; Return it.
    fields))

(defvar semantic-gcc-setup-data nil
  "The GCC setup data.")

(defun semantic-gcc-setup (&optional gcc-cmd)
  "Setup Semantic C parsing based on GCC output.
Optional argument GCC-CMD is an optional command to use instead of \"gcc\"."
  (interactive)
  (let* ((fields (or semantic-gcc-setup-data
		     (semantic-gcc-fields (semantic-gcc-query))))
	 (ver (cdr (assoc 'version fields)))
	 (host (cdr (assoc '--host fields)))
	 (prefix (cdr (assoc '--prefix fields)))
	 (include-root (concat prefix "/include"))
	 (include-cpp (concat include-root "/c++/" ver))
	 (include-cpp-sys (concat include-cpp "/" host))
	 (cppconfig (concat include-cpp-sys "/bits/c++config.h"))
	 )
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    ;; Now setup include paths
    (semantic-add-system-include include-cpp 'c-mode)
    (semantic-add-system-include include-cpp 'c++-mode)
    (semantic-add-system-include include-cpp-sys 'c-mode)
    (semantic-add-system-include include-cpp-sys 'c++-mode)
    ;; Setup the core macro header
    (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
	(add-to-list 'semantic-lex-c-preprocessor-symbol-file cppconfig)
      (setq semantic-lex-c-preprocessor-symbol-file (list cppconfig)))
    (when (featurep 'semantic-c)
      (semantic-c-reset-preprocessor-symbol-map))
    ))

(provide 'semantic-gcc)
;;; semantic-gcc.el ends here
