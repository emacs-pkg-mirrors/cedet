;;; inversion.el --- When you need something in version XX.XX

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: inversion.el,v 1.4 2002/09/03 23:52:05 zappo Exp $

;;; Code:
(defvar inversion-version "1.0beta1"
  "Current version of InVersion.")
(defvar inversion-incompatible-version "0.0alpha1"
  "An earlier release which is incmpatible with this release.")

;; InVersion is free software; you can redistribute it and/or modify
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
;; Keeping track of rapidly developing software is a tough thing to
;; do, expecially if you want to have co-dependent packages which all
;; move at different rates.
;;
;; This library provides a framework for specifying version numbers
;; and (as side effect) have a flexible way of getting a desired feature set.
;;
;; If you would like to use this package to satisfy dependenc replace this:
;; 
;; (require 'spiffy)
;;
;; with this:
;;
;; (require 'inversion)
;; (inversion-require 'spiffy "1.0")
;;
;; If you feel the need to not throw errors, you can do this instead:
;;
;; (let ((err (inversion-test 'spiffy "1.0")))
;;    (if err (your-stuff-here)))
;;
;; If you would like to make inversion optional, do this:
;;
;;(condition-case nil
;;    (require 'inversion)
;;  (error
;;   (defun inversion-test (p v)
;;     (string= v (symbol-value (intern-soft
;;			       (concat (symbol-string p) "-version")))))))
;;
;; Or modify to specify `inversion-require' instead.
;;
;; TODO:
;;  Offer to download newer versions of a package.

;;; History:
;; 
;; Sept 3, 2002:  First general publication.

(defvar inversion-decoder-ring
  '(
    (alpha  "^\\([0-9]+\\)\\.\\([0-9]+\\)alpha\\([0-9]+\\)$" 3)
    (beta   "^\\([0-9]+\\)\\.\\([0-9]+\\)beta\\([0-9]+\\)$" 3)
    (full   "^\\([0-9]+\\)\\.\\([0-9]+\\)$" 2)
    (point  "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$" 3)
    )
  "List of regular expressions for version strings.
Each element is of the form:
  ( RELEASE-TYPE REGEXP MAX )
Where RELEASE-TYPE is a symbo specifying something like `beta'
or `alpha'.  REGEXP is the regular expression to match.
MAX is the maximun number of match-numbers in the release number.
The order of the list is important.  Least stable versions should
be first.  More stable version should be last.")

(defun inversion-decode-version (version-string)
  "Decode VERSION-STRING into an encoded list.
Return value is of the form:
  (RELEASE MAJOR MINOR ...)
where RELEASE is a symbol such as `full', or `beta'."
  (let ((ring inversion-decoder-ring)
	(result nil))
    (while (and ring (not result))
      (if (string-match (car (cdr (car ring))) version-string)
	  (let ((ver nil)
		(num-left (nth 2 (car ring)))
		(count 1))
	    (while (<= count num-left)
	      (setq ver
		    (cons (string-to-int
			   (substring version-string
				      (match-beginning count)
				      (match-end count)))
			  ver)
		    count (1+ count)))
	    (setq result
		  (cons (car (car ring))
			(nreverse ver)))
	    ))
      (setq ring (cdr ring)))
    result))

(defun inversion-package-version (package)
  "Return the decoded version for PACKAGE."
  (let ((ver (symbol-value
	      (intern-soft
	       (concat (symbol-name package)
		       "-version"))))
	(code nil))
    (unless ver
      (error "Package %S does not define %S-version"))
    ;; Decode the code
    (setq code (inversion-decode-version ver))
    (unless code
      (error "%S-version value cannot be decoded"))
    code))

(defun inversion-package-incompatibility-version (package)
  "Return the decoded incompatibility version for PACKAGE.
The incompatibility version is specified by the programmer of
a package when a package is not backward compatible.  It is
not an indication of new features or bug fixes."
  (let ((ver (symbol-value
	      (intern-soft
	       (concat (symbol-name package)
		       "-incompatible-version")))))
    (if (not ver)
	nil
      ;; Decode the code
      (inversion-decode-version ver))))

(defun inversion-recode (code)
  "Convert CODE into a string."
  ;; Do a better job someday.
  (format "%S" code))

(defun inversion-release-to-number (release-symbol)
  "Convert RELEASE-SYMBOL into a number."
  (let* ((ra (assoc release-symbol inversion-decoder-ring))
	 (rn (- (length inversion-decoder-ring)
		(length (member ra inversion-decoder-ring)))))
    rn))

(defun inversion-= (ver1 ver2)
  "Return non-nil if VER1 is equal to VER2."
  (equal ver1 ver2))

(defun inversion-< (ver1 ver2)
  "Return non-nil if VER1 is less than VER2."
  (let ((v1-0 (inversion-release-to-number (nth 0 ver1)))
	(v1-1 (nth 1 ver1))
	(v1-2 (nth 2 ver1))
	(v1-3 (nth 3 ver1))
	;; v2
	(v2-0 (inversion-release-to-number (nth 0 ver2)))
	(v2-1 (nth 1 ver2))
	(v2-2 (nth 2 ver2))
	(v2-3 (nth 3 ver2)))
    (or (and (= v1-0 v2-0)
	     (= v1-1 v2-1)
	     (= v1-2 v2-2)
	     v1-3 v2-3		; all or nothin if elt - is =
	     (< v1-3 v2-3))
	(and (= v1-0 v2-0)
	     (= v1-1 v2-1)
	     (< v1-2 v2-2))
	(and (= v1-0 v2-0)
	     (< v1-1 v2-1))
	(< v1-0 v2-0))))

(defun inversion-test (package minimum &rest reserved)
  "Test that PACKAGE meets the MINIMUM version requirement.
PACKAGE is a symbol, similar to what is passed to `require'.
RESERVED arguments are kept for a later user.
MINIMUM is of similar format to return entries of
`inversion-decode-version', or a classic version string.
This dependson the symbol `PACKAGE-version' being defined
in PACKAGE.
Return nil if everything is ok.  Return an error string otherwise."
  (let ((code (inversion-package-version package))
	(req (if (stringp minimum)
		 (inversion-decode-version minimum)
	       minimum))
	(count 0)
	)
    ;; Perform a test.
    (cond
     ((inversion-= code req)
      ;; Same version.. Yay!
      nil)
     ((inversion-< code req)
      ;; Version is too old!
      (format "You need to upgrade package %s to %s" package minimum))
     ((inversion-< req code)
      ;; Newer is installed.  What to do?
      (let ((incompatible
	     (inversion-package-incompatibility-version package)))
	(cond
	 ((not incompatible) nil)
	 ((or (inversion-= req incompatible)
		   (inversion-< req incompatible))
	       ;; If the requested version is = or < than
	       ;; what the package maintainer says is incompatible,
	       ;; then throw that error.
	       (format "Package %s version is not backward compatible with %s"
		       minimum))
	 ;; Things are ok.
	 (t nil))
	)
      )
     (t
      "Inversion version check failed.")
     )))

(defun inversion-require (package version file &rest reserved)
  "Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument RESERVED is saved for later use."
  (require package file)
  (let ((err (inversion-test package version)))
    (if err (error err))))
  
;;; Inversion tests
;;
(let ((c1 (inversion-package-version 'inversion))
      (c1i (inversion-package-incompatibility-version 'inversion))
      (c2 (inversion-decode-version "1.3alpha2"))
      (c3 (inversion-decode-version "1.3beta4"))
      (c4 (inversion-decode-version "1.3beta5"))
      (c5 (inversion-decode-version "1.3.4"))
      (c6 (inversion-decode-version "2.3")))
  (if (not (and (inversion-= c1 c1)
		(inversion-< c1i c1)
		(inversion-< c2 c3)
		(inversion-< c3 c4)
		(inversion-< c4 c5)
		(inversion-< c6 c5)
		(not (inversion-< c3 c2))
		(not (inversion-< c4 c3))
		(not (inversion-< c5 c4))
		(not (inversion-< c5 c6))
		;; Test the tester on inversion
		(not (inversion-test 'inversion inversion-version))
		))
      (error "Inversion tests failed")
    t))

(provide 'inversion)

;;; inversion.el ends here
