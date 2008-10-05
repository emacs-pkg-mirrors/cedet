;;; cedet-utests.el --- Run all unit tests in the CEDET suite.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-utests.el,v 1.1 2008/10/05 12:51:37 zappo Exp $

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
;; Remembering to run all the unit tests available in CEDET one at a
;; time is a bit time consuming.  This links all the tests together
;; into one command.

(require 'cedet)
;;; Code:
(defvar cedet-utest-test-alist
  '(
    ;; Test inversion
    ("inversion" . inversion-unit-test)

    ;; EZ Image dumping.
    ("ezimage associations" . ezimage-image-association-dump)
    ("ezimage images" . ezimage-image-dump)

    ;; WORKGING interactive tests.
    ("working: wait-for-keypress" . working-wait-for-keypress)
    ("working: sleep" . working-verify-sleep)

    ;; PULSE
    ("pulse interactive test" . pulse-test)

    ;; The EIEIO unit test suite.
    ("eieio" . (lambda () (let ((lib (locate-library "eieio-tests.el"
						     t)))
			    (load-file lib))))
    ("eieio browser" . eieio-browse)

    ;; SEMANTIC tests
    ("semantic: multi-lang parsing" . semantic-utest-main)
    ("semantic: C preprocessor" . semantic-utest-c)
    ("semantic: analyzer tests" . semantic-ia-utest)
    ("semanticdb: data cache" . semantic-test-data-cache)
    ("semantic: throw-on-input" . semantic-test-throw-on-input)

    ;; SRecode
    ("srecode: templates" . srecode-utest-template-output)
    ("srecode: show maps" . srecode-get-maps)
   )
  "Alist of all the ttests in CEDET we should run.")

(defun cedet-utest ()
  "Run the CEDET unittests."
  (interactive)
  (cedet-utest-log-setup)
  (let ((tl cedet-utest-test-alist)
	(notes nil)
	(err nil))
    (dolist (T tl)
      (cedet-utest-add-log-item-start (car T))
      (setq notes nil err nil)
      (condition-case Cerr
	  (progn
	    (funcall (cdr T))
	    )
	(error
	 (setq err (format "ERROR: %S" Cerr))))
      (cedet-utest-add-log-item-done notes err)
      ))
  (cedet-utest-add-log-item-done "All Tests Complete" nil t))

;;; Logging utility.
;;
(defvar cedet-utest-frame nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-buffer nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-frame-parameters
  '((name . "CEDET-UTEST")
    (width . 80)
    (height . 25)
    (minibuffer . t))
  "Frame parameters used for the cedet utest log frame.")

(defun cedet-utest-log-setup ()
  "Setup a frame and buffer for unit testing."
  (when (or (not cedet-utest-frame) (not (frame-live-p cedet-utest-frame)))
    (setq cedet-utest-frame (make-frame cedet-utest-frame-parameters)))
  (when (or (not cedet-utest-buffer) (not (buffer-live-p cedet-utest-buffer)))
    (setq cedet-utest-buffer (get-buffer-create "*CEDET utest log*")))
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (erase-buffer)
    (insert "Setting up tests to run @ " (current-time-string) "\n\n"))
  (let ((oframe (selected-frame)))
    (unwind-protect
	(progn
	  (select-frame cedet-utest-frame)
	  (switch-to-buffer cedet-utest-buffer t))
      (select-frame oframe)))
  )

(defun cedet-utest-add-log-item-start (item)
  "Add ITEM into the log as being started."
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (goto-char (point-max))
    (when (not (bolp)) (insert "\n"))
    (insert "Running " item " ... ")
    (sit-for 0)
    ))

(defun cedet-utest-add-log-item-done (&optional notes err precr)
  "Add into the log that the last item is done.
Apply NOTES to the doneness of the log.
Apply ERR if there was an error in previous item.
Optional argument PRECR indicates to prefix the done msg w/ a newline."
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (goto-char (point-max))
    (when precr (insert "\n"))
    (if err
	(insert err)
      (insert "done")
      (when notes (insert " (" notes ")")))
    (insert "\n")
    (sit-for 0)
    ))

(provide 'cedet-utests)
;;; cedet-utests.el ends here
