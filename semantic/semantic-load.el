;;; semantic-load.el --- Autoload definitions for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-load.el,v 1.30 2002/08/11 18:31:38 zappo Exp $

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
;; Initialize semantic for all supported conditions.

;;; Code:
;;

(load "semantic-al" nil t)

;;; Add parser directories
;;
(let ((dir (file-name-directory (locate-library "semantic"))))
  (add-to-list 'load-path (concat dir "bovine"))
  (add-to-list 'load-path (concat dir "wisent"))
  )

;;; language modes
;;
(autoload 'wisent-wy-mode "wisent-wy"
  "Initialize a buffer for editing BNF code." t)
(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-wy-mode))

;;; Some speedbar major modes
(eval-after-load "speedbar"
  '(progn
     (require 'semantic-cb)
     (require 'semantic-ia-sb)))

;;; Senator configuration.
(defvar global-senator-minor-mode nil
  "*If non-nil enable global use of senator minor mode.")

(custom-add-to-group 'senator
                     'global-senator-minor-mode
                     'custom-variable)

(custom-add-load 'global-senator-minor-mode 'senator)

(defvar global-semantic-show-dirty-mode nil
  "*If non-nil enable global use of show-dirty mode.")

(custom-add-to-group 'semantic
                     'global-semantic-show-dirty-mode
                     'custom-variable)

(custom-add-load 'global-semantic-show-dirty-mode
                 'semantic-util-modes)

(defvar global-semantic-show-unmatched-syntax-mode nil
  "*If non-nil enable global use of show-unmatched-syntax mode.")

(custom-add-to-group 'semantic
                     'global-semantic-show-unmatched-syntax-mode
                     'custom-variable)

(custom-add-load 'global-semantic-show-unmatched-syntax-mode
                 'semantic-util-modes)

(defvar global-semantic-auto-parse-mode nil
  "*If non-nil enable global use of auto-parse mode.")

(custom-add-to-group 'semantic
                     'global-semantic-auto-parse-mode
                     'custom-variable)

(custom-add-load 'global-semantic-auto-parse-mode
                 'semantic-util-modes)

(defvar global-semantic-summary-mode nil
  "*If non-nil enable global use of summary mode.")

(custom-add-to-group 'semantic
                     'global-semantic-summary-mode
                     'custom-variable)

(custom-add-load 'global-semantic-summary-mode
                 'semantic-util-modes)


;;; Useful predefined setup
;;
(defvar semantic-load-turn-everything-on nil
  "Non-nil means turn on all features in the semantic package.")

(defvar semantic-load-turn-useful-things-on nil
  "Non-nil means turn on all `useful' features.
Sadly `useful' here means things Eric wants on as opposed to some
other criteria.")

(when (or semantic-load-turn-everything-on
	  semantic-load-turn-useful-things-on)

  (if (and semantic-load-turn-everything-on
	   (fboundp #'which-func-mode))
      (add-hook 'semantic-init-hooks (lambda ()
				       (which-func-mode 1))))

  (when (and (eq window-system 'x)
	     (locate-library "imenu"))
    (add-hook 'semantic-init-hooks (lambda ()
				     (imenu-add-to-menubar "TOKENS"))))

  (when semantic-load-turn-everything-on
    (global-semantic-show-dirty-mode 1)
    (global-semantic-highlight-edits-mode 1)
    )

  (global-senator-minor-mode 1)
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-auto-parse-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-summary-mode 1)
  (global-semantic-show-parser-state-mode 1)
 )

(provide 'semantic-load)

;;; semantic-load.el ends here
