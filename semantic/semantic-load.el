;;; semantic-load.el --- Autoload definitions for Semantic

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-load.el,v 1.12 2001/10/28 01:57:50 zappo Exp $

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

;;; Parser setup
;;
(autoload 'semantic-bnf-mode "semantic-bnf" "Mode for Bovine Normal Form." t)
(add-to-list 'auto-mode-alist '("\\.bnf$" . semantic-bnf-mode))

(autoload 'semantic-default-c-setup "semantic-c")
(add-hook 'c-mode-hook 'semantic-default-c-setup)
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(autoload 'semantic-default-elisp-setup "semantic-el")
(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

(autoload 'semantic-default-scheme-setup "semantic-scm")
(add-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

(autoload 'semantic-default-make-setup "semantic-make")
(add-hook 'makefile-mode-hook 'semantic-default-make-setup)

(autoload 'semantic-default-texi-setup "semantic-texi")
(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)

(autoload 'semantic-default-java-setup "semantic-java")
(add-hook 'java-mode-hook 'semantic-default-java-setup)

;;; Charts
;;
(autoload 'semantic-chart-nonterminals-by-token "semantic-chart" nil t)
(autoload 'semantic-chart-nonterminal-complexity-token "semantic-chart" nil t)
(autoload 'semantic-chart-database-size "semantic-chart" nil t)


;;; Minor modes
;;

;; semanticdb
(autoload 'global-semanticdb-minor-mode "semanticdb" nil t)

;; senator
(autoload 'senator-minor-mode "senator"
  "Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}"
  t nil)

(defvar global-senator-minor-mode nil
  "*If non-nil enable global use of senator minor mode.")

(custom-add-to-group 'senator
                     'global-senator-minor-mode
                     'custom-variable)

(custom-add-load 'global-senator-minor-mode 'senator)

(autoload 'global-senator-minor-mode "senator"
  "Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  t nil)

;; semantic-show-dirty
(autoload 'semantic-show-dirty-mode
  "semantic-util-modes"
  "Minor mode to display of dirty tokens.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-dirty-mode-map}"
  t nil)

(defvar global-semantic-show-dirty-mode nil
  "*If non-nil enable global use of show-dirty mode.")

(custom-add-to-group 'semantic
                     'global-semantic-show-dirty-mode
                     'custom-variable)

(custom-add-load 'global-semantic-show-dirty-mode
                 'semantic-util-modes)

(autoload 'global-semantic-show-dirty-mode
  "semantic-util-modes"
  "Toggle global use of show-dirty mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  t nil)

;; semantic-show-unmatched-syntax
(autoload 'semantic-show-unmatched-syntax-mode
  "semantic-util-modes"
  "Minor mode to display of unmatched-syntax tokens.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}"
  t nil)

(defvar global-semantic-show-unmatched-syntax-mode nil
  "*If non-nil enable global use of show-unmatched-syntax mode.")

(custom-add-to-group 'semantic
                     'global-semantic-show-unmatched-syntax-mode
                     'custom-variable)

(custom-add-load 'global-semantic-show-unmatched-syntax-mode
                 'semantic-util-modes)

(autoload 'global-semantic-show-unmatched-syntax-mode
  "semantic-util-modes"
  "Toggle global use of show-unmatched-syntax mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  t nil)


;; This turns on semantic partial reparsing
(add-hook 'semantic-change-hooks #'semantic-change-function-mark-dirty)

(defvar semantic-load-turn-everything-on nil
  "Non-nil means turn on all features in the semantic package.")

(when semantic-load-turn-everything-on
  (add-hook 'semantic-init-hooks #'turn-on-eldoc-mode)
  (if (fboundp #'which-func-mode)
      (add-hook 'semantic-init-hooks (lambda ()
				       (which-func-mode 1))))

  (when (and (eq window-system 'x)
	     (locate-library "imenu"))
    (add-hook 'semantic-init-hooks (lambda ()
				     (imenu-add-to-menubar "TOKENS"))))

  (global-semantic-show-dirty-mode 1)
  (global-senator-minor-mode 1)
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semanticdb-minor-mode 1)

  (add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))
  )

(provide 'semantic-load)

;;; semantic-load.el ends here
