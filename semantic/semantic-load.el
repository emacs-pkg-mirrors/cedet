;;; semantic-load.el --- Autoload definitions for Semantic

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-load.el,v 1.2 2001/03/10 01:55:34 zappo Exp $

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

(autoload 'senator-minor-mode "senator" nil t)

(autoload 'global-semanticdb-minor-mode "semanticdb" nil t)
(autoload 'semantic-show-dirty-mode "semantic-util" nil t)

(defvar semantic-load-turn-everything-on nil
  "Non-nil means turn on all features in the semantic package.")

(when semantic-load-turn-everything-on
  (add-hook 'semantic-init-hooks 'senator-minor-mode)
  (add-hook 'semantic-init-hooks 'turn-on-eldoc-mode)
  (if (fboundp 'which-func-mode)
      (add-hook 'semantic-init-hooks 'which-funct-mode))

  (when (eq window-system 'x)
    (add-hook 'semantic-init-hooks (lambda ()
				     (imenu-add-to-menubar "TOKENS"))))

  (semantic-show-dirty-mode 1)
  (global-semanticdb-minor-mode 1)

  (add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))
  )

(provide 'semantic-load)

;;; semantic-load.el ends here
