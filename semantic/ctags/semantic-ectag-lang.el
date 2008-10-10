;;; semantic-ectag-lang.el --- Exuberent Ctags per-language support

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-lang.el,v 1.1 2008/10/10 22:23:47 zappo Exp $

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
;; Support various languages via Exuberent CTags.
;;
;; Support requires:
;;  * Specification of tag 'kind' to get
;;  * Signature parsing.

;;; Code:
(require 'semantic-fw)

;;; C/C++
(defvar-mode-local c-mode semantic-ectag-lang "c"
  "Language name for Exuberent CTags.")

(defvar-mode-local c-mode semantic-ectag-lang-kind "cegmnpsufvt"
  "Kinds of Exuberent CTags available.")

;;; Emacs Lisp
(defvar-mode-local emacs-lisp-mode semantic-ectag-lang "lisp"
  "Language name for Exuberent CTags.")

(defvar-mode-local emacs-lisp-mode semantic-ectag-lang-kind "f"
  "Kinds of Exuberent CTags available.")



(provide 'semantic-ectag-lang)
;;; semantic-ectag-lang.el ends here
