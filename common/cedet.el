;;; cedet.el --- Setup CEDET environment

;; Copyright (C) 2002, 2003 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: CEDET developers <http://sf.net/projects/cedet>
;; Created: 09 Dec 2002
;; Keywords: syntax
;; X-RCS: $Id: cedet.el,v 1.1 2003/01/28 16:07:29 ponced Exp $

;; This file is not part of Emacs

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library automatically setups your [X]Emacs to use CEDET tools.
;;
;; First download the latest CEDET distribution, provided in a
;; cedet-<VERSION>.tar.gz tarball, from the project page at:
;; <http://sf.net/projects/cedet>.
;;  
;; Unpack the tarball in a directory of your choice.  It will install
;; the following directory tree:
;;
;;   cedet
;;     |
;;     +- common
;;     |
;;     +- cogre
;;     |
;;     +- ede
;;     |
;;     +- eieio
;;     |
;;     +- semantic
;;     |
;;     \- speedbar
;;
;; Then, add the following into your ~/.emacs startup file:
;;
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; If you want to turn on useful or all Semantic features by default,
;; respectively add:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;; or
;;   (setq semantic-load-turn-everything-on t)
;;
;; before loading this file, like this:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; That's it!
;;

;;; History:
;;

;;; Code:
(defconst cedet-version "1.0"
  "Current version of CEDET.")

(defconst cedet-packages
  '(
    ;;PACKAGE   MIN-VERSION   SETUP
    (cogre      "0.3"                      )
    (ede        "1.0beta3"                 )
    (eieio      "0.17"                     )
    (semantic   "1.4"         semantic-load)
    (speedbar   "0.14beta4"                )
    )
  "Table of CEDET packages to install.")

;; This file must be in "<INSTALL-DIR>/cedet/common"!
(let ((default-directory
        (file-name-directory
         (or load-file-name (buffer-file-name))))
      (packages cedet-packages)
      package)
  
  ;; Add "<INSTALL-DIR>/cedet/common" to `load-path'.
  (add-to-list 'load-path default-directory)
  (message "%S added to `load-path'" default-directory)
  ;; Require the inversion library.
  (require 'inversion)
  
  ;; Go up to the parent "<INSTALL-DIR>/cedet" directory.
  (setq default-directory (expand-file-name ".."))

  ;; Add the CEDET packages subdirectories to the `load-path' if
  ;; necessary, and do specific setup.
  (while packages
    (setq package (car packages)
          packages (cdr packages))
    (inversion-add-to-load-path (car package) (cadr package))
    (if (nth 2 package)
        (require (nth 2 package))))
  )

(provide 'cedet)

;;; cedet.el ends here
