;;; cogre-periodic.el --- Periodic table of COGRE nodes
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cogre-periodic.el,v 1.1 2009/03/27 01:40:38 zappo Exp $
;;
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
;; Periodic table of COGRE nodes.
;;
;; @TODO - Each time a new node or link is created, add it to the
;;         periodic table.

;;; Code:
(require 'cogre-utest)
(require 'cogre-uml)

(defun cogre-periodic ()
  "Create a periodic table of COGRE objects."
  (interactive)
  ;; Setup the graph.
  (switch-to-buffer (get-buffer-create "*Graph Periodic*"))
  (erase-buffer)
  (cogre "Periodic")
  ;; Put out the base items.
  (cogre-utest-make-node-at 2 1 'cogre-node "cogre-node")
  (cogre-utest-make-node-at 40 1 'cogre-node "cogre-node (2)")
  (cogre-render-buffer cogre-graph)
  (cogre-utest-link-at 2 1 40 1 'cogre-link)
  ;; Put out some UML items.
  (cogre-utest-make-node-at 2 6 'cogre-package "cogre-package")
  (cogre-utest-make-node-at 25 6 'cogre-class "cogre-class")
  (cogre-utest-make-node-at 23 15 'cogre-class "cogre-class (2)")
  (cogre-render-buffer cogre-graph)
  (cogre-utest-link-at 2 6 25 6 'cogre-aggregate)
  (cogre-utest-link-at 23 15 25 6 'cogre-inherit)
  )


(provide 'cogre-periodic)
;;; cogre-periodic.el ends here
