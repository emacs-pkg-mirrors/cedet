;;; ede-shell.el --- A shell controlled by EDE.
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-shell.el,v 1.2 2009/10/15 17:34:04 zappo Exp $
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
;; Run commands through a specialized EDE shell buffer.  Commands will
;; be run as shell commands so users can type in their own thing in
;; the shells for testing purposes.
;;
;; Each thing that EDE wants to use will create a shell to interact with it.

;;; Code:

(defmethod ede-shell-run-something ((target ede-target) command)
  "Create a shell to run stuff for TARGET.
COMMAND is a text string representing the thing to be run."
  (let* ((name (ede-name target))
	 (buff (get-buffer-create (format "*EDE Shell %s" name)))
	 (cp (ede-target-parent target))
	 (dd (oref cp :directory)))
    ;; Show the new buffer.
    (when (not (get-buffer-window buff))
      (switch-to-buffer-other-window buff t))
    ;; Force a shell into the buffer.
    (shell buff)
    (while (eq (point-min) (point))
      (accept-process-output))
    ;; Change the default directory
    (if (not (string= (file-name-as-directory (expand-file-name default-directory))
		      (file-name-as-directory (expand-file-name dd))))
	;; Go there.
	(setq command (concat (concat "cd " dd ";" command))))
    ;; Run the command itself.
    (ede-shell-run-command command)
    ))
  
(defun ede-shell-run-command (command)
  "Run the COMMAND in the current shell-buffer."
  ;; go to end
  (goto-char (point-max))
  ;; Insert the stuff.
  (goto-char (point-max))
  (insert command)
  ;; Send the command.
  (comint-send-input)
  )

(provide 'ede-shell)

;;; ede-shell.el ends here
