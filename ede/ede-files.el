;;; ede-files.el --- Associate projects with files and directories.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-files.el,v 1.1 2008/12/09 20:00:20 zappo Exp $

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
;; Directory and File scanning and matching functions.
;;
;; Basic Model:
;;
;; A directory belongs to a project if a ede-project-autoload structure
;; matches your directory.
;;
;; A toplevel project is one where there is no active project above
;; it.  Finding the toplevel project involves going up a directory
;; till no ede-project-autoload structure matches.
;; 

(require 'ede)

;;; Code:

;;; Placeholders for ROOT directory scanning on base objects
;;
(defmethod ede-project-root ((this ede-project-placeholder))
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems."
  (oref this rootproject))

(defmethod ede-project-root-directory ((this ede-project-placeholder)
				       &optional file)
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  It is ignored in preference
of the anchor file for the project."
  (file-name-directory (expand-file-name (oref this file))))


(defmethod ede-project-root ((this ede-project-autoload))
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

(defmethod ede-project-root-directory ((this ede-project-autoload)
				       &optional file)
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  If there is no FILE, use
the current buffer."
  (when (not file)
    (setq file default-directory))
  (when (slot-boundp this :proj-root)
    (let ((rootfcn (oref this proj-root)))
      (when rootfcn
	(condition-case err
	    (funcall rootfcn file)
	  (error 
	   (funcall rootfcn)))
	))))

;;; DIRECTORY-PROJECT-P
;;
;; For a fresh buffer, or for a path w/ no open buffer, use this
;; routine to determine if there is a known project type here.
(defun ede-directory-project-p (dir)
  "Return a project description object if DIR has a project.
This depends on an up to date `ede-project-class-files' variable."
  (let ((types ede-project-class-files)
	(ret nil))
    ;; Loop over all types, loading in the first type that we find.
    (while (and types (not ret))
      (if (ede-dir-to-projectfile (car types) dir)
	  (progn
	    ;; We found one!  Require it now since we will need it.
	    (require (oref (car types) file))
	    (setq ret (car types))))
      (setq types (cdr types)))
    ret))

;;; TOPLEVEL
;;
;; These utilities will identify the "toplevel" of a project.
;;
(defun ede-toplevel-project-or-nil (path)
  "Starting with PATH, find the toplevel project directory, or return nil.
nil is returned if the current directory is not a part ofa project."
  (if (ede-directory-project-p path)
      (ede-toplevel-project path)
    nil))

(defun ede-toplevel-project (path)
  "Starting with PATH, find the toplevel project directory."
  (let* ((toppath (expand-file-name path))
	 (newpath toppath)
	 (proj (ede-directory-project-p path))
	 (ans nil))
    (if proj
	;; If we already have a project, ask it what the root is.
	(setq ans (ede-project-root-directory proj)))

    ;; If PROJ didn't know, or there is no PROJ, then

    ;; Loop up to the topmost project, and then load that single
    ;; project, and it's sub projects.  When we are done, identify the
    ;; sub-project object belonging to file.
    (while (and (not ans) newpath proj)
      (setq toppath newpath
	    newpath (ede-up-directory toppath))
      (when newpath
	(setq proj (ede-directory-project-p newpath)))

      (when proj
	;; We can home someone in the middle knows too.
	(setq ans (ede-project-root-directory proj)))
      )
    (or ans toppath)))

;;; TOPLEVEL PROJECT
;;
;; The toplevel project is a way to identify the EDE structure that belongs
;; to the top of a project.

(defun ede-toplevel (&optional subproj)
  "Return the ede project which is the root of the current project.
Optional argument SUBPROJ indicates a subproject to start from
instead of the current project."
  (let* ((cp (or subproj (ede-current-project)))
	 )
    (or (and cp (ede-project-root cp))
	(progn
	  (while (ede-parent-project cp)
	    (setq cp (ede-parent-project cp)))
	  cp))))

;;; UTILITIES
;;

(defun ede-up-directory (dir)
  "Return a path that is up one directory.
Argument DIR is the directory to trim upwards."
  (let* ((fad (directory-file-name dir))
	 (fnd (file-name-directory fad)))
    (if (string= dir fnd) ; This will catch the old string-match against
			  ; c:/ for DOS like systems.
	nil
      fnd)))
  
(provide 'ede-files)
;;; ede-files.el ends here
