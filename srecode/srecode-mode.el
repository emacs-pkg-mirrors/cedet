;;; srecode-mode.el --- Minor-mode for inserting templates into other files.

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This uses a bunch of semantic conveniences for making a minor mode.

(require 'srecode)
(require 'srecode-insert)
(require 'srecode-find)
(require 'senator)
(require 'wisent)

;;; Code:
(defcustom global-srecode-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings."
  :group 'srecode
  :type 'boolean
  :require 'srecode-mode
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-srecode-minor-mode (if val 1 -1))))

(defvar srecode-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings.")
(make-variable-buffer-local 'srecode-minor-mode)

(defcustom srecode-minor-mode-hook nil
  "Hook run at the end of the function `srecode-minor-mode'."
  :group 'srecode
  :type 'hook)

;; We don't want to waste space.  There is a menu after all.
;;(add-to-list 'minor-mode-alist '(srecode-minor-mode ""))

(defvar srecode-prefix-key [(control ?c) ?/]
  "The common prefix key in srecode minor mode.")

(defvar srecode-prefix-map
  (let ((km (make-sparse-keymap)))
    ;; Basic template codes
    (define-key km "/" 'srecode-insert)
    (define-key km "." 'srecode-insert-again)
    (define-key km "E" 'srecode-edit)
    ;; Template indirect binding
    (let ((k ?a))
      (while (<= k ?z)
	(define-key km (format "%c" k) 'srecode-bind-insert)
	(setq k (1+ k))))
    ;; Template applications
    (define-key km "G" 'srecode-insert-getset)
    km)
  "Keymap used behind the srecode prefix key in in srecode minor mode.")

(defvar srecode-menu-bar
  (list
   "SRecoder"
   (senator-menu-item
    ["Insert Template"
     srecode-insert
     :active t
     :help "Insert a template by name."
     ])
   (senator-menu-item
    ["Insert Template Again"
     srecode-insert-again
     :active t
     :help "Run the same template as last time again."
     ])
   (senator-menu-item
    ["Edit Template"
     srecode-edit
     :active t
     :help "Edit a template for this language by name."
     ])
   "---"
   '( "Insert ..." :filter srecode-minor-mode-templates-menu )
   "---"
   (senator-menu-item
    ["Dump Tables"
     srecode-dump-templates
     :active t
     :help "Dump the current template table."
     ])
   )
  "Menu for srecode minor mode.")

(defvar srecode-minor-menu nil
  "Menu keymap build from `srecode-menu-bar'.")

(defvar srecode-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km srecode-prefix-key srecode-prefix-map)
    (easy-menu-define srecode-minor-menu km "Srecode Minor Mode Menu"
                      srecode-menu-bar)
    km)
  "Keymap for srecode minor mode.")

;;;###autoload
(defun srecode-minor-mode (&optional arg)
  "Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if srecode-minor-mode 0 1))))
  (setq srecode-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not srecode-minor-mode)))
  (run-hooks 'srecode-minor-mode-hook)
  srecode-minor-mode)

;;;###autoload
(defun global-srecode-minor-mode (&optional arg)
  "Toggle global use of srecode minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-srecode-minor-mode
        (semantic-toggle-minor-mode-globally
         'srecode-minor-mode arg)))

;; Use the semantic minor mode magic stuff.
(semantic-add-minor-mode 'srecode-minor-mode
			 ""
			 srecode-mode-map)

;;; Menu Filters
;;
(defun srecode-minor-mode-templates-menu (menu-def)
  "Create a menu item of cascading filters active for this mode.
MENU-DEF is the menu to bind this into."
  (srecode-load-tables-for-mode major-mode)
  (let* ((modetable (srecode-get-mode-table major-mode))
	 (subtab (oref modetable :tables))
	 (context (car-safe (srecode-calculate-context)))
	 (active nil)
	 (ltab nil)
	 (temp nil)
	 (alltabs nil)
	 )
    (while subtab
      (setq ltab (oref (car subtab) templates))
      (while ltab
	(setq temp (car ltab))
	
	;; Do something with this template.

	(let* ((ctxt (oref temp context))
	       (ctxtcons (assoc ctxt alltabs))
	       (bind (if (slot-boundp temp 'binding)
			 (oref temp binding)))
	       (name (object-name-string temp)))

	  (when (not ctxtcons)
	    (if (string= context ctxt)
		;; If this context is not in the current list of contexts
		;; is equal to the current context, then manage the
		;; active list instead
		(setq active
		      (setq ctxtcons (or active (cons ctxt nil))))
	      ;; This is not an active context, add it to alltabs.
	      (setq ctxtcons (cons ctxt nil))
	      (setq alltabs (cons ctxtcons alltabs))))

	  (let ((new (vector
		      (if bind
			  (concat name "   (" bind ")")
			name)
		      `(lambda () (interactive)
			 (srecode-insert (concat ,ctxt ":" ,name)))
		      t)))

	    (setcdr ctxtcons (cons
			      new
			      (cdr ctxtcons)))))

	(setq ltab (cdr ltab)))
      (setq subtab (cdr subtab)))

    ;; Now create the menu
    (easy-menu-filter-return
     (easy-menu-create-menu
      "Semantic Recoder Filters"
      (append (cdr active)
	      alltabs)
      ))
    ))

;;; Minor Mode commands
;;
(defun srecode-bind-insert ()
  "Bound insert for Srecode macros.
This command will insert whichever srecode template has a binding
to the current key."
  (interactive)
  (let* ((k last-command-char)
	 (ctxt (srecode-calculate-context))
	 ;; Find the template with the binding K
	 (template (srecode-template-get-table-for-binding
		    (srecode-table) k ctxt)))
    ;; test it.
    (when (not template)
      (error "No template bound to %c" k))
    ;; insert
    (srecode-insert template)
    ))

(defun srecode-edit (template-name)
  "Switch to the template buffer for TEMPLATE-NAME.
Template is chosen based on the mode of the starting buffer."
  (interactive (list (srecode-read-template-name "Template Name: ")))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
    (let ((newdict (srecode-create-dictionary))
	  (temp (srecode-template-get-table (srecode-table) template-name)))
      (if (not temp)
	  (error "No Template named %s" template-name))
      ;; We need a template specific table, since tables chain.
      (let ((tab (oref temp :table))
	    (names nil)
	    (ctxt nil))
	(find-file (oref tab :file))
	(setq names (semantic-find-tags-by-name (oref temp :object-name)
						(current-buffer)))
	(cond ((= (length names) 1)
	       (semantic-go-to-tag (car names))
	       (semantic-momentary-highlight-tag (car names)))
	      ((> (length names) 1)
	       (let* ((ctxt (semantic-find-tags-by-name (oref temp :context)
							(current-buffer)))
		      (cls (semantic-find-tags-by-class 'context ctxt))
		      )
		 (while (and names
			     (< (semantic-tag-start (car names))
				(semantic-tag-start (car cls))))
		   (setq names (cdr names)))
		 (if names
		     (progn
		       (semantic-go-to-tag (car names))
		       (semantic-momentary-highlight-tag (car names)))
		   (error "Can't find template %s" template-name))
		 ))
	      (t (error "Can't find template %s" template-name)))
	)))

  

(provide 'srecode-mode)

;;; srecode-mode.el ends here
