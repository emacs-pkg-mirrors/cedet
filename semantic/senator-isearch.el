;;; senator-isearch.el --- SEmantic NAvigaTOR isearch advices

;; Copyright (C) 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 04 Dec 2000
;; Version: 1.0
;; Keywords: tools, syntax
;; VC: $Id: senator-isearch.el,v 1.2 2000/12/08 16:18:32 david_ponce Exp $

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
;; This library advices isearch (and ishl) to allow overriding of the
;; basic search functions used by `isearch-search' and
;; `isearch-lazy-highlight-search' (or `ishl-search').
;;
;; This feature is needed by the SEmantic NAvigaTOR library to extend
;; isearch with an incremental semantic search mode. When isearch is
;; switched to this mode it searches only in language tokens in the
;; current buffer.

;; THIS CODE HAS ONLY BEEN TESTED WITH GNU EMACS 20.7 AND 21.0 AND
;; XEMACS 21.1

;;; Change Log:

;; $Log: senator-isearch.el,v $
;; Revision 1.2  2000/12/08 16:18:32  david_ponce
;; A bunch of XEmacs compatibility code!
;;
;; Revision 1.1  2000/12/05 11:15:14  david_ponce
;; Initial revision needed by senator.el 1.7 (version 2.0) and above.
;;

;;; Code:

;;;;
;;;; Provider of the core search function used by isearch.
;;;;

(defcustom isearch-search-handler-provider 'isearch-default-search-handler
  "Function providing the basic search handlers.
The default function `isearch-default-search-handler' provides one the
built-ins `search-forward', `search-backward', `word-search-forward',
`word-search-backward', `re-search-forward' or `re-search-backward'
depending on current values of variables `isearch-forward',
`isearch-regexp' and `isearch-word'.  Any other user's defined basic
search handler that `isearch-search-handler-provider' returns must
accept the same arguments and have the same behaviour as the above
built-in ones."
  :group 'isearch
  :type 'function)

(defun isearch-default-search-handler ()
  "Return the actual search function used by `isearch-search'.
That is one of the built-in functions `search-forward',
`search-backward', `word-search-forward', `word-search-backward',
`re-search-forward' or `re-search-backward' depending on current
values of variables `isearch-forward', `isearch-regexp' and
`isearch-word'."
  (cond (isearch-word
         (if isearch-forward
             'word-search-forward
           'word-search-backward))
        (isearch-regexp
         (if isearch-forward
             're-search-forward
           're-search-backward))
        (t
         (if isearch-forward
             'search-forward
           'search-backward))))

;;;;
;;;; Advice of `isearch-search' to use the core search function
;;;; provider.
;;;; This feature will probably be included in isearch starting with
;;;; GNU Emacs 21.2.
;;;; 

(cond ;; Compatibility between GNU Emacs and XEmacs
 
 ((featurep 'xemacs) ;; XEmacs stuff

  ;; Ignore this function used by senator but not provided by XEmacs
  (defalias 'isearch-lazy-highlight-cleanup 'ignore)

  ;; Provide `isearch-update-ring' function (from 21.1.9 isearch-mode.el)
  (defun isearch-update-ring (string &optional regexp)
    "Add STRING to the beginning of the search ring.
REGEXP says which ring to use."
    (if (> (length string) 0)
        ;; Update the ring data.
        (if regexp 
            (if (not (setq regexp-search-ring-yank-pointer
                           (member string regexp-search-ring)))
                (progn
                  (setq regexp-search-ring
                        (cons string regexp-search-ring)
                        regexp-search-ring-yank-pointer regexp-search-ring)
                  (if (> (length regexp-search-ring) regexp-search-ring-max)
                      (setcdr (nthcdr (1- regexp-search-ring-max) regexp-search-ring)
                              nil))))
          (if (not (setq search-ring-yank-pointer
                         ;; really need equal test instead of eq.
                         (member string search-ring)))
              (progn
                (setq search-ring (cons string search-ring)
                      search-ring-yank-pointer search-ring)
                (if (> (length search-ring) search-ring-max)
                    (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))))

  ;; `isearch-search' from 21.1.9 isearch-mode.el
  (defadvice isearch-search (around senator activate)
      ;; Do the search with the current search string.
      (isearch-message nil t)
      (isearch-fix-case)
      (condition-case lossage
          (let ((inhibit-quit nil)
                (case-fold-search isearch-case-fold-search))
            (if isearch-regexp (setq isearch-invalid-regexp nil))
            (setq isearch-success
                  (funcall
                   (if isearch-search-handler-provider
                       (funcall isearch-search-handler-provider)
                     (isearch-default-search-handler))
                   isearch-string nil t))
            (setq isearch-just-started nil)
            (if isearch-success
                (setq isearch-other-end
                      (if isearch-forward (match-beginning 0) (match-end 0)))))

        (quit (setq unread-command-event (character-to-event (quit-char)))
              (setq isearch-success nil))

        (invalid-regexp 
         (setq isearch-invalid-regexp (car (cdr lossage)))
         (if (string-match
              "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
              isearch-invalid-regexp)
             (setq isearch-invalid-regexp (gettext "incomplete input")))))

      (if isearch-success
          nil

       ;; If we're being run inside a keyboard macro, then the call to
       ;; ding will signal an error (to terminate the macro).  We must
    ;; turn off isearch-mode first, so that we aren't still in isearch
      ;; mode after the macro exits.  Note that isearch-recursive-edit
        ;; must not be true if a keyboard macro is executing.
        (if (and executing-kbd-macro (not defining-kbd-macro))
            (progn
              (isearch-done)
              (ding nil 'isearch-failed)))

        ;; Ding if failed this time after succeeding last time.
        (and (nth 3 (car isearch-cmds))
             (ding nil 'isearch-failed))
        (goto-char (nth 2 (car isearch-cmds)))))
  
  ) ;; End of XEmacs stuff

 (t ;; GNU Emacs stuff
  
  ;; `isearch-search' from 20.7 (not changed in 21.0) isearch.el
  (defadvice isearch-search (around senator activate)
    ;; Do the search with the current search string.
    (isearch-message nil t)
    (if (and (eq isearch-case-fold-search t) search-upper-case)
        (setq isearch-case-fold-search
              (isearch-no-upper-case-p isearch-string isearch-regexp)))
    (condition-case lossage
        (let ((inhibit-point-motion-hooks search-invisible)
              (inhibit-quit nil)
              (case-fold-search isearch-case-fold-search)
              (retry t))
          (if isearch-regexp (setq isearch-invalid-regexp nil))
          (setq isearch-within-brackets nil)
          (while retry
            (setq isearch-success
                  (funcall
                   (if isearch-search-handler-provider
                       (funcall isearch-search-handler-provider)
                     (isearch-default-search-handler))
                   isearch-string nil t))
            ;; Clear RETRY unless we matched some invisible text
            ;; and we aren't supposed to do that.
            (if (or (eq search-invisible t)
                    (not isearch-success)
                    (bobp) (eobp)
                    (= (match-beginning 0) (match-end 0))
                    (not (isearch-range-invisible
                          (match-beginning 0) (match-end 0))))
                (setq retry nil)))
          (setq isearch-just-started nil)
          (if isearch-success
              (setq isearch-other-end
                    (if isearch-forward (match-beginning 0) (match-end 0)))))
          
      (quit (isearch-unread ?\C-g)
            (setq isearch-success nil))
          
      (invalid-regexp
       (setq isearch-invalid-regexp (car (cdr lossage)))
       (setq isearch-within-brackets (string-match "\\`Unmatched \\["
                                                   isearch-invalid-regexp))
       (if (string-match
            "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
            isearch-invalid-regexp)
           (setq isearch-invalid-regexp "incomplete input")))
      (error
       ;; stack overflow in regexp search.
       (setq isearch-invalid-regexp (car (cdr lossage)))))

    (setq ad-return-value
          (if isearch-success
              nil
            ;; Ding if failed this time after succeeding last time.
            (and (nth 3 (car isearch-cmds))
                 (ding))
            (goto-char (nth 2 (car isearch-cmds))))))
  
  ;; Advice of the isearch lazy highlighting feature to use the core
  ;; search function provider. Lazy highlighting is part of isearch
  ;; for GNU Emacs 21 or provided by the optional ishl.el library for
  ;; Emacs 20.
  ;; Not currently implemented for XEmacs (it seems that ishl does not
  ;; work).

  (if (fboundp 'isearch-lazy-highlight-search)

      ;; GNU Emacs 21 lazy highlighting
      (defadvice isearch-lazy-highlight-search (around senator activate)
        "Search ahead for the next or previous match, for lazy highlighting.
Attempt to do the search exactly the way the pending isearch would."
        (let ((case-fold-search isearch-case-fold-search))
          (setq ad-return-value
                (funcall (if isearch-search-handler-provider
                             (funcall isearch-search-handler-provider)
                           (isearch-default-search-handler))
                         isearch-string
                         (if isearch-forward
                             (if isearch-lazy-highlight-wrapped
                                 isearch-lazy-highlight-start
                               nil)
                           (if isearch-lazy-highlight-wrapped
                               isearch-lazy-highlight-end
                             nil))
                         t))))
    
    ;; Maybe use ishl to provide lazy highlighting
    (if (not (condition-case nil
                 (require 'ishl)
               (error nil)))

        ;; No ishl so ignore this function used by senator
        (defalias 'isearch-lazy-highlight-cleanup 'ignore)

      ;; ishl provided, advice it (from ishl.el 1.5)
      (defadvice ishl-search (around senator activate)
        (let ((case-fold-search isearch-case-fold-search))
          (setq ad-return-value
                (funcall (if isearch-search-handler-provider
                             (funcall isearch-search-handler-provider)
                           (isearch-default-search-handler))
                         isearch-string
                         (if isearch-forward
                             (if ishl-wrapped ishl-start nil)
                           (if ishl-wrapped ishl-end nil))
                         t))))

      ;; provide this function used by senator
      (defalias 'isearch-lazy-highlight-cleanup 'ishl-cleanup))
    
    ) ;; End of ishl stuff

  ) ;; End of GNU Emacs stuff

 ) ;; End of compatibility stuff

(provide 'senator-isearch)

;;; senator-isearch.el ends here
