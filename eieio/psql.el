;;; psql --- PostgresSQL 'psql' tool interface
;;
;; Copyright (C) 1996, 1998, 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; RCS: $Id: psql.el,v 1.5 1999/02/18 19:15:15 zappo Exp $
;; Keywords: OO postgres95 database
;;                                                                          
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;   This tool uses dbif to inherit a connection type, and uses this
;; to interface with a postgres95 database using the psql command
;; line tool which is run inside a comint buffer.  The output from
;; the program is parsed, and the apropriate dbif-tuple type is then
;; generated.

;;;
;; PSQL backend
;;

;;; History:
;; 

(require 'dbif)

;;; Code:
(defvar psql-command "psql"
  "Command used to start a new interactive postgres SQL client.")

(defvar psql-execute-flags '("-n"	;no readline
			     "-F" "\C-?" ;delete char as the separator.
			     )
  "List of flags to use when starting PSQL.
Don't add -q as this mode require the prompt in order to know when to
execute commands.")

(defclass psql-connection (dbif-connection)
  ((host :initarg :host
	 :initarg nil)
   (port :initarg :port
	 :initarg nil)
   (database :initarg :database
	     :initarg nil))
  "Postgressql connection type inheriting from dbif package.")

(defun psql-database-list ()
  "Fetch a listing of all the available databases."
  (prog1
      (condition-case nil
	  (save-excursion
	    (set-buffer (get-buffer-create "*PSQL DB LIST*"))
	    (shell-command "psql -F \C-? -l" (current-buffer))
	    (goto-char (point-min))
	    (let* ((tuple (psql-parse-table))
		   (databases nil)
		   (ret nil))
	      (setq databases (oref tuple :values))
	      ;;The following line, if run in emacs 20.2 while in edb
	      ;; will seg-fault emacs.  Hmmm.
	      ;;(setq databases (oref tuple :value))
	      (while databases
		(setq ret (cons (car (car databases)) ret)
		      databases (cdr databases)))
	      (nreverse ret)))
	;(error nil)
	)
    (kill-buffer "*PSQL DB LIST*")
    ))

(defun psql-set-db (database &optional host port)
  "Create a new connection to DATABASE in postgres.
The server is on HOST via PORT."
  (interactive "sDatabase: \nsHost: \nsPort: ")
  (if (string= database "") (setq database nil))
  (if (string= host "") (setq host nil))
  (if (string= port "") (setq port nil))
  (let ((nb (get-buffer-create (format "*psql-%s-%s-%s*"
				       (if host host (system-name))
				       (if port port "5432")
				       (if database database
					 (user-login-name)))))
	(params nil))
    (save-excursion
      (set-buffer nb)
      (erase-buffer)
      (comint-mode)			;in case user need to look at it
      (setq comint-prompt-regexp "[a-zA-Z0-9]+=>"
	    ;; we should eventually complete on database symbols, but
	    ;; only if this becomes an interactive mode
	    comint-dynamic-complete-functions nil)
      (if port
	  (progn
	    (setq params (cons port params))
	    (setq params (cons "-p" params))))
      (if host
	  (progn
	    (setq params (cons host params))
	    (setq params (cons "-h" params))))
      (if database
	  (progn
	    (setq params (cons database params))
	    (setq params (cons "-d" params))))
      (comint-exec (current-buffer)
		   (format "psql-proc-%s-%s-%s"
			   (if host host (system-name))
			   (if port port "5432")
			   (if database database
			     (user-login-name)))
		   psql-command
		   nil
		   (append psql-execute-flags params))
      (make-local-variable 'dbif-table-list)
      (setq dbif-table-list nil)
      (make-instance psql-connection
		     :buffer nb
		     :host host
		     :port port
		     :database database))))

(defmethod dbif-get-table-info ((dbbuff psql-connection) tablename)
  "Return a psql-tuple object with information about tables in this database.
Argument DBBUFF specifies the current connection.
Argument TABLENAME is the name of the table to query."
  (save-excursion
    (dbif-exec dbbuff (format "\\d %s" tablename))))

(defmethod dbif-get-table-list ((dbbuff psql-connection))
  "Get a list of available tables from the database specified in DBBUFF."
  (save-excursion
    (set-buffer (oref dbbuff buffer))
    (if (not dbif-table-list)
	(setq dbif-table-list (dbif-exec dbbuff "\\d")))
    dbif-table-list))

(defmethod dbif-exec ((dbbuff psql-connection) command)
  "Execute the SQL or PSQL command and grab its output.
The output is checked, and if tabular data results, a psql-tuple object
is returned.
DBBUFF is the current connection.
COMMAND should be a string which will execute the PSQL command.  ie,
SQL should end in a semi-colon, \ commands don't.  A carriage return
is supplied by `comint-mode'"
  (save-excursion
    (set-buffer (oref dbbuff buffer))
    ;; first, wait for the prompt to appear...
    (goto-char (point-max))
    (while (not (save-excursion
		  (beginning-of-line)
		  (looking-at comint-prompt-regexp)))
      (message "Waiting for PSQL prompt...")
      (accept-process-output (get-buffer-process (current-buffer))))
    ;; Insert the command to be executed.  Always place the "execute" part
    ;; into the command.
    (insert command (if (string-match "^\\\\d" command) "" ";"))
    (let ((start-pos (1+ (point))) (tt 0) tvv)
      (comint-send-input)
      ;; wait for the prompt to come back
      (while (not (save-excursion
		    (goto-char (point-max))
		    (beginning-of-line)
		    (looking-at comint-prompt-regexp)))
	(message "Waiting for PSQL output...")
	(accept-process-output (get-buffer-process (current-buffer)) 1))
      (save-restriction
	(narrow-to-region start-pos (save-excursion
				      (goto-char (point-max))
				      (beginning-of-line) (point)))
	;; Now execute commands to parse the output...
	(goto-char (point-min))
	;; The -------- line is not always right there.
	(setq tvv
	      (if (save-excursion (re-search-forward "\+?----" nil t))
		  (psql-parse-table)
		(psql-parse-message)))
	;; Remove extraneous text
	(delete-region (point-min) (point-max)))
      (goto-char (point-max))
      tvv)))

;;; Table Parsing
;;
(defun psql-parse-message ()
  "Read characters just after point treating it as PSQL output message.
Return a message string"
  (buffer-substring (point) (save-excursion (end-of-line) (point))))

(defun psql-parse-table ()
  "Read characters just after point treating it as a PSQL output table.
We must be able to distinguish between tables w/ the DEL character, and
those with pipes.  The pipe ones are safe, where the DEL ones are not."
  (message "Parsing PSQL table...")
  ;; The -1 accounts for title area
  (let ((namelst nil) (datalst nil) (sizelst nil) sublst numcol (numrow 0)
	(sep "\C-?"))
    (while (looking-at "^\\s-*$") (forward-line 1) (beginning-of-line))
    (if (looking-at "\\s-*\\(Database\\|Table\\)\\s-+=")
	(progn
	  (setq sep "|")
	  (re-search-forward "--+\\+?$" nil t)
	  (end-of-line)
	  ;; first, read in the titles of the columns
	  (forward-char 1)))
    (while (re-search-forward
	    (concat "\\( *\\([A-Za-z0-9_]+\\) *\\)\\($\\|" sep "\\)")
	    (save-excursion (end-of-line) (point)) t)
      (setq namelst (cons (match-string 2) namelst)
	    sizelst (cons (- (match-end 1) (match-beginning 1)) sizelst)))
    (setq numcol (length namelst)
	  sizelst (reverse sizelst))
    (forward-line 1)
    (beginning-of-line)
    (while (and (not (eobp)) (or (not dbif-max-parse)
				 (> dbif-max-parse numrow)))
      (if (or (looking-at "\\s-*$") (looking-at "([0-9]+ Rows)$")
	      (looking-at "\\( *\\+\\)?\\(-+\\+\\)+"))
	  nil
	(let (colcnt)
	  (setq colcnt 0
		sublst nil
		numrow (1+ numrow))
	  (if (looking-at (concat "[ ]*" sep "[ ]+"))
	      (goto-char (match-end 0)))
	  (while (< colcnt numcol)
	    (if (looking-at
		 (concat
		  "[ ]*\\([^" sep "]*[^ \t\n" sep "]\\)[ ]*\\("
		  (if (= colcnt (1- numcol)) "\n\\||" sep)
		  "\\)"))
		(progn
		  (setq sublst (cons (match-string 1) sublst))
		  (goto-char (match-beginning 2))
		  (if (looking-at sep) (goto-char (match-end 0))))
	      ;; In this case, there may be nothing in between pipes
	      ;; at all!
	      (if (looking-at (concat sep "[ \t]+\\(" sep "\\)"))
		  (progn
		    (setq sublst (cons "" sublst))
		    (goto-char (match-beginning 1)))
		(error "Psql-parse-table: could not parse table!")))
	    ;; an error prevents us from getting here...
	    (if (< (nth colcnt sizelst) (length (car sublst)))
		(setcar (nthcdr colcnt sizelst) (length (car sublst))))
	    (setq colcnt (1+ colcnt)))
	  (if sublst
	      (setq datalst (cons (reverse sublst) datalst)))))
	(forward-line 1)
	(beginning-of-line)
	)
    ;; return the formed list
    (message "Parsing PSQL table...done (%d items)" (length namelst))
    (dbif-tuple "tmp-tuple"
		:headers (reverse namelst)
		:maxwidths sizelst
		:values (reverse datalst))))

(provide 'psql)

;;; psql.el ends here
