;;; psql - Postgres95 `psql' tool interface
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: psql.el,v 1.2 1996/04/11 21:59:23 zappo Exp $
;;; Keywords: OO postgres95 database
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Updates can be found at:
;;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;;   This tool uses dbif to inherit a connection type, and uses this
;;; to interface with a postgres95 database using the psql command
;;; line tool which is run inside a comint buffer.  The output from
;;; the program is parsed, and the apropriate dbif-tuple type is then
;;; generated.

;;;
;;; PSQL backend
;;;
(require 'dbif)

(defvar psql-command "psql"
  "Command used to start a new interactive postgres SQL client.")

(defvar psql-execute-flags '("-n")
  "List of flags to use when starting PSQL.  Don't add -q as this
mode require the prompt in order to know when to execute commands.")

(defclass psql-connection (dbif-connection)
  ((host :initarg :host
	 :initarg nil)
   (port :initarg :port
	 :initarg nil)
   (database :initarg :database
	     :initarg nil))
  "Postgres 95 connection type inheriting from dbif package.")

(defun psql-set-db (host port database)
  "Create a new connection to the postgres server on HOST via PORT.
Use the USER to connect to DATABASE"
  (interactive "sHost: \nsPort: \nsDatabase: ")
  (if (string= host "") (setq host nil))
  (if (string= port "") (setq port nil))
  (if (string= database "") (setq database nil))
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
      (if database
	  (progn
	    (setq params (cons database params))
	    (setq params (cons "-d" params))))
      (if host
	  (progn
	    (setq params (cons host params))
	    (setq params (cons "-H" params))))
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
  "Returns a psql-tuple object containing information about the tables
in this database."
  (save-excursion
    (dbif-exec dbbuff (format "\\d %s" tablename))))

(defmethod dbif-get-table-list ((dbbuff psql-connection))
  "Get a list of available tables from the database specified in dbbuff"
  (save-excursion
    (set-buffer (oref dbbuff buffer))
    (if (not dbif-table-list)
	(setq dbif-table-list (dbif-exec dbbuff "\\d")))
    dbif-table-list))

(defmethod dbif-exec ((dbbuff psql-connection) command)
  "Execute the SQL or PSQL COMMAND and grab its output.  The output is
checked, and if tabular data results, a psql-tuple object is returned.
COMMAND should be a string which will execute the PSQL command.  ie,
SQL should end in a semi-colon, \ commands don't.  A carriage return
is supplied by comint-mode"
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
    (insert command (if (string-match "\\d" command) "" ";"))
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
  
  
(defun psql-parse-message ()
  "Reads characters from current-buffer just after point treating it
as PSQL output message, and return a message string"
  (buffer-substring (point) (save-excursion (end-of-line) (point))))

(defun psql-parse-table ()
  "Reads characters from current-buffer just after point treating it
as a PSQL output table.  Values are read from the table which is
separated by lines of ---- and cols of |"
  (message "Parsing PSQL table...")
  ;; The -1 accounts for title area
  (let ((namelst nil) (datalst nil) (sizelst nil) sublst numcol (numrow 0))
    ;; first, read in the titles of the columns
    (re-search-forward "--+\\+?$" nil t)
    (forward-char 1)
    (while (re-search-forward "[ ]+\\([A-Za-z0-9_]+\\)[ ]+|" ;no space in name
			      (save-excursion (end-of-line) (point)) t)
      (setq namelst (cons (buffer-substring (match-beginning 1)
					    (match-end 1))
			  namelst))
      (setq sizelst (cons (length (car namelst)) sizelst)))
    (setq numcol (length namelst)
	  sizelst (reverse sizelst))
    (forward-line 1)
    (beginning-of-line)
    (while (and (not (eobp)) (or (not dbif-max-parse) 
				 (> dbif-max-parse numrow)))
      (let (colcnt)
	(setq colcnt 0 sublst nil)
	(if (looking-at "[ ]*|")
	    (progn
	      (setq numrow (1+ numrow))
	      (while (< colcnt numcol)
		(if (looking-at
		     "[ ]*|[ ]+\\([^|]*[^ \t|]\\)[ ]*\\(|\\)")
		    (progn
		      (setq sublst (cons
				    (buffer-substring (match-beginning 1)
						      (match-end 1))
				    sublst))
		      (goto-char (match-beginning 2)))
		  ;; In this case, there may be an embeded CR
		  ;; in the field just printed...
		  (if (looking-at
		       "|[ ]+\\([^|]*[^ \t\n]\\)[ ]*\\(|\\)")
		      (progn
			(setq sublst (cons
				      (buffer-substring (match-beginning 1)
							(match-end 1))
				      sublst))
			(goto-char (match-end 1)))
		    ;; In this case, there may be nothing in between pipes
		    ;; at all!
		    (if (looking-at "|[ \t\n]+\\(|\\)")
			(progn
			  (setq sublst (cons "" sublst))
			  (goto-char (match-beginning 1)))
		      (error "psql-parse-table: could not parse table!"))))
		;; an error prevents us from getting here...
		(if (< (nth colcnt sizelst) (length (car sublst)))
		    (setcar (nthcdr colcnt sizelst) (length (car sublst))))
		(setq colcnt (1+ colcnt)))))
	(if sublst
	    (setq datalst (cons (reverse sublst) datalst))))
	(forward-line 1)
	(beginning-of-line)
	)
    ;; return the formed list
    (message "Parsing PSQL table...done (%d items)" (length namelst))
    (dbif-tuple "tmp-tuple"
		:headers (reverse namelst)
		:maxwidths sizelst
		:values (reverse datalst))))

;;; end of lisp
(provide 'psql)

