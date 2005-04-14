;;; eieio-testsinvoke.el -- eieio tests for method invokation

;;;
;; Copyright (C) 2005 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-test-methodinvoke.el,v 1.4 2005/04/14 18:47:18 zappo Exp $
;; Keywords: oop, lisp, tools
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
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;  
;; Test method invocation order.  From the common lisp reference
;; manual:
;;
;; QUOTE:
;; - All the :before methods are called, in most-specific-first
;;   order.  Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :before method.
;;
;; - The most specific primary method is called. Inside the body of a
;;   primary method, call-next-method may be used to call the next
;;   most specific primary method. When that method returns, the
;;   previous primary method can execute more code, perhaps based on
;;   the returned value or values. The generic function no-next-method
;;   is invoked if call-next-method is used and there are no more
;;   applicable primary methods. The function next-method-p may be
;;   used to determine whether a next method exists. If
;;   call-next-method is not used, only the most specific primary
;;   method is called.
;;   
;; - All the :after methods are called, in most-specific-last order.
;;   Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :after method.
;;

(defvar eieio-test-method-order-list nil
  "List of symbols stored during method invocation.")

(defun eieio-test-method-store ()
  "Store current invocation class symbol in the invocation order list."
  (let* ((keysym (aref [ nil :BEFORE :PRIMARY :AFTER ] 
		       (or eieio-generic-call-key 0)))
	 (c (list eieio-generic-call-methodname keysym scoped-class)))
    (setq eieio-test-method-order-list
	  (cons c eieio-test-method-order-list))))

(defun eieio-test-match (rightanswer)
  "Do a test match."
  (if (equal rightanswer eieio-test-method-order-list)
      t
    (error "Test Failed!")))

;;; This Example was submitted by darkman:
;;
;; drkm <darkman_spam@yahoo.fr>
(defclass A () ())
(defclass AA (A) ())
(defclass AAA (AA) ())

(defmethod F :BEFORE ((p A))
  (eieio-test-method-store))
(defmethod F :BEFORE ((p AA))
  (eieio-test-method-store))
(defmethod F :BEFORE ((p AAA))
  (eieio-test-method-store))

(defmethod F ((p A))
  (eieio-test-method-store))
(defmethod F ((p AA))
  (eieio-test-method-store))

(defmethod F :AFTER ((p A))
  (eieio-test-method-store))
(defmethod F :AFTER ((p AA))
  (eieio-test-method-store))
(defmethod F :AFTER ((p AAA))
  (eieio-test-method-store))

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :BEFORE AAA)
	     (F :BEFORE AA)
	     (F :BEFORE A)
	     ;; Not primary A method
	     (F :PRIMARY AA)
	     ;; No call-next-method in AA to get to A.
	     (F :AFTER A)
	     (F :AFTER AA)
	     (F :AFTER AAA)
	     )))
  (F (AAA nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (eieio-test-match ans))

(defmethod G :BEFORE ((p A))
  (eieio-test-method-store))
(defmethod G :BEFORE ((p AAA))
  (eieio-test-method-store))

(defmethod G ((p A))
  (eieio-test-method-store))

(defmethod G :AFTER ((p A))
  (eieio-test-method-store))
(defmethod G :AFTER ((p AAA))
  (eieio-test-method-store))


(let ((eieio-test-method-order-list nil)
      (ans '(
	     (G :BEFORE AAA)
	     (G :BEFORE A)
	     ;; Not primary A method
	     (G :PRIMARY A)
	     ;; No call-next-method in AA to get to A.
	     (G :AFTER A)
	     (G :AFTER AAA)
	     )))
  (G (AAA nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (eieio-test-match ans))

;;; Test Multiple Inheritance.
;;
(defclass B-base1 () ())
(defclass B-base2 () ())
(defclass B (B-base1 B-base2) ())

(defmethod F :BEFORE ((p B-base1))
  (eieio-test-method-store))

(defmethod F :BEFORE ((p B-base2))
  (eieio-test-method-store))

(defmethod F :BEFORE ((p B))
  (eieio-test-method-store))

(defmethod F ((p B))
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p B-base1))
  (eieio-test-method-store))

(defmethod F ((p B-base2))
  (eieio-test-method-store))

(defmethod F :AFTER ((p B-base1))
  (eieio-test-method-store))

(defmethod F :AFTER ((p B-base2))
  (eieio-test-method-store))

(defmethod F :AFTER ((p B))
  (eieio-test-method-store))

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :BEFORE B)
	     (F :BEFORE B-base1)
	     (F :BEFORE B-base2)
	     (F :PRIMARY B)
	     (F :PRIMARY B-base1)
	     ;; (F :PRIMARY B-base2)
	     ;; Note, call next method would not call B-base2 unless
	     ;; there was a call-next-method in B-base1.
	     (F :AFTER B-base2)
	     (F :AFTER B-base1)
	     (F :AFTER B)
	     )))
  (F (B nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  ;;(message "%S" eieio-test-method-order-list)
  (eieio-test-match ans)
  )

(defmethod H :STATIC ((class A))
  "No need to do work in here."
  'moose)

;; Both of these situations should succeed.
(H A)
(H (A nil))
