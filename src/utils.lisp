(defpackage utils
  (:use :cl)
  (:export :random-string))

(in-package :utils)

;; random-string
(defparameter *characters*
  (concatenate 'string
	       "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"
               "!@#$%&/()=*-_.,;{}+[]"))

(defun random-element (lst)
  (aref lst (random (length lst))))

(defun random-password (length)
  (if (equal length 0)
      nil
      (cons (random-element *characters*)
	    (random-password (- length 1)))))

(defun random-string (length)
    (format nil "~{~a~}" (random-password length)))
