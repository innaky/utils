(defpackage utils
  (:use :cl)
  (:export :random-string
	   :random-letters-and-numbers
	   :directory-p
	   :file-exists-p))

(in-package :utils)

;; Filesystem
(defun directory-p (string-pathname)
  (pathnamep
   (cl-fad:directory-exists-p string-pathname)))

(defun file-exists-p (string-filepath)
  (pathnamep
   (probe-file string-filepath)))

;; random-string
(defparameter *characters*
  (concatenate 'string
	       "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"
               "!@#$%&/()=*-_.,;{}+[]"))

(defparameter *letters-numbers*
  (concatenate 'string
	       "abcdefghijklmnopqrstuvwxyz"
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	       "0123456789"))

(defun random-element (lst)
  (aref lst (random (length lst))))

(defun random-password (length)
  (if (equal length 0)
      nil
      (cons (random-element *characters*)
	    (random-password (- length 1)))))

(defun random-num-letters (length)
  (if (equal length 0)
      nil
      (cons (random-element *letters-numbers*)
	    (random-num-letters (- length 1)))))

(defun random-string (length)
  (format nil "~{~a~}" (random-password length)))

(defun random-letters-and-numbers (length)
  (format nil "~{~a~}" (random-num-letters length)))
