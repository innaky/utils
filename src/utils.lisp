(defpackage utils
  (:use :cl)
  (:export :random-string
	   :random-letters-and-numbers
	   :directory-p
	   :file-exists-p
	   :pg-reverse
	   :list+
	   :combine-cars
	   :mklist
	   :my-member
	   :rfember
	   :rember
	   :firsts))

(in-package :utils)

;; lists
(defun pg-reverse (lst)
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n)) lst))

(defun combine-cars (lst1 lst2)
  "Return a list with sublist, the sublist contain the cars (recursively) of
the `ls1' and `lst2'."
  (if (equal nil lst1)
      nil
      (cons (cons (car lst1)
		  (cons (car lst2) nil))
	    (combine-cars (cdr lst1)
			  (cdr lst2)))))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun firsts (lst)
  (cond
    ((null lst) nil)
    (t (listp (car lst))
       (cons (caar lst) (firsts (cdr lst))))))

(defun my-member (elem lst)
  (cond
    ((null lst) nil)
    (t (or (equal (car lst) elem)
	   (my-member elem (cdr lst))))))

(defun rfember (elem lst)
  (cond
    ((null lst) nil)
    (t (cond
	 ((equal (car lst) elem) (cdr lst))
	 (t (cons (car lst) (rfember elem (cdr lst))))))))

(defun rember (elem lst)
  (cond
    ((null lst) nil)
    (t (cond
	 ((equal (car lst) elem) (rember elem (cdr lst)))
	 (t (cons (car lst) (rember elem (cdr lst))))))))

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
