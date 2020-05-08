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
	   :firsts
	   :replace-atom
	   :full-replace-atom
	   :insert-post-first
	   :insert-post-full
	   :compress
	   :uncompress
	   :string-to-charlst
	   :match?))

(in-package :utils)

(defun match? (elem lst)
  "`elem' exist in `lst'?. Return TRUE if exist else return NIL."
  (if (equal nil lst)
      nil
      (or (equal elem (car lst))
	  (match? elem (cdr lst)))))

(defun string-to-charlst (long-str &optional (position 0))
  "The input is a string the output a list of characters, run from character 0
for default or for any other character, minor to length string."
  (if (not (numberp position))
      (format t "~A is not type numb~%" position)
      (if (or (>= position (length long-str)) (< position 0))
	  nil
	  (let ((end (length long-str)))
	    (if (equal position end)
		nil
		(cons (char long-str position)
		      (string-to-charlst long-str (+ 1 position))))))))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))


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
  "This function is only for documentation, the before `combine-cars' was not util
because with `mapcar' and a lambda with two parameters (or any quantity of parameters) 
solve the problem, this function take two parameters, and compute the same answer 
of the useless last function `combine-cars'."
  (mapcar #'(lambda (x y)
	      (list x y)) lst1 lst2))

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

(defun replace-atom (new old lst)
  (cond
    ((equal lst nil) nil)
    (t (cond
	 ((equal (car lst) old) (cons new (cdr lst)))
	 (t (cons (car lst) (replace-atom new old (cdr lst))))))))

(defun full-replace-atom (new old lst)
  (cond
    ((equal lst nil) nil)
    (t (cond
	 ((equal (car lst) old)
	  (cons new (full-replace-atom new old (cdr lst))))
	 (t (cons (car lst) (full-replace-atom new old (cdr lst))))))))

(defun insert-post-first (new old lst)
  (cond
    ((equal lst nil) nil)
    (t (cond
	 ((equal (car lst) old) (cons old (cons new (cdr lst))))
	 (t (cons (car lst) (insert-post-first new old (cdr lst))))))))

(defun insert-post-full (new old lst)
  (cond
    ((equal lst nil) nil)
    (t (cond
	 ((equal (car lst) old)
	  (cons old (cons new (insert-post-full new old (cdr lst)))))
	 (t (cons (car lst) (insert-post-full new old (cdr lst))))))))

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
