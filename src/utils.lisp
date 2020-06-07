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
	   :match?
	   :substr?
	   :all-true?
	   :take
	   :match-str?
	   :del-letter))

(in-package :utils)

(defun all-true? (lst)
  "`lst' is a list of bools, return T if all are True, else return Nil"
  (if (equal nil lst)
      t
      (and (car lst) (all-true? (cdr lst)))))

(defun take (num lst)
  "This function take a number and a list and return a list with `num' elements."
  (if (equal lst nil)
      nil
      (if (> num 0)
	  (cons (car lst)
		(take (- num 1) (cdr lst))))))

(defun match-str? (str-to-match generic-str)
  "This function check if `str-to-match' exists inside of `generic-str' in the same order.
`str-to-match' and `generic-str' are list of characters."
  (let ((end (length str-to-match)))
    (if (equal generic-str nil)
	nil
	(if (equal (car str-to-match) (car generic-str))
	    (let ((check (take end (cons (car generic-str) (cdr generic-str)))))
	      (if (all-true? (mapcar #'char-equal str-to-match check))
		  t
		  (match-str? str-to-match (cdr generic-str))))
	    (match-str? str-to-match (cdr generic-str))))))

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

(defun substr? (in-str universe-str)
  "If all elements of `in-str' exists inside `universe-str' the function return T, else
return NIL."
  (let ((substr (string-to-charlst in-str))
	(validator-str (string-to-charlst universe-str)))
    (not (match? nil (mapcar #'(lambda (char)
				 (match? char validator-str))
			     substr)))))

(defun del-letter (letter char-lst)
  "`letter' is a char, `char-lst' is a list of chars, this function return a list of chars without the `letter'."
  (cond ((equal char-lst nil) nil)
	((char= letter (car char-lst)) (del-letter letter (cdr char-lst)))
	(t (cons (car char-lst) (del-letter letter (cdr char-lst))))))

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
(defun lst-to-duples (lst)
  "Input a list, output a list with the elements agrupated in tuples."
  (cond
    ((null lst) nil)
    (t (cons (cons (car lst)
		   (cons (cadr lst) nil))
	     (lst-to-duples (cddr lst))))))

(defun pg-reverse (lst)
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n)) lst))

(defun combine-cars (lst1 lst2)
  (mapcar #'list lst1 lst2))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun firsts (lst)
  (cond
    ((null lst) nil)
    (t (listp (car lst))
       (cons (caar lst) (firsts (cdr lst))))))

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
