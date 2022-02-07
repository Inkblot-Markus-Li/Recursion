;;; Markus Y. Li 
;;; LISP 

;;;FUNCTION NAME: dispnth
;;;DESCRIPTION: Write a function dispnth to display the n-th element of a list. You may assume that the input list is always longer than n.
;;;NOTES:(dispnth '(1 (2 3) 4 5) 2) --> (2 3))

(defun dispnth (L a) 
		(if (eql a 1) (first L) 
		(dispnth (rest L) (- a 1))))

;;;FUNCTION NAME: delnth
;;;DESCRIPTION: Write a function delnth to delete the n-th element of a list. You may assume that the input list is always longer than n.
;;;NOTES:(delnth '(1 2 (3 4) 5) 3) --> (1 2 5)

(defun delnth (L a) 
		(if (= 1 a) (rest L)
		(cons (first L) (delnth (rest L)(- a 1)))))

;;;FUNCTION NAME: remv
;;;DESCRIPTION: Write a function remv to remove elements from a list (including all biple appearance)
;;;NOTES:(remv 'a '(a (b) a c) --> ((B) C)

(defun remv(L a)
		(cond ((null L) nil) 
		((= a (first L))(remv (rest L) a))
		(t (cons (first L)(remv (rest L) a)))))

;;;FUNCTION NAME: remv2
;;;DESCRIPTION: Write a function remv2 to remove given list elements from a list (including biple appearance)
;;;NOTES:(remv2 '(a b) '(a b (a b) c)) --> (A B C)

(defun remv2 (L1 L2) 
		(cond ((null L2) nil) 
		((equal(first L2) L1)(remv2 L1 (rest L2)))
		(t (cons (first L2) (remv2 L1 (rest L2))))))

;;;FUNCTION NAME: remvdub
;;;DESCRIPTION: Write a function remvdub to remove duplicate elements from a list
;;;NOTES:(remvdub '(a b a c b a)) --> (A B C)

(defun remvsame(a L) 
		(cond ((null L) nil)
		((eql (first L) a)(remvsame a (rest L)))
		(t (cons (first L) (remvsame a (rest L))))))

(defun remvdub(L) 
		(if (null L) nil 
		(cons (first L)(remvdub(remvsame(first L)(rest L))))))

;;;FUNCTION NAME: remvdub2
;;;DESCRIPTION: Write a function remvdub2 to remove duplicate elements (single element or lists) from a list.
;;;NOTES:(remvdub2 '(a b (a) c b (a))) --> (a b c (a))

(defun remfrst (a L)
		(cond ((null L) nil) 
		((equal (first L) a)(remfrst a (rest L)))
		((eql (first L) a ) (remfrst a (rest L)))
		(t (cons (first L)(remfrst a (rest L))))))

(defun remvdub2 (L) 
		(if (null L) nil 
		(cons (first L) (remvdub2 (remfrst (first L) (rest L))))))

;;;FUNCTION NAME: min2
;;;DESCRIPTION: Write a function min2 to compute the second  smallest of number of an integer list. You may assume the list has at least 2 numbers and all numbers are distinct.
;;;NOTES:(min2 '(1 3 2 5 4)) --> 2

(defun savemax(a L) 
		(cond ((null L) nil) 
		((= (first L) a) (rest L))
		(t (cons (first L) (savemax a (rest L))))))

(defun min1 (L) 
		(cond((null(rest L))(first L)) 
		((< (first L) (first (rest L))) (min1(cons(first L)(rest(rest L)))))
		(t (min1(rest L)))))

(defun min2 (L) 
		(cond ((null L) 0)
		((null(rest L))(first L))
		(t (min1(savemax(min1 L)L)))))

;;;FUNCTION NAME: inde
;;;DESCRIPTION: Write a function inde which returns the index (start from 1) of the occurrence of a given value.
;;;NOTES:(inde 1 '(1 2 1 1 2 2 1)) --> (1 3 4 7)

(defun helper (a L index) 
		(cond ((null L) nil)
		((= (first L) a) (cons index (helper a (rest L)(+ index 1))))
		(t (helper a (rest L)(+ index 1)))))

(defun inde(a L) 
		(if(null L) nil(helper a L 1)))

;;;FUNCTION NAME: nele
;;;DESCRIPTION: Write a function nele which repeats each element in a list n times.
;;;NOTES:(nele '(1 3 5) 3) --> (1 1 1 3 3 3 5 5 5)

(defun nelehelper (L a b) 
		(cond ((null L) nil)
		((< b a) (cons (first L) (nelehelper L a (+ b 1))))
		(t (cons(first L)(nelehelper (rest L) a 1)))))

(defun nele(L a) 
		(if(null L) nil
		(nelehelper L a 1)))

;;;FUNCTION NAME: occr
;;;DESCRIPTION: Write a function occr to display the occurrence of an element of a list, or nil if the list is empty. DO IT RECURSIVELY. Please NO iterative mindset. Two functions only.
;;;NOTES:(occr '(1 2 1 2 3 2)) --> ((1 2) (2 3) (3 1))



;;;FUNCTION NAME: insea
;;;DESCRIPTION: Write a function insea to insert an element to each position of a list. Do it in recursive fashion. Two functions (not including possible @ or app).
;;;NOTES:(insea 4 '(1 2 3)) --> ((4 1 2 3) (1 4 2 3) (1 2 4 3) (1 2 3 4))

(defun sMap(n L1 L2) 
		(cond ((null L1)(cons(append L2 (list n)) nil))
		((null L2) (cons (append (list n) L1) (sMap n (rest L1) (list(first L1)))))
		(t (cons (append L2 (cons n L1))(sMap n (rest L1)(append L2 (list(first L1))))))))
		
(defun insea (a L)
		(sMap a L nil))

;;;FUNCTION NAME: mergesort
;;;DESCRIPTION: Implement function mergesort. (let) is optional, not required.
;;;NOTES: (mergesort '(5 3 2 11 7)) --> (2 3 5 7 11)

(defun split(L) 
		(cond((null L)(list nil nil))
		((null (rest L))(list (list (first L)) nil))
		(t (list (cons (first L)(first (split (rest (rest L)))))(cons (first (rest L))(first (rest (split (rest (rest L))))))))))
		
(defun merger (a b) 
		(cond ((null b) a) 
		((null a) b) 
		((< (first a)(first b))(cons (first a)(merger (rest a) b)))
		(t (cons (first b) (merger a (rest b))))))

(defun mergesort(L) 
		(cond ((null L) nil) 
		((null (rest L)) (list (first L)))
		(t (merger (mergesort (first(split L))) (mergesort(first(rest(split L))))))))

;;;FUNCTION NAME: qs 
;;;DESCRIPTION: Write a function qs(L) quick sort a list.
;;;NOTES: (qs '(4 2 3 1 7 3 5 3 6)) --> (1 2 3 3 3 4 5 6 7)
;;; a) qs is a higher order function with anonymous function
;;; b) qs applies a standard (filter P L) function
;;; c) total two functions, filter and qsort
;;; d) (append) is allowed

(defun qsfilter (P L)
		(cond ((null L) nil)
		((funcall P (first L))(cons (first L)(qsfilter P (rest L))))
		(t (qsfilter P (rest L)))))

(defun qs (L) 
		(if (null L) nil
		(append (append (qs (qsfilter (lambda (x) (< x (first L))) L)) (qsfilter (lambda (x) (= x (first L))) L)) (qs (qsfilter(lambda (x) (> x (first L))) L)))))



;;;FUNCTION NAME: primes
;;;DESCRIPTION: Write a higher order function primes applying standard filter to find all prime numbers from 2 to a given number n.
;;;NOTES: (primes 20) --> (2 3 5 7 11 13 17 19)

(defun filter (P L)
		(cond ((null L) nil) 
		((funcall P (first L)) (cons (first L) (filter P (rest L))))
		(t (filter P (rest L)))))
	
(defun listincr(a n) 
		(if (= n (+ a 1)) nil 
		(cons n (listincr a (+ n 1)))))
		
(defun allprimes(L a) 
		(cond ((null L) nil) 
		((= (mod (first L) a) 0)(allprimes (rest L) 2))
		((> (* a a) (first L))(cons (first L) (allprimes (rest L) 2)))
		(t (allprimes L (+ a 1)))))
		
(defun primes (a) 
		(filter (lambda (x) (and (> x 1) (< x (+ a 1)))) (allprimes (listincr a 0) 2)))
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		