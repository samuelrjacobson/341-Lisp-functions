;determines whether list contains an atom
(defun f1 (L)
	(cond ((null L) nil)		;if L is empty, return nil
		  ((atom (car L)) T)	;else if car of L is an atom, return T
		  (T (f1 (cdr L)))		;else check cdr of L 
	)
)
;counts number of lists of length 1 in list
(defun f2 (L)
	(cond ((null L) 0)									  ;if L is empty, return zero
		  ((atom (car L)) (f2(cdr L)))					  ;else if car of L is an atom, check cdr of L
		  ((equal (length (car L)) 1) (+ 1 (f2(cdr L))))  ;else if length of car of L is 1, add 1 plus count of cdr
		  (T (f2(cdr L)))								  ;else check cdr of L
	}
}
;returns list of odd integers from list
(defun f3 (L)
	(cond ((null L) nil)								;if L is empty, return nil
		  ((oddp (car L)) (cons (car L) (f3 (cdr L))))  ;if car of L is odd, add it to list of odd ints in cdr of L
		  (T (f3 (cdr L)))								;else find list of odd ints in cdr of L
	)
)
;finds minimum integer in list
(defun f4 (L)
	(cond ((atom L) L)									;if L is an atom, return it
		  ((null L) nil)								;else if L is null, return nil
		  ((equal (length L) 1) (car L))				;else if L is list of length 1, return its atom
		  ((> (car L) (cadr L)) (f4(cdr L)))			;else if first atom in L > second, remove first
		  (T (f4(append (list (car L)) (cddr L)))) 		;else if first < second, remove second
	)
)
;finds reverse of list L
(defun f5 (L)
	(cond ((null L) L) 								;if L is empty then reverse is empty
		  (T (append (f5 (cdr L)) (list (car L))))  ;else reverse cdr of L and append first
	) 
)
;returns list containing every other element of original list
(defun f6 (L)
	(cond ((null L) nil)								;if L is null, return nil
		  ((< (length L) 3) (list (car L)))				;else if L has 1 or 2 element, return first
		  (T (append (list (car L)) (f6(cddr L))))		;else add first element to list of other values
	)
)
;returns element at location n in list
(defun f7 (L n)
	(cond ((null L) nil)			;if L is null, return nil
		  ((equal n 1) (car L))		;else if n = 1, return first element
		  (T (f7(cdr L) (- n l)))	;else repeat using cdr of L and n-1
	)
)
;adds up all integers in list
(defun f8 (L)
	(cond ((null L) 0)										;if L is null, return zero
		  ((listp (car L)) (+ (f8 (car L)) (f8 (cdr L))))	;else if car of L is a list, add sum of car of L and sum of cdr of L
		  (T (+ (car L) (f8(cdr L))))						;else add car of L and sum of cdr of L
	)
)
;helper function for f9 and f10
;decides whether x is member of L
(defun my_member (x L)
	(cond ((null L) nil)			;if L is empty then x is not in L
		  ((equal x (car L)) T)		;if x is first element of L then x is in L
		  (T (my_member x (cdr L))) ;else check x is in cdr of L
	)
)
;removes duplicates from list
(defun f9 (L)
	(cond ((null L) L)									;if L is null, return nil
		  ((my_member (car L) (cdr L)) (f9 (cdr L)))	;else if car of L is in cdr of L, lose car of L and check cdr
		  (T (append (list (car L)) (f9 (cdr L))))		;else append car of L with result of cdr of L
	)
)
;returns intersection of two lists
(defun f10 (L1 L2)
	(cond ((null L2) L2)														;if L2 is null, return L2
		  ((null L1) L1)														;else if L1 is null, return L1
		  ((my_member (car L1) L2) (append (list (car L1)) (f10 (cdr L1) L2)))	;else if car of L1 is in L2, append L1 with result of (cdr of L1) and L2
		  (T (f10 (cdr L1) L2))													;else call with (cdr of L1) and L2
	)
)
		  