;;; operations for lists (sets)

(define (order a b)
  (if (<= (length a) (length b))
	  (list a b)
	  (list b a)))

(define (compare x y)
  (cond ((and (string? x) (string? y))
		 (string<? x y))
		((and (number? x) (number? y))
		 (< x y))))

(define (find-elt elt a)
  (let ((found (member elt a)))
	(if found
		(- (length a) (length found))
		#f)))

(define (last-elt a)
  (if (null? a)
	  a
	  (car (list-tail a (- (length a) 1)))))

;; a + b
(define (set-union a b)
  (let lp ((all (sort (append a b) compare))
		   (union '()))
	(cond ((null? all) union)
		  ((equal? (last-elt union)
				   (car all))
		   (lp (cdr all)
			   union))
		  (else 
		   (lp (cdr all)  
			   (append union (list (car all))))))))

;; a - b
(define (set-diff a b)
  (let lp ((from a)
		   (subtract b))
	(cond ((null? subtract) from)
		  (else 
		   (lp (delete (car subtract) from)
			   (cdr subtract))))))

;; a ^ b
(define (set-intersect a b)
  (if (and (list? a) (list? b))
	  (let lp ((union '())
			   (short (sort (car (order a b)) compare))
			   (long (sort (cadr (order a b)) compare)))
		(if (null? short)
			union
			(let* ((elt (car short))
				   (long-tail (member elt long)))
			  (cond ((not long-tail)					 
					 (lp union
						 (cdr short)
						 long))
					(else
					 (lp (append union (list elt))
						 (cdr short)
						 long-tail))))))
	  (pp 'not-lists)))

;; filter a for elements that have an element in b with offset k
(define (set-filter-by-offset a b k)
  ;(pp a)
  ;(pp b)
  (if (and (list? a) (list? b))
	  (let ((res (list-transform-positive a
				   (lambda (x)
					 (and (number? x)
						  (memv (+ x k) b))))))
		(if (null? res)
			#f
			res))
	  #f))
