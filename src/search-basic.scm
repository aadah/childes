;;;; Search utilities

;;; operations for lists
(define (order a b)
  (if (<= (length a) (length b))
	  (list a b)
	  (list b a)))

(define (compare x y)
  (cond ((and (string? x) (string? y))
		 (string<? x y))))

(define (find-elt elt a)
  (let ((found (member elt a)))
	(if found
		(- (length a) (length found))
		#f)))

;; a U b
(define (union-list a b)
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

;; a - b
(define (subtract-list a b)
  (if (and (list? a) (list? b))
	  (let lp ((diff '())
			   (subtract (sort b compare))
			   (from (sort a compare)))
		(if (or (null? subtract) (null? from))
			diff
			(let* ((elt (car subtract))
				   (from-tail (member elt from)))
			  (cond ((not from-tail)
					 (lp diff
						 (cdr subtract)
						 from))
					(else
					 (lp (append diff (list elt))
						 (cdr subtract)
						 from-tail))))))
	  (pp 'not-lists)))

;; a ^ b			  
(define (intersect-list a b)
  (subtract-list a (subtract-list a b)))

;;; operations for alists
