;;; operations for alists

(define (name a)
  (if (list? a)
	  (car a)
	  #f))

(define (indices a)
  (if (list? a)
	  (cdr a)
	  #f))

;; takes union of alists in a and alists in b, with their values being
;; the union of their respective lists
(define (alist-union a b)
  (if (and (alist? a) (alist? b))
	  (let* ((a-keys
			  (map (lambda (x)
					 (car x))
				   a))
			 (b-keys
			  (map (lambda (x)
					 (car x))
				   b))
			 (key-union (set-union a-keys b-keys)))
		(map (lambda (key)
			   (let ((a-vals (assoc key a))
					 (b-vals (assoc key b)))
				 (cond ((not a-vals)
						b-vals)
					   ((not b-vals)
						a-vals)
					   (else 
						(append (list key)
								(set-union 
								 (cdr a-vals) (cdr b-vals)))))))
			 key-union))
	  #f))
		   
(define (alist-union-multi alists)
  (reduce alist-union '() alists))

(define (alist-intersect a b)
  (if (and (alist? a) (alist? b))
	  (let* ((a-keys
			  (map (lambda (x)
					 (car x))
				   a))
			 (b-keys
			  (map (lambda (x)
					 (car x))
				   b))
			 (key-intersect (set-intersect a-keys b-keys)))
		(map (lambda (key)
			   (let ((a-vals (assoc key a))
					 (b-vals (assoc key b)))
				 (cond ((not a-vals)
						#f)
					   ((not b-vals)
						#f)
					   (else
						(append (list key)
								(set-union
								 (cdr a-vals) (cdr b-vals)))))))
			 key-intersect))))

;; filter alists in a against alists in b for elements with an offset
;; of k
(define (alist-filter-by-offset a b k)
  (if (and (alist? a) (alist? b))
	  (let lp ((docs a)
			   (filtered '()))
		(cond ((null? docs)
				   filtered)
			  (else
			   (let* ((doc (car docs))
					  (a-name (name doc))
					  (a-indices (indices doc))
					  (b-indices (indices (assoc a-name b)))
					  (res (set-filter-by-offset a-indices 
												 b-indices
												 k)))
				 (if res
					 (lp (cdr docs)
						 (cons (append
								(list a-name)
								res)
							   filtered))
					 (lp (cdr docs)
						 filtered))))))
	  #f))
