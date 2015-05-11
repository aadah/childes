;;;; search-basic.scm - Search utilities
;;;; depends on: init.scm

;-------------------------------------------------------------------------------

(load "init")

;-------------------------------------------------------------------------------

;;; operations for lists
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
  (pp a)
  (pp b)
  (if (and (list? a) (list? b))
	  (let ((res (list-transform-positive a
				   (lambda (x)
					 (and (number? x)
						  (memv (+ x k) b))))))
		(if (null? res)
			#f
			res))
	  #f))

;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------
;;; utils

(define (word? str)
  (and (string? str)
	   (not (string-find-next-char str #\space))))

(define (phrase? str)
  (and (string? str)
	   (not (not (string-find-next-char str #\space)))))

(define (metric:freq doc)
  (length (indices doc)))

(define (metric:tf-idf doc)

(define (split-string-by-space s)
  (let ((ref (string-find-next-char s #\space)))
	(if ref
		(let lp ((str (string-tail s (+ ref 1)))
				 (lst (list (string-head s ref))))
		  (let ((ref (string-find-next-char str #\space)))
			(pp str)
			(if (not ref)
				(append lst (list str))
				(lp (string-tail str (+ ref 1))
					(append lst (list (string-head str ref))))))))))

(define (descore res)
  (map (lambda (doc)
		 (append (list (car doc))
				 (caddr doc)))
	   res))

;-------------------------------------------------------------------------------

;;; Search API (each of these returns an alist of documents with an
;;; associated score)

;; attach score to each result
(define (s:score res metric)
  (map (lambda (doc)
		 (list (name doc)
			   (metric doc)
			   (indices doc)))
	   res))


;; keyword search
(define (s:keyword w)
  (let ((raw (ii/get *ii* w)))
	(if raw
		(s:score raw metric:freq)
		#f)))

;; keyword search for multiple words
(define (s:keywords . w)
  (if (= (length w) 1)
	  (s:keyword (car w))
	  (let ((raw (map (lambda (x)
						(ii/get *ii* x))
					  w)))
		(if raw
			(let ((res (alist-union-multi raw)))
			  (s:score res metric:freq))
			#f))))

(define (s:keywords-with-offset w1 w2 k)
  (let ((a (ii/get *ii* w1))
		(b (ii/get *ii* w2)))
	(if (and (alist? a) (alist? b))
		(alist-filter-by-offset a b k)
		#f)))

;; assumes a space-delimited phrase
(define (s:phrase phrase)
  (let ((words (split-string-by-space phrase)))	
	(let lp ((docs (map (lambda (x)
						  (ii/get *ii* x))
						(cdr words)))
			 (res (ii/get *ii* (car words)))
			 (k 1))
	  (cond ((null? docs) res)
			((not res) #f)
			(else
			 (pp res)
			 (lp (cdr docs)
				 (alist-filter-by-offset
				  res
				  (car docs)
				  k)
				 (+ k 1)))))))

;; rank alist with (id score)
; assumes string id, integer score
(define (s:rank lst)
  (sort lst
		(lambda (x y)
		  (cond ((> (cadr x) (cadr y))
				 #t)
				((= (cadr x) (cadr y))
				 (string<? (car x) (car y)))
				(else #f)))))
		
(define (s:and res1 res2)
  (s:score
   (alist-intersect (descore res1)
					(descore res2))
   metric:freq))

(define (s:or res1 res2)
  (s:score 
   (alist-union (descore res1) 
				(descore res2))
   metric:freq))