;;;; search-basic.scm - Search utilities
;;;; depends on: init.scm

;-------------------------------------------------------------------------------

(load "set-utils")
(load "alist-utils")
(load "search-utils")
(load "ghelper")

;-------------------------------------------------------------------------------

;;; Search API (each of these returns an alist of documents with an
;;; associated score)

;; keyword search
(define (s:keyword word)
  (let ((raw (ii/get *ii* word)))
	(if raw
		(s:score raw metric:freq)
		#f)))

;; keyword search for multiple words
(define (s:keywords words)
  (if (= (length words) 1)
	  (s:keyword (car w))
	  (let ((raw (map (lambda (x)
						(ii/get *ii* x))
					  words)))
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

;-------------------------------------------------------------------------------

(define search (make-generic-operator 1 'search))

(defhandler search s:keyword word?)
(defhandler search s:keywords words?)
(defhandler search s:phrase phrase?)