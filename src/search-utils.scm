;;; utils

(define (word? str)
  (and (string? str)
	   (not (string-find-next-char str #\space))))

; hack to use and as a procedure
(define (proc-and p q)
  (if p
	  (if q
		  #t
		  #f)
	  #f))

(define (words? lst)
  (and (list? lst)
	   (reduce proc-and
			   #t
			   (map (lambda (w)
					  (word? w))
					lst))))

(define (phrase? str)
  (and (string? str)
	   (not (not (string-find-next-char str #\space)))))

(define (tier? str)
  (not (not (member str *tiers*))))

(define (metric:freq doc)
  (length (indices doc)))

(define (split-string-by-space s)
  (let ((ref (string-find-next-char s #\space)))
	(if ref
		(let lp ((str (string-tail s (+ ref 1)))
				 (lst (list (string-head s ref))))
		  (let ((ref (string-find-next-char str #\space)))
			;(pp str)
			(if (not ref)
				(append lst (list str))
				(lp (string-tail str (+ ref 1))
					(append lst (list (string-head str ref))))))))))

;; attach score to each result
(define (s:score res metric)
  (map (lambda (doc)
		 (list (name doc)
			   (metric doc)
			   (indices doc)))
	   res))


(define (descore res)
  (map (lambda (doc)
		 (append (list (car doc))
				 (caddr doc)))
	   res))

