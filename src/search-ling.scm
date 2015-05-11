(define test-dir "../data/english-na-mor/bates/Free20/")

(define test-file (string-append test-dir "amy20.cha"))

#|(define (split-by-space line)
  (if (not (string-null? line))
      (let ((space-ref
             (string-find-next-char line #\space)))
        (if (eq? #f space-ref)
			(list line)
            (let ((head (string-head line space-ref))
                  (tail (string-tail line (+ 1 space-ref))))
              (append (list head) (split-by-space tail)))))))|#

#|(define (get-participants file)
  (let loop
	  ((raw-participants
		 (r:grep (r:seq (r:bol) "@Participants") file))
	   (participants '()))
	(cond ((null? raw-participants)
		   participants)
		  ((string-upper-case? (car raw-participants))
		   (loop (cdr raw-participants)
				 (cons (list (car raw-participants)) participants)))
		  (else
		   (loop (cdr raw-participants)
				 (append (car participants) (car raw-participants)))))))
|#

#|(define (get-line-type line)
  (let* ((start (string-head line 1)))
	(cond ((string=? start "@") 'header)
		  ((string=? start "*") 'utterance)
		  ((string=? start "%") 'tier))))|#

#|(define (get-tiers file)
  (r:grep (r:seq (r:bol) "%") file))|#

(define (l:keyword word tier)
  (let* ((l-ii (ii/get *ii* (key-for-tier tier)))
		 (raw (ii/get l-ii word)))
	(if raw
		(s:score raw metric:freq)
		#f)))


;; keyword search for multiple words
(define (l:keywords words tier)
  (let ((l-ii (ii/get *ii* (key-for-tier tier))))
	(if (= (length words) 1)
		(l:keyword (car words))
		(let ((raw (map (lambda (x)
						  (ii/get l-ii x))
						words)))
		  (if raw
			  (let ((res (alist-union-multi raw)))
				(s:score res metric:freq))
			  #f)))))
#|
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

|#