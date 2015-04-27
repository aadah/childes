(define test-dir "../data/english-na-mor/bates/Free20/")

(define test-file (string-append test-dir "amy20.cha"))

(define (split-by-space line)
  (if (not (string-null? line))
      (let ((space-ref
             (string-find-next-char line #\space)))
        (if (eq? #f space-ref)
			(list line)
            (let ((head (string-head line space-ref))
                  (tail (string-tail line (+ 1 space-ref))))
              (append (list head) (split-by-space tail)))))))

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

(define (get-line-type line)
  (let* ((start (string-head line 1)))
	(cond ((string=? start "@") 'header)
		  ((string=? start "*") 'utterance)
		  ((string=? start "%") 'tier))))

(define (get-tiers file)
  (r:grep (r:seq (r:bol) "%") file))