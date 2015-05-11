;;;; utils.scm - Holds various small utilities
;;;; depends on: nothing

(define (tagged-list? data tag)
  (and (pair? data)
       (eq? (car data)
	    tag)))

(define (starts-with? str pre)
  (if (< (string-length str)
	 (string-length pre))
      #f
      (let ((start (string-head str
			       (string-length pre))))
	(string=? pre start))))

(define (random-string r)
  (string-append "key-"
		 (apply string-append
			(map (lambda (i)
			       (number->string (random 10)))
			     (iota r)))))
