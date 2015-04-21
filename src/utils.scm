;;;; utils.scm - Holds various small utilities
;;;; depends on: nothing

(define (tagged-list? data tag)
  (and (pair? data)
       (eq? (car data)
	    tag)))
