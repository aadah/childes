;;;; inverted-index-maker.scm - the inverted-index maker
;;;; depends on: inverted-index.scm

;-------------------------------------------------------------------------------

(load "inverted-index")

;-------------------------------------------------------------------------------

(define (inverted-index-maker ii docs)
  (for-each (lambda (doc)
	      (ii/add-doc! ii doc))
	    docs))
