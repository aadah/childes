;;;; parser.scm - the parser
;;;; depends on: document.scm

;-------------------------------------------------------------------------------

(load "document")

;-------------------------------------------------------------------------------

(define (parser type)
  (map (lambda (filename)
	 (create-doc filename type))
       (get-filenames (cond ((eq? type 'plain) PLAIN-DATA-DIR)
			    ((eq? type 'ling) LING-DATA-DIR)
			    (else (error "Can't parse file type" type))))))
