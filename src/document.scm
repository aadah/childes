;;;; document.scm - Code for document data structure.
;;;; depends on: io.scm, utils.scm

;-------------------------------------------------------------------------------

(load "utils")
(load "io")

;-------------------------------------------------------------------------------

(define (plain-doc? doc)
  (tagged-list? doc 'plain))

(define (create-plain-doc filename)
  (let* ((file-port (open (string-append PLAIN-DATA-DIR filename)))
	 (words (get-words file-port)))
    (list 'plain
	  filename
	  words)))

(define (plain-doc/name doc)
  (cadr doc))

(define (plain-doc/words doc)
  (caddr doc))

;-------------------------------------------------------------------------------

(define (doc/name doc)
  (cond ((plain-doc? doc) (plain-doc/name doc))
	;((ling-doc? doc) (ling-doc/name doc)) ; TODO
	(else (error "Can't get name" doc))))

(define (doc/words doc)
  (cond ((plain-doc? doc) (plain-doc/words doc))
	;((ling-doc? doc) (ling-doc/name doc)) ; TODO
	(else (error "Can't get words" doc))))

(define (create-doc filename type)
  (cond ((eq? type 'plain) (create-plain-doc filename))
	;((eq? type 'ling) (create-ling-doc filename)) ; TODO
	(else (error "Can't parse file type" type filename))))
