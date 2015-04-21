;;;; inverted-index.scm - Code for inverted index data structure.
;;;; depends on: document.scm, utils.scm

;-------------------------------------------------------------------------------

(load "utils")
(load "document")

;-------------------------------------------------------------------------------

(define (inverted-index? doc)
  (tagged-list? doc 'inverted-index))

(define (make-inverted-index)
  `(inverted-index
    ,(make-string-hash-table)))

(define (ii/put! ii key val)
  (let ((hash-table (cadr ii)))
    (hash-table/put! hash-table key val))
  'done)

(define (ii/get ii key)
  (let ((hash-table (cadr ii)))
    (hash-table/get hash-table key #f)))

;-------------------------------------------------------------------------------

(define (ii/add-index! doc index) ; helper
  (let* ((indices (cdr doc))
	 (has-index (memq index indices)))
    (if has-index
	'done
	(set-cdr! (last-pair indices)
		  (list index))))
  'done)

(define (ii/update-docs! docs docname index) ; helper
  (set-cdr! (last-pair docs)
	    (list (cons docname
			(list index))))
  'done)

(define (ii/put-sole-entry! ii word docname index) ; helper
  (ii/put! ii
	   word
	   (list (cons docname
		       (list index))))
  'done)

(define (ii/update! ii word docname index)
  (let ((docs (ii/get ii word)))
    (if docs
	(let ((doc (assoc docname docs)))
	  (if doc
	      (ii/add-index! doc index)
	      (ii/update-docs! docs docname index)))
	(ii/put-sole-entry! ii word docname index)))
  'done)

;-------------------------------------------------------------------------------

;; number of times word appears across corpus
(define (ii/corpus-count ii word)
  (let ((docs (ii/get ii word)))
    (if docs
	(apply +
	       (map (lambda (doc)
		      (length (cdr doc)))
		    docs))
	0)))

;; number of documents word appears in
(define (ii/doc-count ii word)
  (let ((docs (ii/get ii word)))
    (if docs
	(length docs)
	0)))

;; number of times word appears in given document
(define (ii/within-doc-count ii word docname)
  (let ((docs (ii/get ii word)))
    (if docs
	(let ((doc (assoc docname docs)))
	  (if doc
	      (length doc)
	      0))
	0)))

;-------------------------------------------------------------------------------

(define (ii/add-plain-doc! ii doc)
  (let ((docname (doc/name doc))
	(words (doc/words doc)))
    (for-each (lambda (index word)
		(ii/update! ii word docname index))
	      (iota (length words))
	      words)))

;-------------------------------------------------------------------------------

(define (ii/add-doc! ii doc)
  (cond ((plain-doc? doc) (ii/add-plain-doc! ii doc))
	;((ling-doc? doc) (ii/add-ling-doc! ii doc)) ; TODO
	(else (error "Don't know how to add doc" doc))))
