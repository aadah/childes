;;;; document.scm - Code for document data structure.
;;;; depends on: io.scm, utils.scm

;-------------------------------------------------------------------------------

(load "utils")
(load "io")
(load "ghelper")

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

(define header-marker "@")
(define participant-marker "*")
(define tier-marker "%")

(define (correct-lines lines)
  (let loop ((split-lines (map (lambda (line)
				 (split-string line))
			       lines))
	     (result '()))
    (if (null? split-lines)
	result
	(if (or (starts-with? (car (car split-lines))
			      header-marker)
		(starts-with? (car (car split-lines))
			      participant-marker)
		(starts-with? (car (car split-lines))
			      tier-marker))
	    (loop (cdr split-lines)
		  (append result
			  (list (car split-lines))))
	    (begin (set-car! (last-pair result)
			     (append (car (last-pair split-lines))
				     (car split-lines)))
		   (loop (cdr split-lines)
			 result))))))

(define (extract-metadata lines)
  (let loop ((lines lines)
	     (result '()))
    (if (null? lines)
	result
	(if (starts-with? (car (car lines))
			  header-marker)
	    (loop (cdr lines)
		  (append result
			  (list (car lines))))
	    (loop (cdr lines)
		  result)))))

(define (group-lines lines)
  (let loop ((lines lines)
	     (groups '()))
    (if (null? lines)
	groups
	(if (starts-with? (car (car lines))
			  participant-marker)
	    (let ((group 
		   (let loop2 ((lines2 (cdr lines))
			       (group (list (car lines))))
		     (if (starts-with? (car (car lines2))
				       tier-marker)
			 (loop2 (cdr lines2)
				(append group
					(list (car lines2))))
			 group))))
	      (loop (list-tail lines
			       (length group))
		    (append groups
			    (list group))))
	    (loop (cdr lines)
		  groups)))))

(define (ling-doc? doc)
  (tagged-list? doc 'ling))

(define (create-ling-doc filename)
  (let* ((file-port (open (string-append LING-DATA-DIR filename)))
	 (lines (correct-lines (get-lines file-port))))
    (list 'ling
	  filename
	  (extract-metadata lines)
	  (group-lines lines))))

(define (ling-doc/name doc)
  (cadr doc))

;(define (ling-doc/words doc)
;  (caddr doc))

(define (ling-doc/metadata doc)
  (caddr doc))

(define (ling-doc/convo doc)
  (cadddr doc))

;-------------------------------------------------------------------------------

;(define create-doc (make-generic-operator 2 'create-doc))

;(defhandler create-doc create-plain-doc string? (lambda (sym) (eq? sym 'plain)))

(define (create-doc filename type)
  (cond ((eq? type 'plain) (create-plain-doc filename))
	((eq? type 'ling) (create-ling-doc filename))
	(else (error "Can't parse file type" type filename))))

(define doc/name (make-generic-operator 1 'doc/name))
(define doc/words (make-generic-operator 1 'doc/words))

(defhandler doc/name plain-doc/name plain-doc?)
;(defhandler doc/name ling-doc/name ling-doc?)

(defhandler doc/words plain-doc/words plain-doc?)
;(defhandler doc/words ling-doc/words ling-doc?)
