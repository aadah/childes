;;;; inverted-index.scm - Code for inverted index data structure.
;;;; depends on: document.scm, utils.scm

;-------------------------------------------------------------------------------

(load "utils")
(load "document")
(load "ghelper")

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

(define (ii/keys ii)
  (let ((hash-table (cadr ii)))
    (hash-table/key-list hash-table)))

(define (ii/values ii)
  (let ((hash-table (cadr ii)))
    (hash-table/datum-list hash-table)))

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

(define *tiers* '("%act:"
		  "%add:"
		  "%alt:"
		  "%cnl:"
		  "%cod:"
		  "%coh:"
		  "%com:"
		  "%def:"
		  "%eng:"
		  "%err:"
		  "%exp:"
		  "%fac:"
		  "%flo:"
		  "%gls:"
		  "%gpx:"
		  "%gra:"
		  "%grt:"
		  "%int:"
		  "%mod:"
		  "%mor:"
		  "%ort:"
		  "%par:"
		  "%pho:"
		  "%sin:"
		  "%sit:"
		  "%spa:"
		  "%tim:"
		  "%trn:"
		  "%xpho:"
		  "*"))

(define *headers* '("@Font:"
		    "@UTF8"
		    "@PID:"
		    "@ColorWords"
		    "@Begin"
		    "@Languages:"
		    "@Participants:"
		    "@Options:"
		    "@ID:"
		    "@Media:"
		    "@End"
		    "@Exceptions:"
		    "@Interaction Type:"
		    "@Location:"
		    "@Number:"
		    "@Recording Quality:"
		    "@Room Layout:"
		    "@Tape Location:"
		    "@Time Duration:"
		    "@Time Start:"
		    "@Transcriber:"
		    "@Transcription:"
		    "@Warning:"
		    "@Activities:"
		    "@Bck:"
		    "@Bg:"
		    "@Blank"
		    "@Comment:"
		    "@Date:"
		    "@Eg:"
		    "@G:"
		    "@New Episode"
		    "@New Language:"
		    "@Page:"
		    "@Situation:"))

(define *tier-keys* (map (lambda (tier)
			   (list tier (random-string 25)))
			 *tiers*))

(define *header-keys* (map (lambda (header)
			     (list header (random-string 25)))
			   *headers*))

(define (key-for-tier tier)
  (cadr (assoc tier *tier-keys*)))

(define (key-for-header header)
  (cadr (assoc header *header-keys*)))

(define (ii/add-ling-doc! ii doc)
  (let* ((docname (ling-doc/name doc))
	 (metadata (ling-doc/metadata doc))
	 (groups (ling-doc/convo doc))
	 (metadata-words (apply append metadata))
	 (group-words (apply append (map (lambda (group)
					   (apply append group))
					 groups)))
	 (words (append metadata-words group-words)))
    (for-each (lambda (index word)
		(ii/update! ii word docname index))
	      (iota (length words))
	      words)
    (for-each (lambda (key-pair)
		(ii/put! ii
			 (cadr key-pair)
			 (make-inverted-index)))
	      *tier-keys*)
    (for-each (lambda (key-pair)
		(ii/put! ii
			 (cadr key-pair)
			 (make-inverted-index)))
	      *header-keys*)
    (let loop ((group-words group-words)
	       (index (+ (length metadata-words)
			 1))
	       (tag (car group-words)))
      (cond ((null? group-words) 'done)
	    ((starts-with? (car group-words)
			   participant-marker)
	     (begin (ii/update! (ii/get ii
					(key-for-tier "*"))
				(car group-words)
				docname
				index)
		    (loop (cdr group-words)
			  (+ index 1)
			  "*")))
	    ((starts-with? (car group-words)
			   tier-marker)
	     (begin (ii/update! (ii/get ii
					(key-for-tier (car group-words)))
				(car group-words)
				docname
				index)
		    (loop (cdr group-words)
			  (+ index 1)
			  (car group-words))))
	    (else (begin (ii/update! (ii/get ii
					     (key-for-tier tag))
				     (car group-words)
				     docname
				     index)
			 (loop (cdr group-words)
			       (+ index 1)
			       tag)))))))

;-------------------------------------------------------------------------------

(define ii/add-doc! (make-generic-operator 2 'ii/add-doc!))

(defhandler ii/add-doc! ii/add-plain-doc! inverted-index? plain-doc?)
(defhandler ii/add-doc! ii/add-ling-doc! inverted-index? ling-doc?)
