(define DATA-DIR "../data/")

#|
Returns a handle on a file. filename is the file name
within the data/ directory.
|#
(define (open filename)
  (open-input-file (string-append DATA-DIR
				  filename)))

(define (close file-port)
  (close-port file-port)
  'done)

(define (get-lines file-port)
  (let loop ((lines '())
	     (port file-port))
    (let ((line (read-line port)))
      (if (eof-object? line)
	  (reverse lines)
	  (loop (cons line lines)
		port
		(read-line port))))))
