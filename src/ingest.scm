(define DATA-DIR "../data/")

#|
Returns a handle on a file. filename is the file name
within the data/ directory.
|#
(define (open filename)
  (open-input-file (string-append DATA-DIR
				  filename)))

#|
Given a file port, closes it.
|#
(define (close file-port)
  (close-port file-port)
  'done)

#|
Gets all lines from a file given the file port.
Returns a list where each element is a line.
|#
(define (get-lines file-port)
  (let loop ((lines '())
	     (port file-port))
    (let ((line (read-line port)))
      (if (eof-object? line)
	  (reverse lines)
	  (loop (cons line lines)
		port)))))
