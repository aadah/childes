;;;; io.scm - Provides file I/O for data/ directory.

;-------------------------------------------------------------------------------

(define DATA-DIR "../data/")

;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------

#|
Returns a port with the given string as the source.
|#
(define (open-string string)
  (open-input-string string))

#|
Close a given string port.
|#
(define (close-string string-port)
  (close-port string-port)
  'done)

#|
Returns a list of all the words in the given string.
|#
(define (split-string string-port)
  (let loop ((words '())
	     (port string-port))
    (let ((word (read port)))
      (if (eof-object? word)
	  (reverse words)
	  (loop (cons (symbol->string word)
		      words)
		port)))))
