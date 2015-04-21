;;;; io.scm - Provides file I/O for data/ directory.
;;;; depends on: nothing

;-------------------------------------------------------------------------------

(define DATA-DIR "../data/")
(define PLAIN-DATA-DIR (string-append DATA-DIR "plain/"))
(define LING-DATA-DIR (string-append DATA-DIR "ling/"))

;-------------------------------------------------------------------------------

#|
Returns a handle on a file. filename is the file name
within the data/ directory.
|#
(define (open filename)
  (open-input-file filename))

#|
Given a file port, closes it.
|#
(define (close file-port)
  (close-port file-port)
  'done)

#|
Gets all lines from a file given the file port.
Returns a list where each element is a line string.
|#
(define (get-lines file-port)
  (let loop ((lines '())
	     (port file-port))
    (let ((line (read-line port)))
      (if (eof-object? line)
	  (begin (close file-port)
		 (reverse lines))
	  (loop (cons line lines)
		port)))))

#|
Gets all words from a file given the file port.
Returns a list where each element is a word string.
|#
(define (get-words file-port)
  (let loop ((words '())
	     (port file-port))
    (let ((word (read-string char-set:whitespace port)))
      (if (eof-object? word)
	  (begin (close file-port)
		 (reverse words))
	  (loop (if (string=? word "")
		    (begin (read-string char-set:not-whitespace file-port)
			   words)
		    (cons word
			  words))
		port)))))

;-------------------------------------------------------------------------------

;;; ignore below for now

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
	  (begin (close file-port)
		 (reverse words))
	  (loop (cons (symbol->string word)
		      words)
		port)))))
