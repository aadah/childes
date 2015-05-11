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
	  (begin (close port)
		 (reverse words))
	  (loop (if (string=? word "")
		    (begin (read-string char-set:not-whitespace port)
			   words)
		    (cons word
			  words))
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
(define (split-string string)
  (let loop ((words '())
	     (port (open-string string)))
    (let ((word (read-string char-set:whitespace port)))
      (if (eof-object? word)
	  (begin (close port)
		 (reverse words))
	  (loop (if (string=? word "")
		    (begin (read-string char-set:not-whitespace port)
			   words)
		    (cons word
			  words))
;(cons (symbol->string word)
;	   words)
		port)))))

;-------------------------------------------------------------------------------

#|
Returns a list of the filenames in the specified directory, given as a string.
Recurses down directories.
|#
(define (get-filenames directory)
  (let ((pathnames (directory-read directory)))
    (apply append (map (lambda (pathname)
			 (if (pathname-type pathname)
			     (if (string=? (pathname-type pathname)
					   "cha")
				 (list (string-append (pathname-name pathname)
						      "."
						      (pathname-type pathname)))
				 '())
			     (if (or (string=? (pathname-name pathname)
					       ".")
				     (string=? (pathname-name pathname)
					       ".."))
				 '()
				 (map (lambda (filename)
					(string-append (pathname-name pathname)
						       "/"
						       filename))
				      (get-filenames (string-append directory
								    "/"
								    (pathname-name pathname)
								    "/"))))))
		       pathnames))))
