;;;; Scheme Regular Expression Language Implementation --
;;;; regexp_opt.scm

(define ((type? type) expr)
  (and (pair? expr)
	      (equal? type (car expr))))

(define (r:dot) '(dot))
(define (r:bol) '(bol))
(define (r:eol) '(eol))
(define (r:bar) '(bar))
(define (r:l-paren) '(l-paren))
(define (r:r-paren) '(r-paren))
(define (r:l-brace) '(l-brace))
(define (r:r-brace) '(r-brace))
(define (r:group name) (cons 'group name))

(define (r:bar->string ere)
  (if ere "|" "\\|"))
(define (r:l-paren->string ere)
  (if ere "(" "\\("))
(define (r:r-paren->string ere)
  (if ere ")" "\\)"))
(define (r:l-brace->string ere)
  (if ere "{" "\\{"))
(define (r:r-brace->string ere)
  (if ere "}" "\\}"))

(define (is-group? expr)
  (and (pair? expr)
	      (pair? (car expr))
		     (equal? (car (car expr)) 'group)))


(define (r:quote string #!optional name)
  (let ((quote-expr
		  (cons 'quot
				   (append-map (lambda (char)
								  (if (memv char
											chars-needing-quoting)
									   (list #\\ char)
									    (list char)))
							      (string->list string)))))
	(if (not (equal? name #!default))
		(list (r:group name) quote-expr)
		quote-expr)))
(define (r:quote->string expr ere groups)
  (if (is-group? expr)
	    (r:compile-group expr ere groups)
		  (list->string (cdr expr))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:char-from string #!optional name)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string name))
    (else
	  (let ((char-from-expr
			 (cons 'char-from
				     (bracket string
							     (lambda (members)
								    (if (lset= eqv? '(#\- #\^)
											   members)
										 '(#\- #\^)
										  (quote-bracketed-contents
										     members)))))))
		   (if (not (equal? name #!default))
			      (list (r:group name) char-from-expr)
				     char-from-expr)))))
(define (r:char-from->string expr ere groups)
  (if (is-group? expr)
	    (r:compile-group expr ere groups)
		  (list->string (cdr expr))))

(define (r:char-not-from string #!optional name)
  (let ((char-not-from-expr
		  (cons 'char-not-from
				   (bracket string
							(lambda (members)
							    (cons #\^ (quote-bracketed-contents
										    members)))))))
	(if (not (equal? name #!default))
		(list (r:group name) char-not-from-expr)
		char-not-from-expr)))
(define (r:char-not-from->string expr ere groups)
  (if (is-group? expr)
	    (r:compile-group expr ere groups)
		  (list->string (cdr expr))))

(define (bracket string procedure)
  (append '(#\[)
		    (procedure (string->list string))
			  '(#\])))

(define (quote-bracketed-contents members)
  (let ((optional
         (lambda (char) (if (memv char members) (list char) '()))))
    (append (optional #\])
            (remove (lambda (c)
					    (memv c chars-needing-quoting-in-brackets))
					members)
            (optional #\^)
            (optional #\-))))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

;;; Means of combination for patterns

(define (r:seq . exprs)
  (cons 'seq
		(map (lambda (expr)
			      (if (r:just-strings expr)
					     (r:quote expr)
						    expr))
			  exprs)))
(define (r:seq->string expr ere groups)
  (if (is-group? expr)
	    (r:compile-group expr ere groups)
		  (apply string-append (map (lambda (node)
									    (cdr groups)
										  (r:compile node ere groups))
									(cdr expr)))))

(define (r:seq-group name . exprs)
  (list (r:group name) (apply r:seq exprs)))

#|
(pp (r:seq-group 'g1 
				  (r:bol)
				   (r:char-from "xyz" 'g2)
				    (r:repeat 3 5 
							     (r:alt (r:quote "foo" 'g3) 
										  (r:quote "bar" 'g4)
										    (r:backref 'g2)))
					 (r:seq (r:backref 'g3) (r:backref 'g4))
					  "cat"
					   (r:eol)))
((group . g1)
 (seq
  (bol)
  ((group . g2) (char-from #\[ #\x #\y #\z #\]))
  (rep
   (alt ((group . g3) (quot #\f #\o #\o))
        (bar)
        ((group . g4) (quot #\b #\a #\r))
        (bar)
        (backref . g2))
   (l-brace)
   "3"
   ","
   "5"
   (r-brace))
  (seq (backref . g3) (backref . g4))
  (quot #\c #\a #\t)
  (eol)))
;Unspecified return value
|#

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . exprs)
  (if (pair? exprs)
	    (cons 'alt
			  (cons (car exprs)
					  (append-map (lambda (expr)
									(list (r:bar) expr))
								    (cdr exprs))))
      (r:seq)))
(define (r:alt->string expr ere groups)
  (string-append (r:l-paren->string ere)
				  (apply string-append 
						 (map (lambda (node)
								   (r:compile node ere groups))
							   (cdr expr)))
				   (r:r-paren->string ere)))
(define (r:alt-group name . exprs)
  (list (r:group name) (apply r:alt exprs)))
(define (r:alt-group->string expr ere groups)
  (r:compile-group expr ere groups))

(define (r:repeat min max expr #!optional name)
  (let ((rep-expr
		  (cons 'rep
				   (append (list expr (r:l-brace) (number->string
												   min))
						      (if (eqv? max min)
								     (r:r-brace)
									    (if max
											   (list ","
													 (number->string
													  max)
													 (r:r-brace))
											      '(","
													(r:r-brace))))))))
	(if (not (equal? name #!default))
		(list (r:group name) rep-expr)
		rep-expr)))
(define (r:repeat->string expr ere groups)
  (if (is-group? expr)
	    (r:compile-group expr ere groups)
		  (string-append (r:l-paren->string ere)
						  (apply string-append 
								 (map (lambda (node)
										   (r:compile node ere
													  groups))
									   (cdr expr)))
						   (r:r-paren->string ere))))

;;; Backreferencing and grouping
(define (r:backref name)
  (cons 'backref name))
(define (r:backref->string backref groups)
  (string-append "\\" (number->string (r:group-ref groups (cdr
														      backref)))))
(define (is-backref? expr)
  (and (pair? expr)
	      (equal? (car expr) 'backref)))

(define (r:group-name group)
  (if (is-group? group)
	    (cdr (car group))
		  ""))
(define (r:group-add groups name count)
  (append groups (list name count)))
(define (r:group-ref groups name)
  (cadr (member name groups)))

#|
(define (r:get-groups expr groups)
  (let ((paren-count (+ 1 (car (last-pair groups)))))
	(cond ((is-group? expr) 
		      (let ((new-groups (r:group-add 
								   groups
								     (r:group-name expr)
									   paren-count)))
				 (r:get-groups (cadr expr) new-groups)))
		    (((type? 'seq) expr)
			    (reduce-left (lambda (groups node)
							     (r:get-groups node groups)
								   )
							 '()
							 (append (list groups) (cdr expr))))
			  ((or ((type? 'alt) expr) 
				      ((type? 'rep) expr))
			      (append groups (list paren-count)))
			    (else groups))))
|#

(define (r:get-groups expr groups)
  (let ((paren-count (+ 1 (car (last-pair groups)))))
	(cond ((is-group? expr) 
		      (let ((new-groups (r:group-add 
								   groups
								     (r:group-name expr)
									   paren-count)))
				 (r:get-groups (cadr expr) new-groups)))
		    (((type? 'seq) expr)
			    (reduce-left (lambda (groups node)
							     (r:get-groups node groups)
								   )
							 '()
							 (append (list groups) (cdr expr))))
			  ((or ((type? 'alt) expr) 
				      ((type? 'rep) expr))
			      (let ((new-groups (append
									   groups
									     (list paren-count))))
					 (reduce-left (lambda (groups node)
									  (r:get-groups node groups)
									    )
								  '()
								  (append (list new-groups) (cdr
															 expr)))))
			    (else groups))))

#|
 (r:get-groups
 (r:seq-group 'g1 
  (r:bol)
  (r:char-from "xyz" 'g2)
  (r:repeat 3 5 
(r:alt (r:quote "foo" 'g3) 
	      (r:quote "bar" 'g4)
		     (r:backref 'g2)))
  (r:seq (r:backref 'g3) (r:backref 'g4))
  "cat"
  (r:eol))
 '(0))
;Value 16: (0 g1 1 g2 2 3 4 g3 5 g4 6)
|#

(define (r:just-strings exprs)
  (or (not (pair? exprs))
	    (and (not (memq (car exprs) types-of-expr))
			    (not (is-group? exprs))
				   (not (is-backref? exprs)))))

(define types-of-expr
  '(dot bol eol quot char-from char-not-from seq alt rep))

;;; Compiling
(define (r:compile expr ere groups)
  (cond (((type? 'dot) expr) ".")
		(((type? 'bol) expr) "^")
		(((type? 'eol) expr) "$")
		(((type? 'bar) expr) (r:bar->string ere))
		(((type? 'l-paren) expr) (r:l-paren->string ere))
		(((type? 'r-paren) expr) (r:r-paren->string ere))
		(((type? 'l-brace) expr) (r:l-brace->string ere))
		(((type? 'r-brace) expr) (r:r-brace->string ere))
		(((type? 'quot) expr) (r:quote->string expr ere groups))
		(((type? 'char-from) expr) (r:char-from->string expr ere
														groups))
		(((type? 'char-not-from) expr) (r:char-not-from->string expr
																ere
																groups))
		(((type? 'seq) expr) (r:seq->string expr ere groups))
		(((type? 'alt) expr) (r:alt->string expr ere groups))
		(((type? 'rep) expr) (r:repeat->string expr ere groups))
		(((type? 'backref) expr) (r:backref->string expr groups))
		((is-group? expr) (r:compile-group expr ere groups))
		((r:just-strings expr) 
		  (if (pair? expr)
			   (string-append (apply string-append expr))
			    expr))
		((else (error "unrecognized expression type")))
		))

(define (r:compile-group group ere groups)
  (if (is-group? group)
	    (string-append (r:l-paren->string ere)
					    (r:compile (cadr group) ere groups)
						 (r:r-paren->string ere))
		  (r:compile group ere groups)))

(define (r:super-compile expr ere)
  (let ((groups (r:get-groups expr '(0))))
	(r:compile expr ere groups)))

#|
 (r:super-compile
 (r:seq-group 'g1 
  (r:bol)
  (r:char-from "xyz" 'g2)
  (r:repeat 3 5 
(r:alt (r:quote "foo" 'g3) 
	      (r:quote "bar" 'g4)
		     (r:backref 'g2)))
  (r:seq (r:backref 'g3) (r:backref 'g4))
  "cat"
  (r:eol))
 #t)
;Value 17: "(^([xyz])(((foo)|(bar)|\\2){3,5})\\5\\6cat$)"
|#


;;; Using system's grep.
(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (write-bourne-shell-egrep-command expr filename)
  (display (bourne-shell-egrep-command-string expr filename)))

(define (bourne-shell-egrep-command-string expr filename)
  (string-append "egrep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

;;; Works for any string without newlines.
(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))


;;; This is MIT/Scheme specific and compatible with grep for the
;;; purposes of this code.

(load-option 'synchronous-subprocess)

(define (r:grep expr filename)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-grep-command-string (r:super-compile expr
																#f)
											   filename)
             'output port)
            0)
		  (r:split-lines (get-output-string port)))))

(define (r:grep* expr filename)
  (let ((port (open-output-string)))
    (run-synchronous-subprocess "grep"
                                (list (r:super-compile expr #f)
									  filename)
                                'output
                                port)
    (r:split-lines (get-output-string port))))

(define (r:egrep expr filename)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-egrep-command-string (r:super-compile expr
																 #t)
												filename)
             'output port)
            0)
		  (r:split-lines (get-output-string port)))))

(define (r:egrep* expr filename)
  (let ((port (open-output-string)))
    (run-synchronous-subprocess "egrep"
                                (list (r:super-compile expr #t)
									  filename)
                                'output
                                port)
    (r:split-lines (get-output-string port))))

(define (r:split-lines string)
  (reverse
   (let ((end (string-length string)))
     (let loop ((i 0) (lines '()))
       (if (< i end)
		      (let ((j
					   (substring-find-next-char string i end
												 #\newline)))
				     (if j
						  (loop (+ j 1)
								       (cons (substring string i j)
											 lines))
						   (cons (substring string i end) lines)))
			     lines)))))

#|
;;; Showing the both grep and egrep work as intended:

(pp (r:grep (r:seq (r:quote "a") (r:dot) (r:quote "c")) "tests.txt"))
("[00]. abc" "[01]. aac"
             "[02]. acc"
             "[03]. zzzaxcqqq"
             "[10]. catcatdogdog"
             "[12]. catcatcatdogdogdog")
;Unspecified return value

(pp (r:egrep (r:seq (r:quote "a") (r:dot) (r:quote "c")) "tests.txt"))
("[00]. abc" "[01]. aac"
             "[02]. acc"
             "[03]. zzzaxcqqq"
             "[10]. catcatdogdog"
             "[12]. catcatcatdogdogdog")
;Unspecified return value

(pp (r:grep (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))
			"tests.txt"))
("[05]. foo" "[06]. bar" "[07]. foo bar baz quux")
;Unspecified return value

(pp (r:egrep (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))
			 "tests.txt"))
("[05]. foo" "[06]. bar" "[07]. foo bar baz quux")
;Unspecified return value

(pp (r:grep (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
			"tests.txt"))
("[09]. catdogcat" "[10]. catcatdogdog"
                   "[11]. dogdogcatdogdog"
                   "[12]. catcatcatdogdogdog"
                   "[13]. acatdogdogcats"
                   "[14]. ifacatdogdogs"
                   "[15]. acatdogdogsme")
;Unspecified return value

(pp (r:egrep (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
			 "tests.txt"))
("[09]. catdogcat" "[10]. catcatdogdog"
                   "[11]. dogdogcatdogdog"
                   "[12]. catcatcatdogdogdog"
                   "[13]. acatdogdogcats"
                   "[14]. ifacatdogdogs"
                   "[15]. acatdogdogsme")
;Unspecified return value

(pp
 (r:grep (r:seq " "
				(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
				(r:eol)) 
         "tests.txt"))
("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog")
;Unspecified return value

(pp
 (r:egrep (r:seq " "
				 (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote
													   "dog")))
				 (r:eol)) 
         "tests.txt"))
("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog")
;Unspecified return value

(pp
 (r:grep
  (let ((digit 
		  (r:char-from "0123456789")))
    (r:seq (r:bol)
		      (r:quote "[")
			     digit
				    digit
					   (r:quote "]")
					      (r:quote ".")
						     (r:quote " ")
							    (r:char-from "ab")
								   (r:repeat 3 5 (r:alt "cat" "dog"))
								      (r:char-not-from "def")
									     (r:eol)))
  "tests.txt"))
("[13]. acatdogdogcats")
;Unspecified return value

(pp
 (r:egrep
  (let ((digit 
		  (r:char-from "0123456789")))
    (r:seq (r:bol)
		      (r:quote "[")
			     digit
				    digit
					   (r:quote "]")
					      (r:quote ".")
						     (r:quote " ")
							    (r:char-from "ab")
								   (r:repeat 3 5 (r:alt "cat" "dog"))
								      (r:char-not-from "def")
									     (r:eol)))
  "tests.txt"))
("[13]. acatdogdogcats")
;Unspecified return value
|#
