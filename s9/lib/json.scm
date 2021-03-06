; Scheme 9 from Empty Space, JSON Library
; By McKay Marston, 2018
; Placed in the Public Domain

; https://www.ietf.org/rfc/rfc4627.txt
; inspired by: https://github.com/tqtifnypmb/scm-json

; (json:load string)      ==> list
; (string->json string)   ==> list
; (json->string list)     ==> string
;
; (load-from-library "json.scm")
;
; JSON:LOAD takes a file (specified in STRING) and loads it into a list. JSON
; types are represented with scheme types as follows:
;
;  array      --> vector
;  dictionary --> list of pairs
;  string     --> string
;  number     --> number
;  null       --> '()
;  true       --> #t
;  false      --> #f
;
; STRING->JSON takes a string and loads into a JSON-style list.
;
; JSON->STRING takes a compatible list and returns a JSON-style string.
;
; Example: (string->json "{\"key\": \"value\"}") ==> (("key" . "value"))
;          (json->string '(("key" . "value"))) ==> "{\"key\":\"value\"}"

(load-from-library "format.scm")
(load-from-library "simple-modules.scm")

(module json
  (define (read-character f)
    (do ((c (read-char f) (read-char f)))
	((or (eof-object? c)
	     (not (char-whitespace? c))) c)))

  (define (peek-character f)
    (do ((c (peek-char f) (peek-char f)))
	((or (eof-object? c)
	     (not (char-whitespace? c))) c)
      (read-char f)))

  (define (want-word file wanted)
    (for-each (lambda (ch) (read-character file))
	      (string->list wanted))
    #t)

  (define (read-true file)
    (want-word file "true")
    #t)

  (define (read-false file)
    (want-word file "false")
    #f)

  (define (read-null file)
    (want-word file "null")
    '())

  (define (read-word file)
    (let loop ((word "")
	       (ch (read-char file)))
      (if (not (or (eof-object? ch)
		   (char=? ch #\")))
	  (loop (string-append word (string ch))
		(read-char file))
	  word)))

  (define (read-fraction file number)
    (let loop ((value number)
	       (ch (peek-character file)))
      (cond
       ((eof-object? ch) (string->number value))
       ((or (char-ci=? #\e ch)
	    (char-numeric? ch)) (read-character file) (loop (string-append value (string ch)) (peek-character file)))
       ((not (char-numeric? ch)) (string->number value))
       (else (error "not a fractional number" (list number value ch))))))

  (define (read-number file)
    (let loop ((number "")
	       (ch (peek-character file)))
      (cond
       ((eof-object? ch) (string->number number))
       ((eqv? ch #\.) (read-character file) (read-fraction file (string-append number (string ch))))
       ((or (eqv? #\- ch)
	    (eqv? #\+ ch)
	    (char-numeric? ch)) (read-character file) (loop (string-append number (string ch))
							    (peek-character file)))
       ((not (char-numeric? ch)) (string->number number))
       (else (error "failed to read number" (cons number ch))))))

  (define (read-object-entry file)
    (if (or (eqv? #\: (peek-character file))
	    (eqv? #\" (peek-character file)))
	(read-character file))
    (let ((key (read-word file)))
      (read-character file) ; throw away #\:
      (cons key (parse file))))

  (define (read-object file)
    (let loop ((acc '())
	       (c (peek-character file)))
      (cond
       ((or (eqv? c #\}) (eof-object? c)) (read-character file) acc)
       ((eqv? c #\,) (read-character file) (loop acc (peek-character file)))
       (else
	(let ((r (read-object-entry file)))
	  (loop (append acc (cons r '()))
		(peek-character file)))))))

  (define (read-array file)
    (list->vector
     (let loop ((ch (peek-character file))
		(array '()))
       (cond
	((char-whitespace? ch) (read-character file) (loop (peek-character file) array))
	((eqv? ch #\,) (read-character file) (loop (peek-character file) array))
	((eqv? ch #\]) (read-character file) array)
	(else (let ((r (append array (cons (parse file) '()))))
		(loop (peek-character file) r)))))))

  (define* (parse file)
    (let loop ((ch (peek-character file)))
      (cond ((eqv? #\{ ch) (read-character file) (read-object file))
	    ((eqv? #\[ ch) (read-character file) (read-array file))
	    ((eqv? #\t ch) (read-true file))
	    ((eqv? #\f ch) (read-false file))
	    ((eqv? #\n ch) (read-null file))
	    ((or (eqv? #\+ ch)
		 (eqv? #\- ch)
		 (char-numeric? ch)) (read-number file))
	    ((eqv? #\" ch) (read-character file) (read-word file))
	    (else (error "parse error" ch)))))

  (define* (jload file)
    (call-with-input-file file
      (lambda (x)
	(parse x))))

  (define (q datum)
    (cond ((string? datum) (format #f "\"~A\"" datum))
	  ((number? datum) (format #f "~D" datum))
	  ((eq? datum #t) "true")
	  ((eq? datum #f) "false")))

  ;; it must be key: value, at least
  (define* (dump block)
    (cond ((null? block) "")
	  ((string? (car block))
	   (cond ((null? (cdr block))
		  (format #f "~A:null" (q (car block))))
		 ((list? (cdr block))
		  (format #f "~A:~A" (q (car block)) (dump (cdr block))))
		 ((vector? (cdr block))
		  (format #f "~A:[~A]" (q (car block))
			  (let ((entries
				 (map q (vector->list (cdr block)))))
			    (string-unsplit #\, entries))))
		 (else
		  (format #f "~A:~A" (q (car block)) (q (cdr block))))))
	  ((list? block)
	   (format #f "{~A}"
		   (let ((entries
			  (map dump block)))
		     (string-unsplit #\, entries))))
	  (else
	   (error "unknown block type" block))))

) ;; module


(define-syntax json:load
  (lambda (file)
    `(using json (jload) (jload ,file))))

(define string->json
  (lambda (source)
    (let* ((p   (sys:pipe))
	   (in  (sys:make-input-port (car p)))
	   (out (sys:make-output-port (cadr p))))
      (display source out)
      (sys:flush out)
      (using json (parse) (parse in)))))

(define json->string
  (lambda (source)
    (using json (dump) (dump source))))
