(library (json parser)
 (export json-parse)
 (import (rnrs))

 ; parser with textual-input port
 (define-record-type parser
  (fields port))

 (define (peek-next-char parser)
   (lookahead-char (parser-port parser)))

 (define (get-next-char parser)
   (get-char (parser-port parser)))

 (define (expect-char parser expected)
   (let ((ch (get-next-char parser)))
    (if (not (char-ci=? ch expected))
     (raise (make-violation))
     ch)))

 (define (expect-string parser expected)
   (for-each (lambda (ch) (expect-char parser ch))
             (string->list expected))
   #t)

 (define (parse-read-true parser)
  (expect-string parser "true")
  #t)

 (define (parse-read-false parser)
  (expect-string parser "false")
  #f)

 (define (parse-read-null parser)
  (expect-string parser "null")
  '())

 ;parse string
 (define (parse-read-string parser)
  (parse-read-till parser #\")  ;drop begining "
  (let loop ((str "")
             (ch (get-next-char parser)))
   (if (not (char=? ch #\"))
    (loop (string-append str (string ch))
          (get-next-char parser))
    str)))

 ;parse array
 (define (parse-read-till parser expect)
  (let loop ((ch (get-next-char parser)))
   (if (char=? ch expect)
    '()
    (loop (get-next-char parser)))))

 (define (parse-read-array parser)
  (parse-read-till parser #\[)  ;drop begining [
  (let loop ((ch (peek-next-char parser))
             (acc '()))
   (cond
    ((char-whitespace? ch) (get-next-char parser) (loop (peek-next-char parser) acc))
    ((eqv? ch #\,) (get-next-char parser) (loop (peek-next-char parser) acc))
    ((eqv? ch #\]) (get-next-char parser) acc)
    (else (let ((r (append acc (cons (parse-read-value parser) '()))))
           (loop (peek-next-char parser) r))))))

 ;parse object
 (define (parse-read-object-entry parser)
  (let ((key (parse-read-string parser)))
   (parse-read-till parser #\:)
   (cons key (parse-read-value parser))))

 (define (parse-read-object parser)
  (parse-read-till parser #\{)  ;drop begining {
  (let loop ((acc '())
             (ch (peek-next-char parser)))
   (cond 
    ((char-whitespace? ch) (get-next-char parser) (loop acc (peek-next-char parser)))
    ((eqv? ch #\,) (get-next-char parser) (loop acc (peek-next-char parser)))
    ((eqv? ch #\}) (get-next-char parser) acc)
    (else (let ((r (parse-read-object-entry parser)))
           (loop (append acc (cons r '())) (peek-next-char parser)))))))

 ;parse number
 (define (parse-read-number-realpart parser acc)
  (assert (not (char-ci=? (peek-next-char parser) #\e)))

  (let loop ((acc2 acc)
             (ch (peek-next-char parser)))
   (cond
    ((eof-object? ch) (string->number acc2))
    ((or (char-ci=? #\e ch)
         (char-numeric? ch)) (get-next-char parser) (loop (string-append acc2 (string ch)) (peek-next-char parser)))
    ((not (char-numeric? ch)) (string->number acc2))
    (else (raise (make-violation))))))

 (define (parse-read-number parser)
  (let loop ((acc "")
             (ch (peek-next-char parser)))
   (cond
    ((eof-object? ch) (string->number acc))
    ((eqv? ch #\.) (get-next-char parser) (parse-read-number-realpart parser (string-append acc (string ch))))
    ((or (eqv? #\- ch)
         (eqv? #\+ ch)
         (char-numeric? ch)) (get-next-char parser) (loop (string-append acc (string ch)) (peek-next-char parser)))
    ((not (char-numeric? ch)) (string->number acc))
    (else (raise (make-violation))))))

 ;parse dispatch
 (define parse-read-value
  (lambda (parser)
   (let loop ((c (peek-next-char parser)))
    (cond 
     ;skip white space
     ((char-whitespace? c) (get-next-char parser) (loop (peek-next-char parser)))
     ((eqv? #\t c) (parse-read-true parser))
     ((eqv? #\f c) (parse-read-false parser))
     ((eqv? #\" c) (parse-read-string parser))
     ((eqv? #\[ c) (parse-read-array parser))
     ((eqv? #\{ c) (parse-read-object parser))
     ((eqv? #\n c) (parse-read-null parser))
     ((or (eqv? #\+ c)
          (eqv? #\- c)
          (char-numeric? c)) (parse-read-number parser))
     (else (raise (make-violation)))))))

 ;Interface
 (define json-parse
  (case-lambda
   (()  
    (parse-read-value (make-parser (current-input-port))))
   ((str) 
    (call-with-port (open-string-input-port str)
       (lambda (port)
        (parse-read-value (make-parser port)))))))
)
