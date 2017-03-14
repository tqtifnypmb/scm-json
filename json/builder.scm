(library (json builder)
 (export json-build)
 (import (rnrs))

 (define-record-type builder
  (fields 
   (mutable result)
   (mutable num-indent)
   (immutable pretty builder-is-pretty)))

 (define (builder-inc-indent builder)
  (builder-num-indent-set! builder (+ (builder-num-indent builder) 1)))

 (define (builder-dec-indent builder)
  (builder-num-indent-set! builder (- (builder-num-indent builder) 1)))

 (define (builder-begin-array builder)
   (if (builder-is-pretty builder)
    (let ((indent (builder-indent-string-1 builder)))
     (builder-result-set! builder (string-append indent "[\n"))
     (builder-inc-indent builder))
    (builder-result-set! builder "[")))

 (define (builder-end-imp builder empty-len sub-len deli)
  (let ((result (builder-result builder)))
   (if (eq? (string-length result) empty-len)
    (string-append result deli)
    (string-append (substring result 0 (- (string-length result) sub-len)) deli))))

 (define (builder-indent-string builder)
  (let loop ((num (builder-num-indent builder))
             (acc ""))
   (if (= num 0)
    acc
    (loop (- num 1) (string-append acc "\t")))))

 (define (builder-indent-string-1 builder)
  (if (> (builder-num-indent builder) 1)
   (begin (builder-dec-indent builder)
          (let ((indent (builder-indent-string builder)))
           (builder-inc-indent builder)
           indent))
   (builder-indent-string builder)))


 (define (builder-end-array-imp builder empty-len sub-len)
  (if (builder-is-pretty builder)
   (begin (builder-dec-indent builder)
          (let ((indent (builder-indent-string builder)))
           (builder-end-imp builder empty-len sub-len (string-append "\n" indent "]"))))
   (builder-end-imp builder empty-len sub-len "]")))

 (define (builder-end-array builder)
  (if (builder-is-pretty builder)
   (builder-end-array-imp builder 2 2)
   (builder-end-array-imp builder 1 1)))

 (define (builder-begin-object builder)
  (if (builder-is-pretty builder)
   (let ((indent (builder-indent-string-1 builder)))
    (builder-result-set! builder (string-append indent "{\n"))
    (builder-inc-indent builder))
   (builder-result-set! builder "{")))
 
 (define (builder-end-object-imp builder empty-len sub-len)
  (if (builder-is-pretty builder)
   (begin (builder-dec-indent builder)
          (let ((indent (builder-indent-string builder)))
           (builder-end-imp builder empty-len sub-len (string-append "\n" indent "}"))))
   (builder-end-imp builder empty-len sub-len "}")))

 (define (builder-end-object builder)
  (if (builder-is-pretty builder)
   (builder-end-object-imp builder 2 2)
   (builder-end-object-imp builder 1 1)))

 (define (builder-append-value-imp builder pre val deli)
  (letrec* ((result (builder-result builder))
            (p+v (string-append pre val))
            (p+r+v (string-append result p+v))
            (r (string-append p+r+v deli)))
   (builder-result-set! builder r)))

 (define (builder-append-value builder val)
  (if (builder-is-pretty builder)
   (let ((indent (builder-indent-string builder)))
    (builder-append-value-imp builder indent val ",\n"))
   (builder-append-value-imp builder "" val ",")))

 (define (list->array val pretty level)
  (let ((b (make-builder "" level pretty)))
   (builder-begin-array b)
   (let loop ((val val))
    (if (not (pair? val))
     (builder-end-array b)
     (begin (builder-append-value b (json-build-value (car val) pretty (+ level 1)))
            (loop (cdr val)))))))

 (define (json-build-read-object-entry val pretty level)
  (assert (pair? val))
  (let* ((key (json-build-value (car val) pretty level))
         (value (json-build-value (cdr val) pretty level)))
   (string-append key ": " value)))

 (define (list->object val pretty level)
  (let ((b (make-builder "" level pretty)))
   (builder-begin-object b)
   (let loop ((val val))
    (if (not (pair? val))
     (builder-end-object b)
     (begin (builder-append-value b (json-build-read-object-entry (car val) pretty (+ level 1)))
            (loop (cdr val)))))))

 (define (list->json val pretty level)
  (let ((first (car val)))
   (if (pair? first)
    (list->object val pretty level)
    (list->array val pretty level))))

 (define (json-build-value val pretty level)
  (cond
   ((number? val) (number->string val))
   ((boolean? val) (if (eqv? val #t) "true" "false"))
   ((eqv? '() val) "null")
   ((list? val) (list->json val pretty level))
   ((string? val) (string-append "\"" val "\""))
   (else (raise (make-violation)))))

 ;Interface
 (define (json-build val pretty)
  (json-build-value val pretty 1))
)
