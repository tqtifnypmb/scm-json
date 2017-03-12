(import (json parser) (rnrs))

(define (expect-equal v1 v2)
 (assert (equal? v1 v2)))

(expect-equal (json-parse "\"abcdef\"") "abcdef")

(expect-equal (json-parse "true") #t)

(expect-equal (json-parse "false") #f)
