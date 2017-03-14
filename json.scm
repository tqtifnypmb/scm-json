(library (tqti json)
 (export json->string
         string->json)
 (import (json builder)
         (json parser)
         (rnrs))

 (define (json->string obj pretty)
  (scm->json obj pretty))

 (define (string->json str)
  (json-parse str))
)
