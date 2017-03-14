(library (tqti json)
 (export json->string
         string->json)
 (import (json builder)
         (json parser)
         (rnrs))

 (define-syntax json->string
  (syntax-rules ()
   ((_ obj pretty) (json-build obj pretty))))

 (define-syntax string->json
  (syntax-rules ()
   ((_ str) (json-parse str))))
)
