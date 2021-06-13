#lang reader "reader.rkt"
#|
(require syntax/parse/define)

(define-syntax-parse-rule (#%string-literal-abc s)
  (string-append s s))

(define-syntax-parse-rule (#%string-literal-f s)
  "i ate your string, sorry")

#f"this is a format string {foo} {bar}"

(module+ test
  (require rackunit)
  (check-equal? #abc"def" "defdef"))

|#

#f

#xyz"xyz"
