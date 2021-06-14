#lang reader "reader.rkt"
(require syntax/parse
         syntax/parse/define
         (for-syntax racket/base))

(require "regexp.rkt")

(define-syntax-parse-rule (#%string-literal-abc s)
  (string-append s s))

(define-syntax-parser #%string-literal-f
  [(_ args #:opts opts)
   (syntax "i ate your string, sorry")]
  [(_ args)
   (syntax "no options")])

#px"[\\s]*"
#px#"[\\s]*"
	
#f"this is a format string {foo} {bar}"
#f"this is a format string {foo} {bar}"opts

(module+ test
  (require rackunit)
  (check-equal? #abc"def" "defdef"))

#f

(string-append "foo" #abc"xyz")

#px"hi"