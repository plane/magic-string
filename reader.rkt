#lang s-exp syntax/module-reader
racket/base
#:read magic-string-read
#:read-syntax magic-string-read-syntax
#:wrapper1 wrapper1

(provide #%string-literal-rx
         #%string-literal-px)

(require "readtable.rkt")

(require syntax/parse/define)

(define-syntax-parse-rule
  (#%string-literal-rx s)
  (regexp s))

(define-syntax-parse-rule
  (#%string-literal-px s)
  (regexp s))