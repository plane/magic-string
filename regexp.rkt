#lang racket/base

(provide #%string-literal-rx
         #%string-literal-px
         #%string-literal-rx#
         #%string-literal-px#)

(require syntax/parse/define)

(define-syntax-parse-rule
  (#%string-literal-rx s)
  (regexp s))

(define-syntax-parse-rule
  (#%string-literal-px s)
  (pregexp s))

(define-syntax-parse-rule
  (#%string-literal-rx# bs)
  (byte-regexp bs))

(define-syntax-parse-rule
  (#%string-literal-px# bs)
  (byte-pregexp bs))
