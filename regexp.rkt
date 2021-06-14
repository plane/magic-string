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
  (#%string-literal-rx# s)
  (byte-regexp s))

(define-syntax-parse-rule
  (#%string-literal-px# s)
  (byte-pregexp s))
