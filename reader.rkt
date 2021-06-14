#lang s-exp syntax/module-reader
racket/base
#:read magic-string-read
#:read-syntax magic-string-read-syntax
#:wrapper1 wrapper1

(require "readtable.rkt")

;; these functions probably don't belong in here, TODO move
(provide #%string-literal-rx
         #%string-literal-px)

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

