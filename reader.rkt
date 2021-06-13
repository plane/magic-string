#lang s-exp syntax/module-reader
racket/base
#:read magic-string-read
#:read-syntax magic-string-read-syntax
#:wrapper1 wrapper1

(require "readtable.rkt")
