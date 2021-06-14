#lang racket/base

(provide make-magic-string-readtable
         magic-string-read
         magic-string-read-syntax
         wrapper1)

(require racket/port
         racket/syntax
         syntax/strip-context)

(define eof? eof-object?)

(define not-eof?
  (compose not eof?))

(define (name-char? ch)
  (case ch
    [(#\space
      #\tab
      #\newline
      #\"
      #\#
      #\(
      #\)
      #\[
      #\]
      #\{
      #\}) #f]
    [else
     (not-eof? ch)]))

(define (read-magic-string src in ch readtable)
  (let* ([name  (read-syntax/recursive src in #f readtable)]
         [name? (not-eof? name)]
         [name  (format-id #f "#%string-literal-~a" name)]
         [data  (read-syntax/recursive src in #f readtable)]
         [data? (not-eof? data)]
         [opts? (name-char? (peek-char in))]
         [opts  (and opts? (read-syntax/recursive src in #f readtable))]
         [opts  (and opts? (symbol->string (syntax-e opts)))])
    (cond
      [(not name?) #f]
      [(not data?) #f]
      [opts?       (strip-context #`(#,name #,data #:opts #,opts))]
      [else        (strip-context #`(#,name #,data))])))

(define (make-magic-string-proc readtable)
  (lambda (ch in src line col pos)
    (define peek-in (peeking-input-port in))
    (let loop ([chars-ahead 0])
      (define peek-ch (read-char peek-in))
      (cond
        [(and (equal? peek-ch #\")
              (positive? chars-ahead))
         (read-magic-string src in ch readtable)]
        [(name-char? peek-ch)
         (loop (add1 chars-ahead))]
        [else (read-syntax/recursive src in ch readtable)]))))
  
(define (make-magic-string-readtable [orig-readtable (current-readtable)])
  (make-readtable orig-readtable
                  #\#
                  'non-terminating-macro
                  (make-magic-string-proc orig-readtable)))

(define (magic-string-read in)
  (parameterize ([current-readtable (make-magic-string-readtable)])
    (read in)))
 
(define (magic-string-read-syntax src in)
  (parameterize ([current-readtable (make-magic-string-readtable)])
    (read-syntax src in)))

(define (wrapper1 thunk)
  (define readtable (make-magic-string-readtable (current-readtable)))
  (parameterize ([current-readtable readtable])
    (thunk)))

(module+ test
  (require rackunit/chk)
  (define (read-test s)
    (magic-string-read
     (open-input-string s)))
  (chk
   (read-test "#f") #f
   (read-test "#t") #t
   (read-test "#false") #false
   (read-test "#true") #true
   (read-test "#\"abc\"") '#"abc"
   (read-test "#f\"f-test\"") '(#%string-literal-f "f-test")
   (read-test "#foo\"bar\"") '(#%string-literal-foo "bar")
   (read-test "#foo\"bar\"baz") '(#%string-literal-foo "bar" #:opts "baz")))
