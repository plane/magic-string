#lang racket/base

(provide make-magic-string-readtable
         magic-string-read
         magic-string-read-syntax
         wrapper1)

(require racket/port
         racket/syntax
         syntax/strip-context)

(define (not-eof . xs)
  (define (not-eof* x)
    (not (equal? x eof)))
  (andmap not-eof* xs))

(define (read-magic-string src in ch readtable)
  (let* ([name  (read-syntax/recursive src in #f readtable)]
         [arg   (read-syntax/recursive src in #f readtable)]
         [name* (format-id name "#%string-literal-~a" name)])
    (if (not-eof name arg)
        (strip-context 
          #`(#,name* #,arg))
        #f)))

(define (name-char? ch)
  (case ch
    [(#\space
      #\(
      #\)
      #\[
      #\]
      #\{
      #\}
      #\#) #f]
    [else
     (not-eof ch)]))

(define (char+port ch in)
  (define prefix-str (string ch))
  (define prefix-port (open-input-string prefix-str))
  (input-port-append #f prefix-port in))

(define (make-magic-string-proc readtable)
  (lambda (ch in src line col pos)
    (define peek-in (peeking-input-port in))
    (let loop ()
      (define peek-ch (read-char peek-in))
      (cond
        [(equal? peek-ch #\")
         (read-magic-string src in ch readtable)]
        [(name-char? peek-ch)
         (loop)]
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
  (require rackunit)
  (define (read-test s)
    (parameterize ([current-readtable (make-magic-string-readtable)])
      (read (open-input-string s))))
  (check-equal? (read-test "#f") #f))
