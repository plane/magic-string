#lang racket/base

(provide make-magic-string-readtable
         magic-string-read
         magic-string-read-syntax
         wrapper1)

(require racket/list
         racket/port
         racket/syntax
         syntax/strip-context
         threading)

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

(define opts-char? name-char?)

(define (char+port ch in)
  (define prefix-str (string ch))
  (define prefix-port (open-input-string prefix-str))
  (input-port-append #f prefix-port in))

(define (ends-with-#? syntax)
  (~> syntax
      syntax-e
      symbol->string
      string->list
      last
      (equal? #\#)))

(define (read-magic-string src in ch readtable)
  (strip-context
   (let* ([name     (read-syntax/recursive src in #f readtable)]
          [name-fmt (format-id #f "#%string-literal-~a" name)]
          [bytes?   (ends-with-#? name)]
          [in       (if bytes? (char+port #\# in) in)]
          [str-arg  (read-syntax/recursive src in #f readtable)]
          [opts?    (opts-char? (peek-char in))]
          [opts     (and opts? (read-syntax/recursive src in #f readtable))]
          [opts-str (and opts? (symbol->string (syntax-e opts)))])
     (cond
       [opts? #`(#,name-fmt #,str-arg #:opts #,opts-str)]
       [else  #`(#,name-fmt #,str-arg)]))))

(define (make-magic-string-proc readtable)
  (lambda (ch in src line col pos)
    (define peek-in (peeking-input-port in))
    (let loop ([name-char-count 0])
      (let* ([peek-ch            (read-char peek-in)]
             [peek-ch+           (peek-char peek-in)]
             [name-char-next?    (name-char? peek-ch)]
             [string-next?       (equal? peek-ch #\")]
             [bytestring-next?   (and (equal? peek-ch #\#)
                                      (equal? peek-ch+ #\"))]
             [any-string-next?   (or string-next? 
                                     bytestring-next?)]
             [magic-string-next? (and any-string-next?
                                      (positive? name-char-count))])
        (cond
          [magic-string-next?
           (read-magic-string src in ch readtable)]
          [name-char-next?
           (loop
            (add1 name-char-count))]
          [else
           (read-syntax/recursive src in ch readtable)])))))
  
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
   (read-test "#rx\"re-test\"") '(#%string-literal-rx "re-test")
   (read-test "#rx#\"re-test\"") '(#%string-literal-rx# #"re-test")
   (read-test "#px\"re-test\"") '(#%string-literal-px "re-test")
   (read-test "#px#\"re-test\"") '(#%string-literal-px# #"re-test")
   (read-test "#foo\"bar\"") '(#%string-literal-foo "bar")
   (read-test "#foo\"bar\"baz") '(#%string-literal-foo "bar" #:opts "baz")))
