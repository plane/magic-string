#lang racket/base

(provide make-magic-string-readtable
         magic-string-read
         magic-string-read-syntax
         wrapper1)

;;
;; reader for Julia-style non-standard string literals, called "magic strings"
;; here for brevity's sake
;;
;; https://docs.julialang.org/en/v1/manual/metaprogramming/#Non-Standard-String-Literals
;;

(require racket/list
         racket/port
         racket/syntax
         syntax/strip-context
         threading)

;; read a magic-string:
;;
;;     #name"arg"          =>  (#%string-literal-name "arg")
;;     #name#"arg"         =>  (#%string-literal-name# #"arg")
;;     #name"arg"opts      =>  (#%string-literal-name "arg" #:opts "opts")
;;     #name#"arg"opts     =>  (#%string-literal-name# #"arg" #:opts "opts")
;;
;; it can be a byte-string or have opts; opts are stringified.
;;
(define (read-magic-string src in ch readtable)
  (strip-context
   (let* ([name     (read-syntax/recursive src in #f readtable)]
          [name-fmt (format-id #f "#%string-literal-~a" name)]
          [bytes?   (syntax-ends-with-#? name)]
          [in       (if bytes? (char+port #\# in) in)]
          [str-arg  (read-syntax/recursive src in #f readtable)]
          [opts?    (opts-char? (peek-char in))]
          [opts     (and opts? (read-syntax/recursive src in #f readtable))]
          [opts-str (and opts? (symbol->string (syntax-e opts)))])
     (cond
       [opts? #`(#,name-fmt #,str-arg #:opts #,opts-str)]
       [else  #`(#,name-fmt #,str-arg)]))))

;;
;; read ahead in the input stream to see if we've got a magic string.
;; if so, read it with `read-magic-string`; otherwise, read normally
;;
(define ((make-magic-string-proc readtable) ch in src line col pos)
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
         (read-syntax/recursive src in ch readtable)]))))

;;
;; #%string-literal-... names and post-string options both continue until one
;; of these chars is found, or end-of-file is reached
;;
(define (name-char? ch)
  (define eof? (eof-object? ch))
  (case ch
    [(#\space
      #\tab
      #\newline
      #\0
      #\1
      #\2
      #\3
      #\4
      #\5
      #\6
      #\7
      #\8
      #\9
      #\"
      #\#
      #\(
      #\)
      #\[
      #\]
      #\{
      #\}) #f]
    [else
     (not eof?)]))

(define (opts-char? ch)
  (name-char? ch))

;;
;; used to essentially push a char we've already read back onto the port,
;; although of course we return a new port instead of mutating the existing
;; port here
;;
(define (char+port ch in)
  (define prefix-str (string ch))
  (define prefix-port (open-input-string prefix-str))
  (input-port-append #f prefix-port in))

;;
;; this is used to check if we have a bytestring or not
;;
(define (syntax-ends-with-#? syntax)
  (~> syntax
      syntax-e
      symbol->string
      string->list
      last
      (equal? #\#)))

;;
;; we'll take over # in the readtable so we can check for magic strings,
;; but we'll fall through to the default dispatch table if we don't find one
;;
(define (make-magic-string-readtable [orig-readtable (current-readtable)])
  (make-readtable orig-readtable
                  #\#
                  'non-terminating-macro
                  (make-magic-string-proc orig-readtable)))

;;
;; these functions allow our #lang to use the modified readtable
;;
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

;;
;; we'll test our reader here, but we aren't concerned with the details
;; of any of the #%string-literal-.+#? forms here; they must be defined
;; provided and tested separately
;;
(module+ test
  (require rackunit/chk)

  (define (read-test s)
    (magic-string-read
     (open-input-string s)))

  (chk
   ;; first, let's make sure we didn't break any existing forms
   (read-test "#t")               #true
   (read-test "#f")               #false
   (read-test "#T")               #true
   (read-test "#F")               #false
   (read-test "#true")            #true
   (read-test "#false")           #false
   (read-test "#(1 2 3)")         '#(1 2 3)
   (read-test "#[1 2 3]")         '#(1 2 3)
   (read-test "#{1 2 3}")         '#(1 2 3)
   (read-test "#3(foo bar)")      '#(foo bar bar)
   (read-test "#5()")             '#(0 0 0 0 0)
   (read-test "#\"abc\"")         '#"abc"

   ;; we'll clobber RE support though; it can be trivially reimplemented
   (read-test "#rx\"re-test\"")   '(#%string-literal-rx "re-test")
   (read-test "#rx#\"re-test\"")  '(#%string-literal-rx# #"re-test")
   (read-test "#px\"re-test\"")   '(#%string-literal-px "re-test")
   (read-test "#px#\"re-test\"")  '(#%string-literal-px# #"re-test")

   ;; now let's test some magic-strings
   (read-test "#f\"f-test\"")     '(#%string-literal-f "f-test")
   (read-test "#foo\"bar\"")      '(#%string-literal-foo "bar")

   ;; they can have optional arguments, which are stringified
   (read-test "#foo\"bar\"baz")   '(#%string-literal-foo "bar" #:opts "baz")))
