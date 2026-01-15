#lang racket

(require rackunit
         syntax/location)

(provide pipe
         compose-pipe)

#| =================== public =================== |#

;; pipeline macro
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  (check-equal? (pipe '(1 2 3 4 5) cdr cdr car) 3)

  (check-equal? ((compose-pipe cdr cdr car) '(1 2 3 4 5)) 3)
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
