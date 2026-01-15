#lang racket

(provide pipe
         compose-pipe)

;; pipeline macro named pipe (not currently used here but move to general lib)
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

