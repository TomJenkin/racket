#lang racket

(provide pipe
         compose-pipe)

;; pipeline macro
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

