#lang racket

(require rackunit
         syntax/location)

(provide pipe
         compose-pipe
         assert
         enumerate
         slice
         rolling
         sum
         mean
         std-dev
         std-dev-sample
         var
         var-sample
         transpose
         date->dd/mm/yyyy)

#| =================== private =================== |#

;; square
(define (square x) (* x x))

;; pad-left
(define (pad-left n width)
  (define s (number->string n))
  (string-append (make-string (max 0 (- width (string-length s))) #\0) s))

#| =================== public =================== |#

;; pipeline macro
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

;; assert
(define (assert condition [msg "assertion failed"])
  (unless condition
    (error 'assert msg)))

;; enumerate
(define (enumerate ls [start 0])
  (for/list ([value ls]
             [ix (in-naturals start)])
    (cons ix value)))

;; transpose list of lists
(define (transpose dt)
  (apply map list dt))

;; slice
(define (slice ls n m)
  (take (list-tail ls n) (- m n)))

;; rolling stats (e.g. rolling mean, std, etc.)
(define (rolling fn n ls)
  (for/list ([i (in-range (length ls))])
    (if (< i (sub1 n)) "" (fn (take (list-tail ls (- i (sub1 n))) n)))))

;; sum
(define (sum ls)
  (apply + ls))

;; mean
(define (mean ls)
  (/ (apply + ls) (length ls)))

;; stdev
(define (std-dev ls)
  (let ([μ (mean ls)]
        [n (length ls)])
    (sqrt (/ (apply + (map (λ (x) (square (- x μ))) ls)) n))))

;; std (sample version)
(define (std-dev-sample ls)
  (let ([μ (mean ls)]
        [n (length ls)])
    (sqrt (/ (apply + (map (λ (x) (square (- x μ))) ls)) (sub1 n)))))

;; variance
(define (var ls)
  (let* ([μ (mean ls)]
         [n (length ls)]
         [sum-sq-diff (apply + (map (λ (x) (expt (- x μ) 2)) ls))])
    (/ sum-sq-diff n)))  ; population variance

;; variance sample
(define (var-sample ls)
  (let* ([μ (mean ls)]
         [n (length ls)]
         [sum-sq-diff (apply + (map (λ (x) (expt (- x μ) 2)) ls))])
    (/ sum-sq-diff (sub1 n))))  ; sample variance (unbiased)

;; date to string
(define (date->dd/mm/yyyy d)
  (string-append
   (pad-left (date-day d) 2) "/"
   (pad-left (date-month d) 2) "/"
   (number->string (date-year d))))

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  (check-equal? (pipe '(1 2 3 4 5) cdr cdr car) 3)

  (check-equal? ((compose-pipe cdr cdr car) '(1 2 3 4 5)) 3)

  (check-exn exn:fail? (lambda () (assert #f)))

  (check-equal? (enumerate '(1 2 3)) '((0 . 1) (1 . 2) (2 . 3)))

  (check-equal? (list-ref (enumerate '(1 2 3 4 5) 1) 2) '(3 . 3))

  (check-equal? (slice '(0 1 2 3 4 5) 0 3) '(0 1 2))

  (check-equal? (rolling mean 3 '(0 1 2 3 4 5)) '("" "" 1 2 3 4))

  (define (var-test ls) (square (std-dev ls)))
  (check-= (var '(0 1 2 3 4 5)) (var-test '(0 1 2 3 4 5)) 1e-10)

  (check-equal? (rolling sum 3 '(0 1 2 3 4 5)) '("" "" 3 6 9 12))

  (check-equal? (transpose '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))

  (define d1 (date 0 0 0 14 1 2016 0 0 #f 0))
  (check-equal? (date->dd/mm/yyyy d1) "14/01/2016")
 
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
