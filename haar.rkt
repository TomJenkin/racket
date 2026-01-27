#lang racket

(require rackunit
         syntax/location
         (prefix-in gt: "gen-tools.rkt"))

(provide haar
         haar-inv
         haar-slice)

(define module-name (path->string (syntax-source-file-name #'here)))

(define (haar xs)
  (define n (length xs))
  (unless (even? n) (error 'haar1 "list length must be even"))
  (define k (/ 1.0 (sqrt 2.0)))
  (define As (for/list ([i (in-range 0 n 2)])
               (* k (+ (list-ref xs i) (list-ref xs (add1 i))))))
  (define Ds (for/list ([i (in-range 0 n 2)])
               (* k (- (list-ref xs i) (list-ref xs (add1 i))))))
  (append As Ds))

;; inverse haar
(define (haar-inv coeffs)
  (define n (length coeffs))
  (unless (even? n) (error 'inv-haar1 "coeff length must be even"))
  (define k (/ 1.0 (sqrt 2.0)))
  (define half (quotient n 2))
  (define As (take coeffs half))
  (define Ds (drop coeffs half))
  (append*
   (for/list ([a (in-list As)] [d (in-list Ds)])
     (list (* k (+ a d))
           (* k (- a d))))))

(define (haar-slice coeffs a b)
  (unless (and (real? a) (real? b) (<= 0 a b 1))
    (error 'haar-slice "need 0 <= a <= b <= 1"))
  (define n (length coeffs))
  (define idxs (range n))
  (define order
    (map car
         (sort (map (λ (i) (cons i (abs (list-ref coeffs i)))) idxs)
               (λ (p q) (< (cdr p) (cdr q)))))) ; ascending by |coeff|
  (define lo (inexact->exact (floor (* a n))))
  (define hi (inexact->exact (floor (* b n))))
  (define keep (list->set (take (drop order lo) (- hi lo))))
  (for/list ([c (in-list coeffs)] [i (in-naturals)])
    (if (set-member? keep i) c 0.0)))

(time
 (test-case
  "haar tests"
  ; recovery tests
  (define (round-10 ls) (gt:round-n ls 10)) 
  (define fn1 (compose round-10 haar-inv haar))
  ;;(define xs '(100 101 99 102 100 101 99 103))
  (define xs (for/list ([i 1000]) (+ 100 (random 21))))
  (define xss (map exact->inexact xs))
  (check-equal? (fn1 xs) xss)
  (define ys (haar xs))
  (check-equal? (round-10 (haar-inv (haar-slice ys 0 1))) (map exact->inexact xs))
  ; check that hp + lp == original series
  (for/list ([e '(0.25 0.5 0.75)])
    (define zs (haar-inv (haar-slice ys 0 e)))
    (define ws (haar-inv (haar-slice ys e 1)))
    (define es (round-10 (map + ws zs)))
    (define xss (map exact->inexact xs))
    (check-equal? es xss))
  (displayln (string-append module-name ": passed tests"))))
