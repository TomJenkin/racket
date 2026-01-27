#lang racket

(require plot
         rackunit
         syntax/location
         (prefix-in gt: "gen-tools.rkt"))

(provide haar
         haar-inv
         haar-filter)

(define module-name (path->string (syntax-source-file-name #'here)))

;; haar transform
(define (haar xs)
  (define n (length xs))
  (unless (even? n) (error 'haar1 "list length must be even"))
  (define k (/ 1.0 (sqrt 2.0)))
  (define As (for/list ([i (in-range 0 n 2)])
               (* k (+ (list-ref xs i) (list-ref xs (add1 i))))))
  (define Ds (for/list ([i (in-range 0 n 2)])
               (* k (- (list-ref xs i) (list-ref xs (add1 i))))))
  (append As Ds))

;; haar inverse transform
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

;; haar filter (e.g. high-pass, low-pass)
(define (haar-filter coeffs a b)
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



(module+ test

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
   (check-equal? (round-10 (haar-inv (haar-filter ys 0 1))) (map exact->inexact xs))
   ; check that hp + lp == original series
   (for/list ([e '(0.05 0.1 0.25 0.5 0.75 0.9 0.95)])
     (define zs (haar-inv (haar-filter ys 0 e)))
     (define ws (haar-inv (haar-filter ys e 1)))
     (define es (round-10 (map + ws zs)))
     (define xss (map exact->inexact xs))
     (check-equal? es xss)))
 
  ;; plots

  (define (random-walk n [start 100])
    (reverse
     (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
       (define step (if (zero? (random 2)) -1 1))
       (cons (+ (car path) step) path))))
  
  ;;(define xss (for/list ([i 1000]) (+ 100 (random 21))))
  (define xs (random-walk 1000 100))
  ;;(define xs (map exact->inexact xss))
  (define hs (haar xs))

  (define cut 0.98)
  
  (define fs1 (haar-inv (haar-filter hs 0 cut)))

  (define fs2 (haar-inv (haar-filter hs cut 1)))

  ;;(lines (map vector (map date->seconds dates) values)) ; timeseries

  (parameterize ([plot-x-label "Index"]
                 [plot-y-label "Value"]
                 [plot-width 500]
                 [plot-height 250])
    ;;(plot (lines (map vector (range (length hs)) hs)))

    (plot (list 
           ;;(lines (map vector (range (length xs)) xs))
           (lines (map vector (range (length xs)) fs1))
           ;;(lines (map vector (range (length xs)) fs2))
           ))


    

    )
  
  (displayln (string-append module-name ": passed all tests!"))

  )