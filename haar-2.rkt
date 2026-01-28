#lang racket
(require racket/list)

(define k (sqrt 0.5))

(define (pow2? n) (and (positive? n) (zero? (bitwise-and n (sub1 n)))))
(define (pp n)
  (unless (pow2? n) (error 'haar "length must be a power of 2, got ~a" n))
  (integer-length (sub1 n)))

;; haar : (listof number) -> (listof number)
(define (haar xs)
  (define P (pp (length xs)))
  (define (step s p)
    (define n (arithmetic-shift 1 p))
    (define m (quotient n 2))
    (define pre (take s n))
    (define suf (drop s n))
    (define pairs (for/list ([i (in-range m)])
                    (list (list-ref pre (* 2 i))
                          (list-ref pre (add1 (* 2 i))))))
    (append (map (λ (ab) (* k (+ (first ab) (second ab)))) pairs)
            (map (λ (ab) (* k (- (first ab) (second ab)))) pairs)
            suf))
  (for/fold ([s xs]) ([p (in-range P 0 -1)]) (step s p)))

;; haar-inv : (listof number) -> (listof number)
(define (haar-inv xs)
  (define P (pp (length xs)))
  (define (step s p)
    (define n (arithmetic-shift 1 p))
    (define m (quotient n 2))
    (define pre (take s n))
    (define suf (drop s n))
    (define as (take pre m))
    (define ds (drop pre m))
    (append (apply append
                   (map (λ (a d)
                          (list (* k (+ a d))
                                (* k (- a d))))
                        as ds))
            suf))
  (for/fold ([s xs]) ([p (in-range 1 (add1 P))]) (step s p)))

#| 
   (define (cumsum xs)
     (reverse
      (car (for/fold ([acc '()] [s 0.0]) ([x xs])
             (define s2 (+ s x))
             (values (cons s2 acc) s2)))))
   
   ;; haar-filter-1d : (listof number) real real -> (listof number)
   (define (haar-filter-1d xs low high)
     (unless (and (real? low) (real? high) (<= 0.0 low high 1.0))
       (error 'haar-filter-1d "need 0<=low<=high<=1, got low=~a high=~a" low high))
   
     ;; rows = (idx val abs)
     (define rows (for/list ([x xs] [i (in-naturals)])
                    (list i x (abs x))))
     (define tot (apply + (map third rows)))
   
     (if (zero? tot)
         xs
         (let* ([sorted (sort rows < #:key third)]              ; abs ascending
                [cms    (map (λ (u) (min 1.0 (/ u tot)))        ; clamp for rounding
                             (cumsum (map third sorted)))]
                [kept   (map (λ (r cm)
                               (list (first r)
                                     (if (and (> cm low) (<= cm high))
                                         (second r)
                                         0.0)))
                             sorted cms)]
                [back   (sort kept < #:key first)])
           (map second back))))
   
 |#

(random-seed 0)
(define (random-walk n [start 100])
  (reverse
   (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
     (define step (if (zero? (random 2)) -1 1))
     (cons (+ (car path) step) path))))
  
;;(define xss (for/list ([i 1000]) (+ 100 (random 21))))
(define xs (random-walk 1024 100))

(define hs (haar xs))
;;(define hs2 (haar-filter-1d hs 0.20 1.00))
;;(define xs2 (haar-inv hs2))
