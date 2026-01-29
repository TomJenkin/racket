#lang racket

(require rackunit
         racket/list
         (prefix-in pc: "python-caller.rkt")
         (prefix-in gt: "gen-tools.rkt"))

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
(define (haarI xs)
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
  
(when #f
  (test-case
   "best test cases - KEEP! (caution does not work when cmd line is too long)"
   (displayln "python haar...")
   ;;(define xs '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
   (define xs (map exact->inexact (random-walk 256 100)))
   (define ps (hash 'fn "haar_arr" 'args (list xs)))
   (define hs (pc:call-python-fn ps #:display-cmd #f))
   (define hsc (pc:list-of-lists->list hs))
   (displayln "python haarI...")
   (define pst (hash 'fn "haarI_arr" 'args (list hsc)))
   (define xst (pc:call-python-fn pst #:display-cmd #f))
   (define xstc (pc:list-of-lists->list xst))
   (displayln "racket haar...")
   (define hs0 (haar xs))
   (displayln "racket haarI...")
   (define xst0 (haarI hs0))
   (check-equal? (gt:list-equal-approx? xs xstc) #t)
   (check-equal? (gt:list-equal-approx? hsc hs0) #t)
   (check-equal? (gt:list-equal-approx? xs xst0) #t)
   (displayln "passed all haar tests!")
   (displayln "ToDo: want to additionally check that racket haar/haarI do HP/LP well!")
   null))

(when #f
  (test-case
   "haar transform"
   (define xs '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
   (define data-1 (hash 'fn "haar_transform" 'args (list xs 0.1 1.0)))
   (define batch (list data-1))
   (define results (pc:call-python-fn batch #:display-cmd #f))
   (define results-clean (pc:list-of-lists->list (first results)))
   (define hs results-clean)
   (displayln hs)))
