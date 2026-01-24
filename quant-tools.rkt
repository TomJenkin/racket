#lang racket

(provide
 (contract-out
  ;;[kmeans-1 kmeans/c-1]
  ;;[kmeans-2 kmeans/c-1]
  ;;[kmeans-3 kmeans/c-1]
  [kmeans-4 kmeans/c-3]))

(define kmeans/c-1
  (-> (non-empty-listof (listof real?)) ; points
      exact-nonnegative-integer?        ; k
      exact-nonnegative-integer?        ; iters
      (listof (listof real?))))         ; centroids

(define kmeans/c-2
  (-> (and/c (listof (listof real?)) (not/c null?))  ; points
      exact-nonnegative-integer?                     ; k
      exact-nonnegative-integer?                     ; iters
      (listof (listof real?))))                      ; centroids

(define kmeans/c-3
  (-> (non-empty-listof (listof real?))        ; points
      exact-nonnegative-integer?               ; k
      exact-nonnegative-integer?               ; iters
      (values
       (listof (listof real?))                 ; centroids
       (listof exact-nonnegative-integer?))))  ; assignments

;; fixed iterations
(define (kmeans-1 points k iters)
  (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
  (define (closest p cs) (argmin (λ (c) (dist2 p c)) cs))
  (define (mean pts)
    (define n (length pts))
    (map (λ (col) (/ (apply + col) n)) (apply map list pts)))
  (define centroids (take points k))
  (for/fold ([cs centroids]) ([i (in-range iters)])
    (define clusters
      (for/list ([c cs])
        (filter (λ (p) (equal? c (closest p cs))) points)))
    (map mean clusters)))

;; fixed iterations with contract
(define/contract (kmeans-2 points k iters)
  kmeans/c-1
  (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
  (define (closest p cs) (argmin (λ (c) (dist2 p c)) cs))
  (define (mean pts)
    (define n (length pts))
    (map (λ (col) (/ (apply + col) n)) (apply map list pts)))
  (define centroids (take points k))
  (for/fold ([cs centroids]) ([i (in-range iters)])
    (define clusters
      (for/list ([c cs])
        (filter (λ (p) (equal? c (closest p cs))) points)))
    (map mean clusters)))

;; stops when converged
(define (kmeans-3 points k iters)
  (define (dist2 p q)
    (for/sum ([x p] [y q]) (sqr (- x y))))
  (define (closest p cs)
    (argmin (λ (c) (dist2 p c)) cs))
  (define (mean pts)
    (define n (length pts))
    (map (λ (col) (/ (apply + col) n))
         (apply map list pts)))
  (define centroids (take points k))
  (let loop ([cs centroids] [i iters])
    (define clusters
      (for/list ([c cs])
        (filter (λ (p) (equal? c (closest p cs))) points)))
    (define new-cs (map mean clusters))
    (if (or (zero? i) (equal? cs new-cs))
        new-cs
        (loop new-cs (sub1 i)))))

;; returns means plus cluster group assignment for points
(define (kmeans-4 points k iters)
  (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
  (define (argmin-index f xs)
    (for/fold ([bi 0] [bv +inf.0] [i 0] #:result bi) ([x xs])
      (define v (f x))
      (if (< v bv) (values i v (add1 i)) (values bi bv (add1 i)))))
  (define (closest-i p cs) (argmin-index (λ (c) (dist2 p c)) cs))
  (define (mean pts)
    (define n (length pts))
    (map (λ (col) (/ (apply + col) n)) (apply map list pts)))
  (define init (take points k))
  (let loop ([cs init] [i iters])
    (define as (map (λ (p) (closest-i p cs)) points))
    (define groups
      (for/list ([j (in-range k)])
        (for/list ([p points] [a as] #:when (= a j)) p)))
    (define new-cs
      (for/list ([j (in-range k)])
        (if (null? (list-ref groups j)) (list-ref cs j) (mean (list-ref groups j)))))
    (if (or (zero? i) (equal? cs new-cs))
        (begin
          (printf "k-means converged (iterations = ~a)\n" (- iters i))
          (values new-cs as))
        (loop new-cs (sub1 i)))))

#| =================== tests =================== |#

(module+ test

  (require plot
           rackunit
           racket/format
           (prefix-in gt: "gen-tools.rkt")
           (prefix-in dt: "data-table.rkt")
           (prefix-in sd: "sample-data.rkt"))

  (define t0 (current-inexact-milliseconds))
  
  ;; ...
  (define pts '((1 1) (1.1 0.9) (0.9 1.2) (5 5) (5.2 4.8) (4.9 5.1)))
  (define aa (kmeans-1 pts 2 1000))
  (define (process-2d fn ls) (map (lambda (row) (map fn row)) ls))
  (define fn (lambda (e) (~r e #:precision 2)))
  ;;(process-2d fn aa)

  ;;(displayln "spx for methods 1-3...")
  (define dt1 sd:data-sp500)
  (define dt2 (dt:table-create dt1 "rolling-tail" (gt:rolling (lambda (e) e) 2 (dt:table-read dt1 "close"))))
  (define dt3 (dt:table-dropna dt2 #:na null))
  ;;(dt:table-print dt3 10 #:head #t)
  (define ls1 (dt:table-read dt3 "rolling-tail"))
  ;;(define ls2 (take ls1 7))
  ;;(define res-1 (kmeans-1 ls1 7 200))
  ;;(define res-1 (kmeans-2 ls1 7 200))
  (define res-1 (kmeans-3 ls1 7 200))
  ;;(gt:list-of-lists-of-numbers-to-strings res-1 #:decimal-places 1)
  ;;ls2

  ;;(displayln "simple example...")
  (define pts-2 '((1 1) (1.2 0.9) (8 8) (8.2 7.9) (0.8 1.1) (7.9 8.1)))
  ;;(define-values (means assigns) (kmeans-4 pts-2 2 20))
  (displayln "simple...")
  (define-values (means assigns) (kmeans-4 pts-2 6 2000))
  ;;(displayln means)    ; -> centroids (means)
  ;;(displayln assigns)  ; -> list of cluster indices per point (0 or 1)

  ;; ======================================================================================

  
  (displayln "rolling...")
  (define-values (means-1 assigns-1) (kmeans-4 ls1 2 2000))
  ;;(displayln means-1)    ; -> centroids (means)
  ;;(displayln assigns-1)  ; -> list of cluster indices per point

  (define (random-matrix rows cols lo hi)
    (for/list ([i rows])
      (for/list ([j cols])
        (+ lo (* (random) (- hi lo))))))

  (displayln "random...")
  (define ls2 (random-matrix 2000 2 -1.0 1.0))
  (define-values (means-2 assigns-2) (kmeans-4 ls2 6 2000))
  
  (displayln "scatter plot:")

  ;; from rolling
  ;;(define data-1 (map append ls1 (map list assigns-1)))
  ;;(define data-2 (map (λ (m k) (append m (list k))) means-1 (sort (remove-duplicates assigns-1) <)))

  ;; from random
  (define data-1 (map append ls2 (map list assigns-2)))
  (define data-2 (map (λ (m k) (append m (list k))) means-2 (sort (remove-duplicates assigns-2) <)))

  (define (group-by-cluster data)
    (for/fold ([h (hash)]) ([p data])
      (define k (third p))
      (hash-update h k (λ (acc) (cons p acc)) '())))

  (define (renders-for data sym size alpha)
    (define groups (group-by-cluster data))
    (for/list ([(k ps) (in-hash groups)])
      (points
       (for/list ([p (reverse ps)])
         (vector (first p) (second p)))
       #:sym sym
       #:size size
       #:alpha alpha
       #:color k)))

  ;; --- build renderers for both datasets ---
  (define renders-1 (renders-for data-1 'fullcircle 5 0.5))
  (define renders-2 (renders-for data-2 'fullsquare 15 1))

  ;; --- plot (data-2 drawn on top because it comes last) ---
  (parameterize ([plot-x-label "x"]
                 [plot-y-label "y"]
                 [plot-width 500]
                 [plot-height 450]
                 [plot-aspect-ratio #f])
    (plot (append renders-1 renders-2)
          #:title "K-Means Clustering"))

  (define t1 (current-inexact-milliseconds))
  (displayln (string-append "runtime: " (~r (- t1 t0) #:precision 0 #:group-sep ",") " ms")))

