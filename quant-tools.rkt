#lang racket

;; K-Means-1 clustering
;; points : (listof (listof number))   ; all points same dimension
;; k      : number of clusters
;; iters  : number of iterations
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

;; kmeans-2 : (listof (listof real?)) nat? nat? -> (listof (listof real?))
;; Runs k-means for a fixed number of iterations on N-dimensional points.
;; Assumes: points non-empty, all points same dimension, and (<= k (length points)).
(define/contract (kmeans-2 points k iters)
  ;;(-> (and/c (listof (listof real?)) (not/c null?))
  ;;    exact-nonnegative-integer?
  ;;    exact-nonnegative-integer?
  ;;    (listof (listof real?)))
  (-> (non-empty-listof (listof real?))
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      (listof (listof real?)))
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

#| =================== tests =================== |#

(module+ test

  (require rackunit
           racket/format
           (prefix-in gt: "gen-tools.rkt")
           (prefix-in dt: "data-table.rkt")
           (prefix-in sd: "sample-data.rkt"))

  ;; ...
  (define pts '((1 1) (1.1 0.9) (0.9 1.2) (5 5) (5.2 4.8) (4.9 5.1)))
  (define aa (kmeans-1 pts 2 1000))
  (define (process-2d fn ls) (map (lambda (row) (map fn row)) ls))
  (define fn (lambda (e) (~r e #:precision 2)))
  ;;(process-2d fn aa)

  ;; cluster k-means
  (define dt1 sd:data-sp500)
  (define dt2 (dt:table-create dt1 "rolling-tail" (gt:rolling (lambda (e) e) 2 (dt:table-read dt1 "close"))))
  (define dt3 (dt:table-dropna dt2 #:na null))
  ;;(dt:table-print dt3 10 #:head #t)
  (define ls1 (dt:table-read dt3 "rolling-tail"))
  ;;(define ls2 (take ls1 7))
  ;;(define res-1 (kmeans-1 ls1 7 100))
  ;;(define res-1 (kmeans-2 ls1 7 100))
  (define res-1 (kmeans-3 ls1 7 100))
  (gt:list-of-lists-of-numbers-to-strings res-1 #:decimal-places 1)

  ;;ls2
 
  )
