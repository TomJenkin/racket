#lang racket

;;(require math)

;; K-Means clustering

;; points : (listof (list x y))
;; k      : number of clusters
;; iters  : number of iterations
(define (kmeans-simple points k iters)
  
  (define (dist2 p q)
    (+ (sqr (- (first p) (first q)))
       (sqr (- (second p) (second q)))))

  (define (closest p centroids)
    (argmin (lambda (c) (dist2 p c)) centroids))

  (define (mean pts)
    (define n (length pts))
    (list (/ (apply + (map first pts)) n)
          (/ (apply + (map second pts)) n)))

  (define centroids (take points k))

  (for/fold ([centroids centroids])
            ([i (in-range iters)])
    (define clusters
      (for/list ([c centroids])
        (filter (λ (p) (equal? c (closest p centroids))) points)))
    (map mean clusters)))




#| =================== tests =================== |#

(module+ test

  (require rackunit
           racket/format)

  (define pts '((1 1) (1.1 0.9) (0.9 1.2) (5 5) (5.2 4.8) (4.9 5.1)))

  (define aa (kmeans-simple pts 2 1000))

  ;;aa

  (define (process-2d fn ls)
    (map (lambda (row) (map fn row)) ls))

  (define fn (lambda (e) (~r e #:precision 2)))
  
  (process-2d fn aa)

 

  )
