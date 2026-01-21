#lang racket

(define (square x) (* x x))

(square 42)

(square 52)

(match-define (list a b) '(33 44))





;; points : (listof (list x y))
;; k      : number of clusters
;; iters  : number of iterations
(define (kmeans-simple points k iters)
  (define (dist2 p q)
    (+ (sqr (- (first p) (first q)))
       (sqr (- (second p) (second q)))))

  (define (closest p centroids)
    (argmin (λ (c) (dist2 p c)) centroids))

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

(define pts
  '((1 1) (1.1 0.9) (0.9 1.2)
    (5 5) (5.2 4.8) (4.9 5.1)))

(kmeans-simple pts 2 5)

