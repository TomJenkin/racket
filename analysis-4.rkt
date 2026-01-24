#lang racket

(require plot
         "quant-tools.rkt")

;; k-means
(define (random-matrix rows cols lo hi)
  (for/list ([i rows])
    (for/list ([j cols])
      (+ lo (* (random) (- hi lo))))))

(define pts (random-matrix 2000 2 -1.0 1.0))
(define-values (means assigns) (kmeans pts 5 2000))

 
;; plot
(define data-1 (map append pts (map list assigns)))
(define data-2 (map (λ (m k) (append m (list k))) means (sort (remove-duplicates assigns) <)))

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

(define renders-1 (renders-for data-1 'fullcircle 5 0.5))
(define renders-2 (renders-for data-2 'fullsquare 15 1))

(parameterize ([plot-x-label "x"]
               [plot-y-label "y"]
               [plot-width 500]
               [plot-height 450]
               [plot-aspect-ratio #f])
  (plot (append renders-1 renders-2)
        #:title "K-Means Clustering"))
