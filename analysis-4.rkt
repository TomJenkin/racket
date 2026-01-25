#lang racket

(require plot
         racket/date
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in qt: "quant-tools.rkt")
         (prefix-in sd: "sample-data.rkt"))


(when #f ; k-means ==========================================================

  (define (random-matrix rows cols lo hi)
    (for/list ([i rows])
      (for/list ([j cols])
        (+ lo (* (random) (- hi lo))))))

  (define pts (random-matrix 2000 2 -1.0 1.0))
  (define-values (means assigns) (qt:kmeans pts 7 2000))

  ;; k-means plot

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
          #:title "K-Means Clustering")))


; equity patterns ==========================================================

(define (cum-rel-change ls)
  (define p0 (first ls))
  (map (λ (p) (/ (- p p0) p0)) ls))

(define dt1 sd:data-sp500)
(dt:table-print dt1 5 #:head #t)
(define r1 (gt:rolling (λ (e) e) 10 (dt:table-read dt1 "close")))
(define dt2 (dt:table-dropna (dt:table-create dt1 "tail" r1)))
(define r2 (dt:table-read dt2 "tail"))
(define r3 (map cum-rel-change r2))
(define dt3 (dt:table-create dt2 "tail-rel" r3))
(define r4 (dt:table-read dt3 "tail-rel"))
(define-values (means-2 assigns-2) (qt:kmeans r4 7 2000))
(define dt4 (dt:table-create dt3 "label" assigns-2))
(define dates-1 (dt:table-read dt4 "date"))
(define values-1 (dt:table-read dt4 "label"))
;;(dt:table-print dt4 5 #:head #t)

;; plot cluster groups/labels
(define (plot-dates-values dates values)
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-width 500]
                 [plot-height 250])
    (plot
     (lines
      (map vector
           (map date->seconds dates)
           values))
     #:x-label "Date"
     #:y-label "Value")))

(plot-dates-values dates-1 values-1)

;; plot cluster means
(define (plot-many-series series-list)
  (define renderers
    (for/list ([ys series-list] [i (in-naturals 1)])
      (define pts
        (for/list ([y ys] [x (in-naturals 0)])
          (vector x y)))
      (lines pts #:label (format "series ~a" i))))

  (parameterize ([plot-x-label "Index"]
                 [plot-y-label "Value"]
                 [plot-width 500]
                 [plot-height 250]
                 ;;[plot-x-grid? #t]
                 ;;[plot-y-grid? #t]
                 )
    (plot renderers)))

(plot-many-series means-2)


(displayln "done!")
 
   

















