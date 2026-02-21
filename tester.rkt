#lang racket

(require
  plot
  racket/date
  (prefix-in dt: "data-table.rkt")
  (prefix-in gt: "gen-tools.rkt")
  (prefix-in cl: "cluster-kmeans.rkt")
  (prefix-in sd: "sample-data.rkt")
  (prefix-in wt: "wavelets-haar.rkt")
  (prefix-in qw: "quant-wrapper.rkt")
  )

;; not working returns zero time
;(define (time/label label thunk)
;  (displayln (format "~a:" label))
;  (time thunk))

(define (normalize xs)
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs)   ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))

(define (process-data rolling-win-length n-clusters)
  (define data-1 sd:data-sp500)
  (define tail (gt:rolling (λ (e) e) rolling-win-length (dt:table-read data-1 "close")))
  (define data-2 (dt:table-dropna (dt:table-create data-1 "tail" tail)))
  (define fn (compose wt:haar normalize))
  (define data-3 (dt:table-create data-2 "tail-rel" (map fn (dt:table-read data-2 "tail"))))
  ;(displayln "kmeans:")
  (define cluster-hash (time (cl:kmeans (dt:table-read data-3 "tail-rel") n-clusters 5000)))
  ;(define cluster-hash (qw:kmeans (dt:table-read data-3 "tail-rel") n-clusters))
  (define assignments (hash-ref cluster-hash 'assignments))
  (define centroids   (hash-ref cluster-hash 'centroids))
  (define data-4 (dt:table-create data-3 "label" assignments))
  (hash 'data data-4 'data-1 data-1 'centroids centroids))

;(displayln "full:")
(define bundle (time (process-data 32 12)))
(define dates1 (dt:table-read (hash-ref bundle 'data) "date"))
(define values1 (dt:table-read (hash-ref bundle 'data) "label"))
(define means1 (hash-ref bundle 'centroids))
 
(dt:table-print (hash-ref bundle 'data-1) 5 #:head #f)

;; PLOTS

;; plot cluster groups/labels
(define (plot-dates-values dates1 values1)
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-width 500]
                 [plot-height 250])
    (plot
     (lines
      (map vector
           (map date->seconds dates1)
           values1))
     #:x-label "Date"
     #:y-label "Value")))

(plot-dates-values dates1 values1)

;; plot cluster means
(define (plot-many-series series-list)
  (define renderers
    (for/list ([ys series-list] [i (in-naturals 1)])
      (define pts
        (for/list ([y ys] [x (in-naturals 0)])
          (vector x y)))
      (lines pts
             ;;#:label (format "series ~a" i)
             )))
   
  (parameterize ([plot-x-label "Index"]
                 [plot-y-label "Value"]
                 [plot-width 500]
                 [plot-height 250]
                 ;;[plot-x-grid? #t]
                 ;;[plot-y-grid? #t]
                 )
    (plot renderers)))
   
(plot-many-series means1)
   
