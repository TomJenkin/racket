#lang racket

(require plot
         ;;rackunit
         math/statistics
         racket/date
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in cl: "cluster-kmeans.rkt")
         (prefix-in sd: "sample-data.rkt")
         (prefix-in wt: "wavelets-haar.rkt"))

;; scaling
(define (normalize-01 xs)
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs)   ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))

; equity patterns ==========================================================

(define (cum-rel-change ls)
  (define p0 (first ls))
  (map (λ (p) (/ (- p p0) p0)) ls))

(define (normalise ls)
  (define p0 (first ls))
  (map (λ (p) (/ p p0)) ls))

(define (abs-diffs ls)
  (for/list ([a ls] [b (rest ls)])
    (abs (- b a))))

(define (scale-0-1 ls)
  (define mn (apply min ls))
  (define mx (apply max ls))
  (define range (- mx mn))
  (map (λ (x) (/ (- x mn) range)) ls))

(define (scale-zscore ls)
  (define μ (mean ls))
  (define σ (stddev ls))
  (if (zero? σ)
      (make-list (length ls) 0.0)
      (map (λ (x) (/ (- x μ) σ)) ls)))

(define (dist-stats lss)
  (define ls (scale-0-1 lss))
  ;;(define ls (scale-zscore lss))
  (list
   (apply min ls)
   (apply max ls)
   (mean ls)
   (stddev ls)
   (variance ls)
   (skewness ls)
   (kurtosis ls)
   ;;(median ls)
   ;;(mode ls)
   ;;(geometric-mean ls)

   ;; sort out and check proper way to calculate quantiles. this may be all you need??
   
   (quantile 0.05 < ls)
   (quantile 0.1 < ls)
   (quantile 0.25 < ls)

   (quantile 0.75 > ls)
   (quantile 0.9 > ls)
   (quantile 0.95 > ls)
   
   ;;(percentile ls 0.25)
   ))
  
;;(define fn cum-rel-change)
;;(define fn normalise)
;;(define fn abs-diffs)
;;(define fn dist-stats)
;;(define fn haar1)
(define fn (compose wt:haar normalize-01))

(define rolling-win-length 32)
(define n-clusters 6)
(define dt1 sd:data-sp500)
(dt:table-print dt1 5 #:head #t)
(define r1 (gt:rolling (λ (e) e) rolling-win-length (dt:table-read dt1 "close")))
(define dt2 (dt:table-dropna (dt:table-create dt1 "tail" r1)))
(define r2 (dt:table-read dt2 "tail"))
(define r3 (map fn r2))
(define dt3 (dt:table-create dt2 "tail-rel" r3))
(define r4 (dt:table-read dt3 "tail-rel"))
(define-values (means-2 assigns-2) (cl:kmeans r4 n-clusters 5000))
(define dt4 (dt:table-create dt3 "label" assigns-2))
(define dates-1 (dt:table-read dt4 "date"))
(define values-1 (dt:table-read dt4 "label"))
;;(dt:table-print dt4 5 #:head #t)

;; do a simple test of label state vs. next move up or down. can be simple table off counts.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


;; PLOTS

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

(plot-many-series means-2)

