#lang racket

(require plot
         rackunit
         math/statistics
         racket/date
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in qt: "quant-tools.rkt")
         (prefix-in sd: "sample-data.rkt"))

;; scaling
(define (normalize-01 xs)
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs)   ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))

;; haar wavelet
(define (haar1 xs)
  (define sqrt2 (sqrt 2))
  (define (step lst accA accD)
    (match lst
      [(list a b rest ...)
       (define avg (/ (+ a b) sqrt2))
       (define diff (/ (- a b) sqrt2))
       (step rest
             (append accA (list avg))
             (append accD (list diff)))]
      ['() (append accA accD)]
      [_ (error "List length must be even")]))
  (step xs '() '()))

(define (inv-haar1 coeffs)
  (define sqrt2 (sqrt 2))
  (define n (length coeffs))
  (unless (even? n)
    (error "Coefficient list length must be even"))
  (define half (/ n 2))
  (define As (take coeffs half))
  (define Ds (drop coeffs half))
  (define (step a-list d-list acc)
    (match (list a-list d-list)
      [(list (list a restA ...) (list d restD ...))
       (define x0 (/ (+ a d) sqrt2))
       (define x1 (/ (- a d) sqrt2))
       (step restA restD (append acc (list x0 x1)))]
      [(list '() '()) acc]
      [_ (error "Mismatched coefficient halves")]))
  (step As Ds '()))

(test-case "haar inverse check"
           (displayln "at some point check zero the minor coeffs and recover near xs")
           (define (round-10 ls) (gt:round-n ls 10)) 
           (define fn1 (compose round-10 inv-haar1 haar1))
           (define fn2 (λ (ls) (map exact->inexact ls)))
           (define xs '(100 101 99 102 100 101 99 103))
           (check-equal? (fn1 xs) (fn2 xs)))


(test-case "list shift check"
           (define xs (range 0 10))
           (define ys (gt:list-shift xs 2))
           (check-equal? ys '(() () 0 1 2 3 4 5 6 7)))


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
(define fn (compose haar1 normalize-01))

(define rolling-win-length 60)
(define n-clusters 6)
(define dt1 sd:data-sp500)
(dt:table-print dt1 5 #:head #t)
(define r1 (gt:rolling (λ (e) e) rolling-win-length (dt:table-read dt1 "close")))
(define dt2 (dt:table-dropna (dt:table-create dt1 "tail" r1)))
(define r2 (dt:table-read dt2 "tail"))
(define r3 (map fn r2))
(define dt3 (dt:table-create dt2 "tail-rel" r3))
(define r4 (dt:table-read dt3 "tail-rel"))
(define-values (means-2 assigns-2) (qt:kmeans r4 n-clusters 5000))
(define dt4 (dt:table-create dt3 "label" assigns-2))
(define dates-1 (dt:table-read dt4 "date"))
(define values-1 (dt:table-read dt4 "label"))
;;(dt:table-print dt4 5 #:head #t)

;; do a simple test of label state vs. next move up or down. can be simple table off counts.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

;; testing

;; note: the timeseries of labels should be moderately stationary. not drifting

(displayln "testing...")
(define aa (range 100 500 10))
(dist-stats aa)


(displayln "done!")
 
   

















