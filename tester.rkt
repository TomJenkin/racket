#lang racket

(require
  plot
  math
  math/statistics
  racket/date
  (prefix-in dt: "data-table.rkt")
  (prefix-in gt: "gen-tools.rkt")
  (prefix-in cl: "cluster-kmeans.rkt")
  (prefix-in sd: "sample-data.rkt")
  (prefix-in wt: "wavelets-haar.rkt")
  (prefix-in qw: "quant-wrapper.rkt")
  )

(define (normalize xs)
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs)   ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))

(define (pct-changes ls)
  (cons '() (for/list ([a ls] [b (cdr ls)])
              (/ (- b a) a 1.0))))

(define (list-shift ls n)
  (cond
    [(positive? n) (append (make-list n '()) (drop-right ls n))]
    [(negative? n) (let ([m (- n)]) (append (drop ls m) (make-list m '())))]
    [else ls]))

(define (process-data rolling-win-length n-clusters iters)
  (define data-1 sd:data-sp500)
  (define tail (gt:rolling (λ (e) e) rolling-win-length (dt:table-read data-1 "close")))
  (define data-2 (dt:table-dropna (dt:table-create data-1 "tail" tail)))
  (define fn (compose wt:haar normalize))
  (define data-3 (dt:table-create data-2 "tail-rel" (map fn (dt:table-read data-2 "tail"))))
  (define cluster-hash
    (if #t
        (gt:timeit "kmeans" (cl:kmeans (dt:table-read data-3 "tail-rel") n-clusters iters #:verbose #t))
        (gt:timeit "kmeans/sklearn" (qw:kmeans (dt:table-read data-3 "tail-rel") n-clusters))))
  (define assignments (hash-ref cluster-hash 'assignments))
  (define centroids   (hash-ref cluster-hash 'centroids))
  (define centroidsI (map wt:haarI centroids))  
  (define data-4 (dt:table-create data-3 "label" assignments))
  (define data-5 (dt:table-create data-4 "change" (pct-changes (dt:table-read data-4 "close"))))
  (define data-6 (dt:table-create data-5 "change-next" (list-shift (dt:table-read data-5 "change") -1)))
  (define data-7 (dt:table-dropna data-6))
  (hash 'data data-7 'data-1 data-1 'centroids centroids 'centroidsI centroidsI))

(define bundle (gt:timeit "full" (process-data 32 10 5000)))
(define dates1 (dt:table-read (hash-ref bundle 'data) "date"))
(define values1 (dt:table-read (hash-ref bundle 'data) "label"))
(define means1 (hash-ref bundle 'centroidsI))

(define cats (dt:table-read (hash-ref bundle 'data) "label"))
(define metrics (dt:table-read (hash-ref bundle 'data) "change-next"))

(define (group-stats-multi cats metrics)
  (define groups (make-hash))
  ;; Group metrics
  (for ([cat cats] [val metrics])
    (hash-update! groups cat (λ (lst) (cons val lst)) '()))
  ;; Compute multiple stats
  (for/hash ([(cat vals) (in-hash groups)])
    (values cat 
            (hash 'count (length vals)
                  'mean (mean vals)
                  'stdev (stddev vals)
                  'skew (skewness vals)
                  'kurt (kurtosis vals)
                  'sum (apply + vals)
                  'min (apply min vals)
                  'max (apply max vals)))))

;; print out group
(group-stats-multi cats metrics)

;(dt:table-headers (hash-ref bundle 'data))
(dt:table-print (dt:table-project (hash-ref bundle 'data) '("date" "close" "label" "change" "change-next")) 5 #:head #f)

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




