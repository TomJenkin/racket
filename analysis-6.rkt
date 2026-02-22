#lang racket

(require
  plot
  math
  json
  math/statistics
  racket/date
  racket/string
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
  (displayln (dt:table-shape data-1))
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

(define bundle (gt:timeit "full" (process-data 32 20 5000)))
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
  (for/list ([(cat vals) (in-hash groups)])
    (hash 'cluster cat
          'count (length vals)
          'mean (mean vals)
          'stdev (stddev vals)
          'skew (skewness vals)
          'kurt (kurtosis vals)
          'sum (apply + vals)
          'min (apply min vals)
          'max (apply max vals))))

;; print out group
(define hs (group-stats-multi cats metrics))


(define package
  (hash 'hs hs))


(define pathx "G:/My Drive/Common/Documents/Code/Racket")
(define fnamex (string-append pathx "/Data/racket_package_01.json"))

(call-with-output-file fnamex
  (lambda (out) (write-json package out))
  #:exists 'replace)



;(take hs 2)

(define (hashes->table hs)
  (define keys (hash-keys (first hs)))
  (cons keys
        (map (λ (h)
               (map (λ (k) (hash-ref h k #f)) keys))
             hs)))
   
(define hst (hashes->table hs))

(define (table->csv table path)
  (call-with-output-file path
    (λ (out)
      (for ([row table])
        (fprintf out "~a\n"
                 (string-join (map ~a row) ","))))
    #:exists 'replace))


(define path "G:/My Drive/Common/Documents/Code/Racket")
(define fname (string-append path "/Data/racket_test_01.csv"))

;(table->csv hst "C:/Users/tomje/Downloads/test.csv")

(table->csv hst fname)
  
(define dtr (dt:table (first hst) (rest hst)))

(dt:table-print dtr 3)


(define fname2 (string-append path "/Data/racket_test_01.json"))

(define h22
  (hasheq
   'name "Tom"
   'age 42
   'skills (list "Scheme" "Python" "AI")))

(call-with-output-file fname2
  (lambda (out) (write-json h22 out))
  #:exists 'replace)


(define h1 (hash 'age 33))
(define h2 (hasheq 'age 33))
(define h3 (hash "age" 33))
(define h4 (hasheq "age" 33))

(hash-ref h1 'age)
(hash-ref h2 'age)
(hash-ref h3 "age")
(hash-ref h4 "age")


(define fname3 (string-append path "/Data/racket_test_03.json"))

(define aa (dt:table->list (hash-ref bundle 'data)))

;(call-with-output-file fname3
;  (lambda (out) (write-json aa out))
;  #:exists 'replace)

;(dt:table-headers (hash-ref bundle 'data))
;(dt:table-print (dt:table-project (hash-ref bundle 'data) '("date" "close" "label" "change" "change-next")) 5 #:head #f)

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




