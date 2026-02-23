#lang racket

(require json
         srfi/19
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in cl: "cluster-kmeans.rkt")
         (prefix-in sd: "sample-data.rkt")
         (prefix-in wt: "wavelets-haar.rkt")
         (prefix-in qw: "quant-wrapper.rkt"))

(define (normalize xs)
  ;; scale to normal range
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs) ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))

(define (pct-changes ls)
  ;; calculate % changes
  (cons '() (for/list ([a ls] [b (cdr ls)])
              (/ (- b a) a 1.0))))

(define (list-shift ls n)
  ;; shift lift left or right
  (cond
    [(positive? n) (append (make-list n '()) (drop-right ls n))]
    [(negative? n) (let ([m (- n)]) (append (drop ls m) (make-list m '())))]
    [else ls]))

(define (process-data rolling-win-length n-clusters iters)
  ;; read market date >> calculate rolling wavelets >> kmeans centroids
  (define data-0 sd:data-sp500)
  ;(displayln (dt:table-shape data-0))
  (define data-1 (dt:table-update data-0 "date" (λ (e) (date->string e "~Y-~m-~d"))))
  (define tail (gt:rolling (λ (e) e) rolling-win-length (dt:table-read data-1 "close")))
  (define data-2 (dt:table-dropna (dt:table-create data-1 "tail" tail)))
  (define fn (compose wt:haar normalize))
  (define data-3 (dt:table-create data-2 "tail_rel" (map fn (dt:table-read data-2 "tail"))))
  (define cluster-hash
    (if #t
        (gt:timeit "kmeans" (cl:kmeans (dt:table-read data-3 "tail_rel") n-clusters iters #:verbose #t))
        (gt:timeit "kmeans/sklearn" (qw:kmeans (dt:table-read data-3 "tail_rel") n-clusters))))
  (define assignments (hash-ref cluster-hash 'assignments))
  (define centroids   (hash-ref cluster-hash 'centroids))
  (define centroidsI (map wt:haarI centroids))  
  (define data-4 (dt:table-create data-3 "label" assignments))
  (define data-5 (dt:table-create data-4 "change" (pct-changes (dt:table-read data-4 "close"))))
  (define data-6 (dt:table-create data-5 "change_next" (list-shift (dt:table-read data-5 "change") -1)))
  (define data-7 (dt:table-dropna data-6))
  (hash 'data data-7 'centroids centroids 'centroidsI centroidsI))

(define bundle
  ;; run data
  (let ([rolling-win-length 32]
        [n-clusters 60]
        [iters 5000])
    (gt:timeit "process-data" (process-data rolling-win-length n-clusters iters))))

;; export ======================================================================

(define package
  ;; export package
  (hash 'data (dt:table->list (hash-ref bundle 'data))
        'centroids (hash-ref bundle 'centroids)
        'centroidsI (hash-ref bundle 'centroidsI)))

(when #t
  ;; write json file
  (let ([fname "G:/My Drive/Common/Documents/Code/Racket/Data/racket_package_01.json"])
    (call-with-output-file fname
      (lambda (out) (write-json package out))
      #:exists 'replace)))
