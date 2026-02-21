#lang racket

(require 
  ;;rackunit
  ;math/statistics
  ;racket/date
  (prefix-in dt: "data-table.rkt")
  (prefix-in gt: "gen-tools.rkt")
  (prefix-in cl: "cluster-kmeans.rkt")
  (prefix-in sd: "sample-data.rkt")
  (prefix-in wt: "wavelets-haar.rkt")
  )

(define (normalize xs)
  (define mn (apply min xs))
  (define mx (apply max xs))
  (define range (- mx mn))
  (if (zero? range)
      (map (λ (_) 0.0) xs)   ; all values identical → all zeros
      (map (λ (x) (/ (- x mn) range)) xs)))



#| 
   (define data
     (let* ([data-1 sd:data-sp500]
            [rolling-win-length 32]
            [tail (gt:rolling (λ (e) e) rolling-win-length (dt:table-read data-1 "close"))]
            [data-2 (dt:table-dropna (dt:table-create data-1 "tail" tail))]
            [fn (compose wt:haar normalize)]
            [data-3 (dt:table-create data-2 "tail-rel" (map fn (dt:table-read data-2 "tail")))]
            [n-clusters 6]
            ;[cluster-hash (call-with-values
            ;               (λ () (cl:kmeans (dt:table-read data-3 "tail-rel") n-clusters 5000)) 
            ;               (λ (x y) (hash 'means x 'assigns y)))]
   
            [cluster-hash (cl:kmeans (dt:table-read data-3 "tail-rel") n-clusters 5000)]
            
            [data-4 (dt:table-create data-3 "label" (hash-ref cluster-hash 'assignments))]
            [data (dt:table-tail data-4 5)]
            )
       (dt:table-print data-1 5 #:head #f)
       data))
   
   (define dates (dt:table-read data "date"))
   (define values (dt:table-read data "label"))
   
 |#

(define data
  (let* ([data-1 sd:data-sp500]
         [rolling-win-length 32]
         [tail (gt:rolling (λ (e) e) rolling-win-length (dt:table-read data-1 "close"))]
         [data-2 (dt:table-dropna (dt:table-create data-1 "tail" tail))]
         [fn (compose wt:haar normalize)]
         [data-3 (dt:table-create data-2 "tail-rel" (map fn (dt:table-read data-2 "tail")))]
         [n-clusters 6]
         [cluster-hash (cl:kmeans (dt:table-read data-3 "tail-rel") n-clusters 5000)]
         [data-4 (dt:table-create data-3 "label" (hash-ref cluster-hash 'assignments))]
         [data (dt:table-tail data-4 5)]
         )
    (dt:table-print data-1 5 #:head #f)
    data))

(define dates (dt:table-read data "date"))
(define values (dt:table-read data "label"))

