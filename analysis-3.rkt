#lang racket

;; testing of 1d-convolutions

(require syntax/location
         (prefix-in dt: "data-table.rkt")
         (prefix-in sd: "sample-data.rkt")
         (prefix-in gt: "gen-tools.rkt"))


;; define data
(define dt1 sd:data-sp500)

;; sample print
(dt:table-print dt1 10 #:head #t)

;; simple kernel
(define (slide-dot signal kernel)
  (define n (length signal))
  (define k (length kernel))
  (when (< n k) (error 'slide-dot "signal shorter than kernel"))
  (for/list ([i (in-range 0 (+ 1 (- n k)))])
    (apply + (map * (take (drop signal i) k) kernel))))

(define signal1 (dt:table-read dt1 "close"))
(define kernel1 (list 0.5 0.2 1 0.6 0.4))

;;(slide-dot signal1 kernel1)

(slide-dot '(1 2 3 4 5) '(1 0 -1))
 
