#lang racket

(require plot
         rackunit
         syntax/location
         "data-table-5.rkt"
         "gen-utils.rkt")

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")
(define df (dataframe-from-csv (string-append file-path file-name)))

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 300])
  (plot
   (lines
    (map vector
         ;;(map date->seconds (hash-ref df "date"))
         (map datetime->real (dataframe-ref df "date"))
         (dataframe-ref df "close")))
   #:x-label "Date"
   #:y-label "Value"
   #:aspect-ratio #f
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " file-name)))

;; extend moving average example
(define ls1 (dataframe-ref df "close"))
;;(define ls2 (rolling mean 5 ls1))
(define df1 (dataframe-set df "mean-close" (rolling mean 5 ls1)))
(define df2 (dataframe-set df1 "std-close" (rolling std-dev 5 ls1)))
(define df3 (dataframe-set df2 "var-close" (rolling var 5 ls1)))
(define df4 (dataframe-dropna df3))
(dataframe-shape df1)
(dataframe-shape df3)
(dataframe-print df3 10)

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
