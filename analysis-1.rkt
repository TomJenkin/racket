#lang racket

(require plot
         rackunit
         syntax/location
         "data-table-5.rkt"
         "gen-utils.rkt")

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")
(define df0 (dataframe-from-csv (string-append file-path file-name)))
(define closes (dataframe-ref df0 "close"))
(define df1 (dataframe-set df0 "mean-close" (rolling mean 5 closes)))
(define df2 (dataframe-set df1 "std-close" (rolling std-dev 5 closes)))
(define df3 (dataframe-set df2 "var-close" (rolling var 5 closes)))
(dataframe-print df3 10 #f) ; head #t
(define df4 (dataframe-dropna df3))

;; !!! sort out date in tail...... 90/10/2026

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 300])
  (plot
   (lines
    (map vector
         ;;(map date->seconds (hash-ref df "date"))
         (map datetime->real (dataframe-ref df0 "date"))
         (dataframe-ref df0 "close")))
   #:x-label "Date"
   #:y-label "Value"
   #:aspect-ratio #f
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " file-name)))


#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
