#lang racket

(require plot
         rackunit
         syntax/location
         "data-table-5.rkt"
         "gen-utils.rkt"
         "data-table-5.rkt")

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

(define df (dataframe-from-csv (string-append file-path file-name)))

;;(dataframe-head df 5)

;;(dataframe-shape df)

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

(define ls1 (dataframe-ref df "close"))
(define ls2 (rolling mean 3 ls1))
(define df1 (dataframe-set df "ma" ls2))
;;(dataframe-head df1 5)

(define df2 (dataframe-dropna df1))
;;(dataframe-head df2 5)
;;(dataframe-shape df1)
;;(dataframe-shape df2)

(displayln "==================================================")

(define (dataframe-print df n)
  (define rows (cons
                (hash-keys (dataframe-hash df))
                (transpose (hash-values (dataframe-hash (dataframe-head df n))))))


  (for ([row rows] [i (in-naturals)])
    (if (= i 0)
        ;; Header row - maybe print differently
        (begin
          (display "H: ")
          (displayln row))
        ;; Data rows
        (printf "~a: ~a\n" i row)))
  
  (displayln "done")
  )

(dataframe-print df2 5)

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
