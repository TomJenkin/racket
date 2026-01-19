#lang racket

(require plot
         rackunit
         syntax/location
         "data-table-7.rkt"
         "gen-utils.rkt")

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

(define t1
  (pipe (table-from-csv (string-append file-path file-name))
        (lambda (t) (table-rename t  (hash "observation_date" "date" "SP500" "close")))
        (lambda (t) (table-dropna t))
        (lambda (t) (table-update t "close" string->number))
        (lambda (t) (table-update t "date" string->date-iso))
        (lambda (t) (table-create t "mean-close" (rolling mean 50 (table-read t "close"))))
        (lambda (t) (table-create t "std-close" (rolling std-dev 50 (table-read t "close"))))
        (lambda (t) (table-create t "var-close" (rolling var 50 (table-read t "close"))))
        (lambda (t) (table-dropna t))
        ))

(table-print t1 10 #:head #t)

;; create a generalised plot function that can be extended to multiple lines!!!!

#|
(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 300])
  (plot
   (lines
    (map vector
         ;;(map date->seconds (hash-ref df "date"))
         (map datetime->real (table-read t1 "date"))
         (table-read t1 "close")))
   #:x-label "Date"
   #:y-label "Value"
   #:aspect-ratio #f
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " file-name)))
|#

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 300])
  (plot
   (list
    ;; first line: close
    (lines
     (map vector
          (map datetime->real (table-read t1 "date"))
          (table-read t1 "close"))
     #:label "Close")

    ;; second line: e.g. another column
    (lines
     (map vector
          (map datetime->real (table-read t1 "date"))
          (table-read t1 "mean-close"))
     #:label "Mean")

    (lines
     (map vector
          (map datetime->real (table-read t1 "date"))
          (table-read t1 "std-close"))
     #:label "STD")

    )
   #:x-label "Date"
   #:y-label "Value"
   #:aspect-ratio #f
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " file-name)))

#| =================== tests =================== |#

(module+ test

  (timeit
   (path->string (syntax-source-file-name #'here))
  
   ))
