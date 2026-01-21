#lang racket

(require plot
         ;;(prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in pt: "plot-tools.rkt")
         (prefix-in sd: "sample-data.rkt"))

(define plot-lines-3
  (pt:make-lines-list-3
   sd:data-sp500 "date"
   (list
    (hash 'col "close" '#:label "Close" '#:color "blue" '#:width 1)
    (hash 'col "mean-close"  '#:label "Mean"  '#:style 'dot)
    (hash 'col "std-close"  '#:label "STD"))))

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 350])
  (plot
   plot-lines-3
   #:x-label "Date"
   #:y-label "Value"
   #:aspect-ratio #f
   ;;#:out-file (string-append file-path "test.png")
   #:title "Market: SP500"))

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location)

  (gt:timeit
   (path->string (syntax-source-file-name #'here))
  
   ))
