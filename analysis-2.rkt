#lang racket

(require plot
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in pt: "plot-tools.rkt"))

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

(define t1
  (gt:pipe (dt:table-from-csv (string-append file-path file-name))
        (lambda (t) (dt:table-rename t  (hash "observation_date" "date" "SP500" "close")))
        (lambda (t) (dt:table-dropna t))
        (lambda (t) (dt:table-update t "close" string->number))
        (lambda (t) (dt:table-update t "date" gt:string->date-iso))
        (lambda (t) (dt:table-create t "mean-close" (gt:rolling gt:mean 50 (dt:table-read t "close"))))
        (lambda (t) (dt:table-create t "std-close" (gt:rolling gt:std-dev 50 (dt:table-read t "close"))))
        (lambda (t) (dt:table-create t "var-close" (gt:rolling gt:var 50 (dt:table-read t "close"))))
        (lambda (t) (dt:table-dropna t))
        ))

(dt:table-print t1 10 #:head #t)

(define plot-lines-3
  (pt:make-lines-list-3
   t1 "date"
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
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " file-name)))

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location)

  (gt:timeit
   (path->string (syntax-source-file-name #'here))
  
   ))
