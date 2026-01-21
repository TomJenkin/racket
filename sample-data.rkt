#lang racket

(require (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt"))

(provide data-sp500)

#| =================== private =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

#| =================== public =================== |#

(define data-sp500
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

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location)

  (gt:timeit
   (path->string (syntax-source-file-name #'here))

   (dt:table-print data-sp500 10 #:head #t)
   
   ))
