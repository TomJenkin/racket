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

#|
(define ls
  (list
   (list (table-read t1 "date") (table-read t1 "close"))
   (list (table-read t1 "date") (table-read t1 "mean-close"))
   (list (table-read t1 "date") (table-read t1 "std-close"))))

(define aa (map (lambda (p) '((lines (map vector (first p) (second p))))) ls))
(define bb (first aa))
(define cc (first bb))
(define dd (first cc))
|#

;; lines-1
(define (make-lines-list-1 t date-col lines-specs)
  (for/list ([spec lines-specs])
    (match-define (list col-name label) spec)
    (lines
     (map vector
          (map datetime->real (table-read t date-col))
          (table-read t col-name))
     #:label label)))

;; lines-2
(define (make-lines-list-2 t date-col lines-specs)
  (define date-values (map datetime->real (table-read t date-col)))
  (map (lambda (spec)
         (lines
          (map vector date-values (table-read t (first spec)))
          #:label (second spec)))
       lines-specs))

(define plot-lines (make-lines-list-2 t1 "date"
                                      '(("close" "Close")
                                        ("mean-close" "Mean")
                                        ;;("std-close" "STD")
                                        )))

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 350])
  (plot
   plot-lines
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
