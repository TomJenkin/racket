#lang racket

(require plot
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt"))

(provide make-lines-list-3)

#| =================== plots =================== |#

;; lines-1
(define (make-lines-list-1 t date-col lines-specs)
  (for/list ([spec lines-specs])
    (match-define (list col-name label) spec)
    (lines
     (map vector
          (map datetime->real (dt:table-read t date-col))
          (dt:table-read t col-name))
     #:label label)))

;; lines-2
(define (make-lines-list-2 t date-col lines-specs)
  (define date-values (map datetime->real (dt:table-read t date-col)))
  (map (lambda (spec)
         (lines
          (map vector date-values (dt:table-read t (first spec)))
          #:label (second spec)))
       lines-specs))

;; lines-3
(define (make-lines-list-3 t date-col lines-specs)
  (define date-values (map datetime->real (dt:table-read t date-col)))
  (for/list ([spec (in-list lines-specs)])
    (define col (hash-ref spec 'col))
    (define pts (map vector date-values (dt:table-read t col)))
    (define kws
      (sort (for/list ([k (in-list (hash-keys spec))]
                       #:when (keyword? k)) k) keyword<?))
    (define vals (map (lambda (k) (hash-ref spec k)) kws))
    (keyword-apply lines kws vals (list pts))))

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location
           (prefix-in sd: "sample-data.rkt"))

  ;;(gt:timeit
  ;; (path->string (syntax-source-file-name #'here))

  (dt:table-print sd:data-sp500 10 #:head #t)

  (define plot-lines-2 (make-lines-list-2 sd:data-sp500 "date"
                                          '(("close" "Close")
                                            ("mean-close" "Mean")
                                            ;;("std-close" "STD")
                                            )))

  (define plot-lines-3
    (make-lines-list-3
     sd:data-sp500 "date"
     (list
      (hash 'col "close" '#:label "Close" '#:color "blue" '#:width 1)
      (hash 'col "mean-close"  '#:label "Mean"  '#:style 'dot)
      (hash 'col "std-close"  '#:label "STD"))))

  (parameterize
      ([plot-x-ticks (date-ticks)]
       [plot-width 1400]
       [plot-height 800]
       [plot-new-window? #t])
    (plot
     ;;plot-lines-2
     plot-lines-3
     #:x-label "Date"
     #:y-label "Value"
     #:aspect-ratio #f
     ;;#:out-file (string-append file-path "test.png")
     #:title "Market: SP500"))

  ;;)
  )
