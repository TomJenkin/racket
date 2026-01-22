#lang racket

(require math/statistics
         (prefix-in dt: "data-table.rkt")
         (prefix-in gt: "gen-tools.rkt"))

(provide data-sp500)

#| =================== private =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

#| =================== public =================== |#

;; method 1
(define data-sp500
  (gt:pipe (dt:table-from-csv (string-append file-path file-name))
           (lambda (t) (dt:table-rename t  (hash "observation_date" "date" "SP500" "close")))
           (lambda (t) (dt:table-dropna t #:na ""))
           ;;(lambda (t) (dt:table-replace t "" '()))
           ;;(lambda (t) (dt:table-dropna t))
           (lambda (t) (dt:table-update t "close" string->number))
           (lambda (t) (dt:table-update t "date" gt:string->date-iso))
           (lambda (t) (dt:table-create t "mean-close" (gt:rolling mean 50 (dt:table-read t "close"))))
           (lambda (t) (dt:table-create t "std-close" (gt:rolling stddev 50 (dt:table-read t "close"))))
           (lambda (t) (dt:table-create t "var-close" (gt:rolling variance 50 (dt:table-read t "close"))))
           (lambda (t) (dt:table-dropna t))
           ))

;; method 2
(define fn-data
  (gt:compose-pipe
   dt:table-from-csv
   (lambda (t) (dt:table-rename t  (hash "observation_date" "date" "SP500" "close")))
   (lambda (t) (dt:table-replace t "" '()))
   dt:table-dropna
   (lambda (t) (dt:table-update t "close" string->number))
   (lambda (t) (dt:table-update t "date" gt:string->date-iso))
   (lambda (t) (dt:table-create t "mean-close" (gt:rolling mean 50 (dt:table-read t "close"))))
   (lambda (t) (dt:table-create t "std-close" (gt:rolling stddev 50 (dt:table-read t "close"))))
   (lambda (t) (dt:table-create t "var-close" (gt:rolling variance 50 (dt:table-read t "close"))))
   (lambda (t) (dt:table-dropna t))))

(define data-sp500-2  (fn-data "C:/Users/tomje/Downloads/SP500.csv"))

;; method 3
(define dt1 (dt:table-from-csv "C:/Users/tomje/Downloads/SP500.csv"))
(define dt2 (dt:table-rename dt1  (hash "observation_date" "date" "SP500" "close")))
(define dt2b (dt:table-replace dt2 "" '()))
(define dt3 (dt:table-dropna dt2b))
(define dt4 (dt:table-update dt3 "close" string->number))
(define dt5 (dt:table-update dt4 "date" gt:string->date-iso))
(define dt6 (dt:table-create dt5 "mean-close" (gt:rolling mean 50 (dt:table-read dt5 "close"))))
(define dt7 (dt:table-create dt6 "std-close" (gt:rolling stddev 50 (dt:table-read dt6 "close"))))
(define dt8 (dt:table-create dt7 "var-close" (gt:rolling variance 50 (dt:table-read dt7 "close"))))
(define dt9 (dt:table-dropna dt8))
;;(dt:table-head dt9 10)

(define data-sp500-3 dt9)

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location)

  (displayln "method 1:")
  (dt:table-print data-sp500 5 #:head #t)
  (displayln "method 2:")
  (dt:table-print data-sp500-2 5 #:head #t)
  (displayln "method 3:")
  (dt:table-print data-sp500-3 5 #:head #t)

  )
