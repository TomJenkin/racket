#lang racket

(require plot
         rackunit
         syntax/location
         "gen-utils.rkt"
         "data-table-5.rkt")

#| =================== plots =================== |#

(define file-path "C:/Users/tomje/Downloads/")
(define file-name "SP500.csv")

(define df (dataframe-from-csv (string-append file-path file-name)))

;;(dataframe-head df 5)

(dataframe-shape df)

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

#|

(define (rolling f n lst [step 1])
  (if (< (length lst) n)
      '()
      (for/list ([i (in-range 0 (- (length lst) n -1) step)])
        (f (take (drop lst i) n)))))

(define aa (rolling (lambda (ls) (apply + ls)) 5 (dataframe-ref df "close")))

(length aa)

(define win-length 5)

;;(define (fn ls) (apply + ls))
(define (fn ls) (apply + ls))

(for/list ([value (dataframe-ref df "close")] [idx (in-naturals)])
  (if (< idx (- win-length 1))
      (list idx "")
  (list idx value)))

|#

#|

;; rolling function
(define (rolling fn n ls)
  (for/list ([i (in-range (- (length ls) n -1))])
    (fn (take (drop ls i) n))))

;; Statistic functions to pass in
(define (mean win) (/ (apply + win) (length win)))
(define (sum win) (apply + win))
(define (maximum win) (apply max win))
(define (minimum win) (apply min win))



;; Usage
(define data '(1 2 3 4 5 6 7 8 9 10))

(rolling mean 3 data)     ;; => (2 3 4 5 6 7 8 9)
(rolling sum 3 data)      ;; => (6 9 12 15 18 21 24 27)
(rolling maximum 3 data)  ;; => (3 4 5 6 7 8 9 10)

;;(for/list ([value (dataframe-ref df "close")] [idx (in-naturals)])
;;  (list idx value))

(define win-length 5)

;;(for/list ([value (dataframe-ref df "close")] [idx (in-naturals)])
;;  (if (< idx (- win-length 1))
;;      (list idx "")
;;  (list idx value)))

|#

;; ............ make so has leading "" or whatever so can append safely

(define (slice ls n m)
  (take (list-tail ls n) (- m n)))

(define aa '(0 1 2 3 4 5 6 7 8 9))

;;  general rolling function (takes: mean, std, etc.)
(define (rolling fn n ls)
  (for/list ([i (in-range (length ls))])
    (if (< i (sub1 n))
        ""
        ;;(take (list-tail ls (- i (sub1 n))) n)
        (fn (take (list-tail ls (- i (sub1 n))) n))
        )))
    
;; Example: mean function
(define (mean window) (/ (apply + window) (length window)))

;; Usage
(rolling mean 3 '(1 2 3 4 5 6))
;; => (2 3 4 5)

(rolling (lambda (w) (apply + w)) 3 '(1 2 3 4 5 6))
;; => (6 9 12)

(rolling mean 3 aa)


#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
