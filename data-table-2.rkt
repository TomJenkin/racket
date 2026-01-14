#lang racket

(require csv-reading
         racket/date
         racket/function
         racket/list
         plot)

#| ======================== utilities ======================== |#

;; pipeline macro named pipe
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

#| ======================== functions ======================== |#

;; structure of data frame
(struct dt-frame (headers data))

;; read csv
(define (read-csv file-name)
  (call-with-input-file file-name csv->list))

;; remove nulls
(define (dt-clean dt)
  (filter (lambda (ls) (not (member "" ls))) dt))

;; transpose list of lists
(define (dt-transpose dt)
  (apply map list dt))

;; make data frame
(define (dt-frame-make dt)
  (dt-frame (car dt) (cdr dt)))

(define (dt-head dt n)
  (display (dt-frame-headers dt))
  (displayln (take (dt-frame-data dt) n)))

(define (dt-create data)
  (unless (apply = (map length data))
    (error 'dt-create "lists lengths are unequal"))
  (make-immutable-hash
   (map (lambda (lst) (cons (car lst) (cdr lst)))
        data)))

#| ======================== executions ======================== |#

;(define dt1 (read-csv "C:/Users/tomje/Downloads/SP500.csv"))
;(define dt2 (dt-clean dt1))
;(define dt3 (dt-transpose dt2))

(define dt-generate (compose-pipe
                     read-csv
                     dt-clean
                     dt-frame-make
                     ))

(define dt (dt-generate "C:/Users/tomje/Downloads/SP500.csv"))

dt

(dt-head dt 3)

(define dt-generate2 (compose-pipe
                     read-csv
                     dt-clean
                     dt-transpose
                     dt-create
                     ))

(define dt22 (dt-generate2 "C:/Users/tomje/Downloads/SP500.csv"))

dt22

