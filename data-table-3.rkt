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

;; read csv
(define (read-csv file-name)
  (call-with-input-file file-name csv->list))

;; remove nulls
(define (dt-clean dt)
  (filter (lambda (ls) (not (member "" ls))) dt))

;; transpose list of lists
(define (dt-transpose dt)
  (apply map list dt))

;; creates table as hash from list of lists
(define (dt-create data)
  (unless (apply = (map length data))
    (error 'dt-create "lists lengths are unequal"))
  (make-immutable-hash
   (map (lambda (lst) (cons (car lst) (cdr lst)))
        data)))

;; head of data frame
(define (dt-head dt n)
  (for/hash ([(k v) (in-hash dt)])
    (values k (take v n))))

;; rename series names
(define (dt-rename dt ns)
  (for/hash ([(k v) (in-hash dt)])
    (values (hash-ref ns k k) v)))

;; change type of series
(define (dt-retype dt fn ns)
  (for/hash ([(k v) (in-hash dt)])
    (if (member k ns) (values k (map fn v)) (values k v))))

;; pipleline functions
(define dt-generate (compose-pipe
                     read-csv
                     dt-clean
                     dt-transpose
                     dt-create
                     (lambda (dt) (dt-rename dt (hash "SP500" "close" "observation_date" "date")))
                     (lambda (dt) (dt-retype dt string->number (list "close")))
                     ))

#| ======================== executions ======================== |#

(define dt (dt-generate "C:/Users/tomje/Downloads/SP500.csv"))

(dt-head dt 3)

;;(dt-rename dt (hash "SP500" "tic"))


