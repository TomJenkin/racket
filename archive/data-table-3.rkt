#lang racket

(require plot
         csv-reading
         ;;racket/date
         ;;racket/function
         ;;racket/list
         )

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

;; convert from string to date
(define (string->date-iso str)
  (apply (λ (y m d) (date 0 0 0 d m y 0 0 #f 0))
         (map string->number (string-split str "-"))))

;; extend the data frame
(define (dt-append dt key value)
  (hash-set dt key value))

;; assert data frame is valid
(define (dt-assert-valid dt)
  (define lengths-hash (make-hash (hash-map dt (lambda (key value) (cons key (length value))))))
  (unless (apply = (hash-values lengths-hash))
    (error 'dt-series-same-length "Series lengths are not the same")))

;; pipleline functions
(define dt-generate (compose-pipe
                     read-csv
                     dt-clean
                     dt-transpose
                     dt-create
                     (lambda (dt) (dt-rename dt (hash "SP500" "close" "observation_date" "date")))
                     (lambda (dt) (dt-retype dt string->number (list "close")))
                     (lambda (dt) (dt-retype dt string->date-iso (list "date")))))

#| ======================== executions ======================== |#

(define tic "SP500")
(define file-path "C:/Users/tomje/Downloads/")
(define file-name (string-append file-path tic ".csv"))
(define dt (dt-generate file-name))
(dt-head dt 3)
(dt-assert-valid dt)

(define dt2 (dt-append dt "test" (list 44 55 88 44)))
(dt-head dt2 3)

;;(dt-assert-valid dt2)
;;(hash-values dt)
;;(hash-keys dt)
;;(dt-transpose (hash-values dt))

#| ======================== plots ======================== |#

(parameterize
    ([plot-x-ticks (date-ticks)]
     [plot-width 600]
     [plot-height 300])
  (plot
   (lines
    (map vector
         ;;(map date->seconds (hash-ref dt "date"))
         (map datetime->real (hash-ref dt "date"))
         (hash-ref dt "close")))
   #:x-label "Date"
   #:y-label "Value"
   ;;#:aspect-ratio #f
   #:out-file (string-append file-path "test.png")
   #:title (string-append "Market: " tic)))
