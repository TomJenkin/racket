#lang racket

(require rackunit
         csv-reading
         syntax/location
         "gen-utils.rkt")

(provide dataframe
         dataframe-ref
         dataframe-set
         dataframe-remove
         dataframe-update
         dataframe-has-key
         dataframe-shape
         dataframe-rename
         dataframe-retype
         dataframe-from-csv
         dataframe-head
         dataframe-tail)

#| =================== private =================== |#

(define (dataframe-guard hash type-name)
  
  ;; validate type is hash
  (unless (hash? hash)
    (error type-name "expected hash, got: ~e" hash))
  
  ;; validate hash is immutable
  (unless (immutable? hash)
    (error type-name 
           "requires immutable hash (use (hash ...) or (hasheq ...), not make-hash)"))
  
  ;; validate all values are lists (change to also include vectors!)
  (for ([(k v) (in-hash hash)])
    (unless (list? v)
      (error type-name "value for key ~e is not a list: ~e" k v)))
  
  ;; validate all lists have same length or hash is empty
  (unless (or (hash-empty? hash)
              (apply = (map length (hash-values hash))))
    (error type-name
           "all lists must have same length. Lengths: ~a"
           (hash-map hash (lambda (k v) (cons k (length v))))))

  hash)

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
  (dataframe (make-immutable-hash (map (lambda (lst) (cons (car lst) (cdr lst))) data))))

;; convert from string to date
(define (string->date-iso str)
  (apply (lambda (y m d) (date 0 0 0 d m y 0 0 #f 0))
         (map string->number (string-split str "-"))))

#| =================== public =================== |#

;; dataframe structure
(struct dataframe (hash)
  #:transparent
  #:guard dataframe-guard)

;; immutable ref
(define (dataframe-ref df key [default #f])
  (hash-ref (dataframe-hash df) key default))

;; immutable set
(define (dataframe-set df key value)
  (dataframe (hash-set (dataframe-hash df) key value)))

;; immutable remove
(define (dataframe-remove df key)
  (dataframe (hash-remove (dataframe-hash df) key)))

;; immutable update
(define (dataframe-update df key updater)
  (dataframe (hash-update (dataframe-hash df) key updater)))

;; immutable has key
(define (dataframe-has-key df key)
  (hash-has-key? (dataframe-hash df) key))

;; rename keys
(define (dataframe-rename df ns)
  (dataframe (for/hash ([(k v) (in-hash (dataframe-hash df))])
               (values (hash-ref ns k k) v))))

;; change type
(define (dataframe-retype dt fn ns)
  (dataframe 
   (for/hash ([(k v) (in-hash (dataframe-hash dt))])
     (if (member k ns) (values k (map fn v)) (values k v)))))

;; shape
(define (dataframe-shape df)
  (define col-length (hash-count (dataframe-hash df)))
  (define first-key (car (hash-keys (dataframe-hash df))))
  (define row-length (length (dataframe-ref df first-key)))
  (list row-length col-length))

;; head of data frame
(define (dataframe-head df n)
  (dataframe
  (for/hash ([(k v) (in-hash (dataframe-hash df))])
    (values k (take v n)))))

;; tail of data frame
(define (dataframe-tail df n)
  (dataframe
  (for/hash ([(k v) (in-hash (dataframe-hash df))])
    (values k (take-right v n)))))

;; read from csv (make more general!)
(define dataframe-from-csv (compose-pipe
                            read-csv
                            dt-clean
                            dt-transpose
                            dt-create
                            (lambda (df) (dataframe-rename df (hash "SP500" "close" "observation_date" "date")))
                            (lambda (df) (dataframe-retype df string->number (list "close")))
                            (lambda (df) (dataframe-retype df string->date-iso (list "date")))))

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))
  
  (define df1 (dataframe (hash 'a '(1 2 3) 'b '(4 5 6))))
  (check-equal? (dataframe-ref df1 'a) '(1 2 3))
  (check-equal? (dataframe-ref df1 'missing-key 'na) 'na)

  (define df2 (dataframe-set df1 'c '(5 6 7)))
  (check-equal? (dataframe-ref df2 'c) '(5 6 7))

  (define df3 (dataframe-remove df2 'c))
  (check-equal? df1 df3)

  (define (double-list lst) (map (lambda (x) (* 2 x)) lst))
  (define df4 (dataframe-update df1 'a double-list))
  (check-equal? (dataframe-ref df4 'a) '(2 4 6))

  (check-equal? (dataframe-has-key df1 'a) #t)

  (define df5 (dataframe-shape df1))
  (check-equal? df5 '(3 2))

  (define file-name "C:/Users/tomje/Downloads/SP500.csv")
  (define df6 (dataframe-from-csv file-name))
  (check-equal? (dataframe? df6) #t)

  (define df7 (dataframe-head df6 3))
  (check-equal? (dataframe-shape df7) '(3 2))

  (define df8 (dataframe-tail df6 3))
  (check-equal? (dataframe-shape df8) '(3 2))
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
