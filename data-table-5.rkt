#lang racket

(require rackunit
         csv-reading
         syntax/location
         "gen-utils.rkt")

(provide (struct-out dataframe)
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
         dataframe-tail
         dataframe-dropna
         dataframe-print

         ;;string->date-iso

         )

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
;;(define (dt-transpose dt)
;;  (apply map list dt))

;; creates table as hash from list of lists
(define (dt-create data)
  ;; e.g. data like: '(("date" "2025-01-02" "2025-01-03" "2025-01-04") ("close" 55 56 43))
  (dataframe (make-immutable-hash (map (lambda (ls) (cons (car ls) (cdr ls))) data))))

;; convert from string to date (assumes str is: yyyy-mm-dd)
(define (string->date-iso str)
  (apply (lambda (y m d) (date 0 0 0 d m y 0 0 #f 0))
         (map string->number (string-split str "-"))))

;; convert from string to date
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! fix this !!!!!!!!!!!!!!!!!!!!!!!!!
(define (string->date-dd/mm/yyyy str)
  (apply (lambda (d m y) (date 0 0 0 d m y 0 0 #f 0))
         (map string->number (string-split str "/"))))

;; print formatter
(define (print-formatter e)
  (cond
    [(date? e) (date->dd/mm/yyyy e)]
    [(number? e) (real->decimal-string e 2)]
    [(string? e) (if (string=? e "") "NA" e)]
    [else e]))

;; print table rows
(define (print-table rows)

  (define (pad-right s w)
    (define n (- w (string-length s)))
    (if (<= n 0)
        s
        (string-append s (make-string n #\space))))

  (define widths
    (apply map
           (lambda col (apply max (map string-length col)))
           rows))
  
  (for-each
   (lambda (row)
     (for ([cell row] [w widths])
       (display (pad-right cell w))
       (display "  "))
     (newline))
   rows))

#| =================== public =================== |#

;; dataframe structure
(struct dataframe (data)
  #:transparent
  #:guard dataframe-guard)

;; immutable ref
(define (dataframe-ref df key [default #f])
  (hash-ref (dataframe-data df) key default))

;; immutable set
(define (dataframe-set df key value)
  (dataframe (hash-set (dataframe-data df) key value)))

;; immutable remove
(define (dataframe-remove df key)
  (dataframe (hash-remove (dataframe-data df) key)))

;; immutable update
(define (dataframe-update df key updater)
  (dataframe (hash-update (dataframe-data df) key updater)))

;; immutable has key
(define (dataframe-has-key df key)
  (hash-has-key? (dataframe-data df) key))

;; rename keys
(define (dataframe-rename df ns)
  (dataframe (for/hash ([(k v) (in-hash (dataframe-data df))])
               (values (hash-ref ns k k) v))))

;; change type
(define (dataframe-retype dt fn ns)
  (dataframe 
   (for/hash ([(k v) (in-hash (dataframe-data dt))])
     (if (member k ns) (values k (map fn v)) (values k v)))))

;; shape
(define (dataframe-shape df)
  (define col-length (hash-count (dataframe-data df)))
  (define first-key (car (hash-keys (dataframe-data df))))
  (define row-length (length (dataframe-ref df first-key)))
  (list row-length col-length))

;; head of data frame
(define (dataframe-head df n)
  (dataframe
   (for/hash ([(k v) (in-hash (dataframe-data df))])
     (values k (take v n)))))

;; tail of data frame
(define (dataframe-tail df n)
  (dataframe
   (for/hash ([(k v) (in-hash (dataframe-data df))])
     (values k (take-right v n)))))

;; dropna (where na is: "")
(define (dataframe-dropna df)
  (define (remove-sublists-with-empty-string ls)
    (filter (lambda (subls) (not (member "" subls))) ls))
  (define ks (hash-keys (dataframe-data df)))
  (define vs (transpose (remove-sublists-with-empty-string (transpose (hash-values (dataframe-data df))))))
  (dataframe (make-immutable-hash (map cons ks vs))))

;; pretty print
(define (dataframe-print df n #:head [head #t])
  (define h1 (hash-keys (dataframe-data df)))
  (define v1 (transpose (hash-values (dataframe-data df))))
  (define v2 (map (lambda (ls) (map print-formatter ls)) v1))
  (define v3 (if head (take v2 n) (take-right v2 n)))
  (define r1 (cons h1 v3))
  (define nn 60)
  (displayln (make-string nn #\=))
  (print-table r1)
  (displayln (make-string nn #\=)))

;; read from csv (make more general!)
(define dataframe-from-csv (compose-pipe
                            read-csv
                            dt-clean
                            ;;dt-transpose
                            transpose
                            dt-create
                            (lambda (df) (dataframe-rename df (hash "SP500" "close" "observation_date" "date")))
                            (lambda (df) (dataframe-retype df string->number (list "close")))
                            (lambda (df) (dataframe-retype df string->date-iso (list "date")))
                            ;;(lambda (df) (dataframe-retype df string->date-dd/mm/yyyy (list "date")))
                            ))


#|
(define file-name "C:/Users/tomje/Downloads/SP500.csv")
;;(define df1 (dataframe-from-csv file-name))
(define df1 (read-csv file-name))
(define df2 (dt-clean df1))
(define df3 (transpose df2))
(define df4 (dt-create df3))
(dataframe-print df4 10 #:head #t)
;;df3
|#

#|
(take df2 5)

(take (first df3) 5)
(take (second df3) 5)

(define df4 (dt-create df3))

(dataframe-print df4 10 #:head #t)

;; this is the bad one
(define df5 (dataframe-rename df4 (hash "SP500" "close" "observation_date" "date")))

(dataframe-print df5 10 #:head #t)

;; df4 is good...

(define df6 (dataframe-head df4 5))

(dataframe-print df6 4 #:head #t)

;;(define ns (hash "SP500" "close" "observation_date" "date"))

(define ns (hash "observation_date" "date" "SP500" "close"))
|#

#|
(define (dataframe-rename-2 df ns)
  (define ks (hash-keys (dataframe-data df)))
  (define ks-new (map (lambda (k) (hash-ref ns k k)) ks))
  (define vs (transpose (hash-values (dataframe-data df))))
  (define vss (cons ks-new vs))
  (define vss2 (take vss 5))
  (define vsss (transpose vss2))
  (define aa (dt-create vsss))
;;(define bb (dataframe-head aa 3))
  ;;(take vss 5)
  (hash-keys (dataframe-data aa))
  ;;(hash-values (dataframe-data aa))
  )

(define ns (hash "observation_date" "date" "SP500" "close"))

;;(dataframe-rename-2 df4 ns)

(define h #hash(("date" . (1 1 1 1)) ("close" . (2 3 5 8))))

(hash-keys h)
(hash-values h)
h
|#

#|
;; creates table as hash from list of lists
(define (dt-create-2 data)
  (dataframe (make-immutable-hash (map (lambda (ls) (cons (car ls) (cdr ls))) data))))

(dataframe-rename-2 df4 ns)


;;(for/hash ([(k v) (in-hash (dataframe-data df6))])
;;               (values (hash-ref ns k k) v))

|#


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

  (check-equal? (dataframe-dropna (dataframe (hash 'a '(1 2 3) 'b '(4 "" 6))))
                (dataframe '#hash((a . (1 3)) (b . (4 6)))))
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
