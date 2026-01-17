#lang racket

(require rackunit
         csv-reading
         syntax/location)

(provide (struct-out table)
         read-csv
         make-table
         table-shape
         table-col
         table-add-col
         table-filter
         table->list
         table-head
         table-tail)

#| =================== private =================== |#

(define (table-guard headers rows type-name)
  
  ;; headers: list of column names
  (unless (and (list? headers)
               (andmap (lambda (h) (or (symbol? h) (string? h))) headers))
    (error type-name "headers must be a list of symbols or strings"))

  ;; headers must be unique
  (unless (= (length headers)
             (length (remove-duplicates headers equal?)))
    (error type-name "headers must be unique"))

  ;; rows: list of rows (each row is a list)
  (unless (and (list? rows) (andmap list? rows))
    (error type-name "rows must be a list of lists"))

  ;; row width must match header count
  (define w (length headers))
  (unless (andmap (lambda (r) (= (length r) w)) rows)
    (error type-name "each row must have ~a columns" w))

  (values headers rows))

#| =================== public =================== |#

;; table struct
(struct table (headers rows)
  #:transparent
  #:guard table-guard)

;; read csv
(define (read-csv file-name)
  (call-with-input-file file-name csv->list))

;; make-table
(define (make-table headers rows)
  (unless (and (list? headers) (andmap list? rows))
    (error 'make-table "headers must be a list and rows must be a list of lists"))
  (define w (length headers))
  (unless (andmap (lambda (r) (= (length r) w)) rows)
    (error 'make-table "every row must have the same length as headers"))
  (table headers rows))

;; table shape
(define (table-shape t)
  (list (length (table-rows t)) (length (table-headers t))))

;; table column
(define (table-col t name)
  (define idx
    (let loop ([hs (table-headers t)] [i 0])
      (cond [(empty? hs) (error 'table-col "unknown column: ~a" name)]
            [(equal? (first hs) name) i]
            [else (loop (rest hs) (add1 i))])))
  (map (lambda (row) (list-ref row idx)) (table-rows t)))

;; add column
(define (table-add-col t name values)
  (define rs (table-rows t))
  (unless (= (length rs) (length values))
    (error 'table-add-col "values length (~a) must match row count (~a)"
           (length values) (length rs)))
  (table (append (table-headers t) (list name))
         (map (lambda (row v) (append row (list v))) rs values)))

;; filter rows
(define (table-filter t pred)
  (table (table-headers t)
         (filter pred (table-rows t))))

;; table to list
(define (table->list t)
  (cons (table-headers t) (table-rows t)))

;; table head
(define (table-head t n)
  (make-table (table-headers t) (take (table-rows t) n)))

;; table tail
(define (table-tail t n)
  (make-table (table-headers t) (take-right (table-rows t) n)))

;; table rename
(define (table-rename t ns)
  (define ks (map (lambda (k) (hash-ref ns k k)) (table-headers t)))
  (make-table ks (table-rows t)))

;; table dropna
(define (table-dropna t)
  (define rs (filter (lambda (row) (not (member "" row))) (table-rows t)))
  (make-table (table-headers t) rs))

#| =================== tests =================== |#

(module+ test

  (define module-name (path->string (syntax-source-file-name #'here)))
  (printf "testing: ~a\n" module-name)
  (define start-time (current-inexact-milliseconds))

  (define t0
    (make-table
     '("date" "a" "b")
     '(("2025-12-15" 33.5 44.2) ("2025-12-16" 31.0 45.1) ("2025-12-17" 30.0 40.0))))

  (check-equal? (table-col t0 "b") '(44.2 45.1 40.0))
  (check-equal? (table-col (table-add-col t0 "c" '(#t #f #t)) "c") '(#t #f #t))
  (check-equal? (table-filter t0 (lambda (row) (> (list-ref row 1) 31)))
                (table '("date" "a" "b") '(("2025-12-15" 33.5 44.2))))
  
  (define file-name "C:/Users/tomje/Downloads/SP500.csv")
  (define data (read-csv file-name))
  (define t1 (make-table (car data) (cdr data)))
  (check-equal? (table-shape t1) '(2610 2))
  (check-equal? (table->list t1) data)
  (define ns (hash "observation_date" "date" "SP500" "close"))
  (check-equal? (table-headers (table-rename t1 ns)) '("date" "close"))
  (check-true (> (first (table-shape t1)) (first (table-shape (table-dropna t1)))))
  
  (define elapsed-time (- (current-inexact-milliseconds) start-time))
  (printf "testing: success! (runtime = ~a ms)\n" (real->decimal-string elapsed-time 1))
  
  )
