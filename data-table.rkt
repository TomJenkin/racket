#lang racket

(require csv-reading
         racket/list
         (prefix-in gt: "gen-tools.rkt"))

(provide (struct-out table)
         table-from-csv
         table-shape
         table-create
         table-read
         table-update
         table-delete
         table-filter
         table->list
         table-head
         table-tail
         table-rename
         table-dropna
         table-print
         table-replace
         )

#| =================== private =================== |#

(define (table-guard headers rows type-name)
  
  ;; headers: list of column names (symbols or strings)
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

;; column index
(define (col-index headers name who)
  (define idx (index-of headers name equal?))
  (unless idx (error who "unknown column: ~a" name))
  idx)

;; print formatter
(define (print-formatter e)
  (cond
    [(date? e) (gt:date->dd/mm/yyyy e)]
    [(number? e) (real->decimal-string e 2)]
    [(string? e) (if (string=? e "") "NA" e)]
    [(list? e) (if (equal? e null) "NA" e)]
    [else e]))

;; print table lines
(define (print-lines rows)

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

(struct table (headers rows)
  #:transparent
  #:guard table-guard)

;; table from csv
(define (table-from-csv file-name)
  (define data (call-with-input-file file-name csv->list))
  (table (car data) (cdr data)))

;; (rows cols)
(define (table-shape t)
  (list (length (table-rows t))
        (length (table-headers t))))

;; get a column by name
(define (table-read t name)
  (define idx (col-index (table-headers t) name 'table-col))
  (for/list ([row (in-list (table-rows t))])
    (list-ref row idx)))

;; add a new column at the end
(define (table-create t name values)
  (define rs (table-rows t))
  (unless (= (length rs) (length values))
    (error 'table-add-col "values length (~a) must match row count (~a)"
           (length values) (length rs)))
  (table (append (table-headers t) (list name))
         (map (lambda (row v) (append row (list v)))
              rs values)))

;; filter rows (predicate receives a row)
(define (table-filter t pred)
  (table (table-headers t)
         (filter pred (table-rows t))))

;; convert back to (cons headers rows)
(define (table->list t)
  (cons (table-headers t) (table-rows t)))

;; first n rows
(define (table-head t n)
  (table (table-headers t)
         (take (table-rows t) n)))

;; last n rows
(define (table-tail t n)
  (table (table-headers t)
         (take-right (table-rows t) n)))

;; rename headers using a mapping hash: old -> new (defaults to old if absent)
(define (table-rename t ns)
  (define new-headers
    (for/list ([h (in-list (table-headers t))])
      (hash-ref ns h h)))
  (table new-headers (table-rows t)))

;; drop rows that contain the empty string ""
(define (table-dropna t #:na [na-value null])
  (define rs
    (filter (lambda (row) (not (member na-value row)))
            (table-rows t)))
  (table (table-headers t) rs))

;; apply f to every value in column `name`
(define (table-update t name fn)
  (define idx (col-index (table-headers t) name 'table-map-col))
  (define new-rows
    (for/list ([row (in-list (table-rows t))])
      (for/list ([v (in-list row)] [i (in-naturals)])
        (if (= i idx) (fn v) v))))
  (table (table-headers t) new-rows))

;; remove column
(define (table-delete t name)
  (define idx (col-index (table-headers t) name 'table-drop-col))

  (define new-headers
    (for/list ([h (in-list (table-headers t))]
               [i (in-naturals)]
               #:unless (= i idx))
      h))

  (define new-rows
    (for/list ([row (in-list (table-rows t))])
      (for/list ([v (in-list row)]
                 [i (in-naturals)]
                 #:unless (= i idx))
        v)))

  (table new-headers new-rows))

;; table-replace
(define (table-replace t old-val new-val)
  (define new-rows
  (map (lambda (row)
         (map (lambda (cell) (if (equal? cell old-val) new-val cell)) row))
       (table-rows t)))
  (table (table-headers t) new-rows)
  )

;; pretty print
(define (table-print t n #:head [head #t])
  (define h1 (table-headers t))
  (define v1 (table-rows t))
  (define v2 (map (lambda (ls) (map print-formatter ls)) v1))
  (define v3 (if head (take v2 n) (take-right v2 n)))
  (define r1 (cons h1 v3))
  (define nn 50)
  (displayln (make-string nn #\=))
  (print-lines r1)
  (displayln (make-string nn #\=)))

#| =================== tests =================== |#


(module+ test
  (require rackunit
           syntax/location)

  (displayln (path->string (syntax-source-file-name #'here)))
  
  (define t0
    (table
     '("date" "a" "b")
     '(("2025-12-15" 33.5 44.2)
       ("2025-12-16" 31.0 45.1)
       ("2025-12-17" 30.0 40.0))))

  (check-equal? (table-read t0 "b") '(44.2 45.1 40.0))
  (check-equal? (table-read (table-create t0 "c" '(#t #f #t)) "c") '(#t #f #t))
  (check-equal? (table-filter t0 (lambda (row) (> (list-ref row 1) 31)))
                (table '("date" "a" "b") '(("2025-12-15" 33.5 44.2))))
  (check-equal? (table-update (table-update t0 "a" number->string) "a" string->number) t0)

  (define file-name "C:/Users/tomje/Downloads/SP500.csv")
  (define t1 (table-from-csv file-name))
  (check-equal? (table-shape t1) '(2610 2))
  (define ns (hash "observation_date" "date" "SP500" "close"))
  (check-equal? (table-headers (table-rename t1 ns)) '("date" "close"))
  (check-true (> (first (table-shape t1)) (first (table-shape (table-dropna (table-replace t1 "" '()))))))
  ;;(table-print t1 3 #:head #f)

  ;;(displayln "write function to work with table-print to find max width of each column!")

  
  ;;)
  )
