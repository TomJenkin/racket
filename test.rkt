#lang racket

(require csv-reading
         ;;"data-table-5.rkt"
         "gen-utils.rkt")


(define (read-csv file-name)
  (call-with-input-file file-name csv->list))

(define dataframe-from-csv (compose-pipe
                            read-csv
                            ;;dt-clean
                            ;;dt-transpose
                            ;;dt-create
                            ;;(lambda (df) (dataframe-rename df (hash "SP500" "close" "observation_date" "date")))
                            ;;(lambda (df) (dataframe-retype df string->number (list "close")))
                            ;;(lambda (df) (dataframe-retype df string->date-iso (list "date")))
                            ;;;;(lambda (df) (dataframe-retype df string->date-dd/mm/yyyy (list "date")))
                            ))






;; A tiny table type: headers + rows
(struct table (headers rows) #:transparent)

;; Constructors / helpers
(define (make-table headers rows)
  (unless (and (list? headers) (andmap list? rows))
    (error 'make-table "headers must be a list and rows must be a list of lists"))
  (define w (length headers))
  (unless (andmap (λ (r) (= (length r) w)) rows)
    (error 'make-table "every row must have the same length as headers"))
  (table headers rows))

(define (table-nrows t) (length (table-rows t)))
(define (table-ncols t) (length (table-headers t)))

;; Select a column by name (returns list of values)
(define (table-col t name)
  (define idx
    (let loop ([hs (table-headers t)] [i 0])
      (cond [(empty? hs) (error 'table-col "unknown column: ~a" name)]
            [(equal? (first hs) name) i]
            [else (loop (rest hs) (add1 i))])))
  (map (λ (row) (list-ref row idx)) (table-rows t)))

;; Add a column from a list of values (one per row)
(define (table-add-col t name values)
  (define rs (table-rows t))
  (unless (= (length rs) (length values))
    (error 'table-add-col "values length (~a) must match row count (~a)"
           (length values) (length rs)))
  (table (append (table-headers t) (list name))
         (map (λ (row v) (append row (list v))) rs values)))

;; Filter rows with a predicate on each row
(define (table-filter t pred)
  (table (table-headers t)
         (filter pred (table-rows t))))

;; Pretty-print (simple)
(define (table->list t)
  (cons (table-headers t) (table-rows t)))


;; ------------------ Example ------------------

(define t
  (make-table
   '("date" "a" "b")
   '(("2025-12-15" 33.5 44.2)
     ("2025-12-16" 31.0 45.1)
     ("2025-12-17" 30.0 40.0))))

;; Pull a column
(table-col t "b") ; '(44.2 45.1 40.0)

;; Add a derived / provided column
(define t2 (table-add-col t "flag" '(#t #f #t)))

;; Filter rows (keep only rows where a > 31)
(define t3
  (table-filter t2 (λ (row) (> (list-ref row 1) 31))))

(table->list t3)


;; ------------------ Example ------------------

(define file-name "C:/Users/tomje/Downloads/SP500.csv")
(define data (read-csv file-name))

(car data)

(take (cdr data) 5)

(make-table (car data) (cdr data))

