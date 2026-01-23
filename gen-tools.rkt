#lang racket

(require syntax/location)

(provide pipe
         compose-pipe
         assert
         enumerate
         slice
         rolling
         ;;sum
         ;;mean
         ;;std-dev
         ;;std-dev-sample
         ;;var
         ;;var-sample
         transpose
         date->dd/mm/yyyy
         string->date-iso
         ;;timeit
         ;;timeit-2
         ;;maximum
         ;;minimum
         type-of
         rows->string
         row-lengths
         )

#| =================== private =================== |#

;; square
(define (square x) (* x x))

;; pad-left
(define (pad-left n width)
  (define s (number->string n))
  (string-append (make-string (max 0 (- width (string-length s))) #\0) s))

#| =================== public =================== |#

(define (type-of x)
  (cond
    [(number? x) 'number]
    [(string? x) 'string]
    [(symbol? x) 'symbol]
    [(list? x) 'list]
    [(pair? x) 'pair]
    [(procedure? x) 'procedure]
    [(vector? x) 'vector]
    [(hash? x) 'hash]
    [(boolean? x) 'boolean]
    [else 'unknown]))

;;(define (module-name)
;;  (path->string (syntax-source-file-name #'here)))

;; timeit2
;; same issues apply here. read timeit comments!
(define-syntax-rule (timeit-2 label expr ...)
  (begin
    (printf "~a: " label)
    (flush-output)
    (time (begin expr ...))))

;; timeit (caution, sometimes supresses output of expression being called, e.g. on multiple lines)
;; reason is possibly only returns last line but some cases not even that. want to have
;; normal behaviour in what you're calling. rework this code!
(define-syntax-rule (timeit label expr ...)
  (let ()
    (printf "~a: " label)
    (flush-output)
    (define start-cpu (current-process-milliseconds))
    (define start-real (current-inexact-milliseconds))
    (begin expr ...)
    (define cpu (- (current-process-milliseconds) start-cpu))
    (define real (- (current-inexact-milliseconds) start-real))
    (printf "CPU: ~a ms, Real: ~a ms\n" cpu (real->decimal-string real 2))))

;; pipeline macro
(define-syntax pipe
  (syntax-rules ()
    [(_ x) x]
    [(_ x f more ...) (pipe (f x) more ...)]))

;; reverse order compose functions
(define (compose-pipe . fs)
  (apply compose (reverse fs)))

;; assert
(define (assert condition [msg "assertion failed"])
  (unless condition
    (error 'assert msg)))

;; enumerate
(define (enumerate ls [start 0])
  (for/list ([value ls]
             [ix (in-naturals start)])
    (cons ix value)))

;; transpose list of lists
(define (transpose dt)
  (apply map list dt))

;; slice
(define (slice ls n m)
  (take (list-tail ls n) (- m n)))

;; rolling stats (e.g. rolling mean, std, etc.)
(define (rolling fn n ls)
  (for/list ([i (in-range (length ls))])
    (if (< i (sub1 n)) null (fn (take (list-tail ls (- i (sub1 n))) n)))))

;; min
(define (minimum ls)
  (apply min ls))

;; max
(define (maximum ls)
  (apply max ls))

;; sum
(define (sum ls)
  (apply + ls))

;; mean
(define (mean ls)
  (/ (apply + ls) (length ls)))

;; stdev
(define (std-dev ls)
  (let ([μ (mean ls)]
        [n (length ls)])
    (sqrt (/ (apply + (map (λ (x) (square (- x μ))) ls)) n))))

;; std (sample version)
(define (std-dev-sample ls)
  (let ([μ (mean ls)]
        [n (length ls)])
    (sqrt (/ (apply + (map (λ (x) (square (- x μ))) ls)) (sub1 n)))))

;; variance
(define (var ls)
  (let* ([μ (mean ls)]
         [n (length ls)]
         [sum-sq-diff (apply + (map (λ (x) (expt (- x μ) 2)) ls))])
    (/ sum-sq-diff n)))  ; population variance

;; variance sample
(define (var-sample ls)
  (let* ([μ (mean ls)]
         [n (length ls)]
         [sum-sq-diff (apply + (map (λ (x) (expt (- x μ) 2)) ls))])
    (/ sum-sq-diff (sub1 n))))  ; sample variance (unbiased)

;; date to string
(define (date->dd/mm/yyyy d)
  (string-append
   (pad-left (date-day d) 2) "/"
   (pad-left (date-month d) 2) "/"
   (number->string (date-year d))))

;; convert from string to date (assumes str is: yyyy-mm-dd)
(define (string->date-iso str)
  (apply (lambda (y m d) (date 0 0 0 d m y 0 0 #f 0))
         (map string->number (string-split str "-"))))

#|
;; print table rows
(define (print-rows rows #:sep [sep " | "])

  ;; max column widths
  (define (max-column-widths rows)
    (map (lambda (col)
           (apply max (map string-length col))) (apply map list rows)))

  ;; pad right
  (define (pad-right s w)
    (define n (string-length s))
    (if (>= n w) s (string-append s (make-string (- w n) #\space))))

  ;; print rows
  (define widths (max-column-widths rows))
  (for ([row rows])
    (displayln (string-join (map (lambda (cell w) (pad-right cell w)) row widths) sep))))
|#

;; print table rows
(define (rows->string rows #:sep [sep " | "])

  ;; max column widths
  (define (max-column-widths rows)
    (map (lambda (col) (apply max (map string-length col))) (apply map list rows)))

  ;; pad right
  (define (pad-right s w)
    (string-append s (make-string (max 0 (- w (string-length s))) #\space)))

  ;; build output string
  (define widths (max-column-widths rows))
  (string-join (map (lambda (row) (string-join (map pad-right row widths) sep)) rows) "\n"))

;; line lengths
(define (row-lengths s)
  (map string-length (string-split s "\n")))

#| =================== tests =================== |#

(module+ test

  (require rackunit
           syntax/location)

  (define rows '(("one" "hello" "goodbye") ("1" "thomas" "three"))) 
  (define rows-string (rows->string rows))
  (define rows-string-lengths (row-lengths rows-string))
  (define nn (first rows-string-lengths))
  (displayln (make-string nn #\-))
  (displayln rows-string)
  (displayln (make-string nn #\-))
  
  (check-equal? (pipe '(1 2 3 4 5) cdr cdr car) 3)

  (check-equal? ((compose-pipe cdr cdr car) '(1 2 3 4 5)) 3)

  (check-exn exn:fail? (lambda () (assert #f)))

  (check-equal? (enumerate '(1 2 3)) '((0 . 1) (1 . 2) (2 . 3)))

  (check-equal? (list-ref (enumerate '(1 2 3 4 5) 1) 2) '(3 . 3))

  (check-equal? (slice '(0 1 2 3 4 5) 0 3) '(0 1 2))

  (check-equal? (rolling mean 3 '(0 1 2 3 4 5)) '(() () 1 2 3 4))

  (define (var-test ls) (square (std-dev ls)))
  (check-= (var '(0 1 2 3 4 5)) (var-test '(0 1 2 3 4 5)) 1e-10)

  (check-equal? (rolling sum 3 '(0 1 2 3 4 5)) '(() () 3 6 9 12))

  (check-equal? (transpose '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))

  (define d1 (date 0 0 0 14 1 2016 0 0 #f 0))
  (check-equal? (date->dd/mm/yyyy d1) "14/01/2016")

  (check-equal? (rolling maximum 3 (range 0 5)) '(() () 2 3 4))

  (check-equal? (rolling minimum 3 (range 0 5)) '(() () 0 1 2))

  )
