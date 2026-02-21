#lang racket

(require json
         (prefix-in gt: "gen-tools.rkt")
         (prefix-in qt: "quant-tools.rkt"))


(define ls (range 22 30))

(for/list ([e ls] [i (in-naturals)])
  (define m (+ e e))
  (define n (+ i m))
  (list i n)
  1)

(for/list ([e (range 2 22)] [m (range 5 12)] [n (in-naturals)])
  (list e m n)
  2)

;; data vs. code

(define data (quote (+ 1 (* 2 3))))
data

(define ns (make-base-namespace))
(eval data ns)

(define a 1)
(define b 3)

(string-replace (jsexpr->string (hash 'a a 'b b)) "\"" "\"\"")

; cool stuff


;; The famous Y combinator - enables recursion without self-reference
(define Y
  (λ (f)
    ((λ (x) (f (λ (y) ((x x) y))))
     (λ (x) (f (λ (y) ((x x) y)))))))

;; Factorial using Y combinator - no named recursion!
(define factorial
  (Y (λ (fact)
       (λ (n)
         (if (zero? n)
             1
             (* n (fact (- n 1))))))))

;; Test it
(factorial 5)  ; => 120

;; Fibonacci using Y combinator
(define fibonacci
  (Y (λ (fib)
       (λ (n)
         (cond [(= n 0) 0]
               [(= n 1) 1]
               [else (+ (fib (- n 1)) 
                        (fib (- n 2)))])))))

(fibonacci 10)  ; => 55


(for/list ([n (range 0 10)] [m (range 5 600)])
  (list n m))

(define mx
  (vector
   (for/list ([n (range 0 5)])
     (for/list ([m (range 0 5)])
       (list n m)))))

mx

(when #t 1)

(define-syntax-rule (when123 test body ...)
  (if test
      (begin body ...)
      (void)))

(when123 #t 33)

(define-syntax-rule (grid/list (n n0 n1) (m m0 m1) body ...)
  (for/list ([n (in-range n0 n1)])
    (for/list ([m (in-range m0 m1)])
      body ...)))

(grid/list (n 0 5) (m 0 5)
           (list n m))

(define (grid/list/fn n0 n1 m0 m1)
  (for/list ([n (range 0 5)])
    (for/list ([m (range 0 5)])
      (list n m))))

(grid/list/fn 0 5 0 5)

(define-syntax-rule (grid/list/1 (n n0 n1) (m m0 m1) xx)
  (for/list ([n (range n0 n1)])
    (for/list ([m (range m0 m1)])
      xx)))

(grid/list/1 (n 0 4) (m 0 4)
             (list n m))

(grid/list/1 (n 0 3) (m 5 7)
             (list (list n m) (list n m)))


(grid/list/1 (n 0 3) (m 0 3)
             (list n m))

(displayln ".............................................")

(grid/list (n 0 2) (m 0 2)
           (displayln "tom")
           (list n m))

(grid/list (n 0 3) (m 0 3) null)

(for/hash ([n (in-range 0 5)]
           [m (in-range 1 5)])
  (values n m))

(for/sum ([n (in-range 0 11)])
  n)

(for*/list ([n (in-range 0 5)]
            [m (in-range 0 5)]
            #:when (even? n)
            #:when (even? m))
  (list n m))

(define (grid/list/fn2 n0 n1 m0 m1 f)
  (for/list ([n (in-range n0 n1)])
    (for/list ([m (in-range m0 m1)])
      (f n m))))

(grid/list/fn2 0 3 0 3 *)

(define aa
  (grid/list/fn2 0 3 0 3
                 (λ (n m) (+ n m))))

aa


