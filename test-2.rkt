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