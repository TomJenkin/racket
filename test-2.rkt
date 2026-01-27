#lang racket

(require (prefix-in gt: "gen-tools.rkt")
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




