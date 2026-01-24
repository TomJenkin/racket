#lang racket

(require (prefix-in gt: "gen-tools.rkt"))


(define ls (range 22 30))

(for/list ([e ls] [i (in-naturals)])
  (define m (+ e e))
  (define n (+ i m))
  (list i n))

