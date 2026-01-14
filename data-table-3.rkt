#lang racket

(require csv-reading
         racket/date
         racket/function
         racket/list
         plot)



(define h1 make-hash)

(define h2 (hash 'a (list 1 2 3) 'b (list 2 3 4)))

(define h3 (hash))

(map list '(1 2 3) '(2 3 4))

h1
h2
h3

(define x (list "hello" 1 2 3 4 5))
(define y (list "tom" 1 2 3 4 5))
(define z (list "tom" 1 2 3 4 22))

(hash (car x) (cdr x) (car y) (cdr y))


(define lists (list x y))

(define h
  (make-immutable-hash
   (map (lambda (lst) (cons (car lst) (cdr lst)))
        lists)))

h

(define data (list x y z))

(define (dt-create data)
  (unless (apply = (map length data))
    (error 'dt-create "lists lengths are unequal"))
  (make-immutable-hash
   (map (lambda (lst) (cons (car lst) (cdr lst)))
        data)))




(define (fn2 x y) (+ x y))

(fn2 33 44)

(dt-create data)

(apply = (map length data))