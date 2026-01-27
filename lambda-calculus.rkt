#lang racket

(require rackunit)

;; primatives
(define TRUE (λ (x) (λ (y) x)))
(define FALSE (λ (x) (λ (y) y)))
(define IF (λ (b) (λ (x) (λ (y) ((b x) y)))))

;; derived
(define AND (λ (p) (λ (q) (((IF p) q) FALSE))))
(define OR (λ (p) (λ (q) (((IF p) TRUE) q))))
(define NOT (λ (b) (((IF b) FALSE) TRUE)))
(define XOR (λ (p) (λ (q) (((IF p) (NOT q)) q))))
(define BOOL (λ (p) (λ (q) (((IF p) q) (NOT q)))))
(define PAIR (λ (a) (λ (b) (λ (f) ((f a) b)))))
(define FST  (λ (p) (p TRUE)))
(define SND  (λ (p) (p FALSE)))

;; handy constants
(define ID (λ (x) x))
(define CONST (λ (x) (λ (y) x))) ; same as TRUE

;; testing
(check-equal? ((TRUE 'then) 'else) 'then)
(check-equal? ((FALSE 'then) 'else) 'else)
(check-equal? (((IF TRUE) 'then) 'else) 'then)
(check-equal? (((IF FALSE) 'then) 'else) 'else)
(check-equal? (((IF (NOT TRUE))  'then) 'else) 'else)
(check-equal? (((IF (NOT FALSE)) 'then) 'else) 'then)
(check-equal? ((((AND TRUE) FALSE) 'then) 'else) 'else)
(check-equal? ((((AND TRUE) TRUE)  'then) 'else) 'then)
(check-equal? ((((OR TRUE) FALSE)  'then) 'else) 'then)
(check-equal? (FST ((PAIR 'a) 'b)) 'a)
(check-equal? (SND ((PAIR 'a) 'b)) 'b)



