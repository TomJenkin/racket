#lang racket

(require rackunit)

#|

See below lambda calculus definition in BNF format (if M is an expression):

M ::= x      ; variable (either an abstraction or an application or another variable)
    | λx.M   ; abstraction
    | M M    ; application

Pure lambda calculus
        ↓
Lambda calculus + evaluation strategy
        ↓
Lambda calculus + primitives + values
        ↓
Lambda calculus + effects + modules + macros
        ↓
Racket

|#

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

;; probably not strictly lambda calculus!!!
(define Y ; Y combinator for recursion
  ; This does NOT work in Racket. Why?
  ; Racket is eager (applicative-order), so (x x) is evaluated immediately → infinite loop.
  (λ (f)
    ((λ (x) (f (x x)))
     (λ (x) (f (x x))))))

; Z combinator (possibly similar to Y), this works in racket.
(define Z
  (λ (f)
    ((λ (x) (f (λ (v) ((x x) v))))
     (λ (x) (f (λ (v) ((x x) v)))))))


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



