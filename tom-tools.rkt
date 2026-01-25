#lang racket

(provide evaluate
         count-down
         ;;pipe
         derivative)

;; ============================================
;; A minimal, readable λ-calculus interpreter
;; ============================================

;; evaluate :: Expression × Environment → Value
(define (evaluate exp env)
  (match exp

    ;; Literal values evaluate to themselves
    [(? number? n) n]
    [(? boolean? b) b]
    [(? string? s) s]
    
    ;; Variable reference: look it up
    [(? symbol? var-name)
     (env var-name)]
    
    ;; λ-expression: create a function value
    [`(λ (,param) ,body)
     ;; When called, evaluate body in extended environment
     (λ (argument)
       (evaluate body
                 (λ (var)
                   (if (equal? var param)
                       argument      ; New binding for param
                       (env var)))))] ; Existing bindings
    
    ;; Application: evaluate function and argument
    [`(,function-expr ,argument-expr)
     (let ([func (evaluate function-expr env)]
           [arg  (evaluate argument-expr env)])
       (func arg))]))

;; tail recursion
(define (count-down n)
  (if (= n 0)
      'done
      (count-down (- n 1))))

;; derivative of function
(define (derivative f #:h [h 1e-6])
  (λ (x)
    (/ (- (f (+ x h))
          (f (- x h)))
       (* 2 h))))


#| =================== tests =================== |#

(module+ test

  (require rackunit
           math/statistics)

  ;; self evaluation
  
  (define empty-env
    (λ (var)
      (error 'evaluate "unbound variable: ~a" var)))

  (define (test-eval expr)
    (displayln "Expression:")
    (displayln expr)
    (displayln "Result:")
    (displayln (evaluate expr empty-env))
    (displayln "----"))

  (test-eval '((λ (x) x) 42)) ;; expected: 42
  (test-eval '((λ (x) 5) 999)) ;; expected: 5

  (check-equal? (evaluate '((λ (x) x) 42) empty-env) 42)
  (check-equal? (evaluate '((λ (x) 5) 999) empty-env) 5)
  
  )
