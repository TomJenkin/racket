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
    [`(lambda (,param) ,body)
     ;; When called, evaluate body in extended environment
     (lambda (argument)
       (evaluate body
                 (lambda (var)
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

#| 
   ;; A tiny pipeline macro named pipe
   (define-syntax pipe
     (syntax-rules ()
       [(_ x) x]
       [(_ x f more ...) (pipe (f x) more ...)]))
 |#

;; derivative of function
(define (derivative f #:h [h 1e-6])
  (lambda (x)
    (/ (- (f (+ x h))
          (f (- x h)))
       (* 2 h))))


#| =================== tests =================== |#

(module+ test

  (require rackunit
           math/statistics)

  (define empty-env
    (lambda (var)
      (error 'evaluate "unbound variable: ~a" var)))

  (define (test-eval expr)
    (displayln "Expression:")
    (displayln expr)
    (displayln "Result:")
    (displayln (evaluate expr empty-env))
    (displayln "----"))

  (test-eval '((lambda (x) x) 42)) ;; expected: 42

  

  
  )
