#lang racket

(require racket/match
         rackunit)

;; Minimalist Scheme-like self-interpreter
;; Supports: numbers, booleans, symbols (lookup), quote, if, lambda, application
;; Primitive procedures: + - * /

(define (lookup sym env)
  (let ([pair (assoc sym env)])
    (if pair
        (cdr pair)
        (error 'lookup "unknown identifier: ~a" sym))))

(define (apply-proc proc args)
  (match proc
    [(list 'closure params body env)
     (let ([bindings (map cons params args)])
       (eval-expr body (append bindings env)))]
    [(list 'primitive fn) (apply fn args)]
    [_ (error 'apply-proc "not a procedure: ~a" proc)]))

(define (eval-expr expr env)
  (cond
    [(number? expr) expr]
    [(boolean? expr) expr]
    [(string? expr) expr]
    [(symbol? expr) (lookup expr env)]
    [(pair? expr)
     (match expr
       [(list 'quote x) x]
       [(list 'if test conseq alt)
        (if (eval-expr test env)
            (eval-expr conseq env)
            (eval-expr alt env))]
       [(list 'lambda params body)
        (list 'closure (map (lambda (s) s) params) body env)]
       [else
        (let* ([proc (eval-expr (car expr) env)]
               [args (map (lambda (e) (eval-expr e env)) (cdr expr))])
          (apply-proc proc args))])]
    [else (error 'eval-expr "unsupported expression: ~a" expr)]))

;; initial environment with a few primitive procedures
(define initial-env
  (let ([mk-prim (lambda (sym fn) (cons sym (list 'primitive fn)))])
    (list (mk-prim '+ (lambda args (apply + args)))
          (mk-prim '- (lambda args (apply - args)))
          (mk-prim '* (lambda args (apply * args)))
          (mk-prim '/ (lambda args (apply / args)))
          (mk-prim '< <)
          (mk-prim '<= <=)
          (mk-prim '> >)
          (mk-prim '>= >=)
          (mk-prim 'equal? equal?))))

;; Convenience evaluator using the initial environment
(define (eval-top expr)
  (eval-expr expr initial-env))

;; Examples and quick tests
(define examples
  (list '(+ 1 2)
        '((lambda (x) (+ x 1)) 41)
        '(if (< 2 3) 7 9)
        '(quote (a b c))))

(define expected-results '(3 42 7 (a b c)))

(for ([ex examples] [exp expected-results])
  (check-equal? (eval-top ex) exp))

(displayln "self-interpreter.rkt: tests passed")
