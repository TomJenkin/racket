#lang racket

;; this is the best haar wavelet library / use this!

(provide haar haarI)

(define (haar xs)
  (define n (length xs))
  (unless (and (positive? n) (zero? (bitwise-and n (sub1 n))))
    (error 'haar-flat "length must be power of two, got ~a" n))
  (let loop ([a (map exact->inexact xs)] [out '()])
    (if (null? (cdr a))
        (cons (car a) out)
        (let-values ([(avg det)
                      (for/fold ([avg '()] [det '()]) ([p (in-slice 2 a)])
                        (define x (first p)) (define y (second p))
                        (values (cons (* 0.5 (+ x y)) avg)
                                (cons (* 0.5 (- x y)) det)))])
          (loop (reverse avg) (append (reverse det) out))))))

(define (haarI cs)
  (define n (length cs))
  (unless (and (positive? n) (zero? (bitwise-and n (sub1 n))))
    (error 'haarI "length must be power of two, got ~a" n))
  (let loop ([a (list (exact->inexact (car cs)))] [rest (cdr cs)] [m 1])
    (if (= m n)
        a
        (let* ([det  (take rest m)]
               [next (apply append
                            (map (λ (av de) (list (+ av de) (- av de))) a det))])
          (loop next (drop rest m) (* 2 m))))))

(module+ test
  
  (require rackunit
           (prefix-in qw: "quant-wrapper.rkt"))
  
  (define xs (map exact->inexact (range 0 (expt 2 10))))
  (displayln (length xs))
  (check-equal? (haarI (haar xs)) xs)
  (time (void (haarI (haar xs))))

  (define ys (map exact->inexact '(1 1 1 1 2 2 2 2)))
  (displayln (haar ys)) ; this looks correct: (1.5 -0.5 0.0 0.0 0.0 0.0 0.0 0.0)
  (displayln (qw:haar ys)) ; why not the same, this looks wrong: (4.2 -1.4 0.0 0.0 0.0 0.0 0.0 0.0)
 
  (displayln "complete!"))
