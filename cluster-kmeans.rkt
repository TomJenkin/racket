#lang racket

#| (provide
    (contract-out
  [kmeans kmeans/c])) |#

(provide kmeans)

#| (define kmeans/c
     (-> (non-empty-listof (listof real?))        ; points
         exact-nonnegative-integer?               ; k
         exact-nonnegative-integer?               ; iters
         (hash/c 'centroids (listof (listof real?))
              'assignments (listof exact-nonnegative-integer?)))) |#

#| (define kmeans/c
     (-> (non-empty-listof (listof real?))        ; points
         exact-nonnegative-integer?               ; k
         exact-nonnegative-integer?               ; iters
         (values
          (listof (listof real?))                 ; centroids
       (listof exact-nonnegative-integer?))))  ; assignments |#

(define (kmeans points k iters #:verbose [verbose #f])
  (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
  (define (closest p cs)
    (for/fold ([b 0] [bv +inf.0] [i 0] #:result b) ([c cs])
      (let ([v (dist2 p c)]) (if (< v bv) (values i v (add1 i)) (values b bv (add1 i))))))
  (define (mean ps) (map (λ (xs) (/ (apply + xs) (length ps))) (apply map list ps)))
  (let loop ([cs (take points k)] [i iters])
    (define assns (map (λ (p) (closest p cs)) points))
    (define groups (for/list ([j k]) (filter-map (λ (p a) (and (= a j) p)) points assns)))
    (define new-cs (map (λ (g c) (if (null? g) c (mean g))) groups cs))
    (if (or (zero? i) (equal? cs new-cs))
        (begin
          (when verbose
            (printf "k-means converged (iterations = ~a)\n" (- iters i)))
          ;;(values new-cs assns))
          (hash 'centroids new-cs 'assignments assns))
        (loop new-cs (sub1 i)))))

(module+ test
  (require rackunit (prefix-in gt: "gen-tools.rkt"))
  (time
   (define pts '((1 1) (1.1 0.9) (0.9 1.2) (5 5) (5.2 4.8) (4.9 5.1)))
   ;;(define-values (centroids assignments) (kmeans pts 2 1000))
   (define cluster-hash (kmeans pts 2 1000))
   (define centroids (hash-ref cluster-hash 'centroids))
   (define assignments (hash-ref cluster-hash 'assignments))
   (check-equal? (map (λ (row) (map (λ (e) (gt:round/n e 2)) row)) centroids) '((5.03 4.97) (1.0 1.03)))
   (check-equal? assignments '(1 1 1 0 0 0))))
