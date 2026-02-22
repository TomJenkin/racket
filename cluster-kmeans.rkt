#lang racket

(require (prefix-in qw: "quant-wrapper.rkt"))

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

;; best choice, simple and tested !!!!
(define (kmeans-1 points k iters #:verbose [verbose #f])
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

#| 
   ;; alternate, not faster, ignore
   (define (kmeans-2 points k iters #:verbose [verbose #f])
     (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
     (define (closest-index p cs)
       (car (for/fold ([best (list 0 (dist2 p (car cs)))]) ([c cs] [i (in-naturals)])
              (let* ([d (dist2 p c)] [best-d (cadr best)])
                (if (< d best-d) (list i d) best)))))
     (define (mean ps) (map (λ (xs) (/ (apply + xs) (length ps))) (apply map list ps)))
     (let loop ([cs (take points k)] [i iters])
       (define assns (map (λ (p) (closest-index p cs)) points))
       (define groups (for/list ([j k]) (filter-map (λ (p a) (and (= a j) p)) points assns)))
       (define new-cs (map (λ (g c) (if (null? g) c (mean g))) groups cs))
       (if (or (zero? i) (equal? cs new-cs))
           (begin (when verbose (printf "k-means converged (iterations = ~a)\n" (- iters i)))
                  (hash 'centroids new-cs 'assignments assns))
           (loop new-cs (sub1 i)))))
   
   ;; a bit faster but not much an considerably more code
   (define (kmeans-3 points k iters #:verbose [verbose #f])
     (define pts (for/vector ([p points]) (list->vector p)))
     (define dims (vector-length (vector-ref pts 0)))
     (define n (vector-length pts))
     
     (define (dist2 p q) 
       (for/fold ([s 0]) ([i (in-range dims)])
         (define d (- (vector-ref p i) (vector-ref q i)))
         (+ s (* d d))))
     
     (define (closest-index p cs-vec)
       (let ([best-d (dist2 p (vector-ref cs-vec 0))])
         (let loop ([best-i 0] [best-d best-d] [i 1])
           (if (= i k) best-i
               (let ([d (dist2 p (vector-ref cs-vec i))])
                 (if (< d best-d)
                     (loop i d (add1 i))
                     (loop best-i best-d (add1 i))))))))
     
     (define (mean idxs)
       (define sums (make-vector dims 0.0))
       (for ([i idxs])
         (define p (vector-ref pts i))
         (for ([j (in-range dims)])
           (vector-set! sums j (+ (vector-ref sums j) (vector-ref p j)))))
       (define len (length idxs))
       (for/vector ([j (in-range dims)]) (/ (vector-ref sums j) len)))
     
     (let loop ([cs (for/vector ([p (take points k)]) (list->vector p))] [i iters])
       (define assns (make-vector n 0))
       (for ([pi (in-range n)])
         (vector-set! assns pi (closest-index (vector-ref pts pi) cs)))
       
       (define new-cs (vector-copy cs))
       (for ([j (in-range k)])
         (define idxs (for/list ([pi (in-range n)] #:when (= (vector-ref assns pi) j)) pi))
         (unless (null? idxs)
           (vector-set! new-cs j (mean idxs))))
       
       (if (or (zero? i) (equal? cs new-cs))
           (begin 
             (when verbose (printf "k-means converged (iterations = ~a)\n" (- iters i)))
             (hash 'centroids (for/vector ([c new-cs]) (vector->list c)) 
                   'assignments (vector->list assns)))
           (loop new-cs (sub1 i)))))
   
 |#

;; call python/sklearn
(define (kmeans-4 points k dummy  #:verbose [verbose #f])
  (qw:kmeans points k))

;; define kmeans as...
(define kmeans kmeans-1)

(module+ test
  (require rackunit (prefix-in gt: "gen-tools.rkt"))
  (define debug-mode #f)
  (time
   (when debug-mode
     (define pts '((1 1) (1.1 0.9) (0.9 1.2) (5 5) (5.2 4.8) (4.9 5.1)))
     ;;(define-values (centroids assignments) (kmeans pts 2 1000))
     (define cluster-hash (kmeans pts 2 1000))
     (define centroids (hash-ref cluster-hash 'centroids))
     (define assignments (hash-ref cluster-hash 'assignments))
     (check-equal? (map (λ (row) (map (λ (e) (gt:round/n e 2)) row)) centroids) '((5.03 4.97) (1.0 1.03)))
     (check-equal? assignments '(1 1 1 0 0 0)))))
