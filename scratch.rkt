#lang racket

(require json
         racket/date)


;; distance testing for kmeans ============================================
;; want a matrix of distances

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

(define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))

(define p '(1 2 3 4 5))
(define q '(1 2 3 40 5))

(dist2 p q)

(dist2 '(1 2 3 4 5) '(1 2 3 40 5))

(define package
  (let ([fname "G:/My Drive/Common/Documents/Code/Racket/Data/racket_package_01.json"])
    (call-with-input-file fname
      (lambda (in)
        (read-json in)))))

(define centroidsI (hash-ref package 'centroidsI))

(define (distances centroids)
  (define (dist2 p q) (for/sum ([x p] [y q]) (sqr (- x y))))
  (for/list ([x centroids])
    (for/list ([y centroids])
      (dist2 x y))))

;(distances centroidsI)


;; json save testing ======================================================

(define bundle
  (hash
   'a (list 2 3 4)
   'b 44
   'c (hash 'a 44 'b 22)
   ;'d (seconds->date (find-seconds 0 0 0 22 2 2026))
   ;'e (date 0 0 0 9 1 2026 0 0 #f 0)
   ))

;bundle

(when #f
  (let ([fname "G:/My Drive/Common/Documents/Code/Racket/Data/test.json"])
    (call-with-output-file fname
      (lambda (out) (write-json bundle out))
      #:exists 'replace)))
