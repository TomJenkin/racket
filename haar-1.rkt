#lang racket

(require racket/list)

;; -------------------------
;; Helpers: matrix <-> columns
;; -------------------------

(define (transpose rows)
  (cond
    [(or (null? rows) (null? (car rows))) '()]
    [else (cons (map car rows)
                (transpose (map cdr rows)))]))

(define matrix->cols transpose)
(define cols->matrix transpose)

;; -------------------------
;; Helpers: power-of-two length / log2
;; -------------------------

(define (pow2? n)
  (and (integer? n) (positive? n)
       (zero? (bitwise-and n (sub1 n)))))

(define (log2pow2 n)
  (unless (pow2? n)
    (error 'haar "length must be a positive power of 2, got ~a" n))
  (integer-length (sub1 n)))

;; -------------------------
;; Haar 1D (in-place style, but purely functional)
;; Matches the behavior of your NumPy in-place code:
;; for p = pp..1, transform only the first 2^p entries.
;; -------------------------

(define k (sqrt 0.5))

(define (haar1d xs)
  (define N (length xs))
  (define pp (log2pow2 N))
  (define (step state p)
    (define n (arithmetic-shift 1 p))
    (define m (quotient n 2))
    (define prefix (take state n))
    (define suffix (drop state n))
    (define pairs (for/list ([i (in-range m)])
                    (list (list-ref prefix (* 2 i))
                          (list-ref prefix (add1 (* 2 i))))))
    (define avgs  (map (λ (ab) (* k (+ (first ab) (second ab)))) pairs))
    (define diffs (map (λ (ab) (* k (- (first ab) (second ab)))) pairs))
    (append avgs diffs suffix))
  (for/fold ([state xs]) ([p (in-range pp 0 -1)])
    (step state p)))

(define (haarI1d xs)
  (define N (length xs))
  (define pp (log2pow2 N))
  (define (step state p)
    (define n (arithmetic-shift 1 p))
    (define m (quotient n 2))
    (define prefix (take state n))
    (define suffix (drop state n))
    (define avgs  (take prefix m))
    (define diffs (drop prefix m))
    (define rebuilt
      (apply append
             (for/list ([a avgs] [d diffs])
               (list (* k (+ a d))
                     (* k (- a d))))))
    (append rebuilt suffix))
  (for/fold ([state xs]) ([p (in-range 1 (add1 pp))])
    (step state p)))

;; -------------------------
;; Haar filter (column-wise)
;; Keeps coefficient if cum-mass in (low, high]
;; cum-mass = cumsum(abs) / sum(abs) after sorting by abs ascending
;; -------------------------

(define (cumsum xs)
  (define-values (rev _sum)
    (for/fold ([acc '()] [s 0.0]) ([x xs])
      (define s2 (+ s x))
      (values (cons s2 acc) s2)))
  (reverse rev))

(define (haar-filter-col xs low high)
  (unless (and (real? low) (real? high) (<= 0.0 low high 1.0))
    (error 'haar-filter "need 0<=low<=high<=1, got low=~a high=~a" low high))

  ;; rows: (idx val abs)
  (define rows
    (for/list ([x xs] [i (in-naturals)])
      (list i x (abs x))))

  (define total-abs (apply + (map third rows)))

  (cond
    [(zero? total-abs) xs] ; all zeros => nothing to keep
    [else
     (define sorted (sort rows < #:key third)) ; ascending abs
     (define abses  (map third sorted))
     (define cms    (map (λ (s) (min 1.0 (/ s total-abs))) (cumsum abses)))

     ;; attach cumulative mass, compute filtered value
     (define sorted+
       (map (λ (row cm)
              (define idx (first row))
              (define val (second row))
              (define keep? (and (> cm low) (<= cm high)))
              (list idx (if keep? val 0.0)))
            sorted cms))

     ;; restore original order by idx, return values
     (define restored (sort sorted+ < #:key first))
     (map second restored)]))

;; -------------------------
;; Matrix-level APIs (list of rows)
;; -------------------------

(define (haar-arr mat)
  (define cols (matrix->cols mat))
  (define cols2 (map haar1d cols))
  (cols->matrix cols2))

(define (haarI-arr mat)
  (define cols (matrix->cols mat))
  (define cols2 (map haarI1d cols))
  (cols->matrix cols2))

(define (haar-filter mat low high)
  (define cols (matrix->cols mat))
  (define cols2 (map (λ (c) (haar-filter-col c low high)) cols))
  (cols->matrix cols2))

;; -------------------------
;; Example (uncomment to try)
;; -------------------------
(define mat '((1.0 10.0)
             (2.0 20.0)
             (3.0 30.0)
             (4.0 40.0)
             (5.0 50.0)
             (6.0 60.0)
             (7.0 70.0)
             (8.0 80.0)))

(define h (haar-arr mat))
(define hf (haar-filter h 0.20 1.00))
(define back (haarI-arr hf))
h
hf
back
