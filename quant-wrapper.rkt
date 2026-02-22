#lang racket

(require (prefix-in pc:"python-caller-subprocess.rkt"))

(provide kmeans haar haarI haar-filter haar-transform)

;; keep for minute, legacy...
#| (define (kmeans xs n-clusters #:random-state [random-state 0])
     (let* ([ps (hash 'fn "kmeans" 'args (list xs n-clusters) 'kwargs (hash 'random_state random-state))]
            [ls (hash-ref (pc:call-python-fn ps) 'results)]
            [result (hash 'labels (first ls) 'centroids (second ls))])
    result)) |#

(define (kmeans xs n-clusters #:random-state [random-state 0])
  (let* ([ps (hash 'fn "kmeans" 'args (list xs n-clusters) 'kwargs (hash 'random_state random-state))]
         [ls (hash-ref (pc:call-python-fn ps) 'results)]
         [result (hash 'centroids (second ls) 'assignments (first ls))])
    result))

(define (haar xs)
  (let* ([ps (hash 'fn "haar_arr" 'args (list xs))]
         [ls (hash-ref (pc:call-python-fn ps) 'results)]
         [rs (map first ls)])
    rs))

(define (haarI xs)
  (let* ([ps (hash 'fn "haarI_arr" 'args (list xs))]
         [ls (hash-ref (pc:call-python-fn ps) 'results)]
         [rs (map first ls)])
    rs))

(define (haar-filter xs low high)
  (let* ([ps (hash 'fn "haar_filter" 'args (list xs low high))]
         [ls (hash-ref (pc:call-python-fn ps) 'results)]
         [rs (map first ls)])
    rs))

(define (haar-transform xs low high)
  (let* ([ps (hash 'fn "haar_transform" 'args (list xs low high))]
         [ls (hash-ref (pc:call-python-fn ps) 'results)]
         [rs (map first ls)])
    rs))


(module+ test

  (require rackunit syntax/location (prefix-in gt:"gen-tools.rkt"))

  (define debug-mode #f)
  
  (time

   (when debug-mode
     (define xs (map exact->inexact '(1 1 1 1 2 2 2 2)))
     (displayln (haar xs)))
   
   (when debug-mode
     (test-case
      "kmeans"
      (define xs (for/list ([a (range 0 20)] [b (range 0 20)]) (list a b)))
      (check-true (hash? (kmeans xs 5 #:random-state 23)))))

   (when debug-mode
     (test-case
      "haar"
      (let* ([xs (for/list ([a (range 0 20)]) (exact->inexact a))]
             [hs (haar xs)]
             [hsi (haarI hs)]
             [hsir (gt:round-n hsi 9)]
             [hsf (haar-filter hs 0.0 1.0)]
             [xst (haar-transform xs 0.0 1.0)]
             [xstr (gt:round-n xst 9)])
        (check-equal? xs hsir)
        (check-equal? hs hsf)
        (check-equal? xs xstr))))

   (define module-name (path->string (syntax-source-file-name #'here)))
   (displayln (string-append module-name ": testing success!"))))
