#lang racket

(require rackunit)

(provide dataframe
         dataframe-ref
         dataframe-set
         dataframe-remove
         dataframe-update
         dataframe-has-key
         dataframe-shape)

#| =================== structs =================== |#

(define (dataframe-guard hash type-name)
  
  ;; validate type is hash
  (unless (hash? hash)
    (error type-name "expected hash, got: ~e" hash))
  
  ;; validate hash is immutable
  (unless (immutable? hash)
    (error type-name 
           "requires immutable hash (use (hash ...) or (hasheq ...), not make-hash)"))
  
  ;; validate all values are lists (change to also include vectors!)
  (for ([(k v) (in-hash hash)])
    (unless (list? v)
      (error type-name "value for key ~e is not a list: ~e" k v)))
  
  ;; validate all lists have same length or hash is empty
  (unless (or (hash-empty? hash)
              (apply = (map length (hash-values hash))))
    (error type-name
           "all lists must have same length. Lengths: ~a"
           (hash-map hash (lambda (k v) (cons k (length v))))))
  
  hash)

(struct dataframe (hash)
  #:transparent
  #:guard dataframe-guard)

#| =================== functions =================== |#

;; immutable ref
(define (dataframe-ref df key [default #f])
  (hash-ref (dataframe-hash df) key default))

;; immutable set
(define (dataframe-set df key value)
  (dataframe (hash-set (dataframe-hash df) key value)))

;; immutable remove
(define (dataframe-remove df key)
  (dataframe (hash-remove (dataframe-hash df) key)))

;; immutable update
(define (dataframe-update df key updater)
  (dataframe (hash-update (dataframe-hash df) key updater)))

;; immutable has key
(define (dataframe-has-key df key)
  (hash-has-key? (dataframe-hash df) key))

;; immutable shape
(define (dataframe-shape df)
  (define col-length (hash-count (dataframe-hash df)))
  (define first-key (car (hash-keys (dataframe-hash df))))
  (define row-length (length (dataframe-ref df first-key)))
  (list row-length col-length))

#| =================== testing =================== |#

(module+ test

  (define module-name
    (file-name-from-path (variable-reference->module-source (#%variable-reference))))

  (printf "testing: ~a\n" module-name)

  (define df1 (dataframe (hash 'a '(1 2 3) 'b '(4 5 6))))
  (check-equal? (dataframe-ref df1 'a) '(1 2 3))
  (check-equal? (dataframe-ref df1 'missing-key 'na) 'na)

  (define df2 (dataframe-set df1 'c '(5 6 7)))
  (check-equal? (dataframe-ref df2 'c) '(5 6 7))

  (define df3 (dataframe-remove df2 'c))
  (check-equal? df1 df3)

  (define (double-list lst) (map (lambda (x) (* 2 x)) lst))
  (define df4 (dataframe-update df1 'a double-list))
  (check-equal? (dataframe-ref df4 'a) '(2 4 6))

  (check-equal? (dataframe-has-key df1 'a) #t)

  (define df5 (dataframe-shape df1))
  (check-equal? df5 '(3 2))
  
  (displayln "testing: success!")
  
  )
