#lang racket

(require rackunit)

(provide dataframe dataframe-ref dataframe-set dataframe-remove dataframe-update)

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
  
  ;; validate all lists have same length
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
(define (dataframe-ref dt key [default #f])
  (hash-ref (dataframe-hash dt) key default))

;; immutable set
(define (dataframe-set dt key value)
  (dataframe (hash-set (dataframe-hash dt) key value)))

;; immutable remove
(define (dataframe-remove dt key)
  (dataframe (hash-remove (dataframe-hash dt) key)))

;; immutable update
(define (dataframe-update dt key updater)
  (dataframe (hash-update (dataframe-hash dt) key updater)))

;; immutable has key
(define (dataframe-has-key dt key)
  (hash-has-key? (dataframe-hash dt) key))

#| =================== testing =================== |#

(module+ test

  (define module-name
    (file-name-from-path (variable-reference->module-source (#%variable-reference))))

  (printf "testing: ~a\n" module-name)

  (define df1 (dataframe (hash 'a '(1 2 3) 'b '(4 5 6))))
  (check-equal? (dataframe-ref df1 'a) '(1 2 3))

  (define df2 (dataframe-set df1 'c '(5 6 7)))
  (check-equal? (dataframe-ref df2 'c) '(5 6 7))

  (define df3 (dataframe-remove df2 'c))
  (check-equal? df1 df3)

  (define (double-list lst) (map (lambda (x) (* 2 x)) lst))
  (define dt4 (dataframe-update df1 'a double-list))
  (check-equal? (dataframe-ref dt4 'a) '(2 4 6))

  (check-equal? (dataframe-has-key df1 'a) #t)
  
  (displayln "testing: success!")
  
  )
