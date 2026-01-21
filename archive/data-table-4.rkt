#lang racket

(require rackunit)

#| =================== structs =================== |#

(define (data-table-validate hash type-name)
  
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

(struct data-table (hash)
  #:transparent
  #:guard data-table-validate)

#| =================== functions =================== |#

;; immutable ref
(define (dt-ref dt key [default #f])
  (hash-ref (data-table-hash dt) key default))

;; immutable set
(define (dt-set dt key value)
  (data-table (hash-set (data-table-hash dt) key value)))

;; immutable remove
(define (dt-remove dt key)
  (data-table (hash-remove (data-table-hash dt) key)))

;; immutable update
(define (dt-update dt key updater)
  (data-table (hash-update (data-table-hash dt) key updater)))

;; immutable has key
(define (dt-has-key dt key)
  (hash-has-key? (data-table-hash dt) key))

#| =================== testing =================== |#

(module+ test

  (define module-name (file-name-from-path 
                       (variable-reference->module-source (#%variable-reference))))

  (printf "running module: ~a\n" module-name)

  (define dt1 (data-table (hash 'a '(1 2 3) 'b '(4 5 6))))
  
  (check-equal? (dt-ref dt1 'a) '(1 2 3))

  (define dt2 (dt-set dt1 'c '(5 6 7)))
  (check-equal? (dt-ref dt2 'c) '(5 6 7))

  (define dt3 (dt-remove dt2 'c))
  (check-equal? dt1 dt3)

  (define (double-list lst)
    (map (lambda (x) (* 2 x)) lst))
  (define dt4 (dt-update dt1 'a double-list))
  (check-equal? (dt-ref dt4 'a) '(2 4 6))

  (check-equal? (dt-has-key dt1 'a) #t)
  
  (displayln "testing success!")
  
  )
