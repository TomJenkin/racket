#lang racket

(define (validate-fn hash type-name)
  ;; Validate type is hash
  (unless (hash? hash)
    (error type-name "expected hash, got: ~e" hash))
  ;; Validate all values are lists
  (for ([(k v) (in-hash hash)])
    (unless (list? v)
      (error type-name "value for key ~e is not a list: ~e" k v)))
  ;; Validate all lists have same length
  (unless (or (hash-empty? hash)
              (apply = (map length (hash-values hash))))
    (error type-name
           "all lists must have same length. Lengths: ~a"
           (hash-map hash (λ (k v) (cons k (length v))))))
  hash)

(struct data-table (hash)
  #:transparent
  #:guard validate-fn)

;; Constructor
(define (make-data-table . args)
  (data-table (apply hash args)))

;; Accessor that looks like hash-ref
(define (dt-ref dt key [default #f])
  (hash-ref (data-table-hash dt) key default))

;; Example usage
(define dt (make-data-table 'a '(1 2 3) 'b '(4 5 6)))
(dt-ref dt 'a)  ; Returns: '(1 2 3)

;; This will error:
;; (make-data-table 'x '(1 2) 'y '(1 2 3))
;; Error: all lists must have same length...

dt