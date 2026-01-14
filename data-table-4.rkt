#lang racket

#| =================== structs =================== |#

(define (data-table-validate hash type-name)
  
  ;; validate type is hash
  (unless (hash? hash)
    (error type-name "expected hash, got: ~e" hash))
  
  ;; validate hash is immutable
  (unless (immutable? hash)
    (error type-name 
           "requires immutable hash (use (hash ...) or (hasheq ...), not make-hash)"))
  
  ;; validate all values are lists
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

;; constructor
(define (make-data-table . args)
  (data-table (apply hash args)))

;; accessor that looks like hash-ref
(define (dt-ref dt key [default #f])
  (hash-ref (data-table-hash dt) key default))

;; extend data table in an immutable way (create new)
(define (dt-append dt key value)
  (data-table (hash-set (data-table-hash dt) key value)))

#| =================== examples =================== |#

;; Example usage
(define dt (make-data-table 'a '(1 2 3) 'b '(4 5 6)))
(dt-ref dt 'a)  ; Returns: '(1 2 3)

;; This will error:
;; (make-data-table 'x '(1 2) 'y '(1 2 3))
;; Error: all lists must have same length...

dt

(define dt2 (dt-append dt "c" (list 3 4 5)))

dt2

;;(define dt3 (dt-append dt "z" (list 3 4 5 6)))

;;dt3

(data-table-hash dt2)

