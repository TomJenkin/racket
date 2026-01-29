#lang racket

;; note: call python functions from racket (limitations on length of command in shell !!!)

(require json racket/system)

(provide call-python-fn
         list-of-lists->list)

(define (list-of-lists->list ls) (map (lambda (e) (first e)) ls))

(define (call-python-fn data #:display-cmd [display-cmd #f])
  ; python call via shell
  (define python-module "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_shell.py")
  (define (shell-quote s) (string-replace s "\"" "\"\""))
  (define input-json (jsexpr->string data))
  (define input-json-shell (shell-quote input-json))
  (define cmd (format "~a ~a \"~a\"" "python" python-module input-json-shell))
  (when display-cmd (displayln cmd))
  (define result-str (with-output-to-string (λ () (unless (system cmd) (error "cmd failed!")))))
  (hash-ref (string->jsexpr result-str) 'results))

(module+ test
   
  (require rackunit syntax/location)
  
  (when #t
    (time
     (test-case
      "single function tests"
      (random-seed 0)
      (define (random-walk n [start 100])
        (reverse
         (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
           (define step (if (zero? (random 2)) -1 1))
           (cons (+ (car path) step) path))))
      (define xs (map exact->inexact (random-walk 8 100)))
      (define data (hash 'fn "haar_arr" 'args (list xs)))
      (define result (call-python-fn data #:display-cmd #f))
      (check-true #t))))
   
  (when #t
    (time
     (test-case
      "batch tests"
      (define data-1 (hash 'fn "dummy" 'args '((1 2 3 "hello"))))
      (define data-2 (hash 'fn "add" 'args '(3 4)))
      (define data-3 (hash 'fn "haar_arr" 'args '((1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))
      (define data-4 (hash 'fn "haarI_arr" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7))))
      (define data-5 (hash 'fn "haar_filter" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7) 0 1.0)))
      (define batch (list data-1 data-2 data-3 data-4 data-5))
      (define results (call-python-fn batch #:display-cmd #f))
      (when #f (for ([e results] [n (in-naturals)])
                 (displayln (hash n e))))
      (check-true #t))))
   
  (define module-name (path->string (syntax-source-file-name #'here)))
  (displayln (string-append module-name ": testing success!")))
