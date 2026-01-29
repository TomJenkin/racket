#lang racket

;; module remotely calls into python libraries

(require json
         racket/system)

(define (list-of-lists->list ls) (map (lambda (e) (first e)) ls))

;; generic python caller
(define (call-python-fn data)
  (define input-json (jsexpr->string data))
  (define (double-quotes s) (string-replace s "\"" "\"\"")) 
  (define input-json-str (double-quotes input-json))
  (define python-file "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (define cmd (format (string-append "python " python-file " \"~a\"") input-json-str))
  (define result-str (with-output-to-string (λ () (system cmd))))
  (hash-ref (string->jsexpr result-str) 'results))

#|
(define (call-python-batch reqs)
  (define input-json (jsexpr->string reqs))
  (define (double-quotes s) (string-replace s "\"" "\"\""))
  (define input-json-str (double-quotes input-json))
  (define python-file "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (define cmd (format (string-append "python " python-file " \"~a\"") input-json-str))
  (define result-str (with-output-to-string (λ () (system cmd))))
  (hash-ref (string->jsexpr result-str) 'result))
|#

;; config
(define data-1 (hash 'fn "dummy" 'args '((1 2 3 "hello"))))
(define data-2 (hash 'fn "add" 'args '(3 4)))
(define data-3 (hash 'fn "haar_arr" 'args '((1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))
(define data-4 (hash 'fn "haarI_arr" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7))))
(define data-5 (hash 'fn "haar_filter" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7) 0 1.0)))

;; calls
;;(call-python-fn data-1)
;;(call-python-fn data-2)
;;(list-of-lists->list (call-python-fn data-3))
;;(list-of-lists->list (call-python-fn data-4))
;;(list-of-lists->list (call-python-fn data-5))

(define batch (list data-1 data-2 data-3 data-4 data-5))
;;(define batch-res (call-python-batch batch))
(define batch-res (call-python-fn batch))


(for ([e batch-res] [n (in-naturals)])
  (displayln (hash n e)))



;; remember that chatgpt or other mentioned decorators in naming functions or similar
