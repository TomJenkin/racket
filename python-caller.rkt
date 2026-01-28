;;(define MOD "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
;;(define MOD "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")

#lang racket

(require json
         racket/system)

(define (call-python-add a b)
  (define input-json (jsexpr->string (hash 'a a 'b b)))
  (define (double-quotes s) (string-replace s "\"" "\"\"")) 
  (define input-json-str (double-quotes input-json))
  (define python-file "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (define cmd (format (string-append "python " python-file " \"~a\"") input-json-str))
  (define result-str (with-output-to-string (λ () (system cmd))))
  (define res (hash-ref (string->jsexpr result-str) 'result))
  res)

(define (call-python-dummy e)
  (define input-json (jsexpr->string (hash 'e e)))
  (define (double-quotes s) (string-replace s "\"" "\"\"")) 
  (define input-json-str (double-quotes input-json))
  (define python-file "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (define cmd (format (string-append "python " python-file " \"~a\"") input-json-str))
  (define result-str (with-output-to-string (λ () (system cmd))))
  (define res (hash-ref (string->jsexpr result-str) 'result))
  res)

;;(call-python-add 3 5)

(call-python-dummy '(1 2 3 4 5 "hello"))

;; remember that chatgpt or other mentioned decorators in naming functions or similar
