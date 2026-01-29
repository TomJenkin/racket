;; "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py"

#lang racket
(require json)

(define (call-python data)
  (define input-json (jsexpr->string data))
  
  ;; Start Python process
  (define-values (proc py-out py-in py-err)
    (subprocess #f #f #f "python" "-u" "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py"))
  
  ;; Write to Python
  (display input-json py-in)
  (newline py-in)
  (flush-output py-in)
  
  ;; Small delay before closing output (helps on Windows)
  (sleep 0.01)
  
  (close-output-port py-in)
  
  ;; Read Python's response
  (define result-str (port->string py-out))
  (close-input-port py-out)
  
  ;; Check for errors
  (define error-str (port->string py-err))
  (close-input-port py-err)
  
  (subprocess-wait proc)
  
  (string->jsexpr result-str))

;; Test
(displayln (call-python (hash 'numbers '(1 2 3 4))))