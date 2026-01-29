#lang racket

;; call python functions from racket

(require json
         rackunit
         racket/system)

(provide call-python-fn
         list-of-lists->list)

(define (list-of-lists->list ls) (map (lambda (e) (first e)) ls))

(define (call-python-fn data #:display-cmd [display-cmd #f])
  ; call a batch of python functions via shell
  (define python-file "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (define (shell-quote s) (string-replace s "\"" "\"\""))
  (define input-json (jsexpr->string data))
  (define input-json-shell (shell-quote input-json))
  (define cmd (format "~a ~a \"~a\"" "python" python-file input-json-shell))
  (when display-cmd (displayln cmd))
  (define result-str (with-output-to-string (λ () (unless (system cmd) (error "cmd failed!")))))
  (hash-ref (string->jsexpr result-str) 'results))

(module+ test

  (when #f
    ; working
    (define xs '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
    (define ps (hash 'fn "haar_arr" 'args (list xs)))
    (define hs (call-python-fn ps #:display-cmd #f))
    null
    )
  
  (when #f
    ; working
    (define data-1 (hash 'fn "dummy" 'args '((1 2 3 "hello"))))
    (define data-2 (hash 'fn "add" 'args '(3 4)))
    (define data-3 (hash 'fn "haar_arr" 'args '((1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))
    (define data-4 (hash 'fn "haarI_arr" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7))))
    (define data-5 (hash 'fn "haar_filter" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7) 0 1.0)))
    (define batch (list data-1 data-2 data-3 data-4 data-5))
    ;;(define batch (list data-2))
    (define batch-result (call-python-fn batch #:display-cmd #t))
    (for ([e batch-result] [n (in-naturals)])
      (displayln (hash n e)))
    )

  (when #f
    ; simple working
    (define cmd "python C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py \"[{\"\"args\"\":[333],\"\"fn\"\":\"\"dummy\"\"}]\"")
    (define result-str (with-output-to-string (λ () (unless (system cmd) (error "cmd failed!")))))
    (define result (string->jsexpr result-str))
    (displayln result))

  (when #f
    ; simple not working
    (system*
     "python"
     "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py"
     "[{\"args\":[333],\"fn\":\"dummy\"}]")
    )

  )

