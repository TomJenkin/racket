#lang racket

(require json
         rackunit)

(provide call-python-fn)

(define (call-python-fn data)
  (define input-json (jsexpr->string data))
  ;; files (you may want to add python path to some PATH as not currently recognised just python)
  (define python "C:\\Users\\tomje\\AppData\\Local\\Microsoft\\WindowsApps\\python.exe")
  (define script "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")
  (check-equal? (file-exists? script) #t)
  ;; create sub-process
  (define-values (proc py-out py-in py-err) (subprocess #f #f #f python "-u" script))
  (check-equal? (subprocess-status proc) 'running)
  ;; write json to stdin
  (display input-json py-in)
  (newline py-in)
  (flush-output py-in)
  (close-output-port py-in)
  ;; read output and close ports
  (define stdout (port->string py-out))
  (define stderr (port->string py-err))
  (close-input-port py-out)
  (close-input-port py-err)
  (subprocess-wait proc)
  ;; error if stderr != ""
  (when (not (string=? (string-trim stderr) "")) (error 'call-python stderr))
  ;; print results
  (define result (string->jsexpr stdout))
  result)

(module+ test

  (when #f
    (random-seed 0)
    (define (random-walk n [start 100])
      (reverse
       (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
         (define step (if (zero? (random 2)) -1 1))
         (cons (+ (car path) step) path))))

    (define xs (map exact->inexact (random-walk 1024 100)))
    (define data (hash 'fn "haar_arr" 'args (list xs)))
    (define result (call-python-fn data))
    (displayln result))

  ; write a similar test but for batch!

  (when #t
    ; working
    (define data-1 (hash 'fn "dummy" 'args '((1 2 3 "hello"))))
    (define data-2 (hash 'fn "add" 'args '(3 4)))
    (define data-3 (hash 'fn "haar_arr" 'args '((1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))
    (define data-4 (hash 'fn "haarI_arr" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7))))
    (define data-5 (hash 'fn "haar_filter" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7) 0 1.0)))
    (define batch (list data-1 data-2 data-3 data-4 data-5))
    ;;(define batch (list data-2))
    (define batch-result (call-python-fn batch))
    (define results (hash-ref batch-result 'results))
    (for ([e results] [n (in-naturals)])
      (displayln (hash n e)))
    null)

  (displayln "testing success!")
  
  )


