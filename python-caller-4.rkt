#lang racket

(require json
         rackunit)

(provide call-python-fn)

(define (call-python-fn data)
  (define input-json (jsexpr->string data))
  ;; files
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
  ;; read output
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

  (random-seed 0)
  (define (random-walk n [start 100])
    (reverse
     (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
       (define step (if (zero? (random 2)) -1 1))
       (cons (+ (car path) step) path))))

  (define xs (map exact->inexact (random-walk 1024 100)))
  (define data (hash 'fn "haar_arr" 'args (list xs)))
  (define result (call-python-fn data))
  ;;(displayln result)
  (displayln "testing success!")
  
  )


