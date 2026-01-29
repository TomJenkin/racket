#lang racket

(require json
         rackunit)


(random-seed 0)
(define (random-walk n [start 100])
  (reverse
   (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
     (define step (if (zero? (random 2)) -1 1))
     (cons (+ (car path) step) path))))

(define xs (map exact->inexact (random-walk 256 100)))

;;(define data (hash 'numbers (range 0 1020894)))

;;(define data (hash 'fn "haar_arr" 'args (list (list 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))

(define data (hash 'fn "haar_arr" 'args (list xs)))

(define input-json (jsexpr->string data))
   
(define python "C:\\Users\\tomje\\AppData\\Local\\Microsoft\\WindowsApps\\python.exe")
;;(define script "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py")
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
(when (not (string=? (string-trim stderr) ""))
  (error 'call-python stderr))
   
;(displayln "STDOUT:")
;(displayln stdout)

;; print results
(define result (string->jsexpr stdout))
(displayln result)







#| 
   (define python "python")
   (define script "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py")
   
   ;; Example data
   (define data (hash 'numbers '(1 2 3 4)))
   (define input-json (jsexpr->string data))
   
   ;; Start Python running THAT file
   (define-values (proc py-out py-in py-err)
     (subprocess #f #f #f
                 python "-u" script))
   
   
   (displayln (subprocess-status proc))
   
   ;; IMPORTANT: check it didn't exit immediately
   (when (number? (subprocess-status proc))
     (define err (port->string py-err))
     (error 'call-python
            (string-append "Python exited immediately:\n" err)))
   
   ;; Write JSON to stdin
   (display input-json py-in)
   (newline py-in)
   (flush-output py-in)
   (close-output-port py-in)
   
   ;; Read output
   (define stdout (port->string py-out))
   (define stderr (port->string py-err))
   
   (close-input-port py-out)
   (close-input-port py-err)
   
   (subprocess-wait proc)
   
   (when (not (string=? (string-trim stderr) ""))
     (error 'call-python stderr))
   
   (displayln "STDOUT:")
   (displayln stdout)
 |#

;;C:\Users\tomje\AppData\Local\Microsoft\WindowsApps\python.exe


#| 
   ;;(define python "python")
   (define python "C:\\Users\\tomje\\AppData\\Local\\Microsoft\\WindowsApps\\python.exe")
   (define script "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py")
   
   (displayln (string-append "script exists? " (if (file-exists? script) "yes" "NO")))
   (displayln (format "python path: ~a" (find-executable-path python)))
 |#

;;C:\Users\tomje\AppData\Local\Microsoft\WindowsApps\python.exe

;;(define python
;;  "C:\\Users\\tomje\\AppData\\Local\\Programs\\Python\\Python312\\python.exe")

#| (define python "C:\\Users\\tomje\\AppData\\Local\\Microsoft\\WindowsApps\\python.exe")
   (define script "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher_02.py")
   (define-values (proc py-out py-in py-err) (subprocess #f #f #f python "-u" script))
(check-equal? (subprocess-status proc) 'running) |#

