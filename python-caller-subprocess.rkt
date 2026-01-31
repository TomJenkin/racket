#lang racket

(require json)

(provide call-python-fn
         list-of-lists->list)

(define python-exe "C:\\Users\\tomje\\AppData\\Local\\Microsoft\\WindowsApps\\python.exe")
(define python-module "C:\\Users\\tomje\\Documents\\Code\\nets\\dispatcher.py")

(define (list-of-lists->list ls) (map (lambda (e) (first e)) ls))

(define (call-python-fn data
                        #:python [python python-exe]
                        #:script [script python-module])
  (define input-json (jsexpr->string data))
  (unless (file-exists? script) (error "missing script file"))
  ;; create sub-process
  (define-values (proc py-out py-in py-err) (subprocess #f #f #f python "-u" script))
  (unless (equal? (subprocess-status proc) 'running) (error "bad python run status"))
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

  (require rackunit syntax/location)


  ;; in wrapper module:

  (define (kmeans22 data n-clusters)

    ;; change to let !!!!!!!!!!!!!!!!!!!!!!!!!!!
    (define args  (list 'fn))
    (define kwargs (hash 'data data 'n-clusters n-clusters))
    
    33)

  

  (test-case
   "kmeans interop"
   
   ;;(define data '((1 2) (3 4) (5 6) (7 8)))

   (define args (for/list ([a (range 0 20)] [b (range 0 20)])
                  (list a b)))

   (define kwargs (hash 'n_clusters 3))

   ;; define conversion function
   (define (fn-data fn #:args [args (list)] #:kwargs [kwargs (hash)])
     (hash 'fn fn 'args (list args) 'kwargs kwargs))
  
   (define ps (fn-data "kmeans" #:args args #:kwargs kwargs))
   (displayln ps)
   (define rs (call-python-fn ps))
   (define rs1 (hash-ref rs 'results))
   (displayln (first rs1))
   (displayln (second rs1))
   )
  
  ;; !! do a speed test into a dummy .py file with no libraries to check base speed of interop/python
  
  (when #f
    (time
     (test-case
      "single function tests"
      (random-seed 0)
      (define (random-walk n [start 100])
        (reverse
         (for/fold ([path (list start)]) ([i (in-range (sub1 n))])
           (define step (if (zero? (random 2)) -1 1))
           (cons (+ (car path) step) path))))

      (define xs (map exact->inexact (random-walk 1024 100)))
      (define data (hash 'fn "haar_arr" 'args (list xs)))
      (define result (call-python-fn data))
      (check-true #t))))

  (when #f
    (time
     (test-case
      "batch tests"
      (define data-1 (hash 'fn "dummy" 'args '((1 2 3 "hello"))))
      (define data-2 (hash 'fn "add" 'args '(3 4)))
      (define data-3 (hash 'fn "haar_arr" 'args '((1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))))
      (define data-4 (hash 'fn "haarI_arr" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7))))
      (define data-5 (hash 'fn "haar_filter" 'args '((12.7 -5.6 -2 -2 -0.7 -0.7 -0.7 -0.7) 0 1.0)))
      (define batch (list data-1 data-2 data-3 data-4 data-5))
      (define batch-result (call-python-fn batch))
      (define results (hash-ref batch-result 'results))
      (check-true #t)
      (when #f (for ([e results] [n (in-naturals)])
                 (displayln (hash n e)))))))

  (define module-name (path->string (syntax-source-file-name #'here)))
  (displayln (string-append module-name ": testing success!")))
