#lang racket

(require json
         racket/date)


;; json save testing ======================================================

(define bundle
  (hash
   'a (list 2 3 4)
   'b 44
   'c (hash 'a 44 'b 22)
   ;'d (seconds->date (find-seconds 0 0 0 22 2 2026))
   ;'e (date 0 0 0 9 1 2026 0 0 #f 0)
   ))

bundle

(when #t
  (let ([fname "G:/My Drive/Common/Documents/Code/Racket/Data/test.json"])
    (call-with-output-file fname
      (lambda (out) (write-json bundle out))
      #:exists 'replace)))
