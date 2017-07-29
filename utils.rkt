#lang racket

(provide (all-defined-out))

(define (list-range lst start end)
  (take (drop lst start) (- end start)))
(define (n-times-string a-string n)
  (cond
    [(< n 1) ""]
    [(= n 1) a-string]
    [else
     (string-append a-string
                    (n-times-string a-string (sub1 n)))]))
