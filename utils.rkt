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


(define (take-up-to n xs)
  (cond [(or (zero? n) (empty? xs)) empty]
        [else (cons (car xs)
                    (take-up-to (- n 1) (cdr xs)))]))
(define (drop-up-to to-drop xs)
  (cond [(or (= to-drop 0) (empty? xs)) xs]
        [else (drop-up-to (sub1 to-drop) (cdr xs))]))
(define (split-into-chunks-of-size-n xs n)
  (cond [(empty? xs) empty]
        [else (let ([first-chunk (take-up-to n xs)]
                    [rest (drop-up-to n xs)])
                (cons first-chunk (split-into-chunks-of-size-n rest n)))]))
