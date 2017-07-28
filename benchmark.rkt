#lang racket

(collect-garbage)
(collect-garbage)
(collect-garbage)

(define as (build-list 1000000 (λ (n) (random 100))))
(define bs (build-list 1000000 (λ (n) (random 100))))

(define (f as bs [acc 0])
  (if (or (null? as) (null? bs))
      acc
      (f (rest as)
         (rest bs)
         (+ acc (* (first as) (first bs))))))

(time (f as bs))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(define (g as bs)
  (let g ([as as]
          [bs bs]
          [acc 0])
    (cond [(or (null? as) (null? bs)) acc]
          [else (match-define (cons this-a more-as) as)
                (match-define (cons this-b more-bs) bs)
                (g more-as
                   more-bs
                   (+ acc (* this-a this-b)))])))

(time (g as bs))
