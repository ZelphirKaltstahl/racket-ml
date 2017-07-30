#lang racket

(require "utils.rkt")
(provide (all-defined-out))

(define (data-empty? data)
  (empty? data))

(define (data-first data)
  (car data))

(define (data-rest data)
  (cdr data))

(define (data-range data start end)
  (list-range data start end))

(define (data-length data)
  (length data))

(define (data-point-length data-point)
  (vector-length data-point))

(define (data-filter predicate data)
  (filter predicate data))

(define (data-partition predicate data)
  (partition predicate data))

(define (data-map procedure data)
  (map procedure data))

(define (data-take data n)
  (take data n))

(define (data-drop data n)
  (drop data n))

(define (data-get-col data col-index)
  (data-map (lambda (data-point)
              (data-point-get-col data-point col-index))
            data))

(define (data-point-get-col data-point col-index)
  (vector-ref data-point col-index))

(define (data-point-take-features data-point n-features)
  (vector-take data-point n-features))

(define (labels-elements-equal? subset)
  (with-handlers ([exn:fail:contract:arity?
                   (lambda (exception)
                     (< (data-length subset) 2))])
    (apply = subset)))

(define (data-majority-prediction data label-column-index)
  (let-values ([(part1 part2)
                (data-partition (lambda (data-point)
                                  (= (data-point-get-col data-point label-column-index) 0))
                                data)])
    (cond [(> (data-length part2) (data-length part1)) 1]
          [else 0])))
