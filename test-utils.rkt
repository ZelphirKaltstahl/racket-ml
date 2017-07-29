#lang racket

(provide (all-defined-out))

(define-syntax check-values-equal?
  (syntax-rules ()
    [(_ a b) (check-equal? (call-with-values (thunk a) list)
                           b)]))

(define (lists-approximately-equal? l1 l2 epsilon)
  (define (approximately-equal? value1 value2 epsilon)
    (<= (abs (- (abs value1)
                (abs value2)))
        epsilon))

  (define (check-elements l1 l2 epsilon)
    (cond [(and (empty? l1) (empty? l2)) true]
          [(approximately-equal? (vector-ref (first l1) 0)
                                 (vector-ref (first l2) 0)
                                 epsilon)
           (check-elements (rest l1)
                           (rest l2)
                           epsilon)]
          [else false]))

  (if (= (length l1) (length l2))
      (check-elements l1 l2 epsilon)
      false))

;; updates a vector in a functional way,
;; returning a new vector which is the same as the given vector,
;; but has at position pos the value value.
(define (vector-set a-vector pos value)
  (vector->immutable-vector
   (for/vector ([index (in-range (vector-length a-vector))])
     (if (= pos index)
         value
         (vector-ref a-vector index)))))
