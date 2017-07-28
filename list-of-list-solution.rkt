#lang racket

#|
Attribution:

This implementation of decision trees in Racket was written by Daniel Prager and
was originally shared at:

https://groups.google.com/forum/#!topic/racket-users/cPuTr8lrXCs

With permission it was added to the project.
|#

(define (string->data s [sep " "])
  (for/list ([line (in-list (string-split s #rx"\r?\n"))])
    (map string->number (string-split line sep))))

(define banknote-data
  (string->data (file->string "data_banknote_authentication.csv") ","))

(define test-data
  (string->data
   "2.771244718 1.784783929 0
1.728571309 1.169761413 0
3.678319846 2.81281357 0
3.961043357 2.61995032 0
2.999208922 2.209014212 0
7.497545867 3.162953546 1
9.00220326 3.339047188 1
7.444542326 0.476683375 1
10.12493903 3.234550982 1
6.642287351 3.319983761 1"))

(define (make-split rows index value)
  (define-values (left right)
    (for/fold ([left null] [right null])
              ([row (in-list rows)])
      (if (< (list-ref row index) value)
          (values (cons row left) right)
          (values left (cons row right)))))
  (list left right))

(define (gini-coefficient splits)
  (for/sum ([split (in-list splits)])
    (define n (* 1.0 (length split)))
    (define (g v) (* (/ v n) (- 1.0 (/ v n))))
    (if (zero? n)
        0
        (let ([m (for/sum ([row (in-list split)] #:when (zero? (last row)))
                   1)])
          (+ (g m) (g (- n m)))))))

(define (get-split rows)
  (define-values (best index value _)
    (for*/fold ([best null] [i -1] [v -1] [score 999])
               ([index (in-range (sub1 (length (first rows))))]
                [row (in-list rows)])
      (let* ([value (list-ref row index)]
             [s (make-split rows index value)]
             [gini (gini-coefficient s)])
        (if (< gini score)
            (values s index value gini)
            (values best i v score)))))
  (list index value best))

(define (to-terminal group)
  (define zeros (count (λ (row) (zero? (last row))) group))
  (if (> zeros (- (length group) zeros)) 0 1))

(define (split node max-depth min-size depth)
  (match-define (list index value (list left right)) node)
  (define (split-if-small branch)
    (if (<= (length branch) min-size)
        (to-terminal branch)
        (split (get-split branch) max-depth min-size (add1 depth))))
  (cond [(null? left) (to-terminal right)]
        [(null? right) (to-terminal left)]
        [(>= depth max-depth) (list index value
                                    (to-terminal left) (to-terminal right))]
        [else (list index value
                    (split-if-small left) (split-if-small right))]))

(define (build-tree rows max-depth min-size)
  (split (get-split rows) max-depth min-size 1))

(define (predict node row)
  (if (list? node)
      (match-let ([(list index value left right) node])
        (predict (if (< (list-ref row index) value)
                     left
                     right)
                 row))
      node))

(define (check-model model validation-set)
  (/ (count (λ (row) (= (predict model row) (last row)))
            validation-set)
     (length validation-set)
     1.0))

;(define test-model (build-tree test-data 1 1))
;(for/list ([row (in-list test-data)])
;  (list row (predict test-model row)))

(define data (shuffle banknote-data))
(define model (time (build-tree (take data 274) 5 10)))

model

(check-model model (drop data 274))

(random-seed 12345)
(define data2 (shuffle banknote-data))
(time
 (void
  (build-tree (take data2 274) 5 10)))
(time
 (for ([i (in-range 20)])
   (build-tree (take data2 274) 5 10)))
