#lang racket

#|
Implementation adapted from:
http://machinelearningmastery.com/implement-decision-tree-algorithm-scratch-python/
|#

(require "csv-to-list.rkt")
(provide (all-defined-out))

(define FILE-PATH "data_banknote_authentication.csv")
(define COLUMN-CONVERTERS (list string->number
                                string->number
                                string->number
                                string->number
                                (lambda (a-class) (inexact->exact (string->number a-class)))))
(define data-set (all-rows FILE-PATH #:column-converters COLUMN-CONVERTERS))


;; =========================================================
;; HELPER PROCEDURES
;; =========================================================
(define (list-range lst start end)
  (take (drop lst start) (- end start)))
;; =========================================================

;; =========================================================
;; ABSTRACTION LAYER
;; (for data representation)
;; =========================================================
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
(define (data-get-col data col-index)
  (data-map (lambda (data-point)
              (data-point-get-col data-point col-index))
            data))
(define (data-point-get-col data-point col-index)
  (vector-ref data-point col-index))

(struct Split
  (index
   value
   subsets
   cost)
  #:transparent)

(struct Node
  (data
   split-feature-index
   split-value
   left right)
  #:transparent)

(define (make-leaf-node data)
  (Node data
        'none
        'none
        empty
        empty))

(define (leaf-node? node)
  (and (empty? (Node-left node))
       (empty? (Node-right node))))
;; =========================================================

(define (labels-elements-equal? subset)
  (with-handlers ([exn:fail:contract:arity?
                   (lambda (exception)
                     (< (data-length subset) 2))])
    (apply = subset)))

(define (class-equals? class-1 class-2)
  (= class-1 class-2))

;; =========================================================
;; DECISION TREE ALGORITHM
;; (its procedures)
;; =========================================================
(define (calc-proportion subset class-label label-column-index)
  (cond [(data-empty? subset) 0]
        [else (let* ([row-count (data-length subset)]
                     [class-count (count
                                   (lambda (row)
                                     (class-equals? class-label
                                                    (data-point-get-col row label-column-index)))
                                   subset)]
                     [prop (/ class-count row-count)])
                (* prop (- 1.0 prop)))]))

#|
The procedure gini-index is used to evaluate the quality of a split.
It is a cost function for a split.
We want to keep the costs for splits low. (also: greedy)
There are other ways of calculating the quality of a split, but for now we
implement gini index.
|#
(define (gini-index subsets label-column-index)
  (for/sum ([subset (in-list subsets)])
    (for/sum ([label (in-list (list 0 1))])
      (calc-proportion subset
                       label
                       label-column-index))))

(define (split-data data index value)
  (let-values ([(part1 part2)
                (data-partition (lambda (data-point)
                                  (< (data-point-get-col data-point index) value))
                                data)])
    (list part1 part2)))

(define (get-best-split data
                        split-cost-function
                        feature-column-indices
                        label-column-index)
  (define (select-better-split earlier-best-result
                               split-cost-function
                               data
                               split-feature-index
                               split-value)
    (let* ([new-split-subsets (split-data data split-feature-index split-value)]
           [new-split-cost (split-cost-function new-split-subsets
                                                label-column-index)])
      (cond [(< new-split-cost (Split-cost earlier-best-result))
             (display "new best split cost:")
             (displayln new-split-cost)
             (Split split-feature-index
                    split-value
                    new-split-subsets
                    new-split-cost)]
            [else earlier-best-result])))

  ;; iterates over values of one feature, to find the best split value
  (define (iter-values split-feature-index remaining-rows current-result)
    ;;(display "remaining rows: ") (display (data-length remaining-rows)) (newline)
    (cond [(data-empty? remaining-rows) current-result]
          [(= (Split-cost current-result) 0.0) current-result]
          [else (iter-values split-feature-index
                             (data-rest remaining-rows)
                             (select-better-split current-result
                                                  split-cost-function
                                                  data
                                                  split-feature-index
                                                  (data-point-get-col (data-first remaining-rows)
                                                                      split-feature-index)))]))

  ;; iterates over features which might be the split feature
  (define (iter-features remaining-feature-column-indices current-result)
    (display "remaining feature column indices: ")
    (displayln remaining-feature-column-indices)
      (cond [(empty? remaining-feature-column-indices) current-result]
            [else (iter-features (cdr remaining-feature-column-indices)
                                 (iter-values (car remaining-feature-column-indices)
                                              data
                                              current-result))]))
  ;; starting the whole thing
  (iter-features feature-column-indices
                 (Split +inf.0 +inf.0 empty +inf.0)))



#|
PREDICTING:
- leaf node of the tree, majority class as prediction
|#

(define (predict-at-leaf-node leaf label-column-index)
  (define partitioning-predicate
    (lambda (data-point)
      (= (data-point-get-col data-point label-column-index) 0)))
  (let-values
      ([(part1 part2) (data-partition partitioning-predicate (Node-data leaf))])
      (cond [(>= (length part1) (length part2)) 0]
            [else 1])))

(define (fit data
             feature-column-indices
             label-column-index
             #:max-depth [max-depth 6]
             #:min-data-points [min-data-points 12]
             #:min-data-points-ratio [min-data-points-ratio 0.02])
  (define all-data-length (data-length data))
  (define current-depth 1)

  #|
  STOP CRITERIA:
  - only one class in a subset (cannot be split any further and does not need to be split)
  - maximum tree depth reached
  - minimum number of data points in a subset
  - minimum ratio of data points in this subset
  |#
  (define (all-same-label? subset)
    (labels-elements-equal? (data-get-col subset label-column-index)))

  (define (insufficient-data-points-for-split? subset)
    (let ([number-of-data-points (data-length subset)])
      (or (<= number-of-data-points min-data-points)
          (< number-of-data-points 2))))

  (define (max-depth-reached? current-depth)
    (>= current-depth max-depth))

  (define (insufficient-data-points-ratio-for-split? subset)
    (<= (/ (data-length subset) all-data-length) min-data-points-ratio))

  #|
  Here we do the recursive splitting.
  |#
  (define (recursive-split subset current-depth)
    (display "recursive split on depth: ") (displayln current-depth)
    #|
    Before splitting further, we check for stopping early conditions.
    |#
    (cond [(max-depth-reached? current-depth)
           (displayln "STOPPING CONDITION: maximum depth")
           (make-leaf-node subset)]
          [(insufficient-data-points-for-split? subset)
           (displayln "STOPPING CONDITION: insuficient number of data points")
           (make-leaf-node subset)]
          [(insufficient-data-points-ratio-for-split? subset)
           (displayln "STOPPING CONDITION: insuficient ratio of data points")
           (make-leaf-node subset)]
          [(all-same-label? subset)
           (displayln "STOPPING CONDITION: all same label")
           (make-leaf-node subset)]
          [else
           ;;(display "input data for searching best split:") (displayln subset)
           (let* ([best-split (get-best-split subset
                                              gini-index
                                              feature-column-indices
                                              label-column-index)])
             #|
             Here are the recursive calls.
             This is not tail recursive, but since the data structure itself is recursive
             and we only have as many procedure calls as there are branches in the tree,
             it is OK to not be tail recursive here.
             |#
             ;; (display "got best split subsets:")
             ;; (displayln (Split-subsets best-split))
             (Node subset
                   (Split-index best-split)
                   (Split-value best-split)
                   (recursive-split (car (Split-subsets best-split))
                                    (add1 current-depth))
                   (recursive-split (cadr (Split-subsets best-split))
                                    (add1 current-depth))))]))
  (recursive-split data 1))

;; =========================================================



(define small-data-set
  (data-range (shuffle data-set)
              0
              (exact-floor (/ (data-length data-set) 5))))

(displayln (string-append "working with "
                          (number->string (data-length data-set))
                          " data points"))
(collect-garbage)
(collect-garbage)
(collect-garbage)
(time
 (let ([a (fit data-set (list 0 1 2 3) 4)])
   (displayln "finished")))

#|
(displayln (string-append "working with "
                          (number->string (data-length small-data-set))
                          " data points"))
(collect-garbage)
(collect-garbage)
(collect-garbage)
(time
 (let ([a (fit small-data-set (list 0 1 2 3) 4)])
   (displayln "finished")))
|#

#;(let ([TEST-DATA (list #(2.771244718 1.784783929 0)
                       #(1.728571309 1.169761413 0)
                       #(3.678319846 2.81281357 0)
                       #(3.961043357 2.61995032 0)
                       #(2.999208922 2.209014212 0)
                       #(7.497545867 3.162953546 1)
                       #(9.00220326 3.339047188 1)
                       #(7.444542326 0.476683375 1)
                       #(10.12493903 3.234550982 1)
                       #(6.642287351 3.319983761 1))])
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time
   (let ([a (fit TEST-DATA
                 (list 0 1)
                 2
                 #:min-data-points 2
                 #:min-data-points-ratio 0.01
                 #:max-depth 3)])
     (displayln "finished"))))

#|
Improvements to do:
- Memoization:
  If I'm reading this right, for a given data set, you should be able
  to memoize calls to `data-get-col`.
  (remember the columns, so that they don't need to be calculated again!)
|#
