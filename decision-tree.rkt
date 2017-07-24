#lang racket

#|
Implementation adapted from:
http://machinelearningmastery.com/implement-decision-tree-algorithm-scratch-python/
|#

(require "csv-to-list.rkt")
(provide (all-defined-out))

;; =========================================================
;; ABSTRACTION LAYER
;; (for data representation)
;; =========================================================
(define (data-empty? data)
  (empty? data))
(define (data-first data)
  (first data))
(define (data-rest data)
  (rest data))
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
  (index value subsets cost)
  #:transparent)
;; =========================================================

(define (class-equals? class-1 class-2)
  (= class-1 class-2))

(define FILE-PATH "data_banknote_authentication.csv")
(define COLUMN-CONVERTERS (list string->number
                                string->number
                                string->number
                                string->number
                                (lambda (a-class) (inexact->exact (string->number a-class)))))
(define data-set (all-rows FILE-PATH #:column-converters COLUMN-CONVERTERS))

#;(define FEATURE-COLUMN-INDICES
  (range (- (data-point-length (data-first data-set)) 1)))
#;(define LABEL-COLUMN-INDEX
  (- (data-point-length (data-first data-set)) 1))


;; =========================================================
;; DECISION TREE ALGORITHM
;; (its procedures)
;; =========================================================
(define (calc-proportion subset class-label label-column-index)
  (cond [(data-empty? subset) 0]
        [else (let* ([row-count (data-length subset)]
                     [class-count
                      (data-length
                       (data-filter (lambda (row)
                                      (class-equals? class-label
                                                     (data-point-get-col row label-column-index)))
                                    subset))]
                     [prop (/ class-count row-count)])
                (* prop (- 1.0 prop)))]))

#|
The procedure gini-index is used to evaluate the quality of a split.
It is a cost function for a split.
We want to keep the costs for splits low. (also: greedy)
There are other ways of calculating the quality of a split, but for now we
implement gini index.
|#
(define (gini-index subsets class-labels label-column-index)
  (apply +
         (map (lambda (subset)
                (apply +
                        (map (lambda (class-label)
                               (calc-proportion subset class-label label-column-index))
                             class-labels)))
              subsets)))

(define (split-data data index value)
  (let-values ([(part1 part2)
                (data-partition (lambda (data-point)
                                  (< (data-point-get-col data-point index) value))
                                data)])
    (list part1 part2))
  #;(list (data-filter (lambda (data-point)
                       (< (data-point-get-col data-point index) value))
                     data)
        (data-filter (lambda (data-point)
                       (>= (data-point-get-col data-point index) value))
                     data)))

(define (get-best-split data
                        split-cost-function
                        feature-column-indices
                        label-column-index)
  (define (select-better-split earlier-best-result
                               split-cost-function
                               data
                               split-feature-index
                               split-value)
    (let* ([class-labels (remove-duplicates (data-get-col data label-column-index))]
           [new-split-subsets (split-data data split-feature-index split-value)]
           [new-split-cost (split-cost-function new-split-subsets
                                                class-labels
                                                label-column-index)])
      (cond [(< new-split-cost (Split-cost earlier-best-result))
             (Split split-feature-index
                    split-value
                    new-split-subsets
                    new-split-cost)]
            [else earlier-best-result])))

  ;; iterates over values of one feature, to find the best split value
  (define (iter-values split-feature-index remaining-rows current-result)
    ;;(display "remaining rows: ") (display (data-length remaining-rows)) (newline)
    (cond [(data-empty? remaining-rows) current-result]
          [else (iter-values split-feature-index
                             (data-rest remaining-rows)
                             (select-better-split current-result
                                                  split-cost-function
                                                  data
                                                  split-feature-index
                                                  (data-point-get-col (data-first remaining-rows)
                                                                      split-feature-index)))]))

  ;; iterates over features which might be the split feature
  (define (iter-features remaining-feature-column-indices
                         current-result)
    (display "remaining feature column indices: ")
    (display remaining-feature-column-indices)
    (newline)
    (cond [(empty? remaining-feature-column-indices) current-result]
          [else (iter-features (rest remaining-feature-column-indices)
                               (iter-values (first remaining-feature-column-indices)
                                            data
                                            current-result))]))
  ;; starting the whole thing
  (iter-features feature-column-indices
                 (Split +inf.0 +inf.0 empty +inf.0)))

;; =========================================================

(define TEST-DATA #(#(2.771244718 1.784783929 0)
                    #(1.728571309 1.169761413 0)
                    #(3.678319846 2.81281357 0)
                    #(3.961043357 2.61995032 0)
                    #(2.999208922 2.209014212 0)
                    #(7.497545867 3.162953546 1)
                    #(9.00220326 3.339047188 1)
                    #(7.444542326 0.476683375 1)
                    #(10.12493903 3.234550982 1)
                    #(6.642287351 3.319983761 1)))

#;(time (get-best-split data-set gini-index (list 0 1 2 3) 4))
#;(time (get-best-split TEST-DATA gini-index (list 0 1) 2))

#|
Improvements to do:
- struct instead of hashes for splits
- Memoization:
  If I'm reading this right, for a given data set, you should be able
  to memoize calls to `data-get-col`.
  (remember the columns, so that they don't need to be calculated again!)
|#
