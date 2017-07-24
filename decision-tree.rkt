#lang racket

#|
Implementation adapted from:
http://machinelearningmastery.com/implement-decision-tree-algorithm-scratch-python/
|#

(require "csv-to-vector.rkt")
(provide (all-defined-out))

(define (data-empty? data)
  (= (data-length data) 0))
(define (data-first data)
  (vector-ref (vector-take data 1) 0))
(define (data-rest data)
  (vector-take-right data (- (data-length data) 1)))
(define (data-length a-data-set)
  (vector-length a-data-set))
(define (data-point-length a-data-set)
  (vector-length a-data-set))
(define (data-filter predicate a-data-set)
  (vector-filter predicate a-data-set))
(define (data-map procedure a-data-set)
  (vector-map procedure a-data-set))
(define (data-get-col data col-index)
  (data-map (lambda (data-point)
              (data-point-get-col data-point col-index))
            data))
(define (data-get-row data row-index)
  (vector-ref data row-index))

(define (class-equals? class-1 class-2)
  (= class-1 class-2))

(define (data-point-get-col data-point col-index)
  (vector-ref data-point col-index))

(define FILE-PATH "data_banknote_authentication.csv")
(define COLUMN-CONVERTERS (list string->number
                                string->number
                                string->number
                                string->number
                                (lambda (a-class)
                                  (inexact->exact
                                   (string->number a-class)))))

(define data-set (all-rows FILE-PATH
                           #:column-converters COLUMN-CONVERTERS))

(define FEATURE-COLUMN-INDICES (range (- (data-point-length (data-get-row data-set 0)) 1)))
(define LABEL-COLUMN-INDEX (- (data-point-length (data-get-row data-set 0)) 1))



(define (calc-proportion subset class-label label-column-index)
  (cond [(data-empty? subset) 0]
        [else (let* ([row-count (data-length subset)]
                     [class-count
                      (data-length
                       (data-filter (lambda (row)
                                      (class-equals? class-label
                                                     (data-point-get-col row
                                                                         label-column-index)))
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
                        (map
                         (lambda (class-label)
                           (calc-proportion subset class-label label-column-index))
                         class-labels)))
              subsets)))

(define (split-data data index value)
  (list (data-filter (lambda (data-point)
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
    (let* ([class-labels (remove-duplicates (vector->list (data-get-col data label-column-index)))]
           [new-split (split-data data split-feature-index split-value)]
           [new-split-cost (split-cost-function new-split class-labels label-column-index)])
      (cond [(< new-split-cost (hash-ref earlier-best-result 'cost))
             (hash 'index split-feature-index
                   'value split-value
                   'subsets new-split
                   'cost new-split-cost)]
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
                 (hash 'index +inf.0
                       'value +inf.0
                       'subsets empty
                       'cost +inf.0)))

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


(time (get-best-split data-set gini-index (list 0 1 2 3) 4))
#;(time (get-best-split TEST-DATA gini-index (list 0 1) 2))

#|
Improvements to do:
- struct instead of hashes for splits
- list of vectors instead of vector of vectors
- vector-take-right is expensive
- `split-data` could partition the data list in a single pass, instead
of making two passes. (You can use the `partition` function from
racket/list.)
- If I'm reading this right, for a given data set, you should be able
to memoize calls to `data-get-col`. (remember the columns, so that they don't need to be calculated again!)
-
|#
