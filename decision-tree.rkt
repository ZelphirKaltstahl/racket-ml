#lang racket

(require "csv-to-list.rkt"
         "utils.rkt"
         "data-representation-abstraction.rkt")
(provide (all-defined-out))

(define FILE-PATH "data_banknote_authentication.csv")
(define COLUMN-CONVERTERS (list string->number
                                string->number
                                string->number
                                string->number
                                (lambda (a-class) (inexact->exact (string->number a-class)))))
(define data-set (all-rows FILE-PATH #:column-converters COLUMN-CONVERTERS))
(define dev-data-set (list #(2.771244718 1.784783929 0)
                           #(1.728571309 1.169761413 0)
                           #(3.678319846 2.81281357 0)
                           #(3.961043357 2.61995032 0)
                           #(2.999208922 2.209014212 0)
                           #(7.497545867 3.162953546 1)
                           #(9.00220326 3.339047188 1)
                           #(7.444542326 0.476683375 1)
                           #(10.12493903 3.234550982 1)
                           #(6.642287351 3.319983761 1)))

;; ===============
;; DATA STRUCTURES
;; ===============
(struct Split (index value subsets cost)
  #:transparent)

(struct Node (data split-feature-index split-value left right)
  #:transparent)

(define (make-leaf-node data)
  (Node data
        'none
        'none
        empty
        empty))

(define (leaf-node? node)
  (and (data-empty? (Node-left node))
       (data-empty? (Node-right node))))

(define (node-majority-prediction node label-column-index)
  (data-majority-prediction (Node-data node) label-column-index))

;; =======================
;; DECISION TREE ALGORITHM
;; =======================
(define (calc-proportion subset class-label label-column-index)

  (define (get-class-counter a-class-label)
    (lambda (row)
      (= a-class-label
         (data-point-get-col row label-column-index))))

  (cond [(data-empty? subset) 0]
        [else (let* ([row-count (data-length subset)]
                     [class-count (count (get-class-counter class-label) subset)]
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

(define (get-best-split data feature-column-indices label-column-index)

  (define-values (col-index value subsets cost)
    (for*/fold ([previous-best-index +inf.0]
                [previous-best-value +inf.0]
                [previous-best-subsets empty]
                [previous-best-cost +inf.0])  ; initial values / previous values
    ([col-index (in-range (sub1 (vector-length (data-first data))))]
     [value (in-list (data-get-col data col-index))])
    (let* ([current-value value]
           [current-index col-index]
           [current-subsets (split-data data col-index current-value)]
           [current-cost (gini-index current-subsets label-column-index)])
      (if (< current-cost previous-best-cost)
          (values current-index
                  current-value
                  current-subsets
                  current-cost)
          (values previous-best-index
                  previous-best-value
                  previous-best-subsets
                  previous-best-cost)))))
  (Split col-index value subsets cost)

  #|
  (define (select-better-split earlier-best-split data split-feature-index split-value)
    (let* ([new-split-subsets (split-data data split-feature-index split-value)]
           [new-split-cost (gini-index new-split-subsets label-column-index)])
      (cond [(< new-split-cost (Split-cost earlier-best-split))
             (display "new best split cost: ") (displayln new-split-cost)
             (Split split-feature-index split-value new-split-subsets new-split-cost)]
            [else earlier-best-split])))



  ;; iterates over values of one feature, to find the best split value
  (define (iter-values split-feature-index remaining-rows current-result)
    (cond
      [(data-empty? remaining-rows) current-result]
      [(= (Split-cost current-result) 0.0) current-result]
      [else
       (let ([better-split
              (select-better-split current-result
                                   data
                                   split-feature-index
                                   (data-point-get-col (data-first remaining-rows)
                                                       split-feature-index))])
         (iter-values split-feature-index
                      (data-rest remaining-rows)
                      better-split))]))

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
                 (Split +inf.0 +inf.0 empty +inf.0))
  |#
  )



#|
PREDICTING:
- leaf node of the tree, majority class as prediction
|#

(define (predict-at-leaf-node leaf label-column-index)
  (node-majority-prediction leaf label-column-index))

(define (fit #:train-data data
             #:feature-column-indices feature-column-indices
             #:label-column-index label-column-index
             #:max-depth [max-depth 6]
             #:min-data-points [min-data-points 12]
             #:min-data-points-ratio [min-data-points-ratio 0.02]
             #:min-impurity-split [min-impurity-split (expt 10 -7)]
             #:stop-at-no-impurity-improvement [stop-at-no-impurity-improvement true])
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

  (define (no-improvement? previous-split-impurity split-impurity)
    (and (<= previous-split-impurity split-impurity)
         stop-at-no-impurity-improvement))

  (define (insufficient-impurity? impurity)
    (< impurity min-impurity-split))
  #|
  Here we do the recursive splitting.
  |#
  (define (recursive-split subset current-depth previous-split-impurity)
    (display "recursive split on depth: ") (displayln current-depth)
    #|
    Before splitting further, we check for stopping early conditions.
    |#
    (cond
      [(max-depth-reached? current-depth)
       (displayln "STOPPING CONDITION: maximum depth")
       (displayln (string-append "INFO: still got "
                                 (number->string (data-length subset))
                                 " data points"))
       (make-leaf-node subset)]
      [(insufficient-data-points-for-split? subset)
       (displayln "STOPPING CONDITION: insuficient number of data points")
       (displayln (string-append "INFO: still got "
                                 (number->string (data-length subset))
                                 " data points"))
       (make-leaf-node subset)]
      [(insufficient-data-points-ratio-for-split? subset)
       (displayln "STOPPING CONDITION: insuficient ratio of data points")
       (displayln (string-append "INFO: still got "
                                 (number->string (data-length subset))
                                 " data points"))
       (make-leaf-node subset)]
      [(all-same-label? subset)
       (displayln "STOPPING CONDITION: all same label")
       (displayln (string-append "INFO: still got "
                                 (number->string (data-length subset))
                                 " data points"))
       (make-leaf-node subset)]
      [else
       (displayln (string-append "INFO: CONTINUING SPLITT: still got "
                                 (number->string (data-length subset))
                                 " data points"))
       ;; (display "input data for searching best split:") (displayln subset)
       (let* ([best-split (get-best-split subset
                                          feature-column-indices
                                          label-column-index)])
         (cond
           [(no-improvement? previous-split-impurity (Split-cost best-split))
            (displayln (string-append "STOPPING CONDITION: "
                                      "no improvement in impurity: previously: "
                                      (number->string previous-split-impurity) " "
                                      "now: "
                                      (number->string (Split-cost best-split))))
            (make-leaf-node subset)]
           [(insufficient-impurity? previous-split-impurity)
            (displayln "STOPPING CONDITION: not enough impurity for splitting further")
            (make-leaf-node subset)]
           [else
            #|
            Here are the recursive calls.
            This is not tail recursive, but since the data structure itself is recursive
            and we only have as many procedure calls as there are branches in the tree,
            it is OK to not be tail recursive here.
            |#
            (Node subset
                  (Split-index best-split)
                  (Split-value best-split)
                  (recursive-split (car (Split-subsets best-split))
                                   (add1 current-depth)
                                   (Split-cost best-split))
                  (recursive-split (cadr (Split-subsets best-split))
                                   (add1 current-depth)
                                   (Split-cost best-split)))]))]))
  (recursive-split data 1 1.0))

(define (predict tree data-point label-column-index)
  #;(displayln tree)
  (cond [(leaf-node? tree)
         (node-majority-prediction tree label-column-index)]
        [else
         (cond [(< (data-point-get-col data-point (Node-split-feature-index tree))
                   (Node-split-value tree))
                (predict (Node-left tree) data-point label-column-index)]
               [else (predict (Node-right tree) data-point label-column-index)])]))

(define (cross-validation-split data-set n-folds #:random-state [random-state false])
  (if random-state
      (random-seed random-state)
      (void))
  (let* ([shuffled-data-set (shuffle data-set)]
         [number-of-data-points (data-length shuffled-data-set)]
         [fold-size (exact-floor (/ number-of-data-points n-folds))])
    (split-into-chunks-of-size-n shuffled-data-set
                                 (exact-ceiling (/ number-of-data-points n-folds)))))

(define (accuracy-metric actual-labels predicted-labels)
  (let ([correct-count (for/sum ([actual-label (in-list actual-labels)]
                                 [predicted-label (in-list predicted-labels)])
                         (if (= actual-label predicted-label) 1 0))]
        [total-count (length actual-labels)])
    (* (/ correct-count total-count) 100.0)))

(define (leave-one-out-k-folds folds left-out-fold)
  (define leave-one-out-filter-procedure
    (lambda (fold)
      (not (equal? fold left-out-fold))))
  (filter leave-one-out-filter-procedure
          folds))

(define (get-predictions tree data-set label-column-index)
  (for/list ([data-point data-set])
    (predict tree data-point label-column-index)))

;; evaluates the algorithm using cross validation split with n folds
(define (evaluate-algorithm #:data-set data-set
                            #:n-folds n-folds
                            #:feature-column-indices feature-column-indices
                            #:label-column-index label-column-index
                            #:max-depth [max-depth 6]
                            #:min-data-points [min-data-points 12]
                            #:min-data-points-ratio [min-data-points-ratio 0.02]
                            #:min-impurity-split [min-impurity-split (expt 10 -7)]
                            #:stop-at-no-impurity-improvement [stop-at-no-impurity-improvement true]
                            #:random-state [random-state false])
  (let ([folds (cross-validation-split data-set
                                       n-folds
                                       #:random-state random-state)])
    (for/list ([fold folds])
      (let* ([train-set (foldr append empty (leave-one-out-k-folds folds fold))]
             [test-set (map (lambda (data-point)
                              (data-point-take-features data-point
                                                        label-column-index))
                            fold)]
             [actual-labels (data-get-col fold label-column-index)]
             [tree (fit #:train-data train-set
                        #:feature-column-indices feature-column-indices
                        #:label-column-index label-column-index
                        #:max-depth max-depth
                        #:min-data-points min-data-points
                        #:min-data-points-ratio min-data-points-ratio
                        #:min-impurity-split min-impurity-split
                        #:stop-at-no-impurity-improvement stop-at-no-impurity-improvement)]
             [predicted-labels (get-predictions tree test-set label-column-index)])
        #;(print-tree tree label-column-index)
        (accuracy-metric actual-labels predicted-labels)))))

;; displays a string representation of a learned decision tree
(define (print-tree tree label-column-index)
  (define (tree->string tree depth)
    (cond [(leaf-node? tree)
           (string-append (n-times-string "  " depth)
                          "["
                          (number->string
                           (node-majority-prediction tree label-column-index))
                          "]\n")]
          [else
           (string-append
            (string-append (n-times-string "  " depth)
                           "[feature:"
                           (number->string (Node-split-feature-index tree))
                           " < "
                           (number->string (Node-split-value tree))
                           "]\n")
            (tree->string (Node-left tree) (add1 depth))
            (tree->string (Node-right tree) (add1 depth)))]))
  (displayln (tree->string tree 0)))

;; =========================================================
;; PRUNING
;; =========================================================
(define (count-leaves tree)
  (cond [(leaf-node? tree) 1]
        [else (+ (count-leaves (Node-left tree))
                 (count-leaves (Node-right tree)))]))

#|
- remove split with the least improvement in impurity / cost
  - to achieve max num of leaves
  - save the split cost inside a node?
  - recalculate the gini-index from the left and right of a node (parent) which produced this leaf.
  - recalculate the gini-index of the leaf.
  - compare costs and see if they are lower than x.
  - inner (cost?) -> inner (cost?) -> leaf
    (parent)         (child)       -> leaf
                     inner (cost?) -> leaf
                     (child)       -> leaf
- remove all splits with less improvement than x in cost?
  - but this can be done already with early stopping parameters!
-
|#

;; =========================================================
;; RUNNING
;; =========================================================


(define shuffled-data-set (shuffle data-set))

(define small-data-set
  (data-range shuffled-data-set
              0
              (exact-floor (/ (data-length shuffled-data-set)
                              5))))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(time
 (for/list ([i (in-range 1)])
   (mean
    (evaluate-algorithm #:data-set (shuffle data-set)
                        #:n-folds 10
                        #:feature-column-indices (list 0 1 2 3)
                        #:label-column-index 4
                        #:max-depth 5
                        #:min-data-points 24
                        #:min-data-points-ratio 0.02
                        #:min-impurity-split (expt 10 -7)
                        #:stop-at-no-impurity-improvement true
                        #:random-state 0))))
(collect-garbage)
(collect-garbage)
(collect-garbage)
#;(time
 (for/list ([i (in-range 1)])
   (define tree (fit #:train-data (shuffle data-set)
                     #:feature-column-indices (list 0 1 2 3)
                     #:label-column-index 4
                     #:max-depth 5
                     #:min-data-points 12
                     #:min-data-points-ratio 0.02
                     #:min-impurity-split (expt 10 -7)
                     #:stop-at-no-impurity-improvement true))
   'done))

#|
IMPROVEMENTS:
- remove data from not leaf nodes by using struct setters
- find the remaining randomness (if there is any) which is not determined by random-state keyword arguments yet (why am I not getting the same result every time?) - maybe shuffle needs to be parametrized with a random seed instead of merely setting the seed before calling shuffle?
- return not only the predicted label, but also how sure we are about the prediction (percentage of data points in the leaf node, which has the predicted label)
|#
