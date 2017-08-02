#lang racket


(require rackunit)
(require "decision-tree.rkt"
         "test-utils.rkt"
         "data-representation-abstraction.rkt")

(define TEST-DATA (list #(2.771244718 1.784783929 0)
                        #(1.728571309 1.169761413 0)
                        #(3.678319846 2.81281357 0)
                        #(3.961043357 2.61995032 0)
                        #(2.999208922 2.209014212 0)
                        #(7.497545867 3.162953546 1)
                        #(9.00220326 3.339047188 1)
                        #(7.444542326 0.476683375 1)
                        #(10.12493903 3.234550982 1)
                        #(6.642287351 3.319983761 1)))
(define PRECISION (expt 10 -9))

(test-case "data-empty? test case"
  (check-true (data-empty? empty)
              "data-empty? is not correct")
  (check-false (data-empty? (list 1 2 3))
               "data-empty? is not correct"))

(test-case "data-first test case"
  (check-equal? (data-first (list #(0 0 0)
                                  #(1 1 1)))
                #(0 0 0)
                "data-first is not correct"))

(test-case "data-rest test case"
  (check-equal? (data-rest (list #(0 0 0)
                                 #(1 1 1)))
                (list #(1 1 1))
                "data-rest is not correct")
  (check-equal? (data-rest (list #(0 0 0)
                                 #(1 1 1)
                                 #(2 2 2)
                                 #(3 3 3)))
                (list #(1 1 1)
                      #(2 2 2)
                      #(3 3 3))
                "data-rest is not correct"))

(test-case "data-length test case"
  (check-equal? (data-length (list #(0)
                                   #(1)
                                   #(2)))
                3
                "data-length is not correct")
  (check-equal? (data-length (list #(0)
                                   #(1)
                                   #(2 3)
                                   #(3 4 5)))
                4
                "data-length is not correct"))

(test-case "data-point-length test case"
  (check-equal? (data-point-length #(0.1 0.2 0.3245 123.213432 'a))
                5
                "data-point-length is not correct"))

(test-case "data-filter test-case"
  (check-equal? (data-filter (lambda (data-point)
                               (> (data-point-get-col data-point 1) 3))
                             TEST-DATA)
                (list #(7.497545867 3.162953546 1)
                      #(9.00220326 3.339047188 1)
                      #(10.12493903 3.234550982 1)
                      #(6.642287351 3.319983761 1)))
  (check-equal? (data-filter (lambda (data-point)
                               (< (data-point-get-col data-point 0) 2))
                             TEST-DATA)
                (list #(1.728571309 1.169761413 0))))

(test-case "data-map test case"
  (check-equal?
   (lists-approximately-equal?
    (data-map (lambda (data-point)
                (vector-set data-point 0
                            (+ (vector-ref data-point 0) 1.0)))
              TEST-DATA)
    (list #(3.771244718 1.784783929 0)
          #(2.728571309 1.169761413 0)
          #(4.678319846 2.81281357 0)
          #(4.961043357 2.61995032 0)
          #(3.999208922 2.209014212 0)
          #(8.497545867 3.162953546 1)
          #(10.00220326 3.339047188 1)
          #(8.444542326 0.476683375 1)
          #(11.12493903 3.234550982 1)
          #(7.642287351 3.319983761 1))
    PRECISION)
   true))

(test-case "data-get-col test case"
  (check-equal? (data-get-col (list #(123 43 1)
                                    #(2132435 23 2)
                                    #(1244 769 3))
                              2)
                (list 1 2 3)))

(test-case "data-point-get-col"
  (check-equal? (data-point-get-col #(0 32 478 282 1) 1)
                32)
  (check-equal? (data-point-get-col #(0 32 478 282 1) 0)
                0))

(test-case "gini-index test case"
  (check-equal? (gini-index (list
                             (list #(1.1 2.2 3.3 4.4 0)
                                   #(1.1 2.2 3.3 4.4 0))
                             (list #(1.1 2.2 3.3 4.4 1)
                                   #(1.1 2.2 3.3 4.4 1)))
                            4)
                0.0
                "gini index of perfect split is not 0.0")
  (check-equal? (gini-index (list
                             (list #(1.1 2.2 3.3 4.4 0)
                                   #(1.1 2.2 3.3 4.4 1))
                             (list #(1.1 2.2 3.3 4.4 0)
                                   #(1.1 2.2 3.3 4.4 1)))
                            4)
                1.0
                "gini index of worst split is not 1.0")
  (check-= (gini-index (list
                        (list #(1.1 2.2 3.3 4.4 0)
                              #(1.1 2.2 3.3 4.4 1)
                              #(1.1 2.2 3.3 4.4 1))
                        (list #(1.1 2.2 3.3 4.4 0)
                              #(1.1 2.2 3.3 4.4 0)
                              #(1.1 2.2 3.3 4.4 1)))
                       4)
           0.888888888
           PRECISION
           (string-append "gini index of split is not "
                          (number->string 0.888888888))))

(test-case
    "split-data test case"
  (check-equal? (split-data (list #(1.0 1.0 1.0 1.0 0)
                                  #(1.2 1.0 1.0 1.0 0)
                                  #(1.4 1.0 1.0 1.0 0)
                                  #(1.6 1.0 1.0 1.0 0)
                                  #(1.8 1.0 1.0 1.0 0)
                                  #(2.0 1.0 1.0 1.0 0))
                            0
                            1.5)
                (list (list #(1.0 1.0 1.0 1.0 0)
                            #(1.2 1.0 1.0 1.0 0)
                            #(1.4 1.0 1.0 1.0 0))
                      (list #(1.6 1.0 1.0 1.0 0)
                            #(1.8 1.0 1.0 1.0 0)
                            #(2.0 1.0 1.0 1.0 0)))
                "split-data does not split correctly")
  (check-equal? (split-data (list #(1.0 1.0 1.0 1.0 0)
                                  #(1.2 4.0 1.0 1.0 0)
                                  #(1.4 1.0 1.0 1.0 0)
                                  #(1.6 3.0 1.0 1.0 0)
                                  #(1.8 1.0 1.0 1.0 0)
                                  #(2.0 2.0 1.0 1.0 0))
                            1
                            2.5)
                (list (list #(1.0 1.0 1.0 1.0 0)
                            #(1.4 1.0 1.0 1.0 0)
                            #(1.8 1.0 1.0 1.0 0)
                            #(2.0 2.0 1.0 1.0 0))
                      (list #(1.2 4.0 1.0 1.0 0)
                            #(1.6 3.0 1.0 1.0 0)))
                "split-data does not split correctly"))

(test-case "get-best-split test case"
  (let ([test-data (list #(2.771244718 1.784783929 0)
                         #(1.728571309 1.169761413 0)
                         #(3.678319846 2.81281357 0)
                         #(3.961043357 2.61995032 0)
                         #(2.999208922 2.209014212 0)
                         #(7.497545867 3.162953546 1)
                         #(9.00220326 3.339047188 1)
                         #(7.444542326 0.476683375 1)
                         #(10.12493903 3.234550982 1)
                         #(6.642287351 3.319983761 1))]
        [feature-columns-indices (list 0 1)]
        [label-column-index 2])
    (check-equal? (get-best-split test-data
                                  feature-columns-indices
                                  label-column-index)
                  (Split 0
                         6.642287351
                         (list (list #(2.771244718 1.784783929 0)
                                     #(1.728571309 1.169761413 0)
                                     #(3.678319846 2.81281357 0)
                                     #(3.961043357 2.61995032 0)
                                     #(2.999208922 2.209014212 0))
                               (list #(7.497545867 3.162953546 1)
                                     #(9.00220326 3.339047188 1)
                                     #(7.444542326 0.476683375 1)
                                     #(10.12493903 3.234550982 1)
                                     #(6.642287351 3.319983761 1)))
                         0.0)
                  "get-best-split does not give the best split")))

(test-case "predict-at-leaf-node test case"
  (check-equal? (predict-at-leaf-node (make-leaf-node (list #(1.0 2.0 0)
                                                            #(3.0 4.0 0)
                                                            #(5.0 6.0 1)
                                                            #(7.0 8.0 1)
                                                            #(9.0 0.0 1)))
                                      2)
                1
                "predict-at-leaf-node does not give the correct label")
  (check-equal? (predict-at-leaf-node (make-leaf-node (list #(1.0 2.0 0)
                                                            #(3.0 4.0 0)
                                                            #(5.0 6.0 0)
                                                            #(7.0 8.0 1)
                                                            #(9.0 0.0 1)))
                                      2)
                0
                "predict-at-leaf-node does not give the correct label"))

(test-case "leaf-node? test case"
  (check-true (leaf-node? (make-leaf-node (list #(5.0 6.0 0)
                                                #(7.0 8.0 1))))
              "leaf-node? is not correct")
  (check-false (let* ([subset (list #(5.0 6.0 0)
                                    #(7.0 8.0 1))]
                      [best-split (get-best-split subset (list 0 1) 2)])
                 (leaf-node? (Node subset
                                   (Split-index best-split)
                                   (Split-value best-split)
                                   (make-leaf-node (list #(5.0 6.0 0)))
                                   (make-leaf-node (list #(7.0 8.0 1))))))
               "leaf-node? is not correct"))

(test-case "fit test case"
  (let ([test-data (list #(1.0 1.0 0)
                         #(1.2 1.0 0)
                         #(1.1 1.0 0)
                         #(1.4 1.0 0)
                         #(1.2 1.0 0)
                         #(1.2 1.0 0) ;;
                         #(2.3 1.0 1)
                         #(2.0 1.0 1)
                         #(2.3 1.0 1)
                         #(2.0 1.0 1)
                         #(2.3 1.0 1)
                         #(2.0 1.0 1)
                         #(2.4 1.0 1))]
        [feature-columns-indices (list 0 1)]
        [label-column-index 2])
    (check-equal? (fit #:train-data test-data
                       #:feature-column-indices (list 0 1)
                       #:label-column-index 2
                       #:max-depth 2
                       #:min-data-points 4
                       #:min-data-points-ratio 0.02)
                  (let ([best-split (get-best-split test-data(list 0 1) 2)])
                    (Node test-data
                          (Split-index best-split)
                          (Split-value best-split)
                          (make-leaf-node (list #(1.0 1.0 0)
                                                #(1.2 1.0 0)
                                                #(1.1 1.0 0)
                                                #(1.4 1.0 0)
                                                #(1.2 1.0 0)
                                                #(1.2 1.0 0)))
                          (make-leaf-node (list #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.4 1.0 1)))))
                  "split is not correct"))
  (let* ([test-data (list #(1.0 1.0 0)
                          #(1.2 1.0 0)
                          #(1.1 1.0 0)
                          #(1.4 1.0 0)
                          #(1.2 1.0 0)
                          #(1.2 1.0 0) ;;
                          #(2.3 1.1 0)
                          #(2.0 1.1 0)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.4 1.0 1))]
         [best-split (get-best-split test-data (list 0 1) 2)])
    (check-equal? (fit #:train-data test-data
                       #:feature-column-indices (list 0 1)
                       #:label-column-index 2
                       #:max-depth 3
                       #:min-data-points 2
                       #:min-data-points-ratio 0.02)
                  (Node test-data
                        (Split-index best-split)
                        (Split-value best-split)
                        (make-leaf-node (list #(1.0 1.0 0)
                                              #(1.2 1.0 0)
                                              #(1.1 1.0 0)
                                              #(1.4 1.0 0)
                                              #(1.2 1.0 0)
                                              #(1.2 1.0 0)))
                        (let* ([subset (list #(2.3 1.1 0)
                                             #(2.0 1.1 0)
                                             #(2.3 1.0 1)
                                             #(2.0 1.0 1)
                                             #(2.3 1.0 1)
                                             #(2.0 1.0 1)
                                             #(2.4 1.0 1))]
                               [best-split (get-best-split subset (list 0 1) 2)])
                          (Node subset
                                (Split-index best-split)
                                (Split-value best-split)
                                (make-leaf-node (list #(2.3 1.0 1)
                                                      #(2.0 1.0 1)
                                                      #(2.3 1.0 1)
                                                      #(2.0 1.0 1)
                                                      #(2.4 1.0 1)))
                                (make-leaf-node (list #(2.3 1.1 0)
                                                      #(2.0 1.1 0))))))
                  "split is not correct"))
  (let* ([test-data (list #(2.3 1.1 0)
                          #(2.0 1.1 0)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.4 1.0 1))]
         [best-split (get-best-split test-data (list 0 1) 2)])
    (check-equal? (fit #:train-data test-data
                       #:feature-column-indices (list 0 1)
                       #:label-column-index 2
                       #:max-depth 3
                       #:min-data-points 2
                       #:min-data-points-ratio 0.02)
                  (Node test-data
                        (Split-index best-split)
                        (Split-value best-split)
                        (make-leaf-node (list #(2.3 1.0 1)
                                              #(2.0 1.0 1)
                                              #(2.3 1.0 1)
                                              #(2.0 1.0 1)
                                              #(2.4 1.0 1)))
                        (make-leaf-node (list #(2.3 1.1 0)
                                              #(2.0 1.1 0))))
                  "split is not correct")))

(test-case "labels-elements-equal? test case"
  (check-true (labels-elements-equal? empty)
              "labels-elements-equal? is not correct")
  (check-true (labels-elements-equal? (list 1 1 1))
              "labels-elements-equal? is not correct")
  (check-true (labels-elements-equal? (data-get-col (list #(1.0 1.0 0)
                                                          #(1.2 1.0 0)
                                                          #(1.1 1.0 0)
                                                          #(1.4 1.0 0)
                                                          #(1.2 1.0 0)
                                                          #(1.2 1.0 0))
                                                    2))
              "labels-elements-equal? is not correct")
  (check-false (labels-elements-equal? (list 1 2 3))
               "labels-elements-equal? is not correct"))

(test-case "data-majority-prediction test case"
  (check-equal? (data-majority-prediction (list #(2.3 1.1 0)
                                                #(2.0 1.1 0)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.4 1.0 1))
                                          2)
                1)
  (check-equal? (data-majority-prediction (list #(2.3 1.1 0)
                                                #(2.0 1.1 0)
                                                #(2.3 1.0 0)
                                                #(2.0 1.0 0)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.4 1.0 1))
                                          2)
                0)
  (check-equal? (data-majority-prediction (list #(2.3 1.1 0)
                                                #(2.0 1.1 0)
                                                #(2.3 1.0 0)
                                                #(2.0 1.0 0)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.4 1.0 1)
                                                #(2.4 1.0 1))
                                          2)
                0))


#;(struct Node
    (data
     predicted-label
     split-feature-index
     split-value
     left right)
    #:transparent)
(test-case "node-majority-prediction test case"
  (check-equal? (node-majority-prediction
                 (Node (list #(2.3 1.1 0)
                             #(2.0 1.1 0)
                             #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.4 1.0 1))
                       1
                       1.1
                       (list #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.4 1.0 1))
                       (list #(2.3 1.1 0)
                             #(2.0 1.1 0)))
                 2)
                1)
  (check-equal? (node-majority-prediction
                 (Node (list #(2.3 1.1 1)
                             #(2.0 1.1 1)
                             #(2.3 1.0 0)
                             #(2.0 1.0 0)
                             #(2.3 1.0 0)
                             #(2.0 1.0 0)
                             #(2.4 1.0 0))
                       1
                       1.1
                       (list #(2.3 1.0 0)
                             #(2.0 1.0 0)
                             #(2.3 1.0 0)
                             #(2.0 1.0 0)
                             #(2.4 1.0 0))
                       (list #(2.3 1.1 1)
                             #(2.0 1.1 1)))
                 2)
                0))

(test-case "data-partition test case"
  (check-values-equal? (data-partition (lambda (data-point)
                                         (= (data-point-get-col data-point 2) 0))
                                       (list #(2.3 1.1 1)
                                             #(2.0 1.1 1)
                                             #(2.3 1.0 0)
                                             #(2.0 1.0 0)
                                             #(2.3 1.0 0)
                                             #(2.0 1.0 0)
                                             #(2.4 1.0 0)))
                       (list (list #(2.3 1.0 0)
                                   #(2.0 1.0 0)
                                   #(2.3 1.0 0)
                                   #(2.0 1.0 0)
                                   #(2.4 1.0 0))
                             (list #(2.3 1.1 1)
                                   #(2.0 1.1 1)))))

(test-case "predict test case"
  (let ([tree (Node (list #(1.0 1.0 0)
                          #(1.2 1.0 0)
                          #(1.1 1.0 0)
                          #(1.4 1.0 0)
                          #(1.2 1.0 0)
                          #(1.2 1.0 0) ;
                          #(2.3 1.1 0)
                          #(2.0 1.1 0) ;;
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.4 1.0 1))
                    0  ;; split index
                    2.0  ;; split value
                    (make-leaf-node (list #(1.0 1.0 0)
                                          #(1.2 1.0 0)
                                          #(1.1 1.0 0)
                                          #(1.4 1.0 0)
                                          #(1.2 1.0 0)
                                          #(1.2 1.0 0)))
                    (Node (list #(2.3 1.1 0)
                                #(2.0 1.1 0)
                                #(2.3 1.0 1)
                                #(2.0 1.0 1)
                                #(2.3 1.0 1)
                                #(2.0 1.0 1)
                                #(2.4 1.0 1))
                          1
                          1.1
                          (make-leaf-node (list #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.3 1.0 1)
                                                #(2.0 1.0 1)
                                                #(2.4 1.0 1)))
                          (make-leaf-node (list #(2.3 1.1 0)
                                                #(2.0 1.1 0)))))])
    (check-equal? (predict tree #(2.3 1.1 0) 2) 0)
    (check-equal? (predict tree #(2.3 1.0 0) 2) 1)))

(test-case "cross-validation-split"
  (check-equal? (cross-validation-split '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
                                        4
                                        #:random-state 12345)
                (list (list 12 3 10 0 13)
                      (list 6 9 7 19 8)
                      (list 18 15 14 5 16)
                      (list 1 4 2 11 17))))

(test-case "accuracy-metric"
  (check-equal? (accuracy-metric (list 0 0 0 0)
                                 (list 1 0 0 0))
                75.0)
  (check-equal? (accuracy-metric (list 0 0 0 0)
                                 (list 1 1 0 0))
                50.0)
  (check-equal? (accuracy-metric (list 1 1 1 1)
                                 (list 1 0 0 0))
                25.0)
  (check-equal? (accuracy-metric (list 0 0 0 0)
                                 (list 0 0 0 0))
                100.0)
  (check-equal? (accuracy-metric (list 0 0 0)
                                 (list 1 0 0))
                (* (/ 2.0 3) 100)))

(test-case "leave-one-out-k-folds"
  (check-equal? (leave-one-out-k-folds (list (list #(1 1)
                                                   #(1 1)
                                                   #(1 1)
                                                   #(1 1))
                                             (list #(2 2)
                                                   #(2 2)
                                                   #(2 2)
                                                   #(2 2))
                                             (list #(3 3)
                                                   #(3 3)
                                                   #(3 3)
                                                   #(3 3))
                                             (list #(4 4)
                                                   #(4 4)
                                                   #(4 4)
                                                   #(4 4)))
                                       (list #(3 3)
                                             #(3 3)
                                             #(3 3)
                                             #(3 3)))
                (list (list #(1 1)
                            #(1 1)
                            #(1 1)
                            #(1 1))
                      (list #(2 2)
                            #(2 2)
                            #(2 2)
                            #(2 2))
                      (list #(4 4)
                            #(4 4)
                            #(4 4)
                            #(4 4)))))

(test-case "data-point-take-features"
  (check-equal? (data-point-take-features #(0 1 2 3 4) 4)
                #(0 1 2 3))
  (check-equal? (data-point-take-features #(10 9 8 4) 3)
                #(10 9 8)))

(test-case "get-predictions"
  (let ([tree (Node (list #(2.3 1.1 0)
                          #(2.0 1.1 0)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.3 1.0 1)
                          #(2.0 1.0 1)
                          #(2.4 1.0 1))
                    1
                    1.1
                    (make-leaf-node (list #(2.3 1.0 1)
                                          #(2.0 1.0 1)
                                          #(2.3 1.0 1)
                                          #(2.0 1.0 1)
                                          #(2.4 1.0 1)))
                    (make-leaf-node (list #(2.3 1.1 0)
                                          #(2.0 1.1 0))))])
    (check-equal? (get-predictions tree (list #(2.4 1.2)
                                              #(1.9 0.9)
                                              #(3.0 3.0)
                                              #(0.0 0.5))
                                   2)
                  (list 0 1 0 1))))

(test-case "evaluate-algorithm"
  (check-equal? (length (evaluate-algorithm #:data-set (shuffle TEST-DATA)
                                            #:n-folds 4
                                            #:feature-column-indices (list 0 1 2 3)
                                            #:label-column-index 2
                                            #:max-depth 3
                                            #:min-data-points 4
                                            #:min-data-points-ratio 0.02
                                            #:min-impurity-split (expt 10 -7)
                                            #:stop-at-no-impurity-improvement true
                                            #:random-state 0))
                4)
  ;; TODO: real test cose
  )

(test-case "count-leaves"
  (check-equal?
   (count-leaves (Node (list #(2.3 1.1 0)
                             #(2.0 1.1 0)
                             #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.3 1.0 1)
                             #(2.0 1.0 1)
                             #(2.4 1.0 1))
                       1
                       1.1
                       (make-leaf-node (list #(2.3 1.0 1)
                                             #(2.0 1.0 1)
                                             #(2.3 1.0 1)
                                             #(2.0 1.0 1)
                                             #(2.4 1.0 1)))
                       (make-leaf-node (list #(2.3 1.1 0)
                                             #(2.0 1.1 0)))))
   2)
  (check-equal?
   (count-leaves (Node (list #(2.3 1.1 3.0 0)
                             #(2.0 1.1 3.0 0)
                             #(2.3 1.0 4.0 0)
                             #(2.0 1.0 3.0 1)
                             #(2.3 1.0 3.0 1)
                             #(2.0 1.0 3.0 1)
                             #(2.4 1.0 3.0 1))
                       1
                       1.1
                       (Node (list #(2.3 1.0 4.0 0)
                                   #(2.0 1.0 3.0 1)
                                   #(2.3 1.0 3.0 1)
                                   #(2.0 1.0 3.0 1)
                                   #(2.4 1.0 3.0 1))
                             2
                             4.0
                             (make-leaf-node (list #(2.0 1.0 3.0 1)
                                                   #(2.3 1.0 3.0 1)
                                                   #(2.0 1.0 3.0 1)
                                                   #(2.4 1.0 3.0 1)))
                             (make-leaf-node (list #(2.3 1.0 4.0 0))))
                       (make-leaf-node (list #(2.3 1.1 3.0 0)
                                             #(2.0 1.1 3.0 0)))))
   3))
