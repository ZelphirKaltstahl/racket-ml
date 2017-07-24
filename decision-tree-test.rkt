#lang racket


(require rackunit)
(require "decision-tree.rkt")

(define (vector-set v i o)
  (vector->immutable-vector
   (for/vector ([j (in-range (vector-length v))])
     (if (= i j)
         o
         (vector-ref v j)))))

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
(define PRECISION (expt 10 -9))

(test-case "data-empty? test case"
  (check-true (data-empty? #())
              "data-empty? is not correct"))

(test-case "data-first test case"
  (check-equal? (data-first #(#(0 0 0)
                              #(1 1 1)))
                #(0 0 0)
                "data-first is not correct"))

(test-case "data-rest test case"
  (check-equal? (data-rest #(#(0 0 0)
                             #(1 1 1)))
                #(#(1 1 1))
                "data-rest is not correct")
  (check-equal? (data-rest #(#(0 0 0)
                             #(1 1 1)
                             #(2 2 2)
                             #(3 3 3)))
                #(#(1 1 1)
                  #(2 2 2)
                  #(3 3 3))
                "data-rest is not correct"))

(test-case "data-length test case"
  (check-equal? (data-length #(#(0)
                               #(1)
                               #(2)))
                3
                "data-length is not correct")
  (check-equal? (data-length #(#(0)
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
                #(#(7.497545867 3.162953546 1)
                  #(9.00220326 3.339047188 1)
                  #(10.12493903 3.234550982 1)
                  #(6.642287351 3.319983761 1)))
  (check-equal? (data-filter (lambda (data-point)
                               (< (data-point-get-col data-point 0) 2))
                             TEST-DATA)
                #(#(1.728571309 1.169761413 0))))


(define (vectors-approximately-equal? v1 v2 epsilon)
  (define (approximately-equal? value1 value2 epsilon)
    (<= (abs (- (abs value1) (abs value2))) epsilon))

  (define (check-elements v1 v2 epsilon)
    (cond [(and (= (vector-length v1) 0) (= (vector-length v2) 0)) true]
          [(approximately-equal? (vector-ref (vector-ref (vector-take v1 1) 0) 0)
                                 (vector-ref (vector-ref (vector-take v2 1) 0) 0)
                                 epsilon)
           (check-elements (vector-take-right v1 (- (vector-length v1) 1))
                           (vector-take-right v2 (- (vector-length v2) 1))
                           epsilon)]
          [else false]))

  (if (= (vector-length v1) (vector-length v2))
      (check-elements v1 v2 epsilon)
      false))

(test-case "data-map test case"
  (check-equal?
   (vectors-approximately-equal?
    (data-map (lambda (data-point)
                (let ([a-copy (vector-copy data-point)])
                  (vector-set a-copy
                              0
                              (+ (vector-ref data-point 0) 1.0))))
              TEST-DATA)
    #(#(3.771244718 1.784783929 0)
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
  (check-equal? (data-get-col #(#(123 43 1)
                                #(2132435 23 2)
                                #(1244 769 3))
                              2)
                #(1 2 3)))

(test-case "data-get-row test case"
  (check-equal? (data-get-row #(#(123 43 1)
                                #(2132435 23 2)
                                #(1244 769 3))
                              2)
                #(1244 769 3)))

(test-case "class-equals? test case"
  (check-true (class-equals? 1 1))
  (check-true (class-equals? 0 0))
  (check-true (class-equals? 2 2))
  (check-false (class-equals? 1 0))
  (check-false (class-equals? 2 0))
  (check-false (class-equals? 1 2)))

(test-case "data-point-get-col"
  (check-equal? (data-point-get-col #(0 32 478 282 1) 1)
                32)
  (check-equal? (data-point-get-col #(0 32 478 282 1) 0)
                0))

(test-case "gini-index test case"
  (check-equal? (gini-index (list
                             #(#(1.1 2.2 3.3 4.4 0)
                               #(1.1 2.2 3.3 4.4 0))
                             #(#(1.1 2.2 3.3 4.4 1)
                               #(1.1 2.2 3.3 4.4 1)))
                            (list 0 1)
                            4)
                0.0
                "gini index of perfect split is not 0.0")
  (check-equal? (gini-index (list
                             #(#(1.1 2.2 3.3 4.4 0)
                               #(1.1 2.2 3.3 4.4 1))
                             #(#(1.1 2.2 3.3 4.4 0)
                               #(1.1 2.2 3.3 4.4 1)))
                            (list 0 1)
                            4)
                1.0
                "gini index of worst split is not 1.0")
  (check-= (gini-index (list
                        #(#(1.1 2.2 3.3 4.4 0)
                          #(1.1 2.2 3.3 4.4 1)
                          #(1.1 2.2 3.3 4.4 1))
                        #(#(1.1 2.2 3.3 4.4 0)
                          #(1.1 2.2 3.3 4.4 0)
                          #(1.1 2.2 3.3 4.4 1)))
                       (list 0 1)
                       4)
           0.888888888
           PRECISION
           (string-append "gini index of split is not "
                          (number->string 0.888888888))))

(test-case
    "split-data test case"
  (check-equal? (split-data #(#(1.0 1.0 1.0 1.0 0)
                              #(1.2 1.0 1.0 1.0 0)
                              #(1.4 1.0 1.0 1.0 0)
                              #(1.6 1.0 1.0 1.0 0)
                              #(1.8 1.0 1.0 1.0 0)
                              #(2.0 1.0 1.0 1.0 0))
                            0
                            1.5)
                (list #(#(1.0 1.0 1.0 1.0 0)
                        #(1.2 1.0 1.0 1.0 0)
                        #(1.4 1.0 1.0 1.0 0))
                      #(#(1.6 1.0 1.0 1.0 0)
                        #(1.8 1.0 1.0 1.0 0)
                        #(2.0 1.0 1.0 1.0 0)))
                "split-data does not split correctly")
  (check-equal? (split-data #(#(1.0 1.0 1.0 1.0 0)
                              #(1.2 4.0 1.0 1.0 0)
                              #(1.4 1.0 1.0 1.0 0)
                              #(1.6 3.0 1.0 1.0 0)
                              #(1.8 1.0 1.0 1.0 0)
                              #(2.0 2.0 1.0 1.0 0))
                            1
                            2.5)
                (list #(#(1.0 1.0 1.0 1.0 0)
                        #(1.4 1.0 1.0 1.0 0)
                        #(1.8 1.0 1.0 1.0 0)
                        #(2.0 2.0 1.0 1.0 0))
                      #(#(1.2 4.0 1.0 1.0 0)
                        #(1.6 3.0 1.0 1.0 0)))
                "split-data does not split correctly"))

(test-case "get-best-split test case"
  (let ([test-data #(#(2.771244718 1.784783929 0)
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
                                  gini-index
                                  feature-columns-indices
                                  label-column-index)
                  (hash 'index 0
                        'value 6.642287351
                        'subsets (list #(#(2.771244718 1.784783929 0)
                                         #(1.728571309 1.169761413 0)
                                         #(3.678319846 2.81281357 0)
                                         #(3.961043357 2.61995032 0)
                                         #(2.999208922 2.209014212 0))
                                       #(#(7.497545867 3.162953546 1)
                                         #(9.00220326 3.339047188 1)
                                         #(7.444542326 0.476683375 1)
                                         #(10.12493903 3.234550982 1)
                                         #(6.642287351 3.319983761 1)))
                        'cost 0.0)
                  "get-best-split does not give the best split")))
