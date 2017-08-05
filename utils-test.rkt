#lang racket

(require rackunit
         "utils.rkt")

(test-case "maximum-for-procedure test case"
  (check-equal? (maximum-for-procedure (list 0 1 2 3)
                                       (lambda (elem) elem)
                                       (lambda (elem) elem))
                3)

  (check-equal? (maximum-for-procedure (list 0 1 2 3)
                                       (lambda (elem) (- elem))
                                       (lambda (elem) (- elem)))
                0)

  (check-equal? (maximum-for-procedure (list 0 1 2 3)
                                       (lambda (elem) 4)
                                       (lambda (elem) elem))
                0)

  (let* ([tolerance 0.051]
         [percetage-one (lambda (a-list)
                          (/ (count (lambda (a-number)
                                      (= a-number 1))
                                    a-list)
                             (length a-list)))]
         [percetage-one-with-tolerance
          (lambda (a-list)
            (+ (percetage-one a-list)
               tolerance))])
    ;; this check tests whether the procedure works with an in-built tolerance
    (check-equal?
     (maximum-for-procedure (list (list 0 0 1 1 1 1 1 1 1 1)  ; 80.0
                                  (list 0 0 1 1 1 1 1 1 1)  ; 77.77777...
                                  (list 0 1 1 1)  ; 75.0
                                  (list 0 0 0 1 1 1 1 1 1 1))  ; 70.0
                            (lambda (elem) (percetage-one elem))
                            (lambda (elem) (percetage-one-with-tolerance elem)))
     (list 0 0 0 1 1 1 1 1 1 1)))

  (let* ([tolerance 0.023]
         [percetage-one (lambda (a-list)
                          (/ (count (lambda (a-number)
                                      (= a-number 1))
                                    a-list)
                             (length a-list)))]
         [percetage-one-with-tolerance
          (lambda (a-list)
            (+ (percetage-one a-list)
               tolerance))])
    ;; this check tests whether the procedure works with an in-built tolerance
    (check-equal?
     (maximum-for-procedure (list (list 0 0 1 1 1 1 1 1 1 1)  ; 80.0
                                  (list 0 0 1 1 1 1 1 1 1)  ; 77.77777...
                                  (list 0 1 1 1)  ; 75.0
                                  (list 0 0 0 1 1 1 1 1 1 1))  ; 70.0
                            (lambda (elem) (percetage-one elem))
                            (lambda (elem) (percetage-one-with-tolerance elem)))
     (list 0 0 1 1 1 1 1 1 1))))
