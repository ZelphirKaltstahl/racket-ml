#lang racket

(require csv-reading)
(require rackunit)

(provide all-rows
         DEFAULT-DATA-READER-MAKER)

;; the data-reader will be able to understand the specific CSV format
(define DEFAULT-DATA-READER-MAKER
  (make-csv-reader-maker
   '((seperator-chars #\,)
     (strip-leading-whitespace . true)
     (strip-trailing-whitespace . true))))

(define (all-rows a-file-path
                  #:a-reader-maker [a-reader-maker DEFAULT-DATA-READER-MAKER]
                  #:column-converters [column-converters empty])
  (define next-row
    (a-reader-maker (open-input-file a-file-path)))

  (define (convert-row row column-converters)
    (define (col-iter remaining-columns remaining-column-converters)
      (cond [(empty? remaining-columns) empty]
            [else (cond [(empty? remaining-column-converters)
                         (cons (first remaining-columns)
                               (col-iter (rest remaining-columns) empty))]
                        [else (cons ((first remaining-column-converters) (first remaining-columns))
                                    (col-iter (rest remaining-columns)
                                              (rest remaining-column-converters)))])]))
    (col-iter row column-converters))

  (define (row-iter)
    (let ([row (next-row)])
      (cond [(empty? row) empty]
            [else (cons (list->vector (convert-row row column-converters))
                        (row-iter))])))
  (row-iter))

#|
====
USAGE EXAMPLE:
(define data-set (all-rows FILE-PATH))
====
|#
