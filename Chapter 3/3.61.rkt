#lang sicp
(define (get-reciprocal s)
  (cons-stream 1
               (stream-map (lambda (x) (-x))
                           (mul-series (stream-cdr s)
                                       (get-reciprocal s)))))