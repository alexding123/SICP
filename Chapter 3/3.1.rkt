#lang sicp
(define (make-accumulator sum)
  (lambda (new)
    (+ sum new)))

; tests
(define A (make-accumulator 10))
(A 10)
(A 30)