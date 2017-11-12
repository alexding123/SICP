#lang sicp
(define (cube x) (* x x x))
(define (weight-a l) (+ (cube (car l)) (cube (cadr l))))
(define candidates (weighted-pairs integers integers weight-a))
(define R-nums (filter-R candidates))
(define (filter-R s)
  (if (= (weight-a (stream-car s)) (weight-a (stream-car (stream-cdr s))))
      (cons-stream (weight-a (stream-car s)) (filter-R (cdr s)))
      (filter-R (cdr s))))