#lang sicp
(define f
    (lambda (first-value)
        (set! f (lambda (second-value) 0))
        first-value))

; test
(+ (f 0) (f 1))
;; from http://sicp.readthedocs.io/en/latest/chp3/8.html