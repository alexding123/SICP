#lang sicp
(define (div-series s1 s2)
    (if (= (stream-car s2) 0)
        (display "constant term of second stream can't be 0! -- DIV-SERIES")
        (scale-stream (mul-series (reciprocal-series s2) s1))))