#lang sicp
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (integrate-series s)
  (mul-streams s
               (stream-map (lambda (x) (1 / x)) integers)))
(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (-x)) (integrate-series sine-series))))
(define sine-series
  (cons-streams 0 (integrate-series cosine-series)))