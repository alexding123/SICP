#lang sicp
(define (smooth s)
  (stream-map (lambda (x y) (/ (+ x y) 2)) s (stream-cdr s)))
(define zero-crossings
  (let ((smoothed (stream-map smooth sense-data))
        (stream-map sign-change-detector smoothed (stream-cdr smoothed)))))