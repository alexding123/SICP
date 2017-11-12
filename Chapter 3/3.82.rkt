#lang sicp
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-cdr experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral p x1 x2 y1 y2)
  (define experiment-stream
    (cons-stream (p (random-in-range x1 x2) (random-in-range y1 y2))
                 experiment-stream))
  (scale-stream (monte-carlo experiment-stream 0 0) 4))  

; tests
(define (circle-test x y)
  (<= (+ (square x) (square y)) 10000))
(estimate-integral circle-test -100 100 -100 100 100000) ; estimate pi