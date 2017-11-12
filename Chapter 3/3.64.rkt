#lang sicp
(define (stream-limit s limit)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) limit)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))