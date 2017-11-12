#lang sicp
(define (ln2-stream n)
  (cons-stream (/ 1.0 n)
               (stream-map (lambda (x) (-x)) (ln2-stream (+ n 1)))))

(define faster-ln2-stream (euler-transform ln2-stream))
(define super-fast-ln2-stream (accelerated-sequence euler-transform ln2-stream))