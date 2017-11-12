#lang sicp
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (scale-stream (integral i v0 dt)))))