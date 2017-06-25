#lang sicp
(define random-init 1.0)
(define rand
  (let ((x random-init))
    (lambda (key)
      (cond ((eq? key 'generate) (rand-update x))
            ((eq? key 'reset) (lambda (value) (set! x value)))
            (else (display "unrecognized command -- RAND"))))))
