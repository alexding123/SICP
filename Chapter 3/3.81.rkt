#lang sicp

(define (random-stream)
  (let ((x random-init))
    (define rand
      (lambda ()
        (set! x (rand-update x))
        x))
    (define rand-reset
      (lambda () (set! x random-init) x))
    (define (handle-command l)
      (cond ((eq? l 'generate) (rand))
            ((eq? l 'reset) (rand-reset))
            (else (display "ERROR: Bad command -- HANDLE-COMMAND"))))
    (lambda (s) (stream-map handle-command s))))