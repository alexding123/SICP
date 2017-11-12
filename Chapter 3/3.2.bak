#lang sicp
; solution
(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((eq? input 'reset-count) (set! count 0))
            ((eq? input 'how-many-calls?) count)
            (else (begin (set! count (+ count 1)) (f input)))))))

; tests
(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)