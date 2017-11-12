#lang sicp
(define (ripple-carry-adder As Bs Ss C)
  (define (one-unit As Bs Ss C)
    (if (and (null? As) (null? Bs) (null? Ss))
        'done
        (let ((new-C (make-wire)))
          (full-adder (car As) (car Bs) C (car Ss) new-C)
          (one-unit (cdr As) (cdr Bs) (cdr Ss) new-C))))
  (let ((C-wire (make-wire)))
    (set-signal! C-wire C)
    (one-unit As Bs Ss C-wire)))