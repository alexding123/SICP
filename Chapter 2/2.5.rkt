#lang sicp
(define (square x) (* x x))
 
(define (fast-expt b n)
  (fast-expt-iter b n 1))
 
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (car x)
  (if (= 0 (remainder x 2)) (+ 1 (car (/ x 2)))
      0))
(define (cdr x)
  (if (= 0 (remainder x 3)) (+ 1 (cdr (/ x 3)))
      0))

(define myNum (cons 2 3))
(display (car myNum))
(display (cdr myNum))
  