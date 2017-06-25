#lang sicp
; routines
(#%require (only racket random))
(define (random-in-range low high)
  (let ((range (round (- high low))))
    (+ low (random range))))
(define (square x)
  (* x x))
; solution
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment) 4))

; tests
(define (circle-test x y)
  (<= (+ (square x) (square y)) 10000))
(estimate-integral circle-test -100 100 -100 100 100000) ; estimate pi