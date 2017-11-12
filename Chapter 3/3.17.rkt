#lang sicp
(define (count-pairs pairs)
  (define (recur current track-list)
    (if (and (pair? current) (eq? #f (memq current track-list)))
        (recur (car current) (recur (cdr current) (cons current track-list)))
        track-list))
  (length (recur pairs '())))

; tests
(count-pairs (cons (cons 1 4) (cons 1 3)))
(define p1 (cons 1 4))
(define p2 (cons p1 4))
(define p3 (cons p1 p2))
(count-pairs p3)