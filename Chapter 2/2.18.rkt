#lang sicp
(define (reverse l)
  (define (reverse-iter l newl)
    (if (null? l)
       newl
       (reverse-iter (cdr l) (cons (car l) newl))))
  (reverse-iter l (list)))
(list 1 4 9 16 25)
(reverse (list 1 4 9 16 25))