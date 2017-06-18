#lang sicp
(define (deep-reverse items)
  (define (reverse-iter l newl)
    (cond ((null? l) newl)
          ((pair? (car l)) (reverse-iter (cdr l) (cons (deep-reverse (car l)) newl)))
          (else (reverse-iter (cdr l) (cons (car l) newl)))))
  (reverse-iter items (list)))

(define x (list (list 1 2) (list 3 4) (list 5 6)))
(deep-reverse x)
          