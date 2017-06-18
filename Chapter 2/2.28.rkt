#lang sicp
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(define (fringe items)
  (define (iter items newItems)
    (cond ((null? items) newItems)
          ((not (pair? items)) (append newItems (list items)))
          (else (iter (cdr items) (append newItems (fringe (car items)))))))
  (iter items (list)))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))