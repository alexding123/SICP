#lang sicp
(define (square-tree items)
  (cond ((null? items) nil)
        ((pair? items) (cons (square-tree (car items)) (square-tree (cdr items))))
        (else (* items items))))
(define (square-tree2 items)
  (map (lambda (sub-item)
         (if (pair? sub-item) ; does not handle null because map does not map to null values
             (square-tree2 sub-item)
             (* sub-item sub-item)))
       items))
(define tree (list 1 (list 2 (list 3 4) 5)))
(square-tree tree)
(square-tree2 tree)
