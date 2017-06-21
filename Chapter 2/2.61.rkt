#lang sicp
(define (adjoin-set set x)
  (define (iter first-half rest x)
    (if (null? rest)
      (append first-half (list x))
      (cond ((= (car rest) x) set)
        ((> (car rest) x) (append first-half (cons x rest)))
        ((< (car rest) x) (iter (append first-half (list (car rest))) (cdr rest) x)))))
  (iter '() set x))

; tests
(define s1 '(1 4 23))
(define s2 (adjoin-set s1 5 ))
s2