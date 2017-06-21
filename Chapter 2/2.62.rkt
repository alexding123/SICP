#lang sicp
(define (adjoin-set set x)
  (define (iter first-half rest x)
    (if (null? rest)
      (append first-half (list x))
      (cond ((= (car rest) x) set)
        ((> (car rest) x) (append first-half (cons x rest)))
        ((< (car rest) x) (iter (append first-half (list (car rest))) (cdr rest) x)))))
  (iter '() set x))

; solution
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
        ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))))
; tests
(define s1 (list 1 23 45))
(define s2 (list 1 45 245))
(union-set s1 s2)