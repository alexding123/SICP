#lang sicp
; from textbook
(define true #t)
 
(define false #f)
 
(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set))))) 
 
(define (adjoin-set x set) 
  (if (element-of-set? x set) 
      set 
      (cons x set))) 
 
(define (intersection-set set1 set2) 
  (cond ((or (null? set1) (null? set2)) '()) 
        ((element-of-set? (car set1) set2)        
         (cons (car set1) 
               (intersection-set (cdr set1) set2))) 
        (else (intersection-set (cdr set1) set2))))
; solution
(define (union-set s1 s2) ; recursively add stuff from s1 to s2
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (union-set (cdr s1) (cons (car s1) s2)))))

; tests
(define s1 '(a b c))
(define s2 '(b d e))
(union-set s1 s2)