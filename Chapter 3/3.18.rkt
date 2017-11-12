#lang sicp
(define (loop? test-list)
  (define (recur current track-list)
    (if (pair? current)
        (if (memq current track-list)
            #t
            (recur (cdr current) (cons current track-list)))
        #f))
  (recur test-list '()))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
; tests
   

(define loop-list (list 1 2 3))
(set-cdr! (last-pair loop-list) loop-list)
(loop? loop-list)