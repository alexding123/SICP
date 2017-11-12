#lang sicp
; the general idea is to have two vars iterating through the list at different paces
; if they meet up, it means that somehow the faster var looped around and met with the slower var
; hence there is a loop

; routine
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; solution
(define (loop? test-list)
  (define (iter slower faster)
    (let ((new-slower (iterate-list slower 1))
          (new-faster (iterate-list faster 2)))
      (cond ((or (null? new-slower) (null? new-faster)) #f)
            ((eq? new-slower new-faster) #t)
            (else (iter new-slower new-faster)))))
  (iter test-list test-list))

(define (iterate-list iterator steps)
  (if (= steps 0)
      iterator
      (if (null? iterator)
          '()
          (iterate-list (cdr iterator) (- steps 1)))))

; tests
(loop? (list 1 2 3))
(define loop (list 1 2 3))
(set-cdr! (last-pair loop) loop)
(loop? loop)