#lang sicp

; solution
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr'()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (display "called with empty queue -- DELETE"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (display "unknown message type -- DISPATCH"))))
    dispatch))

; tests
(define t1 (make-queue))
((t1 'insert-queue!) 10)
((t1 'insert-queue!) 45)
((t1 'delete-queue!))