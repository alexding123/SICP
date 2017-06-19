#lang sicp
;routine
(define (accumulate op init seqs)
  (if (null? seqs)
      init
      (op (car seqs)
          (accumulate op init (cdr seqs)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (filter pred items)
  (define (conditionCons front back)
    (if (pred front)
        (cons front back)
        back))
  (accumulate conditionCons (list) items))
(define (enumerate-interval start end)
  (if (< end start)
      nil
      (cons start (enumerate-interval (+ start 1) end))))

;solution
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens)) ; the position of the number in the list shows its column; the number itself is the row
(define (safe? k positions)
  (define (iter k current-queen-row rest-of-queens) ; k here is the column difference between the current queen and the tested queen
    (if (null? rest-of-queens)
        #t ; if all queens are checked and pass, return true
        (if (or (= (car rest-of-queens) current-queen-row) ; horizontally collide
                (= (+ (car rest-of-queens) k) current-queen-row)
                (= (- (car rest-of-queens) k) current-queen-row))
            #f
            (iter (+ k 1) current-queen-row (cdr rest-of-queens)))))
  (iter 1 (car positions) (cdr positions))) ; again, because we cons the newest queen onto positions, the new queen is accessible with (car positions)

(queens 8)