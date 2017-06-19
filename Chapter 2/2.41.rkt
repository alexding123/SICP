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
(define (permutations s)
  (if (null? s)
      (list s)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
;solution
(define (three-var-pairs n)
  (flatmap permutations
           (flatmap (lambda (i)
             (flatmap (lambda(j)
                    (map (lambda(k)(list i j k)) (enumerate-interval (+ j 1) n)))
                  (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n))))

;test
(three-var-pairs 3)