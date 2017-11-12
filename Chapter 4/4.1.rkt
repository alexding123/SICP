#lang sicp

;; left to right
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps))))
        (cons left
              (list-of-values-lr (rest-operands exps) env)))))

;; right to left
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (eval (rest-operands exps))))
        (cons (eval (first-operand exps) env)
              right))))