#lang sicp
; tons of routines
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
    (cond ((pair? datum) (car datum))
          ((number? datum) 'scheme-number)
          (else (display "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (cond ((pair? datum) (cdr datum))
          ((number? datum) datum)
          (else (display "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (display 
                    "No method for these types -- APPLY-GENERIC"
                    (list op type-tags))))))

(define (install-syntax)
  (put 'syntax 'quote text-of-quotation)
  (put 'syntax 'set! eval-assignment)
  (put 'syntax 'define eval-definition)
  (put 'syntax 'if eval-if)
  (put 'syntax 'lambda (lambda (exp env)
                         (make-procedure (lambda-parameters exp)
                                         (lambda-body exp)
                                         env)))
  (put 'syntax 'begin (lambda (exp env)
                        (eval-sequence (begin-actions exp) env)))
  (put 'syntax 'cond (lambda (exp env)
                       (eval (cond->if exp) env)))
  (put 'syntax 'let (lambda (exp env)
                      (eval (let->combination exp) env)))
  (put 'syntax 'let* (lambda (exp env)
                       (eval (let*->nested-lets exp) env))))
(define let*-varexp cadr)
(define let*-body caddr)
(define (let*->nested-lets exp)
  (let ((sequences (let*-varexp exp))
        (body (let*-body exp)))
    (define (build-up seq)
      (if (null? seq)
          body
          (let ((first (first-exp seq))
                (rest (rest-exps seq)))
            (let->combination (list 'let (list first) (build-up rest))))))
    build-up sequences))