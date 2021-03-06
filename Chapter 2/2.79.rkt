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


(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (display 
                    "No method for these types -- APPLY-GENERIC"
                    (list op type-tags))))))
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
(define (square x)
  (* x x))
; packages provided in book
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (a b) (= a b)))
  'done)
(define (install-rational-package)
    
    ;; internal procedures
    (define (numer x)
        (car x))

    (define (denom x)
        (cdr x))

    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

    ;; interface to rest of the system
    (define (tag x)
        (attach-tag 'rational x))

    (put 'make 'rational
        (lambda (n d)
            (tag (make-rat n d))))

    (put 'equ? '(rational rational)
        (lambda (x y)
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y)))))

'done)

(define (make-rational n d)
    ((get 'make 'rational) n d))
(define (install-rectangular-package)

  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))

'done)

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
(define (install-polar-package)

  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))

'done)

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

(define (install-complex-package)

    ;;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;;; interal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))

    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))

    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))

    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))
    (define (equ? z1 z2)
        (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
    ;;; interface to rest of the system
    (define (tag z)
        (attach-tag 'complex z))

    (put 'add '(complex complex)
        (lambda (z1 z2)
            (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
        (lambda (z1 z2)
            (tag (sub-complex z1 z2))))

    (put 'mul '(complex complex)
        (lambda (z1 z2)
            (tag (mul-complex z1 z2))))

    (put 'div '(complex complex)
        (lambda (z1 z2)
            (tag (div-complex z1 z2))))

    (put 'make-from-real-imag 'complex
        (lambda (x y)
            (tag (make-from-real-imag x y))))

    (put 'make-from-mag-ang 'complex
        (lambda (r a)
            (tag (make-from-mag-ang r a))))


    ; added by Alyssa
    (put 'real-part '(complex) real-part)

    (put 'imag-part '(complex) imag-part)

    (put 'magnitude '(complex) magnitude)
  
    (put 'equ? '(complex complex) equ?)
  
    (put 'angle '(complex) angle)

'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
; generic selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (add a b) (apply-generic 'add a b))
(define (equ? a b) (apply-generic 'equ? a b))
  
; install packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


; tests
(define num (make-scheme-number 4))
(define num2 (make-scheme-number 8))
(equ? (add num num) num2)
(equ? (make-rational 1 2)
            (make-rational 1 2))
(equ? (make-complex-from-real-imag 1 2)
            (make-complex-from-real-imag 1 2))
(equ? (make-complex-from-real-imag 1 2)
            (make-complex-from-real-imag 10086 10086))