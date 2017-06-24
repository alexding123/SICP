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

(define (level type) 
  (cond ((eq? type 'scheme-number) 0) 
        ((eq? type 'rational) 1) 
        ((eq? type 'complex) 2)
        ((eq? type 'polynomial) 3)
        ((bool? type) -1)
        (else (display "type not found -- LEVEL" type))))

(define (raise-to desired-arg arg)
  (if (< (level (type-tag arg)) (level desired-arg))
      (raise-to desired-arg (raise arg))
      arg))
(define (map-raise-to desired-arg)
  (lambda (a) (raise-to desired-arg a)))
(define (highest-level args)
  (if (null? args)
      (display "no argument given -- HIGHEST-LEVEL")
      (if (null? (cdr args))
          (car args)
          (highest-level (cons (if (> (level (car args)) (level (cadr args))) (car args) (cadr args)) (cddr args))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((highest (highest-level type-tags)))
            (let ((raised (map (map-raise-to highest) args)))
              (let ((new-proc (get op (map type-tag raised))))
                (if new-proc
                    (drop (apply new-proc (map contents raised)))
                    (display "No method for these types -- APPLY-GENERIC" (list op type-tags))
                    ))))))))
(define (drop x)
  (if (bool? x)
      x
      (let ((lower-level (project x)))
        (if lower-level
            (if (equ? (raise lower-level) x)
                (drop lower-level)
                x)
            x))))
(define (project x)
  (if (<= (level (type-tag x)) 0)
      #f
      (apply-generic 'project x)))

; generic functions
(define (square x)
  (mul x x))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? a b)
  (and (number? a) (number? b) (= a b)))
(define (bool? b)
  (or (eq? b #t) (eq? b #f)))


; packages
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (scheme-number->rational x)
    (make-rational x 1))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
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
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  (put 'raise '(scheme-number) scheme-number->rational)
  (put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
  'done)
(define (install-rational-package)
    
  ;; internal procedures


  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
 
  (define (rational->complex x)
    (make-complex-from-real-imag (tag x) 0))

  (define (rational->scheme-number r) (round (/ (numer r) (denom r))))
  
  (define (make-rat n d)
    (cons n d))

  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'rational x))

  (put 'make 'rational
       (lambda (n d)
         (tag (make-rat n d))))
  (put 'add '(rational rational)
       (lambda (a b)
         (tag (add-rat a b))))
  (put 'mul '(rational rational)
       (lambda (a b) (tag (mul-rat a b))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (equ? (numer x) (numer y))
              (equ? (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (equ? (numer x) 0)))
  (put 'neg '(rational)
       (lambda (x) (make-rat (neg (numer x)) (denom x))))
  (put 'raise '(rational) rational->complex)
  (put 'project '(rational) rational->scheme-number)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rectangular-package)

  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (drop (add (square (real-part z))
               (square (imag-part z))))))
  (define (angle z)
    (atan (drop (imag-part z)) (drop (real-part z))))

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
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (neg-complex z)
    (make-from-real-imag (neg (real-part z)) (neg (imag-part z))))
  (define (equ-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2))))
  (define (complex->polynomial z)
    (make-polynomial 'indeterminate (list (list 0 (tag z)))))
  (define (complex->rational z)
    (let ((real (real-part z)))
      (if (< (level (type-tag real)) (level 'rational))
          (raise-to 'rational real)
          real)))
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
  (put 'neg '(complex)
       (lambda (z)
         (tag (neg-complex z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'raise '(complex) complex->polynomial)
  (put 'project '(complex) complex->rational)

  ; added by Alyssa
  (put 'real-part '(complex) real-part)

  (put 'imag-part '(complex) imag-part)

  (put 'magnitude '(complex) magnitude)
  
  (put 'equ? '(complex complex) equ-complex?)
  
  (put 'angle '(complex) angle)

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-polynomial-package)
  ; internal procedures
  ; representing polynomials
  (define (make-poly variable term-list)
    (cons variable (sift-through term-list)))
  (define (sift-through term-list)
    (if (null? term-list)
        '()
        (adjoin-term (car term-list) (sift-through (cdr term-list)))))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ; representing terms and term lists
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (display "Polys are not in the same var -- ADD-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1) (term-list p2))) 
        (display "Polys are not in the same var -- SUB-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (display "Polys are not in the same var -- MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (cond ((=number? p2 0) '(0 0))
          ((same-variable? (variable p1) (variable p2))
           (let ((result (div-terms (term-list p1) (term-list p2))))
             (list (make-polynomial (variable p1) (car result)) (make-polynomial (variable p1) (cadr result)))))
          (else (display "Polys are not in the same var -- DIV-POLY" (list p1 p2)))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (display "Polys are not in the same var -- GCD-POLY" (list p1 p2))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  
  (define (poly-equ? p1 p2)
    (cond ((eq? (variable p1) (variable p2)) (equ-terms? (term-list p1) (term-list p2)))
          ((or (eq? (variable p1) 'indeterminate) (eq? variable p2 'indeterminate)) (equ-terms? (term-list p1) (term-list p2)))
          (else #f)))
  (define (equ-terms? l1 l2)
    (if (null? l1)
        (null? l2)
        (if (null? l2)
            #f
            (and (equ-term? (first-term l1) (first-term l2)) (equ-terms? (cdr l1) (cdr l2))))))
  (define (equ-term? t1 t2)
    (and (equ? (order t1) (order t2)) (equ? (coeff t1) (coeff t2))))
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1)) (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms l1 (rest-terms l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1) (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1) (rest-terms l2)))))))))
  (define (sub-terms l1 l2)
    (add-terms l1 (neg-terms l2)))
          
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms l))))))

  (define (div-terms l1 l2)
    (if (empty-termlist? l1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term l1))
              (t2 (first-term l2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) l1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms l1 (mul-terms (list (make-term new-o new-c)) l2)) l2)))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))))
  (define (neg-terms terms)
    (if (null? terms)
        (the-empty-termlist)
        (adjoin-term (make-term (order (first-term terms)) (neg (coeff (first-term terms))))
                     (neg-terms (rest-terms terms)))))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list))
    )

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (polynomial->complex p)
    (let ((constant (get-constant (term-list p))))
      (if (< (level (type-tag constant)) (level 'complex))
          (raise-to 'complex constant)
          constant)))
  (define (get-constant L)
    (cond ((empty-termlist? L) 0)
          ((= (order (first-term L)) 0) (coeff (first-term L)))
          (else (get-constant (rest-terms L)))))
  (define (check-and-convert p1 p2)
    (if (eq? (variable p1) (variable p2))
        (cons p1 p2)
        (cond ((eq? (variable p1) 'indeterminate) (cons (make-poly (variable p2) (term-list p1)) p2))
              ((eq? (variable p2) 'indeterminate) (cons p1 (make-poly (variable p1) (term-list p2))))
              (else (cons p1 p2)))))

  ; interface
  (define (tag x)
    (attach-tag 'polynomial x))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (check-and-convert p1 p2)))
           (tag (add-poly (car result) (cdr result))))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (check-and-convert p1 p2)))
           (tag (sub-poly (car result) (cdr result))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (check-and-convert p1 p2)))
           (tag (mul-poly (car result) (cdr result))))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (check-and-convert p1 p2)))
           (div-poly (car result) (cdr result))))) ; no need to tag cuz it's already done in div-poly
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (null? (term-list p))))
  (put 'neg '(polynomial)
       (lambda (p) (make-polynomial (variable p) (neg-terms (term-list p)))))
  (put 'project '(polynomial) polynomial->complex)
  (put 'equ? '(polynomial polynomial) poly-equ?)
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (check-and-convert p1 p2)))
           (tag (gcd-poly (car result) (cdr result))))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
  
; generic selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (add a b) (apply-generic 'add a b))
(define (sub a b) (apply-generic 'sub a b))
(define (mul a b) (apply-generic 'mul a b))
(define (div a b) (apply-generic 'div a b))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? a) (apply-generic '=zero? a))
(define (neg a) (apply-generic 'neg a))
(define (raise a) (apply-generic 'raise a))
(define (greatest-common-divisor a b)(apply-generic 'greatest-common-divisor a b))
; install packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

; tests
(define poly1 (make-polynomial 'x '((2 1) (1 2) (0 1))))
(define poly2 (make-polynomial 'x '((1 1) (0 1))))
(div poly1 poly2)