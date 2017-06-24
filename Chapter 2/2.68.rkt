#lang sicp
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf x)
    (cadr x))

(define (weight-leaf x)
    (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree)
    (car tree))

(define (right-branch tree)
    (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0)
            (left-branch branch))
          ((= bit 1)
            (right-branch branch))
          (else
            (display "bad bit -- CHOOSE-BRANCH" bit))))
; solution
(define (encode-symbol msg tree)
  (define (handle-leaf leaf)
    (if (eq? msg (symbol-leaf leaf))
            (cons '() #t)
            (cons '() #f)))
  (define (handle-tree msg tree)
    (define left-result (iter msg (left-branch tree)))
    (define right-result (iter msg (right-branch tree)))
    (if (cdr left-result)
        (cons (cons 0 (car left-result)) #t)
        (if (cdr right-result)
            (cons (cons 1 (car right-result)) #t)
            (display "symbol not found -- ENCODE-SYMBOL"))))
        
  (define (iter msg tree)
    (if (leaf? tree)
        (handle-leaf tree)
        (handle-tree msg tree)))
  (car (iter msg tree)))
      
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; tests
(define sample-tree
        (make-code-tree (make-leaf 'A 4)
                        (make-code-tree
                            (make-leaf 'B 2)
                            (make-code-tree (make-leaf 'D 1)
                                            (make-leaf 'C 1)))))
(encode-symbol 'D sample-tree)