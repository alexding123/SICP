#lang sicp
(define (make-leaf symbol weight) 
  (list 'leaf symbol weight)) 

(define (leaf? object) 
  (eq? (car object) 'leaf)) 

(define (symbol-leaf x) (cadr x)) 

(define (weight-leaf x) (caddr x)) 

(define (make-code-tree left right) 
  (list left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right)))) 

(define (left-branch tree) (car tree)) 

(define (right-branch tree) (cadr tree)) 

(define (symbols tree) 
  (if (leaf? tree) 
      (list (symbol-leaf tree)) 
      (caddr tree))) 

(define (weight tree) 
  (if (leaf? tree) 
      (weight-leaf tree) 
      (cadddr tree))) 

(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((< (weight x) (weight (car set))) (cons x set)) 
        (else (cons (car set) 
                    (adjoin-set x (cdr set)))))) 

(define (make-leaf-set pairs) 
  (if (null? pairs) 
      '() 
      (let ((pair (car pairs))) 
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (encode message tree) 
  (if (null? message) 
      '() 
      (append (encode-symbol (car message) tree) 
              (encode (cdr message) tree)))) 
(define (successive-merge set)
  (cond ((= (length set) 0) '())
        ((= (length set) 1) (car set))
        (else (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set))))))
(define (encode-symbol msg tree)
  (define (handle-leaf leaf)
    (if (eq? msg (symbol-leaf leaf))
            '()
            #f))
  (define (handle-tree msg tree)
    (define left-result (encode-symbol msg (left-branch tree)))
    (define right-result (encode-symbol msg (right-branch tree)))
    (if (list? left-result)
        (cons 0 left-result)
        (if (list? right-result)
            (cons 1 right-result)
            (display "symbol not found -- ENCODE-SYMBOL"))))
  (if (leaf? tree)
        (handle-leaf tree)
        (handle-tree msg tree)))
      


; solution
(define tree (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (GET 2) (A 2) (JOB 2) (BOOM 1) (WAH 1))))
(define msg-2 '(SHA NA NA NA NA))
(encode msg-2 tree)