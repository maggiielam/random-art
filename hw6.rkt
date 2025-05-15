#lang racket

; the following is necessary for testing and use by graphics.rkt
(provide (all-defined-out))

; Including the following will introduce procedures make-relation,
; relation?, relation-domain and relation-pairs
(define-struct relation (domain pairs))

; below are the sample relations from the assignment specification
(define less-than (make-relation '(1 2 3) '((1 2) (1 3) (2 3))))
(define less-than-or-equal
  (make-relation '(1 2 3) '((1 1) (1 2) (1 3) (2 2) (2 3) (3 3))))

; include your answers to the homework below

(define (collapse lst)
  (cond [(null? lst) '()]
        [(= (length lst) 1) (list lst)]
        [else (cons (+ (first lst) (second lst)) (collapse(cdr (cdr lst))))]))

(define (stretch lst)
  (cond [(null? lst) '()]
        [(< (car lst) 0) (cons  (- (quotient (car lst) 2) (modulo (car lst) 2)) (cons (quotient (car lst) 2) (stretch(cdr lst))))]
        [else (cons  (+ (quotient (car lst) 2) (modulo (car lst) 2)) (cons (quotient (car lst) 2) (stretch(cdr lst))))]))

(define (all-pairs lst1 lst2)
  (define (helper x)
    (define (helper2 y)
      (list x y))
    (map helper2 lst2))
  (foldr append '() (map helper lst1)))

(define (contains? pair r)
    (ormap (lambda (p) (and (equal? (car pair) (car p)) (equal? (cadr pair) (cadr p)))) (relation-pairs r))) 

(define (reflexive? r)
  (andmap (lambda (pair) (contains? pair r)) (map (lambda (n) (list n n)) (relation-domain r))))

(define (symmetric? r)
  (andmap (lambda (pair) (contains? (list (cadr pair) (car pair)) r)) (relation-pairs r)))
  
(define (transitive? r)
  (let ([pairs (filter (lambda (p) (equal? (car (cdr (car p))) (car (car (cdr p))))) (all-pairs (relation-pairs r) (relation-pairs r)))])
    (andmap (lambda (pairs) (contains? (list (caar pairs) (cadadr pairs)) r)) pairs)))

(define (generate n)
  (cond [(= 0 n) (cond [(<= 0.5 (random)) 'x]
                         [else 'y])]
        [else (let ([rand (random)])
                (cond [(<= rand 0.2) (list 'cos (list '* 'pi (generate(- n 1))))]
                      [(<= rand 0.4) (list 'sin (list '* 'pi (generate(- n 1))))]
                      [(<= rand 0.6) (list '/ (list '+ (generate(- n 1)) (generate(- n 1))) 2.0)]
                      [(<= rand 0.8) (list '/ (list 'round (list '* (generate(- n 1)) 10.0)) 10.0)]
                      [else (list '* (generate(- n 1)) (generate(- n 1)))]))]))
                 