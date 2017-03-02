(define list (lambda v (letrec ((list-helper  (lambda (l) (if (null? l) '() (cons (car l) (list-helper (cdr l))))))) (list-helper v))))
(define + (lambda lq (letrec ((rec (lambda (lst) (if (null? lst) 0 (plus-two (car lst) (rec (cdr lst))))))) (rec lq))))
(define - (lambda lq (letrec ((rec (lambda (lst) (if (null? lst) 0 (minus-two (car lst) (rec (cdr lst))))))) (rec lq))))
(define * (lambda lq (letrec ((rec (lambda (lst) (if (null? lst) 1 (mul-two (car lst) (rec (cdr lst))))))) (rec lq))))
(define / (lambda lq (letrec ((rec (lambda (lst) (if (null? lst) 1 (div-two (car lst) (rec (cdr lst))))))) (rec lq))))
(define = (lambda lq (letrec ((rec (lambda (lst) (if (null? (cdr lst)) #t (and (math-eq-two (car lst) (car (cdr lst))) (rec (cdr lst)))))))   (rec lq))))
(define > (lambda lq (letrec ((rec (lambda (lst) (if (null? (cdr lst)) #t (and (math-greater-two (car lst) (car (cdr lst))) (rec (cdr lst)))))))   (rec lq))))
;(define < (lambda lq (not (or (apply = lq) (apply > lq)))))

