(define shouldDeleteFirstww?
  (lambda (r l)
    (cond ((null? l) #f)
          ((member r (cadar l)) #f)
          ((member r (caddar l)) #t)
          (else (shouldDeleteFirstww? r (cdr l))))
    ))

(define deleteww
  (lambda (l)
    (cond ((null? l) l)
          ((null? (cadr (cdar l))) (deleteww (cdr l)))
          ((shouldDeleteFirstww? (caadr (cdar l)) (cdr l)) (deleteww (cdr l)))
          (else `(,(car l) ,@(deleteww (cdr l)))))
    ))

(define remww
  (lambda (l)
    (begin (set! newL (deleteww l))
           (if (= (length l) (length newL)) l
               (remww newL)))
    ))