#lang racket/base

(require racket/generic
         racket/function)

(provide (struct-out func)
         (struct-out forget)
         gen:profunctor
         dimap
         dimap-curried
         strong-first)

(module+ test
  (require rackunit))

(define-generics profunctor
  (dimap neg profunctor pos))

(define ((dimap-curried neg pos) profunctor)
  (dimap neg profunctor pos))

(define-generics strong
  (strong-first strong)) ; strong-first :: p a b -> p (a, o) (b, o)

(struct func (value)
  #:methods gen:profunctor
  [(define (dimap neg profunctor pos)
     (func (compose1 pos (func-value profunctor) neg)))]
  #:methods gen:strong
  [(define (strong-first strong)
     (func (lambda (consed)
             (cons
              ((func-value strong) (car consed))
              (cdr consed)))))])

(module+ test
  (check-eq?
   ((func-value (dimap (curry * -1) (func add1) (curry * 10))) 5)
   -40)
  (check-equal?
   ((func-value (strong-first (func add1))) (cons 5 "a"))
   (cons 6 "a")))

(struct forget (value)
  #:methods gen:profunctor
  [(define (dimap neg profunctor _)
     (forget (compose1 (forget-value profunctor) neg)))]
  #:methods gen:strong
  [(define (strong-first strong)
     (forget (compose1 car (forget-value strong))))])