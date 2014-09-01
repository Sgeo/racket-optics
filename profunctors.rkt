#lang racket/base

(require racket/generic
         racket/function)

(provide (struct-out func)
         (struct-out forget)
         gen:profunctor)

(module+ test
  (require rackunit))

(define-generics profunctor
  (dimap neg profunctor pos))

(define ((dimap-curried neg pos) profunctor)
  (dimap neg profunctor pos))

(struct func (value)
  #:methods gen:profunctor
  [(define (dimap neg profunctor pos)
     (func (compose1 pos (func-value profunctor) neg)))])

(module+ test
  (check-eq?
   ((func-value (dimap (curry * -1) (func add1) (curry * 10))) 5)
   -40))

(struct forget (value)
  #:methods gen:profunctor
  [(define (dimap neg profunctor _)
     (forget (compose1 (forget-value profunctor) neg)))])