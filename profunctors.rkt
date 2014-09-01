#lang racket/base

(require racket/generic
         racket/function)

(define-generics profunctor
  (dimap profunctor neg pos))

(struct func (value)
  #:methods gen:profunctor
  [(define (dimap profunctor neg pos)
     (func (compose1 pos (func-value profunctor) neg)))])

