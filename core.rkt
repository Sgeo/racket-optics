#lang racket/base

(require racket/function)
(require "profunctors.rkt")

(define iso dimap-curried) ; (iso get reversed-get)

(define (lens getter setter) ; getter just gets focus. setter should accept two arguments: new value for focus and original object
  (define (lifted-getter o)
    (cons (getter o) o))
  (define (lifted-setter consed)
    (setter (car consed) (cdr consed)))
  (compose1 (dimap-curried lifted-getter lifted-setter) strong-first))

(define (o-getter optic)
  (forget-value (optic (forget identity))))