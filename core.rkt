#lang racket/base

(require racket/function)
(require "profunctors.rkt")

(module+ test
  (require rackunit))

(define iso dimap-curried) ; (iso get reversed-get)

(define (lens getter setter) ; getter just gets focus. setter should accept two arguments: new value for focus and original object
  (define (lifted-getter o)
    (cons (getter o) o))
  (define (lifted-setter consed)
    (setter (car consed) (cdr consed)))
  (compose1 (dimap-curried lifted-getter lifted-setter) strong-first)) ; Data flows from lifted-getter -> strong-first -> lifted-setter

(define (o-getter optic)
  (forget-value (optic (forget identity))))

(module+ test
  (define car-o (lens car (lambda (new o) (cons new (cdr o)))))
  (check-equal? ((o-getter (compose1 car-o car-o)) (cons (cons 3 5) 6))
                3))