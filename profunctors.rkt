#lang racket/base

(require racket/generic
         racket/function
         racket/stream)

(provide (struct-out func)
         (struct-out forget)
         gen:profunctor
         dimap
         dimap-curried
         strong-first
         exchange
         exchange-value)

(module+ test
  (require rackunit))

(define-generics profunctor
  (dimap- neg profunctor pos))

(define dimap dimap-) ; Allow dimap definitions to use other dimaps without accidental shadowing

(define ((dimap-curried neg pos) profunctor)
  (dimap neg profunctor pos))

(define-generics strong
  (strong-first- strong)) ; strong-first :: p a b -> p (a, o) (b, o)

(define strong-first strong-first-)

(define-generics redundant
  (listy- redundant)) ; listy :: p a b -> p [a] [b]

(define listy listy-)

(struct func (value)
  #:methods gen:profunctor
  [(define (dimap- neg profunctor pos)
     (func (compose1 pos (func-value profunctor) neg)))]
  #:methods gen:strong
  [(define (strong-first- strong)
     (func (lambda (consed)
             (cons
              ((func-value strong) (car consed))
              (cdr consed)))))]
  #:methods gen:redundant
  [(define (listy- redundant)
     (func
      (lambda (incoming-stream)
        (stream-map (func-value redundant) incoming-stream))))])

(module+ test
  (check-eq?
   ((func-value (dimap (curry * -1) (func add1) (curry * 10))) 5)
   -40)
  (check-equal?
   ((func-value (strong-first (func add1))) (cons 5 "a"))
   (cons 6 "a")))

(struct forget (value)
  #:methods gen:profunctor
  [(define (dimap- neg profunctor _)
     (forget (compose1 (forget-value profunctor) neg)))]
  #:methods gen:strong
  [(define (strong-first strong)
     (forget (compose1 (forget-value strong) car)))]
  #:methods gen:redundant
  [(define (listy- redundant)
   (raise-argument-error "Cannot get from a multilens"))])

(define (stream-join stream)
  (if (stream-empty? stream)
      stream
      (let ([first-stream (stream-first stream)]
            [rest-stream (stream-rest stream)])
        (if (stream-empty? first-stream)
            (stream-join rest-stream)
            (stream-cons
             (stream-first first-stream)
             (stream-join (stream-cons (stream-rest first-stream) rest-stream)))))))

(module+ test
  (check-equal?
   (stream->list (stream-join (stream (stream 1 2) (stream) (stream 3))))
   (list 1 2 3)))

(define (stream-bind stream f)
  (stream-join (stream-map f stream)))

(struct forget-stream (value) ; a -> [r]
  #:methods gen:profunctor
  [(define (dimap neg profunctor _)
     (forget-stream (compose1 (forget-stream-value profunctor) neg)))]
  #:methods gen:strong
  [(define (strong-first strong)
     (forget-stream (compose1 (forget-stream-value strong) car)))])

(struct exchange (value)
  #:methods gen:profunctor
  [(define (dimap- neg profunctor pos)
     (exchange
      (lambda (driver) ; profunctor that this new iso will manipulate
       ((exchange-value profunctor) 
        (dimap pos driver neg)))))]) ; Force this dimap to come before the rest defined so far in the exchange