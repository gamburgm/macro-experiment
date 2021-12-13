#lang racket

(require (for-syntax racket/base syntax/parse))


(define-syntax (define/return stx)
  (syntax-parse stx #:datum-literals (return)
    [(_ (fn-name:id params:id ...) body ... expr:expr)
     (with-syntax ([(cont-func k) (generate-temporaries '(cont-func k))])
       (define x #'(define (fn-name params ...)
           (define (cont-func k)
             (define-syntax return (make-rename-transformer #'k))
             (let () body ... expr))

           (call/cc cont-func)))
     (displayln x)
                x)]))

(define/return (foobar x)
  (when (= x 5)
    (return (add1 x)))

  (+ x 5))
