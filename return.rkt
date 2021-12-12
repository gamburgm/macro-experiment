#lang racket

(require syntax/parse)

(define-syntax (change-return stx)
  (syntax-parse stx
    [(e (~parse (a . d) #'e) k)
     #'((change-return a k) . (change-return d k))]
    [((~datum return) . k) #'k]
    [(s . k) #'s]))

(define-syntax (define/return stx)
  (syntax-parse stx
    [(_ (fn-name:id params:id ...) body ...+)
     (with-syntax ([(cont-func k) (generate-temporaries '(cont-func k))])
       #'(define (fn-name params ...)
           (define (cont-func k)
             (change-return (let () body ...) k))

           (call/cc cont-func)))]))

(define/return (foobar x)
  (when (= x 5)
    (return (add1 x)))

  (+ x 5))
