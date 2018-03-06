#lang racket

(provide LA→L0 LB→LA Rx→LB)
(module+ test (require rackunit))

; THIS FILE ADDS THREE MORE LEVELS TO THE LANGUAGE HIERARCHY

; LA language (just L0 without tags)

#; (datum d)
#; (var id)
#; (λ (id) body)
#; (app f a)
#; (set! id e)

; LB language


#; (datum d)
#; (var id)
#; (λ (id ...) body)
#; (app f a)
#; (set! id e)
#; (let ([id init] ...) body)
#; (letrec ([id init]) body)
#; (define <id> <init>)
#; (define (<f> <a> ...) body)
#; (block e ...)


; Rx language

#; (datum d)
#; (var id)
#; (λ (id ...) body ...)
#; (app f a)
#; (set! id e)
#; (let ([id init] ...) body ...)
#; (letrec ([id init]) body ...)
#; (define (<f> <a> ...) body ...)
#; (begin e ...)
#| begin: semantics of differ from Racket
   this is just a block of expr-or-defs |#
#; (check binary-pred? ((label expr expr) ...))


(define (LA→L0 e)
  (match e
    [`(λ (,id) ,body)
     `(L0: λ (,id) ,(LA→L0 body))]
    [`(app ,f ,a)
     `(L0: app ,(LA→L0 f) ,(LA→L0 a))]
    [`(var ,v)
     `(L0: var ,v)]
    [`(datum ,d)
     `(L0: datum ,d)]
    [`(set! ,id ,init)
     `(L0: set! ,id ,(LA→L0 init))]
    [`(if ,a ,b ,c)
     `(L0: if ,@(map LA→L0 `(,a ,b ,c)))]))


; todo: nested fn definition form


(define Rx→LB ; a.k.a. explicitize
  (match-lambda
    [`(check ,bin-pred? ((,label ,e0 ,e1) ...))
     `(check ,(Rx→LB bin-pred?) (,@(map (λ (x y z) `[,x ,(Rx→LB y) ,(Rx→LB z)]) label e0 e1)))]
    [`(begin ,body ...)
     `(block ,@(map Rx→LB body))]
    [`(λ (,id ...) ,body)
     `(λ (,@id) ,(Rx→LB body))]
    [`(λ (,id ...) ,body ...)
     `(λ (,@id) (block ,@(map Rx→LB body)))]
    [`(set! ,id ,init)
     `(set! ,id ,(Rx→LB init))]
    [`(if ,a ,b ,c)
     `(if ,(Rx→LB a) ,(Rx→LB b) ,(Rx→LB c))]
    [`(let ([,id ,init] ...) ,body ...)
     `(let (,@(map (λ (x y) `[,x ,(Rx→LB y)]) id init)) (block ,@(map Rx→LB body)))]
    [`(letrec ([,id ,init] ...) ,body ...)
     `(letrec ([,id ,init] ...) (block ,@(map Rx→LB body)))]
    [`(define (,f ,as ...) ,body)
     `(define (,f ,@as) ,(Rx→LB body))]
    [`(define (,f ,as ...) ,body ...)
     `(define (,f ,as ...) (block ,@(map Rx→LB body)))]
    [`(define ,id ,val)
     `(define ,id ,(Rx→LB val))]
    [`(,f ,as ...)
     `(app ,(Rx→LB f) ,@(map Rx→LB as))]
    [(? symbol? a) `(var ,a)]
    [(? number? n) `(datum ,n)]))


(module+ test
  (check-equal? (Rx→LB '(let ([a 1] [b 2]) a b))
                '(let ((a (datum 1)) (b (datum 2))) (block (var a) (var b))))

  (check-equal? (Rx→LB '(begin (define (fib n) (if (< n 2) 1 (+ (fib (+ n -1)) (fib (+ n -2)))))
                               (fib 5)))
                '(block (define (fib n) (if (app (var <) (var n) (datum 2)) (datum 1) (app (var +) (app (var fib) (app (var +) (var n) (datum -1))) (app (var fib) (app (var +) (var n) (datum -2))))))
                        (app (var fib) (datum 5))))
  )



(define LB→LA-check
  (match-lambda
    [`(check ,bin-pred? ((,label ,e0 ,e1)))
     `(if (app ,bin-pred? ,e0 ,e1) (datum 0) (datum ,label))]
    [`(check ,bin-pred? ((,label ,e0 ,e1) ,rest ...))
     `(if (app ,bin-pred? ,e0 ,e1) (check ,bin-pred? (,@rest)) (datum ,label))]))

(define LB→LA-if
  (match-lambda
    [`(if ,con ,a ,b)
     `(if ,con ,a ,b)
     #; `(app (app ,con (λ (_) ,a)) (λ (_) ,b))]))

(define LB→LA-multiλ
  (match-lambda
    [`(λ (,a ,b ,cs ...) ,body)
     `(λ (,a) (λ (,b ,@cs) ,body))]))

(define LB→LA-app
  (match-lambda
    [`(app ,f ,a ,bs ... ,c)
     `(app (app ,f ,a ,@bs) ,c)]))

(define LB→LA-let
  (match-lambda
    [`(let ((,id ,val) ...) ,body)
     `(app (λ (,@id) ,body) ,@val)]))

(define LB→LA-letrec
  (match-lambda
    [`(letrec ([,id ,init] ...) ,body)
     `(let (,@(map (λ (id) `[,id (datum 666)]) id))
        (block ,@(map (λ (x y) `(set! ,x ,y)) id init)
               ,body))]))

#;(define LB→LA-negate
    (match-lambda
      [`(neg ,a)
       (- a)]))

(define (LB→LA-define-f e)
  (match e
    [`(block ,xs ... (define (,f ,as ...) ,body) ,ys ...)
     `(block ,@xs (define ,f (λ (,@as) ,body)) ,@ys)]))

(define LB→LA-block
  (match-lambda
    [`(block ,a ,b ,cs ...)
     `(app (λ (dummy) (block ,b ,@cs)) ,a)]
    [`(block ,a)
     a]))

(define (LB→LA-define-block e)
  (define filter-inits
    (match-lambda
      [`(define ,id ,init) `((,id (datum 666)))]
      [_ '()]))
  (define make-sets
    (match-lambda
      [`(define ,id ,init) `(set! ,id ,init)]
      [x x]))
  (match e
    [`(block ,xs ...)
     `(let ,(append-map filter-inits xs)
        (block ,@(map make-sets xs)))]))


(define (LB→LA stx)
  (match stx
    [`(check ,bin-pred? ((,label ,e0 ,e1) ,rest ...))
     (LB→LA (LB→LA-check stx))]
    [`(block ,xs ... (define (,f ,as ...) ,body) ,ys ...)
     (LB→LA (LB→LA-define-f stx))]
    [`(block ,xs ... (define ,a ,b) ,ys ...)
     (LB→LA (LB→LA-define-block stx))]
    ; does below make sense to get rid of block?
    [`(block ,(not `(define ,a ,b)) ...)
     (LB→LA (LB→LA-block stx))]
    [`(let ,bindings ,body)
     (LB→LA (LB→LA-let stx))]
    [`(letrec ([,id ,init] ...) ,body)
     (LB→LA (LB→LA-letrec stx))]
    [`(λ (,a ,b ,cs ...) ,body)
     (LB→LA (LB→LA-multiλ stx))]
    [`(app ,f ,a ,bs ... ,c)
     (LB→LA (LB→LA-app stx))]
    #;[`(if ,a ,b ,c)
       (LB→LA (LB→LA-if stx))]
    [(? list?) (map LB→LA stx)]
    [_ stx]))







(module+ test

  (check-equal? (LB→LA '(check < ((1 2 3))))
                (LB→LA '(if (app < 2 3) (datum 0) (datum 1))))

  (check-equal? (LB→LA '(check < ((1 2 3) (2 3 2))))
                (LB→LA '(if (app < 2 3) (if (app < 3 2) (datum 0) (datum 2)) (datum 1))))
  
  (check-equal? (LB→LA '(letrec ([f (λ (x) (app f x))]) (app f 0)))
                (LB→LA '(let ([f (datum 666)]) (block (set! f (λ (x) (app f x))) (app f 0)))))

  (check-equal? (LB→LA '(letrec ([f (λ (x) (app f x))] [g 1]) (app f 0)))
                (LB→LA '(let ([f (datum 666)] [g (datum 666)]) (block (set! f (λ (x) (app f x))) (set! g 1) (app f 0)))))
  
  (check-equal? (LB→LA '(let ([a 1] [b 2]) 0))
                (LB→LA '(app (λ (a b) 0) 1 2)))

  (check-equal? (LB→LA '((λ (a b) 0) 1 2))
                (LB→LA '((λ (a) (λ (b) 0)) 1 2)))
                            
  (check-equal? (LB→LA '(block 1 2 3))
                (LB→LA '(app (λ (dummy) (app (λ (dummy) 3) 2)) 1)))

  (check-equal? (LB→LA '(block 1))
                (LB→LA '1))
  
  (check-equal? (LB→LA '(block (define f 1) (+ f 1) (define g 2)))
                (LB→LA '(let ((f (datum 666)) (g (datum 666)))
                          (block (set! f 1) (+ f 1) (set! g 2)))))

  (check-equal? (LB→LA '(block (define (fib n) (if (< n 2) 1 (+ (fib (+ n -1)) (fib (+ n -2)))))
                               (fib 5)))
                (LB→LA '(let ((fib (datum 666)))
                          (block
                           (set! fib (λ (n) (if (< n 2) 1 (+ (fib (+ n -1)) (fib (+ n -2))))))
                           (fib 5)))))

  (check-equal? (LB→LA '(if (λ (x) (λ (y) y))
                            "a"
                            "b"))
                (LB→LA '(if (λ (x) (λ (y) y)) "a" "b")))

  (check-equal? (LB→LA (Rx→LB '(begin (define (fib n)
                                        (if (< n 2)
                                            1
                                            (+ (fib (+ n -1)) (fib (+ n -2)))))
                                      (fib 5))))
                (LB→LA '(app (λ (fib)
                               (block (set! fib (λ (n)
                                                  (if (app (app (var <) (var n)) (datum 2))
                                                      (datum 1)
                                                      (app (app (var +)
                                                                (app (var fib)
                                                                     (app (app (var +)
                                                                               (var n))
                                                                          (datum -1))))
                                                           (app (var fib)
                                                                (app (app (var +)
                                                                          (var n))
                                                                     (datum -2)))))))
                                      (app (var fib) (datum 5))))
                             (datum 666))))

  )