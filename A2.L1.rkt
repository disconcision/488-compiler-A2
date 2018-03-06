#lang racket #| Compile A2's Language L0 to A2's Language L1 |#

(provide debruijn index L0→L1)
(module+ test (require rackunit))
(define-syntax-rule (blueprint f [in out] ...)
  (module+ test
    (require rackunit)
    (check-equal? (f in) out) ...))
#| A2's language L0 is A1's language L0 with one additional conditional expression. |#

; If <e1> is true then evaluate <e2>, else evaluate <e3>.
#;(L0: if <e1> <e2> <e3>)

#| A2's language L1 is A1's language L1 with one additional conditional expression. |#

; The nth if expression.
#;(L1: if <n> <e1> <e2> <e3>)

#| DeBruijn Indexing of an L0 Expression

 This is the same as in A1. |#

#;(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
    (define (debruijn′ e) (debruijn e env))
    e)

#| Indexing of a Debruijnized L0 Expression

 For the A1 subset of L0 this is the same.
 The new conditional expressions are also given unique indices. |#

#;(define ((counter [c 0]))
    (set! c (add1 c))
    (sub1 c))

; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.
#;(define (index e [λ-count (counter)] [if-count (counter)])
    (define (index′ e) (index e λ-count if-count))
    e)

#| L0→L1

 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

#;(define (L0→L1 e)
    (define (L0→L1′ e) e)
    (L0→L1′ (index (debruijn e))))


; ----------------------------------------------



(define (L-1→L0 e)
  (match e
    [`(λ (,id) ,body)
     `(L0: λ (,id) ,(L-1→L0 body))]
    [`(if ,a ,b ,c)
     `(L0: if ,@(map L-1→L0 `(,a ,b ,c)))]
    [`(,f ,a)
     `(L0: app ,(L-1→L0 f) ,(L-1→L0 a))]
    [(? symbol?)
     `(L0: var ,e)]
    [(? number?)
     `(L0: datum ,e)]
    [`(set! ,id ,init)
     `(L0: set! ,id ,(L-1→L0 init))]))

(define (L0→L-1 e)
  (match e
    [`(L0: λ (,id) ,body)
     `(λ (,id) ,(L0→L-1 body))]
    [`(L0: λ ,id ,body)
     `(λ ,id ,(L0→L-1 body))]
    [`(L0: λ ,body)
     `(λ ,(L0→L-1 body))]
    [`(L0: if ,a ,b ,c)
     `(if ,@(map L0→L-1 `(,a ,b ,c)))]
    [`(L0: if ,_ ,a ,b ,c)
     `(if ,@(map L0→L-1 `(,a ,b ,c)))]
    [`(L0: app ,f ,a)
     `(,(L0→L-1 f) ,(L0→L-1 a))]
    [`(L0: var ,a)
     a]
    [`(L0: datum ,a)
     a]
    [`(L0: set! ,id ,init)
     `(set! ,id ,(L0→L-1 init))]))



; ----------------------------------------------



#| Language L0 is a slightly enriched version of the LC. |#
#;(L0: λ (<id>) <e>)
#;(L0: app <e1> <e2>)
#;(L0: var <id>)
#;(L0: datum <i>)
#;(L0: set! <id> <e>)
#;(L0: if <e1> <e2> <e3>) ; If <e1> is true then evaluate <e2>, else evaluate <e3>.
;  where <e>, <e1>, <e2>, <e3> are L0 expressions, <id> is an identifier, and <i> is an integer.

#| Semantics of language L0 |#
#;(L0: λ (<id>) <e>)  ; Unary closure creation.
#;(L0: app <e1> <e2>) ; Unary function call.
#;(L0: var <id>)      ; Variable access.
#;(L0: datum <i>)     ; Integer constant.
#;(L0: set! <id> <e>) ; Variable mutation: set <id> to the result of <e>.


#| Language L1.
   In the following, <n> is a natural number.|#
#;(L1: λ <n> <e>)     ; The nth lambda in an L1 expression.
#;(L1: app <e1> <e2>) ; Same meaning as in L0.
#;(L1: var <n>)       ; Reference to a variable <n> scopes up.
#;(L1: var <id>)      ; Free/unbound/open variable reference to <id>.
#;(L1: set! <n> <e>)  ; Set the variable <n> scopes up to the value of <e>.
#;(L1: datum <i>)     ; Same meaning as in L0.
#;(L1: if <n> <e1> <e2> <e3>) ; The nth if expression.


#| DeBruijn Indexing of an L0 Expression

 Replaces each variable referencing a parameter, with a natural number indicating how many scopes up
  the variable is. Free variables are left alone: in later passes they will turn into references to
  pre-defined functions. |#

(blueprint debruijn-t
  ['(λ (x) (λ (y) (λ (z) z)))
   '(λ (λ (λ 0)))]
  ['(λ (x) (λ (y) (λ (z) y)))
   '(λ (λ (λ 1)))]
  ['(λ (x) (λ (y) (λ (z) x)))
   '(λ (λ (λ 2)))]
  ['(λ (x) (x (λ (y) (y x))))
   '(λ (0 (λ (0 1))))]
  ['(λ (x) ((λ (a) (a (x x))) (λ (y) ((λ (z) y) x))))
   '(λ ((λ (0 (1 1))) (λ ((λ 1) 1))))])

(define (debruijn e [env '()])
  (define D (curryr debruijn env))
  (define inrc-binds
    (match-lambda [`(,a ,b) `(,a ,(add1 b))]))
  (define (lookup id env)
    (let ([bind (assoc id env)])
      (if bind
          (second bind)
          (error "no identifier" id))))
  (match e
    [`(L0: λ (,id) ,body)
     `(L0: λ ,(debruijn body `((,id 0) ,@(map inrc-binds env))))]
    [`(L0: app ,f ,a)
     `(L0: app ,(D f) ,(D a))]
    [`(L0: var ,(or '+ '* '< 'call/ec)) ; support other unbound vars?
     e]
    [`(L0: var ,a)
     `(L0: var ,(lookup a env))]
    [`(L0: datum ,d)
     `(L0: datum ,d)]
    [`(L0: set! ,id ,init)
     `(L0: set! ,(lookup id env) ,(D init))]
    [`(L0: if ,a ,b ,c)
     `(L0: if ,@(map D `(,a ,b ,c)))]
    )
  )


(define debruijn-t (compose L0→L-1 debruijn L-1→L0))




#| Indexing λs of a Debruijnized L0 Expression

 Replaces each L0 λ with an L1 λ, replacing the parameter list with a numeric index.
 Indexing starts at the value produced by the optional counter argument count, and is applied
  post-order when considering the expression as a tree. |#

; A class to make counting objects.
(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

(module+ test
  (define c (counter))
  (check-equal? (c) 0)
  (check-equal? (c) 1)
  (define c′ (counter))
  (check-equal? (c′) 0)
  (check-equal? (c) 2))


(blueprint (compose L0→L-1 index debruijn L-1→L0)
  
  ['(λ (x) (λ (y) x))
   '(λ 1 (λ 0 1))]
  ['(λ (x) (x (λ (y) (y x))))
   '(λ 1 (0 (λ 0 (0 1))))]
  ['(λ (x) ((λ (y) x) (λ (z) x)))
   '(λ 2 ((λ 0 1) (λ 1 1)))]

  ; set
  ['(λ (x) (set! x 1))
   '(λ 0 (set! 0 1))]
  ['(λ (x) ((λ (y) x) (set! x 4)))
   '(λ 1 ((λ 0 1) (set! 0 4)))]
  
  ; descend into set init
  ['(λ (x) (set! x ((λ (x) x) x)))
   '(λ 1 (set! 0 ((λ 0 0) 0)))]
  
  ; shadowing
  ['(λ (x) (λ (x) x))
   '(λ 1 (λ 0 0))]
  ['(λ (x) (x (λ (x) (λ (y) x))))
   '(λ 2 (0 (λ 1 (λ 0 1))))])


(blueprint (compose index debruijn L-1→L0)
  
  ['(λ (a) (λ (b) a))
   '(L0: λ 1 (L0: λ 0 (L0: var 1)))]

  ['(λ (a) (λ (b) b))
   '(L0: λ 1 (L0: λ 0 (L0: var 0)))]

  ['(if 3 4 5)
   '(L0: if 0 (L0: datum 3) (L0: datum 4) (L0: datum 5))]

  ['(if 3 (λ (x) 4) 5)
   '(L0: if 0 (L0: datum 3) (L0: λ 0 (L0: datum 4)) (L0: datum 5))]

  ['(if (if 3 (if 4 5 6) 7) (if 8 9 10) 11)
   '(L0: if 3
         (L0: if 1
              (L0: datum 3)
              (L0: if 0
                   (L0: datum 4)
                   (L0: datum 5)
                   (L0: datum 6))
              (L0: datum 7))
         (L0: if 2
              (L0: datum 8)
              (L0: datum 9)
              (L0: datum 10))
         (L0: datum 11))])


; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.
(define (index e [λ-count (counter)] [if-count (counter)])
  (define (I e) (index e λ-count if-count))
  (match e
    [`(L0: λ ,body)
     (let ([indexed-body (I body)])
       `(L0: λ ,(λ-count) ,indexed-body))]
    [`(L0: if ,a ,b ,c)
     (let ([indexed-body (map I `(,a ,b ,c))])
       `(L0: if ,(if-count) ,@indexed-body))]
    [`(L0: app ,f ,a)
     `(L0: app ,(I f) ,(I a))]
    [`(L0: set! ,id ,init)
     `(L0: set! ,id ,(I init))]
    [`(L0: var ,a)
     `(L0: var ,a)]
    [`(L0: datum ,a)
     `(L0: datum ,a)]))


#| L0→L1

 For an L0 expression: debruijnizes, indexes λs, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

(define (L0→L1 e)
  (define L0→L1′
    (match-lambda
      [`(L0: ,a ,as ...)
       `(L1: ,a ,@(map L0→L1′ as))]
      [x x]))
  (L0→L1′ (index (debruijn e))))

