#lang racket #| Macros and Runtime Defining Language M0 |#

(provide T:*id* T:*datum*
         T:set! T:if
         T:λ T:*app*
         T:block
         T:let T:local
         T:cond T:when T:while
         T:breakable T:continuable T:returnable
         T:and T:or
         Ts ; List of all the transformations, defined after all of them.
         standard-library
         M0→L0)

(require "A2.L0.rkt")

; Compile an M0 expression to an L0 expression.
(define (M0→L0 e)
  (expand (standard-library e) Ts))

#| Language M0
   ===========

 M0 is really a language and standard library: the language is essentially an extension of L0,
  that macro-compiles to L0, along with a small standard library written in that language.

 M0 is meant to be for humans to write programs in, so we won't tag it.

 There are seventeen kinds of expression, corresponding to the provides of the form ‘T:<id>’ above.
 For each of those, except T:*id*, T:*datum*, and T:*app*, there's an M0 expression (<id> <part> ...).

 Function application is then any other expression of the form: (<part> ...).
   During expansion, the macro system turns that into (*app* <id>), for T:*app* to transform.

 A simple identifier is an M0 expression meaning variable access.
   During expansion, the macro system turns that into (*id* <id>), for T:*id* to transform.

 An integer is an M0 expression meaning a constant.
  During expansion, the macro system turns that into (*datum* <id>), for T:*datum* to transform.

 It's assumed that M0 programmers will not use identifiers surrounded by asterisks. |#

; An M0 expression:
#;(λ (f a b)
    (set! a b)
    (f a 488))
; During expansion, when some parts of that are expanded, they'll be treated as if that was:
#;(λ (f a b)
    (set! a (*id* b)) ; typo??? a should be id
    (*app* (*id* f) (*id* a) (*datum* 488)))

#| Compiling M0 to L0
   ==================

 Implement:

   1. The transformers mentioned in the ‘provide’ above, for the forms described below.

   2. The standard library, which is a function that attaches the library to an expression,
       described at the end of this file.

 Each transformer ‘T:<id>’ transforms code of the form ‘(<id> <e> ...)’.

 In the given patterns:
   ‘...’  means zero or more of the previous component
   ‘...+’ means one  or more of the previous component

 Other than T:*id*, T:*datum*, T:set!, T:*if*, T:*λ*, and T:*app*, transformers should not
  directly produce L0 forms.

 New identifiers introduced by transformations
 ---------------------------------------------
 In some of your templates you will find that you need to make up the name of an identifier.

 For new “dummy” identifiers that aren't referenced, use ‘_’.
   • we'll assume that user code does not reference any identifier with that name either

 For temporary identifiers that are referenced, use an identifier of the form "*<id>*",
  i.e. surrounded by asterisks. But don't use the names ‘*app*’, ‘*id*’, nor ‘*datum*’.
   • it will be assumed that user code does not use such identifiers |#

(define-syntax-rule (blueprint f [in out] ...)
  (module+ test
    (require rackunit)
    (check-equal? (f in) out) ...))

; *id* *datum* set! if
; --------------------
; The following two special cases should expand to integers 0 and 1:
#;(*id* false)
#;(*id* true)
; Otherwise, they are straightforward untagged versions of L0 expressions.
; Transform those directly to their L0 form.


(blueprint (transformer-function T:*id*)
  [`(*id* false)
   `(L0: datum 0)]
  [`(*id* true)
   `(L0: datum 1)]
  [`(*id* fake-id)
   `(L0: var fake-id)])
(define-transformer T:*id* *id*
  [`(*id* false)
   `(L0: datum 0)]
  [`(*id* true)
   `(L0: datum 1)]
  [`(*id* ,id)
   `(L0: var ,id)])


(blueprint (transformer-function T:*datum*)
  [`(*datum* 666)
   `(L0: datum 666)])
(define-transformer T:*datum* *datum*
  [`(*datum* ,d)
   `(L0: datum ,d)])


(blueprint (transformer-function T:set!)
  [`(set! x 0)
   `(L0: set! x 0)])
(define-transformer T:set! set!
  [`(set! ,id ,init)
   `(L0: set! ,id ,init)])


(blueprint (transformer-function T:if)
  [`(if 0 1 2)
   `(L0: if 0 1 2)])
(define-transformer T:if if
  [`(if ,a ,b ,c)
   `(L0: if ,a ,b ,c)])

; λ
; -
; Extends L0's λ by:
;   allowing more than one body expression
;   allowing zero parameters, and more than one parameter
;
; Transform within the M0 language by wrapping the body in a ‘block’,
;  adding a dummy parameter if there are zero parameters,
;  and currying if there are two or more parameters.
; Transform the unary single-body-expression form to the L0 form.

(blueprint (transformer-function T:λ)
  [`(λ (x y z) 0)
   `(λ (x) (λ (y z) 0))]
  [`(λ () 0 1)
   `(λ () (block 0 1))]
  [`(λ () 0 )
   `(λ (_) 0)]
  [`(λ (x) 0)
   `(L0: λ (x) 0)])

(define-transformer T:λ λ
  [`(λ (,a ,b ..1) ,body ...)
   `(λ (,a) (λ (,@b) ,@body))]
  [`(λ ,a ,body ..2)
   `(λ ,a (block ,@body))]
  [`(λ () ,body ...)
   `(λ (_) ,@body)]
  [`(λ (,a) ,body)
   `(L0: λ (,a) ,body)])



; *app*
; -----
; Extends L0's app by allowing zero arguments, or more than one argument.
; Transform the form with more than one argument into its curried equivalent.
; Transform the no-argument form into a one-argument form with a dummy argument [see ‘block’].
; Transform the unary form to the L0 form.
(blueprint (transformer-function T:*app*)
  [`(*app* f a b c)
   `(*app* (*app* f a b) c)]
  [`(*app* f)
   `(*app* f 0)] ; dummy
  [`(*app* f 0)
   `(L0: app f 0)])

(define-transformer T:*app* *app*
  [`(*app* ,f ,as ..1 ,a)
   `(*app* (*app* ,f ,@as) ,a)]
  [`(*app* ,f)
   `(*app* ,f 0)] ; dummy
  [`(*app* ,f ,a)
   `(L0: app ,f ,a)])



; block
; -----
#;(block <e>
         ...)
; A sequence of zero or more expressions to be evaluated in order,
;  producing the value of the last expression,
;  or the integer 0 if there are none.
;
; Transform the form with no expressions to the integer 0.
; Transform the form with one expression to just the expression.
; Transform the form with more than one expression to a ‘let’ naming the first expression
;  with a dummy variable.
;
; For other M0 forms that need dummy values [e.g. as mentioned for *app*], use (block) for
;  the dummy value.

(blueprint (transformer-function T:block)
  [`(block 1 2 3)
   `(let ([_ 1]) 2 3)]
  [`(block 1)
   1]
  [`(block)
   0])

(define-transformer T:block block
  [`(block ,a ,b ..1)
   `(let ([_ ,a]) ,@b)]
  [`(block ,a)
   a]
  [`(block)
   0])


; let
; ---
#;(let ([<id> <init>]
        ...+)
    <body>
    ...+)
; Evaluates the <init>s in order, then introduces the distinctly named local variables <id>s,
;  initialized by the values of the <init>s, then evaluates the <body>s as a block.
;
; Transform using the standard LC transformation: to an expression that makes and immediately calls
;  a function.

(blueprint (transformer-function T:let)
  [`(let ((a 1) (b 2)) 3 4)
   `(*app* (λ (a b) 3 4) 1 2)])

(define-transformer T:let let
  [`(let ((,id ,val) ..1) ,body ..1)
   `(*app* (λ (,@id) ,@body) ,@val)])

; local
; -----
#;(local [(define (<f-id> (<id> ...)) ; is this a typo!!!!
            <f-body>
            ...+)
          ...+]
    <body>
    ...+)
; Introduces the distinctly named local <f-id>s into scope, to functions created in that scope,
;  then evaluates the <body>s as a block.
;
; Transform using the standard LC+set! transformation: to an expression that initializes
;  all the <f-id>s to dummy values, sets them to their functions, then evaluates the body.


(blueprint (transformer-function T:local)
  [`(local [(define (g) 0)]
      0)
   `(let ([g 0])
      (set! g (λ () 0))
      0)]
  [`(local [(define (f x y) (+ x y))
            (define (g) 0)]
      (f 0 (g)))
   `(let ([f 0] [g 0])
      (set! f (λ (x y) (+ x y)))
      (set! g (λ () 0))
      (f 0 (g)))])

(define-transformer T:local local
  [`(local [(define (,f ,as ...)
              ,f-body ..1) ..1]
      ,body  ..1)
   `(let (,@(map (λ (x) `[,x 0]) f))
      ,@(map (λ (x ys zs) `(set! ,x (λ (,@ys) ,@zs))) f as f-body)
      ,@body)])


; and or
; ------
#;(and <e0> <e> ...+)
#;(or  <e0> <e> ...+)
; Standard short-circuiting operators for two or more boolean expressions.
; Transform to ‘if’s or ‘cond’s.

(blueprint (transformer-function T:and)
  [`(and 1 2 3)
   `(if 1
        (and 2 3)
        false)]
  #;[`(and)
     true])
(define-transformer T:and and
  [`(and ,a ,as ..2)
   `(if ,a
        (and ,@as)
        false)]
  [`(and ,a ,b)
   `(if ,a
        ,b
        false)]
  #;[`(and)
     true])

(blueprint (transformer-function T:or)
  [`(or 1 2 3)
   `(if 1
        true
        (or 2 3))]
  #;[`(or)
     false])
(define-transformer T:or or
  [`(or ,a ,as ..2)
   `(if ,a
        true
        (or ,@as))]
  [`(or ,a ,b)
   `(if ,a
        true
        ,b)]
  #;[`(or)
     false])


; cond
; ----
#;(cond [<condition> <result>
                     ...+]
        ...+)
#;(cond [<condition> <result>
                     ...+]
        ...
        [else <else-result>
              ...+])
; Evaluates the boolean <condition>s in order, until the first true one or possibly the else,
;  then evaluates the corresponding <result>s as a block.
;
; Transform using ‘if’s, ‘when’s, and/or ‘block’s.
(blueprint (transformer-function T:cond)
  [`(cond [(stupid? dog) yell]
          [(ugly? cat) ignore pyschologically-undermine])
   `(if (stupid? dog)
        (block yell)
        (cond [(ugly? cat) ignore pyschologically-undermine]))]
  [`(cond [else 0])
   `(block 0)])

(define-transformer T:cond cond
  [`(cond [,c ,rs ..1]
          [,cs ,rss ..1] ..1)
   `(if ,c
        (block ,@rs)
        (cond ,@(map (λ (x ys) `[,x ,@ys]) cs rss)))]
  [`(cond [else ,rs ..1])
   `(block ,@rs)]
  [`(cond [,c ,rs ..1])
   `(if ,c
        (block ,@rs)
        0)] ; dummy value
  )

; when
; ----
#;(when <condition>
    <body>
    ...+)
; If boolean <condition> is true evaluates the <body>s as a block, otherwise produces a dummy value.

(blueprint (transformer-function T:when)
  [`(when (dirty? hog)
      avoid)
   `(if (dirty? hog) (block avoid) 0)])

(define-transformer T:when when
  [`(when ,c
      ,body ..1)
   `(if ,c (block ,@body) 0)])

; while
; -----
#;(while <condition>
         <body>
         ...+)
; A standard while loop.
; Transform to a recursive no-argument function that is immediately called.

(blueprint (transformer-function T:while)
  [`(while (fiery? furnace)
           mash-ash)
   `(local [(define (**)
              mash-ash
              (when (fiery? furnace) (**)))]
      (**))])

(define-transformer T:while while
  [`(while ,c
           ,body ..1)
   `(local [(define (**)
              ,@body
              (when ,c (**)))]
      (**))])


; returnable breakable continuable
; --------------------------------
#;(returnable <e>
              ...+)
#;(breakable <e>
             ...+)
#;(continuable <e>
               ...+)
; Evaluates the <e>s as a block, in a local scope containing the identifier ‘return’,
;  ‘break’, or ‘continue’ bound to the continuation that escapes the entire expression.
; These are meant to be used manually by the programmer: around a function body, loop, or loop body,
;  to return early, break, or continue.

(define-transformer T:returnable returnable
  [`(returnable ,expr ..1)
   `(call/ec (λ (k) (let ([return k]) ,@expr)))])
(define-transformer T:breakable breakable
  [`(breakable ,expr ..1)
   `(call/ec (λ (k) (let ([break k]) ,@expr)))]) ; so i'd need to provide a value e.g. (break 7)?
(define-transformer T:continuable continuable
  [`(continuable ,expr ..1)
   `(call/ec (λ (k) (let ([continue k]) ,@expr)))])


; List of all the transformations.
(define Ts (list T:*id* T:*datum*
                 T:set! T:if
                 T:λ T:*app*
                 T:block
                 T:let T:local
                 T:cond T:when T:while
                 T:breakable T:continuable T:returnable
                 T:and T:or))

; Standard Library
; ----------------
; Add definitions for the functions described by the comments in the body.
(define (standard-library e)
  `(local [
           ; Boolean logic
           ; -------------
           ; (not b) : the negation of b, implemented with ‘if’
           (define (not b)
             (if b
                 false
                 true))

           (define (>= a b)
             (not (< a b)))
           (define (= a b)
             (and (>= a b)
                  (< a (+ 1 b))))
           ; Arithmetic
           ; ----------
           ; (- a b) : the difference between a and b
           (define (⊖ a)
             (* -1 a))
           ; THESE NEED TESTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           (define (- a b) (+ a (⊖ b)))
           ; (⊖ a) : the negative of a
           ; (> a b) : whether a is greater than b
           ; (>= a b) : whether a is greater than or equal to b
           ; (= a b) : whether a is equal to b
           
           
           (define (<= a b)
             (or (< a b)
                 (= a b)))
           (define (> a b)
             (not (<= a b)))
           
           ]
     ,e))

(blueprint M0→L0
  ['false '(L0: datum 0)]
  ['true '(L0: datum 1)]
  ['a '(L0: var a)]
  ['0 '(L0: datum 0)]
  ['(set! x 0) '(L0: set! x (L0: datum 0))]
  ['(if 0 1 2) '(L0: if (L0: datum 0) (L0: datum 1) (L0: datum 2))]
  [`(λ (x y z) 0) '(L0: λ (x)
                        (L0: λ (y)
                             (L0: λ (z)
                                  (L0: datum 0))))]
  [`(λ () 0) '(L0: λ (_)
                   (L0: datum 0))]
  ['(f a b c) '(L0: app
                    (L0:  app
                          (L0: app
                               (L0: var f)
                               (L0: var a))
                          (L0: var b))
                    (L0: var c))]
  ['(block 1 2 3) '(L0: app
                        (L0: λ (_)
                             (L0: app
                                  (L0: λ (_)
                                       (L0: datum 3))
                                  (L0: datum 2)))
                        (L0: datum 1))]
  [`(let ((a 1) (b 2)) 3 4) '(L0:
                              app
                              (L0:
                               app
                               (L0:
                                λ
                                (a)
                                (L0:
                                 λ
                                 (b)
                                 (L0:
                                  app
                                  (L0:
                                   λ
                                   (_)
                                   (L0: datum 4))
                                  (L0:
                                   datum
                                   3))))
                               (L0: datum 1))
                              (L0: datum 2))]
  ['(local [(define (g) 0)] 0) '(L0:
                                 app
                                 (L0:
                                  λ
                                  (g)
                                  (L0:
                                   app
                                   (L0:
                                    λ
                                    (_)
                                    (L0: datum 0))
                                   (L0:
                                    set!
                                    g
                                    (L0:
                                     λ
                                     (_)
                                     (L0:
                                      datum
                                      0)))))
                                 (L0: datum 0))]
  [`(and 1 2 3) '(L0: if
                      (L0: datum 1)
                      (L0: if
                           (L0: datum 2)
                           (L0: datum 3)
                           (L0: datum 0))
                      (L0: datum 0))]
  [`(or 1 2 3) '(L0: if
                     (L0: datum 1)
                     (L0: datum 1)
                     (L0: if
                          (L0: datum 2)
                          (L0: datum 1)
                          (L0: datum 3)))]
  [`(cond [(a? d) y] [(u? c) i]) '(L0:
                                   if
                                   (L0:
                                    app
                                    (L0: var a?)
                                    (L0: var d))
                                   (L0: var y)
                                   (L0:
                                    if
                                    (L0:
                                     app
                                     (L0: var u?)
                                     (L0: var c))
                                    (L0: var i)
                                    (L0: datum 0)))]
  [`(cond [else 1]) '(L0: datum 1)]
  [`(when (dirty? hog) avoid) '(L0:
                                if
                                (L0:
                                 app
                                 (L0: var dirty?)
                                 (L0: var hog))
                                (L0: var avoid)
                                (L0: datum 0))]
  [`(while (fiery? furnace) mash-ash) '(L0:
                                        app
                                        (L0:
                                         λ
                                         (**)
                                         (L0:
                                          app
                                          (L0:
                                           λ
                                           (_)
                                           (L0:
                                            app
                                            (L0: var **)
                                            (L0: datum 0)))
                                          (L0:
                                           set!
                                           **
                                           (L0:
                                            λ
                                            (_)
                                            (L0:
                                             app
                                             (L0:
                                              λ
                                              (_)
                                              (L0:
                                               if
                                               (L0:
                                                app
                                                (L0:
                                                 var
                                                 fiery?)
                                                (L0:
                                                 var
                                                 furnace))
                                               (L0:
                                                app
                                                (L0:
                                                 var
                                                 **)
                                                (L0:
                                                 datum
                                                 0))
                                               (L0:
                                                datum
                                                0)))
                                             (L0:
                                              var
                                              mash-ash))))))
                                        (L0: datum 0))]
  
  #;[0 0])