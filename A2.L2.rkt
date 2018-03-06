#lang racket #| Compile L1 to sequential language L2 |#

(provide (struct-out compiled:L2) L1→L2)
(module+ test (require rackunit))

#| A2's language L2 is A1's language L2 with three additional expressions. |#

; Jumps to a named statement address, if result is false.
#;(L2: label <name>)      ; Name the current statement location.
#;(L2: jump <name>)       ; Jump/goto a statement location.
#;(L2: jump_false <name>) ; Jump/goto a statement location, when result is false.

#| Compiling L1 to L2 |#

; One additional compilation rule is for L1's new conditional.
#;{(L1: if <n> <e1> <e2> <e3>) →
                               code: {<code-for-e1>
                                      (L2: jump_false else_<n>)
                                      <code-for-e2>
                                      (L2: jump end_<n>)
                                      (L2: label else_<n>)
                                      <code-for-e3>
                                      (L2: label end_<n>)}
                               λs: {<λs-for-e1>
                                    <λs-for-e2>
                                    <λs-for-e3>}}

; A second compilation rule passes through references to an additional function.
#;{(L1: var call/ec) → (L2: closure call_ec)}

#| L1→L2 |#

#;(struct compiled:L2 (code λs) #:transparent)

; Produce a symbol of the form lambda_<n>.
#;(require (only-in racket/syntax format-symbol))
#;(define (lambda_ n)
    (format-symbol "lambda_~a" n))

#;(define (L1→L2 e)
    e)


; -------------------------------------------------------------------------


(provide (struct-out compiled:L2) L1→L2)
(module+ test (require rackunit))

#| Language L2

 This is a machine-like language, for a machine that supports enough to track the extended information
  in the memory model. Along with the support to model the LC, it has support for variable mutation,
  integer constants, and a few named lambdas.

 There are six types of statement in L2.
 All expression results move through a special result location. |#

#;(L2: closure <name>)
; Create a unary closure from the compiled body named <name>.
; Leave a reference to it in result.

#;(L2: variable <n>)
; Look up the variable <n> environments up from the current environment, put its value in result.

; Put the value of result on the stack.
#;(L2: push_result)

; Call the closure that is on the stack, with the argument that is in result.
#;(L2: call)

; Set the variable <n> environments up from the current environment, to the value of result.
#;(L2: set <n>)

; Put integer <i> in result.
#;(L2: set_result <i>)


#| Compiling L1 to L2

 From your tracing examples, you understand how to track the memory model via the above operations.

 Compiling an expression in L1 produces:
  • code: a list of L2 statements for the expression
  • λs: a list of two-element lists (<name> <code>) with the <name>s of the λs in the expression
     paired with the code for their bodies

 The following summarizes your understanding.
 These *descriptions* are slightly informal, in particular they blur the distinction between
  lists and individual elements of a list. Don't blindly turn them into an implementation! |#

#;{(L1: λ <n> <e>) →
                   code: (L2: closure lambda_<n>)
                   λs: {(lambda_<n> <code-for-e>)
                        <λs-for-e>}}

#;{(L1: app <e1> <e2>) →
                       code: {<code-for-e1>
                              (L2: push_result)
                              <code-for-e2>
                              (L2: call)}
                       λs: {<λs-for-e1>
                            <λs-for-e2>}}

#;{(L1: set! <n> <e>) →
                      code: {<code-for-e>
                             (L2: set <n>)}
                      λs: {<λs-for-e>}}

#;{(L1: if <n> <e1> <e2> <e3>) →
                               code: {<code-for-e1>
                                      (L2: jump_false else_<n>)
                                      <code-for-e2>
                                      (L2: jump end_<n>)
                                      (L2: label else_<n>)
                                      <code-for-e3>
                                      (L2: label end_<n>)}
                               λs: {<λs-for-e1>
                                    <λs-for-e2>
                                    <λs-for-e3>}}

; A second compilation rule passes through references to an additional function.
#;{(L1: var call/ec) → (L2: closure call_ec)}

; Each of the following produce a single L2 statement and no λs:
#;{(L1: var <n>) → (L2: variable <n>)}
#;{(L1: var +) → (L2: closure make_add)}
#;{(L1: var *) → (L2: closure make_multiply)}
#;{(L1: var <) → (L2: closure make_less_than)}
#;{(L1: datum <i>) → (L2: set_result <i>)}



#| L1→L2

 Each expression produces an instance of struct compiled:L2, which has two fields for code and λs. |#

(struct compiled:L2 (code λs) #:transparent)

(module+ test
  ; Along with making a constructor named ‘compiled:L2’ and also a pattern with that name,
  ;  the struct declaration implicitly defines two field accessor functions: ‘compiled:L2-code’
  ;  and ‘compiled:L2-λs’, which you might prefer sometimes instead of needing to name results
  ;  via pattern matching.
  (define c (compiled:L2 '((L2: set_result 123)) '((lambda_0 ((L2: variable 0))))))
  (check-equal? (match c [(compiled:L2 some-code some-λs) some-code])
                (compiled:L2-code c))
  (check-equal? (match c [(compiled:L2 some-code some-λs) some-λs])
                (compiled:L2-λs c)))

; Produce a symbol of the form lambda_<n>.
(define (lambda_ n)
  (local-require (only-in racket/syntax format-symbol))
  (format-symbol "lambda_~a" n))

(module+ test

  (define-syntax-rule (check f ((in out) ...))
    (begin (check-equal? (f in) out) ...))

  (check-equal? (lambda_ 123) 'lambda_123)

  (check L1→L2
         (('(L1: var 0)
           (compiled:L2 '((L2: variable 0)) '()))
          
          ('(L1: var +)
           (compiled:L2 '((L2: closure make_add)) '()))
          
          ('(L1: var *)
           (compiled:L2 '((L2: closure make_multiply)) '()))
          
          ('(L1: var <)
           (compiled:L2 '((L2: closure make_less_than)) '()))
          
          ('(L1: datum 0)
           (compiled:L2 '((L2: set_result 0)) '()))

          ('(L1: set! 0 (L1: datum 1))
           (compiled:L2 '((L2: set_result 1)
                          (L2: set 0)) '()))

          ('(L1: λ 1 (L1: var 0))
           (compiled:L2 '((L2: closure lambda_1))
                        '((lambda_1 ((L2: variable 0))))))

          ('(L1: app (L1: λ 1 (L1: var 0)) (L1: datum 2))
           (compiled:L2 '((L2: closure lambda_1)
                          (L2: push_result)
                          (L2: set_result 2)
                          (L2: call))
                        '((lambda_1 ((L2: variable 0))))))

          #;{(L1: if <n> <e1> <e2> <e3>) →
                                         code: {<code-for-e1>
                                                (L2: jump_false else_<n>)
                                                <code-for-e2>
                                                (L2: jump end_<n>)
                                                (L2: label else_<n>)
                                                <code-for-e3>
                                                (L2: label end_<n>)}
                                         λs: {<λs-for-e1>
                                              <λs-for-e2>
                                              <λs-for-e3>}}
          ('(L1: if 7 (L1: datum 0) (L1: λ 8 (L1: datum 1)) (L1: datum 2))
           (compiled:L2 '((L2: set_result 0)
                          (L2: jump_false else_7)
                          (L2: closure lambda_8)
                          (L2: jump end_7)
                          (L2: label else_7)
                          (L2: set_result 2)
                          (L2: label end_7) )
                        '((lambda_8 ((L2: set_result 1))))))
          

          ; church true
          ('(L1: λ 0 (L1: λ 1 (L1: var 1)))
           (compiled:L2
            '((L2: closure lambda_0))
            '((lambda_0 ((L2: closure lambda_1)))
              (lambda_1 ((L2: variable 1))))))

          ; church false
          ('(L1: λ 0 (L1: λ 1 (L1: var 0)))
           (compiled:L2
            '((L2: closure lambda_0))
            '((lambda_0 ((L2: closure lambda_1)))
              (lambda_1 ((L2: variable 0))))))
         
          )))

; shorthand for custom match expanders
(define-syntax-rule (define-match-rule (p ps ...) tem)
  (define-match-expander p
    (λ (stx)
      (syntax-case stx ()
        [(p ps ...)  #'tem]))))

(require (only-in racket/syntax format-symbol))

(define (L1→L2 e)
  (define-match-rule (L1→L2:: f a)
    (app L1→L2 (compiled:L2 f a)))
  (define (else_ n)
    (format-symbol "else_~a" n))
  (define (end_ n)
    (format-symbol "end_~a" n))
  ; sub-function for cases which produce no λs
  (define parse-base
    (match-lambda
      [`(datum ,a)
       `(set_result ,a)]
      [`(var +)
       `(closure make_add)]
      [`(var *)
       `(closure make_multiply)]
      [`(var <)
       `(closure make_less_than)]
      [`(var call/ec)
       `(closure call_ec)]
      [`(var ,a)
       `(variable ,a)]
      [_ #f]))
  ; sub-function for cases which produce λs
  (define parse-comp
    (match-lambda
      [`(app ,(L1→L2:: f-code f-λs)
             ,(L1→L2:: a-code a-λs))
       (compiled:L2 `(,@f-code
                      (L2: push_result)
                      ,@a-code
                      (L2: call))
                    `(,@f-λs ,@a-λs))]
      [`(λ ,id
          ,(L1→L2:: body-code body-λs))
       (compiled:L2 `((L2: closure ,(lambda_ id)))
                    `((,(lambda_ id) ,body-code)
                      ,@body-λs))]
      [`(set! ,id
              ,(L1→L2:: init-code init-λs))
       (compiled:L2 `(,@init-code
                      (L2: set ,id))
                    init-λs)]
      [`(if ,id
            ,(L1→L2:: a-code a-λs)
            ,(L1→L2:: b-code b-λs)
            ,(L1→L2:: c-code c-λs))
       (compiled:L2 `(,@a-code
                      (L2: jump_false ,(else_ id))
                      ,@b-code
                      (L2: jump ,(end_ id))
                      (L2: label ,(else_ id))
                      ,@c-code
                      (L2: label ,(end_ id)))
                    `(,@a-λs ,@b-λs ,@c-λs))]))
  ; banish prefixes
  (define >L1:<
    (match-lambda [`(L1: ,xs ...) xs]))
  ; repatriate prefixes
  (define <L2:>
    (match-lambda [xs `((L2: ,@xs))]))
  ; poor man's manual maybe monad
  (or (let ([temp (parse-base (>L1:< e))])
        (if temp
            (compiled:L2 (<L2:> temp) '())
            #f))
      (parse-comp (>L1:< e))))



