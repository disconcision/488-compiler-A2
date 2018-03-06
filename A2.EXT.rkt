#lang racket

(require "A2.L1.rkt"
         "A2.L2.rkt"
         "A2.X2.rkt"
         "A2.M0.rkt"
         "A2.TESTER.rkt")

(define M0→X2 (compose (λ (x) (apply string-append x))
                       L2→X2 L1→L2 L0→L1 M0→L0))

(A2-test M0→X2
  [255
   255]
  ['(λ (x) x)
   112] ; just the lower byte of the address
  ['((λ (x) 4) 66)
   4]
  ['((λ (x) x) 66)
   66]
  ['(((λ (x) (λ (y) y)) 36) 37)
   37]
  ['(((λ (x) (λ (y) x)) 46) 47)
   46]
  ['((λ (_)
       ((λ (x)
          ((λ (w) x) 4)) 5)) 6)
   5]
  ['((* 6) 7)
   42]
  ['((+ 40) 50)
   90]
  ['((λ (_)
       ((λ (x)
          ((λ (w) x) (set! x 11))) 12)) 13)
   11]
  ['((λ (z)
       ((λ (a)
          ((* 7) ((+ 8) z))) ((λ (x) (set! z 9)) z))) 10)
   119]
  ['(if ((< 1) 0) 2 3)
   3]
  ['(if ((< 1) 2) 2 3)
   2]
  ['((λ (n)
       (if ((< n) 5) 3 4))
     5)
   4]
  ['((λ (f)
       ((λ (_) (f 1))
        (set! f (λ (n)
                  (if ((< n) 2) 8 9)))))
     0)
   8]
  ['((λ (f)
       ((λ (_) (f 3))
        (set! f (λ (n)
                  (if ((< n) 3) n (f 1))))))
     0)
   1]
  ['((λ (f) ((λ (_) (f 7))
             (set! f (λ (n)
                       (if ((< n) 2)
                           1
                           ((+ (f ((+ -1) n))) (f ((+ -2) n))))))))
     0)
   21]
  ['(local
      [(define (fib n)
         (if (< n 3)
             1
             (+ (fib (+ n -1)) (fib (+ n -2)))))]
      (fib 11))
   89]
  ['(call/ec (λ (k) (k 240)))
   240]
  ['(call/ec (λ (k) (+ 2 (k 240))))
   240]
  ['(+ 1 (call/ec (λ (k) (k 240))))
   241]
  ['(+ 1 (call/ec (λ (k) (+ 20 (k 30) 666))))
   31]
  ['(let ([a 1] [b 2] [c 3])
      (+ a b))
   3]
  ['(let ([a 1] [b 2] [c 3]) ; 9
      (local [(define (f x) (* x 2))
              (define (g x) (+ x 2))]
        (+ (f c) (g a))))
   9]
  ['(let ([x 0])
      (set! x 2)
      (+ x (let ([a 3])
             (set! x 6)
             (+ a x))))
   11]
  ['(let ([i 5] [c 1])
      (while (< 0 i)
             (set! i (+ -1 i))
             (set! c (* 2 c)))
      c)
   32]
  ['(let ([i 5] [c 1])
      (breakable
       (while (< 0 i)
              (set! i (+ -1 i))
              ; break out early
              (if (= 2 i)
                  (break 666)
                  0)
              (set! c (* 2 c))))
      c)
   4]
  ['(let ([i 5] [c 1])
      (while (< 0 i)
             (continuable
              (set! i (+ -1 i))
              (if (or (= 2 i) (= 3 i))
                  ; skip two iteration
                  (continue 666)
                  0)
              (set! c (* 2 c))))
      c)
   8]
  )


