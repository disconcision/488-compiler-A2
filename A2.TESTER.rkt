#lang racket

(provide A2-test)

(define (make-file string filename)
  (call-with-output-file filename
    #:exists 'replace
    (λ (out)
      (displayln string out))))

(define (make-exec sourcename execname)
  (system (string-append "gcc -o " execname " " sourcename)))

(define (run-and-return cmd)
  (string->number
   (first
    (string-split
     (with-output-to-string
         (λ () (system cmd)))
     "\n"))))

(define (rm filename)
  (system (string-append "rm " filename)))

(define (compile-test code compiler)
  (let* ([exec-name "autotest-temp"]
         [source-name (string-append exec-name ".s")])
    (make-file (compiler code) source-name)
    (make-exec source-name exec-name)
    (define result
      (run-and-return (string-append "./" exec-name " ; echo $?")))
    (map rm `(,exec-name ,source-name))
    result))

(define-syntax-rule (A2-test compiler [in out] ...)
  (module+ test
    (require rackunit)
    (check-equal? (compile-test in compiler) out) ...))