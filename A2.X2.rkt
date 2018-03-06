#lang racket #| Compile A2's Language L2 to Language X2 |#

(provide L2→X2
         Mac? heap-size postamble)

(require "A2.L2.rkt") ; corrected type A1->A2

(module+ test (require rackunit))

; Whether to emit code for the Mac, that Apple's gcc wrapper for clang handles. |#
(define Mac? (make-parameter #false))

; Size of the heap.
(define heap-size (make-parameter (/ (* 4 (expt 2 30)) 8))) ; 4G of 8-byte data.

; Code to append to main.
;
; If you put the resulting assembly code into a file file.s then the following postamble
;  prints the execution time and lowest byte of the result to the console if run with:
;   gcc -o file file.s ; time ./file ; echo $?
;
(define postamble (make-parameter "movq %rcx, %rax")) ; Return result.

#| X2
   ==
 Language X2 is a subset of 64-bit x86 assembly language, which we'll emit in the AT&T syntax.
 Details are in the rest of this file. |#

#| Machine Model
   =============
 Our current model of execution has a few global variables, which are frequently accessed and updated,
  and a stack with frequent stack-like operations. Many machine architectures provide the following
  model, and optimize for this pattern of use.

 The Model
 ---------
 Program: a sequence of statements.
 Execution: sequential, except after a statement that explicitly sets the current statement address.
 State: a fixed set of global variables, a stack, a large array, and a current statement address. |#

#| Global Variables
   ================
 The global variables in a CPU are called registers.

 From our point of view the general purpose X2 registers are all interchangeable. We'll use:

   register : use
   --------------
   a        : temporary variable
   c        : expression result
   10       : next location to allocate in heap
   11       : current environment

 In 64-bit x86 with the AT&T syntax we refer to them as %rax, %rcx, %r10, and %r11.
 The names are not meaningful so let's hide them. |#

(define (register name) (~a '% name))
(define temp   (register 'rax))
(define result (register 'rcx))
(define next   (register 'r10))
(define env    (register 'r11))

#| Setting and Accessing Registers
   =============================== |#

(module+ test
  ; result = temp
  (check-equal? (move temp result) "movq %rax, %rcx")
  ; result += temp
  (check-equal? (add temp result) "addq %rax, %rcx"))

(define (move from to) (~a 'movq " " from   ", " to))
(define (add from to) (~a 'addq " " from   ", " to))
(define (mul from to) (~a 'imulq " " from   ", " to)) ; TODO: not sure if right op

#| Integer Constants
   =================
 Integer constants are prefixed with "$".
 They can appear as ‘from’ in movq and addq. |#

(module+ test
  ; temp = 488
  (check-equal? (move (constant 488) temp) "movq $488, %rax"))

(define (constant i) (~a '$ i))

#| Addresses of Statements
   =======================
 We can refer to the address of a statement by putting a label before the statement, and
  then use the label. In particular, we can change the execution order by jumping to a
  statement's address.

 We wont jump to [as opposed to call] stored locations, only explicit labels.

 To increase portability and flexibility, without much effort, we'll “mangle” labels by
  potentially adding an underscore [for the Mac's gcc wrapper around clang], and make them
  relative to the current instruction pointer [reasons and details aren't important for us]
  This does make them count as offset dereferences, and the limitation of the previous
  section applies. |#

; main()
;   temp = make_add
;   goto main
#;(labelled 'main
            (movq (label-reference 'make_add) temp)
            (jmp 'main))

(define (mangle name) (~a (if (Mac?) '_ "") name))
(define (labelled name . lines) (list (~a (mangle name) ':)
                                      lines))
(define (label-reference name) (~a (mangle name) "@GOTPCREL(%rip)"))

(define (jump-label name) (~a 'jmp " " (mangle name)))

#| The Stack
   =========
 We can push a value [constant, or contents of a register], and pop a value into a register.

 Also, we can “call” a statement address [see “Addresses of Statements” below] that's stored
  in a register, which:
    1. Pushes the address of the statement that follows the call.
    2. Jumps to the address that's in the register.

 Coversely, we can “return”, which pops an address that's stored on the stack and jumps to it. |#

(define (push from) (~a 'pushq " " from))
(define (pop  to)   (~a 'popq  " "   to))
(define (stack-call from) (~a 'call  " *" from "")) ; ALTERED THIS
(define (return)       (~a 'retq))

#| Dereferencing and Pointer Arithmetic
   ====================================
 We'll store 64-bit data in our heap: the nth piece of data at an address is 8×n bytes after it.

 We can dereference a register containing an address, with an optional offset.
 Most ‘from’s or ‘to’s in the statements we're using can be a dereference, but not at the same time
  in a single statement. |#

(module+ test
  ; result = temp[0]
  (check-equal? (move (★ temp) result) "movq 0(%rax), %rcx")
  ; result[488] = temp
  (check-equal? (move temp (★ result 488)) "movq %rax, 3904(%rcx)"))

(define (⊕ offset) (* 8 offset))
(define (★ register [offset 0]) (~a (⊕ offset) "(" register ")"))

#| Conditional Execution
   =====================
 We can jump to an address conditionally, in particular on condition that two values are equal.
 Comparison sets a CPU flag, that various jump instructions react to.

 For comparison to a constant, the constant must be the first argument.

 We wont jump to calculated locations, only explicit labels. |#

; if (temp == result) goto main
#;(list (cmpq temp result)
        (je 'main))

(define (cmpq from-1 from-2) (~a 'cmpq " " from-1 ", " from-2))
(define (je-label name) (~a 'je " " (mangle name)))

#| L2 Statement to X2
   ==================
 Implement the seven functions needed to translate an L2 statement to an X2 statement or
  [possibly nested] list of statements.

 The nesting of the list structure is irrelevant: L2→X2 will flatten the results. |#

(define (l2→X2 l2) (match l2
                     [`(L2: set_result ,<i>) (set_result <i>)]
                     [`(L2: push_result) (push_result)]
                     [`(L2: closure ,<name>) (closure <name>)]
                     [`(L2: call) (call)]
                     [`(L2: variable ,<n>) (variable <n>)]
                     [`(L2: set ,<n>) (set <n>)]
                     [`(L2: label ,<name>) (label <name>)]
                     [`(L2: jump ,<name>) (jump <name>)]
                     [`(L2: jump_false ,<name>) (jump_false <name>)]))

; commands
#; (movq from to)
#; (addq from to)
#; (constant i)
#; (jmp-label name)
#; (labelled name . lines)
#; (pushq from)
#; (popq  to)
#; (callq from)
#; (retq)
#; (⊕ offset)
#; (★ register [offset 0])
#; (cmpq from-1 from-2)
#; (je-label name)

; Set result to integer i.
(define (set_result i)
  (list
   (move (constant i) result)))

; Push result onto the stack.
(define (push_result)
  (list
   (push result)))

; Put a closure on the heap.
(define (closure name)
  (list
   ; closure is pair (on heap) of code address and environment address
   (move (label-reference name) temp) ; can't move between two addresses...
   (move temp (★ next 0)) ; closure code address <- address of label 'name'
   (move env (★ next 1)) ; closure env address <- current env
   (add (constant (⊕ 2)) next) ; increment heap pointer
   (move next temp) ; can't move between two addresses...
   (add (constant (⊕ -2)) temp)
   (move temp result) ; return address of closure in heap
   ))

; Call the closure that's on the stack, with the argument that's in result.
(define (call)
  (list
   (pop temp) ; this better be the address of a closure
   ; now temp[0] = address of closure code
   ; and temp[1] = address of closure environment
   
   (push env) ; shelve the current environment
   
   ; now we make new environment on the heap (next)
   ; env is pair: [0] address of parent env, [1] stored value
   (move result (★ next 1)) ; new-env stored value <- call argument
   (move (★ temp 1) result) ; clobber result since we can't move between two addresses
   (move result (★ next 0)) ; new-env parent-env <- closure environment

   (move next env) ; env <- new-env
   (add (constant (⊕ 2)) next) ; increment next

   ; jump to address of closure code
   (stack-call (★ temp 0))

   ; restore shelved environment
   (pop env)
   ))

; Puts the value of the variable n levels up from env, into result.
;   To “loop” n times: emits n statements.
(define (variable n)
  (list
   (move env temp)
   (make-list n (move (★ temp 0) temp)) ; temp-env <- parent env
   (move (★ temp 1) result) ; result <- temp-env.value
   ))

; Sets the variable n levels up from env, to the value of result.
;   To “loop” n times: emits n statements.
(define (set n)
  (list
   (move env temp)
   (make-list n (move (★ temp 0) temp)) ; temp-env <- parent env 
   (move result (★ temp 1)))) ; temp-env.value <- result

; Names the current statement address.
(define (label name)
  (list
   (labelled name)))

; Jumps to a named statement address.
(define (jump name)
  (list
   (jump-label name)))

; Jumps to a named statement address, if result is false.
(define (jump_false name)
  (list
   (cmpq (constant 0) result) ; if false == result
   (je-label name))) ; goto label name

#| L2 to X2
   ======== |#

(define (L2→X2 compiled)
  (match-define (compiled:L2 code λs) compiled)
  (map (curryr ~a "\n")
       (flatten (list (~a '.globl "  " (mangle 'main))
                      RTL
                      (map λ→X2 λs)
                      (labelled 'main
                                (move (label-reference 'heap) next)
                                (map l2→X2 code)
                                (postamble)
                                (return))
                      (~a '.comm  "  " (mangle 'heap) "," (heap-size) "," (if (Mac?) 4 32))))))

; For a compiled λ from L2: the code for its body, including a return, labelled by the name of the λ.
(define (λ→X2 a-λ) (labelled (first a-λ)
                             (map l2→X2 (second a-λ))
                             (return)))

#| Runtime Library
   =============== |#

; Addition and Multiplication
; ---------------------------

; Roughly, we've been treating addition as if it's:
#;(define + (λ_make_add (variable_1)
                        (λ_add (variable_0)
                               (primitive-addition variable_0 variable_1))))

; L1→L2 translates ‘+’ to a statement that creates a make_add closure.
(module+ test
  (check-equal? (L1→L2 '(L1: var +)) (compiled:L2
                                      '((L2: closure make_add))
                                      '())))

; Put X2 versions of make_add and add in RTL below.
; Similarly, find the 64-bit x86 instruction for multiplication, and add multiplication.

; Escape Continuations
; --------------------

; The continuation of an expression is:
;
;   The state of the heap, and the stack and env before the expression begins evaluating,
;    and the address of the statement after the expression's statements, with that statement
;    waiting to work with the result.

; Write out the compilation of (call/ec f) to convince yourself that the continuation of
;  that expression is on the stack. And convince yourself that setting the result to v
;  and executing a return with the stack in that state continues as if the value of
;  (call/ec f) is v [and any side-effects until then are in the heap and persist].

; (call/ec f) calls f with an escape continuation k, where (k v) escapes the evaluation
;  of (call/ec f) to produce v. Roughly, we treat call/ec as:
#;(λ_call_ec (f) (f ((λ_make_ec (saved-stack-pointer)
                                (λ_ec (result) (set! stack-pointer saved-stack-pointer)
                                      result))
                     stack-pointer)))
; The CPU's stack pointer is a register:
(define stack-pointer (register 'rsp))

; A2's L1→L2 translates ‘call/ec’ to a statement that creates a call_ec closure.
(module+ test
  (check-equal? (L1→L2 '(L1: var call/ec)) (compiled:L2
                                            '((L2: closure call_ec))
                                            '())))

; Put X2 versions of call_ec, make_ec, and ec in RTL below.

; Booleans
; --------
; As mentioned earlier, we're representing false by the value 0.
; We'll represent true by non-zero values.

; Roughly, we've been treating “less than” as if it's:
#;(define < (λ_make_less_than (variable_1)
                              (λ_less_than (variable_0)
                                           (primitive-less-than variable_1 variable_0))))

; L1→L2 translates ‘<’ to a statement that creates a make_less_than closure.
(module+ test
  (check-equal? (L1→L2 '(L1: var <)) (compiled:L2
                                      '((L2: closure make_less_than))
                                      '())))

; The CPU flags set by a comparison can be stored as a byte, which we then “widen” to a 64 bit value.
; if result < temp
;    result = 1
; else
;    result = 0
#;(list (cmpq temp result)
        (setl result-byte)
        (movzbq result-byte result))

(define (setl to) (~a 'setb  " " to))
(define result-byte (register 'cl))
(define (movzbq from-1 from-2) (~a 'movzbq " " from-1 ", " from-2))

; Put X2 versions of make_less_than and less_than in RTL below.

#;(define + (λ_make_add (variable_1)
                        (λ_add (variable_0)
                               (primitive-addition variable_0 variable_1))))

(define RTL
  (list
  
   (labelled 'ec #; (λ_ec (msg)
                          (set! stack-pointer saved-stack-pointer) msg)
             (variable 1) ; get saved-stack-pointer
             (move result stack-pointer) ; set real stack-pointer to saved-stack-pointer
             (variable 0) ; return input
             (return))

   (labelled 'make_ec #; (λ_make_ec (saved-stack-pointer)
                                    <λ_ec>)
             (closure 'ec)
             (return))

   (labelled 'call_ec #; (λ_call_ec (f)
                                    (f (<λ_make_ec> stack-pointer)))
             ; call f on the result of calling make_ec on the stack-pointer
             (variable 0) (push result) ; push f
             (closure 'make_ec) (push result) ; push make_ec
             (move stack-pointer result) ; get stack-pointer
             (call) ; call λ_make_ec on stack-pointer
             (call) ; call f on that result
             (return))

   
   (labelled 'add
             (variable 1)
             (push result)
             ; pushing since variable clobbers temp
             (variable 0)
             (pop temp)
             (add temp result)
             (return))
   
   (labelled 'make_add
             (closure 'add)
             (return))

   
   (labelled 'multiply
             (variable 1)
             (push result)
             (variable 0)
             (pop temp)
             (mul temp result)
             (return))
   
   (labelled 'make_multiply
             (closure 'multiply)
             (return))

   
   (labelled 'make_less_than
             (closure 'less_than)
             (return))
   
   (labelled 'less_than
             (variable 1)
             (push result)
             (variable 0)
             (pop temp)
             (cmpq result temp) ; had to switch order here
             (setl result-byte)
             (movzbq result-byte result)
             (return))))
