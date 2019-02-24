#lang racket/base
(require racket/match
         (only-in "../tests-driver.rkt" emit current-system-type))

(define bool-f   #x2F)
(define bool-t   #x6F)
(define bool-bit 6)
(define nil      #x3F)
(define word-size 8)

(define fixnum-mask #b11)
(define fixnum-tag #b00)
(define fixnum-shift 2)

(define char-mask #xFF)
(define char-tag #x0F)
(define char-shift 8)

(define (make-label s)
  (if (equal? 'macosx (current-system-type))
      (string-append "_" s)
      s))

(define (compile-program expr)
  (emit "# ~s" expr)
  (emit "    .text")
  (emit (format "    .globl ~a" (make-label "scheme_entry")))
  (emit$ `(label ,(make-label "L_scheme_entry")))
  (emit-expr (- word-size) expr)
  (emit$ 'ret)
  (emit$ `(label ,(make-label "scheme_entry")))
  (emit$ `(movq (reg rsp) (reg rbx)))
  (emit$ `(movq (reg rdi) (reg rsp)))
  (emit$ `(call (label ,(make-label "L_scheme_entry"))))
  (emit$ `(movq (reg rbx) (reg rsp)))
  (emit$ 'ret))

(define (emit-expr si expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr) (emit-if si expr)]
    [(primcall? expr) (emit-primcall si expr)]
    [(and? expr) (emit-and si expr)]
    [(or? expr) (emit-or si expr)]
    [else (error 'compile-program
                 "the expression `~s`, is not implemented" expr)]))

(define (emit-immediate x)
  (emit$ `(movq (int ,(immediate-rep x)) (reg rax))))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (if? expr)
  (match expr
    [`(if ,test ,consequent ,alternative) #t]
    [else #f]))

(define (and? expr)
  (equal? 'and (car expr)))

(define (or? expr)
  (equal? 'or (car expr)))

(define unique-label
  (let ([count 0])
    (lambda ()
      (begin0
          (format "L_~s" count)
        (set! count (add1 count))))))

(define (emit-if si expr)
  (match expr
    [`(if ,test ,con ,alt)
     (define alt-label (unique-label))
     (define end-label (unique-label))
     (emit-expr si test)
     (emit$ `(cmp (int ,bool-f) (reg rax)))
     (emit$ `(je (label ,alt-label)))
     (emit-expr si con)
     (emit$ `(jmp (label ,end-label)))
     (emit$ `(label ,alt-label))
     (emit-expr si alt)
     (emit$ `(label ,end-label))]))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (arithmetic-shift x fixnum-shift)]
    [(boolean? x) (if x bool-t bool-f)]
    [(null? x) nil]
    [(char? x) (bitwise-ior char-tag
                            (arithmetic-shift (char->integer x) char-shift))]))

(module+ test
  (require rackunit)
  (require (only-in "../tests-driver.rkt" compiler test-all)
           "../tests/tests-1.5-req.rkt"
           "../tests/tests-1.4-req.rkt"
           "../tests/tests-1.3-req.rkt"
           "../tests/tests-1.2-req.rkt"
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'compile-program "invalid primitive")))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall si expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define (check-primcall-args prim args)
  (let ([arg-count (getprop prim '*arg-count*)])
    (when (not (= arg-count (length args)))
      (error 'compile-program
             "~s: arity mismatch; expected ~s, given ~s `~s`"
             prim arg-count (length args) args))))


;; Aux

(define symbol-table
  (make-hasheq))

(define (getprop s k)
  (hash-ref! (hash-ref! symbol-table s (make-hasheq)) k #f))

(define (putprop s k v)
  (hash-update! symbol-table
                s
                (lambda (h) (hash-set! h k v) h)
                (make-hasheq (list (cons k v)))))

;; Primitives

(define-primitive ($fxadd1 si arg)
  (emit-expr si arg)
  (emit$ `(addq (int ,(immediate-rep 1)) (reg rax))))

(define-primitive ($fixnum->char si arg)
  (emit-expr si arg)
  (emit$ `(shlq (int ,(- char-shift fixnum-shift)) (reg rax)))
  (emit$ `(orq (int ,char-tag) (reg rax))))

(define-primitive ($char->fixnum si arg)
  (emit-expr si arg)
  (emit$ `(shrq (int ,(- char-shift fixnum-shift)) (reg rax))))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit$ `(and (int ,fixnum-mask) (reg al)))
  (emit$ `(cmp (int ,fixnum-tag) (reg al)))
  (emit=))

(define (emit=)
  (emit-cmp 'sete))

(define (emit<)
  (emit-cmp 'setl))

(define (emit<=)
  (emit-cmp 'setle))

(define (emit>)
  (emit-cmp 'setg))

(define (emit>=)
  (emit-cmp 'setge))

(define (emit-cmp cmp)
  (emit$ `(,cmp (reg al)))
  (emit$ `(movzbq (reg al) (reg rax)))
  (emit$ `(salq (int ,bool-bit) (reg rax)))
  (emit$ `(orq (int ,bool-f) (reg rax))))


(define ($ instr)
  (match instr
    ['ret "    ret"]
    [`(label ,l) (string-append l ":")]
    [`(,rator ,rand)
     (format "    ~a ~a"
             (symbol->string rator)
             (operand->string rand))]
    [`(,rator ,rand1 ,rand2)
     (format "    ~a ~a, ~a"
             (symbol->string rator)
             (operand->string rand1)
             (operand->string rand2))]))

(define (operand->string x)
  (match x
    [`(int ,(? number? n)) (string-append "$" (number->string n))]
    [`(reg ,r) (string-append "%" (symbol->string r))]
    [`(deref ,reg ,offset) (format "~s(%~a)" offset (symbol->string reg))]
    [`(label ,l) l]))

(define (emit$ . args)
  (emit (apply $ args)))

(define-primitive ($fxzero? si arg)
  (emit-expr si arg)
  ;; XXX check-fixnum
  (emit$ `(cmpq (int ,(immediate-rep 0)) (reg rax)))
  (emit=))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit$ `(cmpq (int ,nil) (reg rax)))
  (emit=))

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit$ `(and (int #x3F) (reg al)))
  (emit$ `(cmp (int #x2F) (reg al)))
  (emit=))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit$ `(and (int ,char-mask) (reg al)))
  (emit$ `(cmp (int ,char-tag) (reg al)))
  (emit=))

(define-primitive (not si arg)
  (emit-expr si arg)
  (emit$ `(cmp (int ,bool-f) (reg al)))
  (emit=))

(define-primitive ($fxlognot si arg)
  (emit-expr si arg)
  (emit$ `(shrq (int ,fixnum-shift) (reg rax)))
  (emit$ `(notq (reg rax)))
  (emit$ `(shlq (int ,fixnum-shift) (reg rax))))
;; XXX add more tests, to check 00b

(define-primitive ($fxsub1 si arg)
  (emit-expr si arg)
  (emit$ `(subq (int ,(immediate-rep 1)) (reg rax))))

(define (emit-and si expr)
  (match expr
    [`(and) (emit-expr si #t)]
    [`(and ,e) (emit-expr si e)]
    [`(and ,e0 ,e1 ...)
     (emit-if si `(if ,e0 (and ,@e1) #f))]))

(define (emit-or si expr)
  (match expr
    [`(or) (emit-expr si #f)]
    [`(or ,e) (emit-expr si e)]
    [`(or ,e0 ,e1 ...)
     (emit-if si `(if ,e0 ,e0 (or ,@e1)))]))

(define (emit-binary-expr si arg1 arg2)
  (emit-expr si arg1)
  (emit$ `(movq (reg rax) (deref rsp ,si)))
  (emit-expr (- si word-size) arg2))

(define-primitive (fx+ si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(addq (deref rsp ,si) (reg rax))))

(define-primitive (fx- si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(movq (reg rax) (reg rdi)))
  (emit$ `(movq (deref rsp ,si) (reg rax)))
  (emit$ `(subq (reg rdi) (reg rax))))

;; 4xy = (4x * 4y) / 4
(define-primitive (fx* si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(movq (deref rsp ,si) (reg rdi)))
  (emit$ `(mulq (reg rdi)))
  (emit$ `(shrq (int 2) (reg rax))))

(define-primitive (fxlogor si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(orq (deref rsp ,si) (reg rax))))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
  (emit$ `(notq (reg rax))))

(define-primitive (fxlogand si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(andq (deref rsp ,si) (reg rax))))

(define-primitive (fx= si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(cmpq (deref rsp ,si) (reg rax)))
  (emit=))

(define-primitive (fx< si arg1 arg2)
  (emit-fx-cmp si arg1 arg2)
  (emit<))

(define-primitive (fx<= si arg1 arg2)
  (emit-fx-cmp si arg1 arg2)
  (emit<=))

(define-primitive (fx> si arg1 arg2)
  (emit-fx-cmp si arg1 arg2)
  (emit>))

(define-primitive (fx>= si arg1 arg2)
  (emit-fx-cmp si arg1 arg2)
  (emit>=))

(define (emit-fx-cmp si arg1 arg2)
  (emit-binary-expr si arg1 arg2)
  (emit$ `(movq (reg rax) (reg rdi)))
  (emit$ `(movq (deref rsp ,si) (reg rax)))
  (emit$ `(cmpq (reg rdi) (reg rax))))

;; MAIN

(module+ main
  (require racket/cmdline)

  (command-line
   #:once-each
   [("-c") e "compile" (compile-program (read (open-input-string e)))]))
