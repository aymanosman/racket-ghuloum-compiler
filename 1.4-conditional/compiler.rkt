#lang racket/base
(require racket/match
         (only-in "../tests-driver.rkt" emit current-system-type))

(define bool-f   #x2F)
(define bool-t   #x6F)
(define bool-bit 6)
(define nil      #x3F)

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
  (emit "    .text")
  (emit (format "    .globl ~a" (make-label "scheme_entry")))
  (emit (make-label "scheme_entry:"))
  (emit-expr expr)
  (emit "    ret"))

(define (emit-expr expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr) (emit-if expr)]
    [(and? expr) (emit-and expr)]
    [(or? expr) (emit-or expr)]
    [(primcall? expr) (emit-primcall expr)]
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

(define (emit-if expr)
  (match expr
    [`(if ,test ,con ,alt)
     (define alt-label (unique-label))
     (define end-label (unique-label))
     (emit-expr test)
     (emit$ `(cmp (int ,bool-f) (reg rax)))
     (emit$ `(je (label ,alt-label)))
     (emit-expr con)
     (emit$ `(jmp (label ,end-label)))
     (emit$ `(label ,alt-label))
     (emit-expr alt)
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
           "../tests/tests-1.4-req.rkt"
           "../tests/tests-1.3-req.rkt"
           "../tests/tests-1.2-req.rkt"
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'compile-program "invalid primitive")))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

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

(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit$ `(addq (int ,(immediate-rep 1)) (reg rax))))

(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit$ `(shlq (int ,(- char-shift fixnum-shift)) (reg rax)))
  (emit$ `(orq (int ,char-tag) (reg rax))))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit$ `(shrq (int ,(- char-shift fixnum-shift)) (reg rax))))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit$ `(and (int ,fixnum-mask) (reg al)))
  (emit$ `(cmp (int ,fixnum-tag) (reg al)))
  (emit-true?))

(define (emit-true?)
  (emit$ `(sete (reg al)))
  (emit$ `(movzbq (reg al) (reg rax)))
  (emit$ `(salq (int ,bool-bit) (reg rax)))
  (emit$ `(orq (int ,bool-f) (reg rax))))

(define (operand->string x)
  (match x
    [`(int ,(? number? n)) (string-append "$" (number->string n))]
    [`(reg ,r) (string-append "%" (symbol->string r))]
    [`(label ,l) l]))

(define ($ instr)
  (match instr
    [`(label ,l) (string-append l ":")]
    [`(,rator ,rand)
     (format "    ~a ~a" (symbol->string rator) (operand->string rand))]
    [`(,rator ,rand1 ,rand2)
     (format "    ~a ~a, ~a" (symbol->string rator)
             (operand->string rand1)
             (operand->string rand2))]))

(define (emit$ . args)
  (emit (apply $ args)))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  ;; XXX check-fixnum
  (emit$ `(cmpq (int ,(immediate-rep 0)) (reg rax)))
  (emit-true?))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit$ `(cmpq (int ,nil) (reg rax)))
  (emit-true?))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit$ `(and (int #x3F) (reg al)))
  (emit$ `(cmp (int #x2F) (reg al)))
  (emit-true?))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit$ `(and (int ,char-mask) (reg al)))
  (emit$ `(cmp (int ,char-tag) (reg al)))
  (emit-true?))

(define-primitive (not arg)
  (emit-expr arg)
  (emit$ `(cmp (int ,bool-f) (reg al)))
  (emit-true?))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit$ `(shrq (int ,fixnum-shift) (reg rax)))
  (emit$ `(notq (reg rax)))
  (emit$ `(shlq (int ,fixnum-shift) (reg rax))))
;; XXX add more tests, to check 00b

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit$ `(subq (int ,(immediate-rep 1)) (reg rax))))

(define (emit-and expr)
  (match expr
    [`(and) (emit-expr #t)]
    [`(and ,e) (emit-expr e)]
    [`(and ,e0 ,e1 ...)
     (emit-if `(if ,e0 (and ,@e1) #f))]))

(define (emit-or expr)
  (match expr
    [`(or) (emit-expr #f)]
    [`(or ,e) (emit-expr e)]
    [`(or ,e0 ,e1 ...)
     (emit-if `(if ,e0 ,e0 (or ,@e1)))]))
