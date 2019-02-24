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
  (emit-expr (- word-size) '() expr)
  (emit$ 'ret)
  (emit$ `(label ,(make-label "scheme_entry")))
  (emit$ `(movq (reg rsp) (reg rbx)))
  (emit$ `(movq (reg rdi) (reg rsp)))
  (emit$ `(call (label ,(make-label "L_scheme_entry"))))
  (emit$ `(movq (reg rbx) (reg rsp)))
  (emit$ 'ret))

(module+ test
  (require rackunit)
  (require (only-in "../tests-driver.rkt" compiler test-all)
           "../tests/tests-1.6-opt.rkt"
           "../tests/tests-1.6-req.rkt"
           "../tests/tests-1.5-req.rkt"
           "../tests/tests-1.4-req.rkt"
           "../tests/tests-1.3-req.rkt"
           "../tests/tests-1.2-req.rkt"
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))

(define (emit-function-header l)
  (emit$ `(label ,l))
  (emit$ `(subq (int ,word-size) (reg rsp))))

(define (emit-function-footer)
  (emit$ `(addq (int ,word-size) (reg rsp)))
  (emit$ 'ret))

(define (emit-expr si env expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(variable? expr) (emit-variable-ref env expr)]
    [(if? expr) (emit-if si env expr)]
    [(let? expr) (emit-let si env expr)]
    [(let*? expr) (emit-let* si env expr)]
    [(primcall? expr) (emit-primcall si env expr)]
    [(and? expr) (emit-and si env expr)]
    [(or? expr) (emit-or si env expr)]
    [else (error 'compile-program
                 "the expression `~s`, is not implemented" expr)]))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (variable? expr)
  (symbol? expr))

(define (if? expr)
  (match expr
    [`(if ,test ,consequent ,alternative) #t]
    [else #f]))

(define (let? expr)
  (match expr
    [`(let ,bindings ,body) #t]
    [else #f]))

(define (let*? expr)
  (match expr
    [`(let* ,bindings ,body) #t]
    [else #f]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (and? expr)
  (equal? 'and (car expr)))

(define (or? expr)
  (equal? 'or (car expr)))

(define (emit-immediate x)
  (emit$ `(movq (int ,(immediate-rep x)) (reg rax))))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (arithmetic-shift x fixnum-shift)]
    [(boolean? x) (if x bool-t bool-f)]
    [(null? x) nil]
    [(char? x) (bitwise-ior char-tag
                            (arithmetic-shift (char->integer x) char-shift))]))

(define (emit-variable-ref env var)
  (cond
    [(lookup var env) => emit-stack-load]
    [else (error 'compile-program "variable `~s` is unbound" var)]))

;; Symbol Env -> Number | #f
(define (lookup var env)
  (define b (assoc var env))
  (if b
      (cdr b)
      #f))

;; Number -> EMIT
(define (emit-stack-load si)
  (emit$ `(movq (deref rsp ,si) (reg rax))))

(define unique-label
  (let ([count 0])
    (lambda ()
      (begin0
        (format "L_~s" count)
        (set! count (add1 count))))))

(define (emit-if si env expr)
  (match expr
    [`(if ,test ,con ,alt)
     (define alt-label (unique-label))
     (define end-label (unique-label))
     (emit-expr si env test)
     (emit$ `(cmp (int ,bool-f) (reg rax)))
     (emit$ `(je (label ,alt-label)))
     (emit-expr si env con)
     (emit$ `(jmp (label ,end-label)))
     (emit$ `(label ,alt-label))
     (emit-expr si env alt)
     (emit$ `(label ,end-label))]))

(define (emit-let si env expr)
  (match expr
    [`(let ,bindings ,body)
     (define (process-let bindings si new-env)
       (cond
         [(null? bindings)
          (emit-expr si new-env body)]
         [else
          (let ([b (car bindings)])
            (emit-expr si env (cadr b))
            (emit-stack-save si)
            (process-let
             (cdr bindings)
             (next-stack-index si)
             (extend-env (car b) si new-env)))]))
     (process-let bindings si env)]))

(define (emit-let* si env expr)
  (match expr
    [`(let* ,bindings ,body)
     (define (process-let* bindings si env*)
       (cond
         [(null? bindings)
          (emit-expr si (extend-env* env* env) body)]
         [else
          (let ([b (car bindings)])
            (emit-expr si (extend-env* env* env) (cadr b))
            (let ([b-si (lookup (car b) env*)])
              (if b-si
                  ;; ... overwrite
                  (begin
                    (emit-stack-save b-si)
                    (process-let*
                     (cdr bindings)
                     si
                     env*))
                  ;; ... allocate new stack space
                  (begin
                    (emit-stack-save si)
                    (process-let*
                     (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car b) si env*))))))]))
     (process-let* bindings si '())]))

(define (next-stack-index i)
  (- i word-size))

(define (extend-env var si env)
  (cons (cons var si) env))
(define (extend-env* e1 e2)
  (append e1 e2))

(define (emit-stack-save si)
  (emit$ `(movq (reg rax) (deref rsp ,si))))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'compile-program "invalid primitive")))

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

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

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si env arg* ...) b b* ...)))]))

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit$ `(addq (int ,(immediate-rep 1)) (reg rax))))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit$ `(shlq (int ,(- char-shift fixnum-shift)) (reg rax)))
  (emit$ `(orq (int ,char-tag) (reg rax))))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit$ `(shrq (int ,(- char-shift fixnum-shift)) (reg rax))))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
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

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  ;; XXX check-fixnum
  (emit$ `(cmpq (int ,(immediate-rep 0)) (reg rax)))
  (emit=))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit$ `(cmpq (int ,nil) (reg rax)))
  (emit=))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int #x3F) (reg al)))
  (emit$ `(cmp (int #x2F) (reg al)))
  (emit=))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,char-mask) (reg al)))
  (emit$ `(cmp (int ,char-tag) (reg al)))
  (emit=))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit$ `(cmp (int ,bool-f) (reg al)))
  (emit=))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit$ `(shrq (int ,fixnum-shift) (reg rax)))
  (emit$ `(notq (reg rax)))
  (emit$ `(shlq (int ,fixnum-shift) (reg rax))))
;; XXX add more tests, to check 00b

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit$ `(subq (int ,(immediate-rep 1)) (reg rax))))

(define (emit-and si env expr)
  (match expr
    [`(and) (emit-expr si env #t)]
    [`(and ,e) (emit-expr si env e)]
    [`(and ,e0 ,e1 ...)
     (emit-if si env `(if ,e0 (and ,@e1) #f))]))

(define (emit-or si env expr)
  (match expr
    [`(or) (emit-expr si env #f)]
    [`(or ,e) (emit-expr si env e)]
    [`(or ,e0 ,e1 ...)
     (emit-if si env `(if ,e0 ,e0 (or ,@e1)))]))

(define (emit-binary-expr si env arg1 arg2)
  (emit-expr si env arg1)
  (emit$ `(movq (reg rax) (deref rsp ,si)))
  (emit-expr (- si word-size) env arg2))

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(addq (deref rsp ,si) (reg rax))))

(define-primitive (fx- si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (reg rax) (reg rdi)))
  (emit$ `(movq (deref rsp ,si) (reg rax)))
  (emit$ `(subq (reg rdi) (reg rax))))

;; 4xy = (4x * 4y) / 4
(define-primitive (fx* si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (deref rsp ,si) (reg rdi)))
  (emit$ `(mulq (reg rdi)))
  (emit$ `(shrq (int 2) (reg rax))))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(orq (deref rsp ,si) (reg rax))))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit$ `(notq (reg rax))))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(andq (deref rsp ,si) (reg rax))))

(define-primitive (fx= si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(cmpq (deref rsp ,si) (reg rax)))
  (emit=))

(define-primitive (fx< si env arg1 arg2)
  (emit-fx-cmp si env arg1 arg2)
  (emit<))

(define-primitive (fx<= si env arg1 arg2)
  (emit-fx-cmp si env arg1 arg2)
  (emit<=))

(define-primitive (fx> si env arg1 arg2)
  (emit-fx-cmp si env arg1 arg2)
  (emit>))

(define-primitive (fx>= si env arg1 arg2)
  (emit-fx-cmp si env arg1 arg2)
  (emit>=))

(define (emit-fx-cmp si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (reg rax) (reg rdi)))
  (emit$ `(movq (deref rsp ,si) (reg rax)))
  (emit$ `(cmpq (reg rdi) (reg rax))))

;; MAIN

(module+ main
  (require "../tests-driver.rkt"
           racket/cmdline
           racket/system)

  (define (repl)
    (printf "Welcome to Scheme\n")
    (let loop ()
      (printf "> ")
      (define e (read))
      (if (eof-object? e)
          (void)
          (begin
            (call-with-output-file "stst.s" #:exists 'replace
              (lambda (out)
                (parameterize ([compile-port out])
                  (compile-program e))))
            (unless (system "gcc -o stst runtime.c stst.s")
              (error 'interp "gcc build failed"))
            (unless (system "./stst")
              (error 'interp "./stst failed"))
            (loop)))))

  (command-line
   #:once-each
   [("-c") e "compile" (compile-program (read (open-input-string e)))]
   [("-i") "interactive" (repl)]))
