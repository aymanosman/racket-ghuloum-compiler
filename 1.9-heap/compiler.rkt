#lang racket/base
(require racket/match
         (only-in "../tests-driver.rkt" emit current-system-type))

(define false-rep  #x2F)
(define true-rep   #x6F)
(define bool-bit 6)
(define null-rep   #x3F)
(define word-size 8)

(define fixnum-mask #b11)
(define fixnum-tag #b00)
(define fixnum-shift 2)

(define char-mask #xFF)
(define char-tag #x0F)
(define char-shift 8)

(define pair-mask #b111)
(define pair-tag  #b001)
(define pair-size 16)
(define car-offset 0)
(define cdr-offset 8)

(define vector-mask #b111)
(define vector-tag  #b101)

(define string-mask #b111)
(define string-tag  #b110)

(define (make-label s)
  (if (equal? 'macosx (current-system-type))
      (string-append "_" s)
      s))

(define (compile-program expr)
  (emit "# ~s" expr)
  (emit "    .text")
  (emit-program expr)
  (emit-entry)
  (emit$ 'ret))

(define (emit-entry)
  (emit "    .globl ~a" (make-label "scheme_entry"))
  (emit$ `(label ,(make-label "scheme_entry")))
  ;; rdi=ctx rsi=stack_base rdx=heap

  ;; Save Context
  ;; ctx->rbx = %rbx
  ;; ctx->rbp = %rbp
  ;; ctx->rsp = %rsp
  (emit$ `(movq (reg rdi) (reg rcx)))
  (emit$ `(movq (reg rbx) (deref rcx 0)))
  (emit$ `(movq (reg rbp) (deref rcx 8)))
  (emit$ `(movq (reg rsp) (deref rcx 16)))

  ;; Install stack and heap pointers
  (emit$ '(movq (reg rsi) (reg rsp)))
  (emit$ '(movq (reg rdx) (reg rbp)))

  (emit$ `(callq (label ,(make-label "L_scheme_entry"))))

  ;; Restore stack and frame pointers of caller
  (emit$ '(movq (deref rcx 16) (reg rsp)))
  (emit$ '(movq (deref rcx 8) (reg rbp))))

(module+ test
  (require rackunit)
  (require (only-in "../tests-driver.rkt" compiler test-all)
           "../tests/tests-1.9-req.rkt"
           "../tests/tests-1.8-tail.rkt"
           "../tests/tests-1.8-req.rkt"
           "../tests/tests-1.7-req.rkt"
           "../tests/tests-1.6-opt.rkt"
           "../tests/tests-1.6-req.rkt"
           "../tests/tests-1.5-req.rkt"
           "../tests/tests-1.4-req.rkt"
           "../tests/tests-1.3-req.rkt"
           "../tests/tests-1.2-req.rkt"
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))

(define (emit-program expr)
  (define (passes e)
    (make-begin-explicit e))
  (match expr
    [`(letrec ,bindings ,body)
     (emit-letrec (passes expr))]
    [else
     (emit-scheme-entry (passes expr) '())]))

;; TODO This does not recurse into all subexpressions
(define (make-begin-explicit expr)
  (match expr
    [`(let ([,id* ,b*] ...) ,body)
     `(let ,(map list id* (map make-begin-explicit b*))
        ,(make-begin-explicit body))]
    [`(let ([,id* ,b*] ...) ,body0 ,body1 ,body* ...)
     `(let ,(map list id* (map make-begin-explicit b*))
        (begin ,@(map make-begin-explicit (list* body0 body1 body*))))]
    [(? primcall?) ;; XXX `((? prim? ,prim) ,arg* ...)
     (match-define `(,prim ,arg* ...) expr)
     `(,prim ,@(map make-begin-explicit arg*))]
    ;; ... XXX rest of cases
    [else expr]))

(define (emit-scheme-entry expr env)
  (emit$ `(label ,(make-label "L_scheme_entry")))
  (emit-expr #:tail? #t (- word-size) env expr))

(define (emit-letrec expr)
  (match-define `(letrec ([,name* ,lambda*] ...) ,body) expr)
  (let* ([labels (map (lambda (_) (unique-label)) name*)]
         [env (make-initial-env name* labels)])
    (for-each (emit-lambda env) lambda* labels)
    (emit-scheme-entry body env)))

(define (emit-lambda env)
  (lambda (expr label)
    (emit "# TODO ~s" "<name of function>")
    (emit$ `(label ,label))
    (match-define `(lambda (,v* ...) ,body) expr)
    (let f ([v* v*] [si (- word-size)] [env env])
      (cond
        [(null? v*)
         (emit-expr #:tail? #t si env body)]
        [else
         (f (cdr v*)
            (- si word-size)
            (extend-env (car v*) si env))]))))

(define (emit-app #:tail? [tail? #f] si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-stack-save si)
      (emit-arguments (- si word-size) (cdr args))))

  (define (shift-arguments from to n)
    (unless (zero? n)
      (emit$ `(mov (deref rsp ,from) (reg rax)))
      (emit$ `(mov (reg rax) (deref rsp ,to)))
      (shift-arguments (- from word-size) (- to word-size) (sub1 n))))

  (match-define `(,fun ,args ...) expr)

  (if tail?
      (emit-arguments si args)
      (emit-arguments (- si word-size) args)) ;; leaves room for %rip that will be pushed by callq (I think)

  (if tail?
      (begin
        (shift-arguments si (- word-size) (length args))
        (emit-call #:tail? #t si (lookup-fun! fun env)))
      (begin
        (emit-adjust-base (+ si word-size))
        (emit-call si (lookup-fun! fun env))
        (emit-adjust-base (- (+ si word-size))))))

(define (lookup-fun! fun env)
  (define val (lookup fun env))
  (if val
      val
      (error 'compile-program "function `~s` is not defined" fun)))

(define (emit-adjust-base offset)
  (unless (= 0 offset)
    (emit$ `(addq (int ,offset) (reg rsp)))))

(define (emit-call #:tail? [tail? #f] si label)
  (emit$ `(,(if tail? 'jmp 'callq) (label ,label))))

(define (make-initial-env lambdas labels)
  (map cons lambdas labels))

(define (emit-expr si env expr #:tail? [tail? #f])
  (cond
    [(immediate? expr) (emit-immediate #:tail? tail? expr)]
    [(variable? expr) (emit-variable-ref #:tail? tail? env expr)]
    [(if? expr) (emit-if #:tail? tail? si env expr)]
    [(let? expr) (emit-let #:tail? tail? si env expr)]
    [(let*? expr) (emit-let* #:tail? tail? si env expr)]
    [(begin? expr) (emit-begin #:tail? tail? si env expr)]
    [(primcall? expr) (emit-primcall #:tail? tail? si env expr)]
    [(and? expr) (emit-and #:tail? tail? si env expr)]
    [(or? expr) (emit-or #:tail? tail? si env expr)]
    [(app? expr) (emit-app #:tail? tail? si env expr)]
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

(define (begin? expr)
  (match expr
    [`(begin ,body ...) #t]
    [else #f]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (app? expr)
  (and (pair? expr) (symbol? (car expr))))

(define (and? expr)
  (equal? 'and (car expr)))

(define (or? expr)
  (equal? 'or (car expr)))

(define (emit-immediate #:tail? [tail? #f] x)
  (emit$ `(movq (int ,(immediate-rep x)) (reg rax)))
  (when tail?
    (emit$ 'ret)))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (arithmetic-shift x fixnum-shift)]
    [(boolean? x) (if x true-rep false-rep)]
    [(null? x) null-rep]
    [(char? x) (bitwise-ior char-tag
                            (arithmetic-shift (char->integer x) char-shift))]))

(define (emit-variable-ref #:tail? [tail? #f] env var)
  (cond
    [(lookup var env) => (lambda (si)
                           (emit-stack-load si)
                           (when tail?
                             (emit$ 'ret)))]
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
          (make-label (format "L_~s" count))
        (set! count (add1 count))))))


(define (emit-if #:tail? [tail? #f] si env expr)
  (match-define `(if ,test ,con ,alt) expr)
  (define alt-label (unique-label))
  (define end-label (unique-label))
  (emit-expr si env test)
  (emit$ `(cmpq (int ,false-rep) (reg rax)))
  (emit$ `(je (label ,alt-label)))
  (emit-expr #:tail? tail? si env con)
  (unless tail?
    (emit$ `(jmp (label ,end-label))))
  (emit$ `(label ,alt-label))
  (emit-expr #:tail? tail? si env alt)
  (unless tail?
    (emit$ `(label ,end-label))))

(define (emit-let #:tail? [tail? #f] si env expr)
  (match-define `(let ,bindings ,body) expr)
  (define (process-let bindings si new-env)
    (cond
      [(null? bindings)
       (emit-expr #:tail? tail? si new-env body)]
      [else
       (let ([b (car bindings)])
         (emit-expr si env (cadr b))
         (emit-stack-save si)
         (process-let
          (cdr bindings)
          (next-stack-index si)
          (extend-env (car b) si new-env)))]))
  (process-let bindings si env))

(define (emit-let* #:tail? [tail? #f] si env expr)
  (match-define `(let* ,bindings ,body) expr)
  (define (process-let* bindings si env*)
    (cond
      [(null? bindings)
       (emit-expr #:tail? tail? si (extend-env* env* env) body)]
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
  (process-let* bindings si '()))

(define (next-stack-index i)
  (- i word-size))

(define (extend-env var si env)
  (cons (cons var si) env))

(define (extend-env* e1 e2)
  (append e1 e2))

(define (emit-stack-save si)
  (emit$ `(movq (reg rax) (deref rsp ,si))))

(define (emit-begin #:tail? [tail? #f] si env expr)
  (match expr
    [`(begin ,body)
     (emit-expr #:tail? tail? si env body)]
    [`(begin ,body0 ,body* ...)
     (emit-expr si env body0)
     (emit-expr #:tail? tail? si env `(begin ,@body*))]))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'compile-program "invalid primitive")))

(define (emit-primcall #:tail? [tail? #f] si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)
    (when tail?
      (emit$ 'ret))))

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
    [`(reg ,r) (string-append "%" (symbol->string r))] ;; XXX `(reg (? register? r))
    [`(deref ,reg ,offset) (format "~s(%~a)" offset (symbol->string reg))]
    [`(label ,l) l]
    [else
     (error 'compile-program "invalid operand `~s`" x)]))

(define (emit$ . args)
  (emit (apply $ args)))

(define-syntax-rule (emit-loop condition body ...)
  (begin
    (define loop-label (unique-label))
    (define done-label (unique-label))
    (emit$ `(label ,loop-label))
    (emit$ condition)
    (emit$ `(je (label ,done-label)))
    (emit$ body) ...
    (emit$ `(jmp (label ,loop-label)))
    (emit$ `(label ,done-label))))

;; Primitives

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (define-primitive-helper 'prim-name
       (length '(arg* ...))
       (lambda (si env arg* ...) b b* ...))]))

(define (define-primitive-helper prim-name arg-count emitter)
  (putprop prim-name '*is-prim* #t)
  (putprop prim-name '*arg-count* arg-count)
  (putprop prim-name '*emitter* emitter))

(define-syntax-rule (define-alias alias id)
  (define-primitive-helper 'alias
    (getprop 'id '*arg-count*)
    (getprop 'id '*emitter*)))

(define-primitive (cons si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  ;; place arg1 and arg2 at (%rbp) and 8(%rbp), respectively
  (emit$ `(movq (reg rax) (deref rbp 8)))
  (emit$ `(movq (deref rsp ,si) (reg rax)))
  (emit$ `(movq (reg rax) (deref rbp 0)))
  ;; place the pair-tagged value of %rbp in %rax
  (emit$ `(movq (reg rbp) (reg rax)))
  (emit$ `(orq (int ,pair-tag) (reg rax)))
  ;; bump %rbp by 16
  (emit$ `(addq (int ,pair-size) (reg rbp))))

(define-primitive (pair? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,pair-mask) (reg al)))
  (emit$ `(cmpb (int ,pair-tag) (reg al)))
  (emit=))

(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit$ `(movq (deref rax ,(- car-offset pair-tag)) (reg rax))))

(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit$ `(movq (deref rax ,(- cdr-offset pair-tag)) (reg rax))))

(define-primitive (set-cdr! si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (deref rsp ,si) (reg rbx)))
  (emit$ `(movq (reg rax) (deref rbx ,(- cdr-offset pair-tag)))))

(define-primitive (set-car! si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (deref rsp ,si) (reg rbx)))
  (emit$ `(movq (reg rax) (deref rbx ,(- car-offset pair-tag)))))

(define-primitive (eq? si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq (deref rsp ,si) (reg rbx)))
  (emit$ `(cmpq (reg rbx) (reg rax)))
  (emit=))

(define-primitive (make-vector si env arg1)
  (emit-expr si env arg1)
  (emit$ `(movq (reg rax) (deref rbp 0))) ;; store length in first slot
  (emit$ `(movq (reg rbp) (reg rbx))) ;; save heap pointer
  (emit$ `(addq (int ,word-size) (reg rbp))) ;; advance heap pointer
  (emit$ `(shrq (int ,fixnum-shift) (reg rax))) ;; obtain machine int

  (emit-loop `(cmpq (int 0) (reg rax))
    `(addq (int ,word-size) (reg rbp))
    `(subq (int 1) (reg rax)))

  (emit$ `(movq (reg rbx) (reg rax))) ;; restore heap pointer
  (emit$ `(orq (int ,vector-tag) (reg rax)))) ;; tag as vector

(define-primitive (vector? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,vector-mask) (reg al)))
  (emit$ `(cmpb (int ,vector-tag) (reg al)))
  (emit=))

(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit$ `(subq (int ,vector-tag) (reg rax))) ;; convert to pointer, vector->pointer
  (emit$ `(movq (deref rax 0) (reg rax)))) ;; first slot

;; usage: (vector-set! v i e)
(define-primitive (vector-set! si env arg1 arg2 arg3)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq  (deref rsp ,si) (reg rbx)))
  (emit$ `(mov (reg rax) (reg rdi)))
  (emit-expr si env arg3)
  ;; rbx=v rdi=i rax=e
  (emit$ `(shlq (int 1) (reg rdi))) ;; convert index into offset, so multuple by word-size
  (emit$ `(addq (reg rdi) (reg rbx))) ;; add to vector pointer v
  (emit$ `(subq (int ,vector-tag) (reg rbx))) ;; vector->pointer
  (emit$ `(movq (reg rax) (deref rbx 8)))) ;; store value e in slot

;; usage: (vector-ref v i)
(define-primitive (vector-ref si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  ;; rax=i (rsp)=v
  (emit$ `(shlq (int 1) (reg rax))) ;; index->offset
  (emit$ `(addq (deref rsp ,si) (reg rax))) ;; add to vector pointer
  (emit$ `(subq (int ,vector-tag) (reg rax))) ;; vector->pointer
  (emit$ `(movq (deref rax 8) (reg rax)))) ;; load value at slot

(define-primitive (make-string si env arg)
  (emit-expr si env arg)
  (emit$ `(movq (reg rax) (deref rbp 0))) ;; store length in first slot
  (emit$ `(movq (reg rbp) (reg rbx))) ;; save heap pointer
  (emit$ `(addq (int ,word-size) (reg rbp))) ;; advance heap pointer
  (emit$ `(shrq (int ,fixnum-shift) (reg rax))) ;; obtain machine int

  ;; Calculate Padding
  (emit$ `(movq (int ,word-size) (reg rdx)))
  (emit$ `(subq (reg rax) (reg rdx)))

  (emit-loop `(cmpq (int 0) (reg rax))
    `(addq (int 1) (reg rbp))
    `(subq (int 1) (reg rax)))
  (emit$ `(addq (reg rdx) (reg rbp))) ;; apply padding

  (emit$ `(movq (reg rbx) (reg rax))) ;; restore pointer to string
  (emit$ `(orq (int ,string-tag) (reg rax)))) ;; tag as string

(define-primitive (string? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,string-mask) (reg al)))
  (emit$ `(cmpb (int ,string-tag) (reg al)))
  (emit=))

;; usage: (string-set! s i c)
(define-primitive (string-set! si env arg1 arg2 arg3)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(movq  (deref rsp ,si) (reg rbx)))
  (emit$ `(mov (reg rax) (reg rdi)))
  (emit-expr si env arg3)
  ;; rax=c rdi=i rbx=s
  (emit$ `(shrq (int ,fixnum-shift) (reg rdi))) ;; obtain machine int
  (emit$ `(subq (int ,string-tag) (reg rbx))) ;; string->pointer
  (emit$ `(addq (reg rdi) (reg rbx))) ;; add to string pointer v
  (emit$ `(shrq (int ,char-shift) (reg rax))) ;; obtain machine byte (char)
  (emit$ `(movb (reg al) (deref rbx 8)))) ;; store value c in slot

;; usage: (string-ref v i)
(define-primitive (string-ref si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  ;; rax=i (rsp)=v
  (emit$ `(shrq (int ,fixnum-shift) (reg rax))) ;; obtain machine int
  (emit$ `(addq (deref rsp ,si) (reg rax))) ;; add to string pointer
  (emit$ `(subq (int ,string-tag) (reg rax))) ;; string->pointer
  (emit$ `(movzbq (deref rax 8) (reg rax))) ;; load value at slot
  (emit$ `(shlq (int ,char-shift) (reg rax)))
  (emit$ `(orq (int ,char-tag) (reg rax))))

(define-primitive (string-length si env arg)
  (emit-expr si env arg)
  (emit$ `(subq (int ,string-tag) (reg rax))) ;; convert to pointer, string->pointer
  (emit$ `(movq (deref rax 0) (reg rax)))) ;; first slot

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit$ `(addq (int ,(immediate-rep 1)) (reg rax))))

(define-alias fxadd1 $fxadd1)

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit$ `(shlq (int ,(- char-shift fixnum-shift)) (reg rax)))
  (emit$ `(orq (int ,char-tag) (reg rax))))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit$ `(shrq (int ,(- char-shift fixnum-shift)) (reg rax))))

(define-alias fixnum->char $fixnum->char)
(define-alias char->fixnum $char->fixnum)

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,fixnum-mask) (reg al)))
  (emit$ `(cmpb (int ,fixnum-tag) (reg al)))
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

(define (emit-cmp compare)
  (emit$ `(,compare (reg al)))
  (emit$ `(movzbq (reg al) (reg rax)))
  (emit$ `(salq (int ,bool-bit) (reg rax)))
  (emit$ `(orq (int ,false-rep) (reg rax))))

(define (emit-and #:tail? [tail? #f] si env expr)
  (match expr
    [`(and) (emit-expr #:tail? tail? si env #t)]
    [`(and ,e) (emit-expr #:tail? tail? si env e)]
    [`(and ,e0 ,e1 ...)
     (emit-if #:tail? tail? si env `(if ,e0 (and ,@e1) #f))]))

(define (emit-or #:tail? [tail? #f] si env expr)
  (match expr
    [`(or) (emit-expr #:tail? tail? si env #f)]
    [`(or ,e) (emit-expr #:tail? tail? si env e)]
    [`(or ,e0 ,e1 ...)
     (emit-if #:tail? tail? si env `(if ,e0 ,e0 (or ,@e1)))]))

(define (emit-binary-expr si env arg1 arg2)
  (emit-expr si env arg1)
  (emit$ `(movq (reg rax) (deref rsp ,si))) ;; Could put things in register, rdi, for example
  (emit-expr (- si word-size) env arg2))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit$ `(cmpq (int ,(immediate-rep 0)) (reg rax)))
  (emit=))

(define-alias fxzero? $fxzero?)

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit$ `(cmpq (int ,null-rep) (reg rax)))
  (emit=))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int #x3F) (reg al)))
  (emit$ `(cmpb (int #x2F) (reg al)))
  (emit=))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit$ `(and (int ,char-mask) (reg al)))
  (emit$ `(cmpb (int ,char-tag) (reg al)))
  (emit=))

(define-primitive (char= si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(cmpq (deref rsp ,si) (reg rax)))
  (emit=))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit$ `(cmpb (int ,false-rep) (reg al)))
  (emit=))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit$ `(shrq (int ,fixnum-shift) (reg rax)))
  (emit$ `(notq (reg rax)))
  (emit$ `(shlq (int ,fixnum-shift) (reg rax))))
;; XXX add more tests, to check 00b

(define-alias fxlognot $fxlognot)

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit$ `(subq (int ,(immediate-rep 1)) (reg rax))))

(define-alias fxsub1 $fxsub1)

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(addq (deref rsp ,si) (reg rax))))

(define-primitive (fx- si env arg1 arg2)
  (emit-binary-expr si env arg2 arg1)
  ;; rax=arg1 si(rsp)=arg2 XXX (%let ([rax arg1] [(deref rsp si) arg2]) ...)
  (emit$ `(subq (deref rsp ,si) (reg rax))))

;; 4xy = (4x * 4y) / 4
(define-primitive (fx* si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(mulq (deref rsp ,si)))
  (emit$ `(shrq (int 2) (reg rax))))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-binary-expr si env arg1 arg2)
  (emit$ `(orq (deref rsp ,si) (reg rax))))

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
  (emit-binary-expr si env arg2 arg1)
  (emit$ `(cmpq (deref rsp ,si) (reg rax))))

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
