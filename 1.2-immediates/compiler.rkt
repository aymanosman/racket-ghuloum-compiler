#lang racket/base
(require (only-in "../tests-driver.rkt" emit current-system-type))

(define bool_f   #x2F)
(define bool_t   #x6F)
(define nil      #x3F)

(define fixnum-shift 2)

(define char-tag #x0F)
(define char-shift 8)

(define (make-label s)
  (if (equal? 'macosx (current-system-type))
      (string-append "_" s)
      s))

(define (compile-program x)
  (unless (immediate? x) (error 'compile-program "expected immediate, given `~s`" x))
  (emit "    .text")
  (emit (format "    .globl ~a" (make-label "scheme_entry")))
  (emit (make-label "scheme_entry:"))
  (emit "    movq $~s, %rax" (immediate-rep x))
  (emit "    ret"))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (arithmetic-shift x fixnum-shift)]
    [(boolean? x) (if x bool_t bool_f)]
    [(null? x) nil]
    [(char? x) (bitwise-ior char-tag
                            (arithmetic-shift (char->integer x) char-shift))]))

(module+ test
  (require (only-in "../tests-driver.rkt" compiler test-all)
           "../tests/tests-1.2-req.rkt"
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))
