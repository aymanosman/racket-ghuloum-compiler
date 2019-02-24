#lang racket
(require (only-in "../tests-driver.rkt" emit current-system-type))

(define (make-label s)
  (if (equal? 'macosx (current-system-type))
      (string-append "_" s)
      s))

(define (compile-program x)
  (unless (integer? x) (error 'compile-program "expected integer, given `~s`" x))
  (emit "    .text")
  (emit (format "    .globl ~a" (make-label "scheme_entry")))
  (emit (make-label "scheme_entry:"))
  (emit "    movq $~s, %rax" x)
  (emit "    ret"))

(module+ test
  (require (only-in "../tests-driver.rkt" compiler test-all)
           "../tests/tests-1.1-req.rkt")

  (parameterize ([compiler compile-program])
    (test-all)))
