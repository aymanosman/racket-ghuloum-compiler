#lang racket

(require "../tests-driver.rkt")

(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (fxzero? n)
                       ac
                       (sum (fxsub1 n) (fx+ n ac))))])
     (sum 10000 0)) => "50005000\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))])
     (e 5000000)) => "#t\n"])
