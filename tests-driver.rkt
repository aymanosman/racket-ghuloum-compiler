#lang racket

(provide emit
         test-all
         compiler
         compile-port
         current-system-type
         add-tests-with-string-output)

(define all-tests '())

(define (add-test! t)
  (set! all-tests (cons t all-tests)))

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (add-test!
      '(test-name [expr string  output-string] ...))]))

(define (run-compile expr)
  (call-with-output-file "stst.s" #:exists 'replace
    (lambda (out)
      (parameterize ([compile-port out])
        ((compiler) expr)))))

(define current-system-type (make-parameter 'macosx)) ;; 'linux

(define (build)
  (unless (if (equal? 'macosx (current-system-type))
              (system "gcc -Wall -Wconversion -fomit-frame-pointer -fno-asynchronous-unwind-tables -o stst runtime.c stst.s")
              (system "docker exec box1 gcc -o stst runtime.c stst.s"))
    (error 'make "could not build target")))

(define (execute)
  (unless (if (equal? 'macosx (current-system-type))
              (system "./stst > stst.out")
              (system "docker exec box1 ./stst > stst.out"))
    (error 'make "produced program exited abnormally")))

(define (test-with-string-output test-id expr expected-output)
  (run-compile expr)
  (build)
  (execute)
  (unless (string=? expected-output (file->string "stst.out"))
    (error 'test "output mismatch for test ~s, expected ~s, got ~s"
           test-id expected-output (file->string "stst.out"))))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "test ~s:~s ..." test-id expr)
    (flush-output)
    (case type
      [(string) (test-with-string-output test-id expr out)]
      [else (error 'test "invalid test type ~s" type)])
    (printf " ok\n")))

(define (test-all)
  (dynamic-wind
    (lambda ()
      (start-container))
    (lambda ()
      (do-test-all))
    (lambda ()
      (stop-container))))

;; TODO no-docker-flag

(define (start-container)
  (when (not (equal? 'macosx (current-system-type)))
    (system "docker stop box1 || true")
    (unless (system "docker run -dt --rm --name box1 -v $PWD:/data -w /data gcc")
      (error 'make "failed to start container"))))
(define (stop-container)
  (when (not (equal? 'macosx (current-system-type)))
    (unless (system "docker stop box1")
      (error 'make "failed to stop container"))))

(define (do-test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "passed all ~s tests\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests))
                 (g (add1 i) (cdr tests))])))))))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define compile-port
  (make-parameter
   (current-output-port)))

(define compiler
  (make-parameter
   (lambda (exp) (error 'compiler "you need to define this!!"))))
