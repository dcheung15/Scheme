#lang racket

(require "../../e5.rkt")

(print (fib-using-letrec 10))

(with-output-to-file "output"
                     (lambda ()
                       (print (fib-using-letrec 10)))
                     #:exists 'replace)

