#lang racket

(require "../../e5.rkt")

(print (fib-using-u 11))

(with-output-to-file "output"
                     (lambda ()
                       (print (fib-using-u 11)))
                     #:exists 'replace)

