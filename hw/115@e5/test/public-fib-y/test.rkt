#lang racket

(require "../../e5.rkt")

(print (fib-using-y 9))

(with-output-to-file "output"
                     (lambda ()
                       (print (fib-using-y 9)))
                     #:exists 'replace)

