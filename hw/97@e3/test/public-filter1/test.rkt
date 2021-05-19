#lang racket

(require "../../e3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (filter-t number? '(0 1 err0 2 err1 3 err2 4 5)))))
