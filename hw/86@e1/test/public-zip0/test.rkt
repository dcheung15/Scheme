#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (zip '(1 2 3) '(a b c d e)))))
