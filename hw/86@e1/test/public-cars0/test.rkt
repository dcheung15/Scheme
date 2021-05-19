#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (cars '((1 . a) (2 . b) (3 . c))))))