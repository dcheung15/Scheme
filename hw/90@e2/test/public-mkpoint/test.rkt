#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (mk-point 2 6))))