#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point? (mk-point 'a 6)))))