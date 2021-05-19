#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-dist (mk-point 5 9) (mk-point 9 6)))))