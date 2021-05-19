#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-dist (mk-point 2 7) (mk-point 14 2)))))