#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-dist (mk-point 2 4) (mk-point 7 4)))))