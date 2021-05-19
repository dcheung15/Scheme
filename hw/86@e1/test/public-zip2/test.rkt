#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (zip '() '(a b c d e)))))