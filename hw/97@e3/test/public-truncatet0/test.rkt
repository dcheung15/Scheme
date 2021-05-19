#lang racket

(require "../../e3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (truncate-t 0 2 5 '(0 1 2 3 4 5 6)))))
