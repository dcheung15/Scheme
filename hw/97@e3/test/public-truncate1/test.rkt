#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (truncate-d 0 0 7 '(0 1 2 3 4 5 6)))))