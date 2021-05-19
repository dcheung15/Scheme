#lang racket

(require "../../e3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (truncate-t 0 0 1000 (build-list 1000 (lambda (x) x))))))
