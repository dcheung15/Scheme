#lang racket

(require "../../e3.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (filter-t (lambda (x) #t) '(0 1 2 3 4 5)))))
