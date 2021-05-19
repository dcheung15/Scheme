#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (cars '((cis) (352 486 . 341) (cis ecs) (251 . 341))))))