#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (binary-tree? '(node 2 (node 7 (leaf 2) (leaf a)) (node 24 (node 12 (leaf 2) (leaf 4)) (leaf 3)))))))