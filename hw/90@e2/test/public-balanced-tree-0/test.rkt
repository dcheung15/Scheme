#lang racket

(require "../../e2.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (balanced-tree? '(node 8 (node 5 (leaf 2) (leaf 7)) (node 12 (leaf 11) (leaf 18)))))))