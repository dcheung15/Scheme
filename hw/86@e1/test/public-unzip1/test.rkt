#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (unzip '((cis . 252) (352))))))