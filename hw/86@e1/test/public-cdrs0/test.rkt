#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (cdrs '((a 1) (c . 2) (d 3))))))