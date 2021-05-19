#lang racket

(require "../../e1.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (cdrs '((ecs cis) (phi . 352) (wrong cis . 486))))))