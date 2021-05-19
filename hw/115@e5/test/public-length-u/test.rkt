#lang racket

(require "../../e5.rkt")

(print (length-using-u (range 100 200)))

(with-output-to-file "output"
                     (lambda ()
                       (print (length-using-u (range 100 200))))
                     #:exists 'replace)

