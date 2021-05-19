#lang racket

(require "../../e5.rkt")

(print (sum-using-y (range 20 500)))

(with-output-to-file "output"
                     (lambda ()
                       (print (sum-using-y (range 20 500))))
                     #:exists 'replace)

