#lang racket

(require "../../e4.rkt")

(define (print-sorted-sets s)
  (pretty-print (sort (map pretty-format (set->list s)) string<?)))

(with-output-to-file "output"
                     (lambda ()
                       (displayln (pretty-format (expr->debruijn '((lambda (x) x) ((lambda (x) (y (lambda (x) x))) (lambda (z) z))))))))
