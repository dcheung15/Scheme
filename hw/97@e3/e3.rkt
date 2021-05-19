#lang racket
;; Exercises 3: Direct-style and tail recursion
(provide
      truncate-t
      truncate-d
      filter-t)

;; If we have a list, how can we truncate the list from index i to j?
;; Write a function truncate takes 3 param, i j and list return the
;; list starting at i and end at j, (truncate 2 5 '(0 1 2 3 4 5 6)) ->
;; '(2 3 4 5).
(define (truncate-d now i j lst)
  (define (take i l)
    (match i
      [0 '()]
      [n #:when (> n 0) (remove i l)]
       ))
  (match i
    [0 (take j lst)]
    [n #:when (> n 0) (truncate-d now (- i 1) (- j 1) (rest lst))])
  )

;; Rewrite truncate to use tail-recursion
;; HINT: In tail recursion you may need to revrese the
;;; result when return it You can assume i < j and j <= length of lst
(define (truncate-t now i j lst)
  (define (h now i j lst acc)
    (match i
      [(? positive-integer? i) (h (+ 1 now) (- 1 now) (- j 1) lst acc)]
      [0 (if (>= j 0)
             (if (>= now (length lst))
                 (reverse acc)
                 (h (+ 1 now) i (- j 1) lst (cons (list-ref lst now) acc)))
             (reverse acc))]))
  (h now i j lst '())
  )

;; The function filter-t should filter lst so that it returns the
;; subset of lst that satisfies func.
(define (filter-t func lst)
  (define (h lst acc)
    (match lst
      ['() acc]
      [`(,hd . ,tl) #:when (func hd) (h tl (cons hd acc))]
      [`(,hd . ,tl) (h tl acc)]))
  (reverse (h lst '())))
