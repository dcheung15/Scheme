#lang racket

;; Exercises 5: Fixed-Point Combinators

(provide fib-using-letrec
         length-using-u
         fib-using-u
         fib-using-y
         sum-using-y)

;; The U combinator
(define U (lambda (x) (x x)))

(define (fib-without-letrec x)
  (cond [(= x 0) 1]
        [(= x 1) 1]
        [else (+ (fib-without-letrec (- x 1)) (fib-without-letrec (- x 2)))]))

;; Exercise
;; Use letrec to define fib
(define (fib-using-letrec x)
  (letrec ([fib (lambda (x)
                  (cond
                    [(= x 0) 1]
                    [(= x 1) 1]
                    [else (+ (fib (- x 1)) (fib (- x 2)))]))])
    (fib x))
  )

;; The U Combinator approach
;; (let ([f (lambda (mk-f)
;;            (lambda (x)
;;              (if (= x 0)
;;                  1
;;                  (* x ((mk-f mk-f) (sub1 x))))))])
;;   ((f f) 20))

(define (length-using-u lst)
  (let ([len (U (lambda (f)
                  (lambda (lst)
                    (if (empty? lst)
                        0
                        (add1 ((f f) (rest lst)))))))])
    (len lst)))

;; Define fib using the U combinator
(define (fib-using-u lst)
  (let ([fib (U (lambda (f)
                  (lambda (x)
                    (cond
                      [(= x 0) 1]
                      [(= x 1) 1]
                      [else (+ ((f f) (- x 1)) ((f f) (- x 2)))]))))])
    (fib lst)))


(define Y ((λ (x) (x x))
           (λ (y) (λ (f) (f (λ (x) (((y y) f) x)))))))

;; Define fib using the Y combinator
(define (fib-using-y x)
  (let ([fib (Y (lambda (f)
                  (lambda (x)
                    (cond
                      [(= x 0) 1]
                      [(= x 1) 1]
                      [else (+ (f (- x 1)) (f (- x 2)))]))))])
    (fib x)))

;; Define sum (the sum of a list of numbers) using the Y combinator
(define (sum-using-y lst)
  (let ([sum (Y (lambda (f)
                       (lambda (lst)
                         (if (empty? lst)
                             0
                             (+ (first lst) (f (rest lst)))))))])
    (sum lst)))
