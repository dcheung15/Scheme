;; Exercise e4
#lang racket

(provide expr->debruijn
         expr?
         dexpr?
         next-betas
         next-betas-debruijn)

;; Lambda calculus terms
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e-body)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; Free variables
(define (free-vars e)
  (match e
    [(? symbol? x) (set x)]
    [`(lambda (,x) ,e-body) (set-remove (free-vars e-body) x)]
    [`(,e0 ,e1) (set-union (free-vars e0) (free-vars e1))]))

;; Use capture-avoiding substitution to replace x with e-tgt in e-source
(define (subst e-source x e-tgt)
  (match e-source
    [(? symbol? y) #:when (equal? x y) e-tgt]
    [(? symbol? y) y]
    [`(,e0 ,e1) `(,(subst e0 x e-tgt) ,(subst e1 x e-tgt))]
    [`(lambda (,y) ,e-body) #:when (equal? x y) e-source] ;; leave it alone
    [`(lambda (,y) ,e-body) #:when (not (set-member? (free-vars e-tgt) y))
       `(lambda (,y) ,(subst e-body x e-tgt))]
    [_ (error "No capture-avoiding substitution possible.")]))

;; De-Bruijn Lambda Calculus
;; In this exercise, we will compare the alpha-equivalence of two
;; arbitrary lambda-calculus terms. To check whether two arbitrary
;; terms are alpha-equivalent, we will canonicalize both terms to
;; some common representation. This representation will use
;; De-Bruijn indices. De-Bruijn indices replace the typical
;; (lambda (x) ...) form with a (dlambda ...) form. Then, instead
;; of variables, there are (natural) numbers. The interpretation
;; of a number n is that it is a variable referring to the nth
;; most proximate syntactic binder.
;;
;; For example, we might replace the following LC expression:
;;   '(lambda (x) (lambda (y) (x y))
;; with the equivalent de-bruijn expression:
;;   '(dlambda (dlambda (1 0))
;;
;; Notice how x is replaced by 1, because x is the
;; next-most-proximate binder compared to y (0).

;; De-Bruijn Lambda Terms (definition, do not change)
(define (dexpr? e)
  (match e
    [(? nonnegative-integer? n) #t]         ;; bound variables
    [(? symbol? x) #t]                      ;; free variables
    [`(dlambda ,(? dexpr? e-body)) #t]      ;; lambdas
    [`(,(? dexpr? e0) ,(? dexpr? e1)) #t])) ;; applications

;; Translate a conventional lambda-calculus term to a De-Bruijn term
;; Free variables should be left alone. For example:
;; (expr->debruijn (x (lambda (y) y)) => '(x (dlambda 1))
(define (expr->debruijn e)
  (define (h e x n)
    (match e
      [(? number? n) n]
      [(? symbol? y) #:when (equal? x y) n]
      [(? symbol? y) y]
      [`(dlambda ,e-body) `(dlambda ,(h e-body x (add1 n)))]
      [`(lambda (,y) ,e-body) #:when (equal? x y) e]
      [`(lambda (,y) ,e-body) `(lambda (,y) ,(h e-body x (add1 n)))]
      [`(,e0 ,e1) `(,(h e0 x n) ,(h e1 x n))]))
  (match e
    [(? symbol? x) x]
    [`(lambda (,x) ,e-body) `(dlambda ,(h (expr->debruijn e-body) x 1))]
    [`(,e0 ,e1) `(,(expr->debruijn e0) ,(expr->debruijn e1))]))
  

;; Identify each potential beta reduction of the term e within any subterm.
;; Hint: use subst above...!
;; Return a set of expressions
;;
;; You may assume the following variable convention (which simplifies
;; substitution): free variables are always distinct (i.e., never have
;; the same name as) bound variables.
(define/contract (next-betas e)
  (-> expr? (set/c expr?))
  (match e
    [(? symbol? x) (set)] ;; no possible reductions
    [`((lambda (,x) ,e-body) ,e1)
     ;; compute all potential beta-redexes of e1
     (define e1-betas (next-betas e1))
     (set-add (foldl (lambda (e-next acc)
                       (set-add acc `((lambda (,x) ,e-body) ,e-next)))
                     (set)
                     (set->list e1-betas))
              (subst e-body x e1))]
    [`(lambda (,x) ,e-body) 'todo]))
;; Do not change this definition, we use it to canonicalize terms for outputs
(define/contract (next-betas-debruijn e)
  (-> expr? (set/c dexpr?))
  (foldl (lambda (expr acc) (set-add acc (expr->debruijn expr))) (set) (set->list (next-betas e))))
