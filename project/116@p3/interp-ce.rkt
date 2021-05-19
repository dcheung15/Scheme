#lang racket

;; Assignment 3: A CE (Control and Environment) interpreter for Scheme

(provide interp-ce)

; Your task is to write a CE interpreter for a substantial subset of Scheme/Racket. 
; A CE interpreter is meta-circular to a large degree (e.g., a conditional in the target
; language (scheme-ir?) can be implemented using a conditional in the host language (Racket),
; recursive evaluation of a sub-expression can be implemented as a recursive call to the
; interpreter, however, it's characterized by creating an explicit closure value for lambdas
; that saves its static environment (the environment when it's defined). For example, a CE
; interpreter for the lambda calculus may be defined:
(define (interp-ce-lambda exp [env (hash)])
  (match exp
    [`(lambda (,x) ,body)
     ; Return a closure that pairs the code and current (definition) environment
     `(closure (lambda (,x) ,body) ,env)]
    [`(,efun ,earg)
     ; Evaluate both sub-expressions
     (define vfun (interp-ce-lambda efun env))  
     (define varg (interp-ce-lambda earg env))
     ; the applied function must be a closure
     (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
     ; we extend the *closure's environment* and interp the body
     (interp-ce-lambda body (hash-set env+ x varg))]
    [(? symbol? x)
     ; Look up a variable in the current environment
     (hash-ref env x)]))

; Following is a predicate for the target language you must support. You must support any
; syntax allowed by scheme-ir that runs without error in Racket, returning a correct value..
(define (scheme-ir? exp)
  ; You should support a few built-in functions bound to the following variables at the top-level
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (match exp
    [`(lambda ,(? (listof symbol?)) ,(? scheme-ir?)) #t] ; fixed arguments lambda
    [`(lambda ,(? symbol?) ,(? scheme-ir?)) #t] ; variable argument lambda
    [`(if ,(? scheme-ir?) ,(? scheme-ir?) ,(? scheme-ir?)) #t]
    [`(let ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
    [`(let* ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
    [`(and ,(? scheme-ir?) ...) #t]
    [`(or ,(? scheme-ir?) ...) #t]
    [`(apply ,(? scheme-ir?) ,(? scheme-ir?)) #t]
    [(? (listof scheme-ir?)) #t]
    [(? prim?) #t]
    [(? symbol?) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [''() #t]
    [_ #f]))

; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points. 
(define (interp-ce exp)
  (define (do-call clo-ef args)
      (match clo-ef
        [`(closure (lambda (,xs ...) ,ebody) ,cloenv)
         (interp ebody (foldl (lambda (x e acc) (hash-set acc x e))
                            cloenv xs args))]
        [`(closure (lambda ,x ,ebody) ,cloenv)
         (interp ebody (hash-set cloenv x args))]))
  ;; exp is a Scheme expression that has the form scheme-ir?
  ;; env is a hash from variables to values
  (define (interp exp env abort)
(match exp
  ;; how do we handle a variable
  [(? symbol? s) (hash-ref env s (abort `(error BAD-DNE ,s)))]
  [(? boolean? x) x]
  [''() '()]
  [(? number? n) n]
  [`(lambda ,x ,ebody)
   `(closure ,exp ,env)]
  [`(if ,ec ,et ,ef) (if (interp ec env abort) (interp et env abort) (interp ef env abort))]
  [`(and ,es ...) (andmap (lambda (e) (interp e env abort)) es)]
  [(or) #f]
  [`(or ,e1 ,e2 ...) (or (interp e1 env) (interp `(and ,@e2) env abort))]
  ;[`(or ,es ...) (ormap (lambda (e) (interp e env abort)) es)]
  [`(let ([,xs ,xbnd] ...) ,ebody)
   (interp ebody (foldl (lambda (x e acc) (hash-set acc x (interp e env)))
                  env xs xbnd) abort)]
  [`(let* () ,ebody) (interp ebody env abort)]
  [`(let* ([,fst ,rst] ,bnds ...) ,ebody) (interp `(let ([,fst ,rst]) (let* ,bnds ,ebody)) env abort)]
  [`(lambda (,x ...) ,body) `(closure ,exp ,env)]
      
  ; Untagged application case goes after all other forms
  [`(apply ,ef ,ex)
   ;; Assume that ef evaluates to a closure
   (define clo-ef (interp ef env abort))
   (define args (interp ex env abort))
   (do-call clo-ef args)]
  [`(,ef ,es ...)
   ;; Assume that ef evaluates to a closure
   (define clo-ef (interp ef env abort))
   ;; argument list
   (define v-args (map (lambda (e-arg) (interp e-arg env abort))
                       es))
   (match clo-ef
     [`(closure (lambda (,xs ...) ,e-body) ,clo-env)
      ;; set each of xs to its corresponding position in v-args
      (define new-env
        (foldl (lambda (x v env) (hash-set env x v)) clo-env xs v-args))
      (interp e-body new-env abort)]
     [_ `(error (format "Expected closure but got ~a" clo-ef))])]))

;; end of it
(call/cc (lambda (k) (interp exp (hash) k)
)))
;;(interp-ce '(((lambda (x) (lambda (y) x)) 10) 20))
;;(interp-ce '((lambda (z) (lambda (x y) x)) 30))

