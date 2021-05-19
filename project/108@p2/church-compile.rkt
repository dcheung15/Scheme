#lang racket

;; Project 2: A Church encoding compiler that translates Scheme to the
;; Lambda-calculus

(provide church-compile
         ; provided conversions:
         church->nat
         church->bool
         church->listof)

;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)
;     | (let ([x e] ...) e)
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket

;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))

;; -------------------------------------------------
;; Write your church-compiling code below:

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(define (churchify e)
  (define (prim? expr) (if (member expr prims) expr #f)) ;; checks if the expression can be translated into lambda calculus and vice versa
  
  ;;converts the expression into the input langage
  (define (p-splice expr) (string->symbol (string-append "church-" (symbol->string expr))))

  (match e
    ;Variable
    [(? symbol? x) x]

    ;Let    
    [`(let ([,x ,y]) ,body) (churchify `((lambda (,x) ,body) ,y))]
    [`(let ([,x ,y] ...) ,body) (churchify `((lambda ,x ,body) ,@y))]
    
    ;Curry Lambda
    [`(lambda () ,body) `(lambda (_) ,(churchify body))]
    [`(lambda (,x) ,body) `(lambda (,x) ,(churchify body))]
    [`(lambda (,x ,y ...) ,body) `(lambda (,x) ,(churchify `(lambda ,y ,body)))]
    
    ;If statement
    [`(if ,x ,true ,false) (churchify `(,x (lambda () ,true) (lambda () ,false)))]

    ;And
    [`(and ,e1 ,e2)(churchify `(if ,e1 (if ,e2 #t #f) #f))]
    [`(and ,e1 ,e2 ...) (churchify `(if ,e1 (and ,@e2 #f) #f))]
    
    ;Or
    [`(or ,e1 ,e2)(churchify `(if ,e1 #t (if ,e2 #t #f)))]
    [`(or ,e1 ,e2 ...)(churchify `(if ,e1 #t (or ,@e2 #t) #f))]

    ;Primitives
    [`(,(? prim? p) ,exp ...)(churchify `(,(p-splice p) ,@exp))]
    
    ;Booleans
    [#t (churchify '(lambda (t f) (t)))]
    [#f (churchify '(lambda (t f) (f)))]

    ;Null Values
    [''() (churchify '(lambda (x y) (y)))]
    
    ;Curry Applications 
    [`(,e1) `(,(churchify e1) (lambda (x) x))]
    [`(,e1  ,earg0)`(,(churchify e1) ,(churchify earg0))]
    [`(,e1 ,earg0 ,eargs ...) (churchify `((,e1 ,earg0) ,@eargs))]

    ;Fixed data
    [(? integer? n)
     (define (wrap n)
       (if (zero? n)
           'x
           `(f ,(wrap (sub1 n)))))
       (churchify `(lambda (f x) ,(wrap n)))]
    
    ;anything else
    [_ e]
    ) ;;end of match
  )

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  (define churchAddGen `(lambda (x y) (lambda (f z) (y f (x f z)))))
  (define churchMult `(lambda (x y) (lambda (f z) ((x (y f)) z))))
  (define churchAdd1 `(lambda (x) (lambda (f z) (f ((x f) z)))))
  (define negate `(lambda (x y f) (x f y)))
  (define cons `(lambda (m n) (lambda (wc wn) (wc m n))))
  
  (churchify
   `(let
        ([church-add1 ,churchAdd1]
         [church-+ ,churchAddGen]
         [church-* ,churchMult]
         [church-not, negate]
         [church-cons ,cons]
         )
      ,program)))
