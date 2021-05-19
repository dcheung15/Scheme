#lang racket
(provide    mk-point
      point?
      point-dist
      binary-tree?
      sorted-tree?
      balanced-tree?)

; Now we assume a point is in format of '(point x y), where x and y are integers. For example, '(point 1 3) is a point at x=3, y=4

; Now we need a constructor for the point, which takes into two integers and returns a point
(define (mk-point x y)
  (cons 'point (cons x (cons y '() )))
  )

; Now, how can we know whether a object is a point? We need a function point? takes in a parameter, checks
; 1. first element in this list is 'point
; 2. Second element in this list is number
; 3. Third element in this list is number
; 4. This list only contains these 3 elements
(define (point? x)
  (and (list? x)
       (equal? (length x) 3)
       (equal? (first x) 'point)
       (number? (second x))
       (number? (third x)))
   )

; Remember that point-distance in e0? Now we want to apply that to points!
; Write a function point-dist which takes in 2 points, return the distance between these 2 points
(define (point-dist p1 p2)
  (define (dist x0 y0 x1 y1)
    (define (square x) (* x x))
    (sqrt (+ (square (- x1 x0)) (square (- y1 y0)))))
  (dist (second p1) (third p1) (second p2) (third p2))
  )

; In the video we see how binary tree looks like, please complete the binary-tree?, which checks whether t is a binary tree
;;(node 2 (node 7 (leaf 2) ( leaf 4)) (node 24 (node 12 (leaf 2) (leaf 4)) (leaf 3)))
(define (binary-tree? t)
  (cond
    [(and (equal? (length t) 2) (equal? (first t) 'leaf) (number? (second t))) #t] ;;a leaf
    [(and (equal? (length t) 4) (equal? (first t) 'node) (number? (second t)) (binary-tree? (third t)) (binary-tree? (fourth t))) #t] ;;a node
    [else #f]
    )
  )

;;helper function to get height of the tree
(define (tree-height t)
  (cond [(equal? (first t) 'leaf) 1] ;;a leaf
        [(equal? (first t) 'node) (+ 1 (max (tree-height (third t)) (tree-height (fourth t))))] ;a node and adds 1 to the existing height
        )
  )

; A sorted tree is a tree that, any node is greater than the left side, but less than the right side
; Write a function sorted-tree, checks whether a binary tree is sorted
(define (sorted-tree? t)
  (cond
    [(and (equal? (first t) 'leaf) (binary-tree? t)) #t]
    [(and (equal? (first t) 'node) (< (second t) (second (fourth t))) (> (second t) (second (third t))) (sorted-tree? (third t)) (sorted-tree? (fourth t))) #t]
    [else #f]
    )
  )

; A balanced tree is a binary tree that, keep its height at minimal. 
; In other words, the longest path to any leaf and the shortest path to any leaf is equal or less than 1
; Write a function is-balanced-tree?, checks whether a binary tree is balanced
(define (balanced-tree? t)
  (cond [(equal? (first t) 'leaf) #t] ;;a leaf and if leaves are balanced
        [(equal? (first t) 'node) ;;nodes are balanced when their children are balanced and height does not differ by 1
         (let* ([left-child (third t)]
                [right-child (fourth t)]
                [left-child-height (tree-height left-child)]
                [right-child-height (tree-height right-child)])
        (and (balanced-tree? left-child) (balanced-tree? right-child) (<= (abs (- left-child-height right-child-height)) 1)))]
        )
   )
  