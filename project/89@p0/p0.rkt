#lang racket
;;; Project 0 Tic-tac-toe with Racket
;;; Please immediately read README.md
(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)
 
;; Useful utility functions
; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; Check whether a list is a valid board: Its length is a square of some integer. It contains only the symbols 'X 'O 'E. The number of Xs and Os differ by at most 1. X moves first
(define (board? lst)
  (if (integer? (sqrt(length lst))) #t #f)
  (equal? (car lst) 'X) ;;checks if x is the first move
;; makes sure of the difference between X and O is at most by 1
  (cond
    [(or (equal? (+ (count (lambda (x) (equal? x 'X)) lst) 1) (count (lambda (x) (equal? x 'O)) lst))
         (equal? (- (count (lambda (x) (equal? x 'X)) lst) 1) (count (lambda (x) (equal? x 'O)) lst))
         (equal? (count (lambda (x) (equal? x 'X)) lst) (count (lambda (x) (equal? x 'O)) lst)))]
    [else #f])
  )

;;; From the board, calculate who is making a move this turn
;;return 'X or 'O
(define (next-player board)
  (if (equal? (- (count (lambda (x) (equal? x 'X)) board) 1) (count (lambda (x) (equal? x 'O)) board) )
      'O
      'X)
  )

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
;;board = list, (row & col = numbers), player = 'X or 'O
;; returns bool
(define (valid-move? board row col player)
   (if (and (< row (sqrt (length board)))
            (< col (sqrt (length board)))
            (equal? (next-player board) player))
       #t
       #f)
  )

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (list-set board (+ col (* row (sqrt (length board)))) player))


;; To determine whether there is a winner
(define (winner? board)
      (cond
        [(rowcheck (indexes-of board 'X) (sqrt (length board)) (build-list (sqrt (length board)) values)) 'X] ;;X is winner by row of n
        [(rowcheck (indexes-of board 'O) (sqrt (length board)) (build-list (sqrt (length board)) values)) 'O] ;;O is winner by row of n
        [(colcheck (indexes-of board 'X) (sqrt (length board)) (range 0 (sub1 (length board)) (sqrt (length board)))) 'X] ;;X is winner by col
        [(colcheck (indexes-of board 'O) (sqrt (length board)) (range 0 (sub1 (length board)) (sqrt (length board)))) 'O] ;;O is winner by col
        [(diagonal (indexes-of board 'X) (sqrt (length board)) (range 0 (length board) (add1 (sqrt (length board))))) 'X] ;;X is winner by diagonal
        [(diagonal (indexes-of board 'O) (sqrt (length board)) (range 0 (length board) (add1 (sqrt (length board))))) 'O] ;;O is winner by diagonal
        [(diagonal (indexes-of board 'X) (sqrt (length board)) (range (sub1 (sqrt (length board)) ) (- (length board) (sub1 (sqrt (length board))) ) (sub1 (sqrt (length board)) ))) 'X] ;;X is winner by inverse diagonal
        [(diagonal (indexes-of board 'O) (sqrt (length board)) (range (sub1 (sqrt (length board)) ) (- (length board) (sub1 (sqrt (length board))) ) (sub1 (sqrt (length board)) ))) 'O] ;;O is winner by inverse diagonal
        [else #f];;No winner yet
        ))

(define (rowcheck indx size lst) ;; check the rows for the winner
  (cond
    [(< (length indx) size) #f] ;; if the row isn't all the same, then no winner
    [(andmap eq? lst (take indx size)) #t] ;;if the row is of the same symbol (win)
    [else (rowcheck (list-tail indx (index-where indx (lambda (x) (> x (last lst))))) size (map (lambda (y) (+ size y)) lst))]) 
  )

(define (colcheck indx size lst) ;; check the columns for the winner
  (cond
    [(> (last lst) (* size size)) #f]
    [(listcomp indx lst) #t]
    [else (colcheck indx size (map (lambda (y) (add1 y)) lst))])
  )

(define (listcomp index reqWin)
  (define (checkMatch index reqWin) ;; helper to check if the index is the same the required winning index
    (if (eq? (car index) (car reqWin))
            #t
            #f
         ))
  (define (recurseMatch index reqWin) ;; second helper to recurse and keep checking for the winning index
    (if (eq? (car index) (car reqWin))
            (listcomp (rest index) (rest reqWin))
            (listcomp (rest index) reqWin)
    ))
    (if (eq? '() (cdr index))
        (checkMatch index reqWin)
        (recurseMatch index reqWin)
     )
)

(define (diagonal indx size lst) ;;checks the index for the diagonals and matches it with winning index
  (cond
    [(< (length indx) size) #f] 
    [(listcomp indx lst) #t]
    [else #f])
  )

; Count X
(define (countX board)
  (cond
    [(empty? board) 0]
    [(equal? (car board) 'X) (add1 (countX (cdr board))) ]
    [else (countX (cdr board))]
    )
  )
  
; Count O's
(define (countO board)
  (cond
    [(empty? board) 0]
    [(equal? (car board) 'O) (add1 (countO (cdr board)))]
    [else (countO (cdr board))]
    )
  )  
;  0 1 2      0, 4, 8 && 2 4 6
;  3 4 5      left side: sqrt(len(board)) + 1
;  6 7 8      right side: sqrt(len(board)) (sqrt(len(board)) board)


;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

