#lang racket

(define input (string->list (car (file->lines "input/day06.txt"))))

(define (play input slice count)
  (cond [(not (check-duplicates (take input slice))) count]
        [else (play (cdr input) slice (add1 count))]))

(play input 4 4)   ; 1760
(play input 14 14) ; 2974