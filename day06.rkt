#lang racket

(define input (string->list (car (file->lines "input/day06.txt"))))

(define (solve input slice count)
  (if (not (check-duplicates (take input slice))) (+ slice count)
      (solve (cdr input) slice (add1 count))))

(solve input 4 0)
(solve input 14 0)