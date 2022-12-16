#lang racket

(define to-number
  (λ (e)
    (let ([pair (string-split e ",")])
      (list (string->number (car pair)) (string->number (cadr pair))))))

(define input
  (map (λ (e) (map to-number (string-split e " -> ")))
       (file->lines "input/day14_test.txt")))