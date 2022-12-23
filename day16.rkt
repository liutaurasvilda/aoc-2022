#lang racket

(define input
  (map (λ (e) (string-split e ","))
       (file->lines "input/day16_test.txt")))