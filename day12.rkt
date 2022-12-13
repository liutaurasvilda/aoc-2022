#lang racket

(define input
  (map (λ (e) (string->list e))
       (file->lines "input/day12_test.txt")))

(define (solve heightmap location visited paths)
  (displayln "not implemented"))