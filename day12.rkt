#lang racket

(define to-number
  (λ (e) (if (char-upper-case? e) 0 (- (char->integer e) 96))))

(define input
  (map (λ (e) (map to-number (string->list e)))
       (file->lines "input/day12_test.txt")))

(define (solve heightmap location visited paths)
  (displayln "not implemented"))