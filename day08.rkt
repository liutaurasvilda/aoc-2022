#lang racket

(define input (map (λ (e) (map (λ (f) (string->number f)) (map string (string->list e))))
                   (file->lines "input/day08_test.txt")))

(define (solve input)
  (for* ([i (in-range (length input))]
         [j (in-range (length (car input)))])
    (let ([tree (list-ref (list-ref input i) j)])
          (displayln tree))))

(solve input)