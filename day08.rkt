#lang racket

(define input (map (λ (e) (map (λ (f) (string->number f)) (map string (string->list e))))
                   (file->lines "input/day08_test.txt")))

(define ht (make-hash))
(hash-set! ht "sum" 0)

(define (update ht)
  (hash-set! ht "sum" (add1 (hash-ref ht "sum"))))

(define (solve input)
  (for* ([i (in-range (length input))]
         [j (in-range (length (car input)))])
           (let ([tree (list-ref (list-ref input i) j)])
             (update ht))))

(solve input)