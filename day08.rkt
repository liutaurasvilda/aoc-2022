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
      (if
       (or
        (= i 0) (= j 0) (= i (- (length input) 1)) (= j (- (length (car input)) 1))
        (or
         (< (list-ref (list-ref input (- i 1)) j) tree)
         (< (list-ref (list-ref input (+ i 1)) j) tree)
         (< (list-ref (list-ref input i) (- j 1)) tree)
         (< (list-ref (list-ref input i) (+ j 1)) tree)
         )
        )
       (update ht)
       (display "")
       ))))

(solve input)