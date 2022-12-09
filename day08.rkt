#lang racket

(define input (map (λ (e) (map (λ (f) (string->number f)) (map string (string->list e))))
                   (file->lines "input/day08.txt")))

(define ht (make-hash))
(hash-set! ht "sum" 0)

(define (update ht)
  (hash-set! ht "sum" (add1 (hash-ref ht "sum"))))

(define (left input i j)
  (take (list-ref input i) j))

(define (right input i j)
  (drop (list-ref input i) (add1 j)))

(define (up input i j)
  (map (λ (e) (list-ref e j)) (take input i)))

(define (down input i j)
  (map (λ (e) (list-ref e j)) (drop input (add1 i))))

(define (visible input tree i j)
  (cond [(or
          (andmap (λ (e) (< e tree)) (left input i j))
          (andmap (λ (e) (< e tree)) (right input i j))
          (andmap (λ (e) (< e tree)) (up input i j))
          (andmap (λ (e) (< e tree)) (down input i j)))
         #t]
        [else #f]))

(define (solve input)
  (for* ([i (in-range (length input))]
         [j (in-range (length (car input)))])
    (let ([tree (list-ref (list-ref input i) j)])
      (cond [(or
              (= i 0) (= j 0) (= i (- (length input) 1)) (= j (- (length (car input)) 1))
              (visible input tree i j))
             (update ht)])))
  (hash-ref ht "sum"))

(solve input)
