#lang racket

(define to-number
  (λ (e) (if (char-upper-case? e) 0 (- (char->integer e) 96))))

(define heightmap
  (map (λ (e) (map to-number (string->list e)))
       (file->lines "input/day12_test.txt")))

(define up (λ (e) (list (- (first e) 1) (second e))))
(define down (λ (e) (list (+ (first e) 1) (second e))))
(define left (λ (e) (list (first e) (- (second e) 1))))
(define right (λ (e) (list (first e) (+ (second e) 1))))

(define (element-at location)
  (let ([row (first location)]
        [column (second location)])
    (cond [(and (row > -1) (< row (length heightmap)) (> column -1) (< column (length (list-ref heightmap 0))))
           (list-ref (list-ref heightmap row) column)]
          [else #f])))

(define (allowed-from? location direction)
  (displayln "not implemented"))

(define (solve location visited paths)
  (displayln "not implemented"))

(define start '(0 0))
(define goal '(2 5))