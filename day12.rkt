#lang racket

(define to-number
  (λ (e) (if (char-upper-case? e) 0 (- (char->integer e) 96))))

(define input
  (map (λ (e) (map to-number (string->list e)))
       (file->lines "input/day12_test.txt")))

(define location->up (λ (e) (list (- (first e) 1) (second e))))
(define location->down (λ (e) (list (+ (first e) 1) (second e))))
(define location->left (λ (e) (list (first e) (- (second e) 1))))
(define location->right (λ (e) (list (first e) (+ (second e) 1))))

(define (element-at location)
  (let ([row (first location)]
        [column (second location)])
    (cond [(and (> row -1) (< row (length input)) (> column -1) (< column (length (list-ref input 0))))
           (list-ref (list-ref input row) column)]
          [else #f])))

(define (allowed-from? location direction visited)
  (let* ([next-location (direction location)]
        [current-element (element-at location)]
        [next-element (element-at next-location)])
    (cond [(set-member? visited next-location) #f]
          [(equal? #f next-element) #f]
          [(equal? next-element 0) #t]
          [else (= (add1 current-element) next-element)])))

(define (neighbours-of location)
  (list (location->up location) (location->down location)
        (location->left location) (location->right location)))

(define (solve location visited)
  (displayln "not implemented"))

(define start '(0 0))
(define goal '(2 5))