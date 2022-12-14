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

(define visited (mutable-set))

(define (element-at location)
  (let ([row (first location)]
        [column (second location)])
    (cond [(and (> row -1) (< row (length input)) (> column -1) (< column (length (list-ref input 0))))
           (list-ref (list-ref input row) column)]
          [else #f])))

(define (visitable? source-location target-location)
  (let* ([source-element (element-at source-location)]
         [target-element (element-at target-location)])
    (cond [(or (equal? #f target-element) (set-member? visited target-element)) #f]
          [(or (equal? target-element 0) (= (add1 source-element) target-element)) #t])))

(define (visitable-neighbours-of location)
  (filter (λ (e) (visitable? location e))
          (list (location->up location) (location->down location)
                (location->left location) (location->right location))))

(define (solve start end)
  (displayln "not implemented"))

(define start '(0 0))
(define goal '(2 5))