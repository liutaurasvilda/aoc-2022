#lang racket
(require data/queue)
(require dyoo-while-loop)

(define to-number
  (λ (e)
    (if
     (char-upper-case? e)
     (if (equal? e #\S) 1 26)
     (- (char->integer e) 96))))

(define input
  (map (λ (e) (map to-number (string->list e)))
       (file->lines "input/day12_test.txt")))

(define location->up (λ (e) (list (- (first e) 1) (second e))))
(define location->down (λ (e) (list (+ (first e) 1) (second e))))
(define location->left (λ (e) (list (first e) (- (second e) 1))))
(define location->right (λ (e) (list (first e) (+ (second e) 1))))

(define visited (mutable-set))

(define start '(0 0))
(define target '(2 5))

(define (element-at location)
  (let ([row (first location)]
        [column (second location)])
    (cond [(and (> row -1) (< row (length input)) (> column -1) (< column (length (list-ref input 0))))
           (list-ref (list-ref input row) column)]
          [else #f])))

(define (visitable? source-location target-location)
  (let* ([source-element (element-at source-location)]
         [target-element (element-at target-location)])
    (cond [(equal? #f target-element) #f]
          [(set-member? visited target-location) #f]
          [(= source-element target-element) #t]
          [(= (add1 source-element) target-element) #t])))

(define (visitable-neighbours-of location)
  (filter (λ (e) (visitable? location e))
          (list (location->up location) (location->down location)
                (location->left location) (location->right location))))

(define (solve target q steps)
  (cond [(non-empty-queue? q)
         (let* ([location (dequeue! q)]
                [neighbours (visitable-neighbours-of location)])
           (display "location=")(display location)(display "neighbors=")(displayln neighbours)
           (cond [(member target neighbours) steps]
                 [else
                  (for ([i neighbours])
                    (enqueue! q i)
                    (set-add! visited i))
                  (solve target q (add1 steps))]))]
        [else 0]))

(define q (make-queue))
(enqueue! q start)
(set-add! visited start)

(solve target q 1)