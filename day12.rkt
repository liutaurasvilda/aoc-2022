#lang racket
(require data/queue)
(require dyoo-while-loop)

(define to-number
  (λ (e) (if (char-upper-case? e)
             (if (equal? e #\S) 1 26)
             (- (char->integer e) 96))))

(define input
  (map (λ (e) (map to-number (string->list e)))
       (file->lines "input/day12.txt")))

(define location->up (λ (e) (list (- (first e) 1) (second e))))
(define location->down (λ (e) (list (+ (first e) 1) (second e))))
(define location->left (λ (e) (list (first e) (- (second e) 1))))
(define location->right (λ (e) (list (first e) (+ (second e) 1))))

(struct node (location level) #:transparent)

(define (element-at location)
  (let ([row (first location)]
        [column (second location)])
    (cond [(and (> row -1) (< row (length input)) (> column -1) (< column (length (list-ref input 0))))
           (list-ref (list-ref input row) column)]
          [else #f])))

(define (visitable? source-location target-location visited)
  (let* ([source-element (element-at source-location)]
         [target-element (element-at target-location)])
    (cond [(or (equal? #f target-element) (set-member? visited target-location)) #f]
          [(< (add1 source-element) target-element) #f]
          [else #t])))

(define (visitable-neighbours-of location visited)
  (filter (λ (e) (visitable? location e visited))
          (list (location->up location) (location->down location)
                (location->left location) (location->right location))))

(define (bfs target queue visited)
  (cond [(non-empty-queue? queue)
         (let* ([current (dequeue! queue)]
                [location (node-location current)]
                [neighbours (visitable-neighbours-of location visited)])
           (cond [(member target neighbours) (add1 (node-level current))]
                 [else
                  (for ([i neighbours])
                    (enqueue! queue (node i (add1 (node-level current))))
                    (set-add! visited i))
                  (bfs target queue visited)]))]
        [else -1]))

(define (solve start target)
  (let ([queue (make-queue)]
        [visited (mutable-set)]
        [node (node start 0)])
    (enqueue! queue node)
    (set-add! visited node)
    (bfs target queue visited)))

(solve '(20 0) '(20 68))