#lang racket

(define input
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09_test.txt"))))

(define TODO '((1 1)))

(define (need-move? head tail)
  (or (> (abs (- (first head) (first tail))) 1)
      (> (abs (- (second head) (second tail))) 1)))

(define (move-head head-path direction distance)
  (cond [(equal? direction "U") TODO]
        [(equal? direction "D") TODO]
        [(equal? direction "L") TODO]
        [(equal? direction "R") TODO]))

(define (move-tail head-path tail-path)
  (cond [(need-move? (first head-path) (first tail-path)) TODO]
        [else tail-path]))

(define (solve input head-path tail-path)
  (cond [(empty? input) (length (remove-duplicates tail-path))]
        [else
         (let* ([new-head-path (move-head head-path (first (car input)) (second (car input)))]
                [new-tail-path (move-tail new-head-path tail-path)])
           (solve (cdr input) new-head-path new-tail-path))]))

(solve input '((0 0)) '((0 0)))