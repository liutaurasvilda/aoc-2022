#lang racket
(require rackunit)

(define input
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09.txt"))))

(define test-input
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09_test.txt"))))

(define test-input-2
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09_test2.txt"))))

(define (need-move? head tail)
  (or (> (abs (- (first head) (first tail))) 1)
      (> (abs (- (second head) (second tail))) 1)))

(define (move-tail head tail direction)
  (let* ([head-i (first head)]
         [head-j (second head)]
         [tail-i (first tail)]
         [tail-j (second tail)])
    (cond [(need-move? head tail)
           (cond [(equal? direction "U")
                  (cond
                    [(< tail-j head-j) (list (- tail-i 1) (+ tail-j 1))]
                    [(= tail-j head-j) (list (- tail-i 1) tail-j)]
                    [(> tail-j head-j) (list (- tail-i 1) (- tail-j 1))])]
                 [(equal? direction "D")
                  (cond
                    [(< tail-j head-j) (list (+ tail-i 1) (+ tail-j 1))]
                    [(= tail-j head-j) (list (+ tail-i 1) tail-j)]
                    [(> tail-j head-j) (list (+ tail-i 1) (- tail-j 1))])]
                 [(equal? direction "L")
                  (cond
                    [(< tail-i head-i) (list (+ tail-i 1) (- tail-j 1))]
                    [(= tail-i head-i) (list tail-i (- tail-j 1))]
                    [(> tail-i head-i) (list (- tail-i 1) (- tail-j 1))])]
                 [(equal? direction "R")
                  (cond
                    [(< tail-i head-i) (list (+ tail-i 1) (+ tail-j 1))]
                    [(= tail-i head-i) (list tail-i (+ tail-j 1))]
                    [(> tail-i head-i) (list (- tail-i 1) (+ tail-j 1))])])]
          [else tail])))

(define (track-tail head tail-path direction)
  (let* ([head-i (first head)]
         [head-j (second head)]
         [tail (first tail-path)]
         [tail-i (first tail)]
         [tail-j (second tail)])
    (cond [(need-move? head tail) (track-tail head (cons (move-tail head tail direction) tail-path) direction)]
          [else tail-path])))

(define (move-body head body new-body direction)
  (cond [(empty? body) (reverse new-body)]
        [else
         (let* ([knot (first (track-tail head body direction))]
                [body-result (cons knot new-body)])
           (move-body knot (cdr body) body-result direction))]))

(define (move-head head direction distance)
  (cond [(> distance 0)
         (let* ([i (first head)]
                [j (second head)])
           (cond [(equal? direction "U") (move-head (list (- i 1) j) direction (- distance 1))]
                 [(equal? direction "D") (move-head (list (+ i 1) j) direction (- distance 1))]
                 [(equal? direction "L") (move-head (list i (- j 1)) direction (- distance 1))]
                 [(equal? direction "R") (move-head (list i (+ j 1)) direction (- distance 1))]))]
        [else head]))

(define (solve1 input head tail-path)
  (cond [(empty? input) (length (remove-duplicates tail-path))]
        [else
         (let* ([new-head (move-head head (first (car input)) (second (car input)))]
                [new-tail-path (track-tail new-head tail-path (first (car input)))])
           (solve1 (cdr input) new-head new-tail-path))]))

(define (solve2 input head body tail-path)
  (cond [(empty? input) (length (remove-duplicates tail-path))]
        [else
         (let* ([new-head (move-head head (first (car input)) (second (car input)))]
                [new-body (move-body new-head body '() (first (car input)))]
                [new-tail-path (track-tail (last new-body) tail-path (first (car input)))])
           (display "instruction ") (displayln (car input))
           (display "head ") (displayln new-head)
           (display "body ") (displayln new-body)
           (display "tail ") (displayln (first new-tail-path))
           (displayln "")
           (solve2 (cdr input) new-head new-body new-tail-path))]))


; unit tests
(check-equal? (solve1 test-input '(0 0) '((0 0))) 13 "part1 input/day09_test.txt")
(check-equal? (solve1 input '(0 0) '((0 0))) 6175 "part1 input/day09.txt")
(check-equal? (solve2 test-input '(0 0) '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)) '((0 0))) 1 "part2 input/day09_test.txt")
;(check-equal? (solve2 test-input-2 '(0 0) '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)) '((0 0))) 36 "part2 input/day09_test2.txt")