#lang racket

(define input
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09_test.txt"))))

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
                    [(< tail-j head-j) (move-tail head (list (- tail-i 1) (+ tail-j 1)) direction)]
                    [(= tail-j head-j) (move-tail head (list (- tail-i 1) tail-j) direction)]
                    [(> tail-j head-j) (move-tail head (list (- tail-i 1) (- tail-j 1)) direction)])]
                 [(equal? direction "D")
                  (cond
                    [(< tail-j head-j) (move-tail head (list (+ tail-i 1) (+ tail-j 1)) direction)]
                    [(= tail-j head-j) (move-tail head (list (+ tail-i 1) tail-j) direction)]
                    [(> tail-j head-j) (move-tail head (list (+ tail-i 1) (- tail-j 1)) direction)])]
                 [(equal? direction "L")
                  (cond
                    [(< tail-i head-i) (move-tail head (list (+ tail-i 1) (- tail-j 1)) direction)]
                    [(= tail-i head-i) (move-tail head (list tail-i (- tail-j 1)) direction)]
                    [(> tail-i head-i) (move-tail head (list (- tail-i 1) (- tail-j 1)) direction)])]
                 [(equal? direction "R")
                  (cond
                    [(< tail-i head-i) (move-tail head (list (+ tail-i 1) (+ tail-j 1)) direction)]
                    [(= tail-i head-i) (move-tail head (list tail-i (+ tail-j 1)) direction)]
                    [(> tail-i head-i) (move-tail head (list (- tail-i 1) (+ tail-j 1)) direction)])])]
          [else tail])))

  (define (track-tail head tail-path direction)
    (let* ([head-i (first head)]
           [head-j (second head)]
           [tail (first tail-path)]
           [tail-i (first tail)]
           [tail-j (second tail)])
      (cond [(need-move? head tail)
             (cond [(equal? direction "U")
                    (cond
                      [(< tail-j head-j) (track-tail head (cons (list (- tail-i 1) (+ tail-j 1)) tail-path) direction)]
                      [(= tail-j head-j) (track-tail head (cons (list (- tail-i 1) tail-j) tail-path) direction)]
                      [(> tail-j head-j) (track-tail head (cons (list (- tail-i 1) (- tail-j 1)) tail-path) direction)])]
                   [(equal? direction "D")
                    (cond
                      [(< tail-j head-j) (track-tail head (cons (list (+ tail-i 1) (+ tail-j 1)) tail-path) direction)]
                      [(= tail-j head-j) (track-tail head (cons (list (+ tail-i 1) tail-j) tail-path) direction)]
                      [(> tail-j head-j) (track-tail head (cons (list (+ tail-i 1) (- tail-j 1)) tail-path) direction)])]
                   [(equal? direction "L")
                    (cond
                      [(< tail-i head-i) (track-tail head (cons (list (+ tail-i 1) (- tail-j 1)) tail-path) direction)]
                      [(= tail-i head-i) (track-tail head (cons (list tail-i (- tail-j 1)) tail-path) direction)]
                      [(> tail-i head-i) (track-tail head (cons (list (- tail-i 1) (- tail-j 1)) tail-path) direction)])]
                   [(equal? direction "R")
                    (cond
                      [(< tail-i head-i) (track-tail head (cons (list (+ tail-i 1) (+ tail-j 1)) tail-path) direction)]
                      [(= tail-i head-i) (track-tail head (cons (list tail-i (+ tail-j 1)) tail-path) direction)]
                      [(> tail-i head-i) (track-tail head (cons (list (- tail-i 1) (+ tail-j 1)) tail-path) direction)])])]
            [else tail-path])))

  (define (move-body head body result direction)
    '((0 0))) 

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
             (solve2 (cdr input) new-head new-body new-tail-path))]))

  (solve1 input '(0 0) '((0 0)))
  (solve2 input '(0 0) '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)) '((0 0)))
  ; test 13
  ; real 6175