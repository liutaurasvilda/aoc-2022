#lang racket

(define input
  (map (λ (e) (list (car e) (string->number (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day09_test.txt"))))

(define (need-move? head tail)
  (or (> (abs (- (first head) (first tail))) 1)
      (> (abs (- (second head) (second tail))) 1)))

(define (move-tail head-path tail-path direction)
  (let* ([head (first head-path)]
        [head-i (first head)]
        [head-j (second head)]
        [tail (first tail-path)]
        [tail-i (first tail)]
        [tail-j (second tail)])
    (cond [(need-move? head tail)
           (cond [(equal? direction "U")
                  (cond
                    [(< (tail-j head-j)) (move-tail head-path (cons (list (- tail-i 1) (+ tail-j 1)) tail-path) direction)]
                    [(= (tail-j head-j)) (move-tail head-path (cons (list (- tail-i 1) tail-j) tail-path) direction)]
                    [(> (tail-j head-j)) (move-tail head-path (cons (list (- tail-i 1) (- tail-j 1)) tail-path) direction)])]
                 [(equal? direction "D")
                  (cond
                    [(< (tail-j head-j)) (move-tail head-path (cons (list (+ tail-i 1) (+ tail-j 1)) tail-path) direction)]
                    [(= (tail-j head-j)) (move-tail head-path (cons (list (+ tail-i 1) tail-j) tail-path) direction)]
                    [(> (tail-j head-j)) (move-tail head-path (cons (list (+ tail-i 1) (- tail-j 1)) tail-path) direction)])]
                 [(equal? direction "L")
                  (cond
                    [(< (tail-i head-i)) (move-tail head-path (cons (list (+ tail-i 1) (- tail-j 1)) tail-path) direction)]
                    [(= (tail-i head-i)) (move-tail head-path (cons (list tail-i (- tail-j 1)) tail-path) direction)]
                    [(> (tail-i head-i)) (move-tail head-path (cons (list (- tail-i 1) (- tail-j 1)) tail-path) direction)])
                 [(equal? direction "R")
                  (cond
                    [(< (tail-i head-i)) (move-tail head-path (cons (list (+ tail-i 1) (+ tail-j 1)) tail-path) direction)]
                    [(= (tail-i head-i)) (move-tail head-path (cons (list tail-i (+ tail-j 1)) tail-path) direction)]
                    [(> (tail-i head-i)) (move-tail head-path (cons (list (- tail-i 1) (+ tail-j 1)) tail-path) direction)])]])]
          [else tail-path])))

  (define (move-head head-path direction distance)
    (cond [(> distance 0)
           (let* ([head (car head-path)]
                  [i (first head)]
                  [j (second head)])
             (cond [(equal? direction "U") (move-head (cons (list (- i 1) j) head-path) direction (- distance 1))]
                   [(equal? direction "D") (move-head (cons (list (+ i 1) j) head-path) direction (- distance 1))]
                   [(equal? direction "L") (move-head (cons (list i (- j 1)) head-path) direction (- distance 1))]
                   [(equal? direction "R") (move-head (cons (list i (+ j 1)) head-path) direction (- distance 1))]))]
          [else head-path]))

  (define (solve input head-path tail-path)
    (cond [(empty? input) (length (remove-duplicates tail-path))]
          [else
           (let* ([new-head-path (move-head head-path (first (car input)) (second (car input)))]
                  [new-tail-path (move-tail new-head-path tail-path (first (car input)))])
             (solve (cdr input) new-head-path new-tail-path))]))

  (solve input '((0 0)) '((0 0)))