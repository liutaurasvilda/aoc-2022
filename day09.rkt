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

(define tracker (mutable-set))

(define (move-head head direction)
  (let* ([i (first head)]
         [j (second head)])
    (cond [(equal? direction "U") (list (- i 1) j)]
          [(equal? direction "D") (list (+ i 1) j)]
          [(equal? direction "L") (list i (- j 1))]
          [(equal? direction "R") (list i (+ j 1))])))

(define (need-move? head tail)
  (or (> (abs (- (first head) (first tail))) 1)
      (> (abs (- (second head) (second tail))) 1)))

(define (move-knot head tail direction)
  (let* ([head-i (first head)]
         [head-j (second head)]
         [tail-i (first tail)]
         [tail-j (second tail)])
    (cond [(need-move? head tail)
           (cond [(equal? direction "U")
                  (cond
                    [(= tail-j head-j) (list (- tail-i 1) tail-j)]
                    
                    [(and (< tail-j head-j) (= tail-i head-i)) (list tail-i (+ tail-j 1))]
                    [(and (< tail-j head-j) (> tail-i head-i)) (list (- tail-i 1) (+ tail-j 1))]
                    [(and (< tail-j head-j) (< tail-i head-i)) (list (+ tail-i 1) (+ tail-j 1))]

                    [(and (> tail-j head-j) (= tail-i head-i)) (list tail-i (- tail-j 1))]
                    [(and (> tail-j head-j) (> tail-i head-i)) (list (- tail-i 1) (- tail-j 1))]
                    [(and (> tail-j head-j) (< tail-i head-i)) (list (+ tail-i 1) (- tail-j 1))])]
                 [(equal? direction "D")
                  (cond
                    [(= tail-j head-j) (list (+ tail-i 1) tail-j)]
                    
                    [(and (< tail-j head-j) (= tail-i head-i)) (list tail-i (+ tail-j 1))]
                    [(and (< tail-j head-j) (> tail-i head-i)) (list (- tail-i 1) (+ tail-j 1))]
                    [(and (< tail-j head-j) (< tail-i head-i)) (list (+ tail-i 1) (+ tail-j 1))]

                    [(and (> tail-j head-j) (= tail-i head-i)) (list tail-i (- tail-j 1))]
                    [(and (> tail-j head-j) (> tail-i head-i)) (list (- tail-i 1) (- tail-j 1))]
                    [(and (> tail-j head-j) (< tail-i head-i)) (list (+ tail-i 1) (- tail-j 1))])]
                 [(equal? direction "L")
                  (cond
                    [(= tail-i head-i) (list tail-i (- tail-j 1))]

                    [(and (< tail-i head-i) (= tail-j head-j)) (list (+ tail-i 1) tail-j)]
                    [(and (< tail-i head-i) (> tail-j head-j)) (list (+ tail-i 1) (- tail-j 1))]
                    [(and (< tail-i head-i) (< tail-j head-j)) (list (+ tail-i 1) (+ tail-j 1))]
                    
                    [(and (> tail-i head-i) (= tail-j head-j)) (list (- tail-i 1) tail-j)]
                    [(and (> tail-i head-i) (> tail-j head-j)) (list (- tail-i 1) (- tail-j 1))]
                    [(and (> tail-i head-i) (< tail-j head-j)) (list (- tail-i 1) (+ tail-j 1))])]
                 [(equal? direction "R")
                  (cond
                    [(= tail-i head-i) (list tail-i (+ tail-j 1))]
                    
                    [(and (< tail-i head-i) (= tail-j head-j)) (list (+ tail-i 1) tail-j)]
                    [(and (< tail-i head-i) (> tail-j head-j)) (list (+ tail-i 1) (- tail-j 1))]
                    [(and (< tail-i head-i) (< tail-j head-j)) (list (+ tail-i 1) (+ tail-j 1))]
                    
                    [(and (> tail-i head-i) (= tail-j head-j)) (list (- tail-i 1) tail-j)]
                    [(and (> tail-i head-i) (> tail-j head-j)) (list (- tail-i 1) (- tail-j 1))]
                    [(and (> tail-i head-i) (< tail-j head-j)) (list (- tail-i 1) (+ tail-j 1))])])]
          [else tail])))

(define (move-knots rope direction distance)
  (let* ([head (first rope)]
         [first (second rope)]
         [second (third rope)]
         [third (fourth rope)]
         [fourth (fifth rope)]
         [fifth (sixth rope)]
         [sixth (seventh rope)]
         [seventh (eighth rope)]
         [eighth (ninth rope)]
         [ninth (tenth rope)]
         [new-head (if (> distance 0) (move-head head direction) head)]
         [new-first (move-knot new-head first direction)]
         [new-second (move-knot new-first second direction)]
         [new-third (move-knot new-second third direction)]
         [new-fourth (move-knot new-third fourth direction)]
         [new-fifth (move-knot new-fourth fifth direction)]
         [new-sixth (move-knot new-fifth sixth direction)]
         [new-seventh (move-knot new-sixth seventh direction)]
         [new-eighth (move-knot new-seventh eighth direction)]
         [new-ninth (move-knot new-eighth ninth direction)]
         [new-rope (list new-head new-first new-second new-third new-fourth new-fifth new-sixth new-seventh new-eighth new-ninth)])
    new-rope))

(define (move-rope rope direction distance)
  (cond [(and (< distance 1)
          (not (member #t (for/list ([i (in-range 0 (- (length rope) 1))])
                            (need-move? (list-ref rope i) (list-ref rope (add1 i))))))) rope]
        [else
         (let ([new-rope (move-knots rope direction distance)])
           (set-add! tracker (last new-rope))
           (move-rope new-rope direction (- distance 1)))]))

(define (solve2 input rope)
  (cond [(empty? input) (set-count tracker)]
        [else
         (let ([new-rope (move-rope rope (first (car input)) (second (car input)))])
           (solve2 (cdr input) new-rope))]))

;(solve2 input '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))