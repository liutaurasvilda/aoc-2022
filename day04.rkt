#lang racket

(define input
  (map (λ (e) (list (inclusive-range (first (car e)) (second (car e))) (inclusive-range (first (cadr e)) (second (cadr e)))))
       (map (λ (e) (list (map (λ (f) (string->number f)) (car e)) (map (λ (f) (string->number f)) (cadr e))))
            (map (λ (e) (list (string-split (car e) "-") (string-split (cadr e) "-")))
                 (map (λ (e) (string-split e ",")) (file->lines "input/day04.txt"))))))

(define (play input sum f)
  (cond [(empty? input) sum]
        [(f (car input)) (play (cdr input) (add1 sum) f)]
        [else (play (cdr input) sum f)]))

(play input 0 (λ (e) (or (= (length (set-intersect (car e) (cadr e))) (length (car e))) (= (length (set-intersect (car e) (cadr e))) (length (cadr e))))))
(play input 0 (λ (e) (not (empty? (set-intersect (car e) (cadr e))))))