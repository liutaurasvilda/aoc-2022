#lang racket

(define input
  (map (λ (e) (list (inclusive-range (first (car e)) (second (car e))) (inclusive-range (first (cadr e)) (second (cadr e)))))
       (map (λ (e) (list (map (λ (f) (string->number f)) (car e)) (map (λ (f) (string->number f)) (cadr e))))
            (map (λ (e) (list (string-split (car e) "-") (string-split (cadr e) "-")))
                 (map (λ (e) (string-split e ",")) (file->lines "input/day04.txt"))))))

(define (play input sum)
  (cond [(empty? input) sum]
        [(or (= (length (set-intersect (car (car input)) (cadr (car input)))) (length (car (car input))))
             (= (length (set-intersect (car (car input)) (cadr (car input))))(length (cadr (car input)))))
         (play (cdr input) (add1 sum))]
        [else (play (cdr input) sum)]))

(define (play2 input sum)
  (cond [(empty? input) sum]
        [(not (empty? (set-intersect (car (car input)) (cadr (car input))))) (play2 (cdr input) (add1 sum))]
        [else (play2 (cdr input) sum)]))

(play input 0)
(play2 input 0)