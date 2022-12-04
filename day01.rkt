#lang racket

(define input (map (λ (e) (string->number e)) (file->lines "input/day01.txt")))

(define (calories-calc calories sum calories-bag)
  (cond [(empty? calories) (sort (cons sum calories-bag) >)]
        [(not (car calories)) (calories-calc (cdr calories) 0 (cons sum calories-bag))]
        [else (calories-calc (cdr calories) (+ sum (car calories)) calories-bag)]))

(car (calories-calc input 0 '()))
(apply + (take (calories-calc input 0 '()) 3))