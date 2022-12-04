#lang racket

(define input (map (λ (e) (string->list e)) (file->lines "input/day03.txt")))

(define priority (λ (e) (- (char->integer e) (if (char-lower-case? e) 96 38))))

(apply + (map priority (map (λ (e) (car (set-intersect (car e) (cadr e)))) (map (λ (e) (list (take e (/ (length e) 2)) (drop e (/ (length e) 2)))) input))))

(define (intersect-slice l result)
  (cond [(empty? l) result]
        [(> (length l) 3) (intersect-slice (cdddr l) (append (set-intersect (car l) (cadr l) (caddr l)) result))]
        [else (intersect-slice '() (append (set-intersect (car l) (cadr l) (caddr l)) result))]))
                    
(apply + (map priority (intersect-slice input '())))