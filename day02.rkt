#lang racket

(define h (hash "A" 1 "B" 2 "C" 3 "X" 1 "Y" 2 "Z" 3))

(define input
  (map (λ (e) (list (hash-ref h (car e)) (hash-ref h (cadr e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day02.txt"))))

(define (rps guide score)
  (cond
    [(empty? guide) score]
    [(or
      (= (- (second (car guide)) (first (car guide))) 1)
      (= (- (second (car guide)) (first (car guide))) -2))
     (rps (cdr guide) (apply + (list score 6 (second (car guide)))))]
    [(= (- (second (car guide)) (first (car guide))) 0) (rps (cdr guide) (apply + (list score 3 (second (car guide)))))]
    [else (rps (cdr guide) (apply + (list score 0 (second (car guide)))))]))

(rps input 0)

(define (rps2 guide score)
  (cond
    [(empty? guide) score]
    [(= (second (car guide)) 1)
     (cond
       [(= (first (car guide)) 1) (rps2 (cdr guide) (apply + (list score 0 3)))]
       [(= (first (car guide)) 2) (rps2 (cdr guide) (apply + (list score 0 1)))]
       [(= (first (car guide)) 3) (rps2 (cdr guide) (apply + (list score 0 2)))])]
    [(= (second (car guide)) 2)
     (cond
       [(= (first (car guide)) 1) (rps2 (cdr guide) (apply + (list score 3 1)))]
       [(= (first (car guide)) 2) (rps2 (cdr guide) (apply + (list score 3 2)))]
       [(= (first (car guide)) 3) (rps2 (cdr guide) (apply + (list score 3 3)))])]
    [(= (second (car guide)) 3)
     (cond
       [(= (first (car guide)) 1) (rps2 (cdr guide) (apply + (list score 6 2)))]
       [(= (first (car guide)) 2) (rps2 (cdr guide) (apply + (list score 6 3)))]
       [(= (first (car guide)) 3) (rps2 (cdr guide) (apply + (list score 6 1)))])]))

(rps2 input 0)