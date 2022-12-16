#lang racket

(define to-number
  (λ (e)
    (let ([pair (string-split e ",")])
      (list (string->number (cadr pair)) (string->number (car pair))))))

(define input
  (map (λ (e) (map to-number (string-split e " -> ")))
       (file->lines "input/day14_test.txt")))

(define (start n1 n2)
  (if (< n1 n2) n1 n2))

(define (end n1 n2)
  (if (> n1 n2) n1 n2))

(define (rock p1 p2)
  (let ([i1 (first p1)]
        [j1 (second p1)]
        [i2 (first p2)]
        [j2 (second p2)])
    (cond [(= i1 i2)
           (for/list ([x (in-inclusive-range (start j1 j2) (end j1 j2))])
             (list i1 x))]
          [(= j1 j2)
           (for/list ([x (in-inclusive-range (start i1 i2) (end i1 i2))])
             (list x j1))])))

(define (path rocks)
  (foldr append '() (for/list ([i (in-range 0 (- (length rocks) 1))])
    (rock (list-ref rocks i) (list-ref rocks (add1 i))))))