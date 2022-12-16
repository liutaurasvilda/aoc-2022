#lang racket

(define to-number
  (λ (e)
    (let ([pair (string-split e ",")])
      (list (string->number (cadr pair)) (string->number (car pair))))))

(define input
  (map (λ (e) (map to-number (string-split e " -> ")))
       (file->lines "input/day14_test.txt")))

(define start (λ (n1 n2) (if (< n1 n2) n1 n2)))
(define end (λ (n1 n2) (if (> n1 n2) n1 n2)))

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

(define (rocks coordinates)
  (remove-duplicates
   (foldr append '()
          (for/list ([i (in-range 0 (- (length coordinates) 1))])
            (rock (list-ref coordinates i) (list-ref coordinates (add1 i)))))))

(define (fill-cave h coordinate)
  (cond [(hash-has-key? h (first coordinate))
         (let ([nested-h (hash-ref h (first coordinate))])
           (hash-set! nested-h (second coordinate) "#")) h]
        [else
         (hash-set! h (first coordinate) (make-hash))
         (let ([nested-h (hash-ref h (first coordinate))])
           (hash-set! nested-h (second coordinate) "#")) h]))

(define (build-cave path h)
  (cond [(empty? path) h]
        [else
         (fill-cave h (car path))
         (build-cave (cdr path) h)]))

(define cave (build-cave (foldr append '() (map (λ (e) (rocks e)) input)) (make-hash)))

(define down (λ (e) (list (+ (first e) 1) (second e))))
(define down-left (λ (e) (list (+ (first e) 1) (- (second e) 1))))
(define down-right (λ (e) (list (+ (first e) 1) (+ (second e) 1))))

(define (pour-sand cave sand)
  (displayln "not implemented"))