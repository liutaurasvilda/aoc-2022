#lang racket

(define to-number
  (λ (e)
    (let ([pair (string-split e ",")])
      (list (string->number (cadr pair)) (string->number (car pair))))))

(define input
  (map (λ (e) (map to-number (string-split e " -> ")))
       (file->lines "input/day14.txt")))

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
         (let ([inner-h (hash-ref h (first coordinate))])
           (hash-set! inner-h (second coordinate) "#")) h]
        [else
         (hash-set! h (first coordinate) (make-hash))
         (let ([inner-h (hash-ref h (first coordinate))])
           (hash-set! inner-h (second coordinate) "#")) h]))

(define (build-cave path h)
  (cond [(empty? path) h]
        [else
         (fill-cave h (car path))
         (build-cave (cdr path) h)]))

(define coordinates (foldr append '() (map (λ (e) (rocks e)) input)))

(define cave (build-cave coordinates (make-hash)))

(define down (λ (e) (list (+ (first e) 1) (second e))))
(define down-left (λ (e) (list (+ (first e) 1) (- (second e) 1))))
(define down-right (λ (e) (list (+ (first e) 1) (+ (second e) 1))))

(define (can-fall? sand h)
  (let ([have-row (hash-has-key? h (first sand))])
    (or (not have-row) (not (hash-has-key? (hash-ref h (first sand)) (second sand))))))

(define (rest sand h)
  (cond [(hash-has-key? h (first sand))
         (let ([inner-h (hash-ref h (first sand))])
           (hash-set! inner-h (second sand) "o")) #t]
        [else
         (hash-set! h (first sand) (make-hash))
         (rest sand h)]))

(define (pour-sand sand h abyss)
  (cond [(> (first sand) abyss) h]
        [(can-fall? (down sand) h) (pour-sand (down sand) h abyss)]
        [(can-fall? (down-left sand) h) (pour-sand (down-left sand) h abyss)]
        [(can-fall? (down-right sand) h) (pour-sand (down-right sand) h abyss)]
        [(rest sand h) (pour-sand '(0 500) h abyss)]))

(define (sum-resting h)
  (length (filter (λ (e) (equal? e "o"))
                  (flatten (map (λ (e) (hash-values e)) (hash-values h))))))

(define (by-x x1 x2)
  (< (car x1) (car x2)))

(define abyss (first (last (sort coordinates by-x))))

(sum-resting (pour-sand '(0 500) cave abyss))