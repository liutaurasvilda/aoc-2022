#lang racket

(define input
  (map (λ (e)
         (list
          (list (string->number (car e)) (string->number (cadr e)))
          (list (string->number (caddr e)) (string->number (cadddr e)))))
       (map (λ (e) (string-split e ",")) (file->lines "input/day15.txt"))))

(define (mhd p1 p2)
  (let ([x1 (first p1)]
        [y1 (second p1)]
        [x2 (first p2)]
        [y2 (second p2)])
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(define (deploy-sensors input h)
  (cond [(empty? input) h]
        [else
         (let* ([coodinates (car input)]
                [sensor (car coodinates)]
                [beacon (cadr coodinates)])
           (hash-set! h sensor (list beacon (mhd sensor beacon)))
           (deploy-sensors (cdr input) h))]))

(define sensors-map (deploy-sensors input (make-hash)))

(define (check-illegal x y sensors h)
  (cond [(empty? sensors) #f]
        [(equal? (first (hash-ref h (car sensors))) (list x y)) #f]
        [(or (< (mhd (car sensors) (list x y)) (second (hash-ref h (car sensors))))
             (= (mhd (car sensors) (list x y)) (second (hash-ref h (car sensors))))) #t]
        [else (check-illegal x y (cdr sensors) h)]))

(define (count-illegal x x-end y h count)
  (cond [(> x x-end) count]
        [else
         (let ([is-illegal (check-illegal x y (hash-keys h) h)])
           (cond [(equal? is-illegal #t) (count-illegal (add1 x) x-end y h (add1 count))]
                 [else (count-illegal (add1 x) x-end y h count)]))]))

(count-illegal -8000000 8000000 2000000 sensors-map 0)