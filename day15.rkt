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

(define (outside-mhd-of-sensors? coordinate sensors h)
  (cond [(empty? sensors) #t]
        [else
         (let* ([sensor (car sensors)]
               [covered-area (second (hash-ref h sensor))]
               [coordinate-mhd (mhd sensor coordinate)])
           (cond [(or (< coordinate-mhd covered-area) (= coordinate-mhd covered-area)) #f]
                 [else (outside-mhd-of-sensors? coordinate (cdr sensors) h)]))]))

(define (deploy-sensors input h)
  (cond [(empty? input) h]
        [else
         (let* ([coodinates (car input)]
                [sensor (car coodinates)]
                [beacon (cadr coodinates)])
           (hash-set! h sensor (list beacon (mhd sensor beacon)))
           (deploy-sensors (cdr input) h))]))

(define sensors-map (deploy-sensors input (make-hash)))

(define (is-illegal? x y sensors h)
  (cond [(empty? sensors) #f]
        [(equal? (first (hash-ref h (car sensors))) (list x y)) #f]
        [(or (< (mhd (car sensors) (list x y)) (second (hash-ref h (car sensors))))
             (= (mhd (car sensors) (list x y)) (second (hash-ref h (car sensors))))) #t]
        [else (is-illegal? x y (cdr sensors) h)]))

(define (count-illegal x x-end y h count)
  (cond [(> x x-end) count]
        [(is-illegal? x y (hash-keys h) h) (count-illegal (add1 x) x-end y h (add1 count))]
        [else (count-illegal (add1 x) x-end y h count)]))

(define (find-distress sensors h range)
  (for* ([x (in-inclusive-range 0 range)]
         [y (in-inclusive-range 0 range)])
        (when (outside-mhd-of-sensors? (list x y) sensors h)
      (displayln (list x y)))))

;(count-illegal -6000000 6000000 2000000 sensors-map 0)
;(find-distress (hash-keys sensors-map) sensors-map 4000000)