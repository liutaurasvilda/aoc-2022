#lang racket

(define input
  (map (λ (e)
         (list
          (list (string->number (car e)) (string->number (cadr e)))
          (list (string->number (caddr e)) (string->number (cadddr e)))))
       (map (λ (e) (string-split e ",")) (file->lines "input/day15_test.txt"))))

(define (mhd p1 p2)
  (let ([x1 (first p1)]
        [y1 (second p1)]
        [x2 (first p2)]
        [y2 (second p2)])
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(define (outside-mhd-of-sensors? p sensors h)
  (cond [(empty? sensors) #t]
        [else
         (let* ([sensor (car sensors)]
                [covered-area (second (hash-ref h sensor))]
                [p-mhd (mhd sensor p)])
           (cond [(or (< p-mhd covered-area) (= p-mhd covered-area)) #f]
                 [else (outside-mhd-of-sensors? p (cdr sensors) h)]))]))

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

;(count-illegal -6000000 6000000 2000000 sensors-map 0)

(define (get-mhd-corners p mhd)
  (let* ([up (list (first p) (- (second p) mhd))]
         [down (list (first p) (+ (second p) mhd))]
         [left (list (- (first p) mhd) (second p))]
         [right (list (+ (first p) mhd) (second p))])
    (list up down left right)))

(define (find-sensors-corners sensors sensors-map result)
  (cond [(empty? sensors) result]
        [else
         (let* ([sensor (car sensors)]
                [mhd (second (hash-ref sensors-map sensor))]
                [corners (get-mhd-corners sensor mhd)])
           (find-sensors-corners (cdr sensors) sensors-map (append result corners)))]))

(define location->up (λ (e) (list (first e) (- (second e) 1))))
(define location->up-left (λ (e) (list (- (first e) 1) (- (second e) 1))))
(define location->up-right (λ (e) (list (+ (first e) 1) (- (second e) 1))))
(define location->down (λ (e) (list (first e) (+ (second e) 1))))
(define location->down-left (λ (e) (list (- (first e) 1) (+ (second e) 1))))
(define location->down-right (λ (e) (list (+ (first e) 1) (+ (second e) 1))))
(define location->left (λ (e) (list (- (first e) 1) (second e))))
(define location->right (λ (e) (list (+ (first e) 1) (second e))))

(define (neighbours p)
  (list
   (location->up p)
   (location->up-left p)
   (location->up-right p)
   (location->down p)
   (location->down-left p)
   (location->down-right p)
   (location->left p)
   (location->right p)))

(define sensors-corners-neighbours
  (foldr append '()
         (map (λ (e) (neighbours e))
              (find-sensors-corners (hash-keys sensors-map) sensors-map '()))))

(define legal-range 20)
;(define legal-range 4000000)

(define x-y-within-range-f
  (λ (e)
    (and
     (and (or (> (first e) 0) (= (first e) 0))
          (or (< (first e) legal-range) (= (first e) legal-range)))
     (and (or (> (second e) 0) (= (second e) 0))
          (or (< (second e) legal-range) (= (second e) legal-range))))))

(define outside-sensors-f
  (λ (e) (outside-mhd-of-sensors? e (hash-keys sensors-map) sensors-map)))

(define potential-distress (filter x-y-within-range-f sensors-corners-neighbours))

(define distress (first (filter outside-sensors-f potential-distress)))

(+ (* (first distress) 4000000) (second distress))