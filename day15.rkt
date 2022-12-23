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

; PART 1
;(count-illegal -6000000 6000000 2000000 sensors-map 0)

(define (get-mhd-outside-corners p mhd)
  (let* ([up (list (first p) (- (- (second p) mhd) 1))]
         [down (list (first p) (+ (+ (second p) mhd) 1))]
         [left (list (- (- (first p) mhd) 1) (second p))]
         [right (list (+ (+ (first p) mhd) 1) (second p))])
    (list left up right down)))

(define (find-sensors-outside-corners sensors sensors-map result)
  (cond [(empty? sensors) result]
        [else
         (let* ([sensor (car sensors)]
                [mhd (second (hash-ref sensors-map sensor))]
                [corners (get-mhd-outside-corners sensor mhd)])
           (find-sensors-outside-corners (cdr sensors) sensors-map (append result (list corners))))]))

(define legal-range 4000000)

(define x-y-within-range-f
  (λ (e)
    (and
     (and (or (> (first e) 0) (= (first e) 0))
          (or (< (first e) legal-range) (= (first e) legal-range)))
     (and (or (> (second e) 0) (= (second e) 0))
          (or (< (second e) legal-range) (= (second e) legal-range))))))

(define outside-sensors-f
  (λ (e) (outside-mhd-of-sensors? e (hash-keys sensors-map) sensors-map)))

(define (left-to-up left up)
  (for/list ([i (in-inclusive-range 0 (abs (- (first left) (first up))))])
    (list (+ (first left) i) (- (second left) i))))

(define (up-to-right up right)
  (for/list ([i (in-inclusive-range 0 (abs (- (first up) (first right))))])
    (list (+ (first up) i) (+ (second up) i))))

(define (right-to-down right down)
  (for/list ([i (in-inclusive-range 0 (abs (- (first right) (first down))))])
    (list (- (first right) i) (+ (second right) i))))

(define (down-to-left down left)
  (for/list ([i (in-inclusive-range 0 (abs (- (first down) (first left))))])
    (list (- (first down) i) (- (second down) i))))

(define (side-coordinates all-corners result)
  (cond [(empty? all-corners) result]
        [else
         (let* ([corners (car all-corners)]
                [sides
                 (append
                  (filter outside-sensors-f (filter x-y-within-range-f (left-to-up (first corners) (second corners))))
                  (filter outside-sensors-f (filter x-y-within-range-f (up-to-right (second corners) (third corners))))
                  (filter outside-sensors-f (filter x-y-within-range-f (right-to-down (third corners) (fourth corners))))
                  (filter outside-sensors-f (filter x-y-within-range-f (down-to-left (fourth corners) (first corners)))))])
           (side-coordinates (cdr all-corners) (append result sides)))]))

; PART 2
(define reduced-side-coordinates
  (first (side-coordinates (find-sensors-outside-corners (hash-keys sensors-map) sensors-map '()) '())))

(+ (* (first reduced-side-coordinates) 4000000) (second reduced-side-coordinates))