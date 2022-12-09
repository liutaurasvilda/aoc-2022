#lang racket

(define input (map (λ (e) (map (λ (f) (string->number f)) (map string (string->list e))))
                   (file->lines "input/day08.txt")))

(define ht (make-hash))
(hash-set! ht "visible-trees" 0)
(hash-set! ht "scenic-score" 0)

(define (update-visible ht)
  (hash-set! ht "visible-trees" (add1 (hash-ref ht "visible-trees"))))

(define (update-scenic ht score)
  (cond [(> score (hash-ref ht "scenic-score")) (hash-set! ht "scenic-score" score)]))

(define (count-scenic input tree result)
  (cond [(empty? input) (length result)]
        [(or (= (car input) tree) (> (car input) tree)) (length (cons (car input) result))]
        [else (count-scenic (cdr input) tree (cons (car input) result))]))

(define (left input i j)
  (take (list-ref input i) j))

(define (right input i j)
  (drop (list-ref input i) (add1 j)))

(define (up input i j)
  (map (λ (e) (list-ref e j)) (take input i)))

(define (down input i j)
  (map (λ (e) (list-ref e j)) (drop input (add1 i))))

(define (visible input tree i j)
  (cond [(or
          (andmap (λ (e) (< e tree)) (left input i j))
          (andmap (λ (e) (< e tree)) (right input i j))
          (andmap (λ (e) (< e tree)) (up input i j))
          (andmap (λ (e) (< e tree)) (down input i j)))
         #t]
        [else #f]))

(define (solve input)
  (for* ([i (in-range (length input))]
         [j (in-range (length (car input)))])
    (let ([tree (list-ref (list-ref input i) j)])
      (cond [(or
              (= i 0) (= j 0) (= i (- (length input) 1)) (= j (- (length (car input)) 1))
              (visible input tree i j))
             (update-visible ht)])))
  (hash-ref ht "visible-trees"))

(define (solve2 input)
  (for* ([i (in-range (length input))]
         [j (in-range (length (car input)))])
    (let ([tree (list-ref (list-ref input i) j)])
      (cond
        [(or
          (= i 0) (= j 0) (= i (- (length input) 1)) (= j (- (length (car input)) 1))) (display "")]
        [else
         (update-scenic ht (*
                            (count-scenic (reverse (up input i j)) tree '())
                            (count-scenic (reverse (left input i j)) tree '())
                            (count-scenic (right input i j) tree '())
                            (count-scenic (down input i j) tree '())))]
        )))
  (hash-ref ht "scenic-score"))

(solve input)
(solve2 input)
