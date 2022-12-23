#lang racket

(define input
  (map (λ (e)
         (map (λ (e) (string->number e)) (string-split e ",")))
       (file->lines "input/day18.txt")))

(define (covers? cube remaining-cubes)
  (if (pair? (member cube remaining-cubes)) 1 -1))

(define (count-covered-sides input total-sides)
  (cond [(empty? input) total-sides]
        [else
         (let* ([cube (car input)]
                [remaining-cubes (cdr input)]
                [x-diff-1 (list (- (first cube) 1) (second cube) (third cube))]
                [x-diff-2 (list (+ (first cube) 1) (second cube) (third cube))]
                [y-diff-1 (list (first cube) (- (second cube) 1) (third cube))]
                [y-diff-2 (list (first cube) (+ (second cube) 1) (third cube))]
                [z-diff-1 (list (first cube) (second cube) (- (third cube) 1))]
                [z-diff-2 (list (first cube) (second cube) (+ (third cube) 1))]
                [covered-sides (* (count positive?
                                         (list
                                          (covers? x-diff-1 remaining-cubes)
                                          (covers? x-diff-2 remaining-cubes)
                                          (covers? y-diff-1 remaining-cubes)
                                          (covers? y-diff-2 remaining-cubes)
                                          (covers? z-diff-1 remaining-cubes)
                                          (covers? z-diff-2 remaining-cubes))) 2)])
           (count-covered-sides (cdr input) (- total-sides covered-sides)))]))

(count-covered-sides input (* (length input) 6))