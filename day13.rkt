#lang racket
(require rackunit)

(define input
  (map (λ (e) (read (open-input-string e)))
       (file->lines "input/day13_test.txt")))

(define (in-order? packet1 packet2)
  #f)

(define (solve input index indices)
  (cond [(empty? input) (apply + indices)]
        [(in-order? (car input) (cadr input)) (solve (cddr input) (add1 index) (cons index indices))]
        [else (solve (cddr input) (add1 index) indices)]))

(solve input 1 '())

(check-equal? (in-order? '(1 1 3 1 1) '(1 1 5 1 1)) #t "pair 1")
(check-equal? (in-order? '((1) (2 3 4)) '((1) 4)) #t "pair 2")
(check-equal? (in-order? '(9) '((8 7 6))) #f "pair 3")
(check-equal? (in-order? '((4 4) 4 4) '((4 4) 4 4 4)) #t "pair 4")
(check-equal? (in-order? '(7 7 7 7) '(7 7 7)) #f "pair 5")
(check-equal? (in-order? '() '(3)) #t "pair 6")
(check-equal? (in-order? '((())) '(())) #f "pair 7")
(check-equal? (in-order? '(1 (2 (3 (4 (5 6 7)))) 8 9) '(1 (2 (3 (4 (5 6 0)))) 8 9)) #f "pair 8")