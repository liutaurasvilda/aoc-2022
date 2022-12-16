#lang racket
(require rackunit)

(define input
  (map (λ (e) (string->list e))
       (file->lines "input/day13.txt")))

(define input2
  (map (λ (e) (string->list e))
       (cons "[[2]]" (cons "[[6]]" (file->lines "input/day13.txt")))))

(define (c c)
  (char->integer c))

(define (number? c)
  (pair? (member (char->integer c) (sequence->list (in-inclusive-range 48 58)))))

(define (standardize packet)
  (cons #\[ (cons (car packet) (cons #\] (cdr packet)))))

(define (in-order? packet1 packet2)
  (let ([e1 (car packet1)]
        [e2 (car packet2)])
    (cond [(and (equal? e1 #\]) (number? e2)) #t]
          [(and (number? e1) (equal? e2 #\])) #f]
          [(and (equal? e1 #\]) (equal? e2 #\,)) #t]
          [(and (equal? e1 #\,) (equal? e2 #\])) #f]
          [(and (equal? e1 #\[) (equal? e2 #\])) #f]
          [(and (equal? e1 #\]) (equal? e2 #\[)) #t]
          [(and (number? e1) (not (number? e2))) (in-order? (standardize packet1) packet2)]
          [(and (not (number? e1)) (number? e2)) (in-order? packet1 (standardize packet2))]
          [(> (c e2) (c e1)) #t]
          [(< (c e2) (c e1)) #f]
          [(= (c e1) (c e2)) (in-order? (cdr packet1) (cdr packet2))])))

(define (solve input index indices)
  (cond [(empty? input) (apply + indices)]
        [(in-order? (car input) (cadr input)) (solve (cddr input) (add1 index) (cons index indices))]
        [else (solve (cddr input) (add1 index) indices)]))

(define (solve2 input)
  (let ([sorted (sort input in-order?)])
    (*
     (add1 (index-of sorted '(#\[ #\[ #\2 #\] #\])))
     (add1 (index-of sorted '(#\[ #\[ #\6 #\] #\]))))))

(solve input 1 '())
(solve2 input2)