#lang racket
(require rackunit)

(define input
  (map (λ (e) (string->list e))
       (file->lines "input/day13_test.txt")))

(define (c c)
  (char->integer c))

(define (standardize packet)
  (cons #\[ (cons (car packet) (cons #\] (cdr packet)))))

(define (in-order? packet1 packet2)
  (let ([e1 (car packet1)]
        [e2 (car packet2)])
    (cond [(and (equal? e1 #\]) (char-numeric? e2)) #t]
          [(and (char-numeric? e1) (equal? e2 #\])) #f]
          [(and (equal? e1 #\[) (equal? e2 #\])) #f]
          [(and (equal? e1 #\]) (equal? e2 #\[)) #t]
          [(and (char-numeric? e1) (not (char-numeric? e2))) (in-order? (standardize packet1) packet2)]
          [(and (not (char-numeric? e1)) (char-numeric? e2)) (in-order? packet1 (standardize packet2))]
          [(and (equal? e1 #\]) (equal? e2 #\,)) #t]
          [(and (equal? e1 #\,) (equal? e2 #\])) #f]
          [(> (c e2) (c e1)) #t]
          [(< (c e2) (c e1)) #f]
          [(= (c e1) (c e2)) (in-order? (cdr packet1) (cdr packet2))])))

(define (solve input index indices)
  (cond [(empty? input) (apply + indices)]
        [(in-order? (car input) (cadr input)) (solve (cddr input) (add1 index) (cons index indices))]
        [else (solve (cddr input) (add1 index) indices)]))

(check-equal?
 (in-order?
  '(#\[ #\1 #\, #\1 #\, #\3 #\, #\1 #\, #\1 #\])
  '(#\[ #\1 #\, #\1 #\, #\5 #\, #\1 #\, #\1 #\]))
 #t "pair 1")

(check-equal?
 (in-order?
  '(#\[ #\[ #\1 #\] #\, #\[ #\2 #\, #\3 #\, #\4 #\] #\])
  '(#\[ #\[ #\1 #\] #\, #\4 #\]))
 #t "pair 2")

(check-equal?
 (in-order?
  '(#\[ #\9 #\])
  '(#\[ #\[ #\8 #\, #\7 #\, #\6 #\] #\]))
 #f "pair 3")

(check-equal?
 (in-order?
  '(#\[ #\[ #\4 #\, #\4 #\] #\, #\4 #\, #\4 #\])
  '(#\[ #\[ #\4 #\, #\4 #\] #\, #\4 #\, #\4 #\, #\4 #\]))
 #t "pair 4")

(check-equal?
 (in-order?
  '(#\[ #\7 #\, #\7 #\, #\7 #\, #\7 #\])
  '(#\[ #\7 #\, #\7 #\, #\7 #\]))
 #f "pair 5")

(check-equal?
 (in-order?
  '(#\[ #\])
  '(#\[ #\3 #\]))
 #t "pair 6")

(check-equal?
 (in-order?
  '(#\[ #\[ #\[ #\] #\] #\])
  '(#\[ #\[ #\] #\]))
 #f "pair 7")

(check-equal?
 (in-order?
  '(#\[ #\1 #\, #\[ #\2 #\, #\[ #\3 #\, #\[ #\4 #\, #\[ #\5 #\, #\6 #\, #\7 #\] #\] #\] #\] #\, #\8 #\, #\9 #\])
  '(#\[ #\1 #\, #\[ #\2 #\, #\[ #\3 #\, #\[ #\4 #\, #\[ #\5 #\, #\6 #\, #\0 #\] #\] #\] #\] #\, #\8 #\, #\9 #\]))
 #f "pair 8")
