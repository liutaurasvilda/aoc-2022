#lang racket

(struct monkey ([items #:mutable] operation throw-to) #:transparent)

(define (throw-item a b)
  (let* ([a-items (monkey-items a)]
         [b-items (monkey-items b)]
         [a-updated (cdr a-items)]
         [b-updated (append b-items (list (first a-items)))])
    (set-monkey-items! a a-updated)
    (set-monkey-items! b b-updated)))

(define monkey-0
  (monkey
   '(79 98)
   (λ (e) (inexact->exact (/ (* e 19) 3)))
   (λ (e) (if (= (modulo e 23) 0) 2 3))))

(define monkey-1
  (monkey
   '(54 65 75 74)
   (λ (e) (inexact->exact (/ (+ e 6) 3)))
   (λ (e) (if (= (modulo e 19) 0) 2 0))))

(define monkey-2
  (monkey
   '(79 60 97)
   (λ (e) (inexact->exact (/ (* e e) 3)))
   (λ (e) (if (= (modulo e 13) 0) 1 3))))

(define monkey-3
  (monkey
   '(74)
   (λ (e) (inexact->exact (/ (+ e 3) 3)))
   (λ (e) (if (= (modulo e 17) 0) 0 1))))