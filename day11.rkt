#lang racket

(define h (make-hash))
(hash-set! h "0" 0)
(hash-set! h "1" 0)
(hash-set! h "2" 0)
(hash-set! h "3" 0)

(define (increase-inspected monkey)
  (let* ([monkey-name (monkey-name monkey)]
        [current-inspect-count (hash-ref h monkey-name)]
        [updated-inspect-count (add1 current-inspect-count)])
    (hash-set! h monkey-name updated-inspect-count)))

(struct monkey (name [items #:mutable] operation throw-to) #:transparent)

(define (throw-item a b)
  (let* ([a-items (monkey-items a)]
         [b-items (monkey-items b)]
         [a-updated (cdr a-items)]
         [b-updated (append b-items (list (first a-items)))])
    (set-monkey-items! a a-updated)
    (set-monkey-items! b b-updated)))

(define monkey-0
  (monkey
   "0"
   '(79 98)
   (λ (e) (inexact->exact (/ (* e 19) 3)))
   (λ (e) (if (= (modulo e 23) 0) 2 3))))

(define monkey-1
  (monkey
   "1"
   '(54 65 75 74)
   (λ (e) (inexact->exact (/ (+ e 6) 3)))
   (λ (e) (if (= (modulo e 19) 0) 2 0))))

(define monkey-2
  (monkey
   "2"
   '(79 60 97)
   (λ (e) (inexact->exact (/ (* e e) 3)))
   (λ (e) (if (= (modulo e 13) 0) 1 3))))

(define monkey-3
  (monkey
   "3"
   '(74)
   (λ (e) (inexact->exact (/ (+ e 3) 3)))
   (λ (e) (if (= (modulo e 17) 0) 0 1))))

(define (solve queue monkeys round required-rounds)
  (cond [(> round required-rounds) (apply * (take (sort (hash-values h) >) 2))]
        [else
         (cond [(empty? queue) (solve monkeys monkeys (add1 round) required-rounds)]
               [else
                (displayln "todo monkey inspection")
                (solve (cdr queue) monkeys round required-rounds)])]))