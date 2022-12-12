#lang racket

(struct monkey (name [items #:mutable] operation throw-to) #:transparent)

(define worry-reducer 1)

(define monkey-0
  (monkey
   "0"
   '(79 98)
   (λ (e) (floor (/ (* e 19) worry-reducer)))
   (λ (e) (if (= (modulo e 23) 0) 2 3))))

(define monkey-1
  (monkey
   "1"
   '(54 65 75 74)
   (λ (e) (floor (/ (+ e 6) worry-reducer)))
   (λ (e) (if (= (modulo e 19) 0) 2 0))))

(define monkey-2
  (monkey
   "2"
   '(79 60 97)
   (λ (e) (floor (/ (* e e) worry-reducer)))
   (λ (e) (if (= (modulo e 13) 0) 1 3))))

(define monkey-3
  (monkey
   "3"
   '(74)
   (λ (e) (floor (/ (+ e 3) worry-reducer)))
   (λ (e) (if (= (modulo e 17) 0) 0 1))))

#| REAL INPUT
(define monkey-0
  (monkey
   "0"
   '(54 82 90 88 86 54)
   (λ (e) (floor (/ (* e 7) worry-reducer)))
   (λ (e) (if (= (modulo e 11) 0) 2 6))))

(define monkey-1
  (monkey
   "1"
   '(91 65)
   (λ (e) (floor (/ (* e 13) worry-reducer)))
   (λ (e) (if (= (modulo e 5) 0) 7 4))))

(define monkey-2
  (monkey
   "2"
   '(62 54 57 92 83 63 63)
   (λ (e) (floor (/ (+ e 1) worry-reducer)))
   (λ (e) (if (= (modulo e 7) 0) 1 7))))

(define monkey-3
  (monkey
   "3"
   '(67 72 68)
   (λ (e) (floor (/ (* e e) worry-reducer)))
   (λ (e) (if (= (modulo e 2) 0) 0 6))))

(define monkey-4
  (monkey
   "4"
   '(68 89 90 86 84 57 72 84)
   (λ (e) (floor (/ (+ e 7) worry-reducer)))
   (λ (e) (if (= (modulo e 17) 0) 3 5))))

(define monkey-5
  (monkey
   "5"
   '(79 83 64 58)
   (λ (e) (floor (/ (+ e 6) worry-reducer)))
   (λ (e) (if (= (modulo e 13) 0) 3 0))))

(define monkey-6
  (monkey
   "6"
   '(96 72 89 70 88)
   (λ (e) (floor (/ (+ e 4) worry-reducer)))
   (λ (e) (if (= (modulo e 3) 0) 1 2))))

(define monkey-7
  (monkey
   "7"
   '(79)
   (λ (e) (floor (/ (+ e 8) 3)))
   (λ (e) (if (= (modulo e 19) 0) 4 5))))
|#

(define h (make-hash))
(hash-set! h "0" 0)
(hash-set! h "1" 0)
(hash-set! h "2" 0)
(hash-set! h "3" 0)
;(hash-set! h "4" 0)
;(hash-set! h "5" 0)
;(hash-set! h "6" 0)
;(hash-set! h "7" 0)

;(define monkeys (list monkey-0 monkey-1 monkey-2 monkey-3 monkey-4 monkey-5 monkey-6 monkey-7))
(define monkeys (list monkey-0 monkey-1 monkey-2 monkey-3))

(define (increase-inspected monkey)
  (let* ([monkey-name (monkey-name monkey)]
         [current-inspect-count (hash-ref h monkey-name)]
         [updated-inspect-count (add1 current-inspect-count)])
    (hash-set! h monkey-name updated-inspect-count)))

(define (throw-item a b)
  (let* ([a-items (monkey-items a)]
         [b-items (monkey-items b)]
         [a-updated (cdr a-items)]
         [item-worry ((monkey-operation a) (first a-items))]
         [b-updated (append b-items (list item-worry))])
    (increase-inspected a)
    (set-monkey-items! a a-updated)
    (set-monkey-items! b b-updated)))

(define (inspect monkey monkeys)
  (let ([items (monkey-items monkey)]
        [operation-f (monkey-operation monkey)]
        [throw-to-f (monkey-throw-to monkey)])
    (for ([i items])
      (throw-item monkey (list-ref monkeys (throw-to-f (operation-f i)))))))

(define (solve queue monkeys round required-rounds)
  (cond [(> round required-rounds) (apply * (take (sort (hash-values h) >) 2))]
        [else
         (cond [(empty? queue) (solve monkeys monkeys (add1 round) required-rounds)]
               [else
                (inspect (car queue) monkeys)
                (solve (cdr queue) monkeys round required-rounds)])]))

(solve monkeys monkeys 1 1)