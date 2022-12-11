#lang racket
(require rackunit)

(define input
  (map (λ (e) (if (equal? (car e) "noop") (list 1 (car e)) (list 2 (car e) (string->number (cadr e)))))
       (map (λ (e) (string-split e " ")) (file->lines "input/day10.txt"))))

(define test-input
  (map (λ (e) (if (equal? (car e) "noop") (list 1 (car e)) (list 2 (car e) (string->number (cadr e)))))
       (map (λ (e) (string-split e " ")) (file->lines "input/day10_test.txt"))))

(define test-input-2
  (map (λ (e) (if (equal? (car e) "noop") (list 1 (car e)) (list 2 (car e) (string->number (cadr e)))))
       (map (λ (e) (string-split e " ")) (file->lines "input/day10_test2.txt"))))

(define (execute-instruction input)
  (cons (flatten (list (- (first (car input)) 1) (cdr (car input)))) (cdr input)))

(define (solve input cycle registry h)
  (cond [(empty? input) (+ (hash-ref h 20) (hash-ref h 60) (hash-ref h 100) (hash-ref h 140) (hash-ref h 180) (hash-ref h 220))]
        [else
         (let ([started-instruction (execute-instruction input)])
           (cond [(= (first (car started-instruction)) 0)
                  (cond [(equal? (second (car started-instruction)) "noop") (solve (cdr started-instruction) (add1 cycle) registry (hash-set h cycle (* cycle registry)))]
                        [else (solve (cdr started-instruction) (add1 cycle) (+ registry (third (car started-instruction))) (hash-set h cycle (* cycle registry)))])]
                 [else (solve started-instruction (add1 cycle) registry (hash-set h cycle (* cycle registry)))]))]))


; unit tests
(check-equal? (solve test-input-2 1 1 (hash)) 13140 "test input")
(check-equal? (solve input 1 1 (hash)) 14040 "input")