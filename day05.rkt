#lang racket

(define input
  (map (λ (e) (map (λ (e) (string->number e)) (list (second e) (fourth e) (sixth e))))
       (map (λ (e) (string-split e " "))
            (file->lines "input/day05.txt"))))

(define ht (hash
            1 '("C" "Q" "B")
            2 '("Z" "W" "Q" "R")
            3 '("V" "L" "R" "M" "B")
            4 '("W" "T" "V" "H" "Z" "C")
            5 '("G" "V" "N" "B" "H" "Z" "D")
            6 '("Q" "V" "F" "J" "C" "P" "N" "H")
            7 '("S" "Z" "W" "R" "T" "G" "D")
            8 '("P" "Z" "W" "B" "N" "M" "G" "C")
            9 '("P" "F" "Q" "W" "M" "B" "J" "N")))

(define (play input ht rev)
  (cond [(empty? input) ht]
        [else (let* ([spec (car input)]
                     [qnt (car spec)]
                     [from (cadr spec)]
                     [to (caddr spec)]
                     [content-from (hash-ref ht from)]
                     [content-to (hash-ref ht to)]
                     [new-content-from (drop content-from qnt)]
                     [new-content-to (append (if rev (reverse (take content-from qnt)) (take content-from qnt)) content-to)])
                (play (cdr input) (hash-set (hash-set ht from new-content-from) to new-content-to) rev))]))

(string-join (map (λ (e) (car e)) (hash-values (play input ht #t))) "")
(string-join (map (λ (e) (car e)) (hash-values (play input ht #f))) "")