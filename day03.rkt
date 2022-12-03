#lang racket

(define h (hash #\a 1 #\b 2 #\c 3 #\d 4 #\e 5 #\f 6 #\g 7 #\h 8 #\i 9 #\j 10 #\k 11 #\l 12 #\m 13 #\n 14 #\o 16 #\p 16 #\q 17 #\r 18 #\s 19 #\t 20 #\u 21 #\v 22 #\w 23 #\x 24 #\y 25 #\z 26
                #\A 27 #\B 28 #\C 29 #\D 30 #\E 31 #\F 32 #\G 33 #\H 34 #\I 35 #\J 36 #\K 37 #\L 38 #\M 39 #\N 40 #\O 41 #\P 42 #\Q 43 #\R 44 #\S 45 #\T 46 #\U 47 #\V 48 #\W 49 #\X 50 #\Y 51 #\Z 52))

(define input (map (λ (e) (string->list e)) (file->lines "input/day03.txt")))

(apply + (map (λ (e) (hash-ref h e)) (map (λ (e) (car (set-intersect (car e) (cadr e)))) (map (λ (e) (list (take e (/ (length e) 2)) (drop e (/ (length e) 2)))) input))))

(define (intersect-slice l result)
  (cond [(empty? l) result]
        [(> (length l) 3) (intersect-slice (cdddr l) (append (set-intersect (car l) (cadr l) (caddr l)) result))]
        [else (intersect-slice '() (append (set-intersect (car l) (cadr l) (caddr l)) result))]))
                    
(apply + (map (λ (e) (hash-ref h e)) (intersect-slice input '())))