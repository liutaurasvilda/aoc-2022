#lang racket

(define input (map (λ (e) (if (equal? (car e) "noop") (list 1 (car e)) (list 2 (car e) (string->number (cadr e)))))
                   (map (λ (e) (string-split e " ")) (file->lines "input/day10.txt"))))

(define (execute-instruction input)
  (cons (flatten (list (- (first (car input)) 1) (cdr (car input)))) (cdr input)))

(define (draw-sprite x)
  (cond [(or (= x -2) (< x -2) (> x 40)) (string-join (make-list 40 ".") "")]
        [(= x -1) (string-join (append '("#") (make-list 39 ".")) "")]
        [(= x 0) (string-join (append '("##") (make-list 38 ".")) "")]
        [(= x 38) (string-join (append (make-list 37 ".") '("###") ) "")]
        [(= x 39) (string-join (append (make-list 38 ".") '("##") ) "")]
        [(= x 40) (string-join (append (make-list 39 ".") '("#") ) "")]
        [else (string-join (append (append (make-list (- x 1) ".") '("#" "#" "#"))
                                   (make-list (- 40 (+ (- x 1) 3)) ".")) "")]))

(define (draw-crt sprite cycle crt)
  (let ([ranged-cycle (if (> cycle 40) (if (= (modulo cycle 40) 0) 1 (modulo cycle 40)) cycle)])
    (cond [(equal? (string-ref sprite (- ranged-cycle 1)) #\#) (append crt '("#"))]
          [else (append crt '("."))])))

(define (solve input cycle registry h sprite crt)
  (cond [(empty? input) (list (+ (hash-ref h 20) (hash-ref h 60) (hash-ref h 100) (hash-ref h 140) (hash-ref h 180) (hash-ref h 220)) crt)]
        [else (let ([started-instruction (execute-instruction input)])
                (cond [(= (first (car started-instruction)) 0)
                       (cond
                         [(equal? (second (car started-instruction)) "noop")
                          (solve (cdr started-instruction) (add1 cycle)
                                 registry
                                 (hash-set h cycle (* cycle registry))
                                 (draw-sprite registry)
                                 (draw-crt sprite cycle crt))]
                         [else
                          (solve (cdr started-instruction) (add1 cycle)
                                 (+ registry (third (car started-instruction)))
                                 (hash-set h cycle (* cycle registry))
                                 (draw-sprite (+ registry (third (car started-instruction))))
                                 (draw-crt sprite cycle crt))])]
                      [else
                       (solve started-instruction (add1 cycle)
                              registry
                              (hash-set h cycle (* cycle registry))
                              sprite
                              (draw-crt sprite cycle crt))]))]))


(first (solve input 1 1 (hash) (draw-sprite 1) '()))
(define crt (second (solve input 1 1 (hash) (draw-sprite 1) '())))

(displayln (string-join (take crt 40) ""))
(displayln (string-join (take (drop crt 40) 40) ""))
(displayln (string-join (take (drop crt 80) 40) ""))
(displayln (string-join (take (drop crt 120) 40) ""))
(displayln (string-join (take (drop crt 160) 40) ""))
(displayln (string-join (drop crt 200) ""))
