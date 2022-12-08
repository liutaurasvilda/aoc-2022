#lang racket

(define input (map (λ (e) (string-split e " ")) (file->lines "input/day07.txt")))

(define (cd symbol dir-tree)
  (cond [(equal? symbol "/") (cons symbol '())]
        [(equal? symbol "..") (drop-right dir-tree 1)]
        [else (append dir-tree (list symbol))]))

(define (build-tree input dir-tree ht)
  (cond [(empty? input) ht]
        [(equal? (first (car input)) "$")
         (cond [(equal? (second (car input)) "cd") (build-tree (cdr input) (cd (third (car input)) dir-tree) ht)]
               [(equal? (second (car input)) "ls") (build-tree (cdr input) dir-tree ht)])]
        [else (build-tree (cdr input) dir-tree
                          (hash-set ht (string-join dir-tree "-")
                                    (append
                                     (if (hash-has-key? ht (string-join dir-tree "-")) (hash-ref ht (string-join dir-tree "-")) '())
                                     (list (car input)))))]))

(define tree-struct-temp (build-tree input '() (hash)))

(define (composite-keys ht keys)
  (cond [(empty? keys) ht]
        [else (composite-keys
               (hash-set ht (car keys)
                         (append
                          (map (λ (e) (list "dir" (string-join (list (car keys) (cadr e)) "-")))
                               (filter (λ (e) (equal? (car e) "dir")) (hash-ref ht (car keys))))
                          (filter (λ (e) (not (equal? (car e) "dir"))) (hash-ref ht (car keys)))))
               (cdr keys))]))

(define tree-struct (composite-keys tree-struct-temp (hash-keys tree-struct-temp)))

(define (replace-dir ht l res)
  (cond [(empty? l) res]
        [else (replace-dir ht (cdr l) (append res (hash-ref ht (cadr (car l)))))]))

(define (tree-leaves ht key)
  (cond [(empty? (filter (λ (e) (equal? (car e) "dir")) (hash-ref ht key))) ht]
        [else
         (tree-leaves
          (hash-set ht key
                    (append
                     (replace-dir ht (filter (λ (e) (equal? (car e) "dir")) (hash-ref ht key)) '())
                     (filter (λ (e) (not (equal? (car e) "dir"))) (hash-ref ht key)))) key)]))

(define (tree-with-leaves ht keys)
  (cond [(empty? keys) ht]
        [else (tree-with-leaves (tree-leaves ht (car keys)) (cdr keys))]))

(define tree-with-leaves-struct (tree-with-leaves tree-struct (hash-keys tree-struct)))

(define (sum-tree ht-sum ht keys)
  (cond [(empty? keys) ht-sum]
        [else (sum-tree (hash-set ht-sum (car keys) (apply + (map (λ (e) (string->number (car e))) (hash-ref ht (car keys))))) ht (cdr keys))]))

(define tree-with-sizes-struct (sum-tree (hash) tree-with-leaves-struct (hash-keys tree-with-leaves-struct)))

(define (solve values sum)
  (cond [(empty? values) sum]
        [else (if (< (car values) 100001) (solve (cdr values) (+ sum (car values))) (solve (cdr values) sum))]))

(define space-to-free-up (- 30000000 (- 70000000 (hash-ref tree-with-sizes-struct "/"))))

(solve (hash-values tree-with-sizes-struct) 0)
(car (car (sort (filter (λ (e) (> (cadr e) -1)) (map (λ (e) (list e (- e space-to-free-up))) (hash-values tree-with-sizes-struct))) (λ (x y) (< (second x) (second y))))))