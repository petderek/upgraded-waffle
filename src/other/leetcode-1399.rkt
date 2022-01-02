#lang racket

(define (get-digits n)
  (map
   (lambda (x) (- x 48))
   (map
    char->integer
    (string->list (number->string n)))))

(define (sum-digits n)
  (apply + (get-digits n)))

;; returns a list-of-lists where each number is grouped by sum of digits
(define (get-groups count)
  (let loop [(n count)
             (acc (hash))]
    (define new-hash
      (hash-update acc
                   (sum-digits n)
                   (lambda (x) (append x (list n)))
                   (list)))
    (cond
      [(eq? 1 n) new-hash]
      [(loop (sub1 n) new-hash)])))
    
(define (get-group-max-count n)
  (define ht (get-groups n))
  (define counts
   (map
    length
    (hash-values ht)))
  (let loop [(count 0)
             (max 0)
             (lst counts)]
    (cond
      [(empty? lst) count]
      [(> (first lst) max) (loop 1 (first lst) (rest lst))]
      [(eq? (first lst) max) (loop (add1 count) max (rest lst))]
      [else (loop count max (rest lst))])))
