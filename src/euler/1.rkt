#lang racket

(define (divisible x y)
  (= 0 (modulo x y)))

(define (divisible-by-3 x)
  (divisible x 3))

(define (divisible-by-5 x)
  (divisible x 5))

(define (sum i)
  (cond
    [(or (divisible-by-3 i) (divisible-by-5 i))
      (+ i (sum (- i 1)))]
    [(> i 0)
      (sum (- i 1))]
    [else 0])
   )

(define (summ i total)
    (cond
      [(or (divisible-by-3 i) (divisible-by-5 i))
       (summ (- i 1) (+ i total))]
      [(> i 0)
       (summ (- i 1) total)]
      [else total]))


(time (sum 999999))

(time (summ 999999 0))

