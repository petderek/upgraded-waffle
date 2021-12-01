#lang racket

(define get-lines
  (with-input-from-file "1.txt"
    (lambda ()
      (for/list ([line (in-lines)])
        (string->number line)))))

;; part 1

(let loop [(prev (first get-lines))
           (lines (rest get-lines))
           (sum 0)]
  (cond
    [(empty? lines) sum]
    [(< prev (first lines)) (loop (first lines) (rest lines) (add1 sum))]
    [else (loop (first lines) (rest lines) sum)]))

;; part 2

(let loop [(a (first get-lines))
           (b (second get-lines))
           (c (third get-lines))
           (lines (rest (rest (rest get-lines))))
           (sum 0)]

  (cond
    [(empty? lines) sum]
    [(< (+ a b c) (+ b c (first lines))) (loop b c (first lines) (rest lines) (add1 sum))]
    [else (loop b c (first lines) (rest lines) sum)]))