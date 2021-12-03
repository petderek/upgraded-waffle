#lang racket

(define in
  (with-input-from-file "3.txt"
    (lambda ()
      (for/list ([line (in-lines)])
        line))))

(define buckets
  (make-vector
   (string-length (first in))
   0))

(define (one-most-common position)
  (>
   (vector-ref buckets position)
   (/ (length in) 2)))

(define (binstr->number binlist)
  (for/sum [(i (in-range (sub1 (string-length (first in))) -1 -1))
            (digit (in-list binlist))]
    (* digit (expt 2 i))))

(define (flip num)
  (match num
    [0 1]
    [1 0]))

(define gamma-list
  (let loop [(line (first in))
             (lines (rest in))]
    (for [(character (string->list line))
          (i (in-naturals))]
      (vector-set!
       buckets
       i
       (+
        (vector-ref buckets i)
        (string->number (string character)))))
    (cond
      [(not (empty? lines)) (loop (first lines) (rest lines))]
      [else
        (for/list [(i (in-range (string-length (first in))))]
          (if (one-most-common i) 1 0))])))


(define epsilon-list
  (map flip gamma-list))


;; problem 1

(* (binstr->number gamma-list) (binstr->number epsilon-list))

    