#lang racket

(define in
  (with-input-from-file "3.txt"
    (lambda ()
      (for/list ([line (in-lines)])
        line))))

(define bit-width
  (string-length (first in)))

(define (most-common val size)
  (if
   (>=
    val
    (/ size 2))
   1
   0))

(define (binlist->number binlist)
  (for/sum [(i (in-range (sub1 (string-length (first in))) -1 -1))
            (digit (in-list binlist))]
    (* digit (expt 2 i))))

(define (binstr->number binstr)
  (binlist->number
   (map
    (lambda (c)
      (match c
        [#\0 0]
        [#\1 1]))
    (string->list binstr))))


(define (flip num)
  (match num
    [0 1]
    [1 0]))

(define (is-bit i bit)
  (lambda (line)
    (eq?
     (string->number (string (string-ref line i)))
     bit)))

(define (gamma-list)
  (let loop [(line (first in))
             (lines (rest in))
             (buckets (make-vector bit-width 0))]
    (for [(character (string->list line))
          (i (in-naturals))]
      (vector-set!
       buckets
       i
       (+
        (vector-ref buckets i)
        (string->number (string character)))))
    (cond
      [(not (empty? lines)) (loop (first lines) (rest lines) buckets)]
      [else
       (for/list [(i (in-range (string-length (first in))))]
         (most-common (vector-ref buckets i) (length in)))])))


(define (epsilon-list)
  (map flip (gamma-list)))

(define (get-match-index outer-lines position-to-check)
  (let loop [(line (first outer-lines))
             (inner-lines (rest outer-lines))
             (total 0)]
    (define new-total
      (match (string-ref line position-to-check)
        [#\0 total]
        [#\1 (add1 total)]))
    (cond
      [(empty? inner-lines) (most-common new-total (length outer-lines))]
      [else
       (loop
        (first inner-lines)
        (rest inner-lines)
        new-total)])))
        

(define (oxygen-rating)
  (let outer-loop [(outer-lines in)
                   (position-to-check 0)]
    (cond
      [(eq? 1 (length outer-lines)) (first outer-lines)]
      [else
       (let [(match-index (get-match-index outer-lines position-to-check))]
         (outer-loop
          (filter (is-bit position-to-check match-index) outer-lines)
          (add1 position-to-check)))])))

(define (co2-rating)
  (let outer-loop [(outer-lines in)
                   (position-to-check 0)]
    (cond
      [(eq? 1 (length outer-lines)) (first outer-lines)]
      [else
       (let [(match-index (flip (get-match-index outer-lines position-to-check)))]
         (outer-loop
          (filter (is-bit position-to-check match-index) outer-lines)
          (add1 position-to-check)))])))

;; problem 1
(* (binlist->number (gamma-list)) (binlist->number (epsilon-list)))

;; problem 2
(* (binstr->number (oxygen-rating)) (binstr->number (co2-rating)))
