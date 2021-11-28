#lang racket

(define (count-parens elem v)
  (+ v (cond
         [(eq? elem #\() 1]
         [(eq? elem #\)) -1]
         [else 0])))

(define get-input-as-list
  (string->list
   (call-with-input-file "advent-2015-1.txt"
     port->string)))

;; get total sum of parens
(foldl count-parens 0 get-input-as-list)


;; get position
(for/fold
 ([total 0]
  [position 0])
 ([i (in-naturals 1)]
  [elem get-input-as-list]
  #:break (eq? total -1))         
  (cond
    [(eq? elem #\() (values (+ 1 total) i)]
    [(eq? elem #\)) (values (+ -1 total) i)]
    [else 0]))
