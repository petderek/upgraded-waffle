#lang racket

(define (increment vec i)
  (vector-set!
   vec
   i
   (add1 (vector-ref vec i))))

(define (input-vec)
  (define vec (make-vector 9 0))
  (for [(ch (in-string "3,3,5,1,1,3,4,2,3,4,3,1,1,3,3,1,5,4,4,1,4,1,1,1,3,3,2,3,3,4,2,5,1,4,1,2,2,4,2,5,1,2,2,1,1,1,1,4,5,4,3,1,4,4,4,5,1,1,4,3,4,2,1,1,1,1,5,2,1,4,2,4,2,5,5,5,3,3,5,4,5,1,1,5,5,5,2,1,3,1,1,2,2,2,2,1,1,2,1,5,1,2,1,2,5,5,2,1,1,4,2,1,4,2,1,1,1,4,2,5,1,5,1,1,3,1,4,3,1,3,2,1,3,1,4,1,2,1,5,1,2,1,4,4,1,3,1,1,1,1,1,5,2,1,5,5,5,3,3,1,2,4,3,2,2,2,2,2,4,3,4,4,4,1,2,2,3,1,1,4,1,1,1,2,1,4,2,1,2,1,1,2,1,5,1,1,3,1,4,3,2,1,1,1,5,4,1,2,5,2,2,1,1,1,1,2,3,3,2,5,1,2,1,2,3,4,3,2,1,1,2,4,3,3,1,1,2,5,1,3,3,4,2,3,1,2,1,4,3,2,2,1,1,2,1,4,2,4,1,4,1,4,4,1,4,4,5,4,1,1,1,3,1,1,1,4,3,5,1,1,1,3,4,1,1,4,3,1,4,1,1,5,1,2,2,5,5,2,1,5"))]
    (match ch
      [#\, (void)]
      [x (increment vec (string->number (string x)))]))
  vec)

(define (add-a-day vec)
  (define newfish (vector-ref vec 0))
  (for [(i (in-range 8))]
    (vector-set! vec i (vector-ref vec (add1 i))))
  (vector-set! vec 8 newfish)
  (vector-set!
   vec
   6
   (+
    (vector-ref vec 6)
    newfish))
  vec)

(define (add-n-days vec n)
  (for [(i (in-range n))]
    (add-a-day vec))
  vec)
                 
;; problem 1

(for/sum [(val (in-vector (add-n-days (input-vec) 80)))]
  val)

;; problem 2

(for/sum [(val (in-vector (add-n-days (input-vec) 256)))]
  val)
