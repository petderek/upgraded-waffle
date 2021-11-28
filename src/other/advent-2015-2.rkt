#lang racket

(define (get-sides str)
  (map string->number
       (string-split str "x")))

(define (rotate l)
  (append
   (rest l)
   (list
    (first l))))

(define (one-box-paper-sum sides)
  (let ([surfaces
         (sort
          (get-surfaces-from-one-box sides)
          <)])
    (apply
     +
     (append
      (list(first surfaces))
      surfaces
      surfaces))))
    

(define (get-surfaces-from-one-box sides)
  (map * sides (rotate sides)))


(define (get-smallest-perimeter sides)
  (* 2
     (apply + (rest(sort sides >)))))
  

(define (get-bow sides)
  (apply * sides))
  
  
(define (one-box-ribbon-sum sides)
  (+
   (get-bow sides)
   (get-smallest-perimeter sides)))

(with-input-from-file "advent-2015-2.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (one-box-paper-sum
       (get-sides line)))))

(with-input-from-file "advent-2015-2.txt"
  (lambda ()
    (for/sum ([line (in-lines)])
      (one-box-ribbon-sum
       (get-sides line)))))