#lang racket

(define get-input-as-list
  (string->list
   (call-with-input-file "advent-2015-3.txt"
     port->string)))

(define directions
  (for/list ([direction get-input-as-list])
    (match direction
      [#\v '(0 . -1)]
      [#\^ '(0 . 1)]
      [#\< '(-1 . 0)]
      [#\> '(1 . 0)])))

(define (next-house current-house direction)
  (cons
   (+
    (car current-house)
    (car direction))
   (+
    (cdr current-house)
    (cdr direction))))

(define (accumulate-houses houses direction)
  (append
   houses
   (list
    (next-house
     (last houses)
     direction))))

(define get-house-coordinates
  (for/fold
   ([houses (list '(0 . 0))])
   ([direction directions])
    (accumulate-houses houses direction)))

;; grab number of unique houses
(time (length
       (remove-duplicates get-house-coordinates)))

(define get-robo-santa-coordinates
  (let-values
      ([(santa-houses robo-houses)
        (for/fold
         ([santa (list '(0 . 0))]
          [robo  (list '(0 . 0))])
         ([direction directions]
          [i (in-naturals)])
          (if (even? i)
              (values
               (accumulate-houses santa direction)
               robo)
              (values
               santa
               (accumulate-houses robo direction))))])
    (append santa-houses robo-houses)))

(time (length (remove-duplicates get-robo-santa-coordinates)))