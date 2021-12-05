#lang racket

(struct point (x y))

(define raw-input
  (port->lines (open-input-file "5.txt")))

(define point-input
  (for/list [(line (in-list raw-input))]
    (define pairs (string-split line " -> "))
    (define nums
      (map
       string->number
       (flatten
        (map
         (lambda (x) (string-split x ","))
         pairs))))
    (cons
     (point (first nums) (second nums))
     (point (third nums) (fourth nums)))))

(define largest-points
  (let loop [(points (flatten point-input))
             (acc (point 0 0))]
    (define new-point
      (match (first points)
        [(point x y) (point (max x (point-x acc)) (max y (point-y acc)))]))
    (cond
      [(empty? (rest points)) new-point]
      [else (loop (rest points) new-point)])))

(define upper-x
  (add1
   (point-x largest-points)))

(define upper-y
  (add1
   (point-y largest-points)))

(define (empty-ground)
  (let [(vec (make-vector upper-x))]
    (for [(i (in-range upper-x))]
      (vector-set! vec i (make-vector upper-y 0)))
    vec))

(define (update-ground! ground x y fn)
  (let* [(inner (vector-ref ground x))
         (val (vector-ref inner y))]
    (vector-set! inner y (fn val))))

(define (get-ground ground x y)
  (vector-ref
   (vector-ref
    ground
    x)
   y))
    
(define (is-horizontal-or-vertical pair)
  (let [(x1 (point-x (car pair)))
        (y1 (point-y (car pair)))
        (x2 (point-x (cdr pair)))
        (y2 (point-y (cdr pair)))]
    (or
     (eq? x1 x2)
     (eq? y1 y2))))

(define (mark-ground ground pair)
  (let [(x1 (point-x (car pair)))
        (y1 (point-y (car pair)))
        (x2 (point-x (cdr pair)))
        (y2 (point-y (cdr pair)))]
    (for* [(x (in-inclusive-range (min x1 x2) (max x1 x2)))
           (y (in-inclusive-range (min y1 y2) (max y1 y2)))]
      (update-ground! ground x y add1))
    ground))

(define (mark-ground-diagonal ground pair)
  (let [(x1 (point-x (car pair)))
        (y1 (point-y (car pair)))
        (x2 (point-x (cdr pair)))
        (y2 (point-y (cdr pair)))]
    (define x-step
      (if (< x1 x2) 1 -1))
    (define y-step
      (if (< y1 y2) 1 -1))
    (for [(x (in-inclusive-range x1 x2 x-step))
          (y (in-inclusive-range y1 y2 y-step))]
      (update-ground! ground x y add1))
    ground))
        
(define (count-ground ground fn)
  (for*/sum [(i (in-range upper-x))
             (j (in-range upper-y))]
    (fn (get-ground ground i j))))

(define (at-least-2 x)
  (if (>= x 2) 1 0))

;; problem 1

(let loop [(pairs (filter is-horizontal-or-vertical point-input))
           (ground (empty-ground))]
  (cond
    [(empty? pairs) (count-ground ground at-least-2)]
    [else (loop (rest pairs) (mark-ground ground (first pairs)))]))

;; problem 2

(define partial-ground
  (let loop [(pairs (filter is-horizontal-or-vertical point-input))
             (ground (empty-ground))]
    (cond
      [(empty? pairs) ground]
      [else (loop (rest pairs) (mark-ground ground (first pairs)))])))

(let loop [(pairs (filter (lambda (l) (not (is-horizontal-or-vertical l))) point-input))
           (ground partial-ground)]
  (cond
    [(empty? pairs) (count-ground ground at-least-2)]
    [else (loop (rest pairs) (mark-ground-diagonal ground (first pairs)))]))

