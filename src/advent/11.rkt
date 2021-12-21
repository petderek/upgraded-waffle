#lang racket
(define (2d-vec m n val)
  (for/vector [(i (in-range n))]
    (make-vector m val)))

(define (2d-ref vec m n)
  (vector-ref
   (vector-ref vec n)
   m))

(define (2d-set! vec m n val)
  (vector-set!
   (vector-ref vec n)
   m
   val))

(define (n-length vec)
  (vector-length vec))

(define (m-length vec)
  (vector-length
   (vector-ref vec 0)))

(define (char->number c)
  (string->number (string c)))

(define (digits str)
  (for/list [(c (in-string str))]
    (char->number c)))

(define raw-input
  (port->lines (open-input-file "11.txt")))

(define (input-as-vector)
  (let loop ([lines raw-input]
             [n 0]
             [vec (2d-vec
                   (string-length (first raw-input))
                   (length raw-input)
                   0)])
    (for [(m (in-naturals))
          (digit (in-list (digits (first lines))))]
      (2d-set! vec m n digit))
    (cond
      [(empty? (rest lines)) vec]
      [else (loop (rest lines) (add1 n) vec)])))

(define (increment-all! vec)
  (for* [(m (in-range (m-length vec)))
         (n (in-range (n-length vec)))]
    (define val (2d-ref vec m n))
    (2d-set! vec m n (add1 val))))

(define (visited? marks m n)
  (2d-ref marks m n))

(define (visit! marks m n)
  (2d-set! marks m n #t))

(define (out-of-bounds? vec m n)
  (or
   (< m 0)
   (< n 0)
   (>= m (m-length vec))
   (>= n (n-length vec))))

(define (bfs-flash! vec marks m n)
  (cond
    [(out-of-bounds? vec m n) #f]
    [else
     (2d-set! vec m n (add1 (2d-ref vec m n)))
     (cond
       [(>= 9 (2d-ref vec m n)) #f]
       [(visited? marks m n) #f]
       [else
        (visit! marks m n)
        (for* [(x (in-inclusive-range -1 1))
               (y (in-inclusive-range -1 1))
               #:unless (and (= 0 x) (= 0 y))]
          (bfs-flash! vec marks (+ m x) (+ n y)))])]))
      
(define (flash-all! vec)
  (define marks (2d-vec (m-length vec) (n-length vec) #f))
  (for* [(m (in-range (m-length vec)))
         (n (in-range (n-length vec)))]
    (if (<= 10 (2d-ref vec m n))
        (bfs-flash! vec marks m n)
        #f)))

(define (normalize-all! vec)
  (for*/sum [(m (in-range (m-length vec)))
             (n (in-range (n-length vec)))]
    (define elem (2d-ref vec m n))
    (cond
      [(<= 10 elem)
       (2d-set! vec m n 0)
       1]
      [else 0])))

(define (all-zeros? vec)
  (for/and [(outer (in-vector vec))]
    (for/and [(val (in-vector outer))]
      (= val 0))))

;; problem 1

(define vec-part-1 (input-as-vector))

(for/sum [(i (in-range 100))]
  (increment-all! vec-part-1)
  (flash-all! vec-part-1)
  (normalize-all! vec-part-1))

;; problem 2

(define vec-part-2 (input-as-vector))

(let loop [(i 0)]
  (cond
    [(< 1000 i) #f]
    [(all-zeros? vec-part-2) i]
    [else
     (increment-all! vec-part-2)
     (flash-all! vec-part-2)
     (normalize-all! vec-part-2)
     (loop (add1 i))]))