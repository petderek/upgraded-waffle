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

(define (char->number c)
  (string->number (string c)))

(define (digits str)
  (for/list [(c (in-string str))]
    (char->number c)))

(define raw-input
  (port->lines (open-input-file "9.txt")))

(define input-as-vector
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


(define (n-length vec)
  (vector-length vec))

(define (m-length vec)
  (vector-length
   (vector-ref vec 0)))

(define (top-higher vec m n)
  (cond
    [(>= n (sub1 (n-length vec))) #t]
    [else (< (2d-ref vec m n) (2d-ref vec m (add1 n)))]))

(define (bot-higher vec m n)
  (cond
    [(<= n 0) #t]
    [else (< (2d-ref vec m n) (2d-ref vec m (sub1 n)))]))

(define (left-higher vec m n)
  (cond
    [(<= m 0) #t]
    [else (< (2d-ref vec m n) (2d-ref vec (sub1 m) n))]))

(define (right-higher vec m n)
  (cond
    [(>= m (sub1 (m-length vec))) #t]
    [else (< (2d-ref vec m n) (2d-ref vec (add1 m) n))]))

(define (is-lowpoint vec m n)
  (and
   (top-higher vec m n)
   (bot-higher vec m n)
   (left-higher vec m n)
   (right-higher vec m n)))
  

(define (get-lowpoints vec)
  (for*/list [(m (in-range (m-length vec)))
              (n (in-range (n-length vec)))
              #:when (is-lowpoint vec m n)]
    (list m n (2d-ref vec m n))))

;;deboog

;; problem 1

(apply +
       (map add1
            (map last
                 (get-lowpoints input-as-vector))))

;; problem 2

(define marks (2d-vec (m-length input-as-vector) (n-length input-as-vector) #f))

(define (visited? m n)
  (2d-ref marks m n))

(define (visit m n)
  (2d-set! marks m n #t))

(define (out-of-bounds? m n)
  (or
   (< m 0)
   (< n 0)
   (>= m (m-length input-as-vector))
   (>= n (n-length input-as-vector))))
    

(define (bfs-basin-size m n)
  (cond
    [(out-of-bounds? m n) 0]
    [(visited? m n) 0]
    [(eq? 9 (2d-ref input-as-vector m n)) 0]
    [else
     (visit m n)
     (+
      1
      (bfs-basin-size (add1 m) n)
      (bfs-basin-size (sub1 m) n)
      (bfs-basin-size m (add1 n))
      (bfs-basin-size m (sub1 n)))]))
  
(define basins
  (for*/list [(m (in-range (m-length input-as-vector)))
              (n (in-range (n-length input-as-vector)))]
    (define elem (2d-ref input-as-vector m n))
    (cond
      [(eq? elem 9) 0] ; continue -- nothing to see here
      [(visited? m n) 0] ; continue -- already been calculated
      [else (bfs-basin-size m n)]))) ; dfs will return the size of the basin

(apply * (take (sort basins >) 3))
