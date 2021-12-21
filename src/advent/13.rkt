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

(define raw-input
  (port->lines (open-input-file "13.txt")))

(define grid-input
  (for/list [(line (in-list raw-input))
             #:unless (or (equal? "" line) (string-prefix? line "fold"))]
    (map string->number (string-split line ","))))

(define fold-input
  (for/list [(line (in-list raw-input))
             #:when (string-prefix? line "fold")]
    (define sploot (string-split line "="))
    ;; 11 and 12 are positions of x/y in 'fold along x'
    (define x_or_y (substring (first sploot) 11 12))
    (list x_or_y (string->number (second sploot)))))

(define grid-size
  (let loop [(x 0) (y 0) (lst grid-input)]
    (cond
      [(empty? lst) (list (add1 x) (add1 y))]
      [else
       (define cur (first lst))
       (loop (max x (first cur)) (max y (second cur)) (rest lst))])))


;; this does the reversal too
(define (copy-vec vec fromm fromn tom ton)
  (define msize (add1 (abs (- tom fromm))))
  (define nsize (add1 (abs (- ton fromn))))
  (define mdir
    (cond
      [(< tom fromm) -1]
      [(> tom fromm) 1]))
  (define ndir
    (cond
      [(< ton fromn) -1]
      [(> ton fromn) 1]))
  (define new-vec (2d-vec msize nsize #f))
  (for [(i (in-naturals))
        (m (in-inclusive-range fromm tom mdir))]
    (for [(j (in-naturals))
          (n (in-inclusive-range fromn ton ndir))]
      (2d-set! new-vec i j (2d-ref vec m n))))
  new-vec)

(define (apply-grid-to-vec! vec grid)
  (for [(point (in-list grid))]
    (2d-set! vec (first point) (second point) #t)))

(define (count-dots vec)
  (for*/sum [(outer (in-vector vec))
             (set? (in-vector outer))]
    (if set? 1 0)))

(define (add-vectors vec nvec)
  (define even-newer-vec (2d-vec (m-length nvec) (n-length nvec) 0))
  (for* [(m (in-range (m-length nvec)))
         (n (in-range (n-length nvec)))]
    (2d-set!
     even-newer-vec m n
     (or
      (2d-ref vec m n)
      (2d-ref nvec m n))))
  even-newer-vec)

(define (fold-input-to-directions vec lst)
  (define direction (first lst))
  (define edge (add1 (second lst)))
  (match direction
    ["x" (list (sub1 (m-length vec)) 0 edge (sub1 (n-length vec)))]
    ["y" (list 0 (sub1 (n-length vec)) (sub1 (m-length vec)) edge)]))

;; problem 1
(define prob1vec (2d-vec (first grid-size) (second grid-size) #f))
(apply-grid-to-vec! prob1vec grid-input)
(define lst (fold-input-to-directions prob1vec (first fold-input)))
(define folded (copy-vec prob1vec (first lst) (second lst) (third lst) (fourth lst)))
(define after-first (add-vectors prob1vec folded))
(count-dots after-first)

