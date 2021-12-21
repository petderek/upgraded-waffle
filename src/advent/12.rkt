#lang racket

(define raw-input
  (port->lines (open-input-file "12.txt")))

(define (equals2? x)
  (eqv? 2 x))

(define (is-big? name)
  (match name
    [(regexp #px"[A-Z]+") #t]
    [(regexp #px"[a-z]+") #f]))

(define (input-as-adj-list)
  (define ht (make-hash))
  (for* [(line (in-list raw-input))
         (reversed (in-list (list #t #f)))]
    (define args (string-split line "-"))
    (if reversed
        (hash-update! ht (first args)
                      (lambda (x) (append x (drop args 1))) (list))
        (hash-update! ht (second args)
                      (lambda (x) (append  x (take args 1))) (list))))
  ht)

(define (get-distinct-paths adj)
  (define paths (mutable-set))
  (let loop [(at (list "start"))
             (visited (set))]
    (define newvisit
      (if (is-big? (first at))
          visited
          (set-add visited (first at))))
    (define unvisited-children
      (filter-not
       (lambda (x) (set-member? visited x))
       (hash-ref adj (first at))))
    (cond
      [(equal? "end" (first at)) (set-add! paths (string-join (map ~a (reverse at)) "-"))]
      [else
       (for [(child (in-list unvisited-children))]
         (loop (append (list child) at) newvisit))])
    (set->list paths)))

(define (count-above-2 lst)
  (length (filter equals2? lst)))

(define (get-doubled-paths adj)
  (define paths (mutable-set))
  (let loop [(at (list "start"))
             (trips (hash))]

    (define newtrips
      (cond
        [(is-big? (first at)) trips]
        [else (hash-update trips (first at) add1 0)]))
   
    (define unvisited-children
      (filter
       (lambda (x)
         (cond
           [(equal? "start" x) #f]
           [(is-big? (first at)) #t]
           [(eqv? 1 (hash-ref newtrips (first at))) #t]
           [(eqv? 1 (count-above-2 (hash-values newtrips))) #t]
           [else #f]))
       (hash-ref adj (first at))))
    (cond
      [(equal? "end" (first at)) (set-add! paths (string-join (map ~a (reverse at)) "-"))]
      [else
       (for [(child (in-list unvisited-children))]
         (loop (append (list child) at) newtrips))])
    (set->list paths)))

;; problem 1
(length (get-distinct-paths (input-as-adj-list)))

;; problem 2
(length (get-doubled-paths (input-as-adj-list)))