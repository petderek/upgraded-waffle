#lang racket

;; get rid of spaces and only do commas
(define (normalize-line line)
  (define no-double-spaces (string-replace line "  " " "))
  (string-replace no-double-spaces " " ","))

(define (line->numlist line)
  (map
   string->number
   (string-split
    (normalize-line line)
    ",")))

(define raw-input
  (port->lines (open-input-file "4.txt")))

(define bingo-order
  (line->numlist (first raw-input)))


;; ok so, bingo sheets are 2d vector of (int, bool) pairs... stored in a list

(struct node (value visited))

(define (get-empty-bingo-sheet)
  (define sheet (make-vector 5))
  (for [(i (in-range 5))]
    (vector-set!
     sheet
     i
     (make-vector 5 (node 0 #f))))
  sheet)

(define (populate-bingo-sheet sheet lines)
  (for [(line (in-list lines))
        (i (in-naturals))]
    (define inner (vector-ref sheet i))
    (for [(value (in-list (line->numlist line)))
          (j (in-naturals))]
      (vector-set! inner j (node value #f)))))

(define (get-bingo-node sheet i j)
  (vector-ref
   (vector-ref sheet i)
   j))

(define (set-bingo-node! sheet i j val)
  (define inner (vector-ref sheet i))
  (vector-set!
   inner
   j
   val))

(define (get-bingo-sheets)
  (define input
    (filter
     (lambda (x) (not (string=? x "")))
     (drop raw-input 2)))
  (let loop [(set-of-input (take input 5))
             (rest-of-input (drop input 5))
             (results (list))]
    (define sheet (get-empty-bingo-sheet))
    (populate-bingo-sheet sheet set-of-input)
    (define next-results (append results (list sheet)))
    (cond
      [(empty? rest-of-input) next-results]
      [else (loop (take rest-of-input 5) (drop rest-of-input 5) next-results)])))


(define (update-bingo-sheet sheet drawn)
  (for* [(i (in-range 5))
         (j (in-range 5))]
    (define current (get-bingo-node sheet i j))
    (if (eq? drawn (node-value current))
        (set-bingo-node! sheet i j (node drawn #t))
        (void))))
     

(define (is-winner? sheet)
  (or
   (for/or [(i (in-range 5))]
     (for/and [(j (in-range 5))]
       (node-visited (get-bingo-node sheet i j))))
   (for/or [(j (in-range 5))]
     (for/and [(i (in-range 5))]
       (node-visited (get-bingo-node sheet i j))))))
    
(define (bingo-sheet-sum sheet)
  (for*/sum [(i (in-range 5))
             (j (in-range 5))]
    (define n (get-bingo-node sheet i j))
    (if (node-visited n)
        0
        (node-value n))))
    
;; problem 1

(let loop [(drawn (first bingo-order))
           (next (rest bingo-order))
           (sheets (get-bingo-sheets))]
  (define winners
    (for/sum [(sheet (in-list sheets))]
      (update-bingo-sheet sheet drawn)
      (if (is-winner? sheet) 1 0)))
  (cond
    [(> winners 0) (* drawn (first (map bingo-sheet-sum (filter is-winner? sheets))))]
    [(empty? next) "this shouldn't happen -- no winners but we are out of numbers"]
    [else (loop (first next) (rest next) sheets)]))

;; problem 2

(let loop [(drawn (first bingo-order))
           (next (rest bingo-order))
           (sheets (get-bingo-sheets))]
  (for [(sheet (in-list sheets))]
    (update-bingo-sheet sheet drawn))

  ; we want to have a single sheet that is marked as a winner
  (cond
    [(and (eq? (length sheets) 1) (is-winner? (first sheets)))
     (* drawn (first (map bingo-sheet-sum sheets)))]
    [(empty? next) "this shouldn't happen -- no winners but we are out of numbers"]
    ; filter out all sheets that are winners until only one remains
    [else (loop (first next) (rest next) (filter (lambda (s) (not (is-winner? s))) sheets))]))
