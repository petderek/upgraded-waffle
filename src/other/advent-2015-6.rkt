#lang racket

;; organize the inputs a little
(struct command (func from to))

(define (list->values l)
  (values
   (first l)
   (first (rest l))
   (last l)))

(define (awkward_regex_parse str)
  (list->values
   (string-split
    (regexp-replace
     #px"(.*) (\\d{1,3},\\d{1,3}) through (\\d{1,3},\\d{1,3})"
     str
     "\\1;\\2;\\3")
    ";")))

(define (string->coordinate str)
  (letrec ([l (string-split str ",")]
           [left (first l)]
           [right (last l)])
    (cons (string->number left) (string->number right))))

(define (string->f str)
  (match str
    ["turn off" (lambda (x) (and #f x))]
    ["turn on" (lambda (x) (or #t x))]
    ["toggle" not]))

(define (decrease-brightness x)
  (if (eq? 0 x) 0 (+ -1 x)))

(define (string->f2 str)
  (match str
    ["turn off" decrease-brightness]
    ["turn on" (lambda (x) (+ 1 x))]
    ["toggle" (lambda (x) (+ 2 x))]))
                 

(define (string->command str)
  (letrec-values ([(func_str from_str to_str) (awkward_regex_parse str)]
                  [(f) (string->f func_str)]
                  [(from) (string->coordinate from_str)]
                  [(to) (string->coordinate to_str)])
    (command f from to)))

(define (string->command2 str)
  (letrec-values ([(func_str from_str to_str) (awkward_regex_parse str)]
                  [(f) (string->f2 func_str)]
                  [(from) (string->coordinate from_str)]
                  [(to) (string->coordinate to_str)])
    (command f from to)))
    
    
(define (get-commands v1-or-v2-func)
  (with-input-from-file "advent-2015-6.txt"
    (lambda ()
      (for/list ([line (in-lines)])
        (v1-or-v2-func line)))))

(define (vector-ref-2d vec x y)
  (vector-ref (vector-ref vec x) y))

(define (vector-set-2d! vec x y val)
  (vector-set! (vector-ref vec x) y val))
  

(define (apply-command vec cmd)
  (for* ([i (in-range (car (command-from cmd)) (add1 (car (command-to cmd))))]
         [j (in-range (cdr (command-from cmd)) (add1 (cdr (command-to cmd))))])
    (let ([current (vector-ref-2d vec i j)]
          [func (command-func cmd)])
      (vector-set-2d! vec i j (func current)))))


(define (gimme-a-2d-vector-full-of fill)
  (let ([vec (make-vector 1000)])
    (for [(i (in-range 0 1000))]
      (vector-set! vec i (make-vector 1000 fill)))
    vec))
     
         
;; solution part 1

(let ([vec (gimme-a-2d-vector-full-of #f)])
  (for [(cmd (in-list (get-commands string->command)))]
    (apply-command vec cmd))
  (for/sum ([outer (in-vector vec)])
    (for/sum ([present? (in-vector outer)])
      (if present? 1 0))))

;; solution part 2

(let ([vec (gimme-a-2d-vector-full-of 0)])
  (for [(cmd (in-list (get-commands string->command2)))
        (i (in-naturals))]
    (apply-command vec cmd))
  (for/sum ([outer (in-vector vec)])
    (for/sum ([inner (in-vector outer)]) inner)))